# 2 Patient data
# Download, unzip, and perform initial cleaning of patient records
get_patients <- function(url, destfile, csv_path) {
  # Download zip if not present locally
  if (!fs::file_exists(destfile)) {
    curl::curl_download(url, destfile, quiet = FALSE)
  }
  
  # Read specific CSV from zip, convert to data.table, and set the key
  patients <- readr::read_csv(unz(destfile, csv_path))
  data.table::setDT(patients)
  data.table::setkey(patients, id)
  
  # Remove entirely empty or constant columns/rows
  patients <- janitor::remove_empty(patients, which = c("rows", "cols"), quiet = FALSE)
  patients <- janitor::remove_constant(patients, quiet = FALSE)
  
  return(patients)
}


# 3 Expectations/validations
# Perform data quality checks and export an interactive HTML validation report
validate_patients <- function(data) {
  agent <- data |>
    pointblank::create_agent(label = "Patient Data Quality Validation") |>
    
    # Check: dates are realistic (1900 to present)
    pointblank::col_vals_between(
      where(is.Date),
      as.Date("1900-01-01"),
      as.Date(Sys.Date()),
      na_pass = TRUE,
      label = "Dates should be between 1900 and today."
    ) |>
    
    # Check: death cannot occur before birth
    pointblank::col_vals_gte(
      deathdate,
      pointblank::vars(birthdate),
      na_pass = TRUE,
      label = "Death date must be greater than or equal to birthdate."
    ) |>
    
    # Check: standard US SSN format
    pointblank::col_vals_regex(
      ssn,
      "^[0-9]{3}-[0-9]{2}-[0-9]{4}$",
      label = "SSN must follow the 000-00-0000 format."
    ) |>
    
    # Check: gender is M or F
    pointblank::col_vals_in_set(
      gender,
      set = c("M", "F"),
      label = "Gender must be either 'M' or 'F'."
    ) |>
    
    # Check: marital status is S, M, D, or W
    pointblank::col_vals_in_set(
      marital,
      set = c("S", "M", "D", "W"),
      label = "Marital status must be S, M, D, or W."
    ) |>
    
    # Check: IDs must be present and unique
    pointblank::col_vals_not_null(id, label = "ID cannot be NULL.") |>
    pointblank::rows_distinct(pointblank::vars(id), label = "Each patient ID must be unique.") |>
    
    pointblank::interrogate()
  
  # Export results to a HTML file
  pointblank::export_report(agent, "patient_validation.html")
  return("patient_validation.html")
}

# Standardize categorical variables
process_patients <- function(data) {
  dt <- data.table::as.data.table(data)
  
  # Convert marital abbreviations to descriptive factors
  dt[, marital := factor(
    marital,
    levels = c("S", "M", "D", "W"),
    labels = c("Single", "Married", "Divorced", "Widowed")
  )]

  # Identify and transform other categorical variables. Find character columns with fewer than 10 unique values
  # fctr_candidates <- names(dt)[dt[, lapply(.SD, data.table::uniqueN) < 10, .SDcols = is.character]]
  # dt[, (fctr_candidates) := lapply(.SD, as.factor), .SDcols = fctr_candidates
  
  # Lump infrequent racial groups (prop < 5%) into "Other" to simplify analysis
  if ("race" %in% names(dt)) {
    dt[, race := forcats::fct_lump_prop(race, prop = 0.05)] 
  }
  
  return(dt)
}


# 4 Derived variables
# Determine the latest data entry date to use as a temporal reference point
get_snapshot_date <- function(zip_path) {
  unzip(zip_path, files = "data-fixed/payer_transitions.csv")
  
  # Efficiently read only the required column using duckdb
  last_date <- duckplyr::read_csv_duckdb("data-fixed/payer_transitions.csv") |>
    dplyr::summarise(lastdate = max(start_date, na.rm = TRUE)) |>
    dplyr::collect() |>
    dplyr::pull(lastdate) |>
    as.Date()
    
  return(last_date)
}

# Calculate age and vital status based on the snapshot date
add_derived_variables <- function(data, snapshot_date) {
  dt <- data.table::as.data.table(data)

  # Calculate age in years (using average year length for precision)
  dt[, age := as.integer(as.Date(snapshot_date) - as.Date(birthdate)) %/% 365.241]

  # Determine if patient was alive at the time the data was generated
  dt[, is_living_at_snapshot := is.na(deathdate) | deathdate > snapshot_date]
  
  return(dt)
}


# 5 Names
process_patient_names <- function(data) {
  dt <- data.table::as.data.table(data)
  cols_to_fix <- c("prefix", "first", "middle", "last", "suffix")

  # Ensure all name parts are characters and replace NAs with empty strings
  dt[, (cols_to_fix) := lapply(.SD, as.character), .SDcols = cols_to_fix]
  dt[, (cols_to_fix) := lapply(.SD, \(x) tidyr::replace_na(x, "")), .SDcols = cols_to_fix]
  
  # Assemble full name and clean up extra whitespace
  dt[, full_name := paste(prefix, first, middle, last)]
  dt[suffix != "", full_name := paste0(full_name, ", ", suffix)]
  dt[, full_name := stringr::str_squish(full_name)]
  
  # Remove granular name components to tidy the table
  cols_to_remove <- c("prefix", "first", "middle", "last", "suffix", "maiden")
  dt[, (cols_to_remove) := NULL]
  
  return(dt)
}


# 6 Necessary data (Geospatial & Identity Processing & Mapping)
process_patient_geo_and_id <- function(data) {
  dt <- data.table::as.data.table(data)
  
  # Convert geographic coordinates to numeric for mapping
  dt[, `:=`(lat = as.numeric(lat), lon = as.numeric(lon))]
  
  # Extract specific Driver's License ID format (S + 8 digits)
  dt[, dl := stringr::str_extract(drivers, "S[0-9]{8}")]
  
  # Concatenate full address and remove raw components
  dt[, full_address := paste(address, city, state, zip, sep = ", ")]
  dt[, c("address", "city", "state", "zip", "drivers") := NULL]
  data.table::setnames(dt, "full_address", "address")
  
  return(dt)
}

# Generate an interactive geospatial visualization using leaflet
create_patient_map <- function(data) {
  map <- leaflet::leaflet(data = data) |>
    leaflet::addTiles() |>
    leaflet::addMarkers(
      lng = ~lon, 
      lat = ~lat, 
      label = ~full_name
    )
  
  return(map)
}


# 7 Linkage
# Unzip CSVs and convert them to Parquet for optimized downstream access
convert_data_to_parquet <- function(zip_path) {
  zip::unzip(zip_path)
  fs::dir_create("data-parquet")
  
  files <- fs::dir_ls("data-fixed/", glob = "*.csv")
  
  # Batch conversion process using duckplyr
  purrr::walk(files, function(file) {
    new_file <- file |>
      stringr::str_replace("data-fixed", "data-parquet") |>
      stringr::str_replace(".csv", ".parquet")
    
    duckplyr::read_csv_duckdb(file) |>
      duckplyr::compute_parquet(new_file)
  })
  
  # Clean up temporary CSV files
  fs::dir_delete("data-fixed")
  return("data-parquet")
}

# Extract and filter medical procedure records from Parquet storage
get_processed_procedures <- function(parquet_dir) {
  path <- fs::path(parquet_dir, "procedures.parquet")
  procs <- duckplyr::read_parquet_duckdb(path) |>
    # Select only the ID, ICD10 code, and start date, and filter out missing codes
    dplyr::select(patient, reasoncode_icd10, start) |>
    dplyr::filter(!is.na(reasoncode_icd10)) |>
    dplyr::collect()
  
  dt <- data.table::as.data.table(procs)
  # Extract the year and delete the original date column
  dt[, year := data.table::year(start)][, start := NULL]
  
  return(dt)
}

# Join patients with procedures to analyze disease frequency
analyze_adult_procedures <- function(patients, procedures) {
  # Ensure the patient table contains the birthdate
  p_small <- patients[, .(id, birthdate = as.IDate(birthdate))]
  
  # Data linkage via patient ID
  result <- procedures[p_small, on = .(patient = id), nomatch = NULL]
  
  # Filter for adults and aggregate counts by condition and year
  summary <- result[year - data.table::year(birthdate) >= 18, 
                    .(N = .N), 
                    by = .(reasoncode_icd10, year)]
  
  return(summary)
}

# Decode ICD-10 labels and plot the trend for the top 5 medical conditions
plot_top_conditions <- function(summary_data) {
  # Link codes with Swedish ICD-10 descriptions using decoder
  cond_by_year <- data.table::setDT(decoder::icd10se)[
    summary_data, 
    on = c(key = "reasoncode_icd10")
  ]
  
  # Identify the top 5 most frequent conditions overall
  top5_values <- cond_by_year[, .(total_N = sum(N)), by = value][
    order(-total_N)
  ][1:5, value]
  
  # Visualize temporal trends using ggplot2
  p <- ggplot2::ggplot(
    cond_by_year[value %in% top5_values], 
    ggplot2::aes(x = year, y = N, color = value)
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(color = ggplot2::guide_legend(ncol = 1)) + # Formatting legend labels to prevent text overla
    ggplot2::scale_color_discrete(labels = \(x) stringr::str_wrap(x, width = 40)) +
    ggplot2::labs(
      title = "Top 5 Medical Conditions Over Time (Adults)",
      x = "Year of Procedure",
      y = "Number of Procedures",
      color = "Condition"
    )
  
  return(p)
}


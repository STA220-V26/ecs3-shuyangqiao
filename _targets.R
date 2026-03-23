# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(packages = c("tidyverse", "data.table", "janitor", "curl", "fs", "pointblank", "duckplyr", "decoder"))
# tar_option_set(
  # packages = c("tibble") # Packages that your targets need for their tasks.
  # format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # Pipelines that take a long time to run may benefit from
  # optional distributed computing. To use this capability
  # in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller that scales up to a maximum of two workers
  # which run as local R processes. Each worker launches when there is work
  # to do and exits if 60 seconds pass with no tasks to run.
  #
  #   controller = crew::crew_controller_local(workers = 2, seconds_idle = 60)
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package.
  # For the cloud, see plugin packages like {crew.aws.batch}.
  # The following example is a controller for Sun Grid Engine (SGE).
  #
  #   controller = crew.cluster::crew_controller_sge(
  #     # Number of workers that the pipeline can scale up to:
  #     workers = 10,
  #     # It is recommended to set an idle time so workers can shut themselves
  #     # down if they are not running tasks.
  #     seconds_idle = 120,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.2".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
# )

# Run the R scripts in the R/ folder with your custom functions:
source("R/functions.R")
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
# list(
#   tar_target(
#     name = data,
#     command = tibble(x = rnorm(100), y = rnorm(100))
#     # format = "qs" # Efficient storage for general data objects.
#   ),
#   tar_target(
#     name = model,
#     command = coefficients(lm(y ~ x, data = data))
#   )
# )

list(
  # 2 Patient data
  # Define the remote source for the dataset zip file
  tar_target(data_url, "https://github.com/eribul/cs/raw/refs/heads/main/data.zip"),

  # Download and extract the patients dataset from the zip archive
  tar_target(
    patients, 
    get_patients(data_url, "data.zip", "data-fixed/patients.csv")
  ),

  # 3 Expectations/validations
  # Generate a validation report to check for data integrity
  tar_target(
    patient_validation_report,
    validate_patients(patients),
    format = "file"
  ),

  # Basic data cleaning
  tar_target(
    patients_processed,
    process_patients(patients)
  ),

  # 4 Derived variables
  # Extract the snapshot date from the zip file metadata for age calculations
  tar_target(
    snapshot_date,
    get_snapshot_date("data.zip")
  ),

  # Calculate patient age and other derived metrics based on the snapshot date
  tar_target(
    patients_with_age,
    add_derived_variables(patients_processed, snapshot_date)
  ),

  # 5 Clean and standardize patient names
  tar_target(
    patients_final_names,
    process_patient_names(patients_with_age)
  ),

  # 6
  # Process coordinates (lat/lon), format addresses, and extract Driver's License IDs
  tar_target(
    patients_complete,
    process_patient_geo_and_id(patients_final_names)
  ),

  # Generate an interactive Leaflet map to visualize patient geographic distribution
  tar_target(
    patient_locations_map,
    create_patient_map(patients_complete)
  ),

  # 7 Linkage
  # Convert raw CSV data to Parquet format for optimized storage and performance
  tar_target(
    parquet_folder,
    convert_data_to_parquet("data.zip"),
    format = "file"
  ),

  # Read and pre-process the procedures dataset from the Parquet files
  tar_target(
    procedures_data,
    get_processed_procedures(parquet_folder)
  ),

  # Link patients with procedures to summarize condition trends
  tar_target(
    adult_proc_summary,
    analyze_adult_procedures(patients_complete, procedures_data)
  ),

  # Create a visualization of the top 5 medical conditions over time
  tar_target(
    top_conditions_plot,
    plot_top_conditions(adult_proc_summary)
  )
)


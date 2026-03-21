# 2 Patient data
# 获取并初步清洗患者数据
get_patients <- function(url, destfile, csv_path) {
  # 1. 下载数据（如果本地不存在） [cite: 28-35]
  if (!fs::file_exists(destfile)) {
    curl::curl_download(url, destfile, quiet = FALSE)
  }
  
  # 2. 从 ZIP 中直接读取 CSV [cite: 38, 42]
  # 使用 readr::read_csv 配合 unz 是因为 fread 无法直接读取 zip 内的文件 [cite: 36-37]
  patients <- readr::read_csv(unz(destfile, csv_path))
  
  # 3. 转换为 data.table 并设置主键 [cite: 43, 46]
  data.table::setDT(patients)
  data.table::setkey(patients, id)
  
  # 4. 清理空行/空列及常量列 [cite: 47-51]
  patients <- janitor::remove_empty(patients, which = c("rows", "cols"), quiet = FALSE)
  patients <- janitor::remove_constant(patients, quiet = FALSE)
  
  return(patients)
}


# 3 Expectations/validations
# 数据验证函数
validate_patients <- function(data) {
  agent <- data |>
    pointblank::create_agent(label = "Patient Data Quality Validation") |>
    
    # 检查所有日期列是否在合理范围内（1900年至今） [cite: 64-67]
    pointblank::col_vals_between(
      where(is.Date),
      as.Date("1900-01-01"),
      as.Date(Sys.Date()),
      na_pass = TRUE,
      label = "Dates should be between 1900 and today."
    ) |>
    
    # 检查死亡日期是否晚于或等于出生日期 [cite: 71-74]
    pointblank::col_vals_gte(
      deathdate,
      pointblank::vars(birthdate),
      na_pass = TRUE,
      label = "Death date must be greater than or equal to birthdate."
    ) |>
    
    # 检查 SSN 格式是否正确 [cite: 77-80]
    pointblank::col_vals_regex(
      ssn,
      "^[0-9]{3}-[0-9]{2}-[0-9]{4}$",
      label = "SSN must follow the 000-00-0000 format."
    ) |>
    
    # --- 你需要新增的 3 条验证规则（示例） ---
    
    # 1. 检查性别是否只包含预期值 (M, F) [cite: 58, 61]
    pointblank::col_vals_in_set(
      gender,
      set = c("M", "F"),
      label = "Gender must be either 'M' or 'F'."
    ) |>
    
    # 2. 检查婚姻状况是否在已知范围内 [cite: 57, 61]
    pointblank::col_vals_in_set(
      marital,
      set = c("S", "M", "D", "W"),
      label = "Marital status must be S, M, D, or W."
    ) |>
    
    # 3. 检查 ID 是否唯一（主键约束） 
    pointblank::col_vals_not_null(id, label = "ID cannot be NULL.") |>
    pointblank::rows_distinct(pointblank::vars(id), label = "Each patient ID must be unique.") |>
    
    pointblank::interrogate()
  
  # 导出报告并返回文件路径供 targets 追踪 
  pointblank::export_report(agent, "patient_validation.html")
  return("patient_validation.html")
}


process_patients <- function(data) {
  # 确保是 data.table 格式
  dt <- data.table::as.data.table(data)
  
  # --- 3.1 因子转换 (Factors) --- [cite: 93-94]
  # 将婚姻状况缩写转换为完整标签 [cite: 102-111]
  dt[, marital := factor(
    marital,
    levels = c("S", "M", "D", "W"),
    labels = c("Single", "Married", "Divorced", "Widowed")
  )]
  
  # 自动识别并转换其他分类变量（如性别、种族、州等） [cite: 114-121]
  # 寻找唯一值较少（少于10个）的字符型列
  # fctr_candidates <- names(dt)[dt[, lapply(.SD, data.table::uniqueN) < 10, .SDcols = is.character]]
  # dt[, (fctr_candidates) := lapply(.SD, as.factor), .SDcols = fctr_candidates]
  
  # --- 3.2 隐私保护 (Disclosure Control) --- [cite: 129-131]
  # 使用 forcats 减少低比例种族的细分，将其归类为 "Other" [cite: 136-137]
  # 这能防止通过“性别+种族+州”组合定位到具体的个人 [cite: 132-134]
  if ("race" %in% names(dt)) {
    dt[, race := forcats::fct_lump_prop(race, prop = 0.05)] 
  }
  
  return(dt)
}


# 4 Derived variables
# 函数 1：获取数据快照日期 [cite: 147-155]
get_snapshot_date <- function(zip_path) {
  # 解压特定的文件用于分析 [cite: 150]
  unzip(zip_path, files = "data-fixed/payer_transitions.csv")
  
  # 使用 duckplyr 快速读取并计算最大日期 [cite: 151-152]
  last_date <- duckplyr::read_csv_duckdb("data-fixed/payer_transitions.csv") |>
    dplyr::summarise(lastdate = max(start_date, na.rm = TRUE)) |> # 指南建议用 start_date [cite: 158]
    dplyr::collect() |>
    dplyr::pull(lastdate) |>
    as.Date()
    
  return(last_date)
}

# 函数 2：计算年龄并添加派生变量 [cite: 139-141]
add_derived_variables <- function(data, snapshot_date) {
  dt <- data.table::as.data.table(data)
  
  # 计算在快照日期时的年龄 [cite: 140, 159]
  # 使用整除 %/% 365.241 来获得周岁 
  dt[, age := as.integer(as.Date(snapshot_date) - as.Date(birthdate)) %/% 365.241]
  
  # 标记数据采集时是否在世 [cite: 142-143, 159]
  # 如果死亡日期为空，或者死亡日期晚于快照日期，则认为在采集时“在世”
  dt[, is_living_at_snapshot := is.na(deathdate) | deathdate > snapshot_date]
  
  return(dt)
}


# 5 Names
process_patient_names <- function(data) {
  dt <- data.table::as.data.table(data)
  
  # 1. 核心修复：先将相关的列转回字符型，避免与 Factor 冲突
  cols_to_fix <- c("prefix", "first", "middle", "last", "suffix")
  
  # 使用 as.character 转换，这样就可以安全地替换为 "" 了
  dt[, (cols_to_fix) := lapply(.SD, as.character), .SDcols = cols_to_fix]
  
  # 2. 处理缺失值
  dt[, (cols_to_fix) := lapply(.SD, \(x) tidyr::replace_na(x, "")), .SDcols = cols_to_fix]
  
  # 3. 合并全名
  dt[, full_name := paste(prefix, first, middle, last)]
  dt[suffix != "", full_name := paste0(full_name, ", ", suffix)]
  
  # 4. 清理空格
  dt[, full_name := stringr::str_squish(full_name)]
  
  # 5. 按照指令删除原始列
  # 注意：如果指令要求保留 maiden，请从列表中移除它
  cols_to_remove <- c("prefix", "first", "middle", "last", "suffix", "maiden")
  dt[, (cols_to_remove) := NULL]
  
  return(dt)
}


# 6 Necessary data. Addresses & Driving Licenses
# process_patient_geo_and_id <- function(data) {
#   dt <- data.table::as.data.table(data)
  
#   # 1. 清理经纬度（转换为数值型）
#   dt[, `:=`(lat = as.numeric(lat), lon = as.numeric(lon))]
  
#   # 2. 拼接完整地址
#   dt[, address := paste(address, city, state, zip, sep = ", ")]
  
#   # 3. 提取驾驶证号 (DL)
#   # 驾驶证号通常藏在 'drivers' 列中，我们提取 S999... 这种格式
#   dt[, dl := stringr::str_extract(drivers, "S[0-9]{8}")]
  
#   # 4. 删除不再需要的列
#   dt[, c("city", "state", "zip", "drivers") := NULL]
  
#   return(dt)
# }

process_patient_geo_and_id <- function(data) {
  dt <- data.table::as.data.table(data)
  
  # 1. 转换经纬度：显式指定类型转换 [cite: 188-191]
  dt[, lat := as.numeric(lat)]
  dt[, lon := as.numeric(lon)]
  
  # 2. 提取驾驶证号 (DL) [cite: 198-200]
  # 必须在删除 'drivers' 列之前执行
  dt[, dl := stringr::str_extract(drivers, "S[0-9]{8}")]
  
  # 3. 拼接完整地址 [cite: 193-195]
  # 技巧：创建一个新列名 'full_address' 避免覆盖原始的 'address' 街道列导致 paste 出错
  dt[, full_address := paste(address, city, state, zip, sep = ", ")]
  
  # 4. 批量删除不再需要的列 [cite: 185, 191-192]
  cols_to_remove <- c("address", "city", "state", "zip", "drivers")
  dt[, (cols_to_remove) := NULL]
  
  # 5. 为了保持下游代码兼容，可以把 full_address 改回 address
  # data.table::setnames(dt, "full_address", "address")
  
  return(dt)
}

# 生成患者分布地图 [cite: 198-201]
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
# 1. 将 CSV 转换为 Parquet (Section 7 中的建议) [cite: 212-219]
convert_data_to_parquet <- function(zip_path) {
  zip::unzip(zip_path) # 解压所有文件到 data-fixed [cite: 210]
  fs::dir_create("data-parquet")
  
  files <- fs::dir_ls("data-fixed/", glob = "*.csv")
  
  purrr::walk(files, function(file) {
    new_file <- file |>
      stringr::str_replace("data-fixed", "data-parquet") |>
      stringr::str_replace(".csv", ".parquet")
    
    duckplyr::read_csv_duckdb(file) |>
      duckplyr::compute_parquet(new_file)
  })
  
  fs::dir_delete("data-fixed") # 清理临时文件夹 [cite: 223]
  return("data-parquet")
}

# 2. 获取并处理程序数据 [cite: 224-238]
get_processed_procedures <- function(parquet_dir) {
  # 读取程序表 [cite: 225]
  path <- fs::path(parquet_dir, "procedures.parquet")
  procs <- duckplyr::read_parquet_duckdb(path) |>
    # 只选择 ID、ICD10编码和开始日期，并过滤掉缺失编码 [cite: 233-234]
    dplyr::select(patient, reasoncode_icd10, start) |>
    dplyr::filter(!is.na(reasoncode_icd10)) |>
    dplyr::collect()
  
  dt <- data.table::as.data.table(procs)
  # 提取年份并删除原始日期列 [cite: 238]
  dt[, year := data.table::year(start)][, start := NULL]
  
  return(dt)
}

# 3. 关联患者与程序并分析 [cite: 239-244]
analyze_adult_procedures <- function(patients, procedures) {
  # 确保患者表中有 birthdate
  p_small <- patients[, .(id, birthdate = as.IDate(birthdate))]
  
  # 执行关联 [cite: 242]
  # 过滤：(就医年份 - 出生年份) >= 18 岁 
  # 统计：按 ICD10 编码和年份分组计数 
  result <- procedures[p_small, on = .(patient = id), nomatch = NULL]
  
  summary <- result[year - data.table::year(birthdate) >= 18, 
                    .(N = .N), 
                    by = .(reasoncode_icd10, year)]
  
  return(summary)
}

plot_top_conditions <- function(summary_data) {
  # 1. 使用 {decoder} 包添加描述文本 [cite: 248-251]
  # 注意：这里将编码关联到瑞典语/英语描述
  cond_by_year <- data.table::setDT(decoder::icd10se)[
    summary_data, 
    on = c(key = "reasoncode_icd10")
  ]
  
  # 2. 找出总体频率最高的前 5 种疾病 [cite: 255-256]
  top5_values <- cond_by_year[, .(total_N = sum(N)), by = value][
    order(-total_N)
  ][1:5, value]
  
  # 3. 创建可视化图表 [cite: 257-264]
  p <- ggplot2::ggplot(
    cond_by_year[value %in% top5_values], 
    ggplot2::aes(x = year, y = N, color = value)
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(color = ggplot2::guide_legend(ncol = 1)) +
    # 对过长的标签进行自动换行 [cite: 264]
    ggplot2::scale_color_discrete(labels = \(x) stringr::str_wrap(x, width = 40)) +
    ggplot2::labs(
      title = "Top 5 Medical Conditions Over Time (Adults)",
      x = "Year of Procedure",
      y = "Number of Procedures",
      color = "Condition"
    )
  
  return(p)
}


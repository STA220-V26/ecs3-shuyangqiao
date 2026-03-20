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
  fctr_candidates <- names(dt)[dt[, lapply(.SD, data.table::uniqueN) < 10, .SDcols = is.character]]
  dt[, (fctr_candidates) := lapply(.SD, as.factor), .SDcols = fctr_candidates]
  
  # --- 3.2 隐私保护 (Disclosure Control) --- [cite: 129-131]
  # 使用 forcats 减少低比例种族的细分，将其归类为 "Other" [cite: 136-137]
  # 这能防止通过“性别+种族+州”组合定位到具体的个人 [cite: 132-134]
  if ("race" %in% names(dt)) {
    dt[, race := forcats::fct_lump_prop(race, prop = 0.05)] 
  }
  
  return(dt)
}


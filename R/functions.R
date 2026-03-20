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

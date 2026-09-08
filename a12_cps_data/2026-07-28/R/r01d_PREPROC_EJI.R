# 2026-07-29
# Process Excel file with EJI variable

# load raw data from file
df_raw <- boxr::box_read(
  file_id   = params$raw_data_eji$box_id,
  sheet     = params$raw_data_eji$sheet,
  read_fun  = readxl::read_xls
  )

# tidy variables
df_raw |> 
  janitor::clean_names() |> 
  mutate(
    rpl_ser           = case_when(rpl_ser != -999 ~ rpl_ser),
    school_match_name = standardize_school_names(short_name)
    ) |>  
  
  distinct(school_id, short_name, long_name, school_match_name, rpl_ser) |> 
  box_write_if_diff(
    f_name  = "cps_school_eji.csv",
    comment = paste(
      "EJI percentile ranked Social-Environmental Ranking (SER) values by school, ",
      "matched to school ID and name.",
      "Based on file", params$raw_data_eji$filename,
      ", sheet", params$raw_data_eji$sheet, ". "
    )
  )


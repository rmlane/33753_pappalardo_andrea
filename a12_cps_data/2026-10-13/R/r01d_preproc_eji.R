# 2026-07-29
# Process Excel file with EJI variable

# load raw data from file
eji_raw <- readxl::read_xls(
  nested_here("data-raw", raw_files$eji$filename),
  sheet = raw_files$eji$sheet
  )

# tidy variables
eji_raw |> 
  janitor::clean_names() |> 
  mutate(
    rpl_ser           = case_when(rpl_ser != -999 ~ rpl_ser),
    school_match_name = standardize_school_names(short_name)
    ) |>  
  distinct(school_id, school_match_name, rpl_ser) |> 
  
  # impute rpl ser where missing by averaging values from proximal tracts;
  # data from https://www.atsdr.cdc.gov/place-health/php/eji/eji-explorer.html
  mutate(
    rpl_ser_imp = case_when(
      school_match_name == "BEETHOVEN" ~ mean(
        0.83, # Census Tract: 8446
        0.95, # Census Tract: 3815
        0.89, # Census Tract: 3814
        0.63, # Census Tract: 3818
        0.88, # Census Tract: 8361
        1.00  # Census Tract: 8355
      ),
      
      .default = rpl_ser
    )
  ) |> 
  
  write_if_diff(
    f_name  = nested_here("data", "cps_school_eji.csv"),
    comment = c(
      "EJI percentile ranked Social-Environmental Ranking (SER) values by school, ",
      "matched to school ID and name.",
      "Based on file", params$raw_data_eji$filename,
      ", sheet", params$raw_data_eji$sheet, ". "
    )
  )


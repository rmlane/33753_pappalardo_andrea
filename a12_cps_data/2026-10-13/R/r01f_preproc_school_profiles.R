raw_files |> 
  transpose() |> 
  pluck("filename") |> 
  keep(~ grepl("^cps_school_profiles", .x)) |> 
  map_dfr(~{
    read_csv(nested_here("data-raw", .x)) |> 
      janitor::clean_names() |> 
      select(school_id, short_name, primary_category)
    }) |> 
  mutate(
    school_match_name = standardize_school_names(short_name),
    grade_level       = fct_collapse(
      primary_category,
      "Elementary/Middle School" = c("ES", "MS"),
      "High School"              = c("HS")
      )
    ) |> 
  distinct(school_id, school_match_name, grade_level) |> 
  
  # (maybe) write to file
  write_if_diff(
    f_name  = nested_here("data",  "cps_school_characteristics.csv"),
    comment = c(
      "School id, name, and grade level for all CPS schools between 2021-2022",
      " and 2024-2025. School profiles were downloaded from the Chicago ",
      "Data Portal via API. Data were fetched and merged on", 
      Sys.Date(), "."
    )
  )


cps_datasets_raw <- boxr::box_ls(params$analytic_data_dir_id) |> 
  as.data.frame() |> 
  filter(grepl("^cps_school", name))

cps_datasets_raw <- cps_datasets_raw$id |>
  set_names(cps_datasets_raw$name) |> 
  map(boxr::box_read_csv)

cps_data <- cps_datasets_raw |> 
  set_names(
    gsub("cps_", "", tools::file_path_sans_ext(names(cps_datasets_raw)))
  ) |> 
  map(~{
    .x |> mutate(
      across(
        any_of(c("school_id", matches("name"))), 
        as.character
        )
      )
  })

# time invariant
# cps_school_characteristics
# cps_school_waves
# cps_school_eji

# school-level (time-invariant) vars
cps_time_inv <- full_join(
  cps_data$school_characteristics,
  cps_data$school_waves |> distinct(school_id, wave),
  by     = c("school_id")
  ) |> 
  full_join(
    cps_data$school_eji |> distinct(school_id, rpl_ser),
    by     = c("school_id")
    ) |> 
  distinct(
    school_id, school_match_name, 
    grade_level, region_name, 
    wave, rpl_ser
    ) |> 
  mutate(
    wave = fct_relevel(
      fct_na_value_to_level(wave, "Fall 2024"),
      "Pilot", "Feb 2024", "April 2024", "Fall 2024"
      ),
    impl_date      = as.Date(
      fct_recode(
        wave,
        "2023-09-01" = "Pilot",
        "2024-02-01" = "Feb 2024",
        "2024-04-01" = "April 2024",
        "2024-09-01" = "Fall 2024"
        )
      ),
      impl_schoolyear = fct_collapse(
        wave,
        "2023-2024" = c("Pilot", "Feb 2024", "April 2024"),
        "2024-2025" = c("Fall 2024")
    ),
    .after = wave
  ) |> 
  
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
  )


# combine time-varying data
cps_annual <- full_join(
  cps_data$school_level_student_counts,
  cps_data$school_utilization_counts |> rename(n_events = n),
  by = c("school_match_name", "data_schoolyear")
  ) |> 
  left_join(
    cps_data$schoolyear_dates,
    by = "data_schoolyear"
    ) |> 
  
  arrange(data_schoolyear, school_match_name) |> 
  
  mutate(
    demog_date     = sy_day20,
    asthma_date    = sy_end,
    attend_date    = sy_end,
    
    n_events       = coalesce(n_events, 0),
    any_events     = as.numeric(n_events > 0),
    
    n_events_cum   = cumsum(n_events),
    any_events_cum = as.numeric(n_events_cum > 0),
    .by            = school_match_name
    ) |> 
  relocate(-matches("_count"), -matches("_pct"), -matches("days_per_row"))


# combine time-varying and time-invariant
cps_df_all <- full_join(
  cps_time_inv,
  cps_annual,
  by = "school_match_name"
  ) |> 
  
  # calculate time since implementation for each school by year
  mutate(
    impl_days   = difftime(sy_end, impl_date, units = "days"),
    impl_days   = unlist(map(impl_days, ~ max(.x, 0))),
    impl_binary = as.numeric(impl_days > 0),
    .before     = student_count
    )

# limit to relevant
cps_df_analytic <- cps_df_all |> 
  filter(!is.na(student_count)) |> 
  
  mutate(n_schools = "Overall") |> 
  
  box_write_if_diff(
    "cps_schools_analytic.csv",
    comment = paste(
      "Full analytic dataset. Includes all schools and schoolyears in",
      "the CPS data extract as of ", Sys.Date(), ". CPS data are merged with",
      "EJI value, region, implementation information, and inhaler utilization",
      "counts."
      )
  ) |> 
  
  write_rds(nested_here("data", "cps_schools_analytic.rds"))




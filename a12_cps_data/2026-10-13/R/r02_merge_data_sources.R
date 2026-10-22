


# -------------------------------------------------------------------------

# meta

# create school name-id crosswalk
list(
  "cps_school_characteristics.csv",
  "cps_school_waves.csv",
  "cps_school_eji.csv",
  "cps_school_regions.csv"
) |> 
  map(~{
    read_csv(nested_here("data", .x), show_col_types = FALSE)
  }) |>
  reduce(full_join, by = c("school_id")) |>
  pivot_longer(
    c(matches("school_match_name")),
    values_to        = "school_match_name",
    values_transform = as.character,
    values_drop_na   = TRUE
  ) |> 
  distinct(school_id, school_match_name) |>
  write_if_diff(nested_here("data", "cps_name_id_crosswalk.csv"))

# create school name-id-year crosswalk
merge(
  read_csv(nested_here("data", "cps_name_id_crosswalk.csv")),
  read_csv(nested_here("data", "cps_schoolyear_dates.csv")),
  by = NULL
  ) |> 
  distinct(school_id, school_match_name, data_schoolyear) |> 
  write_if_diff(nested_here("data", "cps_name_id_sy_crosswalk.csv"))

# match dates to waves and calculate time intervals
data.frame(
  wave = fct_inorder(
    c("Pilot", "Feb 2024", "April 2024", "Fall 2024"),
  )
) |> 
  
  mutate(
    impl_schoolyear = fct_collapse(
      wave,
      "2023-2024" = c("Pilot", "Feb 2024", "April 2024"),
      "2024-2025" = c("Fall 2024")
    )
  ) |> 
  
  # add implementation date based on wave and schoolyear dates
  left_join(
    read_csv(nested_here("data", "cps_schoolyear_dates.csv")),
    by = c("impl_schoolyear" = "data_schoolyear")
  ) |>
  
  mutate(
    impl_date      = case_when(
      wave == "Feb 2024" ~ as.Date("2024-02-01"),
      wave == "April 2024" ~ as.Date("2024-04-01"),
      .default = sy_start
    )
  ) |> 
  select(wave, matches("impl")) |> 
  
  # add data schoolyears and calculate time/dose variables
  merge(
    read_csv(nested_here("data", "cps_schoolyear_dates.csv")),
    by = NULL
  ) |> 
  
  # calculate dose/intensity variables
  arrange(wave, data_schoolyear) |> 
  mutate(
    data_date     = sy_end,
    demog_date    = sy_day20,
    sy_days       = as.numeric(difftime(sy_end, sy_start, units = "days")),
    exposure_days = case_when(
      data_date < impl_date ~ 0,
      impl_schoolyear == data_schoolyear ~ as.numeric(
        difftime(data_date, impl_date, units = "days")
      ),
      impl_date < data_date ~ sy_days
    ),
    
    exposure_frac      = round(exposure_days / sy_days, 1),
    
    exposure_stage        = factor(cumsum(case_when(
      exposure_days    == 0 ~ 0,
      .default = 1
    ))),
    .by = wave
  ) |> 
  select(wave, impl_schoolyear, data_schoolyear, exposure_frac, exposure_stage) |> 
  write_if_diff(nested_here("data", "cps_wave_sy_crosswalk.csv"))

  



# -------------------------------------------------------------------------

# unit:      student group
# timescale: annual

full_join(
  read_csv(nested_here("data", "cps_name_id_crosswalk.csv")),
  read_csv(nested_here("data", "cps_student_crosstabs.csv")),
  by = "school_match_name"
  ) |> 
  filter(!is.na(student_count)) |> 
  summarise(
    across(
      c(matches("count"), matches("days_per_row")), 
      ~ sum(.x, na.rm = TRUE)
      ),
    .by = c(
      school_id, data_schoolyear, days_per_student_expected,
      student_gender, student_race, asthma_status
      )
  ) |> 
  mutate(
    across(
      matches("count"), 
      ~ .x / student_count,
      .names = "{gsub('count', 'pct', .col)}"
      ),
    across(
      c("days_per_row_present", "days_per_row_absent"), 
      ~ .x / student_count,
      .names = "{gsub('per_row', 'per_student', .col)}"
      ),
    attd_rate = days_per_row_present / days_per_row_expected
    ) |> 
  select(-student_pct) |> 
  write_if_diff(nested_here("data", "mrg_t9_stugrp.rds"))



# -------------------------------------------------------------------------

# unit:      student group
# timescale: baseline

# isolate first observation per student group
read_rds(nested_here("data", "mrg_t9_stugrp.rds")) |> 
  arrange(school_id, data_schoolyear) |> 
  slice(1, .by = c(school_id, student_gender, student_race, asthma_status)) |> 
  select(
    school_id, student_gender, student_race, asthma_status, student_count,
    matches("pct"), matches("rate")
  ) |> 
  write_if_diff(nested_here("data", "mrg_t0_stugrp.rds"))





# -------------------------------------------------------------------------

# unit:      school
# timescale: annual

# tabulate events by school id and year
full_join(
  # all possible schoolyear/school combos
  read_csv(nested_here("data", "cps_name_id_sy_crosswalk.csv")),
  
  # match utilization to schoolyear
  fuzzyjoin::fuzzy_left_join(
    read_csv(nested_here("data", "cps_utilization_events.csv")),
    read_csv(nested_here("data", "cps_schoolyear_dates.csv")),
    by = list(
      x = c("event_date", "event_date"),
      y = c("sy_start", "next_sy_start")
    ), match_fun = list(`>=`, `<`)
  ),
  
  by = c("school_match_name", "data_schoolyear")
  ) |> 
  
  # count events per school, schoolyear
  summarise(
    n_events = sum(!is.na(event_date)), 
    .by      = c(school_id, data_schoolyear)
  ) |> 
  
  # count cumulative events per school, schoolyear
  arrange(school_id, data_schoolyear) |> 
  mutate(
    any_events     = as.numeric(n_events > 0),
    
    n_events_cum   = cumsum(n_events),
    any_events_cum = as.numeric(n_events_cum > 0),
    .by            = school_id
  ) |> 
  
  write_if_diff(nested_here("data", "cps_utilization_counts.csv"))
  

# sum student counts per school and schoolyear
full_join(
  read_csv(nested_here("data", "cps_name_id_crosswalk.csv")),
  read_csv(nested_here("data", "cps_student_crosstabs.csv")),
  by = "school_match_name"
) |> 
  filter(!is.na(student_count)) |> 
  mutate(
    student_count_row   = student_count,
    asthma_count        = student_count*(asthma_status == "Asthma Dx"),
    across(c(matches("count$"), matches("days_per_row")),
           list(school = ~ sum(.x, na.rm = TRUE))),
    .by                 = c(school_id, data_schoolyear)
  ) |> 
  pivot_longer(c(student_gender, student_race)) |>
  summarise(
    count = sum(student_count, na.rm = TRUE),
    .by   = c(school_id, data_schoolyear, name, value, ends_with("school"))
  ) |> 
  mutate(
    pct = count / sum(count), 
    .by = c(school_id, data_schoolyear, name)
  ) |> 
  arrange(
    school_id, data_schoolyear, name, desc(pct)
  ) |> 
  mutate(
    re_majority = case_when(
      name %in% c("asthma_status", "student_gender") ~ NA,
      max(pct) < 0.5 ~ "No Majority",
      .default = first(value)
    ),
    .by = c(school_id, data_schoolyear, name)
  ) |> 
  group_by(school_id, data_schoolyear) |> 
  fill(re_majority, .direction = "downup") |> 
  ungroup() |> 
  
  pivot_wider(
    names_from  = c(name, value),
    names_glue = "{name}.{snakecase::to_snake_case(value)}.{.value}_school",
    values_from = c(count),
    values_fill = 0,
    id_cols     = c(school_id, data_schoolyear, re_majority, 
                    student_count_school, ends_with("school"))
  ) |> 
  mutate(
    across(
      ends_with("count_school"),
      ~ .x / student_count_school,
      .names = ("{gsub('count', 'pct', .col)}")
    ),
    attd_rate_school = days_per_row_present_school / days_per_row_expected_school
  ) |>
  select(-student_pct_school) |> 
  write_if_diff(nested_here("data", "cps_school_annual_counts.csv"))

  

# combine annual data plus time vars
left_join(
  read_csv(nested_here("data", "cps_school_annual_counts.csv")),
  read_csv(nested_here("data", "cps_utilization_counts.csv")),
  by = c("school_id", "data_schoolyear")
  ) |>
  
  write_if_diff(nested_here("data", "mrg_t9_school.rds"))




# -------------------------------------------------------------------------


# unit:      school
# timescale: baseline

full_join(
  
  # isolate first observation per school
  read_rds(nested_here("data", "mrg_t9_school.rds")) |> 
    arrange(school_id, data_schoolyear) |> 
    slice(1, .by = c(school_id)) |> 
    select(school_id, student_count_school, matches("pct"), matches("rate")) |> 
    select(-matches("\\.")),
  
  # time-invariant chars
  list(
    "cps_school_characteristics.csv",
    "cps_school_waves.csv",
    "cps_school_eji.csv",
    "cps_school_regions.csv"
    ) |> 
    map(~{
      read_csv(nested_here("data", .x), show_col_types = FALSE) |> 
        select(-any_of("school_match_name"))
    }) |>
    reduce(full_join, by = c("school_id"))  |> 
    mutate(
      wave = factor(
        coalesce(wave, "Fall 2024"),
        levels = c("Pilot", "Feb 2024", "April 2024", "Fall 2024")
      )
      ) |> 
    distinct(),
  
  by = "school_id"
  ) |> 
  
  # limit to relevant
  filter(!is.na(student_count_school)) |> 
  write_if_diff(nested_here("data", "mrg_t0_school.rds"))



# -------------------------------------------------------------------------

  
  
  

# Merge -------------------------------------------------------------------


# Combine all data sources


# school-level
l2 <- left_join(
  full_join(
    read_rds(nested_here("data", "mrg_t9_school.rds")),
    read_rds(nested_here("data", "mrg_t0_school.rds")),
    by = c("school_id"),
    suffix = c("", ".t0")
  ),
  
  # time/exposure vars
  read_csv(nested_here("data", "cps_wave_sy_crosswalk.csv")),
  
  by = c("wave", "data_schoolyear")
)

# student group-level
l1 <- full_join(
  read_rds(nested_here("data", "mrg_t9_stugrp.rds")),
  read_rds(nested_here("data", "mrg_t0_stugrp.rds")),
  by     = c("school_id", "student_gender", "student_race", "asthma_status"),
  suffix = c("", ".t0")
)

l12 <- full_join(l2, l1, by = c("school_id", "data_schoolyear"))

# school-level vars, time-invariant
(x_school_t0 <- setdiff(
  names(l12),
  l12 |> 
    group_split(school_id) |> 
    map_dfr(~ janitor::remove_constant(.x)) |> 
    names()
))

# school-level vars, year-specific
(x_school_t9 <- setdiff(
  names(l12),
  l12 |> 
    group_split(school_id, data_schoolyear) |> 
    map_dfr(~ janitor::remove_constant(.x)) |> 
    names() |> 
    c(x_school_t0)
))

# group-level variables, time-invariant
(x_stugrp_t0 <- setdiff(
  names(l12),
  l12 |> 
    group_split(school_id, student_gender, student_race, asthma_status) |> 
    map_dfr(~ janitor::remove_constant(.x)) |> 
    names() |> 
    c(x_school_t0, x_school_t9)
))

# group-level variables, time-varying
(x_stugrp_t9 <- setdiff(
  names(l12), 
  c(x_school_t0, x_school_t9, x_stugrp_t0)
))

list(
  x_school_t0 = x_school_t0,
  x_school_t9 = x_school_t9, 
  x_stugrp_t0 = x_stugrp_t0,
  x_stugrp_t9 = x_stugrp_t9
) |> 
  write_if_diff(nested_here("data", "analytic_data_xlevels.rds"))

l12 |> 
  relocate(any_of(c(x_school_t0, x_school_t9, x_stugrp_t0, x_stugrp_t9))) |> 
  write_if_diff(nested_here("data", "analytic_data.rds"))

# 
# 
#   
#   
#   
# 
# 
# 
# 
# 
# # school-level data, annual
# list(
#   read_csv(nested_here("data", "cps_school_characteristics.csv")),
#   read_csv(nested_here("data", "cps_school_waves.csv",)),
#   read_csv(nested_here("data", "cps_school_eji.csv",)),
#   read_csv(nested_here("data", "cps_school_regions.csv")),
#   
#   
# )
# 
# 
# 
# 
# 
# 
# # 
# # 
# # source_data <- list(
# #   "cps_schoolyear_dates"       = c(unit = "meta",   timescale = "annual"),
# #   "cps_school_characteristics" = c(unit = "school", timescale = "baseline"),
# #   "cps_school_waves"           = c(unit = "school", timescale = "baseline"),
# #   "cps_school_eji"             = c(unit = "school", timescale = "baseline"),
# #   "cps_school_regions"         = c(unit = "school", timescale = "baseline"),
# #   "cps_utilization_events"     = c(unit = "school", timescale = "daily"),
# #   "cps_student_crosstabs"      = c(unit = "studentgroup", timescale = "annual")
# #   )
# # 
# # 
# # 
# # source_data$cps_name_id_crosswalk <- c(
# #   unit = "school", timescale = "baseline"
# #   )
# 
# 
# 
# 
# 
#   
#   # 
#   # 
#   # "cps_school_characteristics.csv",
#   # "cps_school_waves.csv",
#   # "cps_school_eji.csv",
#   # "cps_school_regions.csv"
#   # read_csv(nested_here("data", "cps_name_id_crosswalk.csv")),
#   # read_csv(nested_here("data", "cps_student_crosstabs.csv")),
#   # read_csv(nested_here("data", "cps_schoolyear_dates.csv")),
#   # 
#   # read_csv(nested_here("data", "cps_utilization_events.csv")),)
# 
# 
# # School-level, time baseline: -------------------------------------------
# #   - "cps_school_characteristics.csv"
# #   - "cps_school_eji.csv"
# #   - "cps_school_waves.csv"          
# 
# 
# 
# 
# 
# # 
# # # combine baseline (time-invariant) variables, 
# # schools_t0 <- list(
# #   "cps_school_characteristics.csv",
# #   "cps_school_waves.csv",
# #   "cps_school_eji.csv",
# #   "cps_school_regions.csv"
# #   ) |> 
# #   map(~{
# #     read_csv(nested_here("data", .x), show_col_types = FALSE)
# #   }) |>
# #   reduce(full_join, by = c("school_id")) 
# # 
# # # make school name-id crosswalk
# # schools_t0 |> 
# #   pivot_longer(
# #     c(matches("school_match_name")),
# #     values_to        = "school_match_name",
# #     values_transform = as.character,
# #     values_drop_na   = TRUE
# #   ) |> 
# #   distinct(school_id, school_match_name) |>
# #   write_if_diff(nested_here("data", "cps_name_id_crosswalk.csv"))
# # 
# # # read student counts; add ids
# # right_join(
# #   read_csv(nested_here("data", "cps_name_id_crosswalk.csv")),
# #   read_csv(nested_here("data", "cps_student_crosstabs.csv")),
# #   by = "school_match_name"
# #   ) |> 
# #   summarise(
# #     across(
# #       c(matches("count"), matches("days_per_row")), 
# #       ~ sum(.x)
# #       ),
# #     .by = c(school_id, data_schoolyear, days_per_student_expected)
# #     ) |> 
# #   arrange(school_id, data_schoolyear) 
# # 
# 
# 
# 
# 
# 
# 
# 
# # merge by id
# schools_t0 <- schools_t0 |> 
#   select(-matches("school_match_name")) |> 
#   group_by(school_id) |> 
#   fill(everything(), .direction = "downup") |> 
#   ungroup() |> 
#   
#   # # fill in wave where missing
#   # mutate(
#   #   wave = factor(
#   #     coalesce(wave, "Fall 2024"),
#   #     levels = c("Pilot", "Feb 2024", "April 2024", "Fall 2024")
#   #   ),
#   #   impl_schoolyear = fct_collapse(
#   #     wave,
#   #     "2023-2024" = c("Pilot", "Feb 2024", "April 2024"),
#   #     "2024-2025" = c("Fall 2024")
#   #   )
#   # ) |> 
#   # 
#   # # add implementation date based on wave and schoolyear dates
#   # left_join(
#   #   read_csv(nested_here("data", "cps_schoolyear_dates.csv")),
#   #   by = c("impl_schoolyear" = "data_schoolyear")
#   # ) |> 
#   # 
#   # mutate(
#   #   impl_date      = case_when(
#   #     wave == "Feb 2024" ~ as.Date("2024-02-01"),
#   #     wave == "April 2024" ~ as.Date("2024-04-01"),
#   #     .default = sy_start
#   #   )
#   # ) |> 
#   
#   select(
#     school_id, grade_level, region, 
#     starts_with("rpl_"), 
#     wave, starts_with("impl_")
#   ) |> 
#   distinct() |>
#   
#   mutate(
#     agg_level  = "school", 
#     time_frame = "baseline",
#     .before    = 1
#     ) |> 
#   write_if_diff(nested_here("data", "t0_school.rds")) |> 
#   write_if_diff(nested_here("data", "t0_school.csv"))
# 
# 
# 
# # School-level, time-varying: -----------------------------------------------
# #   - "cps_school_level_counts.csv" 
# #   - "cps_schoolyear_dates.csv"
# #   - "cps_utilization_events.csv"    
# # 
# # # tabulate events by school id and year
# # full_join(
# #   fuzzyjoin::fuzzy_left_join(
# #     read_csv(nested_here("data", "cps_utilization_events.csv")),
# #     read_csv(nested_here("data", "cps_schoolyear_dates.csv")),
# #     by = list(
# #       x = c("event_date", "event_date"),
# #       y = c("sy_start", "next_sy_start")
# #     ), match_fun = list(`>=`, `<`)
# #   ),
# #   merge(
# #     read_csv(nested_here("data", "cps_schoolyear_dates.csv")),
# #     read_csv(nested_here("data", "cps_name_id_crosswalk.csv")),
# #     by = NULL
# #   ) |> 
# #     distinct(school_id, school_match_name, data_schoolyear),
# #   
# #   by = c("school_match_name", "data_schoolyear")
# #   ) |> 
# #   
# #   summarise(
# #     n_events = sum(!is.na(event_date)), 
# #     .by      = c(school_id, data_schoolyear)
# #   ) |> 
# #   arrange(school_id, data_schoolyear) |> 
# #   mutate(
# #     any_events     = as.numeric(n_events > 0),
# #     
# #     n_events_cum   = cumsum(n_events),
# #     any_events_cum = as.numeric(n_events_cum > 0),
# #     .by            = school_id
# #   ) |> 
# #   
# #   mutate(
# #     agg_level  = "school", 
# #     time_frame = "annual",
# #     .before    = 1
# #   ) |>
# #   
#   write_if_diff(nested_here("data", "cps_utilization_counts.csv"))
# 
# 
# 
# 
# # in both schoolyear files, replace school name with id
# right_join(
#   read_csv(nested_here("data", "cps_name_id_crosswalk.csv")),
#   read_csv(nested_here("data", "cps_student_crosstabs.csv")),
#   by = "school_match_name"
# ) |> 
#   summarise(
#     across(c(matches("count"), matches("days_per_row")), ~ sum(.x)),
#     .by = c(school_id, data_schoolyear, 
#             student_gender, student_race, 
#             asthma_status, days_per_student_expected)
#   ) |> 
#   arrange(school_id, data_schoolyear) |> 
#   mutate(
#     across(
#       matches("count"), 
#       ~ .x / student_count,
#       .names = '{gsub("count", "pct", .col)}'
#     ),
#     row_attd    = (days_per_row_present / days_per_row_expected),
#     school_attd = sum(days_per_row_present) / sum(days_per_row_expected),
#     .by         = c(school_id, data_schoolyear)
#   ) |> 
#   
#   full_join(
#     read_csv(nested_here("data", "cps_utilization_counts.csv")),
#     by         = c("school_id", "data_schoolyear")
#     ) |> 
#   
#   write_rds(nested_here("data", "mrg_cps_annual.rds"))
# 
# 
# # add time variables to schools
# merge(
#   read_rds(nested_here("data", "mrg_cps_time_invar.rds")),
#   read_csv(nested_here("data", "cps_schoolyear_dates.csv")),
#   by = NULL
# ) |> 
#   
#   # calculate dose/intensity variables
#   arrange(school_id, data_schoolyear) |> 
#   mutate(
#     data_date    = sy_end,
#     demog_date   = sy_day20,
#     sy_days      = as.numeric(difftime(sy_end, sy_start, units = "days")),
#     sy_impl_days = case_when(
#       data_date < impl_date ~ 0,
#       impl_schoolyear == data_schoolyear ~ as.numeric(difftime(data_date, impl_date, units = "days")),
#       impl_date < data_date ~ sy_days
#     ),
#     
#     sy_impl_frac      = round(sy_impl_days / sy_days, 1),
#     impl_stage        = factor(cumsum(case_when(
#       sy_impl_days    == 0 ~ 0,
#       impl_schoolyear == data_schoolyear ~ 0,
#       .default = 1
#     ))),
#     .by    = school_id
#   ) |> 
#   
#   # add outcome counts etc
#   full_join(
#     read_rds(nested_here("data", "mrg_cps_annual.rds")),
#     by = c("school_id", "data_schoolyear")
#   ) |> 
#   
#   filter(!is.na(student_count)) |> 
#   arrange(school_id, data_schoolyear) |> 
#   
#   # calculate baseline
#   mutate(
#     school_attd_baseline = first(school_attd), 
#     .by = school_id
#   ) |> 
#   mutate(
#     row_attd_baseline     = first(row_attd),
#     row_attd_baseline_dev =  row_attd_baseline - school_attd_baseline,
#     .by                   = c(school_id, student_gender, student_race, asthma_status)
#     ) |> 
#   
#   write_if_diff(nested_here("data", "cps_schools_analytic.rds")) |> 
#   write_if_diff(
#     nested_here("data", "cps_schools_analytic.csv"),
#     comment = c(
#       "Full analytic dataset. Includes group counts for all schools and schoolyears in",
#       "the CPS data extract as of ", Sys.Date(), ". CPS data are merged with",
#       "EJI value, region, implementation information, and inhaler utilization",
#       "counts."
#     )
#   )
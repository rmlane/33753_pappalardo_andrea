

df_school <- df_school |> 
  left_join(
    read_rds(here::here("a12_cps_data/2026-07-08/data/", "mrg_school_chars.rds")),
    by = "school_id"
  ) |> 
  
  left_join(
    read_rds(here::here("a12_cps_data/2026-07-08/data/", "mrg_school_lvl_event_counts.rds")),
    by = c("school_id", "data_schoolyear")
  ) |> 
  
  left_join(
    readxl::read_xls(
      here::here(
        "a12_cps_data/data-raw", 
        "CPS_EJI_Excel.xls"
      ),
      na = c("", -999)
    ) |> 
      janitor::clean_names() |> 
      distinct(school_id, rpl_ser) |> 
      mutate(school_id = as.character(school_id)),
    by = "school_id"
  ) |> 
  
  left_join(
    read_rds(here::here("a12_cps_data/2026-07-08/data/",
                        "pre_school_year_dates.rds")) |> 
      filter(season == "spring") |> 
      distinct(data_schoolyear, end) |> 
      rename(sy_enddate = end),
    by = "data_schoolyear"
  )



df_school <- df_school |> 
  arrange(data_schoolyear) |> 
  mutate(
    data_schoolyear = ordered(data_schoolyear),
    across(where(is.character), fct_infreq),
    any_events_binary = as.numeric(any_events == "1+ Events"),
    
    impl_days = difftime(sy_enddate, impl_date, units = "days"),
    impl_days = unlist(map(impl_days, ~ max(.x, 0)))
  ) |> 
  mutate(across(
    ends_with("count"), 
    ~ (.x / student_count)*100,
    .names = "{gsub('count', 'pct', .col)}"
  )) |> 
  
  mutate(n_schools = "Overall")

anti_join(
  df_school,
  na.omit(df_school)
)

df_school <- df_school |> 
  mutate(
    data_schoolyear.lastyear = case_when(
      data_schoolyear == "2022-2023" ~ "2021-2022",
      data_schoolyear == "2023-2024" ~ "2022-2023",
      data_schoolyear == "2024-2025" ~ "2023-2024"
    )
  )

df_school <- left_join(
  df_school,
  df_school |> select(school_id, data_schoolyear, where(is.numeric)),
  by     = c("school_id", "data_schoolyear.lastyear" = "data_schoolyear"),
  suffix = c("", ".lastyear")
) |> 
  arrange(school_id, data_schoolyear)

write_rds(
  df_school,
  here::here(
    "a12_cps_data/2026-07-22/data", 
    "schools_analytic.rds"
  )
)




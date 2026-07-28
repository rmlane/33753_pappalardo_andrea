# 2026-07-28
# Process new CPS data extract; merge with previously processed variables

library(tidyverse)

df_raw <- openxlsx::read.xlsx(
  here::here(
    "a12_cps_data/data-raw", 
    "CPS SOW Data Pull 7-7-2026 Updated Gender.xlsx"
    ),
  sheet = "Updated SY22, 23, 24 and 25 Ast"
  ) 


df_mod <- df_raw |> 
  janitor::clean_names() |> 
  mutate(
    student_count = asthma_count,
    asthma_count  = student_count*(asthma_status == "Yes"),
    .after        = asthma_status
  )

df_school <- df_mod |> 
  summarise(
    attendance = weighted.mean(average_attendance, student_count),
    attendance_asthma = weighted.mean(
      average_attendance[which(asthma_status == "Yes")], 
      student_count[which(asthma_status == "Yes")]
      ),
    across(c(matches("count"), matches("student_days")), sum), 
    .by = c(student_current_school, enrollment_year)
    ) |> 
  
  mutate(
    days_per_stu_expected = 176,
    days_per_stu_present = (attendance / 100)*days_per_stu_expected,
    days_per_stu_absent = ((100-attendance) / 100)*days_per_stu_expected,
    days_per_school_expected = days_per_stu_expected*student_count,
    days_per_school_present = days_per_stu_present*student_count,
    days_per_school_absent = days_per_stu_absent*student_count,
    days_per_stu_present_asthma = (attendance_asthma / 100)*days_per_stu_expected,
    days_per_stu_absent_asthma = ((100-attendance_asthma) / 100)*days_per_stu_expected,
  )



# calculate majority race/ethnicity at school
df_school <- full_join(
  df_school,
  df_mod |> 
    summarise(
      count = sum(student_count),
      .by   = c(student_current_school, enrollment_year, student_race)
      ) |> 
    mutate(
      pct = count / sum(count), .by = c(student_current_school, enrollment_year)
      ) |> 
    arrange(student_current_school, enrollment_year, desc(pct)) |> 
    slice(1, .by = c(student_current_school, enrollment_year)) |> 
    
    mutate(
        re_majority = snakecase::to_title_case(case_when(
        pct >= 0.5 ~ student_race,
        .default = "NO MAJORITY"
        ))
      ) |> 
    select(student_current_school, enrollment_year, re_majority)  
)

# calculate gender distribution
df_school <- full_join(
  df_school,
  df_mod |> 
    summarise(
      pct_male      = sum(student_count*(tolower(student_gender) == "male")) / sum(student_count),
      pct_female    = sum(student_count*(tolower(student_gender) == "female")) / sum(student_count),
      pct_nonbinary = sum(student_count*(tolower(student_gender) == "non-binary")) / sum(student_count),
      .by           = c(student_current_school, enrollment_year)
      ) 
  )

# df_school <- full_join(
#   df_school,
#   df_mod |> 
#     summarise(
#       attendance = weighted.mean(average_attendance, student_count),
#       count = sum(student_count),
#       .by = c(student_current_school, enrollment_year, asthma_status)
#       ) |> 
#     
#     mutate(
#       asthma_status = fct_recode(
#         asthma_status, 
#         asthma = "Yes", 
#         no_asthma = "No"
#         )
#       ) |>   
#     pivot_wider(
#       names_from = asthma_status,
#       values_from = where(is.numeric)
#     ) |> 
#     mutate(across(matches("count"), ~ coalesce(.x, 0)))
# )


standardize_school_names <- function(vec) {
    v2 <- vec

    v2 <- gsub("0 Pilot", "", v2)
    v2 <- snakecase::to_snake_case(v2)
    v2 <- gsub("_", "", v2)

    case_when(
        v2 == "cant" ~ "canty",
        v2 == "kenwood" ~ "kenwoodhs",
        v2 == "lindbloom" ~ "lindblomhs",
        .default = v2
    )
}
df_school <- df_school |> 
  mutate(
    school_match_name = standardize_school_names(student_current_school),
    .before = 1
    ) |> 
  
  left_join(
    read_rds(here::here("a12_cps_data/2026-07-08/data/", "pre_school_ids.rds")) |> 
      distinct(school_match_name, school_id),
    by = "school_match_name"
  ) |> 
  relocate(starts_with("school")) |>
  rename(data_schoolyear = enrollment_year)




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
  arrange(school_match_name, data_schoolyear)

write_rds(
  df_school,
  here::here(
    "a12_cps_data/2026-07-22/data", 
    "schools_analytic.rds"
    )
)




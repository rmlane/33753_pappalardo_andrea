# crossover; treat group as random




schools |> 
  filter(!(data_schoolyear == "2021-2022")) |> 
  









# Prepare data for utilization analysis
util_mod_df <- full_join(
  
  # select time-varying chars
  schools |> 
    filter(!(data_schoolyear == "2021-2022")) |> 
    mutate(
      # event_rate        = (n_events / (student_count))*1000, # events per 1000 students per year
      # impl_student_days = impl_days*student_count, 
      # impl_event_rate   = (n_events / impl_student_days)*1000,
      male_pct          = student_pct.gender.male,
      impl_exposure = case_when(
        data_schoolyear == impl_schoolyear ~ 
          impl_days / days_per_student_expected,
        .default = impl_binary
      ),
      impl_years = floor(
        impl_days / (days_per_student_expected + 1)
      )
      
    ) |> 
    select(!matches("\\.")) |> 
    select(
      school_id,
      matches("day"),
      matches("data"),
      matches("impl"),
      matches("event"),
      student_count, asthma_count, asthma_pct, matches("pct")
    ) |>
    rename_with(~ paste0(.x, ".tv"), -c("school_id", "data_schoolyear")),
  
  # add baseline chars
  schools |> 
    arrange(school_id, data_schoolyear) |> 
    slice(1, .by = school_id) |> 
    rename(male_pct = student_pct.gender.male) |> 
    select(!matches("\\.")) |> 
    select(
      school_id, grade_level, region_name, 
      student_count, re_majority, wave, rpl_ser,
      matches("pct")
    ) |> 
    rename_with(~ paste0(.x, ".t0"), -c("school_id")) |> 
    mutate(rpl_ser.t0 = coalesce(rpl_ser.t0, 0.5)),
  
  by     = "school_id"
) |> 
  
  # add select yoy changes
  full_join(
    schools |> 
      mutate(across(
        all_of(c(
          "pct_days_per_row_present",
          "pct_days_per_row_absent",
          "asthma_pct",
          "food_allergy_pct",
          "non_food_allergy_pct",
          "student_504_pct",
          "iep_pct",
          "esl_pct",
          "frm_pct",
          "homelessness_pct"
        )),
        list(
          `2022-2023` = ~ case_when(
            data_schoolyear == "2021-2022" ~ (-1)*(.x),
            data_schoolyear == "2022-2023" ~ ( 1)*(.x),
            .default = 0
          ),
          `2023-2024` = ~ case_when(
            data_schoolyear == "2022-2023" ~ (-1)*(.x),
            data_schoolyear == "2023-2024" ~ ( 1)*(.x),
            .default = 0
          ),
          `2024-2025` = ~ case_when(
            data_schoolyear == "2023-2024" ~ (-1)*(.x),
            data_schoolyear == "2024-2025" ~ ( 1)*(.x),
            .default = 0
          )
        ),
        .names = "{.col}_abs_change.{.fn}"
      )) |> 
      summarise(
        across(matches("abs_change"), ~ sum(.x)),
        .by = c(school_id)
      ) |> 
      pivot_longer(-school_id) |> 
      separate(name, c("name", "data_schoolyear"), sep = "\\.") |> 
      pivot_wider(),
    
    by = c("school_id", "data_schoolyear")
  )











library(lmerTest)
summary(lmer(
  asthma_pct_abs_change ~ 
    factor(data_schoolyear) +
    
    
    
    impl_days.tv + 
    asthma_pct.t0 +
    (impl_days.tv | school_id),
  
  data = util_mod_df
  
))

# Develop poisson models for count/rate variables to estimate yoy change.

count_mods <- c(
  'asthma_count', 
  'food_allergy_count', 
  'non_food_allergy_count', 
  'student_504_count', 
  'iep_count', 
  'esl_count', 
  'frm_count', 
  'homelessness_count'
) |>
  set_names() |> 
  map(~{
    geepack::geeglm(
      arsenal::formulize(.x, "data_schoolyear + offset(log(student_count))"),
      family = poisson(link = "log"),
      data   = schools,
      id     = school_id,
      waves  = data_schoolyear,
      corstr = "exchangeable"
    ) 
  })

schooldays_mod <- geepack::geeglm(
  days_per_row_present ~ data_schoolyear + offset(log(days_per_row_expected)),
  family = poisson(link = "log"),
  data   = schools |> mutate(days_per_row_present = round(days_per_row_present)),
  id     = school_id,
  waves  = data_schoolyear,
  corstr = "exchangeable"
) 


# Reported inhaler uses per student-days of program enrollment
util_mod_df <- schools |> 
  select(
    school_id, data_schoolyear,
    student_count, impl_days, n_events
  ) |> 
  mutate(impl_student_days = impl_days*student_count, .after = 2) |> 
  filter(impl_days > 0) |> 
  arrange(data_schoolyear, school_id) |> 
  mutate(across(all_of(c("data_schoolyear", "school_id")), fct_inorder))

util_mod <- geepack::geeglm(
  n_events ~ data_schoolyear + offset(log(impl_student_days)),
  family = poisson(link = "log"),
  data   = util_mod_df,
  id     = school_id,
  waves  = data_schoolyear,
  corstr = "exchangeable"
)

yoy_change <- map(count_mods, ~{
  emmeans::emmeans(
    .x, 
    consec ~ data_schoolyear, 
    offset = log(1000), 
    type   = "response",
    infer  = TRUE
  ) |> 
    c(list(offset = "per 1000 students"))
})

yoy_change$attendance <- emmeans::emmeans(
  schooldays_mod, 
  consec ~ data_schoolyear, 
  offset = log(176), # per schoolyear per student
  type   = "response",
  infer  = TRUE
) |> 
  c(list(offset = "per student per schoolyear (176 days expected)"))

yoy_change$n_events <- emmeans::emmeans(
  util_mod, 
  consec ~ data_schoolyear, 
  offset = log(176*1000), # events per 1000 students per schoolyear
  type   = "response",
  infer  = TRUE
) |> 
  c(list(offset = "per 1000 students per schoolyear of program enrollment (176 days)"))



yoy_change <- transpose(yoy_change)

yearly_estimates <- map2_dfr(
  yoy_change$emmeans,
  yoy_change$offset,
  ~{output <- capture.output(.x)
  
  data.frame(.x) |> 
    mutate(
      rate_offset = .y,
      comments    = output[(which(str_length(output) == 0)+1):length(output)] |> 
        str_trim() |> 
        paste(collapse = "; ")
    ) 
  }, .id = "variable") |> 
  relocate(rate_offset, .after = variable) |> 
  select(-any_of(c("null", "t.ratio", "p.value")))|> 
  janitor::clean_names()


yoy_absolute <-  map2_dfr(
  yoy_change$emmeans,
  yoy_change$offset,
  ~{output <- capture.output(.x)
  
  emmeans::regrid(.x) |>
    emmeans::contrast("consec", infer = c(TRUE, FALSE)) |> 
    data.frame() |> 
    mutate(
      comments    = output[(which(str_length(output) == 0)+1):length(output)] |> 
        str_trim() |> 
        paste(collapse = "; ")
    ) |> 
    rename(abs_change = estimate)
  }, .id = "variable") |> 
  janitor::clean_names()

yoy_ratio <- yoy_change$contrasts |>
  map_dfr(~{
    output <- capture.output(.x)
    
    data.frame(.x) |> 
      mutate(
        comments = output[(which(str_length(output) == 0)+1):length(output)] |> 
          str_trim() |> 
          paste(collapse = "; ")
      )
  }, .id = "variable") |> 
  janitor::clean_names()

yoy_change <- list(yoy_absolute, yoy_ratio) |> 
  map(~{
    .x |> 
      mutate(
        data_schoolyear  = gsub("\\(([0-9\\-]+)\\) . \\(([0-9\\-]+)\\)", "\\1", contrast),
        change_since_schoolyear = gsub("\\(([0-9\\-]+)\\) . \\(([0-9\\-]+)\\)", "\\2", contrast),
        .keep = "unused",
        .after = 1
      )
  }) |> 
  
  reduce(
    full_join,
    by = c("variable", "data_schoolyear", "change_since_schoolyear"),
    suffix = c(".abs_change", ".ratio")
  )

full_join(
  yearly_estimates,
  yoy_change,
  by     = c("variable", "data_schoolyear"),
  suffix = c(".rate", ".change")
  ) |> 
  
  write_rds(nested_here("output", "annual_change.rds"))
  
  
  
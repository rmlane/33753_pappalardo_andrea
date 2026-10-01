tableby(
  wave ~ 
    notest(n_schools, cat.stats = "count", cat.simplify = TRUE) +
    + notest(impl_days, numeric.stats = "mean", numeric.simplify = TRUE, digits = 0) +
    
    + n_events 
  + n_events_cum
  + factor(any_events) 
  + factor(any_events_cum),
  
  strata = data_schoolyear,
  data    = schools |> filter(data_schoolyear %in% c("2023-2024", "2024-2025")),
  control = tableby.control(
    test           = FALSE,
    total          = FALSE,
    digits.n	     = NA,
    digits        = 1,
    numeric.stats  = c("Nmiss", "medianrange", "q1q3",  "meansd")
  )
) |> 
  summary(text = TRUE) |> 
  as.data.frame()|>
  write_rds(nested_here("output", "describe_waves.rds"))
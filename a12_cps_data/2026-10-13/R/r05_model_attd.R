m_primary <- glmer(
  
  cbind(attended_days,
        expected_days - attended_days) ~
    
    factor(year) +
    fractional_exposure +
    years_since_start +
    
    baseline_att_z +
    poverty_z +
    enrollment_z +
    school_level +
    
    (1 + time | school),
  
  family = binomial,
  data = dat
)
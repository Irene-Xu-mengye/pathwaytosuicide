library(effsize)

# Calculate hedges g for gtsummary ------------------
hedges_g <- function(data, variable, by,...) {
  g <- effsize::cohen.d(as.formula(glue::glue("{variable} ~ {by}")), paired = F, hedges.correction = T, data = data)$estimate
  return(round(g,2))
}


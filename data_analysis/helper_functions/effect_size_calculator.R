library(esc)

# This function calculates hedges' g ----------------------------------------
t_value <- function(data, variable, by, ...) {
  t <- t.test(data[[variable]] ~ as.factor(data[[by]]))[1]
  return(t$statistic)
}

get_n <- function(data, var, groupvar) {
  data$var <- unlist(data[, var])
  nrow(filter(data, !is.na(var) & group == groupvar))
}

hedges_g <- function(data, variable, by,...) {
  effsize <- esc_t(
    t = t_value(data, variable, by),
    grp1n = get_n(data, variable, "Attempters"),
    grp2n = get_n(data, variable, "Aborted/Interrupted Attempters"),
    es.type = "g"
  )
  effsize$es <- unname(effsize$es)
  return(round(effsize$es, 2))
}
# ----------------------------------------------------------------------------


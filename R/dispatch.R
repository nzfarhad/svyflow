# Map (kobo_type, aggregation_method) to the aggregator function that handles
# it. All aggregators share the signature
# `function(design, ques, disag, level, ms_options)` for dispatcher uniformity.
#
# Extending: to add a new integer aggregation, add a branch here AND extend
# .INT_METHODS in utils.R so validate_plan() accepts it.
pick_aggregator <- function(kobo_type, aggregation_method) {
  if (kobo_type == "select_one")      return(single_select_svy)
  if (kobo_type == "select_multiple") return(multi_select_svy)
  if (kobo_type == "integer") {
    return(switch(
      aggregation_method,
      mean   = stat_mean_svy,
      sum    = stat_sum_svy,
      median = function(d, q, dg, lv, ms) stat_quantile_svy(d, q, dg, lv, 0.50, "median",  ms),
      firstq = function(d, q, dg, lv, ms) stat_quantile_svy(d, q, dg, lv, 0.25, "1st_Qu",  ms),
      thirdq = function(d, q, dg, lv, ms) stat_quantile_svy(d, q, dg, lv, 0.75, "3rd_Qu",  ms),
      min    = stat_min_unweighted,
      max    = stat_max_unweighted,
      stop("unknown aggregation_method '", aggregation_method,
           "' for kobo_type 'integer'")
    ))
  }
  stop("unknown kobo_type '", kobo_type, "'")
}

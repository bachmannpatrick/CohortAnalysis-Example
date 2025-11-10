#' Create a Cohort Table Using Periodic (Month/Week) Transaction Data
#'
#' Builds a cohort table with rows as cohorts and columns as periods (months or ISO weeks).
#'
#' @param df A data.frame / data.table with event-level data.
#' @param id_var Unquoted ID column (customer/user id).
#' @param date   Unquoted date/datetime column.
#' @param period One of "month" or "week". Defaults to "month".
#' @param week_start Integer day for week start (1 = Monday ... 7 = Sunday). Used only when period = "week". Default 1.
#' @param metric Either "customers" (default) or "transactions".
#' @param fill Value to fill missing cells (use NA to match pivot_wider default). Default is NA_integer_.
#' @param keep_cohort_start If TRUE, keeps the original cohort start date in a column `cohort_start`.
#'
#' @return A data.table with cohorts as rows and period labels as columns.
#' @export
#' @examples
#' # Monthly cohorts (default)
#' f_BuildCohortTable(dataset, "CustomerID", "Date")
#'
#' # Weekly cohorts (ISO-week columns like 2025-W01)
#' f_BuildCohortTable(dataset, "CustomerID", "Date", period = "week")
#' #' # If you ever want distinct transactions instead of users:
#' cohort_table_period(online_cohorts, CustomerID, InvoiceDate, metric = "transactions")
f_BuildCohortTable <- function(df, id.var, date.var,
                                period = c("month", "week"),
                                metric = c("customers", "transactions"),
                                fill = NA_integer_,
                                keep_cohort_start = FALSE) {
  
  period <- match.arg(period)
  metric <- match.arg(metric)
  
  # Create new ID variable - of type "character"
  df[, (id.var):=as.character(get(id.var))]
  
  #make sure to use a data.table
  DT <- data.table::as.data.table(df)
  
  # Normalize columns & coerce to Date
  DT[, id := .subset2(.SD, id.var)]
  DT[, event_date := ymd(.subset2(.SD, date.var))]
  
  # Compute period start (month start or week start)
  if (period == "month") {
    DT[, period_start := lubridate::floor_date(event_date, unit = "month")]
    fmt_label <- function(d) format(d, "%Y-%m")
  } else {
    DT[, period_start := lubridate::floor_date(event_date, unit = "week", week_start = week_start)]
    # Use ISO week labels (e.g., 2025-W01)
    fmt_label <- function(d) strftime(d, format = "%G-W%V")
  }
  
  # Cohort is the first period the id appears in
  DT[, cohort_start := min(period_start), by = id]
  
  # Aggregate
  if (metric == "customers") {
        agg <- DT[, .(value = data.table::uniqueN(id)), by = .(cohort_start, period_start)]
  } else {
    agg <- DT[, .(value = .N), by = .(cohort_start, period_start)]
  }
  
  # Label columns for casting
  agg[, period_label := fmt_label(period_start)]
  
  # Wide table: cohorts x periods
  wide <- data.table::dcast(
    agg,
    cohort_start ~ period_label,
    value.var = "value",
    fill = fill
  )
  
  # Order by actual cohort_start
  data.table::setorder(wide, cohort_start)
  
  # Optionally keep the real cohort_start, but also provide sequential cohort index
  if (keep_cohort_start) {
    wide[, cohort := seq_len(.N)]
    data.table::setcolorder(wide, c("cohort", "cohort_start",
                                    setdiff(names(wide), c("cohort", "cohort_start"))))
  } else {
    # Match your original: replace cohort_start with simple 1..N
    data.table::setnames(wide, "cohort_start", "cohort")
    wide[, cohort := seq_len(.N)]
  }
  
  wide[]
}

#utils::globalVariables(c("id","event_date","period_start","cohort_start","period_label","users"))


#' Convert Values of a Cohort Table to Percentages
#'
#' Converts the numeric values of a cohort table to percentages
#' relative to each cohort’s initial value (the diagonal element).
#'
#' @param cohort_table A data.table or data.frame containing cohort values.
#'                     The first column must be named 'cohort'.
#' @param decimals Integer specifying number of decimal places to round to.
#'
#' @return A data.table with percentage values.
#' @export
#'
#' @examples
#' cohort_table <- cohort_table_period(online_cohorts, CustomerID, InvoiceDate)
#' cohort_table_pct(cohort_table, decimals = 1)
f_ConvertToPercentage <- function(cohort_table, decimals = 1) {
  
  # Ensure data.table
  DT <- data.table::as.data.table(cohort_table)
  
  # Extract diagonal (one per cohort)
  diag_vals <- diag(as.matrix(DT[, !"cohort"]))
  
  # Copy table and compute percentages
  DT_pct <- data.table::copy(DT)
  num_cols <- setdiff(names(DT_pct), "cohort")
  
  DT_pct[, (num_cols) := lapply(.SD, function(x, d = diag_vals) round(x / d * 100, decimals)),
         .SDcols = num_cols]
  
  # Reset cohort numbering (if desired)
  DT_pct[, cohort := .I]
  
  # Return as data.table (fast and compact)
  DT_pct[]
}


#' Left-shift a Cohort Table
#'
#' Shifts each cohort's values to the left (removing NAs and filling with zeros).
#' Useful for aligning retention curves where the first period always starts at t0.
#'
#' @param cohort_table A data.table or data.frame where the first column is 'cohort'
#'                     and subsequent columns represent sequential periods.
#' @return A data.table with left-shifted values and renamed columns (t0, t1, ...).
#' @export
#'
#' @examples
#' cohort_table <- cohort_table_period(online_cohorts, CustomerID, InvoiceDate)
#' shift_left(cohort_table)
f_shiftLeft <- function(cohort_table) {
  
  DT <- data.table::as.data.table(cohort_table)
  n_cols <- ncol(DT)
  num_cols <- setdiff(names(DT), "cohort")
  
  # Convert to matrix (faster for row operations)
  M <- as.matrix(DT[, ..num_cols])
  
  # For each row: remove NAs, left-shift, and fill trailing positions with 0
  M_shifted <- t(apply(M, 1, function(row) {
    vals <- row[!is.na(row)]
    c(vals, rep(0, n_cols - 1 - length(vals)))
  }))
  
  # Recombine with cohort column
  DT_shifted <- data.table::data.table(cohort = DT$cohort, M_shifted)
  
  # Rename time columns as t0, t1, t2, ...
  data.table::setnames(DT_shifted,
                       old = names(DT_shifted)[-1],
                       new = paste0("t", 0:(n_cols - 2)))
  
  DT_shifted[]
}








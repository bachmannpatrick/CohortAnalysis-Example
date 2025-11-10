# STEP 0 — # Load required packages and functions ----
  library(data.table)
  library(lubridate)
  library(ggplot2)

  # source auxilary functions
  source("functions/f_BuildCohorts.r")
  source("functions/f_CohortTableFunctions.r")

  #set a seed
  set.seed(42)

# STEP 1 — Prepare the data ----
  # Load transaction data  
  #mydata <- fread("data/Gift-SAMPLE.csv")
  mydata <- fread("data/cohort_example_dataset.csv")


  # fix column names
  names(mydata) <- c("Id", "SighupMonth","Date", "Price")
  # fix Date (using lubridate)
  mydata[, Date:=ymd(Date)]
  # fix format of Id variable
  mydata[, Id:=as.character(Id)]
  # order the data
  setkey(mydata, "Id", "Date")

  # build monthly cohorts
  temp <- f_BuildCohorts(mydata, min(mydata$Date), max(mydata$Date),option = "month", id.var = "Id", date.var = "Date")
  mydata <- temp$Data[, list(Id, Date, Price, Cohort)]
  
  # How many customers per cohort?
  table(temp$CustomerCohorts$Cohort)
  
  # What cohorts are available?
  unique(temp$CustomerCohorts[, list(Cohort.date, Cohort)])
    

  # Get the first purchase and customer "age"
  mydata[, first_purchase:=min(Date), by=Id]
  mydata[, cohort := format(first_purchase, "%Y-%m")]
  mydata[, age := interval(floor_date(first_purchase, "month"), floor_date(Date, "month")) %/% months(1)]

  # Active if any purchase in a cohort-month:
  retention_counts <- unique(mydata[, .(Id, cohort, age)])[ , .(retained = .N), by = .(cohort, age)]
  cohort_size <- unique(mydata[, .(Id, cohort)])[ , .(n_cohort = .N), by = cohort]
  retention <- cohort_size[retention_counts, on = "cohort"][, retention_rate := retained / n_cohort]

  # Revenue per cohort-month and Average Revenue Per User (ARPU):
  revenue <- mydata[, .(revenue = sum(Price)), by = .(cohort, age)]
  revenue <- cohort_size[revenue, on = "cohort"][, arpu := revenue / n_cohort]

# STEP 2 — create a monthly cohort table ----
  month_table <- f_BuildCohortTable(mydata, id.var = "Id", date.var = "Date", period = c("month"), metric = "customers")

# STEP 3 - create a monthly cohort table with percentages ----
  percentage_table <- f_ConvertToPercentage(month_table)
  
# STEP 4 - shift the monthly cohort table for better plotting ----
  percentage_table <- f_shiftLeft(percentage_table)
  
# STEP 5 - create a line plot ----
  # Convert to a long table
  DT_long <- melt(
    percentage_table,
    id.vars = "cohort",          # keep cohort as grouping variable
    variable.name = "period_label",  # keeps t0, t1, t2, ...
    value.name = "value"             # numeric values
  )
  
  # Plot using ggplot (line plot)
  ggplot(DT_long, aes(period_label, value, colour = factor(cohort), group = cohort)) +
    geom_line(size = 1) +
    geom_point(size = 1) +
    labs(x = "Time", y = "% customers") + 
    theme_minimal()

# STEP 6 - create a heatmap ----
  
  ggplot(DT_long, aes(period_label, reorder(cohort, -order(cohort)))) +
    geom_raster(aes(fill = log(value))) +
    coord_equal(ratio = 1) +
    geom_text(aes(label = glue::glue("{round(value,0)}%")), 
              size  = 3, 
              colour = "snow") +
    scale_fill_gradient(guide = "none") +
    labs(x = "Time", y = "Cohort") + 
    theme_minimal(base_size = 16) +
    theme(panel.grid   = element_blank(),
          panel.border = element_blank())
   
  
  
  
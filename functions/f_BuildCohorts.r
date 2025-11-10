#' Build cohorts
#' 
#Description
#'Groups customers into cohorts.
#
#Arguments
#' @param  data dataset containing the transactional data. The dataset must contain a column labeled  "Id" that allows unique customer identification and a column labeled "Date", indicating the purchase date.
#' @param  start.date start date of period where cohorts are created.
#' @param  stop.date end date of period where cohorts are created.
#' @param  option intervall for cohorts. Options: monthly cohorts ("month"), weekly cohorts ("week") or yearly cohorts("year").
#' @param  id.var variable name of the customer idientifier.
#' @param  date.var variable name of the time record.
# 

#' @details
#' 
#'  \code{data} dataset containing the transactional data. The dataset must contain a column labeled  "Id" that allows unique customer identification and a column labeled "Date", indicating the purchase date. "Price" and "Purchased Items" are optional, but note that ???Price??? is required if you plan to calculate a CLV later on. "Price" specifies the price of the transaction, "Purchased Items" is the number of items purchased in this particular transaction.
#'  
#'  \code{start.date} must be formated as date. It defines the beginning of the period where cohorts are created.
#'
#'  \code{stop.date} must be formated as date. It defines the end of the period where cohorts are created.
#'
#'  \code{option} defines the intervall for cohort creation. Possilbe options are \code{"week", "month", "year"}. By default, cohorts are created on a monthly basis, \code{option="month"}.
#' 


# Return Values
#'@return
#'  Returns a list
#'  \describe{
#'          \item{$Data}{includes the data provided, the start date of each cohort and the cohort number.}
#'           \item{ $CustomerCohorts}{consists of a customer list with the corresponding cohort and start date of this cohort.}
#'  }

f_BuildCohorts <- function(data, start.date, stop.date, option="month", id.var="Id",date.var="Date"){
  
  # Ensure that required packages are loaded
  require(data.table)
  
  # Make sure data is a data.table format
  data <- data.table(data)
  
#   # Check if date variables are all formatted correctly
#   if(inherits(data[, get(date.var)], "Date")==FALSE){
#     warning("Wrong format for *date.var* parameter!")
#   }
  
  # if(inherits(start.date, "Date")==FALSE){
  #     warning("Wrong format for *start.date* parameter!")
  # }
  # if(inherits(stop.date, "Date")==FALSE){
  #   warning("Wrong format for *stop.date* parameter!")
  # }
  
  # # Check plausibility of start.date and stop.date
  # # Start.date
  # if(start.date < min(data[, get(date.var)])){
  #   warning("Parameter *start.date* is earlier than observed dates!")
  # }
  
  # if(start.date > max(data[, get(date.var)])){
  #   warning("Parameter *start.date* is later than observed dates!")
  # }

  # # Stop.date
  # if(stop.date < min(data[, get(date.var)])){
  #   warning("Parameter *stop.date* is later than observed dates!")
  # }
  
  # if(stop.date > max(data[, get(date.var)])){
  #   warning("Parameter *stop.date* is later than observed dates!")
  # }
  
  
  # Define date format
  if(option=="month"){
    date.format <-"%Y-%m"
  } else if(option=="week"){
    date.format <- "%Y-%W"
  } else if(option=="day"){
    date.format <- "%Y-%m-%d"
  } else if(option=="year"){
    date.format <- "%Y"
  } else {warning("Wrong input for *option* parameter!")}
  
  
  # Function to create monthly intervals
  create_monthly_intervals <- function(start_date, end_date, date.format) {
    # Convert input strings to Date objects
    start_date <- as.Date(start_date)
    end_date <- as.Date(end_date)
    
    # Initialize vector to store intervals
    intervals <- c()
    
    # Loop through each month
    current_date <- start_date
    while (current_date <= end_date) {
      # Calculate the end of the current month
      end_of_month <- ceiling_date(current_date, "month") - 1
      
      # Append interval to the vector
      intervals <- c(intervals, format(end_of_month, format=date.format))
      
      # Move to the next month
      current_date <- end_of_month + 1
    }
    
    return(intervals)
  }
  
  # Create monthly intervals of the calibration period
  intervals <- create_monthly_intervals(start.date, stop.date, date.format)
  #intervals <- format(seq(start.date, stop.date, by=option), format=date.format)
  intervals <- data.table(Date=intervals,  Cohort=1:length(intervals))
  
  
  # Create new ID variable - of type "character"
  data[, (id.var):=as.character(get(id.var))]
  
  # Find all first purchases
  first.purchases <- data[, list(Date=min(get(date.var))), by=id.var]

  # Format dates in first.purchase 
  first.purchases$Date  <- format(first.purchases$Date, format=date.format)
  
  # Assign the cohort numbers according the interval
  first.purchases.reduced <- merge(first.purchases, intervals, by="Date")


  # Rename date column
  setnames(first.purchases.reduced, "Date", "Cohort.date")
  
  # Merge cohorts into transactional data
  data  <- merge(data, first.purchases.reduced, by = id.var)

  # set Id-Variable name
  #colnames(first.purchases.reduced)[which(names(first.purchases.reduced) == "id_tmp")] <- id.var
  #setnames(first.purchases.reduced, "id_tmp", id.var)
  
  # Remove temporary ID variable
  #data[ , "id_tmp":=NULL]
  
 return(list("Data"=data[order(data$Cohort),], "CustomerCohorts"=first.purchases.reduced))
}

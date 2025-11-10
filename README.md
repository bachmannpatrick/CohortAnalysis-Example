# CohortAnalysis-Example
In this example, you can analyze customer retention over time using cohort analysis. 
A 'cohort' is a group of customers who share a common characteristic - here, their signup month. 
The goal is to calculate retention rates for each cohort and visualize them in a heatmap.

#Dataset Description
The dataset ('cohort_exercise_dataset.csv') contains:
- CustomerID: Unique customer identifier
- SignupMonth: The month the customer first signed up
- Month: Observation month for activity
- Spending: Amount spent in that month (0 means inactive)



#Available Auxiliary Functions
For detailed documentation see the function file.
- f_BuildCohorts(): Groups customers into cohorts.
- f_BuildCohortTable(): Create a cohort table using periodic (month/week) transaction data
- f_ConvertToPercentage: Convert values of a cohort table to percentages
- f_shiftLeft: Left-shift a cohort table
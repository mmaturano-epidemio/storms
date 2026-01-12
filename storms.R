# Assignment
# The basic goal of this assignment is to explore the NOAA Storm Database 
# and answer some basic questions about severe weather events. 
# You must use the database to answer the questions below and show the code 
# for your entire analysis. Your analysis can consist of tables, figures, 
# or other summaries. You may use any R package you want to support your analysis.
# 
# Questions
# Your data analysis must address the following questions:
# Across the United States, which types of events (as indicated in the
# EVTYPE variable) are most harmful with respect to population health?
#   
# Across the United States, which types of events have the greatest 
# economic consequences?
#   
# Consider writing your report as if it were to be read by a government
# or municipal manager who might be responsible for preparing for severe 
# weather events and will need to prioritize resources for different types of events.
# However, there is no need to make any specific recommendations in your report.
# 
# Requirements
# For this assignment you will need some specific tools
# 
# RStudio: You will need RStudio to publish your completed analysis document to RPubs.
# You can also use RStudio to edit/write your analysis.
# 
# # knitr: You will need the knitr package in order to compile your R Markdown 
# document and convert it to HTML

# ===========================================================================
# ---- Get files and libraries ----
# First we get the package manager and libraries we'll need
if(!require(pacman)){
  install.packages("pacman")
}
pacman::p_load(data.table, ggplot2, lubridate)

# Now we download the data if needed, and read the file
if(!file.exists("FStormData.csv")){
  download.file(url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
                destfile = file.path(getwd(), "FStormData.csv"))
}

storms <- fread("FStormData.csv", na.strings = "") |> janitor::clean_names()

# ---- Data exploration ----
head(storms)
storms |> skimr::skim()

# This data needs transformation
dates <- names(storms)[names(storms) %like% "date"] 
times <- names(storms)[names(storms) %like% "time"]
tidy_storms <- copy(storms)
tidy_storms[, bgn_datetime := lubridate::mdy_hm(paste(bgn_date, bgn_time))]
tidy_storms[, end_datetime := lubridate::mdy_hm(paste(end_date, end_time))]
tidy_storms[is.na(end_time),.N / tidy_storms[,.N]]

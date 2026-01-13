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
pacman::p_load(data.table, 
               ggplot2, 
               lubridate,
               flextable,
               skimr,
               stringr)

# Now we download the data if needed, and read the file
if(!file.exists("FStormData.csv")){
  download.file(url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
                destfile = file.path(getwd(), "FStormData.csv"))
}

storms <- fread("FStormData.csv", na.strings = "") |> janitor::clean_names()

# ---- Data cleaning ----
dates <- names(storms)[names(storms) %like% "date"] 
times <- names(storms)[names(storms) %like% "_time"]
tidy_storms <- copy(storms)

tidy_storms[, (dates) := lapply(.SD, sub, pattern = " .*$", replacement = ""), .SDcols = dates]
tidy_storms[, (dates) := lapply(.SD, mdy), .SDcols = dates]
tidy_storms[, colMeans(is.na(.SD)) |> round(2), .SDcols = (dates)] # NA proportion
tidy_storms[, tail(.SD, 10), .SDcols = (dates)] # Last 10 rows

# Times have different formats:
set.seed(42)
tidy_storms[!is.na(end_time), unique(end_time)] |> sample(size = 7)

# We won't use them so we won't clean them
# We select just the cols we will use:
tidy_storms <- tidy_storms[, .(bgn_date, end_date, state = as.factor(state), 
                           state_2 = as.factor(state_2), county = as.factor(county),
                           countyname, evtype, fatalities,
                           injuries, propdmg, propdmgexp, cropdmg, cropdmgexp)]

# Acording to NOAA, the types of events are:
# Astronomical Low Tide, Avalanche, Blizzard, Coastal Flood,
# Cold/Wind Chill, Debris Flow, Dense Fog, Dense Smoke, Drought,
# Dust Devil, Dust Storm, Excessive Heat, 
# Extreme Cold/Wind Chill, Flash Flood, Flood, Freezing Fog,
# Frost/Freeze, Funnel Cloud, Hail, Heat, Heavy Rain, Heavy Snow,
# High Surf, High Wind, Hurricane/Typhoon, Ice Storm, 
# Lakeshore Flood, Lake-Effect Snow, Lightning, Marine Hail,
# Marine High Wind, Marine Strong Wind, Marine Thunderstorm Wind,
# Rip Current, Seiche, Sleet, Storm Tidem, Strong Wind, Thunderstorm Wind,
# Tornado, Tropical Depression, Tropical Storm, Tsunami, 
# Volcanic Ash, Waterspout, Wildfire, Winter Storm, Winter Weather
tidy_storms[, evtype := stringr::str_to_title(evtype)]
tidy_storms[, unique(evtype)] |> sample(20)
tidy_storms[, clean_event := fcase(evtype %like% "Tornado", Tornado,
                                   default = evtype)]

# ---- Human loses ----
tidy_storms[, .(fatalities, injuries), evtype]

# Función rápida para mapear exponentes
map_exp <- function(x) {
  x <- toupper(x)
  ifelse(x == "K", 1000,
         ifelse(x == "M", 1000000,
                ifelse(x == "B", 1000000000, 
                       ifelse(x %in% c("0":"8"), 10^as.numeric(x), 1))))
}

# Crear columnas de daño real
tidy_storms[, prop_total := propdmg * map_exp(propdmgexp)]
tidy_storms[, crop_total := cropdmg * map_exp(cropdmgexp)]
tidy_storms[, economic_loss := prop_total + crop_total]
# ==============================================================================

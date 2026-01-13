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

storms <- fread("FStormData.csv", 
                na.strings = "", 
                showProgress = TRUE, 
                verbose = TRUE) |> janitor::clean_names()

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
# Astronomical High Tide, Astronomical Low Tide, Avalanche, Blizzard, 
# Coastal Flood, Cold/Wind Chill, Debris Flow, Dense Fog, Dense Smoke, Drought,
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
tidy_storms[, unique(evtype)] |> sort()
tidy_storms <- tidy_storms[!clean_event %like% "(?i)Summary"]
tidy_storms[, clean_event := fcase(
  evtype %like% "(?i)Flash Flood", "Flash Flood", # 
  evtype %like% "(?i)Light|Ligh?tning|Ligntning", "Lightning", #
  evtype %like% "(?i) Devil|Devel", "Dust Devil", #
  evtype %like% "(?i)Dust Storm", "Dust Storm", #
  evtype %like% "(?i)Lakeshore Flood", "Lakeshore Flood", # 
  evtype %like% "(?i)Funnel Cloud", "Funnel Cloud", #
  evtype %like% "(?i)Coastal Flood|Cstl Flood|Tidal Flood", "Coastal Flood", #
  evtype %like% "(?i)Lake Effect Snow", "Lake-Effect Snow", #
  evtype %like% "(?i)Marine (Thunderstorm|Tstm)", "Marine Thunderstorm Wind", #
  evtype %like% "(?i)Winter Storm", "Winter Storm", #
  evtype %like% "(?i)Winter Weather", "Winter Weather", #
  evtype %like% "(?i)Tornado|Torn?da?o|Gustnado|Landspout", "Tornado", # 
  evtype %like% "(?i)Ice Storm", "Ice Storm", #
  evtype %like% "(?i)Thunderstorm|Tstm|Storm Force|Burst|Thund?e?e?r?sto?r?m", "Thunderstorm Wind",
  evtype %like% "(?i)Flood|Fld|Stream", "Flood", # 
  evtype %like% "(?i)Surf|High Tide|High Water|Swells|Waves", "High Surf", # 
  evtype %like% "(?i)Hurricane|Typhoon", "Hurricane/Typhoon", #
  evtype %like% "(?i)Marine Hail", "Marine Hail", # 
  evtype %like% "(?i)Hail", "Hail", #
  evtype %like% "(?i)Excessive Heat", "Excessive Heat", #
  evtype %like% "(?i)Heat|Warm|Hot", "Heat", # 
  evtype %like% "(?i)Extreme Cold|Extreme Wind Chill", "Extreme Cold/Wind Chill", # 
  evtype %like% "(?i)Cold|Wind Chill|Hypothermia", "Cold/Wind Chill", #
  evtype %like% "(?i)Tropical Storm", "Tropical Storm", #
  evtype %like% "(?i)Tropical Depression", "Tropical Depression", #
  evtype %like% "(?i)Tsunami", "Tsunami", #
  evtype %like% "(?i)Drought", "Drought", #
  evtype %like% "(?i)Smoke", "Dense Smoke", #
  evtype %like% "(?i)Avalanche", "Avalanche", #
  evtype %like% "(?i)Debris", "Debris Flow", #
  evtype %like% "(?i)Current", "Rip Current",
  evtype %like% "(?i)Astronomical High", "Astronomical High Tide",
  evtype %like% "(?i)Astronomical Low", "Astronomical Low Tide",
  evtype %like% "(?i)Seiche", "Seiche",
  evtype %like% "(?i)Sleet", "Sleet",
  evtype %like% "(?i)Sleet", "Sleet",
  evtype %like% "(?i)Tidem", "Storm Tidem",
  evtype %like% "(?i)Volcanic Ash", "Volcanic Ash",# 
  evtype %like% "(?i)High Wind", "High Wind", #
  evtype %like% "(?i)Marine Strong Wind", "Marine Strong Wind", #
  evtype %like% "(?i)Marine High Wind", "Marine High Wind", #
  evtype %like% "(?i)Strong Wind|Gusty", "Strong Wind",
  evtype %like% "(?i)Snow", "Heavy Snow", #
  evtype %like% "(?i)Rain|Shower|Precip", "Heavy Rain", #
  evtype %like% "(?i)Wildfire|Fire|Forest Fire", "Wildfire", #
  evtype %like% "(?i)Freezing Fog", "Freezing Fog", #
  evtype %like% "(?i)Fog", "Dense Fog", #
  evtype %like% "(?i)Waterspout", "Waterspout", #
  evtype %like% "(?i)Blizzard", "Blizzard", #
  evtype %like% "(?i)Frost|Freeze", "Frost/Freeze", #
    default = evtype
)]
tidy_storms[, unique(clean_event)]
check_impacto <- tidy_storms[clean_event == evtype, 
                             .(Fatalidades = sum(fatalities), N = .N), 
                             by = evtype][order(-Fatalidades)]
head(check_impacto, 50)
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

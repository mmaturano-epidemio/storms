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
# if(!file.exists("FStormData.csv")){
#   download.file(url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
#                 destfile = file.path(getwd(), "FStormData.csv"))
# storms <- fread("FStormData.csv", 
#                 na.strings = "", 
#                 showProgress = TRUE, 
#                 verbose = TRUE) |> janitor::clean_names()
storms <- readRDS("FStormData.RDS")

# ---- Data cleaning ----
str(storms)

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
tidy_storms <- tidy_storms[!evtype %like% "(?i)Summary"]
tidy_storms[, clean_event := fcase(
  evtype %like% "(?i)Flash Flood", "Flash Flood", # 
  evtype %like% "(?i)Ligh?tning|Ligntning", "Lightning", #
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
  evtype %like% "(?i)Landslide|Mud ?slide|Debris|Rock Slide", "Debris Flow",
  evtype %like% "(?i)Storm Surge|Coastal ?storm", "Storm Surge/Tide",
  evtype %like% "(?i)Extreme Windchill|Low Temp|Hypothermia|Exposure", "Extreme Cold/Wind Chill",
  evtype %like% "(?i)Glaze|Ice (On Road|Roads)|Black Ice|Freezing (Drizzle|Spray)|Wintry Mix", "Winter Weather",
  evtype %like% "(?i)^Winds?$", "Strong Wind",
  evtype %like% "(?i)Thundertorm|Tunderstorm", "Thunderstorm Wind",
  evtype %like% "(?i)Rough Seas|High Seas|Heavy Seas|Marine (Mishap|Accident)|Drowning", "High Surf",
  evtype %like% "(?i)Avalance", "Avalanche",
  evtype %like% "(?i)Whirlwind", "Tornado",
  evtype %like% "(?i)Rapidly Rising Water", "Flash Flood",
  evtype %like% "(?i)Other|Summary|\\?|None", "Other",
    default = evtype
)]
tidy_storms[, uniqueN(clean_event)]
tidy_storms[clean_event == evtype,
            .(fatalities = sum(fatalities), N = .N),
            by = clean_event][order(-fatalities)][fatalities > 0]

map_exp <- function(x) {
  x <- toupper(as.character(x))
  fcase(
    x == "H", 1e2,
    x == "K", 1e3,
    x == "M", 1e6,
    x == "B", 1e9,
    x %in% c("", "-", "?", "+"), 1,
    x %in% as.character(0:8), 10, 
    default = 1
  )
}

tidy_storms[, prop_total := propdmg * map_exp(propdmgexp)]
tidy_storms[, crop_total := cropdmg * map_exp(cropdmgexp)]
tidy_storms[, economic_loss := prop_total + crop_total]
tidy_storms[, year := year(bgn_date)]
tidy_storms[, uniqueN(evtype), year]
tidy_storms[year%in%c(seq(1950, 1990, 5)), unique(evtype), year]
# We can see that most years on the dataset only consider three events:
# Hail, tornado, and Tstm Wind. On the one hand, including all these years
# Could heavily bias the analysis in favour of these events, simple for 
# More data being available on them. On the other, eliminating them means losing
# The majority of the registered years. We will firs eliminate them, and then
# Use them on a sensitivity analysis. 
clean_storms <- tidy_storms[year > 1992]
# ==============================================================================
# ---- Data Analysis ----
saveRDS(object = clean_storms, file = file.path(getwd(), "clean_storms.rds"))
clean_storms <- readRDS("clean_storms.rds")
# ---- Human loses ----
total_fatalities <- clean_storms[, sum(fatalities)]
total_injuries <- clean_storms[, sum(injuries)]
clean_storms[, .(fatalities = sum(fatalities),
                 fatalities_pct = round(sum(fatalities) / total_fatalities * 100, 2),
                 injuries = sum(injuries),
                 injuries_pct = round(sum(injuries) / total_injuries * 100, 2)), 
             evtype][order(-fatalities)][
                 !(injuries == 0 & fatalities == 0)][1:20]


# ---- Economic loses ----
total_loss <- clean_storms[, sum(economic_loss)]
clean_storms[, .(economic_loss = sum(economic_loss, na.rm = TRUE), 
                 economic_loss_pct = round(sum(economic_loss, na.rm = TRUE) / total_loss *100, 2),
                 total_events = .N), 
            by = clean_event][order(-economic_loss)][economic_loss > 0]

clean_storms[, unique(state)]
clean_storms[, unique(state_2)]

# ---- Total loses ----
# Definir valores de monetización
vsl_value <- 10e6    # 10 millones USD
injury_value <- 5e5  # 500 mil USD

# Calcular Carga de Salud y Carga Total
clean_storms[, health_loss := (fatalities * vsl_value) + (injuries * injury_value)]
clean_storms[, total_burden := economic_loss + health_loss]

# Top 10 para el Manager (Tabla resumen)
top_10_events <- clean_storms[, .(
  total_burden_B = sum(total_burden) / 1e9,
  economic_loss_B = sum(economic_loss) / 1e9,
  health_loss_B = sum(health_loss) / 1e9,
  fatalities = sum(fatalities),
  total_events = .N
), by = clean_event][order(-total_burden_B)][1:10]

# Ver tabla
flextable::flextable(top_10_events) |> 
  flextable::colformat_double(digits = 2)

# ---- By region ----
northeast <- c("CT", "ME", "MA", "NH", "RI", "VT", "NJ", "NY", "PA")
midwest   <- c("IL", "IN", "MI", "OH", "WI", "IA", "KS", "MN", "MO", "NE", "ND", "SD")
south     <- c("DE", "FL", "GA", "MD", "NC", "SC", "VA", "DC", "WV", "AL", "KY", "MS", "TN", "AR", "LA", "OK", "TX")
west      <- c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY", "AK", "CA", "HI", "OR", "WA")

clean_storms[, region := fcase(
  state_2 %in% northeast, "Northeast",
  state_2 %in% midwest,   "Midwest",
  state_2 %in% south,     "South",
  state_2 %in% west,      "West",
  state_2 %in% c("PR", "GU", "AS", "VI", "MH", "AM"), "Territories",
  default = "Marine/Other"
)]


# Preparamos datos para el heatmap
heatmap_data <- clean_storms[, .(burden = sum(total_burden)), by = .(year, clean_event, region)]

# Filtramos solo el Top 15 de eventos globales para que el eje Y no sea eterno
top_15_names <- clean_storms[, .(t = sum(total_burden)), by = clean_event][order(-t)][1:15, clean_event]

# 1. Definir las dimensiones de la cuadrícula completa
years_vec <- unique(clean_storms$year)
regions_vec <- unique(clean_storms[region != "Other/Marine", region])

# Recordamos tu lista del top 15 (asegúrate de tenerla definida)
# top_15_names <- clean_storms[, .(t = sum(total_burden)), by = clean_event][order(-t)][1:15, clean_event]

# 2. Usar CJ() para crear todas las combinaciones posibles
complete_grid <- CJ(year = years_vec,
                    clean_event = top_15_names,
                    region = regions_vec)

# Mira cómo complete_grid tiene ahora muchas más filas que tus datos agregados
# print(dim(complete_grid))

# 3. Tus datos agregados actuales (asegúrate de que esta parte ya la corriste)
heatmap_regiones_actual <- clean_storms[clean_event %in% top_15_names & region != "Other/Marine", 
                                        .(burden = sum(total_burden)), 
                                        by = .(year, clean_event, region)]

# 4. Realizar un RIGHT JOIN: Mantenemos toda la cuadrícula y pegamos los datos
# Usamos 'heatmap_regiones_actual[complete_grid, ...]' para que el resultado tenga
# todas las filas de complete_grid.
heatmap_full <- heatmap_regiones_actual[complete_grid, on = .(year, clean_event, region)]

# 5. El paso clave: Reemplazar NA por 0 en la columna de carga (burden)
heatmap_full[is.na(burden), burden := 0]

# Ahora heatmap_full es una cuadrícula sólida sin huecos.
# Definimos el orden lógico: primero las 4 regiones principales, luego el resto
niveles_region <- c("Northeast", "Midwest", "South", "West", "Territories", "Marine/Other")

# Aplicamos el orden a nuestro DT final
heatmap_full[, region := factor(region, levels = niveles_region)]
ggplot(heatmap_full, aes(x = year, y = clean_event, fill = burden)) +
  geom_tile(color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(
    trans = "log10", 
    # Recuperamos la referencia con etiquetas formateadas como moneda
    labels = scales::label_dollar(scale = 1e-6, suffix = "M"),
    na.value = "#440154", # Color morado oscuro para los ceros
    name = "Carga Total\n(Salud + Eco)"
  ) + 
  facet_wrap(~region, ncol = 2) +
  theme_minimal(base_size = 11) + 
  labs(
    title = "Distribución Geográfica del Impacto de Eventos Extremos",
    subtitle = "Impacto acumulado anual (USD). Escala logarítmica para resaltar eventos de baja frecuencia.",
    x = "Año", 
    y = "Tipo de Evento"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    strip.background = element_rect(fill = "gray95", color = NA), # Resalta el nombre de la región
    strip.text = element_text(face = "bold"),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.key.height = unit(1.5, "cm") # Hace la leyenda más fácil de leer
  )

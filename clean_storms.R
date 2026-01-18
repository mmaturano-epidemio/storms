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

# clean_names here is included so that the dataset has tidy column names

tidy_storms <- copy(storms)

tidy_storms <- tidy_storms[, .(bgn_date, 
                               end_date, 
                               state = as.factor(state),
                               state_2 = as.factor(state_2), 
                               county = as.factor(county),
                               countyname, 
                               evtype, 
                               fatalities,
                               injuries, 
                               propdmg, 
                               propdmgexp, 
                               cropdmg, 
                               cropdmgexp,
                               remarks, 
                               refnum)]


dates <- names(tidy_storms)[names(tidy_storms) %like% "date"] 

# Therefore, we format these dates:
tidy_storms[, (dates) := lapply(.SD, sub, pattern = " .*$", replacement = ""), .SDcols = dates]
tidy_storms[, (dates) := lapply(.SD, mdy), .SDcols = dates]

# And we create an "year" column
tidy_storms[, year := year(bgn_date)]
tidy_storms[, .(`Reported events` = uniqueN(evtype)), year]
tidy_storms[year%in%c(seq(1950, 1990, 5)), 
            .(`Reported events` = unique(evtype)), year]

tidy_storms <- tidy_storms[year > 1992]
tidy_storms[, colMeans(is.na(.SD)) |> round(2)] |> sort(decreasing = TRUE)

tidy_storms[, .(propdmgexp = unique(propdmgexp))] |> c()
tidy_storms[, .(cropdmgexp = unique(cropdmgexp))] |> c()


map_exp <- function(x) {
  x <- toupper(as.character(x))
  fcase(
    x %like% "H", 1e2,
    x %like% "K", 1e3,
    x %like% "M", 1e6,
    x %like% "B", 1e9,
    is.na(x) | x %chin% c("", "-", "?", "+"), 1, 
    x %chin% as.character(0:9), 10^as.numeric(x),  
    default = 1
  )
}

tidy_storms[, prop_total := propdmg * map_exp(propdmgexp)]
tidy_storms[, crop_total := cropdmg * map_exp(cropdmgexp)]
tidy_storms[, economic_loss := prop_total + crop_total]

# Unify evtype format:
tidy_storms[, evtype := stringr::str_to_title(evtype)]

# We see the first 10 evtypes
tidy_storms[, .(evtype = unique(evtype))][, head(evtype, 10)] 

# We count how many evtypes we have
tidy_storms[, uniqueN(evtype)] 

# Eliminate "summary" rows
tidy_storms <- tidy_storms[!evtype %like% "(?i)Summary"]
tidy_storms[, clean_event := fcase(
  # 1. HEAT (Consolidamos el impacto #1 en salud)
  evtype %like% "(?i)Heat|Warm|Hot|Record High", "Excessive Heat",
  evtype %like% "(?i)Tornado|Torn?da?o|Whirlwind|Gustnado", "Tornado",
  evtype %like% "(?i)Hurricane|Typhoon", "Hurricane/Typhoon",
  evtype %like% "(?i)Tropical Storm", "Tropical Storm",
  evtype %like% "(?i)Flash Flood|Rapidly Rising", "Flash Flood",
  evtype %like% "(?i)Lakeshore Flood", "Lakeshore Flood",
  evtype %like% "(?i)Coastal Flood|Cstl Flood|Tidal", "Coastal Flood",
  evtype %like% "(?i)Flood|Fld|Stream|Urban", "Flood",
  evtype %like% "(?i)Marine (Thunderstorm|Tstm)", "Marine Thunderstorm Wind",
  evtype %like% "(?i)Thunderstorm|Tstm|Thund?e?e?r?sto?r?m|Burst|Tunderstorm", "Thunderstorm Wind",
  evtype %like% "(?i)Marine High Wind", "Marine High Wind",
  evtype %like% "(?i)High Wind|High  Wind", "High Wind",
  evtype %like% "(?i)Marine Strong Wind", "Marine Strong Wind",
  evtype %like% "(?i)Strong Wind|Gusty|^Winds?$", "Strong Wind",
  evtype %like% "(?i)Extreme Cold|Extreme Wind Chill", "Extreme Cold/Wind Chill",
  evtype %like% "(?i)Cold|Wind Chill|Hypothermia|Low Temp|Exposure", "Cold/Wind Chill",
  evtype %like% "(?i)Blizzard", "Blizzard",
  evtype %like% "(?i)Winter Storm", "Winter Storm",
  evtype %like% "(?i)Winter Weather|Wintry|Glaze|Black Ice|Ice On Road|Freezing", "Winter Weather",
  evtype %like% "(?i)Ice Storm", "Ice Storm",
  evtype %like% "(?i)Snow", "Heavy Snow",
  evtype %like% "(?i)Frost|Freeze", "Frost/Freeze",
  evtype %like% "(?i)Rip Current", "Rip Current",
  evtype %like% "(?i)Surf|High Tide|High Water|Swells|Waves|Seas|Drowning", "High Surf",
  evtype %like% "(?i)Storm Surge|Coastal ?storm", "Storm Surge/Tide",
  evtype %like% "(?i)Ligh?tning|Ligntning", "Lightning",
  evtype %like% "(?i)Wildfire|Fire|Forest Fire", "Wildfire",
  evtype %like% "(?i)Landslide|Mud ?slide|Debris|Rock Slide", "Debris Flow",
  evtype %like% "(?i)Avalanche|Avalance", "Avalanche",
  evtype %like% "(?i)Drought", "Drought",
  evtype %like% "(?i)Fog", "Dense Fog",
  evtype %like% "(?i)Rain|Shower|Precip", "Heavy Rain",
  evtype %like% "(?i)Other|Summary|\\?|None", "Other",
  default = evtype
)]

tidy_storms[, uniqueN(clean_event)]

northeast <- c("CT", "ME", "MA", "NH", "RI", "VT", "NJ", "NY", "PA")
midwest   <- c("IL", "IN", "MI", "OH", "WI", "IA", "KS", "MN", "MO", "NE", "ND", "SD")
south     <- c("DE", "FL", "GA", "MD", "NC", "SC", "VA", "DC", "WV", "AL", "KY", "MS", "TN", "AR", "LA", "OK", "TX")
west      <- c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY", "AK", "CA", "HI", "OR", "WA")

tidy_storms[, region := fcase(
  state_2 %in% northeast, "Northeast",
  state_2 %in% midwest,   "Midwest",
  state_2 %in% south,     "South",
  state_2 %in% west,      "West",
  state_2 %in% c("PR", "GU", "AS", "VI", "MH", "AM"), "Territories",
  default = "Marine/Other"
)]

tidy_storms |> names()
tidy_storms[countyname == "NAPA" & year == 2006, .(evtype, propdmg, propdmgexp, remarks)]


# Correcting the 2006 Napa Valley outlier
tidy_storms[countyname == "NAPA" & year == 2006 & propdmgexp == "B", 
            `:=`(prop_total = prop_total / 1000, 
                 economic_loss = (prop_total / 1000) + crop_total)]

### Data analysis ----
 

# ---- Health impact ----

total_fatalities <- tidy_storms[, sum(fatalities)]
total_injuries <- tidy_storms[, sum(injuries)]

health_table <- tidy_storms[, .(fatalities = sum(fatalities),
                                 fatalities_pct = round(sum(fatalities) / 
                                                          total_fatalities * 100, 2),
                                 injuries = sum(injuries),
                                 injuries_pct = round(sum(injuries) /
                                                        total_injuries * 100, 2)),
                             clean_event][
                               order(-fatalities)][
                                 , `:=`(fatalities_cumpct = cumsum(fatalities_pct),
                                        injuries_cumpct = cumsum(injuries_pct))][
                                          !(injuries == 0 & fatalities == 0)][1:20]

setcolorder(health_table, c("clean_event", "fatalities", 
                                       "fatalities_pct", "fatalities_cumpct",
                                       "injuries", "injuries_pct", "injuries_cumpct"))
health_table

# ---- Economic impact ----
total_loss <- tidy_storms[, sum(economic_loss, na.rm = TRUE)]
prop_loss <- tidy_storms[, sum(prop_total, na.rm = TRUE)]
crop_loss <- tidy_storms[, sum(crop_total, na.rm = TRUE)]
total_loss == prop_loss + crop_loss

economy_table <- tidy_storms[, .(prop_total = sum(prop_total),
                                 prop_total_pct = round(sum(prop_total) / 
                                                          prop_loss * 100, 2),
                                 crop_total = sum(crop_total),
                                 crop_total_pct = round(sum(crop_total) /
                                                        crop_loss * 100, 2)),
                             clean_event][
                               order(-prop_total)][
                                 , `:=`(prop_total_cumpct = cumsum(prop_total_pct),
                                        crop_total_cumpct = cumsum(crop_total_pct))][
                                          !(crop_total == 0 & prop_total == 0)][1:20]

setcolorder(economy_table, c("clean_event", "prop_total", 
                                       "prop_total_pct", "prop_total_cumpct",
                                       "crop_total", "crop_total_pct", "crop_total_cumpct"))
economy_table

# ---- Global impact ----
health_events <- health_table[, unique(clean_event)]
eco_events <- economy_table[, unique(clean_event)]
health_events[!health_events %chin% eco_events]
eco_events[!eco_events %chin% health_events]
# We see some events are very relevant in terms of health and almost insignificant in terms
# of economic impact, and visceversa. 

# Therefore, we need a metric that can account for the global impact of events. 
# We will use the Value of Statistical Life in order to measure health impact in terms of
# economical loss. https://en.wikipedia.org/wiki/Value_of_life
# We will take the 13.2 million USD value recommended by the US Department of Transportation
# to account for each fatality, and  10% of said value for each injury.
# This approach may be imperfect and somewhat arbitrary, but it can help us quantify the
# global impact of extreme wheather events.

vsl_value <- 13.2e6    
injury_value <- 13.2e5  

# Estimate Global Burden
tidy_storms[, health_loss := (fatalities * vsl_value) + (injuries * injury_value)]
tidy_storms[, total_burden := economic_loss + health_loss]

# Top 10 
top_10_events <- tidy_storms[, .(
  total_burden_B = sum(total_burden) / 1e9,
  economic_loss_B = sum(economic_loss) / 1e9,
  health_loss_B = sum(health_loss) / 1e9,
  fatalities = sum(fatalities),
  total_events = .N
), by = clean_event][order(-total_burden_B)][1:10]

# Print table
flextable::flextable(top_10_events) |> 
  flextable::colformat_double(digits = 2)

# Finally, we will use this metric for estimating the impact by region:
top_15_names <- tidy_storms[, .(t = sum(total_burden)), by = clean_event][order(-t)][1:15, clean_event]

# 2. Todas las regiones (incluyendo Marine/Other para que no quede el hueco blanco)
# Nota: Usamos el nombre exacto que pusiste en el fcase original
regions_vec <- unique(tidy_storms$region) 

# 3. El orden de las facetas que querías
niveles_region <- c("Northeast", "Midwest", "South", "West", "Territories", "Marine/Other")

# A. Cuadrícula completa
complete_grid <- CJ(year = unique(tidy_storms$year),
                    clean_event = top_15_names,
                    region = regions_vec)

# B. Datos agregados (sin transformar factores todavía)
heatmap_regiones_actual <- tidy_storms[clean_event %in% top_15_names, 
                                        .(burden = sum(total_burden)), 
                                        by = .(year, clean_event, region)]

# C. Join limpio: Traemos los datos a la cuadrícula completa
heatmap_full <- heatmap_regiones_actual[complete_grid, on = .(year, clean_event, region)]

# D. Rellenar NAs con 0
heatmap_full[is.na(burden), burden := 0]
# Orden de regiones para las facetas
heatmap_full[, region := factor(region, levels = niveles_region)]

# Orden de eventos: el más severo (Flood) quedará al final del factor, o sea, ARRIBA
heatmap_full[, clean_event := factor(clean_event, levels = rev(top_15_names))]
ggplot(heatmap_full, aes(x = year, y = clean_event, fill = burden)) +
  geom_tile(color = "white", linewidth = 0.05) +
  scale_fill_viridis_c(
    trans = "log10", 
    labels = scales::label_dollar(scale = 1e-6, suffix = "M"),
    na.value = "#440154", 
    name = "Carga Total\n(Eco + Salud)"
  ) + 
  facet_wrap(~region, ncol = 2) +
  theme_minimal(base_size = 11) + 
  labs(
    title = "Jerarquía de Severidad por Región y Año",
    subtitle = "Eventos ordenados por impacto global acumulado (más severos arriba)",
    x = "Año", y = ""
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 9),
    strip.background = element_rect(fill = "gray95", color = NA),
    strip.text = element_text(face = "bold"),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm")
  )
graphics.off()

health_long <- melt(health_table[1:10], 
                    id.vars = "clean_event", 
                    measure.vars = c("fatalities", "injuries"),
                    variable.name = "type", value.name = "count")

ggplot(health_long, aes(x = reorder(clean_event, count), y = count, fill = type)) +
  geom_col() + coord_flip() + facet_wrap(~type, scales = "free_x") +
  scale_fill_manual(values = c("fatalities" = "#B22222", "injuries" = "#E69F00")) +
  theme_minimal() + 
  labs(title = "Top 10 Health Impact", x = "", y = "People Affected") +
  theme(legend.position = "none")

# Hacemos el merge de las dos tablas que ya tenías
# Usamos 'all = TRUE' para no perder eventos que solo están en una de las dos
summary_table <- merge(health_table[, .(clean_event, fatalities, injuries)], 
                       economy_table[, .(clean_event, prop_total, crop_total)], 
                       by = "clean_event", all = TRUE)

# Rellenamos NAs con 0 por si un evento solo tenía daños económicos o solo salud
summary_table[is.na(summary_table)] <- 0

# Añadimos la columna de Burden (Carga) para ordenar la tabla final
# Usando los valores VSL que definiste (13.2M por muerte, 1.32M por herido)
summary_table[, total_burden_B := ((fatalities * 13.2) + (injuries * 1.32) + 
                                     (prop_total / 1e3) + (crop_total / 1e3)) / 1e3]

# Mostramos el Top 15 Global
final_tab <- summary_table[order(-total_burden_B)][1:15]

flextable::flextable(final_tab) |> 
  flextable::set_header_labels(
    clean_event = "Event Type",
    fatalities = "Deaths",
    injuries = "Injuries",
    prop_total = "Prop. Damage ($M)",
    crop_total = "Crop Damage ($M)",
    total_burden_B = "Global Burden ($B)"
  ) |> 
  flextable::colformat_double(digits = 2) |> 
  flextable::autofit()
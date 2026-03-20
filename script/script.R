## load the required packages
library(tidyverse)
library(lme4)
library(sjPlot)
library(simr)
library(broom.mixed)
library(lattice)
library(performance)
library(see)
library(ggrepel)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(geodata)
library(terra)
library(scales)
## Load the data set
nail <- read.csv("./data/nail.csv", fileEncoding="latin1")
EAsocieties <- read.csv("./data/eHRAF coverage.csv", fileEncoding="latin1")
nail1 <- read.csv("./data/nail_transformed_v2.csv", fileEncoding="latin1")

## Merge two data frames to extract coordinates
nail_all <- left_join(nail1, EAsocieties, by = "Society")
## Remove Bedouin because it does not have geographic coordinates
nail_all <-  nail_all %>% filter(Society != "Bedouin")
# Remove duplicates based on Society
nail_geo <- nail_all %>% distinct(Society, .keep_all = TRUE)
nail_geo <- nail_geo %>%
  mutate(Society = if_else(str_detect(Society, "Abip"), "Abipón", Society))
write.csv(nail_geo,"./data/nail_geo.csv", row.names = FALSE)


## counting subsistence type for mapping
nail_geo %>% count(Subsistence)

world <- ne_countries(scale = "medium", returnclass = "sf")
# Convert points to sf object (WGS84 = EPSG:4326 by default)
points_sf <- st_as_sf(nail_geo, coords = c("Long", "Lat"), crs = 4326)

# Plot
nail_geo$Subsistence <- factor(nail_geo$Subsistence,
                               levels = c("Hunter-gatherers","Primarily hunter-gatherers", 
                                          "Horticulturalists", "Pastoralists", 
                                          "Agro-pastoralists","Intensive agriculturalists", 
                                          "Other subsistence combinations"
                                          
                                          # add any other categories you actually have
                               ))

# Convert to sf object
nail_geo <- st_as_sf(nail_geo, coords = c("Long", "Lat"), crs = 4326)

# ── Plot: full world map ─────────────────────────────────────────────────────
ggplot() +
  
  # Very light world base
  geom_sf(data = world,
          fill = "grey96",
          color = "grey80",
          linewidth = 0.2) +
  
  # Your points — a bit larger so visible on world scale
  geom_sf(data = nail_geo,
          aes(color = Subsistence),
          size = 2,
          alpha = 0.52,
          shape = 16) +
  
  # Smart non-overlapping labels
  geom_text_repel(
    data = nail_geo,
    aes(label = Society,
        geometry = geometry),   # ← important when using sf objects
    stat = "sf_coordinates",    # tells repel to use point coordinates
    size = 3,
    color = "black",
    bg.color = "white",
    bg.r = 0.16,                # thin white halo
    point.padding = 0.5,
    box.padding = 0.9,
    segment.color = "grey50",
    segment.size = 0.35,
    min.segment.length = 0,
    max.overlaps = 25,          # allow more when many points
    force = 1.5,                # stronger push away from points & other labels
    seed = 2025                 # change if you want different arrangement
  ) +
  
  # Colors — Dark2 or Set1 usually work well for categories
  scale_color_brewer(palette = "Dark2",
                     name = "Subsistence Type",
                     drop = FALSE) +   # keeps all levels even if some missing
  
  # Full world view
  coord_sf(
    expand = TRUE,
    # optional: crs = "+proj=robin"   # ← try Robinson projection if you prefer
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.key.size = unit(1, "lines"),
    plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 8)),
    plot.subtitle = element_text(hjust = 0.5, color = "grey50"),
    panel.grid = element_line(color = "grey92", linewidth = 0.3),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
  )

ggsave("figure/Fig.1.png", width = 14, height = 9, dpi = 300)





###################
# distribution of context by subsistence

# Identify the context columns for reshaping
context_columns <- c("general", "status.signaling", "ritual", 
                     "ornamentation", "magic", "tooling", "punishment")

# Pivot the data to a long format for ggplot
df_long <- nail1 %>%
  select(Subsistence, all_of(context_columns)) %>%
  pivot_longer(cols = all_of(context_columns), 
               names_to = "Context_Type", 
               values_to = "Presence") %>%
  filter(Presence == 1)

# Create a bar chart showing the frequency of contexts within each subsistence category
ggplot(df_long, aes(x = Subsistence, fill = Context_Type)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Distribution of Nail Modification Contexts by Subsistence Type",
       x = "Subsistence Category",
       y = "Count",
       fill = "Context") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figure/context by subsistence.png", width = 14, height = 9, dpi = 300)

# Create a bar chart showing subsistence categories grouped by context
ggplot(df_long, aes(x = Context_Type, fill = Subsistence)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Subsistence Strategies Distributed by Modification Context",
       x = "Context Type",
       y = "Count",
       fill = "Subsistence Category") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figure/subsistence by context.png", width = 14, height = 9, dpi = 300)


# Create a bar chart showing percentages of subsistence within each context
ggplot(df_long, aes(x = Context_Type, fill = Subsistence)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  theme_minimal() +
  labs(title = "Percentage of Subsistence Strategies Distributed by Context",
       x = "Context Type",
       y = "Percentage",
       fill = "Subsistence Category") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figure/subsistence by context ratio.png", width = 14, height = 9, dpi = 300)

######################## finger morphology

finger_columns <- c("trimming", "long.nail", "surface.scraping", 
                    "painting", "extraction", "accessories")

# Pivot the data to a long format for ggplot
df_long_finger <- nail1 %>%
  select(Subsistence, all_of(finger_columns)) %>%
  pivot_longer(cols = all_of(finger_columns), 
               names_to = "Finger_Modification", 
               values_to = "Presence") %>%
  filter(Presence == 1)

# Create a bar chart showing percentages of subsistence within each finger modification
ggplot(df_long_finger, aes(x = Finger_Modification, fill = Subsistence)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  theme_minimal() +
  labs(title = "Percentage of Subsistence Strategies by Finger Modification Type",
       x = "Finger Modification Type",
       y = "Percentage",
       fill = "Subsistence Category") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figure/subsistence by morpology ratio.png", width = 14, height = 9, dpi = 300)

############ Environemntal factors

df <- read.csv("./data/nail_geo.csv", fileEncoding="latin1")

# 2. Get Ecological Zones (FAO)
eco_data <- worldclim_global(var = "bio", res = 10, path = tempdir())

# 3. Define Ecological Risk (Bio15 = Precipitation Seasonality)
# This is a standard proxy for ecological risk in hunter-gatherer studies
risk_raster <- eco_data[["wc2.1_10m_bio_15"]]

# 4. Get Ecological Zones from the 'dismo' or 'envirem' packages 
# if 'geodata' is restrictive. 
# However, a robust way within 'geodata' is to use 'travel' or 'pop' 
# for human-centric risk, or stick to Bioclimatic variables.

# Create spatial points from your Lat/Long
# Ensure your dataframe 'df' has no NA values in Lat/Long columns
points <- vect(df, geom = c("Long", "Lat"), crs = "EPSG:4326")

# 5. Extract values
df$ecological_risk <- terra::extract(risk_raster, points)[, 2]
bioclim_data <- worldclim_global(var = "bio", res = 10, path = tempdir())
# This assumes your dataframe is named df and already contains the ecological_risk column
df <- df %>%
  mutate(risk_level = case_when(
    ecological_risk < 25 ~ "Low Risk",
    ecological_risk >= 25 & ecological_risk <= 75 ~ "Moderate Risk",
    ecological_risk > 75 ~ "High Risk"
  ))

# Converting the new column into an ordered factor for better plotting
df$risk_level <- factor(df$risk_level, levels = c("Low Risk", "Moderate Risk", "High Risk"))





# 3. Extract the specific layers
risk_layer <- bioclim_data[["wc2.1_10m_bio_15"]]
temp_layer <- bioclim_data[["wc2.1_10m_bio_1"]]
precip_layer <- bioclim_data[["wc2.1_10m_bio_12"]]

# 4. Prepare coordinates for extraction
points <- vect(df, geom = c("Long", "Lat"), crs = "EPSG:4326")

# 5. Extract values
df$ecological_risk <- terra::extract(risk_layer, points)[, 2]
df$annual_temp <- terra::extract(temp_layer, points)[, 2]
df$annual_precip <- terra::extract(precip_layer, points)[, 2]

# 6. Create Ecological Zones based on Whitley & Clark (1985) or similar 
# archaeological environmental standards
df <- df %>%
  mutate(ecological_zones = case_when(
    annual_precip < 250 ~ "Arid",
    annual_precip >= 250 & annual_precip < 500 ~ "Semi-Arid",
    annual_temp > 20 & annual_precip >= 1500 ~ "Tropical",
    annual_temp < 0 ~ "Arctic/Alpine",
    TRUE ~ "Temperate"
  ))
############## enviromental visualazation
############ecological risk-context
write.csv(df, "./data/nail_geo_env_levels.csv", fileEncoding="latin1")
nail_data <- read.csv("./data/nail_geo_env_levels.csv")
# Identify the context columns for reshaping
context_columns <- c("general", "status.signaling", "ritual", 
                     "ornamentation", "magic", "tooling", "punishment")

# Pivot the data to a long format and filter for valid risk levels
df_long <- nail_data %>%
  filter(!is.na(risk_level)) %>%
  select(risk_level, all_of(context_columns)) %>%
  pivot_longer(cols = all_of(context_columns), 
               names_to = "Context_Type", 
               values_to = "Presence") %>%
  filter(Presence == 1) %>%
  # Clean names for better display
  mutate(Context_Type = str_to_title(gsub("\\.", " ", Context_Type)),
         risk_level = factor(risk_level, levels = c("Low Risk", "Moderate Risk", "High Risk")))

# Create a bar chart showing the percentage of risk levels within each context
ggplot(df_long, aes(x = Context_Type, fill = risk_level)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d() +
  theme_minimal() +
  labs(title = "Percentage of Ecological Risk Levels by Cultural Context",
       x = "Cultural Context",
       y = "Percentage",
       fill = "Risk Level") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot
ggsave("risk_level_by_context.png", width = 12, height = 8, dpi = 300)

################# ecological zones-context
# Pivot the data to a long format specifically for ecological zones
df_long_eco <- nail_data %>%
  filter(!is.na(ecological_zones)) %>%
  select(ecological_zones, all_of(context_columns)) %>%
  pivot_longer(cols = all_of(context_columns), 
               names_to = "Context_Type", 
               values_to = "Presence") %>%
  filter(Presence == 1) %>%
  # Standardize the labels for the visualization
  mutate(Context_Type = str_to_title(gsub("\\.", " ", Context_Type)))

# Create the percentage bar chart
ggplot(df_long_eco, aes(x = Context_Type, fill = ecological_zones)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set3", name = "Ecological Zone") +
  theme_minimal() +
  labs(title = "Percentage of Ecological Zones by Cultural Context",
       x = "Cultural Context",
       y = "Percentage of Instances",
       fill = "Ecological Zone") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the resulting figure
ggsave("ecological_zones_by_context.png", width = 12, height = 8, dpi = 300)


###### ecological zone by morphology
finger_columns <- c("trimming", "long.nail", "surface.scraping", 
                    "painting", "extraction", "accessories")

# 3. Reshape the data to a long format
# We select the ecological zones and the finger modification columns
# Then we filter for cases where the modification is present (value equals 1)
df_long_finger_eco <- nail_data %>%
  select(ecological_zones, all_of(finger_columns)) %>%
  pivot_longer(cols = all_of(finger_columns), 
               names_to = "Finger_Modification", 
               values_to = "Presence") %>%
  filter(Presence == 1) %>%
  # Remove rows with missing ecological data to ensure a clean visualization
  filter(!is.na(ecological_zones)) %>%
  # Format the modification names for the plot labels
  mutate(Finger_Modification = str_to_title(gsub("\\.", " ", Finger_Modification)))

# 4. Create the stacked bar chart showing percentages
ggplot(df_long_finger_eco, aes(x = Finger_Modification, fill = ecological_zones)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  scale_fill_viridis_d(name = "Ecological Zone") +
  theme_minimal() +
  labs(title = "Percentage of Ecological Zones by Finger Modification Type",
       x = "Finger Modification Type",
       y = "Percentage of Observations",
       fill = "Ecological Zone") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 5. Save the final figure to your working directory
ggsave("eco_zones_by_morphology_ratio.png", width = 14, height = 9, dpi = 300)

#### risk level by morphology

# 2. Define the columns for finger morphology
finger_columns <- c("trimming", "long.nail", "surface.scraping", 
                    "painting", "extraction", "accessories")

# 3. Pivot the data to a long format for the risk level analysis
df_long_finger_risk <- nail_data %>%
  select(risk_level, all_of(finger_columns)) %>%
  pivot_longer(cols = all_of(finger_columns), 
               names_to = "Finger_Modification", 
               values_to = "Presence") %>%
  # Keep only instances where the modification is recorded as present
  filter(Presence == 1) %>%
  # Filter out missing values for risk_level
  filter(!is.na(risk_level)) %>%
  # Standardize modification names for display and set the order for risk levels
  mutate(Finger_Modification = str_to_title(gsub("\\.", " ", Finger_Modification)),
         risk_level = factor(risk_level, levels = c("Low Risk", "Moderate Risk", "High Risk")))

# 4. Create the bar chart showing percentages of risk levels
ggplot(df_long_finger_risk, aes(x = Finger_Modification, fill = risk_level)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  scale_fill_viridis_d(name = "Risk Level") +
  theme_minimal() +
  labs(title = "Percentage of Ecological Risk Levels by Finger Modification Type",
       x = "Finger Modification Type",
       y = "Percentage of Observations",
       fill = "Risk Level") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 5. Save the final figure to your working directory
ggsave("risk_level_by_morphology_ratio.png", width = 14, height = 9, dpi = 300)
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
library(stringr)
library(patchwork)
library(openxlsx)
library(rstatix)

## Load the data set
nail <- read.csv("data/nail.csv", fileEncoding="latin1")
EAsocieties <- read.csv("data/eHRAF coverage.csv", fileEncoding="latin1")
nail1 <- read.csv("data/nail_transformed_v2.csv", fileEncoding="latin1")

## Merge two data frames to extract coordinates
nail_all <- left_join(nail1, EAsocieties, by = "OWC_Code")
## Remove Bedouin because it does not have geographic coordinates
nail_all <-  nail_all %>% filter(Society.y != "Bedouin")
# Remove duplicates based on Society
nail_geo <- nail_all %>% distinct(Society.y, .keep_all = TRUE)
# nail_geo <- nail_geo %>%
#   mutate(Society = if_else(str_detect(Society, "Abip"), "Abipón", Society))
write.csv(nail_geo,"data/nail_geo.csv", row.names = FALSE)


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
nail_data <- df
write.csv(nail_data,"data/nail_geo_env_levels.csv", row.names = FALSE)

# ─────────────────────────────────────────────
# COLOR-BLIND FRIENDLY PALETTE (Okabe-Ito)
# ─────────────────────────────────────────────
okabe_ito <- c(
  "#E69F00", "#56B4E9", "#009E73",
  "#F0E442", "#0072B2", "#D55E00", "#CC79A7"
)

# ─────────────────────────────────────────────
# SHARED THEME (Font sizes maximum scaling for legibility)
# ─────────────────────────────────────────────
shared_theme <- theme_minimal(base_size = 28) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 26, face = "bold"),
    axis.text.y  = element_text(size = 22, face = "bold"),
    axis.title   = element_text(size = 26, face = "bold"),
    legend.text  = element_text(size = 26),
    legend.title = element_text(size = 26, face = "bold"),
    plot.title   = element_text(size = 26, face = "bold"),
    panel.grid.major.x = element_blank()
  )

# ─────────────────────────────────────────────
# CONTEXT COLUMNS  (punishment removed)
# ─────────────────────────────────────────────
context_columns <- c("general", "status.signaling", "ritual",
                     "ornamentation", "magic", "tooling")

finger_columns <- c("trimming", "long.nail", "surface.scraping",
                    "painting", "extraction", "accessories")

# ══════════════════════════════════════════════
# ROW 1 — SUBSISTENCE
# ══════════════════════════════════════════════

# Plot a – subsistence by context (%)
df_sub_ctx <- nail_data %>%
  select(Subsistence, all_of(context_columns)) %>%
  pivot_longer(cols = all_of(context_columns),
               names_to  = "Context_Type",
               values_to = "Presence") %>%
  filter(Presence == 1) %>%
  mutate(Context_Type = str_to_title(gsub("\\.", " ", Context_Type)))

p1 <- ggplot(df_sub_ctx, aes(x = Context_Type, fill = Subsistence)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = okabe_ito) +
  shared_theme +
  theme(legend.position = "none") + 
  labs(title = "Subsistence by Context",
       x = "Cultural Context", y = "Percentage", fill = "Subsistence")

# Plot b – subsistence by finger modification (%)
df_sub_finger <- nail_data %>%
  select(Subsistence, all_of(finger_columns)) %>%
  pivot_longer(cols = all_of(finger_columns),
               names_to  = "Finger_Modification",
               values_to = "Presence") %>%
  filter(Presence == 1) %>%
  mutate(Finger_Modification = str_to_title(gsub("\\.", " ", Finger_Modification)))

p2 <- ggplot(df_sub_finger, aes(x = Finger_Modification, fill = Subsistence)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = okabe_ito) +
  shared_theme +
  labs(title = "Subsistence by Modification",
       x = "Finger Modification Type", y = "Percentage", fill = "Subsistence")

# ══════════════════════════════════════════════
# ROW 2 — ECOLOGICAL ZONE
# ══════════════════════════════════════════════

# Plot c – ecological zone by context (%)
df_eco_ctx <- nail_data %>%
  filter(!is.na(ecological_zones)) %>%
  select(ecological_zones, all_of(context_columns)) %>%
  pivot_longer(cols = all_of(context_columns),
               names_to  = "Context_Type",
               values_to = "Presence") %>%
  filter(Presence == 1) %>%
  mutate(Context_Type = str_to_title(gsub("\\.", " ", Context_Type)))

p3 <- ggplot(df_eco_ctx, aes(x = Context_Type, fill = ecological_zones)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = okabe_ito, name = "Ecological Zone") +
  shared_theme +
  theme(legend.position = "none") + 
  labs(title = "Ecological Zone by Context",
       x = "Cultural Context", y = "Percentage", fill = "Ecological Zone")

# Plot d – ecological zone by finger modification (%)
df_eco_finger <- nail_data %>%
  filter(!is.na(ecological_zones)) %>%
  select(ecological_zones, all_of(finger_columns)) %>%
  pivot_longer(cols = all_of(finger_columns),
               names_to  = "Finger_Modification",
               values_to = "Presence") %>%
  filter(Presence == 1) %>%
  mutate(Finger_Modification = str_to_title(gsub("\\.", " ", Finger_Modification)))

p4 <- ggplot(df_eco_finger, aes(x = Finger_Modification, fill = ecological_zones)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = okabe_ito, name = "Ecological Zone") +
  shared_theme +
  labs(title = "Ecological Zone by Modification",
       x = "Finger Modification Type", y = "Percentage", fill = "Ecological Zone")

# ══════════════════════════════════════════════
# ROW 3 — ECOLOGICAL RISK LEVEL
# ══════════════════════════════════════════════

# Plot e – risk level by context (%)
df_risk_ctx <- nail_data %>%
  filter(!is.na(risk_level)) %>%
  select(risk_level, all_of(context_columns)) %>%
  pivot_longer(cols = all_of(context_columns),
               names_to  = "Context_Type",
               values_to = "Presence") %>%
  filter(Presence == 1) %>%
  mutate(Context_Type = str_to_title(gsub("\\.", " ", Context_Type)),
         risk_level = factor(risk_level,
                             levels = c("Low Risk", "Moderate Risk", "High Risk")))

p5 <- ggplot(df_risk_ctx, aes(x = Context_Type, fill = risk_level)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = okabe_ito, name = "Risk Level") +
  shared_theme +
  theme(legend.position = "none") + 
  labs(title = "Ecological Risk by Context",
       x = "Cultural Context", y = "Percentage", fill = "Risk Level")

# Plot f – risk level by finger modification (%)
df_risk_finger <- nail_data %>%
  filter(!is.na(risk_level)) %>%
  select(risk_level, all_of(finger_columns)) %>%
  pivot_longer(cols = all_of(finger_columns),
               names_to  = "Finger_Modification",
               values_to = "Presence") %>%
  filter(Presence == 1) %>%
  mutate(Finger_Modification = str_to_title(gsub("\\.", " ", Finger_Modification)),
         risk_level = factor(risk_level,
                             levels = c("Low Risk", "Moderate Risk", "High Risk")))

p6 <- ggplot(df_risk_finger, aes(x = Finger_Modification, fill = risk_level)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = okabe_ito, name = "Risk Level") +
  shared_theme +
  labs(title = "Ecological Risk by Modification",
       x = "Finger Modification Type", y = "Percentage", fill = "Risk Level")

# ══════════════════════════════════════════════
# PATCHWORK  –  2 columns × 3 rows
# ══════════════════════════════════════════════

combined_figure <- (p1 | p2) /
  (p3 | p4) /
  (p5 | p6) +
  plot_annotation(
    tag_levels = 'A',
    theme      = theme(
      plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
      plot.tag   = element_text(size = 30, face = "bold")
    )
  )

# Expanded dimensions to prevent overlapping with large text
ggsave("figure/combined_nail_figure.png",
       plot    = combined_figure,
       width   = 24,
       height  = 30,
       dpi     = 600)
##################################stats test###########

# ══════════════════════════════════════════════
# HELPER FUNCTION
# Runs chi-square; falls back to Fisher if any
# expected cell count is below 5
# ══════════════════════════════════════════════

run_test <- function(data, group_var, label) {
  tbl <- table(data[[group_var]], data[["fill_var"]])
  expected <- chisq.test(tbl)$expected
  cat("\n─────────────────────────────────────\n")
  cat(label, "\n")
  cat("─────────────────────────────────────\n")
  if (any(expected < 5)) {
    cat("Note: expected counts < 5 detected — using Fisher's exact test\n")
    print(fisher.test(tbl, simulate.p.value = TRUE, B = 10000))
  } else {
    print(chisq.test(tbl))
  }
}

# ══════════════════════════════════════════════
# ROW 1 — SUBSISTENCE
# ══════════════════════════════════════════════
# a: subsistence by context
df_sub_ctx_long <- nail_data %>%
  select(Subsistence, all_of(context_columns)) %>%
  pivot_longer(cols = all_of(context_columns),
               names_to  = "fill_var",
               values_to = "Presence") %>%
  filter(Presence == 1) %>%
  mutate(fill_var = str_to_title(gsub("\\.", " ", fill_var)))

run_test(df_sub_ctx_long, group_var = "Subsistence",
         label = "a: Subsistence by Context")

# b: subsistence by finger modification
df_sub_finger_long <- nail_data %>%
  select(Subsistence, all_of(finger_columns)) %>%
  pivot_longer(cols = all_of(finger_columns),
               names_to  = "fill_var",
               values_to = "Presence") %>%
  filter(Presence == 1) %>%
  mutate(fill_var = str_to_title(gsub("\\.", " ", fill_var)))

run_test(df_sub_finger_long, group_var = "Subsistence",
         label = "b: Subsistence by Finger Modification")

# ══════════════════════════════════════════════
# ROW 2 — ECOLOGICAL ZONE
# ══════════════════════════════════════════════

# c: ecological zone by context
df_eco_ctx_long <- nail_data %>%
  filter(!is.na(ecological_zones)) %>%
  select(ecological_zones, all_of(context_columns)) %>%
  pivot_longer(cols = all_of(context_columns),
               names_to  = "fill_var",
               values_to = "Presence") %>%
  filter(Presence == 1) %>%
  mutate(fill_var = str_to_title(gsub("\\.", " ", fill_var))) %>%
  rename(Subsistence = ecological_zones)

run_test(df_eco_ctx_long, group_var = "Subsistence",
         label = "c: Ecological Zone by Context")

# d: ecological zone by finger modification
df_eco_finger_long <- nail_data %>%
  filter(!is.na(ecological_zones)) %>%
  select(ecological_zones, all_of(finger_columns)) %>%
  pivot_longer(cols = all_of(finger_columns),
               names_to  = "fill_var",
               values_to = "Presence") %>%
  filter(Presence == 1) %>%
  mutate(fill_var = str_to_title(gsub("\\.", " ", fill_var))) %>%
  rename(Subsistence = ecological_zones)

run_test(df_eco_finger_long, group_var = "Subsistence",
         label = "d: Ecological Zone by Finger Modification")

# ══════════════════════════════════════════════
# ROW 3 — ECOLOGICAL RISK LEVEL
# ══════════════════════════════════════════════

# e: risk level by context
df_risk_ctx_long <- nail_data %>%
  filter(!is.na(risk_level)) %>%
  select(risk_level, all_of(context_columns)) %>%
  pivot_longer(cols = all_of(context_columns),
               names_to  = "fill_var",
               values_to = "Presence") %>%
  filter(Presence == 1) %>%
  mutate(fill_var = str_to_title(gsub("\\.", " ", fill_var)),
         risk_level = factor(risk_level,
                             levels = c("Low Risk", "Moderate Risk", "High Risk"))) %>%
  rename(Subsistence = risk_level)

run_test(df_risk_ctx_long, group_var = "Subsistence",
         label = "e: Ecological Risk by Context")

# f: risk level by finger modification
df_risk_finger_long <- nail_data %>%
  filter(!is.na(risk_level)) %>%
  select(risk_level, all_of(finger_columns)) %>%
  pivot_longer(cols = all_of(finger_columns),
               names_to  = "fill_var",
               values_to = "Presence") %>%
  filter(Presence == 1) %>%
  mutate(fill_var = str_to_title(gsub("\\.", " ", fill_var)),
         risk_level = factor(risk_level,
                             levels = c("Low Risk", "Moderate Risk", "High Risk"))) %>%
  rename(Subsistence = risk_level)

run_test(df_risk_finger_long, group_var = "Subsistence",
         label = "f: Ecological Risk by Finger Modification")


###########################

# ══════════════════════════════════════════════
# HELPER FUNCTION
# For one group level, counts how often each
# context/modification is present, then tests
# whether frequencies differ from equal distribution
# ══════════════════════════════════════════════

within_group_test <- function(data, group_var, type_columns, label) {
  
  cat("\n═══════════════════════════════════════════\n")
  cat(label, "\n")
  cat("═══════════════════════════════════════════\n")
  
  groups <- unique(data[[group_var]])
  groups <- groups[!is.na(groups)]
  
  results <- map_dfr(groups, function(g) {
    
    subset_data <- data %>% filter(.data[[group_var]] == g)
    n_cases     <- nrow(subset_data)
    
    # Count presence of each type
    counts <- subset_data %>%
      select(all_of(type_columns)) %>%
      summarise(across(everything(), sum, na.rm = TRUE)) %>%
      pivot_longer(everything(),
                   names_to  = "Type",
                   values_to = "Count") %>%
      mutate(
        Type       = str_to_title(gsub("\\.", " ", Type)),
        Group      = g,
        N_cases    = n_cases,
        Proportion = Count / n_cases
      )
    
    # Goodness-of-fit chi-square across all types within this group
    # Null: all types equally likely
    chi_result  <- chisq.test(counts$Count)
    
    # Binomial test per type: is this type's frequency
    # different from the mean frequency across all types?
    expected_p  <- mean(counts$Proportion)
    
    counts <- counts %>%
      mutate(
        Binomial_p = map_dbl(Count, ~ binom.test(.x, n_cases,
                                                 p = expected_p,
                                                 alternative = "two.sided")$p.value),
        Binomial_p_adj = p.adjust(Binomial_p, method = "bonferroni"),
        Significant    = ifelse(Binomial_p_adj < 0.05, "*", ""),
        Chi_sq_overall = round(chi_result$statistic, 3),
        Chi_sq_p       = round(chi_result$p.value, 4)
      )
    
    counts
  })
  
  # Print a clean summary table
  results %>%
    select(Group, Type, N_cases, Count, Proportion,
           Binomial_p_adj, Significant,
           Chi_sq_overall, Chi_sq_p) %>%
    mutate(Proportion = round(Proportion, 3),
           Binomial_p_adj = round(Binomial_p_adj, 4)) %>%
    print(n = Inf)
  
  invisible(results)
}

# ══════════════════════════════════════════════
# ROW 1 — SUBSISTENCE
# ══════════════════════════════════════════════

within_group_test(nail_data,
                  group_var    = "Subsistence",
                  type_columns = context_columns,
                  label        = "a: Within each Subsistence group — Context frequency")

within_group_test(nail_data,
                  group_var    = "Subsistence",
                  type_columns = finger_columns,
                  label        = "b: Within each Subsistence group — Modification frequency")

# ══════════════════════════════════════════════
# ROW 2 — ECOLOGICAL ZONE
# ══════════════════════════════════════════════

within_group_test(nail_data %>% filter(!is.na(ecological_zones)),
                  group_var    = "ecological_zones",
                  type_columns = context_columns,
                  label        = "c: Within each Ecological Zone — Context frequency")

within_group_test(nail_data %>% filter(!is.na(ecological_zones)),
                  group_var    = "ecological_zones",
                  type_columns = finger_columns,
                  label        = "d: Within each Ecological Zone — Modification frequency")

# ══════════════════════════════════════════════
# ROW 3 — RISK LEVEL
# ══════════════════════════════════════════════

nail_data <- nail_data %>%
  mutate(risk_level = factor(risk_level,
                             levels = c("Low Risk", "Moderate Risk", "High Risk")))

within_group_test(nail_data %>% filter(!is.na(risk_level)),
                  group_var    = "risk_level",
                  type_columns = context_columns,
                  label        = "e: Within each Risk Level — Context frequency")

within_group_test(nail_data %>% filter(!is.na(risk_level)),
                  group_var    = "risk_level",
                  type_columns = finger_columns,
                  label        = "f: Within each Risk Level — Modification frequency")



export_analysis_results <- function(data, file_name = "Analysis_Results.xlsx") {
  
  # 1. Run the analysis and store each result in a named list
  # The names given here will become the tab names in Excel
  results_list <- list(
    "Subsistence_Context"  = within_group_test(data, "Subsistence", context_columns, "A"),
    "Subsistence_Mod"      = within_group_test(data, "Subsistence", finger_columns, "B"),
    "Ecological_Context"   = within_group_test(data, "ecological_zones", context_columns, "C"),
    "Ecological_Mod"       = within_group_test(data, "ecological_zones", finger_columns, "D"),
    "Risk_Context"         = within_group_test(data, "risk_level", context_columns, "E"),
    "Risk_Mod"             = within_group_test(data, "risk_level", finger_columns, "F")
  )
  
  # 2. Export the list to a single Excel file
  write.xlsx(results_list, file = file_name)
  
  message("Success: Results exported to ", file_name)
}

export_analysis_results(nail_data, "Archaeological_Stats_Results.xlsx")



###########bar plot############

# Read the dataset
df <- read.csv("data/nail_geo_env_levels.csv", stringsAsFactors = FALSE)
n_total <- nrow(df)

modification_vars <- c(
  "trimming",
  "long.nail",
  "surface.scraping",
  "painting",
  "extraction",
  "accessories"
)

context_vars <- c(
  "general",
  "status.signaling",
  "ritual",
  "ornamentation",
  "magic",
  "tooling",
  "punishment"
)

# Summarize counts and percentages
summary_df <- bind_rows(
  tibble(variable = modification_vars, category = "Modification"),
  tibble(variable = context_vars, category = "Context")
) %>%
  rowwise() %>%
  mutate(
    n = sum(df[[variable]], na.rm = TRUE),
    pct = (n / n_total) * 100
  ) %>%
  ungroup()

mod_df <- summary_df %>%
  filter(category == "Modification") %>%
  arrange(desc(pct), variable) %>%
  mutate(variable = factor(variable, levels = rev(variable)))

ctx_df <- summary_df %>%
  filter(category == "Context") %>%
  arrange(desc(pct), variable) %>%
  mutate(variable = factor(variable, levels = rev(variable)))

# Panel 1: Modification
p_mod <- ggplot(mod_df, aes(x = variable, y = pct)) +
  geom_col(fill = "#2C5E8A", width = 0.7) +
  geom_text(aes(label = n), hjust = -0.25, size = 3.8) +
  coord_flip() +
  scale_y_continuous(
    limits = c(0, max(mod_df$pct) * 1.15),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "A. Modification",
    x = NULL,
    y = "Percentage of Cases (N = 91)"
  ) +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

# Panel 2: Context
p_ctx <- ggplot(ctx_df, aes(x = variable, y = pct)) +
  geom_col(fill = "#3B7A57", width = 0.7) +
  geom_text(aes(label = n), hjust = -0.25, size = 3.8) +
  coord_flip() +
  scale_y_continuous(
    limits = c(0, max(ctx_df$pct) * 1.15),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "B. Context",
    x = NULL,
    y = "Percentage of Cases (N = 91)"
  ) +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

# Combine into one panel (stacked vertically; use `p_mod + p_ctx` for side-by-side)
combined_plot <- p_mod / p_ctx

print(combined_plot)

# Optional: save to file
ggsave("figure/nail_frequency_panels.png", combined_plot, width = 7, height = 8, dpi = 300)





library(tidyverse)
library(spaMM)

# Prepare factors for predictors
df <- df %>%
  mutate(
    ecological_zones = factor(ecological_zones),
    risk_level = factor(risk_level),
    Subsistence = factor(Subsistence)
  )

# Helper function to fit a spatial mixed effects logistic regression model
# using a Matern correlation matrix over Lat + Long
fit_spatial_logit <- function(data, outcome, predictors = c("ecological_zones", "risk_level", "Subsistence")) {
  rhs <- paste(predictors, collapse = " + ")
  fml <- as.formula(paste0(outcome, " ~ ", rhs, " + Matern(1 | Lat + Long)"))
  
  model_data <- data %>%
    select(all_of(c(outcome, predictors, "Lat", "Long"))) %>%
    drop_na()
  
  fitme(
    formula = fml,
    data = model_data,
    family = binomial(link = "logit")
  )
}

# ---------------------------------------------------------
# 1. MODIFICATION MODELS (separated from context)
# ---------------------------------------------------------
mod_models <- lapply(modification_vars, function(v) {
  message("Fitting modification model for: ", v)
  fit_spatial_logit(df, outcome = v)
})
names(mod_models) <- modification_vars

# Example: inspect long.nail model (directly addresses the tropical / long nail claim)
summary(mod_models[["long.nail"]])

# Extract fixed effects and Matern spatial parameters for all modification models
mod_summaries <- lapply(names(mod_models), function(v) {
  s <- summary(mod_models[[v]])
  fe <- as.data.frame(s$beta_table)
  fe$outcome <- v
  fe$category <- "modification"
  fe$term <- rownames(fe)
  fe
}) %>% bind_rows()

# ---------------------------------------------------------
# 2. CONTEXT MODELS (separated from modification)
# ---------------------------------------------------------
ctx_models <- lapply(context_vars, function(v) {
  message("Fitting context model for: ", v)
  fit_spatial_logit(df, outcome = v)
})
names(ctx_models) <- context_vars

# Example: inspect tooling model
summary(ctx_models[["tooling"]])

# Extract fixed effects and Matern spatial parameters for all context models
ctx_summaries <- lapply(names(ctx_models), function(v) {
  s <- summary(ctx_models[[v]])
  fe <- as.data.frame(s$beta_table)
  fe$outcome <- v
  fe$category <- "context"
  fe$term <- rownames(fe)
  fe
}) %>% bind_rows()

# Optional: compare a spatial model against a non-spatial GLM for a given outcome
# (for instance, testing whether Matern spatial correlation improves fit for long.nail)
mod_data_long <- df %>%
  select(long.nail, ecological_zones, risk_level, Subsistence, Lat, Long) %>%
  drop_na()

m_spatial_long <- fitme(
  long.nail ~ ecological_zones + risk_level + Subsistence + Matern(1 | Lat + Long),
  data = mod_data_long,
  family = binomial(link = "logit")
)

m_nonspatial_long <- fitme(
  long.nail ~ ecological_zones + risk_level + Subsistence,
  data = mod_data_long,
  family = binomial(link = "logit")
)

# Likelihood ratio / AIC comparison
AIC(m_nonspatial_long)
AIC(m_spatial_long)
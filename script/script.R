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
## Load the data set
nail <- read.csv("./data/nail.csv", fileEncoding="latin1")
EAsocieties <- read.csv("./data/eHRAF coverage.csv", fileEncoding="latin1")

## Merge two data frames to extract coordinates
nail_all <- left_join(nail, EAsocieties, by = "Society")
## Remove Bedouin because it does not have geographic coordinates
nail_all <-  nail_all %>% filter(Society != "Bedouin")
# Remove duplicates based on Society
nail_geo <- nail_all %>% distinct(Society, .keep_all = TRUE)
nail_geo <- nail_geo %>%
  mutate(Society = if_else(str_detect(Society, "Abip"), "Abipón", Society))
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
# GLMM analysis
# Ensure categorical variables are factors
# Setting ANS as the reference level so the model predicts the probability of SNS
nail$Count <- factor(nail$Count, levels = c("ANS", "SNS"))
nail$Timelines <- factor(nail$Timelines)
nail$Size <- factor(nail$Size)
nail$Reliance <- factor(nail$Reliance)
nail$Society <- factor(nail$Society)

# Fit the multi-level logistic regression model
# We include Society as a random intercept (1 | Society)
# Fixed effects: Timelines, Size, Reliance
model <- glmer(Count ~ Timelines + Size + Reliance + (1 | Society), 
               data = nail, 
               family = binomial(link = "logit"))

# Display the summary of the model results
summary(model)


# Comprehensive diagnostic plots using the 'performance' package
check_model(model)
ggplot2::ggsave("./figure/modelcheck.jpg", dpi = 300)



# Calculate Odds Ratios for easier interpretation
exp(fixef(model))

# Tidy the model results into a data frame
model_output <- tidy(model, conf.int = TRUE, effects = "fixed")

# Save to CSV
write.csv(model_output, "data/model_results.csv", row.names = FALSE)

# Forest Plot for Visualizing Effect Sizes
plot_model(model, 
           show.values = TRUE, 
           value.offset = .3,
           title = "Predictors of Symbolic Number Systems (SNS)",
           axis.labels = c("Low Reliance", "Small Size", "Late Timeline"),
           vline.color = "grey") +
  theme_minimal()
ggplot2::ggsave("./figure/Fig.2.jpg", width = 30, height = 20, units = "cm", dpi = 300)

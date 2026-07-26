# Run the data-preparation and model-fitting scripts before this script.

library(mgcv)
library(dplyr)
library(ggplot2)
library(sf)
#confirm crs
x_range <- range(rent_data$centroid_x, na.rm = TRUE)
y_range <- range(rent_data$centroid_y, na.rm = TRUE)

x_range
y_range
gk_zone <- floor(median(rent_data$centroid_x, na.rm = TRUE) / 1e6)
gk_zone

###
rent_points <- st_as_sf(
  rent_data,
  coords = c("centroid_x", "centroid_y"),
  crs = 5678,
  remove = FALSE
)




###
data_crs <- 5678

##get boundary of munich and districts
munich_cache <- "munich_districts.geojson"

if (file.exists(munich_cache)) {
  
  munich_raw <- st_read(
    munich_cache,
    quiet = TRUE
  )
  
} else {
  
  munich_url <- paste0(
    "https://geoportal.muenchen.de/geoserver/gsm_wfs/ows?",
    "service=WFS&",
    "version=1.0.0&",
    "request=GetFeature&",
    "typeName=gsm_wfs:vablock_stadtbezirk&",
    "outputFormat=application/json&"
  )
  
  munich_raw <- st_read(
    munich_url,
    quiet = TRUE
  )
  
  st_write(
    munich_raw,
    munich_cache,
    quiet = TRUE
  )
}



##transform Crs and check for duplicated districts
munich_raw
munich_clean <- munich_raw %>%
  st_transform(crs = data_crs) %>%
  group_by(sb_nummer, sb_name) %>%
  summarise(
    geometry = st_union(geometry),
    .groups = "drop"
  )


stopifnot(nrow(munich_clean) == 25)

##check crs

plot(st_geometry(munich_clean))
plot(
  st_geometry(rent_points),
  add = TRUE,
  pch = 16,
  cex = 0.2
)




#build boundary
munich_border <- st_union(munich_clean)
munich_border


##find a point in each district

label_geometry <- suppressWarnings(
  st_point_on_surface(st_geometry(munich_clean))
)

label_coordinates <- st_coordinates(label_geometry)
label_coordinates
munich_centers <- munich_clean %>%
  st_drop_geometry() %>%
  mutate(
    label_x = label_coordinates[, 1],
    label_y = label_coordinates[, 2]
  )
munich_centers

# district number
district_legend <- munich_clean %>%
  st_drop_geometry() %>%
  select(sb_nummer, sb_name) %>%
  arrange(sb_nummer)

district_legend


#create munich grid

bbox <- st_bbox(munich_border)
bbox
grid <- expand.grid(
  centroid_x = seq(
    bbox["xmin"],
    bbox["xmax"],
    length.out = 200
  ),
  centroid_y = seq(
    bbox["ymin"],
    bbox["ymax"],
    length.out = 200
  )
)

grid_sf <- st_as_sf(
  grid,
  coords = c("centroid_x", "centroid_y"),
  crs = data_crs,
  remove = FALSE
)

inside <- lengths(
  st_intersects(grid_sf, munich_border)
) > 0

grid_munich <- grid[inside, ]




##delate duplicate location from  our data
observed_locations <- rent_data %>%
  select(centroid_x, centroid_y) %>%
  distinct()

##mask for m1 and m3
grid_munich$too_far <- exclude.too.far(
  g1 = grid_munich$centroid_x,
  g2 = grid_munich$centroid_y,
  d1 = observed_locations$centroid_x,
  d2 = observed_locations$centroid_y,
  dist = 0.03
)

table(grid_munich$too_far)

## mask for m2
year_mask <- function(year_value) {
  
  observed_locations_year <- rent_data %>%
    filter(MSP_f == as.character(year_value)) %>%
    select(centroid_x, centroid_y) %>%
    distinct()
  
  exclude.too.far(
    g1 = grid_munich$centroid_x,
    g2 = grid_munich$centroid_y,
    d1 = observed_locations_year$centroid_x,
    d2 = observed_locations_year$centroid_y,
    dist = 0.03
  )
}

mask_2021 <- year_mask("2021")
mask_2023 <- year_mask("2023")
mask_2025 <- year_mask("2025")


## munich map with district numbers
p_base <- ggplot() +
  
  geom_sf(
    data = munich_clean,
    fill = "gray95",
    color = "white",
    linewidth = 0.5
  ) +
  
  geom_sf(
    data = munich_border,
    fill = NA,
    color = "black",
    linewidth = 0.8
  ) +
  
  geom_text(
    data = munich_centers,
    aes(
      x = label_x,
      y = label_y,
      label = sb_nummer
    ),
    size = 3.5,
    fontface = "bold",
    color = "black"
  ) +
  
  coord_sf(datum = NA) +
  
  theme_void()

p_base

## predict
make_pred_grid <- function(
    grid_munich,
    original_data,
    year
) {
  
  grid_new <- grid_munich
  
  grid_new$wfl.gekappt <- median(
    original_data$wfl.gekappt
  )
  
  grid_new$age <- median(
    original_data$age
  )
  
  year_levels <- levels(original_data$MSP_f)
  
  grid_new$MSP_f <- factor(
    year,
    levels = year_levels
  )
  
  grid_new$MSP_f_ordered <- ordered(
    year,
    levels = year_levels
  )
  
  grid_new
}


#Identify spatial-term columns


find_spatial_columns <- function(term_names) {
  
  grep(
    "centroid_x.*centroid_y|centroid_y.*centroid_x",
    term_names
  )
}

#Extract the complete spatial contribution
extract_spatial_effect <- function(
    model,
    newdata
) {
  
  pred_terms <- predict(
    model,
    newdata = newdata,
    type = "terms"
  )
  
  spatial_cols <- find_spatial_columns(
    colnames(pred_terms)
  )
  
  if (length(spatial_cols) == 0) {
    stop(
      "no spatial smooth was found"
    )
  }
  rowSums(
    pred_terms[, spatial_cols, drop = FALSE]
  )
  #rowsum for m2 zb : 0 + a + 0 = a
}

## for m3
extract_difference_smooth <- function(
    model,
    newdata,
    difference_year
) {
  
  pred_terms <- predict(
    model,
    newdata = newdata,
    type = "terms"
  )
  
  term_names <- colnames(pred_terms)
  
  spatial_cols <- find_spatial_columns(term_names)
  
  difference_cols <- spatial_cols[
    grepl(
      difference_year,
      term_names[spatial_cols]
    )
  ]
  
  if (length(difference_cols) != 1) {
    
    stop(
      "Expected exactly one difference smooth ")
    
  }
  
  as.numeric(
    pred_terms[, difference_cols]
  )
}


#color scale
get_effect_limits <- function(x) {
  
  x <- x[
    is.finite(x)
  ]
 
  range(x)
}
fill_scale <- function(
    effect_limits,
    legend_name
) {
  
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    na.value = "grey90",
    limits = effect_limits,
    
    breaks = c(
      effect_limits[1],
      0,
      effect_limits[2]
    ),
    
    labels = scales::label_number(
      accuracy = 0.01
    ),
    name = legend_name
  )
}


#map theme
theme_map <- theme_minimal(base_size = 14) +
  
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    
    strip.text = element_text(
      face = "bold",
      size = 15
    ),
    
    strip.background = element_blank(),
    
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    
    plot.caption = element_text(
      size = 9,
      colour = "grey40"
    ),
    
    legend.position = "right"
  )

###Figure1 with M1
grid_m1 <- make_pred_grid(
  grid_munich,
  original_data = rent_data,
  year = "2021"
)

grid_m1$spatial_effect <- extract_spatial_effect(
  model = M1,
  newdata = grid_m1
)

grid_m1$spatial_effect[
  grid_m1$too_far
] <- NA
limits_m1 <- get_effect_limits(
  grid_m1$spatial_effect
)
limits_m1
p_m1 <- ggplot() +
  
  geom_tile(
    data = grid_m1,
    aes(
      x = centroid_x,
      y = centroid_y,
      fill = spatial_effect
    )
  ) +
  
  geom_sf(
    data = munich_clean,
    fill = NA,
    color = "white",
    linewidth = 0.25
  ) +
  
  geom_sf(
    data = munich_border,
    fill = NA,
    color = "black",
    linewidth = 0.6
  ) +
  
  geom_text(
    data = munich_centers,
    aes(
      x = label_x,
      y = label_y,
      label = sb_nummer
    ),
    size = 3.5,
    fontface = "bold",
    color = "black"
  ) +
  
  fill_scale(
    effect_limits = limits_m1,
    legend_name = "Spatial effect\n(log-link scale)"
  ) +
  
  coord_sf(datum = NA) +
  
  labs(
    title = "Overall Spatial Effect on Rent in Munich",
    subtitle = paste0(
      "Model 1: common spatial smooth across 2021, 2023 and 2025"
    ),
    caption = paste0(
      "Grey areas are too far from observed locations and are ",
      "masked to limit spatial extrapolation."
    ),
    x = NULL,
    y = NULL
  ) +
  
  theme_map

p_m1

p_m1_clean <- p_m1 +
  labs(
    title = NULL,
    subtitle = NULL,
    caption = NULL
  )

ggsave(
  filename = "spatial_effect_m1.pdf",
  plot = p_m1_clean,
  width = 9,
  height = 5,
  units = "in"
)


##Figure2 with M2

get_year_spatial <- function(
    model,
    year_string,
    grid_munich,
    original_data,
    exclusion_mask
) {
  
  grid_year <- make_pred_grid(
    grid_munich,
    original_data = original_data,
    year = year_string
  )
  
  grid_year$spatial_effect <- extract_spatial_effect(
    model = model,
    newdata = grid_year
  )
  

  grid_year$spatial_effect[
    exclusion_mask
  ] <- NA
  
  grid_year$year <- year_string
  
  grid_year
}


sp_2021 <- get_year_spatial(
  model = M2,
  year_string = "2021",
  grid_munich,
  original_data = rent_data,
  exclusion_mask = mask_2021
)

sp_2023 <- get_year_spatial(
  model = M2,
  year_string = "2023",
  grid_munich,
  original_data = rent_data,
  exclusion_mask = mask_2023
)

sp_2025 <- get_year_spatial(
  model = M2,
  year_string = "2025",
  grid_munich,
  original_data = rent_data,
  exclusion_mask = mask_2025
)


sp_m2_all <- bind_rows(
  sp_2021,
  sp_2023,
  sp_2025
)
limits_m2 <- get_effect_limits(
  sp_m2_all$spatial_effect
)
limits_m2

sp_m2_all$year <- factor(
  sp_m2_all$year,
  levels = c("2021", "2023", "2025")
)



p_m2 <- ggplot() +
  
  geom_tile(
    data = sp_m2_all,
    aes(
      x = centroid_x,
      y = centroid_y,
      fill = spatial_effect
    )
  ) +
  
  geom_sf(
    data = munich_clean,
    fill = NA,
    color = "white",
    linewidth = 0.25
  ) +
  
  geom_sf(
    data = munich_border,
    fill = NA,
    color = "black",
    linewidth = 0.5
  ) +
  
  facet_wrap(
    ~ year,
    nrow = 1
  ) +
  
  fill_scale(
    effect_limits = limits_m2,
    legend_name = "Spatial effect\n(log-link scale)"
  ) +
  
  coord_sf(datum = NA) +
  
  labs(
    title = "Year-Specific Spatial Effects on Rent in Munich",
    subtitle = paste0(
      "Model 2: one independently estimated spatial surface per year; ",
      "all panels use the same colour scale"
    ),
    caption = paste0(
      "Each panel shows the spatial pattern estimated from the corresponding ",
      "year. Grey areas indicate locations with limited spatial support ",
      "in that year."
    ),
    x = NULL,
    y = NULL
  ) +
  
  theme_map +
  
  theme(
    panel.spacing = grid::unit(
      0.5,
      "lines"
    )
  )

p_m2
p_m2_clean <- p_m2 +
  labs(
    title = NULL,
    subtitle = NULL,
    caption = NULL
  )

ggsave(
  filename = "spatial_effect_m2.pdf",
  plot = p_m2_clean,
  width = 9,
  height = 5,
  units = "in"
)


##Figure 3 with M3

get_difference_map <- function(
    model,
    difference_year,
    grid_munich,
    original_data
) {
  
  grid_difference <- make_pred_grid(
    grid_munich,
    original_data = original_data,
    year = difference_year
  )
 
  
  grid_difference$diff_effect <- extract_difference_smooth(
    model = model,
    newdata = grid_difference,
    difference_year = difference_year
  )
  

  
  grid_difference$diff_effect[
    grid_difference$too_far
  ] <- NA
  
  grid_difference$comparison <- paste0(
    difference_year,
    " vs 2021"
  )
  
  grid_difference
}


diff_2023 <- get_difference_map(
  model = M3,
  difference_year = "2023",
  grid_munich,
  original_data = rent_data
)

diff_2025 <- get_difference_map(
  model = M3,
  difference_year = "2025",
  grid_munich,
  original_data = rent_data
)


sp_diff_all <- bind_rows(
  diff_2023,
  diff_2025
)

limits_m3 <- get_effect_limits(
  sp_diff_all$diff_effect
)

limits_m3

sp_diff_all$comparison <- factor(
  sp_diff_all$comparison,
  levels = c(
    "2023 vs 2021",
    "2025 vs 2021"
  )
)


p_diff <- ggplot() +
  
  geom_tile(
    data = sp_diff_all,
    aes(
      x = centroid_x,
      y = centroid_y,
      fill = diff_effect
    )
  ) +
  
  geom_sf(
    data = munich_clean,
    fill = NA,
    color = "white",
    linewidth = 0.25
  ) +
  
  geom_sf(
    data = munich_border,
    fill = NA,
    color = "black",
    linewidth = 0.5
  ) +
  
  facet_wrap(
    ~ comparison,
    nrow = 1
  ) +
  
  fill_scale(
    effect_limits = limits_m3,
    legend_name = "Difference in\nspatial effect"
  ) +
  
  coord_sf(datum = NA) +
  
  labs(
    title = "Changes in the Spatial Rent Pattern Relative to 2021",
    subtitle = paste0(
      "Model 3 ordered-factor difference smooths; positive values indicate ",
      "a more positive spatial contribution relative to 2021"
    ),
    caption = paste0(
      "The maps show estimated differences descriptively. ",
      "Statistical conclusions should be based on the overall smooth tests. ",
      "Grey areas are masked to limit extrapolation."
    ),
    x = NULL,
    y = NULL
  ) +
  
  theme_map +
  
  theme(
    panel.spacing = grid::unit(
      1,
      "lines"
    )
  )

p_diff
p_diff_clean <- p_diff +
  labs(
    title = NULL,
    subtitle = NULL,
    caption = NULL
  )

ggsave(
  filename = "spatial_effect_m3.pdf",
  plot = p_diff_clean,
  width = 9,
  height = 5,
  units = "in"
)


## Figure 1 with M1_k60

grid_m1_k60 <- make_pred_grid(
  grid_munich,
  original_data = rent_data,
  year = "2021"
)

grid_m1_k60$spatial_effect <- extract_spatial_effect(
  model = M1_k60,
  newdata = grid_m1_k60
)

grid_m1_k60$spatial_effect[
  grid_m1_k60$too_far
] <- NA

limits_m1_k60 <- get_effect_limits(
  grid_m1_k60$spatial_effect
)

limits_m1_k60


p_m1_k60 <- ggplot() +
  
  geom_tile(
    data = grid_m1_k60,
    aes(
      x = centroid_x,
      y = centroid_y,
      fill = spatial_effect
    )
  ) +
  
  geom_sf(
    data = munich_clean,
    fill = NA,
    color = "white",
    linewidth = 0.25
  ) +
  
  geom_sf(
    data = munich_border,
    fill = NA,
    color = "black",
    linewidth = 0.6
  ) +
  
  geom_text(
    data = munich_centers,
    aes(
      x = label_x,
      y = label_y,
      label = sb_nummer
    ),
    size = 3.5,
    fontface = "bold",
    color = "black"
  ) +
  
  fill_scale(
    effect_limits = limits_m1_k60,
    legend_name = "Spatial effect\n(log-link scale)"
  ) +
  
  coord_sf(datum = NA) +
  
  labs(
    title = "Overall Spatial Effect on Rent in Munich",
    subtitle = paste0(
      "Model 1: common spatial smooth across 2021, 2023 and 2025"
    ),
    caption = paste0(
      "Grey areas are too far from observed locations and are ",
      "masked to limit spatial extrapolation."
    ),
    x = NULL,
    y = NULL
  ) +
  
  theme_map


p_m1_k60
p_m1_k60_clean <- p_m1_k60 +
  labs(
    title = NULL,
    subtitle = NULL,
    caption = NULL
  )

ggsave(
  filename = "spatial_effect_m1_k60.pdf",
  plot = p_m1_k60_clean,
  width = 9,
  height = 5,
  units = "in"
)

## Figure 2 with M2_k60

sp_2021_k60 <- get_year_spatial(
  model = M2_k60,
  year_string = "2021",
  grid_munich = grid_munich,
  original_data = rent_data,
  exclusion_mask = mask_2021
)

sp_2023_k60 <- get_year_spatial(
  model = M2_k60,
  year_string = "2023",
  grid_munich = grid_munich,
  original_data = rent_data,
  exclusion_mask = mask_2023
)

sp_2025_k60 <- get_year_spatial(
  model = M2_k60,
  year_string = "2025",
  grid_munich = grid_munich,
  original_data = rent_data,
  exclusion_mask = mask_2025
)


sp_m2_k60_all <- bind_rows(
  sp_2021_k60,
  sp_2023_k60,
  sp_2025_k60
)


limits_m2_k60 <- get_effect_limits(
  sp_m2_k60_all$spatial_effect
)

limits_m2_k60


sp_m2_k60_all$year <- factor(
  sp_m2_k60_all$year,
  levels = c("2021", "2023", "2025")
)


p_m2_k60 <- ggplot() +
  
  geom_tile(
    data = sp_m2_k60_all,
    aes(
      x = centroid_x,
      y = centroid_y,
      fill = spatial_effect
    )
  ) +
  
  geom_sf(
    data = munich_clean,
    fill = NA,
    color = "white",
    linewidth = 0.25
  ) +
  
  geom_sf(
    data = munich_border,
    fill = NA,
    color = "black",
    linewidth = 0.5
  ) +
  
  facet_wrap(
    ~ year,
    nrow = 1
  ) +
  
  fill_scale(
    effect_limits = limits_m2_k60,
    legend_name = "Spatial effect\n(log-link scale)"
  ) +
  
  coord_sf(datum = NA) +
  
  labs(
    title = "Year-Specific Spatial Effects on Rent in Munich",
    subtitle = paste0(
      "Model 2: one independently estimated spatial surface per year; ",
      "all panels use the same colour scale"
    ),
    caption = paste0(
      "Each panel shows the spatial pattern estimated from the corresponding ",
      "year. Grey areas indicate locations with limited spatial support ",
      "in that year."
    ),
    x = NULL,
    y = NULL
  ) +
  
  theme_map +
  
  theme(
    panel.spacing = grid::unit(
      0.5,
      "lines"
    )
  )


p_m2_k60

p_m2_k60_clean <- p_m2_k60 +
  labs(
    title = NULL,
    subtitle = NULL,
    caption = NULL
  )

ggsave(
  filename = "spatial_effect_m2_k60.pdf",
  plot = p_m2_k60_clean,
  width = 9,
  height = 5,
  units = "in"
)

## Figure 3 with M3_k60

diff_2023_k60 <- get_difference_map(
  model = M3_k60,
  difference_year = "2023",
  grid_munich = grid_munich,
  original_data = rent_data
)

diff_2025_k60 <- get_difference_map(
  model = M3_k60,
  difference_year = "2025",
  grid_munich = grid_munich,
  original_data = rent_data
)


sp_diff_k60_all <- bind_rows(
  diff_2023_k60,
  diff_2025_k60
)


limits_m3_k60 <- get_effect_limits(
  sp_diff_k60_all$diff_effect
)

limits_m3_k60


sp_diff_k60_all$comparison <- factor(
  sp_diff_k60_all$comparison,
  levels = c(
    "2023 vs 2021",
    "2025 vs 2021"
  )
)


p_diff_k60 <- ggplot() +
  
  geom_tile(
    data = sp_diff_k60_all,
    aes(
      x = centroid_x,
      y = centroid_y,
      fill = diff_effect
    )
  ) +
  
  geom_sf(
    data = munich_clean,
    fill = NA,
    color = "white",
    linewidth = 0.25
  ) +
  
  geom_sf(
    data = munich_border,
    fill = NA,
    color = "black",
    linewidth = 0.5
  ) +
  
  facet_wrap(
    ~ comparison,
    nrow = 1
  ) +
  
  fill_scale(
    effect_limits = limits_m3_k60,
    legend_name = "Difference in\nspatial effect"
  ) +
  
  coord_sf(datum = NA) +
  
  labs(
    title = "Changes in the Spatial Rent Pattern Relative to 2021",
    subtitle = paste0(
      "Model 3 ordered-factor difference smooths; positive values indicate ",
      "a more positive spatial contribution relative to 2021"
    ),
    caption = paste0(
      "The maps show estimated differences descriptively. ",
      "Statistical conclusions should be based on the overall smooth tests. ",
      "Grey areas are masked to limit extrapolation."
    ),
    x = NULL,
    y = NULL
  ) +
  
  theme_map +
  
  theme(
    panel.spacing = grid::unit(
      1,
      "lines"
    )
  )


p_diff_k60

p_diff_k60_clean <- p_diff_k60 +
  labs(
    title = NULL,
    subtitle = NULL,
    caption = NULL
  )

ggsave(
  filename = "spatial_effect_m3_k60.pdf",
  plot = p_diff_k60_clean,
  width = 9,
  height = 5,
  units = "in"
)


## Figure 1 with M1_k120


grid_m1_k120 <- make_pred_grid(
  grid_munich,
  original_data = rent_data,
  year = "2021"
)


grid_m1_k120$spatial_effect <- extract_spatial_effect(
  model = M1_k120,
  newdata = grid_m1_k120
)


grid_m1_k120$spatial_effect[
  grid_m1_k120$too_far
] <- NA


limits_m1_k120 <- get_effect_limits(
  grid_m1_k120$spatial_effect
)

limits_m1_k120


p_m1_k120 <- ggplot() +
  
  geom_tile(
    data = grid_m1_k120,
    aes(
      x = centroid_x,
      y = centroid_y,
      fill = spatial_effect
    )
  ) +
  
  geom_sf(
    data = munich_clean,
    fill = NA,
    color = "white",
    linewidth = 0.25
  ) +
  
  geom_sf(
    data = munich_border,
    fill = NA,
    color = "black",
    linewidth = 0.6
  ) +
  
  geom_text(
    data = munich_centers,
    aes(
      x = label_x,
      y = label_y,
      label = sb_nummer
    ),
    size = 3.5,
    fontface = "bold",
    color = "black"
  ) +
  
  fill_scale(
    effect_limits = limits_m1_k120,
    legend_name = "Spatial effect\n(log-link scale)"
  ) +
  
  coord_sf(datum = NA) +
  
  labs(
    title = "Overall Spatial Effect on Rent in Munich",
    subtitle = paste0(
      "Model 1: common spatial smooth across 2021, 2023 and 2025"
    ),
    caption = paste0(
      "Grey areas are too far from observed locations and are ",
      "masked to limit spatial extrapolation."
    ),
    x = NULL,
    y = NULL
  ) +
  
  theme_map


p_m1_k120
p_m1_k120_clean <- p_m1_k120 +
  labs(
    title = NULL,
    subtitle = NULL,
    caption = NULL
  )

ggsave(
  filename = "spatial_effect_m1_k120.pdf",
  plot = p_m1_k120_clean,
  width = 9,
  height = 5,
  units = "in"
)

##figure 2 mit m2 k120


sp_2021_k120 <- get_year_spatial(
  model = M2_k120,
  year_string = "2021",
  grid_munich = grid_munich,
  original_data = rent_data,
  exclusion_mask = mask_2021
)


sp_2023_k120 <- get_year_spatial(
  model = M2_k120,
  year_string = "2023",
  grid_munich = grid_munich,
  original_data = rent_data,
  exclusion_mask = mask_2023
)


sp_2025_k120 <- get_year_spatial(
  model = M2_k120,
  year_string = "2025",
  grid_munich = grid_munich,
  original_data = rent_data,
  exclusion_mask = mask_2025
)


sp_m2_k120_all <- bind_rows(
  sp_2021_k120,
  sp_2023_k120,
  sp_2025_k120
)


limits_m2_k120 <- get_effect_limits(
  sp_m2_k120_all$spatial_effect
)

limits_m2_k120


sp_m2_k120_all$year <- factor(
  sp_m2_k120_all$year,
  levels = c("2021", "2023", "2025")
)


p_m2_k120 <- ggplot() +
  
  geom_tile(
    data = sp_m2_k120_all,
    aes(
      x = centroid_x,
      y = centroid_y,
      fill = spatial_effect
    )
  ) +
  
  geom_sf(
    data = munich_clean,
    fill = NA,
    color = "white",
    linewidth = 0.25
  ) +
  
  geom_sf(
    data = munich_border,
    fill = NA,
    color = "black",
    linewidth = 0.5
  ) +
  
  facet_wrap(
    ~ year,
    nrow = 1
  ) +
  
  fill_scale(
    effect_limits = limits_m2_k120,
    legend_name = "Spatial effect\n(log-link scale)"
  ) +
  
  coord_sf(datum = NA) +
  
  labs(
    title = "Year-Specific Spatial Effects on Rent in Munich",
    subtitle = paste0(
      "Model 2: one independently estimated spatial surface per year; ",
      "all panels use the same colour scale"
    ),
    caption = paste0(
      "Each panel shows the spatial pattern estimated from the corresponding ",
      "year. Grey areas indicate locations with limited spatial support ",
      "in that year."
    ),
    x = NULL,
    y = NULL
  ) +
  
  theme_map +
  
  theme(
    panel.spacing = grid::unit(
      0.5,
      "lines"
    )
  )


p_m2_k120
p_m2_k120_clean <- p_m2_k120 +
  labs(
    title = NULL,
    subtitle = NULL,
    caption = NULL
  )

ggsave(
  filename = "spatial_effect_m2_k120.pdf",
  plot = p_m2_k120_clean,
  width = 9,
  height = 5,
  units = "in"
)



## Figure 3 with M3_k120_diff20


diff_2023_k120_diff20 <- get_difference_map(
  model = M3_k120_diff20,
  difference_year = "2023",
  grid_munich = grid_munich,
  original_data = rent_data
)


diff_2025_k120_diff20 <- get_difference_map(
  model = M3_k120_diff20,
  difference_year = "2025",
  grid_munich = grid_munich,
  original_data = rent_data
)


sp_diff_k120_diff20_all <- bind_rows(
  diff_2023_k120_diff20,
  diff_2025_k120_diff20
)


limits_m3_k120_diff20 <- get_effect_limits(
  sp_diff_k120_diff20_all$diff_effect
)

limits_m3_k120_diff20


sp_diff_k120_diff20_all$comparison <- factor(
  sp_diff_k120_diff20_all$comparison,
  levels = c(
    "2023 vs 2021",
    "2025 vs 2021"
  )
)


p_diff_k120_diff20 <- ggplot() +
  
  geom_tile(
    data = sp_diff_k120_diff20_all,
    aes(
      x = centroid_x,
      y = centroid_y,
      fill = diff_effect
    )
  ) +
  
  geom_sf(
    data = munich_clean,
    fill = NA,
    color = "white",
    linewidth = 0.25
  ) +
  
  geom_sf(
    data = munich_border,
    fill = NA,
    color = "black",
    linewidth = 0.5
  ) +
  
  facet_wrap(
    ~ comparison,
    nrow = 1
  ) +
  
  fill_scale(
    effect_limits = limits_m3_k120_diff20,
    legend_name = "Difference in\nspatial effect"
  ) +
  
  coord_sf(datum = NA) +
  
  labs(
    title = "Changes in the Spatial Rent Pattern Relative to 2021",
    subtitle = paste0(
      "Model 3 ordered-factor difference smooths; positive values indicate ",
      "a more positive spatial contribution relative to 2021"
    ),
    caption = paste0(
      "The maps show estimated differences descriptively. ",
      "Statistical conclusions should be based on the overall smooth tests. ",
      "Grey areas are masked to limit extrapolation."
    ),
    x = NULL,
    y = NULL
  ) +
  
  theme_map +
  
  theme(
    panel.spacing = grid::unit(
      1,
      "lines"
    )
  )


p_diff_k120_diff20
p_diff_k120_diff20_clean <- p_diff_k120_diff20 +
  labs(
    title = NULL,
    subtitle = NULL,
    caption = NULL
  )

ggsave(
  filename = "spatial_effect_m3_k120.pdf",
  plot = p_diff_k120_diff20_clean,
  width = 9,
  height = 5,
  units = "in"
)

library(dplyr)
library(ggplot2)

# Replace the file paths in the three load() statements below with the local paths to the corresponding data files.

load("~/Desktop/daten_cai_21.RData")
load("~/Desktop/daten_cai_23.RData")
load("~/Desktop/daten_cai_25.RData")


ls()
names(daten_cai.21)
names(daten_cai.23)
names(daten_cai.25)

dim(daten_cai.21)
dim(daten_cai.23)
dim(daten_cai.25)

#DATA PREPARING
#age
daten_cai.21$bj <- as.numeric(as.character(daten_cai.21$bj))
daten_cai.23$bj <- as.numeric(as.character(daten_cai.23$bj))
daten_cai.25$bj <- as.numeric(as.character(daten_cai.25$bj))

daten_cai.21$age <- 2021 - daten_cai.21$bj
daten_cai.23$age <- 2023 - daten_cai.23$bj
daten_cai.25$age <- 2025 - daten_cai.25$bj

sum(is.na(daten_cai.21$bj))
sum(is.na(daten_cai.23$bj))
sum(is.na(daten_cai.25$bj))

summary(daten_cai.21$age)
summary(daten_cai.23$age)
summary(daten_cai.25$age)



# pooled data

rent_data <- bind_rows(
  daten_cai.21,
  daten_cai.23,
  daten_cai.25
)

#factor_MSP
rent_data$MSP_f <- factor(
  rent_data$MSP,
  levels = c(2021, 2023, 2025)
)
rent_data$MSP_f_ordered <- factor(
  rent_data$MSP,
  levels = c(2021, 2023, 2025),
  ordered = TRUE
)

#factor bezirk
rent_data$Bezirk_f <- factor(rent_data$Bezirk)


#data check
vars_core <- c(
  "nmqm",
  "wfl.gekappt",
  "bj",
  "age",
  "centroid_x",
  "centroid_y",
  "MSP"
)
vars_core

##missing check
missing_check <- sapply(
  rent_data[vars_core],
  function(x) sum(is.na(x))
)

missing_check

#Check for non-finite values
finite_check <- sapply(
  rent_data[c(
    "nmqm",
    "wfl.gekappt",
    "bj",
    "age",
    "centroid_x",
    "centroid_y"
  )],
  function(x) sum(!is.finite(x))
)

finite_check

##positive, range check
data_quality <- rent_data %>%
  summarise(
    n = n(),
    
    rent_non_positive = sum(nmqm <= 0, na.rm = TRUE),
    area_non_positive = sum(wfl.gekappt <= 0, na.rm = TRUE),
    age_negative = sum(age < 0, na.rm = TRUE),
    
    rent_min = min(nmqm, na.rm = TRUE),
    rent_max = max(nmqm, na.rm = TRUE),
    
    area_min = min(wfl.gekappt, na.rm = TRUE),
    area_max = max(wfl.gekappt, na.rm = TRUE),
    
    age_min = min(age, na.rm = TRUE),
    age_max = max(age, na.rm = TRUE)
  )

data_quality



# 5. Coordinate range check

coord_check <- rent_data %>%
  summarise(
    x_min = min(centroid_x, na.rm = TRUE),
    x_max = max(centroid_x, na.rm = TRUE),
    y_min = min(centroid_y, na.rm = TRUE),
    y_max = max(centroid_y, na.rm = TRUE),
    
    n_unique_locations = n_distinct(
      centroid_x,
      centroid_y
    )
  )

coord_check


#response check (skewness, mean, median)
skewness_fun <- function(x) {
  x <- x[is.finite(x)]
  mean(
    (x - mean(x))^3
  ) / sd(x)^3
}

response_check <- rent_data %>%
  summarise(
    mean = mean(nmqm, na.rm = TRUE),
    median = median(nmqm, na.rm = TRUE),
    sd = sd(nmqm, na.rm = TRUE),
    skewness = skewness_fun(nmqm)
  )
response_check

response_by_year <- rent_data %>%
  group_by(MSP_f) %>%
  summarise(
    n = n(),
    mean = mean(nmqm, na.rm = TRUE),
    median = median(nmqm, na.rm = TRUE),
    sd = sd(nmqm, na.rm = TRUE),
    skewness = skewness_fun(nmqm),
    .groups = "drop"
  )

response_by_year
distribution <- ggplot(
  rent_data,
  aes(x = nmqm)
) +
  geom_histogram(
    bins = 30,
    colour = "white"
  ) +
  labs(
    x = "Net rent per square metre (EUR)",
    y = "Count",
    title = "Distribution of Net Rent per Square Metre"
  ) +
  theme_minimal()

#visual exploration
#area
area <- ggplot(
  rent_data,
  aes(
    x = wfl.gekappt,
    y = nmqm
  )
) +
  geom_point(
    alpha = 0.08,
    size = 0.7
  ) +
  geom_smooth(
    method = "loess",
    se = FALSE
  ) +
  facet_wrap(
    ~ MSP_f
  ) +
  labs(
    x = "Living area (square metres)",
    y = "Net rent per square metre (EUR)",
    title = "Exploratory Relationship Between Living Area and Rent"
  ) +
  theme_minimal()
#age
age <- ggplot(
  rent_data,
  aes(
    x = age,
    y = nmqm
  )
) +
  geom_point(
    alpha = 0.08,
    size = 0.7
  ) +
  geom_smooth(
    method = "loess",
    se = FALSE
  ) +
  facet_wrap(
    ~ MSP_f
  ) +
  labs(
    x = "Building age (years)",
    y = "Net rent per square metre (EUR)",
    title = "Exploratory Relationship Between Building Age and Rent"
  ) +
  theme_minimal()

print(distribution)
print(area)
print(age)

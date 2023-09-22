## John Snow cholera map
## John Snow Society 210th Anniversary Cholera Map Competition
## https://johnsnowsociety.org/broadsheets-lectures-social/#mapcompetition
## Deadline 2023-09-27
## Last update 2023-09-22

## Load packages ----

library(tidyverse)
library(HistData)

## Import the datasets ----

# broad_st_pump <- HistData::Snow.pumps |>
#   as_tibble() |>
#   filter(label == "Broad St") |>
#   select(x, y)

deaths <- HistData::Snow.deaths |>
  as_tibble() |>
  rename(x_case = x, y_case = y)

pumps <- HistData::Snow.pumps |>
  as_tibble() |>
  rename(x_pump = x, y_pump = y)

pumps_rep <- bind_rows(replicate(nrow(deaths),
                                 pumps, simplify = FALSE)) |>
  mutate(case = rep(deaths$case, each = nrow(pumps)),
         .before = pump)

cases_pumps <- pumps_rep |>
  left_join(deaths) |>
  mutate(dist = sqrt((x_case - x_pump)^2 + (y_case - y_pump)^2))

min_dist <- cases_pumps |>
  slice_min(dist, by = case)

p <- ggplot() +
  geom_point(data = filter(min_dist, !label %in% c("Broad St", "So Soho", "Crown Chapel",
                                                   "Briddle St")),
             aes(x = x_case, y = y_case),
             col = "grey90", size = 0.2) +
  geom_point(data = filter(min_dist, label == "Broad St"),
             aes(x = x_case, y = y_case),
             col = "cyan4", size = 0.2) +
  geom_point(data = filter(min_dist, label == "So Soho"),
             aes(x = x_case, y = y_case),
             col = "darkorange", size = 0.2) +
  geom_point(data = filter(min_dist, label == "Crown Chapel"),
             aes(x = x_case, y = y_case),
             col = "purple", size = 0.2) +
  geom_point(data = filter(min_dist, label == "Briddle St"),
             aes(x = x_case, y = y_case),
             col = "gold2", size = 0.2) +
  geom_segment(data = filter(min_dist, label == "Broad St"),
               aes(x = x_case, xend = x_pump,
                   y = y_case, yend = y_pump),
               col = "cyan4", linewidth = 0.1) +
  geom_segment(data = filter(min_dist, label == "So Soho"),
               aes(x = x_case, xend = x_pump,
                   y = y_case, yend = y_pump),
               col = "darkorange", linewidth = 0.1) +
  geom_segment(data = filter(min_dist, label == "Crown Chapel"),
               aes(x = x_case, xend = x_pump,
                   y = y_case, yend = y_pump),
               col = "purple", linewidth = 0.1) +
  geom_segment(data = filter(min_dist, label == "Briddle St"),
               aes(x = x_case, xend = x_pump,
                   y = y_case, yend = y_pump),
               col = "gold2", linewidth = 0.1)

ggsave("2023_09_27_john_snow_society/2023_09_27_john_snow_cholera_map.png",
       p, dpi = 320, width = 12, height = 6)



  geom_point(aes(x = x_case, y = y_case, col = label)) +
  geom_segment(aes(x = x_case, xend = x_pump,
                   y = y_case, yend = y_pump,
                   col = label),
               linewidth = 0.01)

ggsave("2023_09_27_john_snow_society/2023_09_27_john_snow_cholera_map.png", p, dpi = 320, width = 12, height = 6)

## John Snow cholera map
## John Snow Society 210th Anniversary Cholera Map Competition
## https://johnsnowsociety.org/broadsheets-lectures-social/#mapcompetition
## Deadline 2023-09-27
## Last update 2023-09-26

## 📦 Load the packages ----

library(tidyverse)
library(HistData)
library(showtext)

## 🔤 Import fonts ----

font_add_google("Roboto Condensed", "Roboto Condensed")
font_add_google("Raleway", "Raleway")
font_add_google("Raleway Dots", "Raleway Dots")
showtext_auto()

## ⬇️ Import the datasets ----

deaths <- HistData::Snow.deaths |>
  as_tibble() |>
  rename(x_case = x, y_case = y)

pumps <- HistData::Snow.pumps |>
  as_tibble() |>
  rename(x_pump = x, y_pump = y)

## 🧹 Clean & prep the data ----

pumps_rep <- bind_rows(replicate(nrow(deaths),
                                 pumps,
                                 simplify = FALSE)) |>
  mutate(case = rep(deaths$case, each = nrow(pumps)),
         .before = pump)

cases_pumps <- pumps_rep |>
  left_join(deaths) |>
  mutate(dist = sqrt((x_case - x_pump)^2 + (y_case - y_pump)^2)) |>
  slice_min(dist, by = case)

rm(deaths, pumps, pumps_rep)

## ✏️ Create the plot ----

p <- ggplot() +
  geom_segment(data = filter(cases_pumps, label == "Broad St"),
               aes(x = x_case, xend = x_pump,
                   y = y_case, yend = y_pump),
               colour = "#02f0fc", linewidth = 0.2) +
  geom_point(data = cases_pumps,
             aes(x = x_case, y = y_case),
             shape = 21, size = 2,
             colour = "#03a4da", fill = "#040940") +
  geom_point(data = filter(cases_pumps, label == "Broad St"),
             aes(x = x_case, y = y_case),
             shape = 21, size = 2,
             colour = "#02f0fc", fill = "#040940") +
  labs(title = "Map of the 1854 cholera outbreak",
       subtitle = "Each dot represents a case and the lines connect the cases closest to the Broad Street pump",
       caption = "John Snow Society 210th Anniversary Cholera Map Competition | Visualisation by Jonathan Kitt") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#040940", colour = "#040940"),
        plot.background = element_rect(fill = "#040940", colour = "#040940"),
        plot.title = element_text(family = "Raleway", colour = "#02f0fc",
                                  face = "bold", size = 50, hjust = 0.5,
                                  margin = margin(t = 20)),
        plot.subtitle = element_text(family = "Raleway", colour = "#02f0fc",
                                     size = 40, hjust = 0.5,
                                     margin = margin(t = 5)),
        plot.caption = element_text(family = "Raleway", colour = "#02f0fc",
                                    size = 20, hjust = 0.5,
                                    margin = margin(b = 5)))

## 💾 Export plot ----

ggsave("2023_09_27_john_snow_society/2023_09_27_john_snow_cholera_map.png",
       p, dpi = 320, width = 12, height = 6)

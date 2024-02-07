library(ABwaterwells)
library(dplyr)
library(tidyr)
library(dtplyr)
library(lubridate)
library(units)
library(ggplot2)
library(stringr)
library(patchwork)

# query water wells
wells <-
  request_awwid(
    "wells",
    select = c("gicwellid", "wellid", "latitude", "longitude")
  ) |>
  metricate()

reports <-
  request_awwid(
    "wellreports",
    select = c("wellid", "wellreportid", "totaldepthdrilled", "drillingmethod")
  ) |>
  metricate()

pumptests <-
  request_awwid("pumptests", select = c("wellreportid", "staticwaterlevel", "testdate")) |>
  metricate()

dtw <- query_staticwater(wells, reports, pumptests)

dtw_m <- dtw |>
  mutate(year = year(testdate)) |>
  drop_na(year) |>
  drop_units()

dtw_m |>
  filter(year >= 1940, year <= 2023) |>
  group_by(year, drillingmethod) |>
  tally() |>
  ggplot(aes(year, n, colour = drillingmethod)) +
  geom_line()

p_dtw <- dtw_m |>
  filter(year < 2024, year >= 1940, str_detect(tolower(drillingmethod), "rotary")) |>
  group_by(year) |>
  summarize(
    staticwaterlevel = mean(staticwaterlevel, na.rm = TRUE),
    number = n(),
    .groups = "drop"
  ) |>
  ggplot(aes(year, staticwaterlevel)) +
  geom_line() +
  geom_point() +
  scale_y_reverse() +
  ylab("DTW [m]") +
  xlab("Year") +
  theme_minimal()

p_depthdrilled <- dtw_m |>
  filter(year < 2024, year >= 1940, str_detect(tolower(drillingmethod), "rotary")) |>
  group_by(year) |>
  summarize(totaldepthdrilled = mean(totaldepthdrilled, na.rm = TRUE)) |>
  ungroup() |>
  ggplot(aes(year, totaldepthdrilled)) +
  geom_line() +
  geom_point() +
  ylab("Total depth drilled [m]") +
  xlab("Year") +
  theme_minimal()

(p_dtw / p_depthdrilled)

dtw_m |>
  filter(year < 2024, year >= 1940, drillingmethod == "Rotary") |>
  mutate(cluster = kmeans(cbind(latitude, longitude), 10)$cluster) |>
  ggplot(aes(longitude, latitude, colour = cluster)) +
  geom_point() +
  scale_colour_binned(breaks = 1:10, type = "viridis") +
  coord_map() +
  theme_minimal()


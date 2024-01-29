library(ABwaterwells)
library(dplyr)
library(tidyr)
library(dtplyr)
library(lubridate)
library(units)
library(ggplot2)

# query water wells
wells <-
  request_awwid("wells", select = c("gicwellid", "wellid", "latitude", "longitude")) |>
  metricate()

reports <-
  request_awwid("wellreports", select = c("wellid", "wellreportid")) |>
  metricate()

pumptests <-
  request_awwid("pumptests", select = c("wellreportid", "staticwaterlevel", "testdate")) |>
  metricate()

dtw <- query_staticwater(wells, reports, pumptests)

# select water wells that have a range of testdates
dtw <- dtw |>
  lazy_dt() |>
  group_by(gicwellid) |>
  mutate(testrange = max(testdate, na.rm = TRUE) - min(testdate, na.rm = TRUE)) |>
  ungroup() |>
  collect()

dtw_m <- dtw |>
  mutate(year = year(testdate)) |>
  drop_na(year) |>
  drop_units()

dtw_yearly <- dtw_m |>
  group_by(gicwellid, latitude, longitude, year) |>
  summarise(staticwaterlevel = mean(staticwaterlevel, na.rm = TRUE), .groups = "drop")

# plot maps
dtw_yearly |>
  filter(year %in% c(1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) |>
  ggplot(aes(x = longitude, y = latitude, colour = staticwaterlevel)) +
  geom_point(size = 0.2) +
  scale_colour_distiller(palette = "Spectral", trans = "sqrt") +
  facet_wrap(vars(year)) +
  guides(colour = guide_colourbar(reverse = TRUE))

dtw_m |>
  filter(year < 2024, year >= 1940) |>
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
  xlab("Year")

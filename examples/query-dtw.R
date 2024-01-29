library(ABwaterwells)
library(dplyr)
library(tidyr)
library(lubridate)
library(units)
library(ggplot2)
library(gganimate)

# query water wells
wells <- request_awwid("wells", select = c("gicwellid", "wellid", "latitude", "longitude"))
reports <- request_awwid("wellreports", select = c("wellid", "wellreportid"))
pumptests <- request_awwid("pumptests", select = c("wellreportid", "staticwaterlevel", "testdate"))

dtw <- query_staticwater(wells, reports, pumptests) |>
  mutate(testdate = as_datetime(testdate))

# select water wells that have a range of testdates
dtw <- dtw |>
  group_by(gicwellid) |>
  mutate(testrange = max(testdate, na.rm = TRUE) - min(testdate, na.rm = TRUE)) |>
  ungroup()

dtw_long <- dtw |>
  filter(testrange > 3600 * 24 * 90) |>
  mutate(year = year(testdate)) |>
  drop_na(year)

# plot animated map
p <- dtw_long |>
  ggplot(aes(x = longitude, y = latitude, colour = staticwaterlevel)) +
  geom_point() +
  scale_colour_viridis_c(trans = "log") +
  transition_time(year)

animate(p, renderer = av_renderer())

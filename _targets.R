# _targets.R
library(targets)
library(ABwaterwells)

con <- awwid_connect()

list(
  tar_awwid_table(wells, con$wells),
  tar_awwid_table(wellreports, con$wellreports),
  tar_awwid_table(lithologies, con$lithologies),

  tar_target(wells_m, metricate(wells)),
  tar_target(reports_m, metricate(wellreports)),
  tar_target(
    lithologs,
    query_lithologs(wells_m, reports_m, metricate(lithologies))
  )
)

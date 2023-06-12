future::plan("multisession")

wells <-
  request_awwid("wells") |>
  metricate()

reports <-
  request_awwid("wellreports", select = "wellreportid,wellid,totaldepthdrilled") |>
  metricate()

lithologies <-
  request_awwid("lithologies") |>
  metricate()

lithologs <- query_awwid_lithologs(wells, reports, lithologies)

screens <-
  request_awwid("screens") |>
  metricate()

perfs <-
  request_awwid("perforations") |>
  metricate()

screen_intervals <- query_awwid_screens(wells, reports, screens, perfs)

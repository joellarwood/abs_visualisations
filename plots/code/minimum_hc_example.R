hc <- highchart() %>%
  hc_chart(type="column") %>%
  hc_xAxis(type="category") %>%
  hc_add_series(
    name = "Things",
    data = list(
      list(
        name = "Animals",
        y = 10,
        drilldown = "animals"
      )
    )
  ) %>%
  hc_drilldown(
    series = list(
      list(
        name = "Animals",
        id = "animals",
        data = list(
          list(
            name = "Cats",
            y = 2,
            drilldown = "cats"
          )
        )
      ),
      list(
        name = "Cats",
        id = "cats",
        data = list(list(name = "white cats", y = 2), list(name = "black cats", y = 3), list(name = "red cats",y = 4))
      )
    )
  )

hc

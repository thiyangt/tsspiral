library(tidyverse)
library(lubridate)

# ---------------------------------------------------------
# Calendar of hourly polar bar charts
# ---------------------------------------------------------

calendar_hourly_polar <- function(data, datetime, value,
                                  month = NULL,
                                  radius = 0.35,
                                  bar_width = 0.85,
                                  cell_width = 1,
                                  cell_height = 1) {

  datetime <- enquo(datetime)
  value    <- enquo(value)

  # -------------------------------------------------------
  # Prepare data
  # -------------------------------------------------------

  dat <- data |>
    transmute(
      datetime = !!datetime,
      value = !!value
    ) |>
    mutate(
      date = as.Date(datetime),
      hour = hour(datetime)
    )

  # -------------------------------------------------------
  # Select month
  # -------------------------------------------------------

  if (!is.null(month)) {

    month_start <- floor_date(as.Date(month), "month")

    dat <- dat |>
      filter(
        floor_date(date, "month") == month_start
      )

  } else {

    month_start <- floor_date(min(dat$date), "month")
  }

  # -------------------------------------------------------
  # Calendar structure
  # -------------------------------------------------------

  month_end <- ceiling_date(month_start, "month") - days(1)

  calendar_days <- tibble(
    date = seq(
      month_start,
      month_end,
      by = "day"
    )
  ) |>
    mutate(

      weekday = wday(
        date,
        week_start = 1
      ),

      week_in_month =
        (day(date) +
           wday(month_start, week_start = 1) -
           2) %/% 7 + 1,

      calendar_x = weekday,
      calendar_y = -week_in_month
    )


  # -------------------------------------------------------
  # Complete every day to 24 hours
  # -------------------------------------------------------

  dat <- calendar_days |>
    select(date) |>
    crossing(hour = 0:23) |>
    left_join(
      dat |>
        select(date, hour, value),
      by = c("date", "hour")
    ) |>
    left_join(
      calendar_days |>
        select(
          date,
          calendar_x,
          calendar_y
        ),
      by = "date"
    )


  # -------------------------------------------------------
  # Scale values
  #
  # The bar length is scaled between 0 and radius.
  # -------------------------------------------------------

  value_range <- range(
    dat$value,
    na.rm = TRUE
  )

  if (diff(value_range) == 0) {

    dat <- dat |>
      mutate(
        bar_radius = radius / 2
      )

  } else {

    dat <- dat |>
      mutate(
        bar_radius =
          radius *
          (value - value_range[1]) /
          diff(value_range)
      )
  }


  # -------------------------------------------------------
  # Create polar bars
  # -------------------------------------------------------

  # Each hour occupies 1/24 of the circle.

  dat <- dat |>
    mutate(
      theta_mid =
        2 * pi * hour / 24 - pi / 2,

      theta_width =
        2 * pi / 24 * bar_width,

      theta_start =
        theta_mid - theta_width / 2,

      theta_end =
        theta_mid + theta_width / 2
    )


  # -------------------------------------------------------
  # Convert each polar bar to a polygon
  # -------------------------------------------------------

  make_bar <- function(
    xc,
    yc,
    theta_start,
    theta_end,
    height,
    id
  ) {

    theta <- seq(
      theta_start,
      theta_end,
      length.out = 10
    )

    tibble(

      id = id,

      x = c(
        xc,
        xc + height * cos(theta),
        xc
      ),

      y = c(
        yc,
        yc + height * sin(theta),
        yc
      )
    )
  }


  polar_bars <- map_dfr(
    seq_len(nrow(dat)),
    function(i) {

      # Missing observations have no visible bar
      if (is.na(dat$bar_radius[i])) {

        return(NULL)
      }

      make_bar(
        xc = dat$calendar_x[i],
        yc = dat$calendar_y[i],
        theta_start = dat$theta_start[i],
        theta_end = dat$theta_end[i],
        height = dat$bar_radius[i],
        id = i
      ) |>
        mutate(
          date = dat$date[i],
          hour = dat$hour[i],
          value = dat$value[i]
        )
    }
  )


  # -------------------------------------------------------
  # Plot
  # -------------------------------------------------------

  ggplot() +

    # Calendar cells
    geom_tile(
      data = calendar_days,
      aes(
        x = calendar_x,
        y = calendar_y
      ),
      fill = NA,
      linewidth = 0.4,
      colour = "grey75",
      width = cell_width,
      height = cell_height
    ) +

    # 24 hourly polar bars
    geom_polygon(
      data = polar_bars,
      aes(
        x = x,
        y = y,
        group = id,
        fill = value
      ),
      colour = "white",
      linewidth = 0.15
    ) +

    # Day number
    geom_text(
      data = calendar_days,
      aes(
        x = calendar_x - 0.40,
        y = calendar_y + 0.40,
        label = day(date)
      ),
      hjust = 0,
      vjust = 1,
      size = 3
    ) +

    # Weekdays
    scale_x_continuous(
      breaks = 1:7,
      labels = c(
        "Mon", "Tue", "Wed", "Thu",
        "Fri", "Sat", "Sun"
      ),
      limits = c(
        0.4,
        7.6
      ),
      expand = c(0, 0)
    ) +

    scale_y_continuous(
      breaks = -(1:6),
      labels = NULL,
      expand = c(0, 0)
    ) +

    scale_fill_viridis_c(
      na.value = "black"
    ) +

    coord_equal() +

    labs(
      x = NULL,
      y = NULL,
      fill = "Value",
      title = format(
        month_start,
        "%B %Y"
      )
    ) +

    theme_minimal() +

    theme(
      panel.grid = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_blank()
    )
}


# =========================================================
# Example
# =========================================================

set.seed(123)

hourly_data <- tibble(

  datetime = seq(
    from = as.POSIXct(
      "2026-01-01 00:00"
    ),
    to = as.POSIXct(
      "2026-01-31 23:00"
    ),
    by = "hour"
  )
) |>
  mutate(

    hour = hour(datetime),

    day = day(datetime),

    temperature =
      25 +
      4 * sin(
        2 * pi * hour / 24
      ) +
      0.05 * day +
      rnorm(
        n(),
        0,
        0.8
      )
  )


calendar_hourly_polar(
  hourly_data,
  datetime,
  temperature,
  month = "2026-01-01"
)

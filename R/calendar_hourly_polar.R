#' Calendar of Hourly Polar Bar Charts
#'
#' Creates a monthly calendar visualization in which each calendar day
#' contains a 24-hour polar bar chart. Each bar represents one hour of
#' the day, with the angular position corresponding to the hour and the
#' bar length and colour representing the observed value.
#'
#' Missing hourly observations are retained in the calendar but are not
#' drawn as bars. The calendar is arranged from Monday to Sunday, with
#' each day positioned according to its calendar date.
#'
#' @param data A data frame containing the hourly observations.
#' @param datetime A date-time column containing the observation times.
#' @param value A numeric column containing the values to be visualized.
#' @param month A date or character value specifying the month to plot.
#'   For example, \code{"2026-01-01"}. If \code{NULL}, the month containing
#'   the earliest observation is used.
#' @param radius Numeric. Maximum radius of the hourly bars within each
#'   calendar cell. Defaults to \code{0.35}.
#' @param bar_width Numeric between 0 and 1. Controls the angular width
#'   of each hourly bar relative to the 24-hour sector. Defaults to
#'   \code{0.85}.
#' @param cell_width Numeric. Width of each calendar cell. Defaults to
#'   \code{1}.
#' @param cell_height Numeric. Height of each calendar cell. Defaults to
#'   \code{1}.
#'
#' @return A ggplot object containing a calendar of hourly polar bar charts.
#'
#' @details
#' Each day is represented by a separate polar bar chart containing up to
#' 24 bars. Hour 0 starts at the top of the circle and the hours proceed
#' clockwise through hour 23.
#'
#' The bar length is scaled according to the range of the observed values
#' in the selected month, while colour represents the original value.
#' Missing hourly observations are left blank.
#'
#' The function is useful for examining intraday patterns while retaining
#' the context of the calendar. For example, daily temperature, air quality,
#' electricity demand, traffic, or other regularly recorded hourly
#' measurements can be displayed.
#'
#' @examples
#' set.seed(123)
#'
#' hourly_data <- tibble::tibble(
#'   datetime = seq(
#'     from = as.POSIXct("2026-01-01 00:00"),
#'     to = as.POSIXct("2026-01-31 23:00"),
#'     by = "hour"
#'   )
#' ) |>
#'   dplyr::mutate(
#'     hour = lubridate::hour(datetime),
#'     day = lubridate::day(datetime),
#'     temperature =
#'       25 +
#'       4 * sin(2 * pi * hour / 24) +
#'       0.05 * day +
#'       stats::rnorm(length(datetime), 0, 0.8)
#'   )
#'
#' calendar_hourly_polar(
#'   hourly_data,
#'   datetime,
#'   temperature,
#'   month = "2026-01-01"
#' )
#'
#' @importFrom dplyr transmute mutate filter select left_join
#' @importFrom ggplot2 ggplot geom_tile geom_polygon geom_text
#'   scale_x_continuous scale_y_continuous scale_fill_viridis_c
#'   coord_equal labs theme_minimal theme element_blank
#' @importFrom lubridate floor_date ceiling_date hour day wday
#' @importFrom purrr map_dfr
#' @importFrom tidyr crossing
#' @importFrom rlang enquo
#'
#' @export
calendar_hourly_polar <- function(data,
                                  datetime,
                                  value,
                                  month = NULL,
                                  radius = 0.35,
                                  bar_width = 0.85,
                                  cell_width = 1,
                                  cell_height = 1) {

  datetime <- rlang::enquo(datetime)
  value <- rlang::enquo(value)

  # -------------------------------------------------------
  # Prepare data
  # -------------------------------------------------------

  dat <- data |>
    dplyr::transmute(
      datetime = !!datetime,
      value = !!value
    ) |>
    dplyr::mutate(
      date = as.Date(datetime),
      hour = lubridate::hour(datetime)
    )

  # -------------------------------------------------------
  # Select month
  # -------------------------------------------------------

  if (!is.null(month)) {

    month_start <- lubridate::floor_date(
      as.Date(month),
      "month"
    )

    dat <- dat |>
      dplyr::filter(
        lubridate::floor_date(date, "month") == month_start
      )

  } else {

    if (nrow(dat) == 0) {
      stop("No observations available.")
    }

    month_start <- lubridate::floor_date(
      min(dat$date),
      "month"
    )
  }

  # -------------------------------------------------------
  # Calendar structure
  # -------------------------------------------------------

  month_end <- lubridate::ceiling_date(
    month_start,
    "month"
  ) - lubridate::days(1)

  calendar_days <- tibble::tibble(
    date = seq(
      month_start,
      month_end,
      by = "day"
    )
  ) |>
    dplyr::mutate(

      weekday = lubridate::wday(
        date,
        week_start = 1
      ),

      week_in_month =
        (
          lubridate::day(date) +
            lubridate::wday(
              month_start,
              week_start = 1
            ) -
            2
        ) %/% 7 + 1,

      calendar_x = weekday,
      calendar_y = -week_in_month
    )

  # -------------------------------------------------------
  # Complete every day to 24 hours
  # -------------------------------------------------------

  dat <- calendar_days |>
    dplyr::select(date) |>
    tidyr::crossing(hour = 0:23) |>
    dplyr::left_join(
      dat |>
        dplyr::select(date, hour, value),
      by = c("date", "hour")
    ) |>
    dplyr::left_join(
      calendar_days |>
        dplyr::select(
          date,
          calendar_x,
          calendar_y
        ),
      by = "date"
    )

  # -------------------------------------------------------
  # Check value
  # -------------------------------------------------------

  if (!is.numeric(dat$value)) {
    stop("`value` must be numeric.")
  }

  if (all(is.na(dat$value))) {
    stop("No non-missing values are available for the selected month.")
  }

  # -------------------------------------------------------
  # Scale values to bar length
  # -------------------------------------------------------

  value_range <- range(
    dat$value,
    na.rm = TRUE
  )

  if (diff(value_range) == 0) {

    dat <- dat |>
      dplyr::mutate(
        bar_radius = radius / 2
      )

  } else {

    dat <- dat |>
      dplyr::mutate(
        bar_radius =
          radius *
          (value - value_range[1]) /
          diff(value_range)
      )
  }

  # -------------------------------------------------------
  # Angular position of each hour
  # -------------------------------------------------------

  dat <- dat |>
    dplyr::mutate(

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
  # Function to create one polar bar
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

    tibble::tibble(

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

  # -------------------------------------------------------
  # Convert polar bars to polygons
  # -------------------------------------------------------

  polar_bars <- purrr::map_dfr(
    seq_len(nrow(dat)),
    function(i) {

      # Do not draw missing observations
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
        dplyr::mutate(
          date = dat$date[i],
          hour = dat$hour[i],
          value = dat$value[i]
        )
    }
  )

  # -------------------------------------------------------
  # Plot
  # -------------------------------------------------------

  ggplot2::ggplot() +

    # Calendar cells
    ggplot2::geom_tile(
      data = calendar_days,
      ggplot2::aes(
        x = calendar_x,
        y = calendar_y
      ),
      fill = NA,
      linewidth = 0.4,
      colour = "grey75",
      width = cell_width,
      height = cell_height
    ) +

    # Hourly polar bars
    ggplot2::geom_polygon(
      data = polar_bars,
      ggplot2::aes(
        x = x,
        y = y,
        group = id,
        fill = value
      ),
      colour = "white",
      linewidth = 0.15
    ) +

    # Day number
    ggplot2::geom_text(
      data = calendar_days,
      ggplot2::aes(
        x = calendar_x - 0.40,
        y = calendar_y + 0.40,
        label = lubridate::day(date)
      ),
      hjust = 0,
      vjust = 1,
      size = 3
    ) +

    # Weekdays
    ggplot2::scale_x_continuous(
      breaks = 1:7,
      labels = c(
        "Mon",
        "Tue",
        "Wed",
        "Thu",
        "Fri",
        "Sat",
        "Sun"
      ),
      limits = c(0.4, 7.6),
      expand = c(0, 0)
    ) +

    ggplot2::scale_y_continuous(
      breaks = -(1:6),
      labels = NULL,
      expand = c(0, 0)
    ) +

    ggplot2::scale_fill_viridis_c(
      na.value = "black"
    ) +

    ggplot2::coord_equal() +

    ggplot2::labs(
      x = NULL,
      y = NULL,
      fill = "Value",
      title = format(
        month_start,
        "%B %Y"
      )
    ) +

    ggplot2::theme_minimal() +

    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank()
    )
}

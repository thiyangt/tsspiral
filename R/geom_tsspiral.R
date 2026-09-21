# ============================================================
# geom_tsspiral.R
# ============================================================
#' Spiral Time-Series Geometry
#'
#' Create a spiral time-series plot using a fixed 365-day calendar.
#'
#' `geom_tsspiral()` displays each year of a time series as a radial
#' ring. The angular position represents the day of the year. The
#' spiral always contains 365 angular divisions.
#'
#' The spiral starts at 12 o'clock on January 1 and proceeds
#' clockwise through the year.
#'
#' The input frequency is detected automatically. Daily observations
#' are used directly. Weekly, monthly, quarterly, and annual
#' observations are expanded to daily observations before plotting.
#'
#' Calendar labels are displayed only around the outermost ring:
#'
#' \itemize{
#'   \item daily data: month names,
#'   \item monthly data: month names,
#'   \item quarterly data: quarter names,
#'   \item weekly data: week numbers,
#'   \item annual data: month names.
#' }
#'
#' @param mapping Set of aesthetic mappings created by
#'   [ggplot2::aes()].
#' @param data A data frame, tibble, or tsibble containing the
#'   time-series data.
#' @param stat Statistical transformation used by the layer.
#'   Defaults to `StatTSSpiral`.
#' @param position Position adjustment. Defaults to `"identity"`.
#' @param na.rm Logical. Should missing values be removed silently?
#' @param ring_spacing Numeric value controlling the distance between
#'   yearly rings.
#' @param ... Other arguments passed to [ggplot2::layer()].
#'
#' @return A ggplot2 layer.
#'
#' @details
#'
#' The spiral uses a fixed 365-day calendar. January 1 starts at
#' 12 o'clock and the calendar proceeds clockwise.
#'
#' For day `d`, the angular position is
#'
#' \deqn{
#' \theta_d =
#' \frac{\pi}{2}
#' -
#' \frac{2\pi(d-1)}{365}.
#' }
#'
#' Consequently, January 1 is positioned at 12 o'clock and later
#' days move clockwise around the spiral.
#'
#' Leap years do not create a 366th angular division. February 29 is
#' mapped to the February 28 position and the days after February 29
#' are shifted by one day.
#'
#' Monthly, quarterly, weekly, and annual observations are expanded
#' to their corresponding daily intervals. Consequently, a monthly
#' observation fills the complete angular region corresponding to
#' that month rather than appearing as a thin radial slice.
#'
#' @examples
#' \dontrun{
#'
#' library(ggplot2)
#' library(viridis)
#'
#' # Daily data
#' dat <- data.frame(
#'   date = seq.Date(
#'     as.Date("2020-01-01"),
#'     as.Date("2023-12-31"),
#'     by = "day"
#'   )
#' )
#'
#' dat$value <- sin(
#'   2 * pi * as.numeric(format(dat$date, "%j")) / 365
#' ) + rnorm(nrow(dat), sd = 0.2)
#'
#' ggplot(
#'   dat,
#'   aes(
#'     x = date,
#'     y = value,
#'     fill = value
#'   )
#' ) +
#'   geom_tsspiral(ring_spacing = 2) +
#'   scale_fill_viridis_c() +
#'   theme_void()
#'
#' }
#'
#' @export
geom_tsspiral <- function(
    mapping = NULL,
    data = NULL,
    stat = StatTSSpiral,
    position = "identity",
    na.rm = FALSE,
    ring_spacing = 1,
    ...) {

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTSSpiral,
    position = position,
    params = list(
      ring_spacing = ring_spacing,
      na.rm = na.rm,
      ...
    )
  )
}


# ============================================================
# Frequency detection
# ============================================================

#' Detect Time-Series Frequency
#'
#' Internal function used to detect the approximate frequency of
#' the supplied time series.
#'
#' @param x Date vector.
#'
#' @return A character string describing the frequency.
#'
#' @keywords internal
detect_tsspiral_frequency <- function(x) {

  x <- as.Date(x)

  d <- diff(sort(unique(x)))

  d <- as.numeric(d)

  d <- d[
    is.finite(d) &
      d > 0
  ]

  if (length(d) == 0) {
    return("annual")
  }

  md <- median(d)

  # Sub-daily data
  if (md < 1) {

    warning(
      "Sub-daily data detected. ",
      "geom_tsspiral() represents the series at daily resolution.",
      call. = FALSE
    )

    return("high")
  }

  # Daily
  if (md <= 1.5) {
    return("daily")
  }

  # Weekly
  if (md >= 5 && md <= 9) {
    return("weekly")
  }

  # Monthly
  if (md >= 25 && md <= 35) {
    return("monthly")
  }

  # Quarterly
  if (md >= 75 && md <= 105) {
    return("quarterly")
  }

  # Annual
  if (md >= 330) {
    return("annual")
  }

  stop(
    "Could not determine the time-series frequency. ",
    "The data should be approximately daily, weekly, monthly, ",
    "quarterly, or annual.",
    call. = FALSE
  )
}


# ============================================================
# Expand observations to daily resolution
# ============================================================

#' Expand Time Series to Daily Resolution
#'
#' Internal function that converts weekly, monthly, quarterly,
#' and annual observations into daily observations.
#'
#' @param data Data frame containing `x`.
#' @param frequency Detected frequency.
#'
#' @return A data frame with daily observations.
#'
#' @keywords internal
expand_tsspiral_daily <- function(
    data,
    frequency) {

  data$x <- as.Date(data$x)

  # ----------------------------------------------------------
  # Daily and high-frequency data
  # ----------------------------------------------------------

  if (frequency %in% c("daily", "high")) {
    return(data)
  }


  # ----------------------------------------------------------
  # Weekly data
  # ----------------------------------------------------------

  if (frequency == "weekly") {

    x <- sort(unique(data$x))

    result <- lapply(
      seq_along(x),
      function(i) {

        start <- x[i]

        # Each weekly observation represents seven days.
        end <- start + 6

        dates <- seq.Date(
          start,
          end,
          by = "day"
        )

        row <- data[
          match(start, data$x),
          ,
          drop = FALSE
        ]

        row[
          rep(1, length(dates)),
          ,
          drop = FALSE
        ] |>
          transform(x = dates)
      }
    )

    return(
      do.call(
        rbind,
        result
      )
    )
  }


  # ----------------------------------------------------------
  # Monthly data
  # ----------------------------------------------------------

  if (frequency == "monthly") {

    x <- sort(unique(data$x))

    result <- lapply(
      x,
      function(start) {

        month_start <-
          lubridate::floor_date(
            start,
            "month"
          )

        month_end <-
          lubridate::ceiling_date(
            month_start,
            "month"
          ) -
          lubridate::days(1)

        dates <- seq.Date(
          month_start,
          month_end,
          by = "day"
        )

        row <- data[
          match(start, data$x),
          ,
          drop = FALSE
        ]

        row[
          rep(1, length(dates)),
          ,
          drop = FALSE
        ] |>
          transform(x = dates)
      }
    )

    return(
      do.call(
        rbind,
        result
      )
    )
  }


  # ----------------------------------------------------------
  # Quarterly data
  # ----------------------------------------------------------

  if (frequency == "quarterly") {

    x <- sort(unique(data$x))

    result <- lapply(
      x,
      function(start) {

        quarter_start <-
          lubridate::floor_date(
            start,
            "quarter"
          )

        quarter_end <-
          lubridate::ceiling_date(
            quarter_start,
            "quarter"
          ) -
          lubridate::days(1)

        dates <- seq.Date(
          quarter_start,
          quarter_end,
          by = "day"
        )

        row <- data[
          match(start, data$x),
          ,
          drop = FALSE
        ]

        row[
          rep(1, length(dates)),
          ,
          drop = FALSE
        ] |>
          transform(x = dates)
      }
    )

    return(
      do.call(
        rbind,
        result
      )
    )
  }


  # ----------------------------------------------------------
  # Annual data
  # ----------------------------------------------------------

  if (frequency == "annual") {

    years <- sort(
      unique(
        lubridate::year(data$x)
      )
    )

    result <- lapply(
      years,
      function(y) {

        year_start <- as.Date(
          sprintf(
            "%d-01-01",
            y
          )
        )

        year_end <- as.Date(
          sprintf(
            "%d-12-31",
            y
          )
        )

        dates <- seq.Date(
          year_start,
          year_end,
          by = "day"
        )

        row <- data[
          lubridate::year(data$x) == y,
          ,
          drop = FALSE
        ][1, , drop = FALSE]

        row[
          rep(1, length(dates)),
          ,
          drop = FALSE
        ] |>
          transform(x = dates)
      }
    )

    return(
      do.call(
        rbind,
        result
      )
    )
  }


  stop(
    "Unsupported time-series frequency.",
    call. = FALSE
  )
}


# ============================================================
# Fixed 365-day calendar
# ============================================================

#' Convert Date to Fixed 365-Day Calendar
#'
#' Internal function that maps dates onto a fixed 365-day calendar.
#'
#' @param x Date vector.
#'
#' @return Integer vector from 1 to 365.
#'
#' @keywords internal
tsspiral_day <- function(x) {

  x <- as.Date(x)

  doy <- lubridate::yday(x)

  leap <- lubridate::leap_year(x)

  spiral_day <- doy

  # ----------------------------------------------------------
  # February 29
  # ----------------------------------------------------------

  feb29 <-
    leap &
    lubridate::month(x) == 2 &
    lubridate::day(x) == 29

  spiral_day[feb29] <- 59


  # ----------------------------------------------------------
  # Days after February 29
  # ----------------------------------------------------------

  after_feb29 <-
    leap &
    doy > 60

  spiral_day[after_feb29] <-
    doy[after_feb29] - 1


  # ----------------------------------------------------------
  # Keep within 365 divisions
  # ----------------------------------------------------------

  spiral_day <- pmax(
    1,
    pmin(
      spiral_day,
      365
    )
  )

  spiral_day
}


# ============================================================
# Calendar labels
# ============================================================

#' Generate Spiral Calendar Labels
#'
#' Internal function generating labels for the outer cycle.
#'
#' @param frequency Detected time-series frequency.
#'
#' @return Data frame containing label positions and labels.
#'
#' @keywords internal
tsspiral_calendar_labels <- function(
    frequency) {

  # ----------------------------------------------------------
  # Month labels
  # ----------------------------------------------------------

  if (
    frequency %in%
    c(
      "daily",
      "monthly",
      "annual",
      "high"
    )
  ) {

    month_start <- c(
      1,
      32,
      60,
      91,
      121,
      152,
      182,
      213,
      244,
      274,
      305,
      335
    )

    return(
      data.frame(
        day = month_start,
        label = month.abb,
        stringsAsFactors = FALSE
      )
    )
  }


  # ----------------------------------------------------------
  # Quarter labels
  # ----------------------------------------------------------

  if (frequency == "quarterly") {

    return(
      data.frame(
        day = c(
          1,
          91,
          182,
          274
        ),
        label = c(
          "Q1",
          "Q2",
          "Q3",
          "Q4"
        ),
        stringsAsFactors = FALSE
      )
    )
  }


  # ----------------------------------------------------------
  # Week labels
  # ----------------------------------------------------------

  if (frequency == "weekly") {

    week_start <- seq(
      from = 1,
      to = 365,
      by = 7
    )

    return(
      data.frame(
        day = week_start,
        label = sprintf(
          "W%02d",
          seq_along(week_start)
        ),
        stringsAsFactors = FALSE
      )
    )
  }


  data.frame(
    day = numeric(0),
    label = character(0),
    stringsAsFactors = FALSE
  )
}


# ============================================================
# Statistical transformation
# ============================================================

#' Statistical Transformation for Spiral Time Series
#'
#' @keywords internal
StatTSSpiral <- ggplot2::ggproto(
  "StatTSSpiral",
  ggplot2::Stat,

  required_aes = c(
    "x",
    "y",
    "fill"
  ),

  compute_panel = function(
    data,
    scales,
    ...) {

    # --------------------------------------------------------
    # Recover Date after ggplot2 conversion
    # --------------------------------------------------------

    if (inherits(data$x, "Date")) {

      x <- data$x

    } else if (inherits(data$x, "POSIXt")) {

      x <- as.Date(data$x)

    } else {

      x <- as.Date(
        as.numeric(data$x),
        origin = "1970-01-01"
      )
    }

    data$x <- x


    # --------------------------------------------------------
    # Detect frequency
    # --------------------------------------------------------

    frequency <-
      detect_tsspiral_frequency(x)


    # --------------------------------------------------------
    # Expand to daily resolution
    # --------------------------------------------------------

    daily <-
      expand_tsspiral_daily(
        data,
        frequency
      )


    # --------------------------------------------------------
    # Year
    # --------------------------------------------------------

    daily$year <-
      lubridate::year(
        daily$x
      )


    # --------------------------------------------------------
    # Fixed 365-day position
    # --------------------------------------------------------

    daily$spiral_day <-
      tsspiral_day(
        daily$x
      )


    # --------------------------------------------------------
    # Year index
    # --------------------------------------------------------

    years <-
      sort(
        unique(
          daily$year
        )
      )

    daily$year_id <-
      match(
        daily$year,
        years
      )


    # --------------------------------------------------------
    # Clockwise angle
    #
    # January 1 starts at 12 o'clock.
    # Decreasing angle produces clockwise movement.
    # --------------------------------------------------------

    daily$angle_start <-
      pi / 2 -
      2 *
      pi *
      (
        daily$spiral_day - 1
      ) /
      365

    daily$angle_end <-
      pi / 2 -
      2 *
      pi *
      daily$spiral_day /
      365


    # --------------------------------------------------------
    # Save frequency
    # --------------------------------------------------------

    daily$tsspiral_frequency <-
      frequency

    daily
  }
)


# ============================================================
# Geometry
# ============================================================

#' Spiral Time-Series Geometry
#'
#' Internal ggproto geometry used by [geom_tsspiral()].
#'
#' @keywords internal
GeomTSSpiral <- ggplot2::ggproto(
  "GeomTSSpiral",
  ggplot2::Geom,

  required_aes = c(
    "x",
    "y",
    "fill"
  ),

  default_aes = ggplot2::aes(
    colour = NA,
    linewidth = 0.1,
    linetype = 1,
    alpha = 1
  ),

  draw_key = ggplot2::draw_key_polygon,

  draw_panel = function(
    data,
    panel_params,
    coord,
    ring_spacing = 1,
    na.rm = FALSE) {

    # --------------------------------------------------------
    # Frequency
    # --------------------------------------------------------

    frequency <-
      unique(
        data$tsspiral_frequency
      )

    if (
      length(frequency) == 0 ||
      is.na(frequency[1])
    ) {

      frequency <- "daily"

    } else {

      frequency <- frequency[1]
    }


    # --------------------------------------------------------
    # Years
    # --------------------------------------------------------

    years <-
      sort(
        unique(
          data$year
        )
      )

    year_id <-
      match(
        data$year,
        years
      )


    # --------------------------------------------------------
    # Ring dimensions
    # --------------------------------------------------------

    ring_width <-
      0.8 *
      ring_spacing

    radius <-
      year_id *
      ring_spacing

    inner_radius <-
      radius -
      ring_width / 2

    outer_radius <-
      radius +
      ring_width / 2

    max_radius <-
      max(
        outer_radius,
        na.rm = TRUE
      )

    scale_radius <-
      0.42 /
      max_radius

    inner_radius <-
      inner_radius *
      scale_radius

    outer_radius <-
      outer_radius *
      scale_radius


    # ========================================================
    # Time-series polygons
    # ========================================================

    spiral_grobs <- lapply(
      seq_len(nrow(data)),
      function(i) {

        # ----------------------------------------------------
        # Missing observations create gaps
        # ----------------------------------------------------

        if (
          is.na(data$fill[i]) ||
          is.na(data$angle_start[i]) ||
          is.na(data$angle_end[i])
        ) {

          return(NULL)
        }


        # ----------------------------------------------------
        # Angular boundaries
        # ----------------------------------------------------

        angles <- seq(
          data$angle_start[i],
          data$angle_end[i],
          length.out = 8
        )


        # ----------------------------------------------------
        # Outer boundary
        # ----------------------------------------------------

        outer_x <-
          0.5 +
          outer_radius[i] *
          cos(angles)

        outer_y <-
          0.5 +
          outer_radius[i] *
          sin(angles)


        # ----------------------------------------------------
        # Inner boundary
        # ----------------------------------------------------

        inner_x <-
          0.5 +
          inner_radius[i] *
          cos(
            rev(angles)
          )

        inner_y <-
          0.5 +
          inner_radius[i] *
          sin(
            rev(angles)
          )


        # ----------------------------------------------------
        # Polygon
        # ----------------------------------------------------

        grid::polygonGrob(
          x = c(
            outer_x,
            inner_x
          ),
          y = c(
            outer_y,
            inner_y
          ),
          default.units = "npc",
          gp = grid::gpar(
            fill = scales::alpha(
              data$fill[i],
              data$alpha[i]
            ),
            col = data$colour[i],
            lwd =
              data$linewidth[i] *
              0.8,
            lty =
              data$linetype[i]
          )
        )
      }
    )


    # --------------------------------------------------------
    # Remove NULL grobs
    # --------------------------------------------------------

    spiral_grobs <-
      Filter(
        Negate(is.null),
        spiral_grobs
      )


    # ========================================================
    # Year labels
    # ========================================================

    year_grobs <- lapply(
      seq_along(years),
      function(i) {

        r <-
          i *
          ring_spacing *
          scale_radius

        grid::textGrob(
          label = years[i],
          x = grid::unit(
            0.5 + r,
            "npc"
          ),
          y = grid::unit(
            0.5,
            "npc"
          ),
          just = c(
            "left",
            "centre"
          ),
          gp = grid::gpar(
            fontsize = 8
          )
        )
      }
    )


    # ========================================================
    # Calendar labels
    # ========================================================

    calendar_labels <-
      tsspiral_calendar_labels(
        frequency
      )

    calendar_grobs <- list()


    if (
      nrow(calendar_labels) > 0
    ) {

      # ------------------------------------------------------
      # Clockwise calendar angles
      #
      # January 1 starts at 12 o'clock.
      # ------------------------------------------------------

      calendar_labels$angle <-
        pi / 2 -
        2 *
        pi *
        (
          calendar_labels$day - 0.5
        ) /
        365


      # ------------------------------------------------------
      # Outer ring
      # ------------------------------------------------------

      outer_year <-
        length(years)

      outer_r <-
        outer_year *
        ring_spacing *
        scale_radius

      label_r <-
        outer_r +
        0.055


      # ------------------------------------------------------
      # Calendar text grobs
      # ------------------------------------------------------

      calendar_grobs <- lapply(
        seq_len(
          nrow(calendar_labels)
        ),
        function(i) {

          angle <-
            calendar_labels$angle[i]


          # --------------------------------------------------
          # Position
          # --------------------------------------------------

          x <-
            0.5 +
            label_r *
            cos(angle)

          y <-
            0.5 +
            label_r *
            sin(angle)


          # --------------------------------------------------
          # Tangential text rotation
          # --------------------------------------------------

          rotation <-
            angle *
            180 /
            pi +
            90


          # Keep labels readable
          if (
            rotation > 90 &&
            rotation < 270
          ) {

            rotation <-
              rotation +
              180
          }


          grid::textGrob(
            label =
              calendar_labels$label[i],
            x = grid::unit(
              x,
              "npc"
            ),
            y = grid::unit(
              y,
              "npc"
            ),
            rot = rotation,
            just = "centre",
            gp = grid::gpar(
              fontsize = 7
            )
          )
        }
      )
    }


    # ========================================================
    # Combine grobs
    # ========================================================

    all_grobs <- c(
      spiral_grobs,
      year_grobs,
      calendar_grobs
    )

    all_grobs <-
      Filter(
        Negate(is.null),
        all_grobs
      )


    # Important:
    # gList() must receive individual grobs, not lists.
    # ========================================================

    grid::grobTree(
      children =
        do.call(
          grid::gList,
          all_grobs
        )
    )
  }
)

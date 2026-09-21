# ============================================================
# geom_tsspiralmiss.R
# ============================================================
#' Spiral Missingness Geometry
#'
#' Create a spiral plot showing missing observations in a time series.
#'
#' `geom_tsspiralmiss()` displays each year of a time series as a
#' radial ring using a fixed 365-day calendar. Observed periods are
#' shown in one colour and missing periods are shown in another.
#'
#' The spiral starts at 12 o'clock on January 1 and proceeds
#' clockwise through the year.
#'
#' The input frequency is detected automatically. Daily observations
#' are used directly. Weekly, monthly, quarterly, and annual
#' observations are expanded to daily observations before plotting.
#'
#' @param mapping Set of aesthetic mappings created by
#'   [ggplot2::aes()].
#' @param data A data frame, tibble, or tsibble containing the
#'   time-series data.
#' @param stat Statistical transformation used by the layer.
#'   Defaults to `StatTSSpiralMiss`.
#' @param position Position adjustment. Defaults to `"identity"`.
#' @param na.rm Logical. Should missing values be removed silently?
#' @param ring_spacing Numeric value controlling the distance between
#'   yearly rings.
#' @param observed_colour Colour used for observed periods.
#' @param missing_colour Colour used for missing periods.
#' @param ... Other arguments passed to [ggplot2::layer()].
#'
#' @return A ggplot2 layer.
#'
#' @details
#'
#' The geometry is designed specifically for exploring the temporal
#' structure of missing observations.
#'
#' January 1 is positioned at 12 o'clock and time proceeds clockwise.
#' Each year is represented by one radial ring, with 365 fixed angular
#' divisions.
#'
#' A period is classified as observed when a corresponding observation
#' is present in the input data. Periods without an observation are
#' classified as missing.
#'
#' Leap years do not create a 366th angular division. February 29 is
#' mapped to the February 28 position and dates after February 29 are
#' shifted by one day.
#'
#' @examples
#' \dontrun{
#'
#' library(ggplot2)
#'
#' set.seed(123)
#'
#' dat <- data.frame(
#'   date = seq.Date(
#'     as.Date("2020-01-01"),
#'     as.Date("2023-12-31"),
#'     by = "day"
#'   )
#' )
#'
#' # Remove some observations
#' dat <- dat[
#'   !(
#'     dat$date >= as.Date("2020-03-01") &
#'       dat$date <= as.Date("2020-03-20")
#'   ),
#' ]
#'
#' dat <- dat[
#'   !(
#'     dat$date >= as.Date("2021-07-10") &
#'       dat$date <= as.Date("2021-08-05")
#'   ),
#' ]
#'
#' ggplot(
#'   dat,
#'   aes(x = date)
#' ) +
#'   geom_tsspiralmiss(
#'     ring_spacing = 2
#'   ) +
#'   theme_void()
#'
#' }
#'
#' @export
geom_tsspiralmiss <- function(
    mapping = NULL,
    data = NULL,
    stat = StatTSSpiralMiss,
    position = "identity",
    na.rm = FALSE,
    ring_spacing = 1,
    observed_colour = "grey85",
    missing_colour = "black",
    ...) {

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTSSpiralMiss,
    position = position,
    params = list(
      ring_spacing = ring_spacing,
      observed_colour = observed_colour,
      missing_colour = missing_colour,
      na.rm = na.rm,
      ...
    )
  )
}


# ============================================================
# Statistical transformation
# ============================================================

#' Statistical Transformation for Spiral Missingness
#'
#' Internal statistical transformation used by
#' [geom_tsspiralmiss()].
#'
#' @keywords internal
StatTSSpiralMiss <- ggplot2::ggproto(
  "StatTSSpiralMiss",
  ggplot2::Stat,

  required_aes = "x",

  compute_panel = function(
    data,
    scales,
    ...) {

    # --------------------------------------------------------
    # Recover Date
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

    x <- sort(unique(x))


    # --------------------------------------------------------
    # Detect frequency
    # --------------------------------------------------------

    frequency <-
      detect_tsspiral_frequency(x)


    # --------------------------------------------------------
    # Create complete daily calendar
    # --------------------------------------------------------

    start_date <- min(x, na.rm = TRUE)
    end_date <- max(x, na.rm = TRUE)

    all_dates <- seq.Date(
      start_date,
      end_date,
      by = "day"
    )


    # --------------------------------------------------------
    # Determine observed dates
    # --------------------------------------------------------

    observed <-
      all_dates %in% x


    daily <- data.frame(
      x = all_dates,
      observed = observed
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
    # Frequency
    # --------------------------------------------------------

    daily$tsspiral_frequency <-
      frequency

    daily
  }
)


# ============================================================
# Geometry
# ============================================================

#' Spiral Missingness Geometry
#'
#' Internal ggproto geometry used by [geom_tsspiralmiss()].
#'
#' @keywords internal
GeomTSSpiralMiss <- ggplot2::ggproto(
  "GeomTSSpiralMiss",
  ggplot2::Geom,

  required_aes = "x",

  default_aes = ggplot2::aes(
    alpha = 1
  ),

  draw_key = ggplot2::draw_key_rect,

  draw_panel = function(
    data,
    panel_params,
    coord,
    ring_spacing = 1,
    observed_colour = "grey85",
    missing_colour = "black",
    na.rm = FALSE) {

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
    # Calendar polygons
    # ========================================================

    spiral_grobs <- lapply(
      seq_len(nrow(data)),
      function(i) {

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
        # Colour
        # ----------------------------------------------------

        fill_colour <-
          if (data$observed[i]) {
            observed_colour
          } else {
            missing_colour
          }


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
              fill_colour,
              data$alpha[i]
            ),
            col = NA
          )
        )
      }
    )


    # --------------------------------------------------------
    # Year labels
    # --------------------------------------------------------

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
      # Calendar labels
      # ------------------------------------------------------

      calendar_grobs <- lapply(
        seq_len(
          nrow(calendar_labels)
        ),
        function(i) {

          angle <-
            calendar_labels$angle[i]

          x <-
            0.5 +
            label_r *
            cos(angle)

          y <-
            0.5 +
            label_r *
            sin(angle)

          rotation <-
            angle *
            180 /
            pi +
            90

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

    grid::grobTree(
      children =
        do.call(
          grid::gList,
          all_grobs
        )
    )
  }
)

# ============================================================
# geom_tsspiralbar.R
# ============================================================
#' Spiral Time-Series Bar Geometry
#'
#' Create a spiral bar plot for time-series data.
#'
#' Each year is represented by a radial ring. The angular position
#' represents time within the year, while the radial height of each
#' bar represents the magnitude of the time-series value.
#'
#' January 1 starts at 12 o'clock and the spiral proceeds clockwise.
#'
#' @param mapping Set of aesthetic mappings created by
#'   [ggplot2::aes()].
#' @param data A data frame, tibble, or tsibble.
#' @param stat Statistical transformation used by the layer.
#' @param position Position adjustment.
#' @param na.rm Logical. Should missing values be removed silently?
#' @param ring_spacing Numeric value controlling the distance between
#'   yearly rings.
#' @param ... Other arguments passed to [ggplot2::layer()].
#'
#' @return A ggplot2 layer.
#'
#' @examples
#' \dontrun{
#'
#' library(ggplot2)
#'
#' ggplot(
#'   dat,
#'   aes(
#'     x = date,
#'     y = value,
#'     fill = value
#'   )
#' ) +
#'   geom_tsspiralbar(
#'     ring_spacing = 2
#'   ) +
#'   scale_fill_viridis_c() +
#'   theme_void()
#'
#' }
#'
#' @export
geom_tsspiralbar <- function(
    mapping = NULL,
    data = NULL,
    stat = StatTSSpiralBar,
    position = "identity",
    na.rm = FALSE,
    ring_spacing = 1,
    ...) {

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTSSpiralBar,
    position = position,
    params = list(
      ring_spacing = ring_spacing,
      na.rm = na.rm,
      ...
    )
  )
}


# ============================================================
# Statistical transformation
# ============================================================

#' Statistical Transformation for Spiral Bars
#'
#' @keywords internal
StatTSSpiralBar <- ggplot2::ggproto(
  "StatTSSpiralBar",
  ggplot2::Stat,

  required_aes = c(
    "x",
    "y"
  ),

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

    data$x <- x


    # --------------------------------------------------------
    # Detect frequency
    # --------------------------------------------------------

    frequency <-
      detect_tsspiral_frequency(x)


    # --------------------------------------------------------
    # Expand to daily resolution
    # --------------------------------------------------------

    data <-
      expand_tsspiral_daily(
        data,
        frequency
      )


    # --------------------------------------------------------
    # Year
    # --------------------------------------------------------

    data$year <-
      lubridate::year(
        data$x
      )


    # --------------------------------------------------------
    # Fixed 365-day calendar
    # --------------------------------------------------------

    data$spiral_day <-
      tsspiral_day(
        data$x
      )


    # --------------------------------------------------------
    # Year index
    # --------------------------------------------------------

    years <-
      sort(
        unique(
          data$year
        )
      )

    data$year_id <-
      match(
        data$year,
        years
      )


    # --------------------------------------------------------
    # Clockwise angles
    # --------------------------------------------------------

    data$angle_start <-
      pi / 2 -
      2 *
      pi *
      (
        data$spiral_day - 1
      ) /
      365

    data$angle_end <-
      pi / 2 -
      2 *
      pi *
      data$spiral_day /
      365


    data$tsspiral_frequency <-
      frequency

    data
  }
)


# ============================================================
# Geometry
# ============================================================

#' Spiral Time-Series Bar Geometry
#'
#' Internal ggproto geometry used by [geom_tsspiralbar()].
#'
#' @keywords internal
GeomTSSpiralBar <- ggplot2::ggproto(
  "GeomTSSpiralBar",
  ggplot2::Geom,

  required_aes = c(
    "x",
    "y"
  ),

  default_aes = ggplot2::aes(
    fill = "grey70",
    colour = NA,
    linewidth = 0.1,
    linetype = 1,
    alpha = 1
  ),

  draw_key = ggplot2::draw_key_rect,

  draw_panel = function(
    data,
    panel_params,
    coord,
    ring_spacing = 1,
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

    base_radius <-
      year_id *
      ring_spacing


    # --------------------------------------------------------
    # Scale bar height
    #
    # Use the maximum absolute value so the bars fit
    # within the plotting region.
    # --------------------------------------------------------

    ymax <-
      max(
        abs(data$y),
        na.rm = TRUE
      )

    if (
      !is.finite(ymax) ||
      ymax == 0
    ) {

      ymax <- 1

    }


    # Maximum outward bar length
    bar_scale <-
      0.7 *
      ring_spacing /
      ymax


    # --------------------------------------------------------
    # Radius scaling
    # --------------------------------------------------------

    max_radius <-
      max(
        base_radius +
          abs(data$y) *
          bar_scale,
        na.rm = TRUE
      )

    scale_radius <-
      0.42 /
      max_radius


    # ========================================================
    # Draw bars
    # ========================================================

    spiral_grobs <- lapply(
      seq_len(nrow(data)),
      function(i) {

        if (
          is.na(data$y[i]) ||
          is.na(data$angle_start[i]) ||
          is.na(data$angle_end[i])
        ) {

          return(NULL)
        }


        # ----------------------------------------------------
        # Base radius
        # ----------------------------------------------------

        r0 <-
          base_radius[i] *
          scale_radius


        # ----------------------------------------------------
        # Bar height
        # ----------------------------------------------------

        r1 <-
          (
            base_radius[i] +
              data$y[i] *
              bar_scale
          ) *
          scale_radius


        # ----------------------------------------------------
        # Prevent negative radius
        # ----------------------------------------------------

        r1 <-
          max(
            r0 + 0.001,
            r1
          )


        # ----------------------------------------------------
        # Angular boundaries
        # ----------------------------------------------------

        angles <- seq(
          data$angle_start[i],
          data$angle_end[i],
          length.out = 5
        )


        # ----------------------------------------------------
        # Outer boundary
        # ----------------------------------------------------

        outer_x <-
          0.5 +
          r1 *
          cos(angles)

        outer_y <-
          0.5 +
          r1 *
          sin(angles)


        # ----------------------------------------------------
        # Inner boundary
        # ----------------------------------------------------

        inner_x <-
          0.5 +
          r0 *
          cos(
            rev(angles)
          )

        inner_y <-
          0.5 +
          r0 *
          sin(
            rev(angles)
          )


        # ----------------------------------------------------
        # Bar polygon
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

    frequency <-
      unique(
        data$tsspiral_frequency
      )

    frequency <-
      frequency[1]

    calendar_labels <-
      tsspiral_calendar_labels(
        frequency
      )

    calendar_grobs <- list()


    if (
      nrow(calendar_labels) > 0
    ) {

      calendar_labels$angle <-
        pi / 2 -
        2 *
        pi *
        (
          calendar_labels$day - 0.5
        ) /
        365


      outer_year <-
        length(years)

      outer_r <-
        outer_year *
        ring_spacing *
        scale_radius

      label_r <-
        outer_r +
        0.055


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

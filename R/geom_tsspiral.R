#' Spiral Time-Series Geometry
#'
#' Creates a spiral representation of a time series using ggplot2.
#' Each year is represented by a separate ring, while the position
#' within each ring represents the day/week/month etc of the year.
#'
#' @description
#' `geom_tsspiral()` provides a ggplot2 geometry for visualising
#'  time series in a spiral layout. Observations from different
#' years are arranged as concentric rings, allowing seasonal patterns
#' to be compared across years.
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()].
#'   The `x` aesthetic should contain the time index, `y` should contain
#'   the observed value, and `fill` controls the colour of observations.
#' @param data A data frame containing the time series.
#' @param stat The statistical transformation to use. Defaults to
#'   `"identity"`.
#' @param position Position adjustment. Defaults to `"identity"`.
#' @param na.rm Logical indicating whether missing values should be
#'   removed. Defaults to `FALSE`.
#' @param ring_spacing Numeric value controlling the spacing between
#'   consecutive yearly rings. Defaults to `1`.
#' @param ... Other arguments passed to [ggplot2::layer()].
#'
#' @details
#' The date supplied through the `x` aesthetic determines the position
#' of each observation in the spiral:
#'
#' \itemize{
#'   \item The year determines the radial position.
#'   \item The day/week/month/quarter/hour etc. of the year determines the angular position.
#'   \item The `fill` aesthetic determines the colour of the observation.
#' }
#'
#' Missing observations are not drawn, allowing gaps in the time series
#' to remain visible.
#'
#' Year labels are displayed on the corresponding rings.
#'
#' @return
#' A ggplot2 layer that draws the time series as a spiral.
#'
#' @examples
#' library(ggplot2)
#'
#' dat <- data.frame(
#'   date = seq.Date(
#'     as.Date("2023-01-01"),
#'     as.Date("2025-12-31"),
#'     by = "day"
#'   ),
#'   value = rnorm(1096, 300, 30)
#' )
#'
#' ggplot(
#'   dat,
#'   aes(
#'     x = date,
#'     y = value,
#'     fill = value
#'   )
#' ) +
#'   geom_tsspiral()
#'
#' @examples
#' # Increase the spacing between yearly rings
#' ggplot(
#'   dat,
#'   aes(
#'     x = date,
#'     y = value,
#'     fill = value
#'   )
#' ) +
#'   geom_tsspiral(ring_spacing = 2)
#'
#' @seealso
#' [ggplot2::layer()]
#'
#' @export
geom_tsspiral <- function(mapping = NULL, data = NULL,
                          stat = "identity",
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


#' @noRd
GeomTSSpiral <- ggplot2::ggproto(
  "GeomTSSpiral",
  ggplot2::Geom,

  required_aes = c("x", "y", "fill"),

  default_aes = ggplot2::aes(
    colour = NA,
    linewidth = 0.1,
    alpha = 1
  ),

  draw_panel = function(data, panel_params, coord,
                        ring_spacing = 1,
                        na.rm = FALSE) {

    date <- as.Date(data$x)

    year <- as.integer(format(date, "%Y"))
    day  <- as.integer(format(date, "%j"))

    min_year <- min(year, na.rm = TRUE)

    year_id <- year - min_year + 1

    angle <- 2 * pi * (day - 1) / 365

    radius <- year_id * ring_spacing

    max_radius <- max(radius, na.rm = TRUE)

    r1 <- (radius - ring_spacing / 2) /
      (max_radius + ring_spacing)

    r2 <- (radius + ring_spacing / 2) /
      (max_radius + ring_spacing)

    width <- 2 * pi / 365

    grobs <- vector("list", length(angle))

    for (i in seq_along(angle)) {

      if (is.na(data$y[i])) {
        next
      }

      a1 <- angle[i] - width / 2
      a2 <- angle[i] + width / 2

      theta <- c(a1, a2, a2, a1)

      x <- 0.5 + 0.45 * c(
        r1[i] * cos(theta[1]),
        r1[i] * cos(theta[2]),
        r2[i] * cos(theta[3]),
        r2[i] * cos(theta[4])
      )

      y <- 0.5 + 0.45 * c(
        r1[i] * sin(theta[1]),
        r1[i] * sin(theta[2]),
        r2[i] * sin(theta[3]),
        r2[i] * sin(theta[4])
      )

      grobs[[i]] <- grid::polygonGrob(
        x = grid::unit(x, "npc"),
        y = grid::unit(y, "npc"),
        gp = grid::gpar(
          fill = data$fill[i],
          col = data$colour[i],
          alpha = data$alpha[i],
          lwd = data$linewidth[i]
        )
      )
    }

    grobs <- Filter(
      Negate(is.null),
      grobs
    )

    # Add year labels
    years <- sort(unique(year))

    for (i in seq_along(years)) {

      r <- (i * ring_spacing) /
        (max_radius + ring_spacing)

      grobs[[length(grobs) + 1]] <-
        grid::textGrob(
          label = years[i],
          x = grid::unit(0.5, "npc"),
          y = grid::unit(
            0.5 + 0.45 * r,
            "npc"
          ),
          gp = grid::gpar(
            fontsize = 9,
            fontface = "bold"
          )
        )
    }

    if (length(grobs) == 0) {
      return(grid::nullGrob())
    }

    do.call(
      grid::grobTree,
      grobs
    )
  },

  draw_key = ggplot2::draw_key_rect
)


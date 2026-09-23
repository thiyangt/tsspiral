#' Aligned Temporal Hierarchical Spiral Plot
#'
#' Visualise a time series at multiple temporal resolutions using
#' concentric rings with a common temporal coordinate system.
#'
#' The outer ring represents daily observations, followed inward by
#' weekly, monthly, quarterly, and yearly observations. Temporal
#' boundaries are aligned across all rings.
#'
#' @param data A data frame containing a date variable and a numeric
#'   variable.
#' @param date Unquoted name of the date column.
#' @param value Unquoted name of the numeric variable to visualise.
#' @param fun Aggregation function used for weekly, monthly,
#'   quarterly, and yearly aggregation. Defaults to `mean`.
#' @param ring_width Width of each ring. Defaults to `1`.
#' @param ring_gap Gap between rings. Defaults to `0.05`.
#' @param na.value Colour used for missing values. Defaults to `"grey90"`.
#' @param daily Logical; draw the daily ring. Defaults to `TRUE`.
#' @param weekly Logical; draw the weekly ring. Defaults to `TRUE`.
#' @param monthly Logical; draw the monthly ring. Defaults to `TRUE`.
#' @param quarterly Logical; draw the quarterly ring. Defaults to `TRUE`.
#' @param yearly Logical; draw the yearly ring. Defaults to `TRUE`.
#' @param direction Direction of the temporal axis. `1` produces a
#'   clockwise direction and `-1` produces an anticlockwise direction.
#' @param start_angle Starting angle in radians. Defaults to
#'   `-pi / 2`, placing the beginning of the series at the top.
#'
#' @return A `ggplot2` object.
#'
#' @details
#' All temporal levels use a common angular coordinate based on the
#' complete date range. Consequently, observations at different
#' temporal resolutions are spatially aligned.
#'
#' For example, the monthly January ring occupies the same angular
#' region as the corresponding daily observations, weeks, quarter,
#' and year.
#'
#' The outer-to-inner hierarchy is:
#'
#' \itemize{
#'   \item Daily
#'   \item Weekly
#'   \item Monthly
#'   \item Quarterly
#'   \item Yearly
#' }
#'
#' @examples
#'
#' set.seed(123)
#'
#' dat <- data.frame(
#'   date = seq.Date(
#'     from = as.Date("2020-01-01"),
#'     to = as.Date("2023-12-31"),
#'     by = "day"
#'   )
#' )
#'
#' dat$value <- 20 +
#'   5 * sin(
#'     2 * pi * lubridate::yday(dat$date) / 365.25
#'   ) +
#'   rnorm(nrow(dat), 0, 1)
#'
#' temporal_hierarchical(
#'   data = dat,
#'   date = date,
#'   value = value
#' )
#'
#' @export
temporal_hierarchical <- function(
    data,
    date,
    value,
    fun = mean,
    ring_width = 1,
    ring_gap = 0.05,
    na.value = "grey90",
    daily = TRUE,
    weekly = TRUE,
    monthly = TRUE,
    quarterly = TRUE,
    yearly = TRUE,
    direction = 1,
    start_angle = -pi / 2) {

  # ---------------------------------------------------------------
  # Packages
  # ---------------------------------------------------------------

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.", call. = FALSE)
  }

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required.", call. = FALSE)
  }

  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("Package 'lubridate' is required.", call. = FALSE)
  }

  if (!requireNamespace("rlang", quietly = TRUE)) {
    stop("Package 'rlang' is required.", call. = FALSE)
  }

  # ---------------------------------------------------------------
  # Check arguments
  # ---------------------------------------------------------------

  if (!is.function(fun)) {
    stop("'fun' must be a function.", call. = FALSE)
  }

  if (ring_width <= 0) {
    stop(
      "'ring_width' must be greater than zero.",
      call. = FALSE
    )
  }

  if (ring_gap < 0) {
    stop(
      "'ring_gap' cannot be negative.",
      call. = FALSE
    )
  }

  if (!direction %in% c(-1, 1)) {
    stop(
      "'direction' must be either 1 or -1.",
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------
  # Capture variables
  # ---------------------------------------------------------------

  date_col <- rlang::enquo(date)
  value_col <- rlang::enquo(value)

  # ---------------------------------------------------------------
  # Prepare data
  # ---------------------------------------------------------------

  dat <- data |>
    dplyr::transmute(
      date = as.Date(!!date_col),
      value = as.numeric(!!value_col)
    ) |>
    dplyr::filter(!is.na(date)) |>
    dplyr::arrange(date)

  if (nrow(dat) == 0) {
    stop("No valid dates found.", call. = FALSE)
  }

  # ---------------------------------------------------------------
  # Complete daily sequence
  # ---------------------------------------------------------------

  complete_dates <- data.frame(
    date = seq(
      min(dat$date),
      max(dat$date),
      by = "day"
    )
  )

  dat <- complete_dates |>
    dplyr::left_join(
      dat,
      by = "date"
    )

  # ---------------------------------------------------------------
  # Global temporal coordinate
  # ---------------------------------------------------------------

  global_start <- min(dat$date)
  global_end <- max(dat$date) + 1

  total_days <- as.numeric(
    global_end - global_start
  )

  date_to_angle <- function(x) {

    direction * 2 * pi *
      as.numeric(x - global_start) /
      total_days +
      start_angle
  }

  # ---------------------------------------------------------------
  # Create one temporal level
  # ---------------------------------------------------------------

  create_level <- function(
    level,
    period_start,
    period_end,
    value_data,
    ring) {

    period_start <- as.Date(period_start)
    period_end <- as.Date(period_end)

    tibble <- data.frame(
      period_start = period_start,
      period_end = period_end,
      value = value_data
    )

    tibble$level <- level
    tibble$ring <- ring

    tibble$angle_start <- date_to_angle(
      tibble$period_start
    )

    tibble$angle_end <- date_to_angle(
      tibble$period_end
    )

    tibble$angle <- (
      tibble$angle_start +
        tibble$angle_end
    ) / 2

    tibble$width <- abs(
      tibble$angle_end -
        tibble$angle_start
    )

    tibble
  }

  # ---------------------------------------------------------------
  # Ring hierarchy
  # ---------------------------------------------------------------

  levels <- character()

  if (daily) {
    levels <- c(levels, "Daily")
  }

  if (weekly) {
    levels <- c(levels, "Weekly")
  }

  if (monthly) {
    levels <- c(levels, "Monthly")
  }

  if (quarterly) {
    levels <- c(levels, "Quarterly")
  }

  if (yearly) {
    levels <- c(levels, "Yearly")
  }

  if (length(levels) == 0) {
    stop(
      "At least one temporal level must be selected.",
      call. = FALSE
    )
  }

  # Reverse so Daily is outermost
  levels <- rev(levels)

  # ---------------------------------------------------------------
  # DAILY
  # ---------------------------------------------------------------

  results <- list()

  if (daily) {

    ring <- match("Daily", levels)

    daily_dat <- dat |>
      dplyr::mutate(
        period_start = date,
        period_end = date + 1
      )

    results[["Daily"]] <- create_level(
      level = "Daily",
      period_start = daily_dat$period_start,
      period_end = daily_dat$period_end,
      value_data = daily_dat$value,
      ring = ring
    )
  }

  # ---------------------------------------------------------------
  # WEEKLY
  # ---------------------------------------------------------------

  if (weekly) {

    ring <- match("Weekly", levels)

    weekly_dat <- dat |>
      dplyr::mutate(
        period_start = lubridate::floor_date(
          date,
          unit = "week",
          week_start = 1
        )
      ) |>
      dplyr::group_by(period_start) |>
      dplyr::summarise(
        value = if (all(is.na(value))) {
          NA_real_
        } else {
          fun(value, na.rm = TRUE)
        },
        .groups = "drop"
      ) |>
      dplyr::mutate(
        period_end = period_start + 7
      )

    results[["Weekly"]] <- create_level(
      level = "Weekly",
      period_start = weekly_dat$period_start,
      period_end = weekly_dat$period_end,
      value_data = weekly_dat$value,
      ring = ring
    )
  }

  # ---------------------------------------------------------------
  # MONTHLY
  # ---------------------------------------------------------------

  if (monthly) {

    ring <- match("Monthly", levels)

    monthly_dat <- dat |>
      dplyr::mutate(
        period_start = lubridate::floor_date(
          date,
          unit = "month"
        )
      ) |>
      dplyr::group_by(period_start) |>
      dplyr::summarise(
        value = if (all(is.na(value))) {
          NA_real_
        } else {
          fun(value, na.rm = TRUE)
        },
        .groups = "drop"
      ) |>
      dplyr::mutate(
        period_end = dplyr::lead(period_start)
      )

    # The final month needs its natural calendar boundary.
    monthly_dat$period_end[
      nrow(monthly_dat)
    ] <- seq.Date(
      from = monthly_dat$period_start[
        nrow(monthly_dat)
      ],
      by = "month",
      length.out = 2
    )[2]

    results[["Monthly"]] <- create_level(
      level = "Monthly",
      period_start = monthly_dat$period_start,
      period_end = monthly_dat$period_end,
      value_data = monthly_dat$value,
      ring = ring
    )
  }

  # ---------------------------------------------------------------
  # QUARTERLY
  # ---------------------------------------------------------------

  if (quarterly) {

    ring <- match("Quarterly", levels)

    quarterly_dat <- dat |>
      dplyr::mutate(
        period_start = lubridate::floor_date(
          date,
          unit = "quarter"
        )
      ) |>
      dplyr::group_by(period_start) |>
      dplyr::summarise(
        value = if (all(is.na(value))) {
          NA_real_
        } else {
          fun(value, na.rm = TRUE)
        },
        .groups = "drop"
      ) |>
      dplyr::mutate(
        period_end = dplyr::lead(period_start)
      )

    # Natural end of the final quarter.
    quarterly_dat$period_end[
      nrow(quarterly_dat)
    ] <- quarterly_dat$period_start[
      nrow(quarterly_dat)
    ] |>
      lubridate::ceiling_date(
        unit = "quarter"
      )

    results[["Quarterly"]] <- create_level(
      level = "Quarterly",
      period_start = quarterly_dat$period_start,
      period_end = quarterly_dat$period_end,
      value_data = quarterly_dat$value,
      ring = ring
    )
  }

  # ---------------------------------------------------------------
  # YEARLY
  # ---------------------------------------------------------------

  if (yearly) {

    ring <- match("Yearly", levels)

    yearly_dat <- dat |>
      dplyr::mutate(
        period_start = lubridate::floor_date(
          date,
          unit = "year"
        )
      ) |>
      dplyr::group_by(period_start) |>
      dplyr::summarise(
        value = if (all(is.na(value))) {
          NA_real_
        } else {
          fun(value, na.rm = TRUE)
        },
        .groups = "drop"
      ) |>
      dplyr::mutate(
        period_end = dplyr::lead(period_start)
      )

    # Natural end of the final year.
    yearly_dat$period_end[
      nrow(yearly_dat)
    ] <- yearly_dat$period_start[
      nrow(yearly_dat)
    ] |>
      lubridate::ceiling_date(
        unit = "year"
      )

    results[["Yearly"]] <- create_level(
      level = "Yearly",
      period_start = yearly_dat$period_start,
      period_end = yearly_dat$period_end,
      value_data = yearly_dat$value,
      ring = ring
    )
  }

  # ---------------------------------------------------------------
  # Combine
  # ---------------------------------------------------------------

  plot_dat <- dplyr::bind_rows(results)

  # ---------------------------------------------------------------
  # Keep only the part of a period inside the requested range
  # ---------------------------------------------------------------

  plot_dat$period_start <- pmax(
    plot_dat$period_start,
    global_start
  )

  plot_dat$period_end <- pmin(
    plot_dat$period_end,
    global_end
  )

  # ---------------------------------------------------------------
  # Recalculate angles after truncation
  # ---------------------------------------------------------------

  plot_dat$angle_start <- date_to_angle(
    plot_dat$period_start
  )

  plot_dat$angle_end <- date_to_angle(
    plot_dat$period_end
  )

  plot_dat$angle <- (
    plot_dat$angle_start +
      plot_dat$angle_end
  ) / 2

  plot_dat$width <- abs(
    plot_dat$angle_end -
      plot_dat$angle_start
  )

  # ---------------------------------------------------------------
  # Radius
  # ---------------------------------------------------------------

  plot_dat$inner <- (
    plot_dat$ring - 1
  ) * (ring_width + ring_gap)

  plot_dat$outer <- (
    plot_dat$ring * ring_width
  ) +
    (plot_dat$ring - 1) * ring_gap

  plot_dat$radius <- (
    plot_dat$inner +
      plot_dat$outer
  ) / 2

  # ---------------------------------------------------------------
  # Plot
  # ---------------------------------------------------------------

  p <- ggplot2::ggplot(
    plot_dat
  ) +

    ggplot2::geom_tile(
      ggplot2::aes(
        x = angle,
        y = radius,
        width = width,
        height = ring_width,
        fill = value
      ),
      colour = NA
    ) +

    ggplot2::scale_fill_viridis_c(
      option = "C",
      na.value = na.value,
      name = rlang::as_label(value_col)
    ) +

    ggplot2::coord_polar(
      theta = "x",
      start = 0,
      direction = 1,
      clip = "off"
    ) +

    ggplot2::scale_y_continuous(
      limits = c(
        0,
        max(plot_dat$outer) + ring_gap * 2
      ),
      expand = c(0, 0)
    ) +

    ggplot2::theme_void() +

    ggplot2::theme(
      legend.position = "right",
      plot.margin = ggplot2::margin(
        20, 20, 20, 20
      )
    )

  # ---------------------------------------------------------------
  # Ring labels
  # ---------------------------------------------------------------

  label_dat <- plot_dat |>
    dplyr::group_by(level, ring) |>
    dplyr::summarise(
      radius = mean(radius),
      .groups = "drop"
    )

  p +
    ggplot2::geom_text(
      data = label_dat,
      ggplot2::aes(
        x = start_angle,
        y = radius,
        label = level
      ),
      inherit.aes = FALSE,
      hjust = 0.5,
      fontface = "bold",
      size = 3
    )
}

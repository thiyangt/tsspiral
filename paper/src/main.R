#### ---- Figure 1 ----

# Three annual rings with January of one year split into 31 daily segments
# Needs only ggplot2.

library(ggplot2)

# ---- calendar helpers -------------------------------------------------------
is_leap  <- function(y) (y %% 4 == 0 & y %% 100 != 0) | (y %% 400 == 0)
days_in  <- function(y) ifelse(is_leap(y), 366, 365)

# angular position: Jan 1 at the top, days advance clockwise
#   theta = pi/2 - 2*pi*(d - 1) / D_Y
# d may be fractional so arcs can be sampled smoothly
polar <- function(r, d, D, cx = 0, cy = 0) {
  theta <- pi / 2 - 2 * pi * (d - 1) / D
  data.frame(x = cx + r * cos(theta), y = cy + r * sin(theta))
}

# ---- settings ---------------------------------------------------------------
years    <- c(2022, 2023, 2024)
r0       <- 60          # radius of the first ring
dr       <- 50          # radial step between years: r_t = r0 + k * dr
zoom_yr  <- 2023        # year whose January is enlarged
band_in  <- 98          # January band radii on the zoom_yr ring
band_out <- 122

ring_r   <- r0 + (seq_along(years) - 1) * dr
ring_lab <- sprintf("%d  (k = %d, %d days)", years, seq_along(years) - 1, days_in(years))

# ---- ring outlines ----------------------------------------------------------
rings <- do.call(rbind, lapply(seq_along(years), function(i) {
  D <- days_in(years[i])
  p <- polar(ring_r[i], seq(1, D + 1, length.out = 400), D)
  p$ring <- ring_lab[i]
  p
}))

# ---- month ticks on every ring ---------------------------------------------
ticks <- do.call(rbind, lapply(seq_along(years), function(i) {
  y <- years[i]; D <- days_in(y)
  doy <- as.integer(format(as.Date(sprintf("%d-%02d-01", y, 1:12)), "%j"))
  a <- polar(ring_r[i] - 5, doy, D)
  b <- polar(ring_r[i] + 5, doy, D)
  data.frame(x = a$x, y = a$y, xend = b$x, yend = b$y, ring = ring_lab[i])
}))

# ---- month labels (outside the outer ring) ---------------------------------
mid   <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
start <- cumsum(c(1, mid[-12]))
mlab  <- cbind(polar(max(ring_r) + 18, start + mid / 2, 365),
               label = month.abb)

# ---- January split into 31 daily segments on the zoom_yr ring --------------
Dz <- days_in(zoom_yr)

sector <- function(i, make_pt, n = 12) {
  t <- seq(i, i + 1, length.out = n)
  rbind(make_pt(t, "out"), make_pt(rev(t), "in"))
}

jan_band <- do.call(rbind, lapply(1:31, function(i) {
  pt <- function(t, side) polar(if (side == "out") band_out else band_in, t, Dz)
  s <- sector(i, pt)
  s$day <- i
  s$shade <- if (i %% 2 == 1) "a" else "b"
  s
}))

# ---- enlarged strip (same 31 segments, magnified) --------------------------
R_in  <- 380; R_out <- 420
zx    <- -40;  zy <- -35                 # centre of the magnifying arc
dth   <- 2 * pi / Dz
half  <- 31 * dth / 2
zpt <- function(i, R) {                   # i = 0..31 boundaries, Jan 1 at the top
  a <- half - i * dth
  data.frame(x = zx + R * cos(a), y = zy + R * sin(a))
}

jan_zoom <- do.call(rbind, lapply(1:31, function(i) {
  pt <- function(t, side) zpt(t - 1, if (side == "out") R_out else R_in)
  s <- sector(i, pt)
  s$day <- i
  s$shade <- if (i %% 2 == 1) "a" else "b"
  s
}))

zlab_days <- c(1, 5, 10, 15, 20, 25, 31)
zlab <- do.call(rbind, lapply(zlab_days, function(d) {
  p <- zpt(d - 0.5, R_out + 8)
  data.frame(x = p$x, y = p$y, label = paste("Jan", d))
}))

# dashed leaders from the band to the strip
p_start <- polar(band_out, 1,  Dz); p_end <- polar(band_out, 32, Dz)
z_start <- zpt(0,  R_in);           z_end <- zpt(31, R_in)
leaders <- data.frame(x    = c(p_start$x, p_end$x),
                      y    = c(p_start$y, p_end$y),
                      xend = c(z_start$x, z_end$x),
                      yend = c(z_start$y, z_end$y))

# ---- plot ------------------------------------------------------------------
cols <- setNames(c("#1b9e77", "#d95f02", "#7570b3"), ring_lab)

p <- ggplot() +
  # rings and month ticks
  geom_path(data = rings, aes(x, y, colour = ring), linewidth = 0.9) +
  geom_segment(data = ticks, aes(x = x, y = y, xend = xend, yend = yend, colour = ring),
               linewidth = 0.4, show.legend = FALSE) +
  # January 31-segment band on the 2023 ring
  geom_polygon(data = jan_band, aes(x, y, group = day, alpha = shade),
               fill = "#BA7517", show.legend = FALSE) +
  # enlarged strip
  geom_polygon(data = jan_zoom, aes(x, y, group = day, alpha = shade),
               fill = "#BA7517", show.legend = FALSE) +
  scale_alpha_manual(values = c(a = 0.30, b = 0.70)) +
  geom_segment(data = leaders, aes(x = x, y = y, xend = xend, yend = yend),
               linetype = "dashed", linewidth = 0.3, colour = "grey40") +
  # 1 Jan axis
  geom_segment(aes(x = 0, y = 0, xend = 0, yend = max(ring_r) + 10),
               linetype = "dashed", linewidth = 0.3, colour = "grey40") +
  annotate("text", x = 0, y = max(ring_r) + 40, label = "1 Jan", size = 3.3) +
  # labels
  geom_text(data = mlab, aes(x, y, label = label), size = 3, colour = "grey30") +
  geom_text(data = zlab, aes(x, y, label = label), size = 3, hjust = 0) +
  annotate("text", x = 270, y = 0, label = paste("Jan", zoom_yr), fontface = "bold", size = 3.5) +
  annotate("text", x = 270, y = -14, label = "31 segments", size = 3) +
  scale_colour_manual(values = cols, name = NULL) +
  coord_fixed(xlim = c(-190, 480)) +
  theme_void() +
  theme(legend.position = "inside",            # needs ggplot2 >= 3.5.0
        legend.position.inside = c(0.84, 0.92),
        legend.text = element_text(size = 9))

print(p)
# ggsave("tsspiral_rings_january.png", p, width = 9, height = 6, dpi = 300, bg = "white")

#### ---- Figure 2 ----
library(readr)
library(tsspiral)
library(viridis)
library(ggplot2)
library(tsibble)
library(tidyverse)
library(patchwork)

data(tourism.srilanka)
ts <- tourism.srilanka |>
  mutate(Date = dmy(Date)) |>        
  as_tsibble(index = Date)
p1 <- ggplot(ts, aes(x=Date, y=Value)) + geom_line() + ggtitle("(a) ggplot2: geom_line")

p2 <- ggplot(
  ts,
  aes(
    x = Date,
    y = Value,
    fill = Value
  )
) +
  geom_tsspiral(
    ring_spacing = 2
  ) +
  scale_fill_viridis_c(
    option = "inferno",
    direction = -1,
    guide = guide_colorbar(reverse = FALSE)
  ) +
  theme_void() + ggtitle("(b) tsspiral: geom_tsspiral")

git_colours <- rev(c(
  "#d73027",
  "#f46d43",
  "#fdae61",
  "#ffffbf",
  "#74add1",
  "#4575b4"
))

p3 <- ts |>
  dplyr::mutate(
    Value_group = cut(
      Value,
      breaks = c(-Inf, 999, 2500, 5000, 7500, 10000, Inf),
      labels = c(
        "<1000",
        "1000–2500",
        "2501–5000",
        "5001–7500",
        "7501–10000",
        ">10000"
      ),
      right = TRUE
    )
  ) |>
  ggplot(
    aes(
      x = Date,
      y = Value,
      fill = Value_group
    )
  ) +
  geom_tsspiral(
    ring_spacing = 2
  ) +
  scale_fill_manual(
    values = git_colours,
    drop = FALSE
  ) +
  theme_void() + ggtitle("(c) tsspiral: geom_tsspiral")
(p1/(p2|p3))

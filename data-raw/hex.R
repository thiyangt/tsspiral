library(hexSticker)
library(here)
imgurl <- here("data-raw", "spiral.png")
imgurl
sticker(imgurl, package = "tsspiral",
        s_x = 1, s_y = 0.75, s_width=.43,
        p_size = 30,
        h_color="#FF4000", h_fill="navy", p_color = "mediumspringgreen",
             filename="C:/Users/DELL/Documents/tsspiral/data-raw/hex.png")



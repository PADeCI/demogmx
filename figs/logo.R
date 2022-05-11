library(hexSticker)
install.packages("hexSticker")

library(showtext)

font_add_google("Raleway", "gochie")

sticker(subplot = "figs/mx.png",
        package = "demogmx",
        p_color = "black",
        p_size = 19,
        p_family = "gochie",
        s_x = 1, s_y = 0.75,
        s_width = 0.6,
        h_fill = "white",
        h_color = "black",
        filename = "figs/demogmx_logo.png")

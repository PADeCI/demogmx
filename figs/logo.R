library(hexSticker)
#install.packages("hexSticker")

library(showtext)

font_add_google("Raleway", "gochie")


appDir <- system.file( package = "demogmx")
imgurl <- "/Users/marianafernandez/Documents/PADeCI/demogmx/figs/mx.png"

sticker(imgurl, package="demogmx", p_size=6, s_x=1, s_y=.75, s_width=.6,
        h_fill="#6A6968",
        h_color="#252529",
        p_family = "gochie",
        filename="figs/img.png")

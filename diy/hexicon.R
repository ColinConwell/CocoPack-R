if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('this.path', 'hexSticker', 'pkgdown')

setwd(this.dir())

?sticker

sticker(
  subplot = "../man/icon/logo.png",  
  package = "", # "Coco-Pack-R" if you want to add text              
  p_size = 20,
  s_x = 1,
  s_y = 1,
  s_height = 1,                        
  s_width = 1,   
  h_fill = "black",                  
  h_color = "black",
  filename = "coco-pack-hex-logo.png",
  white_around_sticker = TRUE,
)
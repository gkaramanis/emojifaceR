library(magick)
library(image.libfacedetection)
library(tidyverse)
library(ragg)

# Script modified from:
# http://www.bnosac.be/index.php/blog/89-human-face-detection-with-r

# Read in image
# Politicians
image <- image_read("https://upload.wikimedia.org/wikipedia/commons/thumb/8/80/G20_politicians_group_photo_%28cropped%29.jpg/1600px-G20_politicians_group_photo_%28cropped%29.jpg?20170726201632")

# Heavy metal band
# image <- image_read("https://i0.wp.com/rocknloadmag.com/wp-content/uploads/2021/07/TENTATION-.jpeg?fit=1200%2C799&ssl=1")

# Detect faces
faces <- image_detect_faces(image)

# "Shortcut" to image info
info <- image_info(image)

# Get face rectangles and calculate their center
marks <- faces$detections %>% 
  mutate(
    x0 = x + width/2,
    y0 = info$height - y - height/2
  ) %>% 
  rowwise() %>% 
  # Use one emoji for all faces
  # mutate(emoji = "🙂")
  # Or assign one random emoji to each face
  mutate(emoji = sample(c("💀", "👿", "🦹"), 1, replace = TRUE))

# Emoji size range, change according to image
s <- 8
r <- 1.3

# Use AGG graphic device in RStudio settings for emoji to show, otherwise they always show in saved image (below)
image_ggplot(image) +
  geom_text(data = marks, aes(x = x0, y = y0, label = emoji, size = width/height)) +
  scale_size(range = c(s, s * r)) +
  theme(
    legend.position = "none"
  )

ggsave(here::here("plots/emoji.png"), device = agg_png, width = 8, height = info$height / info$width * 8)

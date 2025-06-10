makeGif = function(imgs, fps = 1, loop = 0, output = 'Gif.gif') {
  suppressMessages(library(magick))
  imgs = image_join(lapply(imgs, image_read))
  imgs_an = image_animate(imgs, fps = fps, loop = loop)
  image_write(imgs_an, path = output)
}

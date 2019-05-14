scale_colour_ki <- function(..., aesthetics = "colour", direction = 1, na.value = "grey50") {
  discrete_scale(aesthetics, "ki", ki_pal(direction), na.value = na.value, ...)
}
scale_fill_ki <- function(..., aesthetics = "fill", direction = 1, na.value = "grey50") {
  discrete_scale(aesthetics, "ki", ki_pal(direction), na.value = na.value, ...)
}
scale_color_ki <- scale_colour_ki

ki_pal <- function(direction = 1) {
  force(direction)
  function(n) {
    ki_colours <- c(
      rgb(212, 9, 99, maxColorValue = 255), # cyklamen
      rgb(136, 196, 197, maxColorValue = 255), # teal
      rgb(189, 171, 179, maxColorValue = 255), # lavender
      rgb(135, 0, 82, maxColorValue = 255), # plum
      rgb(151, 216, 218, maxColorValue = 255), # aqua
      rgb(128, 128, 128, maxColorValue = 255) # gray
    )
    pal <- ki_colours[seq_len(n)]
    if (direction == -1) {
      pal <- rev(pal)
    }
    pal
  }
}

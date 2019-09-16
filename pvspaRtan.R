theme_PleasVal <-  function(){
  theme_minimal(base_size = 16, base_family = "") %+replace%
    theme(
      axis.text = element_text(size=rel(0.8),color = "#565656"),
      axis.ticks = element_line(color = "#D6D6D6"),
      legend.key = element_rect(colour = "White"),
      panel.grid.major = element_line(colour = "#C6C6C6", size = 0.2, lineend = "butt"),
      panel.grid.minor = element_line(colour = "#E6E6E6", size = 0.05, linetype = "longdash")
    )
}

#main Spartan Colors
spartanStyles <- c(
  'Dark Blue' = "#011638",
  'Spartan Blue' = "#003569",
  'Light Blue' = "#26547C",
  'Blue Grey' = "#4B6C8C",
  'Spartan Grey'= "#D6D6D6",
  'Blue Gray'= "#4B6C8C",
  'Spartan Gray'= "#D6D6D6",
  'Dark Grey' = "#565656",
  'Dark Gray' = "#565656"
)

#extract the hex code
styles <- function(...){
  cols <-  c(...)

  if (is.null(cols))
    return(spartanStyles)

  spartanStyles[cols]
}

#create separate palettes
spartanPalettes <- list(
  `main` = styles('Dark Blue', 'Spartan Blue', 'Light Blue', 'Blue Grey', 'Dark Grey'),
  `blues` = styles('Dark Blue', 'Spartan Blue', 'Light Blue', 'Blue Grey'),
  `mono` = styles('Spartan Blue')
)

# Functionn to access them
spartan_palette <- function(palette = "main", reverse = FALSE, ...) {
  pal <- spartanPalettes[[palette]]
  if (reverse) pal <- rev(pal)
  colorRampPalette(pal, ...)
}

# Functionn for color scale scale
scale_color_spartan <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- spartan_palette(palette = palette, reverse = reverse)


  if (discrete) {
    discrete_scale("colour", paste0("spartan", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}


scale_fill_spartan <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {


  pal <- spartan_palette(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("spartan", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}






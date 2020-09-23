theme_cg = function(...) {
  # sysfonts::font_add_google("Merriweather", "Merriweather")
  # font = "Merriweather"   #assign font family up front

  `%+replace%` = ggplot2::`%+replace%`

  font = "Palatino"   #assign font family up front

  ggplot2::theme_minimal(...) %+replace%    #replace elements we want to change

    ggplot2::theme(
      #grid elements
      # panel.grid.major = ggplot2::element_blank(),
      #strip major gridlines
      # panel.grid.minor = ggplot2::element_blank(),
      #strip minor gridlines
      axis.ticks = ggplot2::element_blank(),       #strip axis ticks

      #text elements
      plot.title = ggplot2::element_text(
        family = font,
        size = 20,
        face = "bold",
        hjust = 0, #left align
        vjust = 1 #raise slightly
      ),


      plot.subtitle = ggplot2::element_text(
        family = font,
        size = 14
      ),


      plot.caption = ggplot2::element_text(
        family = font,
        size = 9,
        hjust = 1 #right align
      ),


      axis.title = ggplot2::element_text(
        family = font,
        size = 10),


      axis.text = ggplot2::element_text(
        family = font,
        size = 9
      ),


      axis.text.x = ggplot2::element_text(
        margin = ggplot2::margin(5, b = 10)
      )
    )
}

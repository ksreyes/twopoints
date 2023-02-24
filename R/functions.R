library(htmltools)
library(distilltools)
library(stringr)
library(tidyverse)

# ICON LINK
# Credits to jhelvy (https://github.com/jhelvy/jhelvy_distill/blob/master/R/functions.R)

icon_link <- function(icon = "far fa-star", 
                      text = NULL, 
                      url = NULL, 
                      title = NULL,
                      class = "icon-link",
                      target = "_blank") {
  
  icon <- htmltools::tag("i", list(class = icon))
  icon_text <- htmltools::HTML(paste0(icon, "", text))
  
  if(is.null(url)) {
    return(icon_text)
  } else {
    return(htmltools::a(href = url, icon_text, class = class, title = title, target = target))
  }
}
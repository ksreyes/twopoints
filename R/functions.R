library(htmltools)
# library(distilltools)
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

end_mark <- function(center = FALSE) {
  if (center == TRUE) {
    return("<p style='text-align:center'>![](/images/logo-icon.png){.endmark}</p>")
  } else {
    return("![](/images/logo-icon.png){.endmark}")
  }
}

pikachu <- function(center = FALSE) {
  if (center == TRUE) {
    return("<p style='text-align:center'>![](/images/pikachu.png){.endmark}</p>")
  } else {
    return("![](/images/pikachu.png){.endmark}")
  }
}

squint <- function(center = FALSE) {
  if (center == TRUE) {
    return("<p style='text-align:center'>![](/images/squint.png){.endmark}</p>")
  } else {
    return("![](/images/squint.png){.endmark}")
  }
}
##### Converting lubridate date to string ####

date_to_string <- function(x){
  require(glue)
  require(lubridate)
  glue("{day(x)} {month(x, label = T, abbr = F)} {year(x)}")
}


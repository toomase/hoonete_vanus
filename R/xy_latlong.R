## Funktsioon konverdib x ja y koordinaadid lat long koordinaatideks
## Selleks kasutatakse Maaameti teenust: http://www.maaamet.ee/rr/geo-lest/url/
## Telia aadresside x ja y koordinaadid saab kätte vaatest MATRIX.G_Objekt_aadress
## Vajalik on numbrist kaks viimast kohta kas eemaldada või muuta komakohaks

xy_latlong <- function(x, y){
    library(rvest)
    library(dplyr)
    library(stringr)
    
    maaamet_url <- "http://www.maaamet.ee/rr/geo-lest/url/?xy="
    
    # pane x ja y koordinaat kokku
    xy <- str_c(x, y, sep = ",")
    
    # genereeri maaameti teenuse url
    xy_latlong_url <- str_c(maaamet_url, str_c(xy, collapse = ","), sep = "")
    
    # lae andmed teenuse väljundist ja genereeri vektor
    xy_lat_long <- read_html(xy_latlong_url) %>% 
        html_text() %>% 
        str_split(., "\\\n") %>% 
        unlist()
    
    # tagasta lat long vektorina
    return(xy_lat_long)
}

## Näidid
# x <- rep(539082.00, 10)
# y <- rep(6583133.00, 10)
# 
# coord_xy_raw <- data_frame(x, y)
# 
# coord_xy_raw %>%
#     mutate(lat_long = xy_latlong(x, y))
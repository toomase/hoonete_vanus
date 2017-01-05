## Muuda värvi vahemikke samasugusteks nagu on http://code.waag.org/buildings/#51.4028,5.7272,14
## Eraldi kaardid kompaktsete asumite kohta nagu Õismäe, Mustamäe ja Lasnamäe
## Seal tuleb kasutada teistsugust skaalat kui üldisel kaardil
## Nii näeb kaardilt hästi ära, kuidas linnaosa majad on ehitatud

library(htmlwidgets)
library(leaflet)
library(tidyverse)
library(stringr)
library(sf)
library(viridis)
library(webshot)

# lae andmed
load("output/majad_tallinnas_vanusega.RData")

# koosta Viridis värvipallett etteantud vahemikega
pal <- colorBin(plasma(10),
                domain = majad_tallinnas_vanusega$esmane_kasutus, 
                bins = c(1300, 1850, 1920, 1945, 1960, 1975, 1990, 
                         2000, 2008, 2016),
                na.color = 'transparent')

# popup tekst
popup <- paste0(majad_tallinnas_vanusega$aadress, "<br>",
                "Esmane kasutus: ", "<b>", majad_tallinnas_vanusega$esmane_kasutus,
                "<b>")

# koosta kaart hoonete vanustega
kaart <- majad_tallinnas_vanusega %>% 
  # nuuda sf element Spatial-ks, mida leaflet oskab kuvada
  as("Spatial") %>%
  leaflet() %>% 
  addProviderTiles("CartoDB.DarkMatterNoLabels", group = "CartoDB") %>%
  addPolygons(fillColor = ~pal(esmane_kasutus), 
              color = "#b2aeae",
              fillOpacity = 0.7, 
              weight = 0.3, 
              smoothFactor = 0.5,
              popup = popup) %>%
  addLegend(pal = pal, 
            values = majad_tallinnas_vanusega$esmane_kasutus, 
            position = "bottomright", 
            title = "Hoone esmane kasutus",
            labFormat = labelFormat(big.mark = "")) %>% 
  setView(24.7467027, 59.4339144, zoom = 12)

# salvesta interaktiivne kaart blogis kasutamiseks
saveWidget(kaart, file = "majad_tallinnas_vanusega.html", selfcontained = TRUE)

# salvesta kaardist pilt blogis viitamiseks
webshot("majad_tallinnas_vanusega.html", file = "output/majad_tallinnas_vanusega.png",
        cliprect = "viewport", vwidth = 1200, vheight = 900, delay = 1)
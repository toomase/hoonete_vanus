## Eesmärk on koostada teemakaart, kus iga Tallinna hoone on värvitud vastavalt oma vanusele 
## Andmetöötlus Tallinna hoonetele esmase kasutuse info külge sidumiseks
## Aluseks on OpenStreetMap kaart kõigi Tallinna hoonetega
## Hoonete vanused on saadud Ehitusregistrist
## Kuna Ehitusregistris pole koordinaate, siis on tabel seotud aadressregistriga
## Seejärel on omavahel seotud OSM hooned ja hoonete vanused

library(tidyverse)
library(stringr)
library(janitor)
library(rvest)
library(sf)

# funktsioon x, y koordinaatide teisendamiseks lat-long koordinaatideks
source("R/xy_latlong.R")

# lae Eesti asumite piiridega kaart
# andmed pärit aadressilt: http://www.gadm.org/download
eesti_asumid_raw <- read_rds("data/EST_adm3.rds")
eesti_asumid <- st_as_sf(eesti_asumid_raw)

# lae Tallinna hooned
# OSM andmed pärit aadressilt: http://download.bbbike.org/osm/bbbike/Tallinn/
tallinna_hooned_raw <- st_read("data/tal_buildings/buildings.shp")

# Ehitusregistrist kõigi Tallinna majade vanused
# https://www.ehr.ee/app/esileht?1 otsing "Tallinn"
majade_vanus_raw <- read_csv2("data/tallinna_majade_vanus.csv") %>% 
  clean_names() %>% 
  mutate_all(repair_encoding, from = "ISO-8859-1")

# Kogu Eesti aadressiregistri väljavõte, et saada hoonete koordinaadid
# Ehitusregistri andmetega sidumiseks orig_tunnus
# http://xgis.maaamet.ee/adsavalik/ads
aadressiregister_raw <- read_csv2("data/1_17122016_21244_784_1.csv")

# Paranda kodeeringud
aadressiregister <- aadressiregister_raw %>% 
  clean_names() %>% 
  mutate_all(repair_encoding, from = "ISO-8859-1") %>% 
  select(orig_tunnus, olek, taisaadress, x = viitepunkt_x, y = viitepunkt_y)

# ainult need Tallinna hooned, mille kohta on esmane kasutuse aasta olemas
majade_vanus <- majade_vanus_raw %>% 
  mutate(esmane_kasutus = as.numeric(esmane_kasutus)) %>% 
  filter(ehitis == "Hoone", esmane_kasutus >= 1300, 
         !str_detect(ehitise_nimetus, "lammutatud"),
         !str_detect(ehitisregistri_kood, "kasutusest maas"),
         str_detect(aadress, "Harju maakond, Tallinna linn|Harju maakond, Tallinn")) %>% 
  inner_join(aadressiregister, by = c("ehitisregistri_kood" = "orig_tunnus")) %>% 
  filter(!is.na(x))

# abifunktsioon, mis võimaldab dataframest koordinaate konvertida
xy_latlong_df <- function(df){
  x <- df %>% 
    .$x
  
  y <- df %>% 
    .$y
  
  xy_latlong(x = x, y = y)
}

## teisenda koordinaadid lat-long formaati
## kuna läbi maaameti lehe saba korraga teha 200 tk, siis on kasutatud map funktsiooni
majade_koordinaadid <- majade_vanus %>%
  select(x, y) %>% 
  mutate(tunnus = as.character(cut_interval(row_number(), length = 200))) %>%
  nest(-tunnus) %>%
  mutate(tunnused = map(data, xy_latlong_df)) %>%
  unnest(tunnused) 

# lisa lat long koordinaadid majade vanuse tabelile
majade_vanus_koordinaatidega <- majade_vanus %>%
  select(-x, -y, -x8, -ehitis) %>% 
  bind_cols(majade_koordinaadid) %>% 
  mutate(latitude = as.numeric(word(tunnused, 1, sep = fixed(","))),
         longitude = as.numeric(word(tunnused, 2, sep = fixed(",")))) %>% 
  select(-tunnus, -tunnused)

# Leia Tallinna hoonete nimekirjast need, mille kohta on alla 3 koordinaadi
# Need annavad edasises andmetöötluse errorit ja tuleb seetõttu eemaldada
tallinna_hooned_vead <- tallinna_hooned_raw %>% 
  st_geometry() %>% 
  as.character() %>% 
  data_frame(koord = .) %>% 
  mutate(komakohad = str_count(koord, "\\,"),
         jrk = row_number()) %>% 
  # leia need read, mille kohta on kokku 6 lat/long koordinaati
  filter(komakohad == 5) %>% 
  .$jrk

# eemalda vigased polügonid
tallinna_hooned <- tallinna_hooned_raw %>% 
  filter(!row_number() %in% tallinna_hooned_vead)

## Nõmme linnaosa (algselt oli plaanis ainult Nõmme kohta teha)
## neid koode ei kasuta, aga näitena jätan need alles
# nomme_linnaosa <- eesti_asumid %>% 
#   filter(NAME_3 == "Nõmme", TYPE_3 == "Linnaosa")

## leia Nõmme linnaosas olevate hoonete rea numbrid
# nomme_majade_rea_nr <- st_intersects(nomme_linnaosa, tallinna_hooned) %>% 
#   .[[1]]

## Nõmmel asuvad majad
# majad_nommel <- tallinna_hooned %>% 
#   filter(row_number() %in% nomme_majade_rea_nr)

## Kaardil kõik Nõmme majad
# majad_nommel %>% 
#   as("Spatial") %>% 
#   leaflet() %>% 
#   addProviderTiles("CartoDB.Positron") %>% 
#   addPolygons()

# lisa eraldi koordinaatide veerg, et saaks teha spatialpointsdataframe objekti
# see on vajalik andmete sidumiseks majade polygoni tabeliga
coordinates(majade_vanus_koordinaatidega) <- ~ longitude + latitude

# anna projektsioon
proj4string(majade_vanus_koordinaatidega) <- CRS("+proj=longlat")

# muuda projektsioon samaks, mis majade polügoni tabelil
majade_vanus_koordinaatidega <- spTransform(majade_vanus_koordinaatidega, 
                                                  proj4string(tallinna_hooned %>% as("Spatial")))

# leia need majad (osm_id), mille kohta on vanus olemas
majade_asukoht_ja_vanus <- over(majade_vanus_koordinaatidega, tallinna_hooned %>% 
                                  as("Spatial"))

# hoonete polügoni tabeli kujul ja samas järjekorras hoone esmane kasutuse aasta
esmane_kasutus <- majade_vanus_koordinaatidega %>% 
  as.data.frame() %>% 
  tbl_df() %>% 
  bind_cols(majade_asukoht_ja_vanus) %>% 
  select(osm_id, esmane_kasutus, aadress) %>% 
  right_join(tallinna_hooned) %>% 
  distinct(osm_id, .keep_all = TRUE) %>% 
  select(esmane_kasutus, aadress)

# Majade polügonidele esmase kasutuse aasta külge
majad_tallinnas_vanusega <- tallinna_hooned %>% 
  bind_cols(esmane_kasutus) %>% 
  filter(!is.na(esmane_kasutus)) %>% 
  mutate(aadress = str_replace(aadress, "Harju maakond, Tallinn, ", ""))

# Salvesta andmed, kus on Tallinna hooned vanusega
save(majad_tallinnas_vanusega, file = "output/majad_tallinnas_vanusega.RData")
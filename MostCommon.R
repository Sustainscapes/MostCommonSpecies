library(tidyverse)
library(data.table)
library(janitor)

Artlist <- data.table::fread("artsliste.csv") %>% as.data.frame()

library(readr)

frekvens2 <- read_csv("alledata-frekvens2.txt") %>%
  janitor::clean_names() %>%
  as_tibble()

frekvens2$species <- str_split(frekvens2$specieslist, ",")

frekvens2 <- frekvens2 %>%
  dplyr::select(-specieslist) %>%
  unnest(species) %>%
  mutate(species = str_trim(species, side = "both"),
         species = str_remove_all(species, "\\}"),
         site = str_remove_all(site, "\\{"),
         species = as.numeric(species)) %>%
  rename(ArtID = species) %>%
  left_join(Artlist) %>%
  dplyr::select("site", "plot", "year", "ArtID", "NavnDansk",
                "LatArt", "LatFam", "LatOrd", "LatKla",
                "LatDivision", "LatRige")

## number of species per family

PerFamily <- frekvens2 %>%
  dplyr::select(LatArt, LatFam) %>%
  distinct() %>%
  group_by(LatFam) %>%
  summarise(Species_number = n()) %>%
  arrange(desc(Species_number)) %>%
  rename(Family = LatFam)


PerGenus <- frekvens2

PerGenus$Genus <- str_split(frekvens2$LatArt," ", simplify = T)[,1]

PerGenus <- PerGenus %>%
  dplyr::select(LatArt, Genus) %>%
  distinct() %>%
  group_by(Genus) %>%
  summarise(Species_number = n()) %>%
  arrange(desc(Species_number))

PerSpeciesPlot <- frekvens2 %>%
  dplyr::select("site", "plot", "LatArt") %>%
  distinct() %>%
  group_by(LatArt) %>%
  summarise(Ocurrences = n()) %>%
  arrange(desc(Ocurrences))


PerSpeciesSite <- frekvens2 %>%
  dplyr::select("site", "LatArt") %>%
  distinct() %>%
  group_by(LatArt) %>%
  summarise(Ocurrences = n()) %>%
  arrange(desc(Ocurrences))


### Model habitat type

Sites <- data.table::fread("/home/derek/Documents/denmark_stratification/Novana/alledata-abiotiske.csv") %>%
  as.data.frame() %>%
  dplyr::select("site", "plot", "year", "UTMx", "UTMy", "sekhabtype") %>%
  mutate(site = as.character(site)) %>%
  left_join(frekvens2)


library(caret)

ForModel <- Sites %>%
  dplyr::select("sekhabtype","LatArt")



ForModel %>% dplyr::group_by(sekhabtype) %>% summarise(n = n()) %>% dplyr::arrange(desc(n))



DummyMod <- dummyVars(~ LatArt, data = ForModel)

ForModel2 <- predict(DummyMod, ForModel)



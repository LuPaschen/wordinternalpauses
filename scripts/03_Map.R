# R script 3/3 for submission "word-internal pauses"

library(tidyverse)
library(lingtypology)

# All 53 languages from DoReCo 2.0
languages_map_53 <- data.frame(lang = c("Anal", "Arapaho", "Isimjeega-Rootigaanga", "Bainounk-Gujaher", "Beja", "Bora", "Cabécar", "Cashinahua", "Daakie", "Ngalkbun",
                                        "Dolgan", "English", "Evenki", "Orkon-Fanbak", "French", "Goemai", "Gorowa", "Gurindji", "Ho-Chunk", "Jehai",
                                        "Jejueo", "Kakabe", "Kamas-Koibal", "Anta-Komnzo-Wára-Wérè-Kémä", "Light Warlpiri", "Lower Sorbian", "Trinitario-Javeriano-Loretano", "Movima", "Nafsan", "Nisvai",
                                        "N||ng", "Northern Alta", "Northern Kurdish", "Pnar", "Resígaro", "Ruuli", "Sadu", "Sanzhi-Icari", "Savosavo", "Sumi Naga",
                                        "Svan", "Karko", "Tabasaran", "Teop", "Texistepec Popoluca", "Totoli", "Kipchak Urum", "Vera'a", "Warlpiri", "Angguruk Yali",
                                        "Yongning Na", "Yucatec Maya", "Yuracaré"),
                               macro_area = c("Eur", "NAm", "Afr", "Afr", "Afr", "SAm", "NAm", "SAm", "Pap", "Aus",
                                              "Eur", "Eur", "Eur", "Pap", "Eur", "Afr", "Afr", "Aus", "NAm", "Eur",
                                              "Eur", "Afr", "Eur", "Pap", "Aus", "Eur", "SAm", "SAm", "Pap", "Pap",
                                              "Afr", "Pap", "Eur", "Eur", "SAm", "Afr", "Eur", "Eur", "Pap", "Eur",
                                              "Eur", "Afr", "Eur", "Pap", "NAm", "Pap", "Eur", "Pap", "Aus", "Pap",
                                              "Eur", "NAm", "SAm"))

# Filtering: Only languages with at least one <<wip>> label
languages_map <- languages_map_53 %>%
  filter(!(lang %in% c("Light Warlpiri", "French", "Goemai", "Anta-Komnzo-Wára-Wérè-Kémä", "N||ng", "Northern Kurdish", "Sumi Naga", "Texistepec Popoluca", "Vera'a", "Yucatec Maya"))) # we also have to exclude Light Warlpiri because Glottolog has no coordinates for it / coordinates would be the same as Warlpiri

map.feature(languages = languages_map$lang,
            features= languages_map$macro_area,
            tile = "Esri.WorldGrayCanvas",
            color = c("purple", "blue", "red", "black", "green", "orange"),
            legend = FALSE)

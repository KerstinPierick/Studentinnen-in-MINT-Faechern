#########################################################################################
#########################################################################################
###
###                          Studentinnen in MINT-Fächern 
###         
###                     Skript 1: Zusammenfügen der Datensätze
### 
###
### Autoren:
### Roman Link (rlink@uni-goettingen.de)
### Kerstin Pierick (kerstin.pierick@uni-goettingen.de)
###
### Februar 2019
###
#########################################################################################
#########################################################################################

# Pakete laden --------------------------------------------------------------------------
library(tidyverse)

# Daten einlesen ------------------------------------------------------------------------

# Studienanfänger
anfg <- read_csv2("Daten/tidy/studienanfaenger_tidy.csv", locale = locale(encoding = "latin1"))
# Es gibt Probleme mit den Umlauten
unique(anfg$fach_name) %>% sort


# Studierende
studis <- read_csv2("Daten/tidy/studierende_tidy.csv", locale = locale(encoding = "latin1"))


# Fächergruppe 
fg <- read_csv("Daten/tidy/faecherklassifikation.csv")

# Daten zusammenfügen -------------------------------------------------------------------

dat0 <- anfg %>%
  # joint Studienanfänger und Studierende
  left_join(studis) %>%
  # bringt den fach_code in die richtige Form
  mutate(fach_code = as.numeric(str_sub(fach_code, 3, 5))) %>%
  left_join(fg)

# Daten für unsere Zwecke zusammenfassen-------------------------------------------------

dat <- dat0 %>%
  # Ich habe hier erstmal deutsche und ausländische Studierende mit reingenommen, wir können 
  # überlegen, ob es sinnvoller ist, nur Deutsche zu nehmen 
  select(semester, bundesland, fg_code, fg_name, fach_name, 
         anfaenger_insg_w, anfaenger_insg_m, studis_insg_w, studis_insg_m) %>%
  gather(key, anzahl, -semester, -bundesland, -fg_code, -fg_name, -fach_name) %>%
         # Die Bindestriche durch Nullen ersetzen
  mutate(anzahl = as.numeric(str_replace(anzahl, "-", "0")),
         # Neue Variable Mint: 1 für Mathe/Naturwissenschaften/Ingenieur, 0 für Rest
         mint = if_else(fg_code %in% c(4, 8), 1, 0),
         # Neue Variable: Extrahiert das Jahr aus semester
         jahr = as.numeric(str_sub(semester, 4, 7))) %>%
  # Den key in einzelne Variablen aufteilen
  separate(key, c("studi_typ", "nationalitaet", "geschlecht"), sep = "_") %>%
  group_by(semester, fg_code, fg_name, fach_name, 
           studi_typ, nationalitaet, geschlecht, jahr, mint) %>%
  # Aufsummieren der Studis aller Bundesländer
  summarise(anzahl = sum(anzahl)) %>%
  ungroup() %>%
  # Unnötige Variablen raus
  select(-semester, -nationalitaet)

  
  
sapply(dat, class)

# Export --------------------------------------------------------------------------------
write.csv(dat, "Daten/tidy/studianfg_zusammfssg.csv", row.names = FALSE)


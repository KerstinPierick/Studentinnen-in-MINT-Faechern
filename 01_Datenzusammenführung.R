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
anfg <- read_csv2("Daten/tidy/studienanfaenger_tidy.csv")
# Es gibt Probleme mit den Umlauten

# Fächergruppe 
fg <- read_csv("Daten/tidy/faecherklassifikation.csv")

# Daten zusammenfügen -------------------------------------------------------------------

dat0 <- anfg %>%
  # bringt den fach_code in die richtige Form
  mutate(fach_code = as.numeric(str_sub(fach_code, 3, 5))) %>%
  left_join(fg)

# Daten für unsere Zwecke zusammenfassen-------------------------------------------------

dat <- dat0 %>%
  # Die Bindestriche durch Nullen ersetzen
  mutate(frauen  = as.numeric(str_replace(anfaenger_insg_w, "-", "0")),
         maenner = as.numeric(str_replace(anfaenger_insg_m, "-", "0"))) %>%
  select(semester, bundesland, frauen, maenner, fg_code, fg_name, fach_name) %>%
  group_by(semester, fg_code, fg_name, fach_name) %>%
  summarise(frauen = sum(frauen),
            maenner = sum(maenner)) %>%
  # Neue Variable Mint: 1 für Mathe/Naturwissenschaften/Ingenieur, 0 für Rest
  mutate(mint = if_else(fg_code %in% c(4, 8), 1, 0),
         # Neue Variable: Extrahiert das Jahr aus semester
         jahr = str_sub(semester, 4, 7))

# Export --------------------------------------------------------------------------------

write_csv(dat, "Daten/tidy/studianfg_zusammfssg.csv")


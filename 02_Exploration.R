#########################################################################################
#########################################################################################
###
###                          Studentinnen in MINT-Fächern 
###         
###                          Skript 2: Erste Exploration
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

dat <- read_csv("Daten/tidy/studianfg_zusammfssg.csv") 

# Abbildungen für alle Fachgruppen ------------------------------------------------------

# Studi-Anfänger
dat %>%
  filter(studi_typ == "anfaenger") %>%
  group_by(jahr, geschlecht, mint, fg_name) %>%
  summarise(anzahl = sum(anzahl)) %>%
  ggplot(dat, mapping = aes(x = jahr, y = anzahl, fill = geschlecht)) + 
  geom_area() +
  facet_wrap(~fg_name, scales = "free")

# Studierende
dat %>%
  filter(studi_typ == "studis") %>%
  group_by(jahr, geschlecht, mint, fg_name) %>%
  summarise(anzahl = sum(anzahl)) %>%
  ggplot(dat, mapping = aes(x = jahr, y = anzahl, fill = geschlecht)) + 
  geom_area() +
  facet_wrap(~fg_name, scales = "free")

# Abbildung Mint/Rest-- -----------------------------------------------------------------

# Studi-Anfänger
dat %>%
  filter(studi_typ == "anfaenger") %>%
  group_by(jahr, geschlecht, mint) %>% 
  summarise(anzahl = sum(anzahl)) %>%
  ggplot(dat, mapping = aes(x = jahr, y = anzahl, fill = geschlecht)) + 
  geom_area() +
  facet_wrap(~mint, scales = "free")

# Studierende
dat %>%
  filter(studi_typ == "studis") %>%
  group_by(jahr, geschlecht, mint) %>% 
  summarise(anzahl = sum(anzahl)) %>%
  ggplot(dat, mapping = aes(x = jahr, y = anzahl, fill = geschlecht)) + 
  geom_area() +
  facet_wrap(~mint, scales = "free")


# Frauenanteil Unterschied 1998/2017: Studiengangs-Ranking -------------------------------

dat_frauenant <- dat %>%
  spread(geschlecht, anzahl) %>%
  mutate(frauenanteil = 100 * w / (w + m)) %>%
  # Nur das erste und letze Jahr, und keine Studiengänge mit weniger als 100 Studienanfängern
  filter(jahr %in% c(1998, 2017) & (w + m) >= 100) %>%
  select(-w, -m) %>%
  spread(jahr, frauenanteil) %>%
  mutate(aenderung = `2017` - `1998`)

# Für Studienanfänger und Studierende gemischt ---------

# Die meisten Frauen 1998
arrange(dat_frauenant, desc(`1998`))
# Die wenigsten Frauen 1998 
arrange(dat_frauenant, `1998`)
# Die meisten Frauen 2017
arrange(dat_frauenant, desc(`2017`))
# Die wenigsten Frauen 2017
arrange(dat_frauenant, `2017`)
# Die stärksten Zunahmen des Frauenanteils
arrange(dat_frauenant, desc(aenderung))
# Die stärksten Abnahmen des Frauenanteils
arrange(dat_frauenant, aenderung)

# Plot Änderung Vergleich Mint/Nicht Mint
dat_frauenant %>% 
  ggplot(aes(x = aenderung, y = mint, color = mint)) +
  geom_jitter() +
  facet_wrap(~studi_typ, scales =  "free")

# Scatterplot Studierende ~ Studi-Anfänger ----------------------------------------------
dat %>% 
  spread(geschlecht, anzahl) %>%
  mutate(frauenanteil = 100 * w / (w + m)) %>%
  spread(studi_typ, frauenanteil) %>%
  ggplot(aes(anfaenger, studis)) +
  geom_point()

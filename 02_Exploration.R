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
library(forcats)
library(plotly)

# Grafiksettings laden
source("R/gg_screen_themes_20190302.R")

# Daten einlesen ------------------------------------------------------------------------

dat <- read_csv("Daten/tidy/studianfg_zusammfssg.csv", 
                 col_types = list(jahr = col_double()),
                 locale = locale(encoding = "UTF-8")) 
dat

# Wurden Umlaute korrekt dargestellt?
unique(dat$fach_name) %>% sort # ja
# gibt es NA-Werte?
sapply(dat, function(x) sum(is.na(x)))

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

dat_frauenant %>% 
  ggplot(aes(x = mint, y = aenderung, color = mint)) +
  stat_summary(geom = "pointrange", fun.data = "mean_cl_boot") +
  geom_violin() +
  facet_wrap(~studi_typ, scales =  "free") +
  geom_hline(aes(yintercept = 0), lty = 2) +
  coord_flip()


# Lineplot Frauenanteile pro Disziplin --------------------------------------------------
anteile <- dat %>% 
  group_by(fg_name, fach_name, jahr, studi_typ, mint, geschlecht) %>%
  summarize(anzahl = sum(anzahl)) %>%
  spread(geschlecht, anzahl) %>%
  mutate(frauenanteil = 100 * w / (m + w)) %>%
  filter(!is.na(frauenanteil))

anteile %>%
  ggplot(aes(x = jahr, y = frauenanteil)) +
  stat_summary(aes(col = fg_name), geom = "line", fun.y = mean, alpha = 0.3) +
  stat_summary(geom = "line", fun.y = mean) +
  stat_summary(geom = "ribbon", fun.ymin = function(x) mean(x) - 2 * sd(x) / sqrt(length(x)), 
                                fun.ymax = function(x) mean(x) + 2 * sd(x) / sqrt(length(x)),
               fill = "grey", alpha = 0.4) +
#  geom_line(aes(group = fg_name), alpha = 0.2) +
  geom_hline(aes(yintercept = 50), lty = 2) +
  facet_grid(mint ~ studi_typ)



# Nach Studienbereichen -----------------------------------------------------------------

# Datensatz umbauen: Frauenanteile 
anteile_sb <- dat %>%
  group_by(fg_code, fg_name, sb_name, studi_typ, geschlecht, jahr, mint) %>%
  summarise(anzahl = sum(anzahl)) %>%
  spread(geschlecht, anzahl) %>%
  mutate(frauenanteil = 100 * w / (w + m)) 

# Abbildung Studienanfänger: Alle Fächergruppen

anteile_sb %>%
  filter(studi_typ == "anfaenger", fg_code != 10) %>%
  ungroup() %>%
  mutate(fg_name = fct_reorder(fg_name, frauenanteil, mean, na.rm  = T)) %>%
  ggplot(aes(x = jahr, y = frauenanteil)) +
  stat_summary(geom = "ribbon", 
               fun.ymin = function(x) mean(x) -  sd(x) / sqrt(length(x)), 
               fun.ymax = function(x) mean(x) +  sd(x) / sqrt(length(x)),
               fill = "lightgrey", alpha = 0.8) +
  geom_line(aes( group = sb_name), color = "steelblue", alpha = 0.7) +
  facet_wrap(~fg_name, ncol = 2) +
  stat_summary(geom = "line", fun.y = mean, lwd = 1.15) +
  geom_hline(aes(yintercept = 50), lty = 2) +
  theme_pub()

# Abbildung Studienanfänger: Nur Naturwissenschaften

anteile_sb %>%
  filter(studi_typ == "anfaenger", fg_code == 4) %>%
  ggplot(aes(x = jahr, y = frauenanteil, color = sb_name)) +
  geom_line() +
  facet_wrap(~fg_name)

# Abbildung Studienanfänger: Nur Ingenieure

anteile_sb %>%
  filter(studi_typ == "anfaenger", fg_code == 8) %>%
  ggplot(aes(x = jahr, y = frauenanteil, color = sb_name)) +
  geom_line() +
  facet_wrap(~fg_name)

# Abbildung Studierende: Alle Fächergruppen
anteile_sb %>%
  filter(studi_typ == "studis", fg_code != 10) %>%
  ungroup() %>%
  mutate(fg_name = fct_reorder(fg_name, frauenanteil, mean, na.rm  = T)) %>%
  ggplot(aes(x = jahr, y = frauenanteil)) +
  stat_summary(geom = "ribbon", 
               fun.ymin = function(x) mean(x) -  sd(x) / sqrt(length(x)), 
               fun.ymax = function(x) mean(x) +  sd(x) / sqrt(length(x)),
               fill = "lightgrey") +
  geom_line(aes( group = sb_name), color = "steelblue", alpha = 0.7) +
  facet_wrap(~fg_name) +
  stat_summary(geom = "line", fun.y = mean, lwd = 1.15) +
  geom_hline(aes(yintercept = 50), lty = 2)

# Abbildung Studierende: Nur Naturwissenschaften

anteile_sb %>%
  filter(studi_typ == "studis", fg_code == 4) %>%
  ggplot(aes(x = jahr, y = frauenanteil, color = sb_name)) +
  geom_line() +
  facet_wrap(~fg_name)

# Abbildung Studierende: Nur Ingenieure

anteile_sb %>%
  filter(studi_typ == "studis", fg_code == 8) %>%
  ggplot(aes(x = jahr, y = frauenanteil, color = sb_name)) +
  geom_line() +
  facet_wrap(~fg_name)





anteile_sb %>%
  filter(fg_code != 10) %>%
  ungroup() %>% 
  select(-m, -w) %>%
  spread(studi_typ, frauenanteil) %>%
  mutate(prop_weibl_beginner = anfaenger / studis) %>%
  ggplot(aes(x = jahr, y = prop_weibl_beginner)) +
  stat_summary(geom = "ribbon", 
               fun.ymin = function(x) mean(x) -  sd(x) / sqrt(length(x)), 
               fun.ymax = function(x) mean(x) +  sd(x) / sqrt(length(x)),
               fill = "lightgrey") +
  geom_line(aes( group = sb_name), color = "steelblue", alpha = 0.7) +
  facet_wrap(~fg_name) +#, scales = "free") +
  stat_summary(geom = "line", fun.y = mean, lwd = 1.15) +
  geom_hline(aes(yintercept = 1), lty = 2)













# Rankings nach Studienbereichen --------------------------------------------------------

aenderung_sb <- anteile_sb %>% 
  select(-m, -w) %>%
  filter(jahr %in% c(1998, 2017)) %>%
  spread(jahr, frauenanteil) %>%
  mutate(aenderung = `2017` - `1998`)

# Rankings Studienanfänger -------

aenderung_anfg <- filter(aenderung_sb, studi_typ == "anfaenger")

# Die meisten Frauen 1998
arrange(aenderung_anfg, desc(`1998`))
# Die wenigsten Frauen 1998 
arrange(aenderung_anfg, `1998`)
# Die meisten Frauen 2017
arrange(aenderung_anfg, desc(`2017`))
# Die wenigsten Frauen 2017
arrange(aenderung_anfg, `2017`)
# Die stärksten Zunahmen des Frauenanteils
arrange(aenderung_anfg, desc(aenderung))
# Die stärksten Abnahmen des Frauenanteils
arrange(aenderung_anfg, aenderung)

# Rankings Studierende -------

aenderung_stud <- filter(aenderung_sb, studi_typ == "studis")

# Die meisten Frauen 1998
arrange(aenderung_stud, desc(`1998`))
# Die wenigsten Frauen 1998 
arrange(aenderung_stud, `1998`)
# Die meisten Frauen 2017
arrange(aenderung_stud, desc(`2017`))
# Die wenigsten Frauen 2017
arrange(aenderung_stud, `2017`)
# Die stärksten Zunahmen des Frauenanteils
arrange(aenderung_stud, desc(aenderung))
# Die stärksten Abnahmen des Frauenanteils
arrange(aenderung_stud, aenderung)

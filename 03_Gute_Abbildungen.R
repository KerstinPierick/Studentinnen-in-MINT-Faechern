#########################################################################################
#########################################################################################
###
###                          Studentinnen in MINT-Fächern 
###         
###                           Skript 3: Gute Abbildungen
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
library(htmlwidgets)

source("R/gg_screen_themes_20190302.R")

# Daten einlesen ------------------------------------------------------------------------

dat <- read_csv("Daten/tidy/studianfg_zusammfssg.csv", 
                col_types = list(jahr = col_double()),
                locale = locale(encoding = "UTF-8")) 

anteile_sb <- dat %>%
  group_by(fg_code, fg_name, sb_name, studi_typ, geschlecht, jahr, mint) %>%
  summarise(anzahl = sum(anzahl)) %>%
  spread(geschlecht, anzahl) %>%
  mutate(frauenanteil = 100 * w / (w + m)) 

# Studienanfänger -----------------------------------------------------------------------

(p_anfg <- anteile_sb %>%
  rename(Jahr = jahr, Studienbereich = sb_name, Frauenanteil = frauenanteil) %>%
  filter(studi_typ == "anfaenger", fg_code != 10) %>%
  ungroup() %>%
  mutate(fg_name = gsub("ae", "ä", fg_name),
    fg_name = fct_reorder(fg_name, Frauenanteil, mean, na.rm  = T)) %>%
  ggplot(aes(x = Jahr, y = Frauenanteil)) +
  stat_summary(geom = "ribbon", 
               fun.ymin = function(x) mean(x) -  sd(x) / sqrt(length(x)), 
               fun.ymax = function(x) mean(x) +  sd(x) / sqrt(length(x)),
               fill = "lightgrey", alpha = 0.8) +
  geom_line(aes( group = Studienbereich), color = "steelblue", alpha = 0.7) +
  facet_wrap(~fg_name, ncol = 2) +
  stat_summary(geom = "line", fun.y = mean, lwd = 1.15) +
  geom_hline(aes(yintercept = 50), lty = 2) +
  labs(title = "Studienanfänger", x = "Jahr", y = "Frauenanteil (%)") +
  theme_screen())# +
 #coord_equal(ratio = 5/25)) +


(p_anfg1 <- ggplotly(p_anfg, width = 800, height = 1400))
saveWidget(p_anfg1, "p_anfaenger.html")

# Studierende ---------------------------------------------------------------------------

(p_stud <- anteile_sb %>%
   rename(Jahr = jahr, Studienbereich = sb_name, Frauenanteil = frauenanteil) %>%
   filter(studi_typ == "studis", fg_code != 10) %>%
   ungroup() %>%
   mutate(fg_name = gsub("ae", "ä", fg_name),
          fg_name = fct_reorder(fg_name, Frauenanteil, mean, na.rm  = T)) %>%
   ggplot(aes(x = Jahr, y = Frauenanteil)) +
   stat_summary(geom = "ribbon", 
                fun.ymin = function(x) mean(x) -  sd(x) / sqrt(length(x)), 
                fun.ymax = function(x) mean(x) +  sd(x) / sqrt(length(x)),
                fill = "lightgrey", alpha = 0.8) +
   geom_line(aes( group = Studienbereich), color = "steelblue", alpha = 0.7) +
   facet_wrap(~fg_name, ncol = 2) +
   stat_summary(geom = "line", fun.y = mean, lwd = 1.15) +
   geom_hline(aes(yintercept = 50), lty = 2) +
   labs(title = "Studierende", x = "Jahr", y = "Frauenanteil (%)") +
   theme_screen())# +
#coord_equal(ratio = 5/25)) +


(p_stud1 <- ggplotly(p_stud, width = 800, height = 1400))
saveWidget(p_stud1, "p_studis.html")

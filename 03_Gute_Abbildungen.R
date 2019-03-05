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

# Graphiksettings laden
source("R/gg_screen_themes_20190302.R")

# 01 Grafiken für Studienanfänger -------------------------------------------------------

# a) Daten einlesen ---------------------------------------------------------------------
# Einlesen der Rohdaten
dat <- read_csv("Daten/tidy/studianfg_zusammfssg.csv", 
                col_types = list(jahr = col_double()),
                locale = locale(encoding = "UTF-8")) 

# Anteile auf Studienbereichslevel
anteile_sb <- dat %>%
  group_by(fg_code, fg_name, sb_name, studi_typ, geschlecht, jahr, mint) %>%
  summarise(anzahl = sum(anzahl)) %>%
  ungroup() %>%
  spread(geschlecht, anzahl) %>%
  mutate(frauenanteil = round(100 * w / (w + m), digits = 1)) %>%
  group_by(sb_name) %>%
  mutate(n_stud = sum(m + w)) %>%
  ungroup() 
anteile_sb

# Anteile auf Fächergruppenlevel
anteile_fg <- dat %>%
  group_by( fg_name, studi_typ, geschlecht, jahr, fg_code) %>%
  summarise(anzahl = sum(anzahl)) %>%
  ungroup() %>%
  spread(geschlecht, anzahl) %>%
  mutate(Frauenanteil = round(100 * w / (w + m), digits = 1)) %>%
  filter(!is.na(Frauenanteil)) %>%
  rename(Jahr = jahr) %>%
  filter(fg_code != 10) %>%
  ungroup() %>%
  mutate(fg_name = gsub("ae", "ä", fg_name),
         fg_name = fct_reorder(fg_name, Frauenanteil, mean, na.rm  = T))
anteile_fg

# Pseudodatensatz für Branding
labs <- tibble(fg_name = unique(anteile_fg$fg_name), # nötig um Änderung der Facet-Reihenfolge zu vermeiden
               x = 2007.5,
               y = 12,
               label = ifelse(fg_name == "Geisteswissenschaften",
                 "Roman Matthias Link und Kerstin Pierick 2019\nQuelle: Statistisches Bundesamt Deutschland",
                 NA)
)

# Studienanfänger -----------------------------------------------------------------------
# Erstellen von ggplot2-Objekt
(p_anfg <- anteile_sb %>%
  rename(Jahr = jahr, Studienbereich = sb_name, Frauenanteil = frauenanteil) %>%
  filter(studi_typ == "anfaenger", fg_code != 10) %>%
  ungroup() %>%
  mutate(fg_name = gsub("ae", "ä", fg_name),
    fg_name = fct_reorder(fg_name, Frauenanteil, mean, na.rm  = T)) %>%
  ggplot(aes(x = Jahr)) +
  geom_line(aes(y = Frauenanteil, group = Studienbereich), color = "steelblue", alpha = 0.7) +
  geom_line(data = filter(anteile_fg, studi_typ == "anfaenger"), aes(y = Frauenanteil), lwd = 1.15) + 
  geom_text(data = labs, aes(x = x, y = y, label = label), size = 3.2) +
  facet_wrap(~fg_name, ncol = 2) +
  geom_hline(aes(yintercept = 50), lty = 2) +
  labs(title = "Frauenanteil bei den Studienanfängern", x = "Jahr", y = "Frauenanteil (%)\n") +
  theme_screen() +
  theme(plot.margin = unit(c(3, 3, 3, 10), "mm"))) 

# Konvertieren in plotly html-Widget
p_anfg1 <- ggplotly(p_anfg, width = 700, height = 1050)
# Export als html-Widget
saveWidget(p_anfg1, "p_anfaenger.html")

# Studierende ---------------------------------------------------------------------------

(p_stud <- anteile_sb %>%
   rename(Jahr = jahr, Studienbereich = sb_name, Frauenanteil = frauenanteil) %>%
   filter(studi_typ == "studis", fg_code != 10) %>%
   ungroup() %>%
   mutate(fg_name = gsub("ae", "ä", fg_name),
          fg_name = fct_reorder(fg_name, Frauenanteil, mean, na.rm  = T)) %>%
   ggplot(aes(x = Jahr)) +
   # geom_ribbon(data = anteile_fg, aes(ymin = Frauenanteil - 2*standardfehler * 1000,
   #                                    ymax = Frauenanteil + 2*standardfehler * 1000),
   #             fill = "lightgrey", alpha = 0.8) + 
   geom_line(aes(y = Frauenanteil, group = Studienbereich), color = "steelblue", alpha = 0.7) +
   geom_line(data = filter(anteile_fg, studi_typ == "studis"), aes(y = Frauenanteil), lwd = 1.15) + 
   facet_wrap(~fg_name, ncol = 2) +
   geom_hline(aes(yintercept = 50), lty = 2) +
   labs(title = "Frauenanteil bei den Studierenden", x = "Jahr", y = "Frauenanteil (%)\n", 
        caption = "Roman Matthias Link und Kerstin Pierick 2019 
                  Quelle: Statistisches Bundesamt Deutschland") +
   theme_screen() +
   theme(plot.margin = unit(c(3, 3, 3, 10), "mm"))) 


p_stud1 <- ggplotly(p_stud, width = 700, height = 1050) %>%
  layout(annotations = list(x = 2012, y = 8, font = list(size =8),
                            text = "Roman Matthias Link und Kerstin Pierick 2019
Quelle: Statistisches Bundesamt Deutschland", 
                            showarrow = FALSE))
saveWidget(p_stud1, "p_studis.html")

# Absolute Zahlen Studi-Anfänger --------------------------------------------------------

dat %>%
  filter(studi_typ == "anfaenger") %>%
  filter(fg_code != 10)  %>%
  group_by(jahr, geschlecht, mint, fg_name) %>%
  summarise(anzahl = sum(anzahl)) %>%
  ggplot(dat, mapping = aes(x = jahr, y = anzahl, fill = geschlecht)) + 
  geom_area() +
  facet_wrap(~fg_name, scales = "free", ncol = 2)

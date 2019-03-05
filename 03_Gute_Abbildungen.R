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

# 01 Daten einlesen und vorbereiten -----------------------------------------------------
# Einlesen der Rohdaten
dat <- read_csv("Daten/tidy/studianfg_zusammfssg.csv", 
                col_types = list(jahr = col_double()),
                locale = locale(encoding = "UTF-8")) %>%
  mutate(anzahl = ifelse(is.na(anzahl), 0, anzahl)) # NA-Studierendenzahlen (von nicht existenten Fächern) für Summen auf 0 setzen

# Anteile auf Studienbereichslevel
anteile_sb <- dat %>%
  group_by(fg_code, fg_name, sb_name, studi_typ, geschlecht, jahr, mint) %>%
  summarise(anzahl = sum(anzahl, na.rm = T)) %>%
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
  summarise(anzahl = sum(anzahl, na.rm = T)) %>%
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

# 02 Relative Frauenanteile -------------------------------------------------------------
# a) StudienanfängerInnen ---------------------------------------------------------------
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
  geom_text(data = labs, aes(x = x, y = y, label = label), size = 2.24) +
  facet_wrap(~fg_name, ncol = 2) +
  geom_hline(aes(yintercept = 50), lty = 2) +
  labs(title = "Frauenanteil bei den StudienanfängerInnen", x = "Jahr", y = "Frauenanteil (%)\n") +
  theme_screen() +
  theme(plot.margin = unit(c(3, 3, 3, 10), "mm"))) 

# Konvertieren in plotly html-Widget
p_anfg1 <- ggplotly(p_anfg, width = 700, height = 1050)
# Export als html-Widget
saveWidget(p_anfg1, "p_rel_anf.html")

# b) Studierende ------------------------------------------------------------------------
# Erstellen von ggplot2-Objekt
(p_studi <- anteile_sb %>%
   rename(Jahr = jahr, Studienbereich = sb_name, Frauenanteil = frauenanteil) %>%
   filter(studi_typ == "studis", fg_code != 10) %>%
   ungroup() %>%
   mutate(fg_name = gsub("ae", "ä", fg_name),
          fg_name = fct_reorder(fg_name, Frauenanteil, mean, na.rm  = T)) %>%
   ggplot(aes(x = Jahr)) +
   geom_line(aes(y = Frauenanteil, group = Studienbereich), color = "steelblue", alpha = 0.7) +
   geom_line(data = filter(anteile_fg, studi_typ == "studis"), aes(y = Frauenanteil), lwd = 1.15) + 
   geom_text(data = labs, aes(x = x, y = y, label = label), size = 2.24) +
   facet_wrap(~fg_name, ncol = 2) +
   geom_hline(aes(yintercept = 50), lty = 2) +
   labs(title = "Frauenanteil bei den Studierenden", x = "Jahr", y = "Frauenanteil (%)\n") +
   theme_screen() +
   theme(plot.margin = unit(c(3, 3, 3, 10), "mm"))) 

# Konvertieren in plotly html-Widget
p_studi1 <- ggplotly(p_studi, width = 700, height = 1050)
# Export als html-Widget
saveWidget(p_studi1, "p_rel_stud.html")


# 03 Absolute Zahlen --------------------------------------------------------------------
# vorbereiten des Brandings
labs1 <- mutate(labs, y = 3000)

# vorbereiten der Daten (aus anteile_fg, da hier schon korrekt aggregiert und geordnet)
absdat <- anteile_fg %>%
  select(-Frauenanteil) %>%
  gather(geschlecht, anzahl, m, w) %>%
  mutate(Geschlecht = ifelse(geschlecht == "m", "männlich", "weiblich"))
  
# a) StudienanfängerInnen ---------------------------------------------------------------
# Erstellen des Plots
abs_anf <- absdat %>%
  filter(studi_typ == "anfaenger") %>%
  ggplot() + 
  geom_area(aes(x = Jahr, y = anzahl, fill = Geschlecht), alpha = 0.9, col = 1, size = 0.4) +
  geom_text(data = labs1, aes(x = x, y = y, label = label), size = 2.24) +
  facet_wrap(~fg_name, scales = "free", ncol = 2)  +
  theme_screen() +
  labs(title = "Absolute Zahlen StudienanfängerInnen", x = "Jahr", y = "Anzahl") +
  scale_fill_manual(values = c("#FF8400", "#336B22"))


# Export als plotly html-Widget
ggplotly(abs_anf, width = 700, height = 1050) %>%
saveWidget("p_abs_anf.html")

# b) Studierende ------------------------------------------------------------------------
# Erstellen des Plots
abs_stud <- absdat %>%
  filter(studi_typ == "studis") %>%
  ggplot() + 
  geom_area(aes(x = Jahr, y = anzahl, fill = Geschlecht), alpha = 0.9, col = 1, size = 0.4) +
  geom_text(data = labs1, aes(x = x, y = y, label = label), size = 2.24) +
  facet_wrap(~fg_name, scales = "free", ncol = 2)  +
  theme_screen() +
  labs(title = "Absolute Zahlen Studierende", x = "Jahr", y = "Anzahl") +
  scale_fill_manual(values = c("#FF8400", "#336B22"))

# Export als plotly html-Widget
ggplotly(abs_stud, width = 700, height = 1050) %>%
  saveWidget("p_abs_stud.html")

# 03 Änderungen des Frauenanteils -------------------------------------------------------
# neue Studiengänge
new <- anteile_sb %>% 
  filter(jahr == 1998 & (m + w == 0)) %>%
  .$sb_name %>% unique
  
#  Vorbereitung des Datensatzes
changes <- anteile_sb %>%
  filter(!(sb_name %in% new),
         fg_code != 10) %>%
  rename(Studienbereich = sb_name) %>%
  ungroup() %>%
  mutate(Studienbereich = gsub("ae", "ä", Studienbereich)) %>%
  filter(jahr %in% c(1998, 2017)) %>% 
  group_by(Studienbereich, studi_typ, mint) %>%
  summarize(change = frauenanteil[2] - frauenanteil[1])
changes

# Mittelwerte
changes %>% group_by(mint, studi_typ) %>%
 summarize(change = mean(change))
mean(changes$change)

# mittelwerte Anfänger/Studis
mchange <- tapply(changes$change, changes$studi_typ, mean) %>% round(2)

# Funktion für logischen Vektor für die n extremsten Werte in einem Vektor
ordfun <- function(x, n,  desc = FALSE){
  if(desc) x %in% sort(x, decreasing = TRUE)[1:n]
  else x %in%sort(x)[1:n]
}

# extremste Änderungen bei den Studis
extreme <- changes  %>% 
  ungroup() %>%
  group_by(studi_typ) %>%
  mutate(lo5 = ordfun(change, 10),
         hi5 = ordfun(change, 10, desc = TRUE),
         extreme = lo5 | hi5,
         `Änderung` = round(change, 2),
         Richtung = ifelse(sign(change) > 0, "Zunahme", "Abnahme")) %>%
  filter(extreme) 
extreme

# a) StudienanfängerInnen ---------------------------------------------------------------
`Duchschnittliche Änderung bei den StudienanfängerInnen` <- mchange[1]
# make plot
ext_anf <- extreme %>% 
  ungroup()%>%
  filter(studi_typ == "anfaenger") %>%
  mutate(Studienbereich = gsub("wirtschaftswiss.", "wi.wi.", Studienbereich),
         Studienbereich = gsub("Wirtschafts", "Wirtsch.", Studienbereich),
         Studienbereich = fct_reorder(Studienbereich, change, mean),
         which = ifelse(hi5, "Stärkste Zunahme", "Stärkste Abnahme")) %>%
  ggplot(aes(x = Studienbereich, y = `Änderung`, fill = Richtung)) +
  geom_col(col = 1, size = 0.4, alpha = 0.9) + 
  geom_hline(aes(yintercept = `Duchschnittliche Änderung bei den StudienanfängerInnen`), lty = 2) +
  coord_flip() +
  theme_screen() +
  labs(title = "Änderung des Frauenanteils bei den StudienfängerInnen 1998-2017", y = "Änderung des Frauenanteils") +
  facet_wrap(~which, scales = "free", ncol = 1) +
  scale_fill_manual(values = c("#FF8400", "#336B22")) +
  theme(legend.position = "none",
        axis.title.y = element_blank())

# Export als plotly html-Widget
ggplotly(ext_anf, width = 700, height = 700) %>%
  saveWidget("p_ext_anf.html")

# b) Studierende ------------------------------------------------------------------------
`Duchschnittliche Änderung bei den Studierenden` <- mchange[2]
# make plot
ext_stud <- extreme %>% 
  ungroup()%>%
  filter(studi_typ == "studis") %>%
  mutate(Studienbereich = gsub("wirtschaftswiss.", "wi.wi.", Studienbereich),
         Studienbereich = gsub("Wirtschafts", "Wirtsch.", Studienbereich),
         Studienbereich = fct_reorder(Studienbereich, change, mean),
         which = ifelse(hi5, "Stärkste Zunahme", "Stärkste Abnahme")) %>%
  ggplot(aes(x = Studienbereich, y = `Änderung`, fill = Richtung)) +
  geom_col(col = 1, size = 0.4, alpha = 0.9) + 
  geom_hline(aes(yintercept = `Duchschnittliche Änderung bei den Studierenden`), lty = 2) +
  coord_flip() +
  theme_screen() +
  labs(title = "Änderung des Frauenanteils bei den Studierenden 1998-2017", y = "Änderung des Frauenanteils") +
  facet_wrap(~which, scales = "free", ncol = 1) +
  scale_fill_manual(values = c("#FF8400", "#336B22")) +
  theme(legend.position = "none",
        axis.title.y = element_blank())


# Export als plotly html-Widget
ggplotly(ext_stud, width = 700, height = 700) %>%
  saveWidget("p_ext_stud.html")


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
  mutate(anzahl  = ifelse(is.na(anzahl), 0, anzahl), # NA-Studierendenzahlen (von nicht existenten Fächern) für Summen auf 0 setzen
         fg_name = gsub("wissenchaft", "wissenschaft", fg_name),               # Kürzen der Fachgruppenbezeichnungen
         fg_name = gsub("wissenschaft|wissenschaften", "wiss.", fg_name),
         fg_name = gsub("medizin", "med.", fg_name),
         fg_name = gsub(" und", ",", fg_name),
         fg_name = gsub("ungs", "gs.", fg_name),
         fg_name = gsub("Veterinaer", "Vet.", fg_name),
         fg_name = gsub("ae", "ä", fg_name))       

unique(dat$fg_name)


# Anteile auf Studienbereichslevel
anteile_sb <- dat %>%
  group_by(fg_code, fg_name, sb_name, studi_typ, geschlecht, jahr, mint) %>%
  summarise(anzahl = sum(anzahl, na.rm = T)) %>%
  ungroup() %>%
  spread(geschlecht, anzahl) %>%
  mutate(Frauenanteil = round(100 * w / (w + m), digits = 1),
         fg_name = fct_reorder(fg_name, Frauenanteil, mean, na.rm  = T)) %>%
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
  mutate(fg_name = fct_reorder(fg_name, Frauenanteil, mean, na.rm  = T))
anteile_fg

# Pseudodatensatz für Branding
labs <- tibble(fg_name = unique(anteile_fg$fg_name), # nötig um Änderung der Facet-Reihenfolge zu vermeiden
               x = 2007.5,
               y = 12,
               label = ifelse(fg_name == "Geisteswiss.",
                 "Kerstin Pierick u. Roman Mathias Link, 2019\nQuelle: Statistisches Bundesamt Deutschland",
                 NA)
)

# 02 Relative Frauenanteile -------------------------------------------------------------
# a) StudienanfängerInnen ---------------------------------------------------------------
# Erstellen von ggplot2-Objekt
p_anfg <- anteile_sb %>%
  rename(Jahr = jahr, Studienbereich = sb_name, Frauenanteil = Frauenanteil) %>%
  filter(studi_typ == "anfaenger", fg_code != 10) %>%
  ungroup() %>%
  ggplot(aes(x = Jahr)) +
  geom_line(aes(y = Frauenanteil, group = Studienbereich), color = "steelblue", alpha = 0.7) +
  geom_line(data = filter(anteile_fg, studi_typ == "anfaenger"), aes(y = Frauenanteil), lwd = 1.15) + 
  geom_text(data = labs, aes(x = x, y = y, label = label), size = 2.24) +
  facet_wrap(~fg_name, ncol = 2) +
  geom_hline(aes(yintercept = 50), lty = 2) +
  labs(title = "Frauenanteil bei den StudienanfängerInnen", x = "Jahr", y = "Frauenanteil (%)\n") +
  theme_screen() +
  theme(plot.margin = unit(c(3, 3, 3, 10), "mm")) 

# Konvertieren in plotly html-Widget
p_anfg1 <- ggplotly(p_anfg, width = 700, height = 1050)
# Export als html-Widget
saveWidget(p_anfg1, "p_rel_anf.html")

# b) Studierende ------------------------------------------------------------------------
# Erstellen von ggplot2-Objekt
p_studi <- anteile_sb %>%
   rename(Jahr = jahr, Studienbereich = sb_name, Frauenanteil = Frauenanteil) %>%
   filter(studi_typ == "studis", fg_code != 10) %>%
   ungroup() %>%
   ggplot(aes(x = Jahr)) +
   geom_line(aes(y = Frauenanteil, group = Studienbereich), color = "steelblue", alpha = 0.7) +
   geom_line(data = filter(anteile_fg, studi_typ == "studis"), aes(y = Frauenanteil), lwd = 1.15) + 
   geom_text(data = labs, aes(x = x, y = y, label = label), size = 2.24) +
   facet_wrap(~fg_name, ncol = 2) +
   geom_hline(aes(yintercept = 50), lty = 2) +
   labs(title = "Frauenanteil bei den Studierenden", x = "Jahr", y = "Frauenanteil (%)\n") +
   theme_screen() +
   theme(plot.margin = unit(c(3, 3, 3, 10), "mm")) 

# Konvertieren in plotly html-Widget
p_studi1 <- ggplotly(p_studi, width = 700, height = 1050)
# Export als html-Widget
saveWidget(p_studi1, "p_rel_stud.html")


# 03 Absolute Zahlen --------------------------------------------------------------------
# vorbereiten des Brandings
labs1 <- mutate(labs, y = 5)

# vorbereiten der Daten (aus anteile_fg, da hier schon korrekt aggregiert und geordnet)
absdat <- anteile_fg %>%
  select(-Frauenanteil) %>%
  gather(geschlecht, anzahl, m, w) %>%
  mutate(Geschlecht = ifelse(geschlecht == "m", "männlich", "weiblich"))
  
# a) StudienanfängerInnen ---------------------------------------------------------------
# Erstellen des Plots
abs_anf <- absdat %>%
  filter(studi_typ == "anfaenger") %>%
  mutate(`Anzahl (in 1000 StudienanfängerInnen)` = anzahl/1000) %>%
  ggplot() + 
  geom_area(aes(x = Jahr, y = `Anzahl (in 1000 StudienanfängerInnen)` , fill = Geschlecht), alpha = 0.9, col = 1, size = 0.4) +
  geom_text(data = labs1, aes(x = x, y = y, label = label), size = 2.24) +
  facet_wrap(~fg_name, scales = "free", ncol = 2)  +
  theme_screen() +
  labs(title = "Absolute Zahlen StudienanfängerInnen", x = "Jahr", y = "Anzahl (in 1000 StudienanfängerInnen)\n") +
  scale_fill_manual(values = c("#FF8400", "#336B22")) +
  theme(plot.margin = unit(c(3, 3, 3, 10), "mm"))


# Export als plotly html-Widget
ggplotly(abs_anf, width = 700, height = 1050) %>%
saveWidget("p_abs_anf.html")

# b) Studierende ------------------------------------------------------------------------
# Erstellen des Plots
abs_stud <- absdat %>%
  filter(studi_typ == "studis") %>%
  mutate(`Anzahl (in 1000 Studierenden)` = anzahl/1000) %>%
  ggplot() + 
  geom_area(aes(x = Jahr, y = `Anzahl (in 1000 Studierenden)` , fill = Geschlecht), alpha = 0.9, col = 1, size = 0.4) +
  geom_text(data = mutate(labs1, y = 30), aes(x = x, y = y, label = label), size = 2.24) +
  facet_wrap(~fg_name, scales = "free", ncol = 2)  +
  theme_screen() +
  labs(title = "Absolute Zahlen Studierende", x = "Jahr", y = "Anzahl (in 1000 Studierenden)\n") +
  scale_fill_manual(values = c("#FF8400", "#336B22")) +
  theme(plot.margin = unit(c(3, 3, 3, 10), "mm"))

# Export als plotly html-Widget
ggplotly(abs_stud, width = 700, height = 1050) %>%
  saveWidget("p_abs_stud.html")

# 04 Änderungen des Frauenanteils -------------------------------------------------------
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
  summarize(change = Frauenanteil[2] - Frauenanteil[1])
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
         Richtung = ifelse(sign(change) > 0, "Zunahme", "Abnahme"),
         Studienbereich = gsub("Wirtschaftsingenieurwesen mit wirtschaftswiss. Schwerpunkt", "Wirtsch.ing.wesen (Schwerp. WiWi)", Studienbereich),
         Studienbereich = gsub("Wirtschafts", "Wirtsch.", Studienbereich),
         Studienbereich = gsub("Elektrotechnik und", "Elektro- u.", Studienbereich),
         Studienbereich = gsub("wissenschaft|wissenschaften", "wiss.", Studienbereich),
         Studienbereich = gsub("allgemein", "allg.", Studienbereich),
         Studienbereich = gsub(" und", " u.", Studienbereich),
         which = ifelse(hi5, "Stärkste Zunahme", "Geringste Zunahme"),
         which = fct_reorder(which, lo5, mean)
  ) %>%
  filter(extreme) 
extreme

extreme_labs <- data.frame(x = 1, y = -8, 
                           which = factor("Geringste Zunahme", levels = levels(extreme$which)), 
                           label = "Pierick, K + Link, RM, 2019. Quelle: destatis.de")

# a) StudienanfängerInnen ---------------------------------------------------------------
`Duchschnittliche Änderung bei den StudienanfängerInnen` <- mchange[1]
# make plot
ext_anf <- extreme %>% 
  ungroup()%>%
  filter(studi_typ == "anfaenger") %>%
  mutate(Studienbereich = fct_reorder(Studienbereich, change, mean)) %>%
  ggplot(aes(x = Studienbereich, y = `Änderung`)) +
  geom_hline(aes(yintercept = `Duchschnittliche Änderung bei den StudienanfängerInnen`), lty = 2) +
  geom_col(aes(fill = Richtung), col = 1, size = 0.4, alpha = 0.9) + 
  geom_text(data = extreme_labs, aes(x = x, y = y, label = label), size = 2.24) +
  coord_flip() +
  theme_screen() +
  labs(title = "Änderung des Frauenanteils bei den StudienfängerInnen 1998-2017", y = "\nÄnderung des Frauenanteils (Prozentpunkte)") +
  facet_wrap(~which, scales = "free", ncol = 1) +
  scale_fill_manual(values = c("#5584EF", "#F4914F")) +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        plot.margin = unit(c(3, 3, 10, 3), "mm"))

# Export als plotly html-Widget
ggplotly(ext_anf, width = 700, height = 700) %>%
  saveWidget("p_ext_anf.html")

# b) Studierende ------------------------------------------------------------------------
`Duchschnittliche Änderung bei den Studierenden` <- mchange[2]
# make plot
ext_stud <- extreme %>% 
  ungroup()%>%
  filter(studi_typ == "studis") %>%
  mutate(Studienbereich = fct_reorder(Studienbereich, change, mean)) %>%
  ggplot(aes(x = Studienbereich, y = `Änderung`)) +
  geom_hline(aes(yintercept = `Duchschnittliche Änderung bei den Studierenden`), lty = 2) +
  geom_col(aes(fill = Richtung), col = 1, size = 0.4, alpha = 0.9) + 
  geom_text(data = extreme_labs, aes(x = x, y = y, label = label), size = 2.24) +
  labs(title = "Änderung des Frauenanteils bei den Studierenden 1998-2017", y = "\nÄnderung des Frauenanteils (Prozentpunkte)\n") +
  coord_flip() +
  theme_screen() +
  facet_wrap(~which, scales = "free", ncol = 1) +
  scale_fill_manual(values = c("#5584EF", "#F4914F")) +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        plot.margin = unit(c(3, 3, 10, 3), "mm"))
ext_stud 

# Export als plotly html-Widget
ggplotly(ext_stud, width = 700, height = 700) %>%
  saveWidget("p_ext_stud.html")



# 05 Relation Frauenanteil AnfängerInnen/Studis -----------------------------------------
# Umbauen von Daten auf Fächergruppenlevel
ratio_fg <- anteile_fg %>%
  select(-m, -w) %>%
  spread(studi_typ, Frauenanteil) %>%
  mutate(`Rel. Frauenanteil AnfängerInnen zu Studis` = anfaenger / studis)


# Erstellen von ggplot2-Objekt
ratio <- 
  anteile_sb %>%
  filter(fg_code != 10) %>%
  rename(Jahr = jahr, Studienbereich = sb_name, Frauenanteil = Frauenanteil) %>%
  ungroup() %>%
  mutate(fg_name = gsub("ae", "ä", fg_name),
         fg_name = fct_reorder(fg_name, Frauenanteil, mean, na.rm  = T)) %>%
  select(-m, -w) %>%
  spread(studi_typ, Frauenanteil) %>%
  mutate(`Rel. Frauenanteil AnfängerInnen zu Studis` = anfaenger / studis) %>%
  ggplot(aes(x = Jahr)) +
  geom_line(aes(y = `Rel. Frauenanteil AnfängerInnen zu Studis`, group = Studienbereich), color = "steelblue", alpha = 0.7) +
  geom_line(data = ratio_fg, aes(y = `Rel. Frauenanteil AnfängerInnen zu Studis`), lwd = 1.15) + 
  geom_text(data = mutate(labs, y = 0.8), aes(x = x, y = y, label = label), size = 2.24) +
  facet_wrap(~fg_name, ncol = 2) +
  geom_hline(aes(yintercept = 1), lty = 2) +
  labs(title = "Verhältnis Frauenanteil StudienanfängerInnen zu Studierenden", 
       x = "Jahr", y = "(Frauenanteil AnfängerInnen) / (Frauenanteil Studierende)\n") +
  theme_screen() +
  theme(plot.margin = unit(c(3, 3, 3, 10), "mm"))

# Export als plotly html-Widget
ggplotly(ratio, width = 700, height = 1050) %>%
  saveWidget("p_prob_ratio_Frauenanteil.html")

# Interpretation: Verhältnis > 1 - rel. mehr Frauen unter Studienanfängern als 
# unter Studierenden
# Generell steigen die Studierendenzahlen an, daher ist eine zeitlich verzögerte
# Zunahme der Frauen unter den Studierenden zu erwarten, auch wenn der Anteil der 
# Studienanfängerinnen gleich bleibt.
# Nimmt die Zahl der weiblichen Studis nicht zu gibt es zwei potentielle Erklärungen:
# 1) längere Studiendauer bei Männern
# 2) höhere Abbrecherquoten bei Frauen


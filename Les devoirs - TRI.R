# Notes -------------------------------------------------------------------

# Il faut télécharger les logiciels  : R et RStudio
# https://cran.r-project.org/
# https://rstudio.com/products/rstudio/download/

# Il faut télécharger les données put cet exemple de cette lien :
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/NABLZX


# Nettoyer l'environnement ------------------------------------------------

rm(list = ls())


# Joindre les packages ----------------------------------------------------
# Install and load necessary packages -------------------------------------

update.packages(ask = FALSE)

install.packages("mirt")
install.packages("magrittr")
install.packages("foreign")
install.packages("ggplot2")

library(mirt) # Pour les analyses en utilisant la TRI
library(magrittr) # Pour le pipe %>%
library(foreign) # Pour utiliser les données en SPSS
library(ggplot2) # Pour les graphiques

# Lire les données --------------------------------------------------------

budd <- read.spss("Latinx Health Behaviors and Health Outcomes dataset_7.14.2020.sav",
                  to.data.frame = TRUE)

# Préparer les données ----------------------------------------------------

# Changer les réponses aux chiffres
text_responses <- c("None at all ",
                    "Several days ",
                    "More than half the days ",
                    "Nearly every day")

# 0 = Jamais
# 1 = Plusieurs jours
# 2 = Plus de la moitié des jours
# 3 = Presque tous les jours

budd$phq9_1 <- match(budd$PHQ_1_combine, text_responses) - 1
budd$phq9_2 <- match(budd$PHQ_2_combine, text_responses) - 1
budd$phq9_3 <- match(budd$PHQ_3_combine, text_responses) - 1
budd$phq9_4 <- match(budd$PHQ_4_combine, text_responses) - 1
budd$phq9_5 <- match(budd$PHQ_5_combine, text_responses) - 1
budd$phq9_6 <- match(budd$PHQ_6_combine, text_responses) - 1
budd$phq9_7 <- match(budd$PHQ_7_combine, text_responses) - 1
budd$phq9_8 <- match(budd$PHQ_8_combine, text_responses) - 1
budd$phq9_9 <- match(budd$PHQ_9_combine, text_responses) - 1

# Renommer les items
budd_phq9 <- budd[paste0("phq9_", 1:9)]

# Voir les données
head(budd_phq9) # Ou : View(budd_phq9)

# Les analyses en utilisant la TRI ----------------------------------------
# IRT analyses ------------------------------------------------------------

# Créer un modèle avec un seul facteur
# Calculate a 1F model using mirt
phq9_1F_model <- 'F = 1-9'

# Faire les analyses
# Ajouter SE = TRUE pour « information matrix »
phq9_1F_mirt <- mirt(budd_phq9,
                     phq9_1F_model,
                     itemtype = "graded", # Pour les réponses ordonnées
                     SE = TRUE,
                     technical = list(removeEmptyRows = TRUE,
                                      NCYCLES = 5000,
                                      theta_lim = c(-4, 4)))

# Voir des résultats
# Ici, vous trouverez les paramètres a et b
coef(phq9_1F_mirt,
     IRTpars = TRUE,
     simplify = TRUE)

phq9_fscores <- fscores(phq9_1F_mirt) # Calculer les scores

# Voir un graphique de scores
plot(density(phq9_fscores))

# Enregistrer les probabilités en fonction du thêta
phq9_probtrace <- probtrace(phq9_1F_mirt,
                            c(seq(-3, 3, by = 0.05))) %>% 
  as.matrix(ncol = 36) %>% cbind(c(seq(-3, 3, by = 0.05)), .)

colnames(phq9_probtrace)[1] <- "theta"

# Voir le dataframe pour les probabilités 
head(phq9_probtrace) # Ou, utiliser View(phq9_probtrace)

# Les réponses
# 0 = Jamais
# 1 = Plusieurs jours
# 2 = Plus de la moitié des jours
# 3 = Presque tous les jours

# Voir les noms dans le dataframe
colnames(phq9_probtrace)

# Choisir l'item numéro 2
phq9_2_probtrace <- as.data.frame(phq9_probtrace[, c(1, 6:9)])
names(phq9_2_probtrace)[2:5] <- paste0("p.", 1:4)

# Voir le dataframe 
head(phq9_2_probtrace)

# Notez bien :
# p.1 = Jamais
# p.2 = Plusieurs jours
# p.3 = Plus de la moitié des jours
# p.4 = Presque tous les jours


# CCI ---------------------------------------------------------------------
# Une courbe caractéristique d’item ;	« Item Charachteristic Curve »

# En utilisant mirt
itemplot(phq9_1F_mirt,
         2,
         theta_lim = c(-3, 3),
         main = "à quelle fréquence...Être triste, déprimé(e) ou désespéré(e)")

# En utilisant ggplot2
icc_phq9_2 <- ggplot(phq9_2_probtrace, aes(x = theta)) +
  geom_line(aes(y = p.4, colour = "p.4")) +
  scale_colour_manual("",
                      breaks = c("p.1", "p.2", "p.3", "p.4"),
                      values = c("p.1" = "black",
                                 "p.2" = "orange",
                                 "p.3" = "red", 
                                 "p.4" = "dark blue"),
                      labels = c("0: Jamais",
                                 "1: Plusieurs jours",
                                 "2: Plus de la moitié des jours",
                                 "3: Presque tous les jours")) +
  ylab(label = "P") +
  xlab(label = "La dépression (thêta)") +
  ggtitle("Une courbe caractéristique d’item") +
  labs(subtitle = "à quelle fréquence...2. Être triste, déprimé(e) ou désespéré(e)") +
  theme(axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 22),
        plot.subtitle = element_text(size = 18),
        legend.text = element_text(size = 16))
icc_phq9_2

icc_phq9_2 + geom_vline(xintercept = 1.462,
                        colour = "red")

icc_phq9_2 + geom_line(aes(y = p.3, colour = "p.3"))

icc_phq9_2 + geom_line(aes(y = p.3, colour = "p.3")) +
  geom_line(aes(y = p.2, colour = "p.2"))

icc_phq9_2 + geom_line(aes(y = p.3, colour = "p.3")) +
  geom_line(aes(y = p.2, colour = "p.2")) +
  geom_line(aes(y = p.1, colour = "p.1"))

# Ajouter les lignes pours les "b"
icc_phq9_2 + geom_line(aes(y = p.3, colour = "p.3")) +
  geom_line(aes(y = p.2, colour = "p.2")) +
  geom_line(aes(y = p.1, colour = "p.1")) +
  geom_vline(xintercept = -0.133,
             colour = "red") +
  geom_vline(xintercept = 0.760,
             colour = "red") +
  geom_vline(xintercept = 1.462,
             colour = "red")


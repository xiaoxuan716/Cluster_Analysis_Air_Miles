

# Préparation des fichiers ---------------------------------------------------
# Préparation préalable des fichiers pour des modifications 

# Mise en place générale ----------------------------------------------
# Téléchargement des fichiers, transformation préalable des variables

# setwd pour mettre le dossier où les jeux de données sont
setwd("")

# librairies utilisées
# pour en installer de nouvelles : install.packages("readr")

library(readr) # pour télécharger les fichier, ça va plus vite
library(dplyr)
library(ggplot2)
library(forcats)
library(pander)
library(pastecs)
library(stringr)
library(tidyr)
library(lubridate)
library(purrr)


mem_d <- as.data.frame(read_csv("MEMBERS_DIM.csv", progress = TRUE))
liste.epicerie <- as.data.frame(read_csv("liste_epicerie.csv", progress = TRUE))
pt_f <- as.data.frame(read_csv("REWARD_FACT.csv", progress = TRUE))
tr_f <- as.data.frame(read_csv("TRANSACTION_FACT.csv", progress = TRUE))

# changé pour les données en sorties (au cas)
setwd("")


# Transformation préalable
liste.epicerie  <- liste.epicerie %>%
  mutate(MEMBER_ID = as.numeric(MEMBER_ID))

mem_d <- mem_d %>%
  mutate(
    MEMBER_ID = as.factor(MEMBER_ID),
    GENDER = as.factor(GENDER),
    LANGUAGE = as.factor(LANGUAGE),
    TIER = as.factor(TIER),
    SMALL_BUSINESS_FLAG = as.factor(SMALL_BUSINESS_FLAG),
    EMAILABLE_FLAG = as.factor(EMAILABLE_FLAG),
    MAILABLE_FLAG = as.factor(MAILABLE_FLAG)
  )

pt_f <- pt_f %>%
  mutate(
    MEMBER_ID = as.factor(MEMBER_ID),
    REWARDS_CATEGORY = as.factor(REWARDS_CATEGORY)
  )

tr_f <- tr_f %>%
  mutate(
    MEMBER_ID = as.factor(MEMBER_ID),
    RETAILER = as.factor(RETAILER)
  )


# Liste de membres épicerie ------------------------------------
# Créer une liste des membres épiceries à partir des transactions (identification des membres qui ont déjà achetés en épicerie)
liste.epicerie <- tr_f %>%
  select(MEMBER_ID, RETAILER) %>%
  filter(RETAILER =="GROCERY") %>%
  select(MEMBER_ID) %>%
  distinct()
write.csv(liste.epicerie, "liste_epicerie")


tr_f %>%
  select(MEMBER_ID, RETAILER)



# Dimension membres epicerie ----------------------------------------------

membres.epicerie <- membres %>%
  inner_join(liste.epicerie, by = "MEMBER_ID") %>%
  select(-X, -X1)
head(membres.epicerie)


# Nettoyage de la dimension membres ----------------------------------------------------
# enlever la variable TIER
membres.epicerie <- membres.epicerie %>%
  select(-TIER)

# enlever les ? pour la variable SMALL_BUSINESS_FLAG
membres.epicerie <- membres.epicerie %>%
  mutate(SMALL_BUSINESS_FLAG = as.factor(ifelse(as.character(SMALL_BUSINESS_FLAG) == "?", NA, as.character(SMALL_BUSINESS_FLAG))))


# Changer les EMAILABLE_FLAG NA en NC (non-configurés)
membres.epicerie <- membres.epicerie %>%
  mutate(EMAILABLE_FLAG = as.factor(replace_na(as.character(EMAILABLE_FLAG), "NC")))

# ajouter un âge crédible
membres.epicerie$annee.mem <- ymd("2019-12-31") %m-% months(membres.epicerie$TENURE_MONTHS)
membres.epicerie

membres.epicerie$nb.mem <- interval(membres.epicerie$annee.mem, ymd("2019-12-31"))

membres.epicerie <- membres.epicerie %>%
  mutate(nb.mem = time_length(nb.mem, unit = "year"))

membres.epicerie$nb.mem <- round(as.numeric(membres.epicerie$nb.mem), 0)

membres.epicerie <- membres.epicerie %>%
  mutate(nb.mem = nb.mem * -1)

membres.epicerie <- membres.epicerie %>%
  mutate(age_adhesion = rowSums(membres.epicerie[, c("AGE", "nb.mem")], na.rm = TRUE))

membres.epicerie <- membres.epicerie %>%
  mutate(nb.mem = abs(nb.mem))

membres.epicerie$AGE <- replace(membres.epicerie$AGE, membres.epicerie$age_adhesion < 16, NA)

membres.epicerie$AGE <- replace(membres.epicerie$AGE, membres.epicerie$AGE > 100, NA)

# garder les variables sociodémographiques d'intérêt
membres.epicerie <- membres.epicerie %>%
  select(-age_adhesion, -nb.mem, -annee.mem)
summary(membres.epicerie)
summarytools::dfSummary(membres.epicerie)


# Imputation des valeurs manquantes

library("missRanger")

set.seed(123456)
membres.epicerie <-
  missRanger(
    membres.epicerie,
    formula = AGE +
      GENDER +
      LANGUAGE ~
      AGE +
      GENDER +
      LANGUAGE +
      TENURE_MONTHS +
      EMAILABLE_FLAG +
      MAILABLE_FLAG
    ,
    num.trees = 100
  )

# arrondir les valeurs imput?es pour qu'elles soient en concordance avec le fichier d'origine
membres.epicerie <- membres.epicerie %>%
  mutate(AGE = round(AGE, 0))
#write.csv(membres.epicerie, "membres_epicerie.csv")



# Fait transactions -----------------------------------------------------

tr.epiceries <- tr_f %>%
  inner_join(liste.epicerie, by = "MEMBER_ID")

library(forcats)
tr.epiceries$RETAILER <- as.character(tr.epiceries$RETAILER)
tr.epiceries$RETAILER <-
  as.factor(ifelse(
    tr.epiceries$RETAILER == "SPECIALTY RETAIL",
    "RETAIL",
    ifelse(tr.epiceries$RETAILER == "OTHER RETAIL", "RETAIL",
           tr.epiceries$RETAILER)
  ))
tr.epiceries <- tr.epiceries %>%
  mutate(RETAILER = fct_lump(RETAILER,
                             p = 0.05))
tr.epiceries <- tr.epiceries %>%
  mutate(RETAILER = fct_explicit_na(RETAILER, "Other"))

#write.csv(tr.epiceries, "tr_epiceries.csv")

# Fait retraits -----------------------------------------------------------

pt.epiceries <- pt_f %>%
  inner_join(liste.epicerie, by = "MEMBER_ID")

#write.csv(pt.epiceries, "pt_epiceries.csv")
















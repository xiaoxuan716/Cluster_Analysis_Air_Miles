

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

setwd(" ")


clusters_tous <- as.data.frame(read_csv("clusters_membres.csv", progress = TRUE))
clusters_rewards <- as.data.frame(read_csv("membres_rewards.csv", progress = TRUE))
sociodemo <- as.data.frame(read_csv("membres_epicerie.csv", progress = TRUE)) 

clusters1 <- clusters_tous[clusters_tous$clusters.3==1,]
clusters2 <- clusters_tous[clusters_tous$clusters.3==2,]
clusters3 <- clusters_tous[clusters_tous$clusters.3==3,]

summarytools::dfSummary(clusters1)
summarytools::dfSummary(clusters2)
summarytools::dfSummary(clusters3)
summarytools::dfSummary(clusters_rewards)





clusters1 %>%
  inner_join(sociodemo, by = c("id" = "MEMBER_ID")) %>%
  summarytools::dfSummary()

clusters2 %>%
  inner_join(sociodemo, by = c("id" = "MEMBER_ID")) %>%
  summarytools::dfSummary()

clusters3 %>%
  inner_join(sociodemo, by = c("id" = "MEMBER_ID")) %>%
  summarytools::dfSummary()

clusters_rewards %>%
  inner_join(sociodemo, by = c("id" = "MEMBER_ID")) %>%
  summarytools::dfSummary()

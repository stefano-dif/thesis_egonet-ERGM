source("data_processing.R")

check_KL_deg <- function(x, transform = TRUE){
  if (transform){
    x <- as_tibble(x$summary.deg)
    x <- rbind(x$obs, x$mean)
  }
  return(KL(x, unit = 'log'))
}

check_KL_esp <- function(x, transform = TRUE){
  if (transform){
    x <- as_tibble(x$summary.espart)
    x <- rbind(x$obs/sum(x$obs), x$mean/sum(x$mean))
  }
  return(KL(x, unit = 'log'))
}

# -----

library(dplyr)
library(ggplot2)
library(patchwork)
library(readr)
library(sna)
library(ergm)
library(ergm.ego)
library(intergraph)
library(igraph)
library(parallel)
cat("core numbers:", detectCores())
select <- dplyr::select

par(mar=c(1,1,1,1))


# ----

survey_DE <- data_preprocessing(survey = survey,
                                section = "DE",
                                seed = 1234)

survey_FR <- data_preprocessing(survey = survey,
                                section = "FR",
                                seed = 1234)

survey_IT <- data_preprocessing(survey = survey,
                                section = "IT",
                                seed = 1234)

survey_UK <- data_preprocessing(survey = survey,
                                section = "UK",
                                seed = 1234)

# ----

egor_DE_cow <- construct_egor_cow(survey_DE,
                                  seed = 1234)

egor_FR_cow <- construct_egor_cow(survey_FR,
                                  seed = 1234)

egor_IT_cow <- construct_egor_cow(survey_IT,
                                  seed = 1234)

egor_UK_cow <- construct_egor_cow(survey_UK,
                                  seed = 1234)

# ----

egor_DE_fr <- construct_egor_fr(survey_DE,
                                seed = 1234)

egor_FR_fr <- construct_egor_fr(survey_FR,
                                seed = 1234)

egor_IT_fr <- construct_egor_fr(survey_IT,
                                seed = 1234)

egor_UK_fr <- construct_egor_fr(survey_UK,
                                seed = 1234)

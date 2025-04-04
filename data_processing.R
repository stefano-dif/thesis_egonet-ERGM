library(dplyr)
library(readr)
library(sna)
library(ergm)
library(ergm.ego)
library(intergraph)
library(fitdistrplus)
select <- dplyr::select

par(mar=c(1,1,1,1))

# -----

survey <- read_csv("data_sample.csv")

survey %>% 
  select(-c(1, "RecordedDate")) -> survey

colSums(is.na(survey))

survey[is.na(survey$cohabitant_num), ]$cohabitant_num <- 0
survey[is.na(survey$noncohabitant_num), ]$noncohabitant_num <- 0

survey %>% 
  filter(size_classmate_group <= 27 | is.na(size_classmate_group)) %>% 
  filter(size_coworker_group <= 27 | is.na(size_coworker_group)) %>% 
  filter(size_friend_group <= 27 | is.na(size_friend_group)) -> survey

table(survey$country)

# -----

data_preprocessing <- function(survey, section = "UK", seed = 1234){
  
  set.seed(1234)
  
  survey %>% filter(country == section) %>% select(-country,-age) %>% select(sex, age_group, everything()) -> survey_UK
  
  survey_UK[!is.na(survey_UK$size_classmate_group) & survey_UK$size_classmate_group > 0, ]$size_coworker_group <- 0
  survey_UK[!is.na(survey_UK$size_coworker_group) & survey_UK$size_coworker_group > 0, ]$size_classmate_group <- 0
  
  survey_UK[!is.na(survey_UK$pdf_work_contacts_0_17) & survey_UK$pdf_work_contacts_0_17 > 0, ]$pdf_class_contacts_0_17 <- 0
  survey_UK[!is.na(survey_UK$pdf_class_contacts_0_17) & survey_UK$pdf_class_contacts_0_17 > 0, ]$pdf_work_contacts_0_17 <- 0
  
  survey_UK[!is.na(survey_UK$pdf_work_contacts_18_29) & survey_UK$pdf_work_contacts_18_29 > 0, ]$pdf_class_contacts_18_29 <- 0
  survey_UK[!is.na(survey_UK$pdf_class_contacts_18_29) & survey_UK$pdf_class_contacts_18_29 > 0, ]$pdf_work_contacts_18_29 <- 0
  
  survey_UK[!is.na(survey_UK$pdf_work_contacts_30_59) & survey_UK$pdf_work_contacts_30_59 > 0, ]$pdf_class_contacts_30_59 <- 0
  survey_UK[!is.na(survey_UK$pdf_class_contacts_30_59) & survey_UK$pdf_class_contacts_30_59 > 0, ]$pdf_work_contacts_30_59 <- 0
  
  survey_UK[!is.na(survey_UK$`pdf_work_contacts_60_+`) & survey_UK$`pdf_work_contacts_60_+` > 0, ]$`pdf_class_contacts_60_+` <- 0
  survey_UK[!is.na(survey_UK$`pdf_class_contacts_60_+`) & survey_UK$`pdf_class_contacts_60_+` > 0, ]$`pdf_work_contacts_60_+` <- 0
  
  survey_UK %>% 
    filter(!is.na(size_classmate_group) &
             !is.na(size_coworker_group) & !is.na(size_friend_group)) -> survey_UK
  
  survey_UK[is.na(survey_UK$coworker_closure), ]$coworker_closure <- "None of them"
  survey_UK[is.na(survey_UK$friend_closure), ]$friend_closure <- "None of them"
  
  
  survey_UK[is.na(survey_UK$M_politics), "M_politics"] <- "I prefer not to answer"
  
  
  survey_UK[is.na(survey_UK$pdf_work_contacts_0_17), ]$pdf_work_contacts_0_17 <- mean(survey_UK$pdf_work_contacts_0_17, na.rm = T)
  
  survey_UK[is.na(survey_UK$pdf_work_contacts_18_29), ]$pdf_work_contacts_18_29 <- mean(survey_UK$pdf_work_contacts_18_29, na.rm = T)
  
  survey_UK[is.na(survey_UK$pdf_work_contacts_30_59), ]$pdf_work_contacts_30_59 <- mean(survey_UK$pdf_work_contacts_30_59, na.rm = T)
  
  survey_UK[is.na(survey_UK$`pdf_work_contacts_60_+`), ]$`pdf_work_contacts_60_+` <- mean(survey_UK$`pdf_work_contacts_60_+`, na.rm = T)
  
  
  survey_UK[is.na(survey_UK$pdf_class_contacts_0_17), ]$pdf_class_contacts_0_17 <- 0
  
  survey_UK[is.na(survey_UK$pdf_class_contacts_18_29), ]$pdf_class_contacts_18_29 <- 0
  
  survey_UK[is.na(survey_UK$pdf_class_contacts_30_59), ]$pdf_class_contacts_30_59 <- 0
  
  survey_UK[is.na(survey_UK$`pdf_class_contacts_60_+`), ]$`pdf_class_contacts_60_+` <- 0
  
  
  survey_UK[is.na(survey_UK$pdf_friend_contacts_0_17), ]$pdf_friend_contacts_0_17 <- mean(survey_UK$pdf_friend_contacts_0_17, na.rm = T)
  
  survey_UK[is.na(survey_UK$pdf_friend_contacts_18_29), ]$pdf_friend_contacts_18_29 <- mean(survey_UK$pdf_friend_contacts_18_29, na.rm = T)
  
  survey_UK[is.na(survey_UK$pdf_friend_contacts_30_59), ]$pdf_friend_contacts_30_59 <- mean(survey_UK$pdf_friend_contacts_30_59, na.rm = T)
  
  survey_UK[is.na(survey_UK$`pdf_friend_contacts_60_+`), ]$`pdf_friend_contacts_60_+` <- mean(survey_UK$`pdf_friend_contacts_60_+`, na.rm = T)
  
  
  survey_UK <- survey_UK %>% 
    mutate(size_coworker_group = size_coworker_group + size_classmate_group,
           pdf_work_contacts_0_17 = pdf_work_contacts_0_17 + pdf_class_contacts_0_17,
           pdf_work_contacts_18_29 = pdf_work_contacts_18_29 + pdf_class_contacts_18_29,
           pdf_work_contacts_30_59 = pdf_work_contacts_30_59 + pdf_class_contacts_30_59,
           `pdf_work_contacts_60_+` = `pdf_work_contacts_60_+` + `pdf_class_contacts_60_+`)
  
  trans <- c("None of them" = 0, "Very few of them" = 0.1, "Some of them" = 0.25,
             "About a half of them" = 0.5, "Most of them" = 0.75,
             "Almost all of them" = 0.9, "All of them" = 1)
  
  survey_UK$coworker_closure <- trans[survey_UK$coworker_closure]
  survey_UK$friend_closure <- trans[survey_UK$friend_closure]
  
  reass <- c(0, 1, 2, rep(5, 5), rep(10, 5), rep(15, 5), rep(20, 5), rep(25, 5))
  names(reass) <- as.character(0:27)
  
  survey_UK$size_coworker_group <- reass[as.character(survey_UK$size_coworker_group)]
  survey_UK$size_friend_group <- reass[as.character(survey_UK$size_friend_group)]
  names(survey_UK$size_coworker_group) <- NULL
  names(survey_UK$size_friend_group) <- NULL
  
  survey_UK %>% select(size_coworker_group) %>% table() -> temp
  
  temp <- c(temp[1], temp[2], rep(temp[3]/5, 5), rep(temp[4]/5, 5), rep(temp[5]/5, 5), rep(temp[6]/5, 5), rep(temp[7]/5, 5))
  names(temp) <- as.character(1:27)
  
  temp2 <- sample(as.numeric(names(temp)), size = nrow(survey_UK), replace = T, prob = temp)
  
  fit1 <- fitdist(temp2, distr = "gamma")
  plot(fit1)
  
  #Now we reassign to the values of the gamma distribution
  
  prob_cow_5 <- dgamma(3:7, shape = fit1$estimate[1], rate = fit1$estimate[2])
  
  survey_UK[survey_UK$size_coworker_group == 5, ]$size_coworker_group <- sample(3:7,
                                                                                size = length(survey_UK[survey_UK$size_coworker_group == 5, ]$size_coworker_group),
                                                                                replace = T,
                                                                                prob = prob_cow_5
  )
  
  prob_cow_10 <- dgamma(8:12, shape = fit1$estimate[1], rate = fit1$estimate[2])
  
  survey_UK[survey_UK$size_coworker_group == 10, ]$size_coworker_group <- sample(8:12,
                                                                                 size = length(survey_UK[survey_UK$size_coworker_group == 10, ]$size_coworker_group),
                                                                                 replace = T,
                                                                                 prob = prob_cow_10
  )
  
  prob_cow_15 <- dgamma(13:17, shape = fit1$estimate[1], rate = fit1$estimate[2])
  
  survey_UK[survey_UK$size_coworker_group == 15, ]$size_coworker_group <- sample(13:17,
                                                                                 size = length(survey_UK[survey_UK$size_coworker_group == 15, ]$size_coworker_group),
                                                                                 replace = T,
                                                                                 prob = prob_cow_15
  )
  
  prob_cow_20 <- dgamma(18:22, shape = fit1$estimate[1], rate = fit1$estimate[2])
  
  survey_UK[survey_UK$size_coworker_group == 20, ]$size_coworker_group <- sample(18:22,
                                                                                 size = length(survey_UK[survey_UK$size_coworker_group == 20, ]$size_coworker_group),
                                                                                 replace = T,
                                                                                 prob = prob_cow_20
  )
  
  prob_cow_25 <- dgamma(23:27, shape = fit1$estimate[1], rate = fit1$estimate[2])
  
  survey_UK[survey_UK$size_coworker_group == 25, ]$size_coworker_group <- sample(23:27,
                                                                                 size = length(survey_UK[survey_UK$size_coworker_group == 25, ]$size_coworker_group),
                                                                                 replace = T,
                                                                                 prob = prob_cow_25
  )
  
  survey_UK %>% select(size_friend_group) %>% table() -> temp
  
  temp <- c(temp[1], temp[2], temp[3], rep(temp[4]/5, 5), rep(temp[5]/5, 5), rep(temp[6]/5, 5), rep(temp[7]/5, 5), rep(temp[8]/5, 5))
  names(temp) <- as.character(0:27)
  
  temp2 <- sample(as.numeric(names(temp)), size = nrow(survey_UK), replace = T, prob = temp)
  
  
  fit1 <- fitdist(temp2, distr = "gamma")
  plot(fit1)
  
  #Now we reassign to the values of the gamma distribution
  
  prob_friends_5 <- dgamma(3:7, shape = fit1$estimate[1], rate = fit1$estimate[2])
  
  survey_UK[survey_UK$size_friend_group == 5, ]$size_friend_group <- sample(3:7,
                                                                            size = length(survey_UK[survey_UK$size_friend_group == 5, ]$size_friend_group),
                                                                            replace = T,
                                                                            prob = prob_friends_5
  )
  
  prob_friends_10 <- dgamma(8:12, shape = fit1$estimate[1], rate = fit1$estimate[2])
  
  survey_UK[survey_UK$size_friend_group == 10, ]$size_friend_group <- sample(8:12,
                                                                             size = length(survey_UK[survey_UK$size_friend_group == 10, ]$size_friend_group),
                                                                             replace = T,
                                                                             prob = prob_friends_10
  )
  
  prob_friends_15 <- dgamma(13:17, shape = fit1$estimate[1], rate = fit1$estimate[2])
  
  survey_UK[survey_UK$size_friend_group == 15, ]$size_friend_group <- sample(13:17,
                                                                             size = length(survey_UK[survey_UK$size_friend_group == 15, ]$size_friend_group),
                                                                             replace = T,
                                                                             prob = prob_friends_15
  )
  
  prob_friends_20 <- dgamma(18:22, shape = fit1$estimate[1], rate = fit1$estimate[2])
  
  survey_UK[survey_UK$size_friend_group == 20, ]$size_friend_group <- sample(18:22,
                                                                             size = length(survey_UK[survey_UK$size_friend_group == 20, ]$size_friend_group),
                                                                             replace = T,
                                                                             prob = prob_friends_20
  )
  
  prob_friends_25 <- dgamma(23:27, shape = fit1$estimate[1], rate = fit1$estimate[2])
  
  survey_UK[survey_UK$size_friend_group == 25, ]$size_friend_group <- sample(23:27,
                                                                             size = length(survey_UK[survey_UK$size_friend_group == 25, ]$size_friend_group),
                                                                             replace = T,
                                                                             prob = prob_friends_25
  )
  
  survey_UK$size_coworker_group %>% hist()
  survey_UK$size_friend_group %>% hist()
  
  return(survey_UK)
}



# -------
# -------


construct_egor_cow <- function(survey_UK, seed = 1234){
  
  set.seed(1234)
  
  egos_uk <- survey_UK %>% 
    select(1:5, cohabitant_num, noncohabitant_num, adult_cohabitant_num,
           adult_noncohabitant_num) %>% 
    mutate(.EGOID = 1:nrow(survey_UK)) %>% 
    select(.EGOID, everything())
  
  temp <- survey_UK %>% 
    mutate(.EGOID = egos_uk$.EGOID) %>% 
    select(.EGOID, size_coworker_group, starts_with("pdf_work"))
  
  alters_uk_cow <- tibble()
  
  for (egoid in 1:nrow(temp)){
    ego_tibble <- tibble(.ALTID = 0, .EGOID = egoid, age_group = "",
                         .rows = temp$size_coworker_group[egoid])
    k <- temp$size_coworker_group[temp$.EGOID == egoid]
    if (k > 0){
      for (j in 1:k){
        ego_tibble[j, ".ALTID"] <- j
        ego_tibble[j, "age_group"] <- sample(c("0-17", "18-29", "30-59", "60+"), size = 1,
                                             prob = c(temp$pdf_work_contacts_0_17[temp$.EGOID == egoid],
                                                      temp$pdf_work_contacts_18_29[temp$.EGOID == egoid],
                                                      temp$pdf_work_contacts_30_59[temp$.EGOID == egoid],
                                                      temp$`pdf_work_contacts_60_+`[temp$.EGOID == egoid]))
      }
    }
    
    alters_uk_cow <- rbind(alters_uk_cow, ego_tibble)
  }
  
  aaties_uk_cow <- tibble()
  
  for (egoid in 1:nrow(survey_UK)){
    #compute number of edges
    aa_cow <- survey_UK$coworker_closure[egoid] * survey_UK$size_coworker_group[egoid]*(survey_UK$size_coworker_group[egoid]-1)/2
    
    #place edges at random
    temp <- alters_uk_cow %>% 
      filter(.EGOID == egoid) %>% 
      .[[".ALTID"]]
    
    if (length(temp) < 2){
      next
    }
    
    edges <- sample(combn(temp, m = 2, replace = F, simplify = F), size = aa_cow)
    
    ego_tibble <- tibble(.EGOID = egoid, .SRCID = 0, .TGTID = 0,
                         .rows = length(edges))
    
    if (length(edges) > 0){
      for (j in 1:length(edges)){
        ego_tibble[j, ".SRCID"] <- edges[[j]][1]
        ego_tibble[j, ".TGTID"] <- edges[[j]][2]
      }
    }
    
    aaties_uk_cow <- rbind(aaties_uk_cow, ego_tibble)
  }
  
  aaties_uk_cow <- aaties_uk_cow %>% inner_join(alters_uk_cow, 
                                                join_by(.EGOID == .EGOID,
                                                        .SRCID == .ALTID)) %>% inner_join(alters_uk_cow, 
                                                                                          join_by(.EGOID == .EGOID,
                                                                                                  .TGTID == .ALTID)) %>% 
    filter(age_group.x != "0-17" & age_group.y != "0-17") %>% select(1:3)
  
  
  alters_uk_cow <- alters_uk_cow %>% filter(age_group != "0-17")
  
  
  ego_cow <- egor(alters = alters_uk_cow, 
                  egos = egos_uk, 
                  aaties = aaties_uk_cow,
                  ID.vars = list(ego = ".EGOID",
                                 alter = ".ALTID",
                                 source = ".SRCID",
                                 target = ".TGTID"),
                  alter_design = list(max = 27))
  
  return(ego_cow)
}

# ----
# ----

construct_egor_fr <- function(survey_UK, seed = 1234){
  
  set.seed(1234)
  
  egos_uk_2 <- survey_UK %>% 
    select(1:5, cohabitant_num, noncohabitant_num, adult_cohabitant_num,
           adult_noncohabitant_num, size_coworker_group, coworker_closure) %>% 
    mutate(.EGOID = 1:nrow(survey_UK)) %>% 
    select(.EGOID, everything())
  
  temp <- survey_UK %>% 
    mutate(.EGOID = egos_uk_2$.EGOID) %>% 
    select(.EGOID, size_friend_group, starts_with("pdf_friend"))
  
  alters_uk_fr <- tibble()
  
  for (egoid in 1:nrow(temp)){
    ego_tibble <- tibble(.ALTID = 0, .EGOID = egoid, age_group = "", 
                         .rows = temp$size_friend_group[egoid])
    k <- temp$size_friend_group[temp$.EGOID == egoid]
    if (k > 0){
      for (j in 1:k){
        ego_tibble[j, ".ALTID"] <- j
        ego_tibble[j, "age_group"] <- sample(c("0-17", "18-29", "30-59", "60+"), size = 1,
                                             prob = c(temp$pdf_friend_contacts_0_17[temp$.EGOID == egoid],
                                                      temp$pdf_friend_contacts_18_29[temp$.EGOID == egoid],
                                                      temp$pdf_friend_contacts_30_59[temp$.EGOID == egoid],
                                                      temp$`pdf_friend_contacts_60_+`[temp$.EGOID == egoid]))
      }
    }
    
    alters_uk_fr <- rbind(alters_uk_fr, ego_tibble)
  }
  
  
  aaties_uk_fr <- tibble()
  
  for (egoid in 1:nrow(survey_UK)){
    #compute number of edges
    aa_fr <- survey_UK$friend_closure[egoid] * survey_UK$size_friend_group[egoid]*(survey_UK$size_friend_group[egoid]-1)/2
    
    #place edges at random
    temp <- alters_uk_fr %>% 
      filter(.EGOID == egoid) %>% 
      .[[".ALTID"]]
    
    if (length(temp) < 2){
      next
    }
    
    edges <- sample(combn(temp, m = 2, replace = F, simplify = F), size = aa_fr)
    
    ego_tibble <- tibble(.EGOID = egoid, .SRCID = 0, .TGTID = 0,
                         .rows = length(edges))
    
    if (length(edges) > 0){
      for (j in 1:length(edges)){
        ego_tibble[j, ".SRCID"] <- edges[[j]][1]
        ego_tibble[j, ".TGTID"] <- edges[[j]][2]
      }
    }
    
    aaties_uk_fr <- rbind(aaties_uk_fr, ego_tibble)
  }
  
  aaties_uk_fr <- aaties_uk_fr %>% inner_join(alters_uk_fr, 
                                              join_by(.EGOID == .EGOID,
                                                      .SRCID == .ALTID)) %>% inner_join(alters_uk_fr, 
                                                                                        join_by(.EGOID == .EGOID,
                                                                                                .TGTID == .ALTID)) %>% 
    filter(age_group.x != "0-17" & age_group.y != "0-17") %>% select(1:3)
  
  
  alters_uk_fr <- alters_uk_fr %>% filter(age_group != "0-17")
  
  
  ego_fr <- egor(alters = alters_uk_fr, 
                 egos = egos_uk_2, 
                 aaties = aaties_uk_fr,
                 ID.vars = list(ego = ".EGOID",
                                alter = ".ALTID",
                                source = ".SRCID",
                                target = ".TGTID"))
  
  return(ego_fr)
}

# ----
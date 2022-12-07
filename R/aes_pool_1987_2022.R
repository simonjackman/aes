########################################################
## pool 1987-2022 AES
##
## simon jackman
## simon.jackman@sydney.edu.au
## ussc, univ of sydney
## 2021-07-19 14:50:16
## 2022-10-10 18:35:50
########################################################
library(tidyverse)
library(here)
library(foreign)
library(ussc)
library(mgcv)
library(ggplot2)
library(ggrepel)

## leader attributes
leader_attributes <- tribble(
  ~abb, ~label,
  "mor", "moral",
  "int", "intelligent",
  "com", "compassionate",
  "sen", "sensible",
  "lea", "strong_leader",
  "dec", "decent",
  "rel", "reliable",
  "kno", "knowledgable",
  "ins", "inspiring",
  "dep", "dependable",
  "hon", "honest",
  "arr", "arrogant",
  "tru", "trustworthy",
  "cmps", "compassionate",
  "cmpt", "competent",
  "cmp", "compassionate",
  "1",   "intelligent",
  "2",   "compassionate",
  "3",   "competent",
  "4",   "sensible",
  "5",   "strong_leader",
  "6",   "honest",
  "7",   "knowledgeable",
  "8",   "inspiring",
  "9",   "trustworthy",
  "a_qa",   "intelligent",
  "b_qa",   "compassionate",
  "c_qa",   "competent",
  "d_qa",   "sensible",
  "e_qa",   "strong_leader",
  "f_qa",   "honest",
  "g_qa",   "knowledgeable",
  "h_qa",   "inspiring",
  "i_qa",   "trustworthy"
)

renameTraits <- function(oldName) {
  s <- str_split(oldName,pattern = "_",n=2)
  prefix <- unlist(lapply(s,function(x)x[[1]]))
  suffix <- unlist(lapply(s,function(x)x[[2]]))
  print(prefix)
  print(suffix)
  newName <- leader_attributes %>%
    semi_join(tibble(abb = suffix),by = "abb") %>% 
    distinct() %>% 
    pull(label)
  print(newName)
  
  out <- paste(prefix, newName, sep = "_")
  
  cat(paste("newname:",out,"\n"))
  
  return(out)
}

## 1987
library(foreign)
d1987 <- read.spss(here("../1987/aes87.sav")) %>%
  as_tibble() %>%
  select(
    hvote = Q5REPS,
    pm_like = Q19BOBH,
    opp_like = Q19JOHNH,
    #starts_with("C4PK"),
    #starts_with("C4JH"),
    ##trust_in_government = C5,
    #anzus_item = G7,
    #trust_item = G6,
    birth_year = Q74) %>% 
  mutate(weight = 1) %>% 
  mutate(
    pm_like = match(pm_like,levels(pm_like)) - 1,
    pm_like = if_else(pm_like == max(pm_like), NA_real_, pm_like),
    opp_like = match(opp_like,levels(opp_like)) - 1,
    opp_like = if_else(opp_like == max(opp_like), NA_real_, opp_like)
  ) %>% 
  ##rename_with(.cols = starts_with("pm_")  & !ends_with("like"), .fn = ~ renameTraits(.x)) %>% 
  ##rename_with(.cols = starts_with("opp_") & !ends_with("like"), .fn = ~ renameTraits(.x)) %>% 
  mutate(birth_year = ifelse(birth_year == -1, NA, as.numeric(as.character(birth_year)) +
                               1900))

## 1990
library(foreign)
d1990 <- read.spss(here("../1990/aes90.sav")) %>%
  as_tibble() %>%
  select(
    hvote = B6REPS,
    pm_like = C1BH,
    opp_like = C1AP,
    #starts_with("C4PK"),
    #starts_with("C4JH"),
    ##trust_in_government = C5,
    #anzus_item = G7,
    #trust_item = G6,
    birth_year = J2) %>% 
  mutate(weight = 1) %>% 
  mutate(
    pm_like = match(pm_like,levels(pm_like))-1,
    pm_like = if_else(pm_like == max(pm_like), NA_real_, pm_like),
    opp_like = match(opp_like,levels(opp_like))-1,
    opp_like = if_else(opp_like == max(opp_like), NA_real_, opp_like)
    ) %>% 
  ##rename_with(.cols = starts_with("pm_")  & !ends_with("like"), .fn = ~ renameTraits(.x)) %>% 
  ##rename_with(.cols = starts_with("opp_") & !ends_with("like"), .fn = ~ renameTraits(.x)) %>% 
  mutate(birth_year = ifelse(birth_year == -1, NA, as.numeric(as.character(birth_year)) +
                               1900))

## 1993
library(foreign)
d1993 <- read.spss(here("../1993/aes93.sav")) %>%
  as_tibble() %>%
  select(
    hvote = B6REPS,
    pm_like = C1PK,
    opp_like = C1JHEW,
    starts_with("C4PK"),
    starts_with("C4JH"),
    ##trust_in_government = C5,
    anzus_item = G7,
    trust_item = G6,
    birth_year = J2,
    weight = WEIGHT
  ) %>%
  rename_with(.cols = starts_with("C4PK"),
              .fn = ~ tolower(str_replace(
                .x, pattern = "C4PK", replacement = "pm_"
              ))) %>%
  rename_with(.cols = starts_with("C4JH"),
              .fn = ~ tolower(str_replace(
                .x, pattern = "C4JH", replacement = "opp_"
              ))) %>%
  mutate(
    pm_like = match(pm_like,levels(pm_like)) - 1,
    pm_like = if_else(pm_like == min(pm_like), NA_real_, pm_like),
    opp_like = match(opp_like,levels(opp_like)) - 1,
    opp_like = if_else(opp_like == min(opp_like), NA_real_, opp_like)
  ) %>% 
  rename_with(.cols = starts_with("pm_")  & !ends_with("like"), .fn = ~renameTraits(.x)) %>% 
  rename_with(.cols = starts_with("opp_") & !ends_with("like"), .fn = ~renameTraits(.x)) %>% 
  mutate(birth_year = ifelse(birth_year == -1, NA, as.numeric(as.character(birth_year)) +
                               1900))

## 1996
d1996 <- read.spss(here("../1996/aes96.sav")) %>%
  as_tibble() %>%
  select(hvote = B9REPS,
         pm_like = C1KEAT,
         opp_like = C1HOW,
         starts_with("C3PK"),
         starts_with("C3JH"),
         trust_in_government = C5,
         anzus_item = F4,
         trust_item = F5,
         birth_year = J2) %>%
  rename_with(
    .cols = starts_with("C3PK"),
    .fn = ~ tolower(str_replace(.x, pattern = "C3PK", replacement = "pm_"))
  ) %>%
  rename_with(
    .cols = starts_with("C3JH"),
    .fn = ~ tolower(str_replace(.x, pattern = "C3JH", replacement = "opp_"))
  ) %>% 
  mutate(
    pm_like = match(pm_like,levels(pm_like)) - 1,
    pm_like = if_else(pm_like == min(pm_like), NA_real_, pm_like),
    opp_like = match(opp_like,levels(opp_like)) - 1,
    opp_like = if_else(opp_like == min(opp_like), NA_real_, opp_like)
  ) %>% 
  rename_with(.cols = starts_with("pm_")  & !ends_with("like"), .fn = ~ renameTraits(.x)) %>% 
  rename_with(.cols = starts_with("opp_") & !ends_with("like"), .fn = ~ renameTraits(.x)) %>% 
  mutate(weight = 1,
         birth_year = ifelse(birth_year == "Blank",
                             NA,
                             as.numeric(as.character(birth_year)) + 1900)
         )

# 1998
d1998 <- read.spss(here("../1998/aes98.sav")) %>%
  as_tibble() %>%
  select(hvote = B11REPS,
         pm_like = C1HOW,
         opp_like = C1BEAZ,
         starts_with("C2JH"),
         starts_with("C3KB"),
         trust_in_government = C5,
         anzus_item = F1,
         trust_item = F2,
         birth_year = I2) %>%
  rename_with(
    .cols = starts_with("C2JH"),
    .fn = ~ tolower(str_replace(.x, pattern = "C2JH", replacement = "pm_"))
  ) %>%
  rename_with(
    .cols = starts_with("C3KB"),
    .fn = ~ tolower(str_replace(.x, pattern = "C3KB", replacement = "opp_"))
  ) %>% 
  mutate(
    pm_like = match(pm_like,levels(pm_like)) - 1,
    pm_like = if_else(pm_like == min(pm_like), NA_real_, pm_like),
    opp_like = match(opp_like,levels(opp_like)) - 1,
    opp_like = if_else(opp_like == min(opp_like), NA_real_, opp_like)
  ) %>% 
  rename_with(.cols = starts_with("pm_")  & !ends_with("like"), .fn = ~ renameTraits(.x)) %>% 
  rename_with(.cols = starts_with("opp_") & !ends_with("like"), .fn = ~ renameTraits(.x)) %>% 
  mutate(weight = 1,
         birth_year = ifelse(birth_year == "Missing",
                             NA,
                             as.numeric(as.character(birth_year)) + 1900)
         )

# 2001
d2001 <- read.spss(here("../2001/aes2001.sav")) %>%
  as_tibble() %>%
  select(
    hvote = B11REPS,
    pm_like = C1HOW,
    opp_like = C1BEAZ,
    starts_with("C2JH"),
    starts_with("C3KB"),
    satisfaction_with_democracy = C7,
    trust_in_government = C8,
    big_interests_government = C9,
    anzus_item = F1,
    trust_item = F2,
    birth_year = I2
  ) %>%
  rename_with(
    .cols = starts_with("C2JH"),
    .fn = ~ tolower(str_replace(.x, pattern = "C2JH", replacement = "pm_"))
  ) %>%
  rename_with(
    .cols = starts_with("C3KB"),
    .fn = ~ tolower(str_replace(.x, pattern = "C3KB", replacement = "opp_"))
  ) %>%
  mutate(
    pm_like = match(pm_like,levels(pm_like)) - 1,
    pm_like = if_else(pm_like == min(pm_like), NA_real_, pm_like),
    opp_like = match(opp_like,levels(opp_like)) - 1,
    opp_like = if_else(opp_like == min(opp_like), NA_real_, opp_like)
  ) %>% 
  rename_with(.cols = starts_with("pm_")  & !ends_with("like"), .fn = ~ renameTraits(.x)) %>% 
  rename_with(.cols = starts_with("opp_") & !ends_with("like"), .fn = ~ renameTraits(.x)) %>% 
  mutate(
    weight = 1,
    birth_year = ifelse(birth_year == "Missing", NA, as.numeric(as.character(birth_year)) +
                          1900)
  )

# 2004
d2004 <- read.spss(here("../2004/aes_2004_01079.sav")) %>% 
  as_tibble() %>%
  select(hvote = b11reps,
         pm_like = c1how,
         opp_like = c1lath,
         starts_with("c2jh"),
         starts_with("c3ml"),
         satisfaction_with_democracy = c7,
         trust_in_government = c8,
         big_interests_government = c9,
         anzus_item = f1,
         trust_item = f2,
         birth_year = i2) %>%
  rename_with(
    .cols = starts_with("c2jh"),
    .fn = ~ str_replace(.x, pattern = "c2jh", replacement = "pm_")
  ) %>%
  rename_with(
    .cols = starts_with("c3ml"),
    .fn = ~ str_replace(.x, pattern = "c3ml", replacement = "opp_")
  ) %>%
  mutate(
    pm_like = match(pm_like,levels(pm_like)) - 1,
    pm_like = if_else(pm_like == min(pm_like), NA_real_, pm_like),
    opp_like = match(opp_like,levels(opp_like)) - 1,
    opp_like = if_else(opp_like == min(opp_like), NA_real_, opp_like)
  ) %>% 
  rename_with(.cols = starts_with("pm_")  & !ends_with("like"), .fn = ~ renameTraits(.x)) %>% 
  rename_with(.cols = starts_with("opp_") & !ends_with("like"), .fn = ~ renameTraits(.x)) %>% 
  mutate(
    weight = 1,
    birth_year = ifelse(birth_year == "Missing", NA,
                        as.numeric(as.character(birth_year)) +
                          1900)
  )

# 2007
d2007 <- read.spss(here("../2007/aes_2007_01120.sav")) %>%
  as_tibble() %>%
  select(
    hvote = b11reps,
    pm_like = c1howard,
    opp_like = c1rudd,
    starts_with("c2jh"),
    starts_with("c3kr"),
    satisfaction_with_democracy = c7,
    trust_in_government = c8,
    big_interests_government = c9,
    anzus_item = f1,
    trust_item = f2,
    birth_year = h2
  ) %>%
  rename_with(
    .cols = starts_with("c2jh"),
    .fn = ~ str_replace(.x, pattern = "c2jh", replacement = "pm_")
  ) %>%
  rename_with(
    .cols = starts_with("c3kr"),
    .fn = ~ str_replace(.x, pattern = "c3kr", replacement = "opp_")
  ) %>%
  mutate(
    pm_like = match(pm_like,levels(pm_like)) - 1,
    pm_like = if_else(pm_like == min(pm_like), NA_real_, pm_like),
    opp_like = match(opp_like,levels(opp_like)) - 1,
    opp_like = if_else(opp_like == min(opp_like), NA_real_, opp_like)
  ) %>% 
  rename_with(.cols = starts_with("pm_")  & !ends_with("like"), .fn = ~ renameTraits(.x)) %>% 
  rename_with(.cols = starts_with("opp_") & !ends_with("like"), .fn = ~ renameTraits(.x)) %>% 
  mutate(
    weight = 1,
    birth_year = ifelse(birth_year == "Missing", NA, as.numeric(as.character(birth_year)) +
                          1900)
  )

# 2010
d2010 <- read.spss(here("../2010/2. Australian Election Study, 2010.sav")) %>%
  as_tibble() %>%
  select(hvote = b9reps,
         pm_like = c3julia,
         opp_like = c3abbott,
         starts_with("c4jg"),
         starts_with("c5ta"),
         satisfaction_with_democracy = c7,
         trust_in_government = c9,
         big_interests_government = c10,
         anzus_item=f3,
         trust_item=f4,
         birth_year=h2,
         weight=weight2) %>%
  rename_with(
    .cols = starts_with("c4jg"),
    .fn = ~ str_replace(.x, pattern = "c4jg", replacement = "pm_")
  ) %>%
  rename_with(
    .cols = starts_with("c5ta"),
    .fn = ~ str_replace(.x, pattern = "c5ta", replacement = "opp_")
  ) %>%
  mutate(
    pm_like = match(pm_like,levels(pm_like)) - 1,
    pm_like = if_else(pm_like == min(pm_like), NA_real_, pm_like),
    opp_like = match(opp_like,levels(opp_like)) - 1,
    opp_like = if_else(opp_like == min(opp_like), NA_real_, opp_like)
  ) %>% 
  rename_with(.cols = starts_with("pm_")  & !ends_with("like"), .fn = ~ renameTraits(.x)) %>% 
  rename_with(.cols = starts_with("opp_") & !ends_with("like"), .fn = ~ renameTraits(.x)) %>% 
  mutate(weight = ifelse(is.na(weight),
                         mean(weight[hvote %in% c("Voted informal/Did not vote")],na.rm=TRUE),
                         weight),
         birth_year = ifelse(birth_year=="Missing",NA,as.numeric(as.character(birth_year))+1900))

# 2013
d2013 <-
  read.spss(here("../2013/2. Australian Election Study, 2013.sav")) %>%
  as_tibble() %>%
  select(
    hvote = b9reps,
    pm_like = c1rudd,
    opp_like = c1abbott,
    starts_with("c2kr"),
    starts_with("c3ta"),
    satisfaction_with_democracy = c4,
    trust_in_government = c5,
    big_interests_government = c6,
    anzus_item = f3,
    trust_item = f4,
    birth_year = h2,
    weight = weight
  ) %>%
  rename_with(
    .cols = starts_with("c2kr"),
    .fn = ~ str_replace(.x, pattern = "c2kr", replacement = "pm_")
  ) %>%
  rename_with(
    .cols = starts_with("c3ta"),
    .fn = ~ str_replace(.x, pattern = "c3ta", replacement = "opp_")
  ) %>%
  mutate(
    pm_like = match(pm_like,levels(pm_like)) - 1,
    pm_like = if_else(pm_like == min(pm_like), NA_real_, pm_like),
    opp_like = match(opp_like,levels(opp_like)) - 1,
    opp_like = if_else(opp_like == min(opp_like), NA_real_, opp_like)
  ) %>% 
  rename_with(.cols = starts_with("pm_")  & !ends_with("like"), .fn = ~ renameTraits(.x)) %>% 
  rename_with(.cols = starts_with("opp_") & !ends_with("like"), .fn = ~ renameTraits(.x)) %>% 
  mutate(birth_year = ifelse(birth_year == "Missing", NA, as.numeric(as.character(birth_year))))

# d2016
d2016 <- read.spss(here("../2016/2. Australian Election Study, 2016.sav")) %>%
  as_tibble() %>%
  select(hvote = B9_1,
         pm_like = C1_1,
         opp_like = C1_2,
         starts_with("C2_"),
         starts_with("C3_"),
         satisfaction_with_democracy = C5,
         trust_in_government = C6,
         big_interests_government = C7,
         anzus_item = F3,
         trust_item = F4,
         birth_year = H2,
         weight = wt_aec) %>%
  rename_with(
    .cols = starts_with("C3_"),
    .fn = ~ tolower(str_replace(.x, pattern = "C3_", replacement = "pm_"))
  ) %>%
  rename_with(
    .cols = starts_with("C2_"),
    .fn = ~ tolower(str_replace(.x, pattern = "C2_", replacement = "opp_"))
  ) %>%
  mutate(
    pm_like = match(pm_like,levels(pm_like)) - 1,
    pm_like = if_else(pm_like == min(pm_like), NA_real_, pm_like),
    opp_like = match(opp_like,levels(opp_like)) - 1,
    opp_like = if_else(opp_like == min(opp_like), NA_real_, opp_like)
  ) %>% 
  rename_with(.cols = starts_with("pm_")  & !ends_with("like"), .fn = ~ renameTraits(.x)) %>% 
  rename_with(.cols = starts_with("opp_") & !ends_with("like"), .fn = ~ renameTraits(.x)) %>% 
  mutate(birth_year = ifelse(birth_year == "Item skipped",
                             NA,
                             as.numeric(as.character(birth_year))))

# d2019
d2019 <- read.spss(here("../2019/aes19_unrestricted.sav")) %>%
  as_tibble() %>%
  select(hvote = B9_1,
         pm_like = C1_1,
         opp_like = C1_2,
         starts_with("C2"),
         starts_with("C3"),
         satisfaction_with_democracy = C5,
         trust_in_government = C6,
         big_interests_government = C7,
         anzus_item = F3,
         trust_item = F4,
         birth_year = H2,
         weight = wt_pooled) %>%
  rename_with(
    .cols = starts_with("C2_"),
    .fn = ~ tolower(str_replace(.x, pattern = "C2_", replacement = "pm_"))
  ) %>%
  rename_with(
    .cols = starts_with("C3_"),
    .fn = ~ tolower(str_replace(.x, pattern = "C3_", replacement = "opp_"))
  ) %>%
  mutate(
    pm_like = match(pm_like,levels(pm_like)) - 1,
    pm_like = if_else(pm_like == min(pm_like), NA_real_, pm_like),
    opp_like = match(opp_like,levels(opp_like)) - 1,
    opp_like = if_else(opp_like == min(opp_like), NA_real_, opp_like)
  ) %>% 
  select(-ends_with("order")) %>% 
  rename_with(.cols = starts_with("pm_")  & !ends_with("like"), .fn = ~ renameTraits(.x)) %>% 
  rename_with(.cols = starts_with("opp_") & !ends_with("like"), .fn = ~ renameTraits(.x)) %>% 
  mutate(birth_year = ifelse(birth_year == "Item skipped",
                             NA,
                             as.numeric(as.character(birth_year))
                             )
         )

## make factor
makeFactor <- function(x){
  cl <- as.character(match.call()$x)
  print(cl)
  l <- levs[[cl]]
  print(l)
  z <- factor(x,
              levels = as.vector(l),
              labels = names(l))
  return(z)
}

#d2022
levs <- attributes(
  readstata13::read.dta13(
    here("data/2790 AES 2022 Datafile - Weighted Final 2022-10-28 copy.dta")
  )
)$label.table

d2022 <- readstata13::read.dta13(here("data/2790 AES 2022 Datafile - Weighted Final 2022-10-28 copy.dta"),
                                 convert.factors = TRUE,
                                 generate.factors = TRUE) %>% 
  as_tibble() %>% 
  ##%>% 
  ##mutate(weight = case_when(
  ##  B9A == "Liberal Party" ~ .9968,
  ##  B9A == "Labor Party (ALP)" ~ .8579,
  ##  B9A == "National Party" ~ 1.2413,
  ##  B9A == "Greens" ~ .8145,
  ##  B9A == "Other (please specify party)" ~ 1.6581,
  ##  TRUE ~ 1),
  ##  weight = weight/sum(weight) * n()
  ##  ) %>%
  mutate(B9_1 = factor(B9_1,
                       levels = as.vector(levs$B9_1),
                       labels = names(levs$B9_1))) %>% 
  mutate(dem_house22 = factor(dem_house22,
                              levels = as.vector(levs$dem_house22),
                              labels = names(levs$dem_house22))) %>% 
  mutate(hvote = B9_1) %>% 
  mutate(
    across(
      starts_with("C2_") | starts_with("C3_"),
      .f = ~ makeFactor(.x)
    ),
    across(
      starts_with("C1_") | C5 | C6 | C7 | F3 | F4,
      .f = ~ makeFactor(.x)
    )
  ) %>% 
  select(weight,
         dem_house22,
         hvote,
         starts_with("B9"),
         pm_like = C1_1,
         opp_like = C1_2,
         starts_with("C2"),
         starts_with("C3"),
         satisfaction_with_democracy = C5,
         trust_in_government = C6,
         big_interests_government = C7,
         anzus_item = F3,
         trust_item = F4,
         birth_year = H2) %>% 
  rename_with(
    .cols = starts_with("C2_"),
    .fn = ~ tolower(str_replace(.x, pattern = "C2_", replacement = "pm_"))
  ) %>%
  rename_with(
    .cols = starts_with("C3_"),
    .fn = ~ tolower(str_replace(.x, pattern = "C3_", replacement = "opp_"))
  ) %>%
  mutate(
    pm_like = match(pm_like,levels(pm_like)) - 1,
    pm_like = if_else(pm_like == min(pm_like), NA_real_, pm_like),
    opp_like = match(opp_like,levels(opp_like)) - 1,
    opp_like = if_else(opp_like == min(opp_like), NA_real_, opp_like)
  ) %>% 
  select(-ends_with("order")) %>% 
  rename_with(.cols = starts_with("pm_")  & !ends_with("like"), .fn = ~ renameTraits(.x)) %>% 
  rename_with(.cols = starts_with("opp_") & !ends_with("like"), .fn = ~ renameTraits(.x)) %>% 
  rename_with(
    .cols = starts_with("C3_"),
    .fn = ~ tolower(str_replace(.x, pattern = "C3_", replacement = "opp_"))
  ) %>% 
  mutate(
    hvote_collapsed = case_when(
      hvote == "Liberal" ~ "Coalition",
      hvote == "Liberal National Party of Queensland" ~ "Coalition",
      hvote == "Country Liberals (NT)" ~ "Coalition",
      hvote == "National Party" ~ "Coalition",
      hvote == "Labor" ~ "Labor",
      hvote == "Labor Party (ALP)" ~ "Labor",
      hvote == "Greens" ~ "Greens", 
      hvote %in% c("Refused",
                   "Don't know",
                   "Did not vote",
                   "Not eligible to vote",
                   "No answer",
                   "Item skipped",
                   "Does not apply",
                   "Voted informal / Did not vote") ~ NA_character_,
      ##hvote == "No party" ~ NA_character_,
      is.na(hvote) ~ NA_character_,
      TRUE ~ "Other"
    )
  )


tab <- d2022 %>% 
  filter(!is.na(hvote_collapsed)) %>% 
  count(hvote_collapsed,wt = weight) %>% 
  mutate(p = n/sum(n))

v2022_actuals <- tribble(
  ~hvote_collapsed, ~actual,
  "Coalition", .3569,
  "Greens",    .1225,
  "Labor",     .3258,
  "Other",     .1948
)

tab <- tab %>% 
  left_join(v2022_actuals,
            by = "hvote_collapsed") %>% 
  mutate(w = actual/p,
         p_adj = p*w)

## 2022 CSES
d2022_cses <-
  read.spss(here("../2022/data/SRC2932 CSES 2022 Final Data_20220616.sav")) %>%
  as_tibble() %>%
  filter(p_citizen == "Yes") %>%
  select(
    hvote = Q15,
    p_age,
    pm_like = Q24a,
    opp_like = Q24c,
    satisfaction_with_democracy = Q27,
    weight = weight_long
  ) %>%
  mutate(pm_like  = recode(
    pm_like,
    `Don't know` = "Refused",
    `Haven't heard of candidate` = "Refused"
  )) %>%
  mutate(opp_like = recode(
    opp_like,
    `Don't know` = "Refused",
    `Haven't heard of candidate` = "Refused"
  )) %>% 
  mutate(
    pm_like = match(pm_like,levels(pm_like)) - 1,
    opp_like = match(opp_like,levels(opp_like)) - 1,
    pm_like = if_else(pm_like == min(pm_like),NA_real_,pm_like),
    opp_like = if_else(pm_like == min(opp_like),NA_real_,opp_like)
    ) %>% 
  mutate(age = levels(p_age)[match(p_age,levels(p_age))],
         age = if_else(p_age == "Refused",NA_real_,as.numeric(age)),
         age = age + sample(x = c(0,1),
                            prob = c(1/3, 2/3),
                            replace = TRUE,
                            size = n())
         ) %>% 
  mutate(birth_year = 2022 - age) %>% 
  select(-age)


## d2022_cses
d2022_cses <- d2022_cses %>% 
  filter(!(hvote %in% c("Refused","Don't know","Did not vote","Not eligible to vote"))) %>% 
  mutate(
    hvote_collapsed = case_when(
      grepl(pattern="Liberal",hvote) ~ "Coalition",
      hvote == "National Party" ~ "Coalition",
      hvote == "Labor Party (ALP)" ~ "Labor",
      hvote == "Greens" ~ "Greens", TRUE ~ "Other"
      )
    )

tab <- d2022_cses %>% 
  count(hvote_collapsed,wt=weight) %>% 
  mutate(p=n/sum(n))

v2022_actuals <- tribble(
  ~hvote_collapsed, ~actual,
  "Coalition", .3569,
  "Greens",    .1225,
  "Labor",     .3258,
  "Other",     .1948
)

tab <- tab %>% 
  left_join(v2022_actuals,
            by="hvote_collapsed") %>% 
  mutate(w = actual/p,
         p_adj = p*w)
  
print(tab)

d2022_cses <- d2022_cses %>%
  left_join(tab %>%
              select(hvote_collapsed, w),
            by = "hvote_collapsed") %>%
  mutate(weight_simon = weight * w,
         weight_simon = weight_simon / sum(weight_simon) * n())

tab <- left_join(
  d2022_cses %>%
    count(hvote_collapsed) %>%
    mutate(p_unweighted = n / sum(n)),
  tab %>%
    select(hvote_collapsed,
           actual,
           p_weight_long = p),
  by = "hvote_collapsed") %>%
  left_join(
    d2022_cses %>%
      count(hvote_collapsed, wt = weight_simon) %>%
      mutate(p_reweighted = n / sum(n)),
    by = "hvote_collapsed"
  ) %>% 
  select(hvote_collapsed,
         p_unweighted,
         p_weight_long,
         p_reweighted,
         actual)

theObjects <- bind_rows(d1987,
                        d1990,
                        d1993,
                        d1996,
                        d1998,
                        d2001,
                        d2004,
                        d2007,
                        d2010,
                        d2013,
                        d2016,
                        d2019,
                        d2022 #,
                        #d2022_cses %>%
                        #  mutate(weight = weight_simon)
                        )

allNames <- colnames(theObjects)

vote_dk_nv <- c("NO RESPONSE",
                "DID NOT VOTE",
                "Missing",
                "Informal|Didnt vote",
                "-1",
                "Blank",
                "No party",
                "Informal/Didnt vote",
                "Informal/Didn't vote",
                "Informal/Did not vote",
                "Voted informal",
                "Does not apply",
                "Item skipped",
                "Voted informal / Did not vote",
                "No answer")
                
d <-
  bind_rows(d1987,
            d1990,
            d1993,
            d1996,
            d1998,
            d2001,
            d2004,
            d2007,
            d2010,
            d2013,
            d2016,
            d2019,
            d2022 %>% select(-contains("collapse")),
            ##d2022_cses %>% select(-contains("collapse")),
            .id = "year") %>%
  mutate(year = c(1987, 1990, 1993, 1996, 1998, 2001, 2004, 2007, 2010, 2013, 2016, 2019, 2022)[as.numeric(year)]) %>%
  mutate(pm_like  = if_else(year > 1990, pm_like - 1, pm_like)) %>%
  mutate(opp_like = if_else(year > 1990, opp_like - 1, opp_like)) %>% 
  mutate(
    hvote_collapse = case_when(
      hvote %in% c(
        "Liberal", "LIBERAL",
        "Liberal Party",
        "NATIONAL",
        "National Party of Australia",
        "National (country)",
        "National (Country)",
        "National (Country) Party",
        "National Party",
        "Liberal National Party",
        "Liberal National Party of Queensland"
      ) ~ "Coalition",
      hvote %in% c("Labor", "Australian Labor Party", "Labor Party (ALP)", "LABOR") ~ "Labor",
      hvote %in% c("Greens", "Australian Greens") ~ "Greens",
      hvote %in% vote_dk_nv ~ NA_character_,
      TRUE ~ "Other"
    )
  ) %>%
  mutate(
    anzus_item = ifelse(
      anzus_item %in% c("-1", "Blank", "Missing", "Item skipped", "Item Skipped"),
      NA,
      as.character(anzus_item)
    ),
    trust_item = ifelse(
      trust_item %in% c("-1", "Blank", "Missing", "Item skipped", "Item Skipped"),
      NA,
      as.character(trust_item)
    ),
    y_anzus_numeric = match(
      anzus_item,
      c(
        "Very important",
        "Fairly important",
        "Not at all important",
        "Not very important"
      )
    ),
    y_trust_numeric = match(
      trust_item,
      c("A great deal",
        "A fair amount",
        "Not very much",
        "None at all")
    ),
    y_anzus = ifelse(anzus_item %in% c("Very important", "Fairly important"), 1, 0),
    y_anzus = ifelse(is.na(y_anzus), 0, y_anzus),
    y_trust = ifelse(trust_item %in% c("A great deal", "A fair amount"), 1, 0),
    y_trust = ifelse(is.na(y_trust), 0, y_trust)
  ) %>%
  mutate(age = year - birth_year) %>%
  filter(age < 100 & age > 17) %>%
  filter(!is.na(birth_year)) %>%
  mutate(generations = cut(
    birth_year,
    breaks = c(
      min(birth_year, na.rm = TRUE) - 1,
      1927,
      1945,
      1964,
      1980,
      1996,
      max(birth_year, na.rm = TRUE)
    ),
    labels = c(
      "Greatest Generation, <1928",
      "Silent Generation, 1928-45",
      "Boomers, 1946-64",
      "Gen X, 1965-80",
      "Millennials, 1981-96",
      "Gen Z, >1996"
    ),
    right = TRUE)
    ) %>% 
  mutate(
    age_cut = cut(age,
                  breaks = c(min(age,na.rm = TRUE),
                             30,50,70,max(age,na.rm = TRUE)
                           )
                  )
  )

save("d", file = here("data/d.RData"))

v <- d %>% 
  filter(!is.na(hvote_collapse)) %>% 
  count(year,generations,hvote_collapse,wt = weight) %>%
  group_by(year,generations) %>%
  mutate(p = n/sum(n)*100) %>% 
  ungroup()

median_ages_generations_year <- d %>%
  filter(!is.na(hvote_collapse)) %>%
  group_by(generations, year) %>%
  summarise(age = floor(median(age, na.rm = TRUE)),
            n = n()) %>%
  ungroup() %>%
  left_join(
    v %>%
      filter(hvote_collapse %in% c("Coalition", "Labor", "Greens")) %>%
      group_by(year, generations) %>%
      summarise(flag = all(n < 10)) %>%
      ungroup(),
    by = c("generations", "year")
  ) %>%
  mutate(age = if_else(flag, NA_real_, age))

vlines <- expand_grid(year = d %>% distinct(year) %>% pull(year),
                      generations = d %>% distinct(generations) %>% pull(generations),
                      hvote_collapse = c("Coalition", "Labor", "Greens")) %>% 
  mutate(y_lo = 0, y_hi = 60)

fig_two_sarah <- v %>% 
  mutate(p = if_else(n < 10, NA_real_, p)) %>% 
  filter(hvote_collapse %in% c("Coalition","Labor","Greens"))

ggplot(fig_two_sarah,
       aes(
         x = year,
         y = p,
         group = hvote_collapse,
         color = hvote_collapse
       )) +
  geom_segment(
    data = vlines,
    inherit.aes = FALSE,
    color = gray(.95),
    aes(
      x = year,
      xend = year,
      y = y_lo,
      yend = y_hi
    )
  ) +
  geom_point() +
  geom_line() +
  geom_text(
    data = median_ages_generations_year,
    inherit.aes = FALSE,
    aes(x = year,
        label = age,
        y = 65),
    size = 2,
    vjust = 0
  ) +
  scale_color_manual(name = "",
                     values = ussc_colors(c("Coalition", "Labor", "Greens"))) +
  scale_y_continuous(
    "Percent",
    minor_breaks = NULL,
    expand = c(0, 0),
    limits = c(0, max())
  ) +
  scale_x_continuous(
    "",
    expand = expansion(add = c(1, 1)),
    breaks = unique(v$year),
    labels = function(x) {
      str_sub(x, 3, 4)
    },
    minor_breaks = NULL
  ) +
  facet_wrap( ~ generations) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(size = 8))

v2 <- d %>% 
  filter(!is.na(age_cut)) %>% 
  filter(!is.na(hvote_collapse)) %>% 
  count(year,age_cut,hvote_collapse,wt = weight) %>%
  group_by(year,age_cut) %>%
  mutate(p = n/sum(n)*100) %>% 
  ungroup()

ggplot(v2 %>% 
         filter(hvote_collapse %in% c("Labor","Coalition")),
       aes(x = year,
           y = p,
           group = hvote_collapse,
           color = hvote_collapse)) + 
  geom_point() + 
  geom_line() + 
  scale_color_manual(name = "",
                     values = ussc_colors(c("Coalition","Labor"))) +
  scale_x_continuous("",
                     breaks = unique(v2$year),
                     labels = function(x){
                       str_sub(x,3,4)
                       },
                     minor_breaks = NULL) + 
  scale_y_continuous("Percent",minor_breaks = NULL) +
  facet_wrap(~age_cut) + 
  theme_minimal()

## make birth year the x-axis
mymodel <- function(obj,outcome){
  require(mgcv)
  obj <- obj %>%
    mutate(year_fct = factor(year)) %>% 
    mutate(y = factor(hvote_collapse == outcome))
  
  ndata <- obj %>% 
    distinct(birth_year,year) %>% 
    mutate(age = year - birth_year) %>% 
    filter(age > 17 & age < 85) %>% 
    mutate(year_fct = factor(year))
  
  m <- gam(y ~ s(birth_year,year_fct,bs="fs",k=3),
           data = obj,
           family = binomial,
           weight = weight)
  
  ndata <- ndata %>%
    mutate(phat = predict(m,
                          newdata = ndata,
                          type = "response")
           )
  
  return(ndata)
}

m <- list()
m[["Coalition"]] <- mymodel(d,"Coalition")
m[["Labor"]] <- mymodel(d,"Labor")
m[["Greens"]] <- mymodel(d,"Greens")

plotData <- bind_rows(m, .id = "hvote") %>%
  mutate(phat = phat * 100,
         y2022 = year == 2022)

save("plotData",
     file = here("data/plotData.RData"))

ggplot(
  plotData,
  aes(
    x = birth_year,
    y = phat,
    group = year,
    color = hvote,
    alpha = y2022,
    size = y2022
  )
) +
  geom_line(show.legend = FALSE) +
  scale_color_manual(name = "", values = ussc_colors(c("Coalition", "Labor", "Greens"))) +
  scale_alpha_manual(name = "", values = c("TRUE" = 1, "FALSE" = .2)) +
  scale_size_manual(name = "", values = c("TRUE" = 1.2, "FALSE" = .5)) +
  scale_x_continuous("Birth year") +
  scale_y_continuous("Percentage") +
  facet_wrap( ~ hvote) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

save("d", file = here("data/aes_cumulative_for_2022_launch.RData"))

round(prop.table(xtabs(weight~hvote_collapse+year,data=d),2)*100)

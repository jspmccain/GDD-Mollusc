#Coding the exclusion criteria

library(magrittr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(stringr)
library(gridExtra)
library(beepr)

select <- dplyr::select
filter <- dplyr::filter

setwd("C:\\Users\\Scott\\Dropbox\\GDD_2016\\Data\\S1_GDD Data")

df_fin <- read.csv("GDD_dataset.csv")

#this script filters out certain conditions of specific data sets. The exclusion criteria for each case are outlined within the complementary table. 
#This script also uses established length-to-weight relationships for octopi and squid, to convert the weight to length.

df_fin <- df_fin[!c(df_fin$unique.id == "AlmadaVillela1982_M.edulis" & df_fin$temp > 20 | df_fin$unique.id == "AlmadaVillela1982_M.edulis" & df_fin$temp < 10),]
df_fin <- df_fin[!c(df_fin$unique.id == "Andre2009_O.ocellatus_Fig2" & df_fin$time > 42 & df_fin$temp == 20 | df_fin$unique.id == "Andre2009_O.ocellatus_Fig2" & df_fin$temp == 25 & df_fin$time > 38),]
df_fin <- df_fin[!c(df_fin$unique.id == "Britz1996_H.midae" & df_fin$temp > 20),]
df_fin <- df_fin[!c(df_fin$unique.id == "El-Eman1982_H.duryi" & df_fin$temp == 33),]
df_fin <- df_fin[!c(df_fin$unique.id == "El-Eman1982_B.Alexandrina" & df_fin$temp == 10 | df_fin$unique.id == "El-Eman1982_B.Alexandrina" & df_fin$temp == 33),]
df_fin <- df_fin[!c(df_fin$unique.id == "El-Eman1982_B.Truncatus" & df_fin$temp == 33), ]
# df_fin <- df_fin[!c(df_fin$unique.id == "Hayhurt2001_M.edulis_Fig7" & df_fin$temp < 10), ]# removed for first revision.
# df_fin <- df_fin[!c(df_fin$unique.id == "Hayhurt2001_M.trossulus_Fig7" & df_fin$temp < 10), ]# removed for first revision.
df_fin <- df_fin[!c(df_fin$unique.id == "His1989_M.gallo_25" & df_fin$temp == 30), ]
df_fin <- df_fin[!c(df_fin$unique.id == "His1989_M.gallo_30ppt" & df_fin$temp == 30), ]
df_fin <- df_fin[!c(df_fin$unique.id == "Hodgson1988_C.hastata_Fig5" & df_fin$temp >= 19), ]
df_fin <- df_fin[!c(df_fin$unique.id == "Hodgson1988_C.hastata_Fig3" & df_fin$temp >= 19), ]
df_fin <- df_fin[!c(df_fin$unique.id == "Lima1985_C.plana_Fig2" & df_fin$temp == 12 | df_fin$unique.id == "Lima1985_C.plana_Fig2" & df_fin$temp == 29),]
df_fin <- df_fin[!c(df_fin$unique.id == "Lima1985_C.plana_Fig3"),]
df_fin <- df_fin[!c(df_fin$unique.id == "Mann1979_T.phillipinarum_Table1_shell_weight" & df_fin$temp == 24),]

##size transformations

#raw data were natural logged in the paper, exponentiate to use raw values
df_fin[df_fin$unique.id == "Andre2009_O.pallidus_Fig2",]$size <- exp(df_fin[df_fin$unique.id == "Andre2009_O.pallidus_Fig2",]$size)

#equation from :http://scialert.net/fulltext/?doi=jbs.2009.357.361: Table 4
cuttle_weight_to_length <- function(x){
  length <- exp((log(x) - log(0.2204))/2.733)
  return(length)
}

df_fin[df_fin$unique.id == "Domingues2002_S.officinalis",]$size <- cuttle_weight_to_length(df_fin[df_fin$unique.id == "Domingues2002_S.officinalis",]$size)
df_fin[df_fin$unique.id == "Domingues2002_S.officinalis",]$size.met <- rep("length.mm", df_fin[df_fin$unique.id == "Domingues2002_S.officinalis",]$size.met %>% length())


octo_weight_to_length <- function(x){
  length <- exp((log(x) - log(0.976))/2.691)
  return(length)
}

df_fin[df_fin$unique.id == "Forsythe1988_O.bimaculoides_female",]$size <- octo_weight_to_length(df_fin[df_fin$unique.id == "Forsythe1988_O.bimaculoides_female",]$size)
df_fin[df_fin$unique.id == "Forsythe1988_O.bimaculoides_female",]$size.met <- rep("length.mm", df_fin[df_fin$unique.id == "Forsythe1988_O.bimaculoides_female",]$size.met %>% length())

df_fin[df_fin$unique.id == "Forsythe1988_O.bimaculoides_male",]$size <- octo_weight_to_length(df_fin[df_fin$unique.id == "Forsythe1988_O.bimaculoides_male",]$size)
df_fin[df_fin$unique.id == "Forsythe1988_O.bimaculoides_male",]$size.met <- rep("length.mm", df_fin[df_fin$unique.id == "Forsythe1988_O.bimaculoides_male",]$size.met %>% length())

df_fin[df_fin$unique.id == "Andre2009_O.pallidus_Fig2",]$size <- octo_weight_to_length(df_fin[df_fin$unique.id == "Andre2009_O.pallidus_Fig2",]$size)
df_fin[df_fin$unique.id == "Andre2009_O.pallidus_Fig2",]$size.met <- rep("length.mm", df_fin[df_fin$unique.id == "Andre2009_O.pallidus_Fig2",]$size.met %>% length())

df_fin[df_fin$unique.id == "Andre2009_O.ocellatus_Fig2",]$size <- octo_weight_to_length(df_fin[df_fin$unique.id == "Andre2009_O.ocellatus_Fig2",]$size)
df_fin[df_fin$unique.id == "Andre2009_O.ocellatus_Fig2",]$size.met <- rep("length.mm", df_fin[df_fin$unique.id == "Andre2009_O.ocellatus_Fig2",]$size.met %>% length())

#TO do:

#transform:

weight_to_length_general <- function(x, a = 0.00454, b = 1.9184){
  length <- exp((log(x) - log(a))/b)
  return(length)
}

# unique.id == "Britz1996_H.midae"
#length to weight relationship determined from [1] T. Najmudeen, Biometric relationships of the Indian abalone Haliotis varia Linnaeus 1758 from Mandapam waters of Gulf of Mannar, south-east coast of India, Indian J. Fish. 62 (2015) 146-150.
df_fin[df_fin$unique.id == "Britz1996_H.midae",]$size <- weight_to_length_general(df_fin[df_fin$unique.id == "Britz1996_H.midae",]$size, a = 0.00454, b = 1.9184)
df_fin[df_fin$unique.id == "Britz1996_H.midae",]$size.met <- rep("length.mm", df_fin[df_fin$unique.id == "Britz1996_H.midae",]$size.met %>% length())

# parameters for converting weight to length found in Yap, W.G. 1977. Population biology of the Japanese little-neck Clam, Tapes philippinarum, in Kanoehe Bay, Oahu, Hawaiian Islands. Pacific Science. 31, 3.

# for live weight: a = 0.00033, b = 2.862
# for dry weight and shell weight: a = log(0.015), b = 2.847

weight_to_length_tapes <- function(x, a = 0.00033, b = 2.862){
  length <- exp((log(x) - a)/b)
  return(length)
}

df_fin[df_fin$unique.id == "Mann1979_T.phillipinarum_Table1_dry_weight",]$size <- weight_to_length_tapes(df_fin[df_fin$unique.id == "Mann1979_T.phillipinarum_Table1_dry_weight",]$size, a = log(0.015), b = 2.847)
df_fin[df_fin$unique.id == "Mann1979_T.phillipinarum_Table1_dry_weight",]$size.met <- rep("length.mm", df_fin[df_fin$unique.id == "Mann1979_T.phillipinarum_Table1_dry_weight",]$size.met %>% length())

 
df_fin[df_fin$unique.id == "Mann1979_T.phillipinarum_Table1_live_weight",]$size <- weight_to_length_tapes(df_fin[df_fin$unique.id == "Mann1979_T.phillipinarum_Table1_live_weight",]$size, a = 0.00033, b = 2.862)
df_fin[df_fin$unique.id == "Mann1979_T.phillipinarum_Table1_live_weight",]$size.met <- rep("length.mm", df_fin[df_fin$unique.id == "Mann1979_T.phillipinarum_Table1_live_weight",]$size.met %>% length())


df_fin[df_fin$unique.id == "Mann1979_T.phillipinarum_Table1_shell_weight",]$size <- weight_to_length_tapes(df_fin[df_fin$unique.id == "Mann1979_T.phillipinarum_Table1_shell_weight",]$size, a = log(0.015), b = 2.847)
df_fin[df_fin$unique.id == "Mann1979_T.phillipinarum_Table1_shell_weight",]$size.met <- rep("length.mm", df_fin[df_fin$unique.id == "Mann1979_T.phillipinarum_Table1_shell_weight",]$size.met %>% length())





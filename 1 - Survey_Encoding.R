##There is a blog post on the Harkive website that provides further detail
## about this script, along with a video walkthrough
##- http://harkive.org/surv_enc_viz

#This script takes the Sample of Harkive Survey Data and:
#       1) Converts Text Responses to Numeric Values
#       2) Creates additional variables (sums, means, sd)
#       3) Creates new CSV file of numeric values

##Load in survey data and assign to a variable h16

h16 <- read.csv("h16_survey_sample.csv")

## Responses Q6:Q90 to be converted from text to numerics 
## Responses range from Never (0) to Daily (6); Strongly Agree (3) to Strongly Disagree (-3), etc.
## 'blank' responses to be graded as (0), denoting a neutral value
#Using dpylr package to create a function for this.
#Based on string matching, text observations are converted to corresponding numbers
##NB: Spelling errors/different phrasing in each Likert Scale needs to be 
##taken into when creating these factors.

##

#install.packages('dplyr')
library(dplyr)

factorise_likert <- function(x) {
  case_when(x %in% c("Daily") ~ 6,
            x %in% c("Very Often") ~ 5,
            x %in% c("Often") ~ 4,
            x %in% c("Neither Rarely nor Often") ~ 3,
            x %in% c("Rarely", "Sometimes") ~ 2,
            x %in% c("Very Rarely") ~ 1,
            x %in% c("Never ", "Never", "") ~ 0)
}

factorise_agree <- function(x) {
  case_when(x %in% c("Strongly Agree") ~ 3,
            x %in% c("Agree") ~ 2,
            x %in% c("Somewhat Agree") ~ 1,
            x %in% c("Neither Agree nor Disagree", "Neither Agree Nor Disagree", "") ~ 0,
            x %in% c("Somewhat Disagree") ~ -1,
            x %in% c("Disagree") ~ -2,
            x %in% c("Strongly Disagree") ~ -3)
}

factorise_format <- function(x) {
  case_when(x %in% c("", "Other") ~ 'I' ,
            x %in% c("MP3/Digital Files") ~ 'A',
            x %in% c("Physical Formats") ~ 'B',
            x %in% c("Radio") ~ 'C',
            x %in% c("Live Music") ~ 'D',
            x %in% c("Performing/Making Music") ~ 'E',
            x %in% c("Podcasts") ~ 'F',
            x %in% c("Streaming") ~ 'G')
}

factorise_importance <- function(x) {
  case_when(x %in% c("Very Important") ~ 3,
            x %in% c("Important") ~ 2,
            x %in% c("Somewhat important") ~ 1,
            x %in% c("Neither Important nor Unimportant", "") ~ 0,
            x %in% c("Somewhat unimporant") ~ -1,
            x %in% c("Unimportant") ~ -2,
            x %in% c("Not important at all") ~ -3)
}

##Now apply these functions to each column

head(h16$q6)

n6 <- sapply(h16$q6, factorise_likert)
n7 <- sapply(h16$q7, factorise_likert)
n8 <- sapply(h16$q8, factorise_likert)
n9 <- sapply(h16$q9, factorise_likert)
n10 <- sapply(h16$q10, factorise_likert)
n11 <- sapply(h16$q11, factorise_likert)
n12 <- sapply(h16$q12, factorise_likert)

head(n6)

#bind these to dataframe

h16$n6 <- n6
h16$n7 <- n7
h16$n8 <- n8
h16$n9 <- n9
h16$n10 <- n10
h16$n11 <- n11
h16$n12 <- n12

##use dyplr mutate to add columns together and get total across new values 
##then calculate mean and SD of those figures

h16 <- mutate(h16, n6_12sum = n6 + n7 + n8 + n9 + n10 + n11 + n12)

library(matrixStats)
group6_12 <- c('n6', 'n7', 'n8', 'n9','n10','n11','n12')
h16 <- h16 %>% 
  mutate(mean6_12 = rowMeans(.[group6_12]), sd6_12 = rowSds(as.matrix(.[group6_12])))

a <- c(0,0,0,0,0,0)
sd(a)
b <- c(6,6,6,6,6,6)
sd(b)
c <- c(6,4,5,0,0,1)
sd(c)

#Now Columns 14:15

n14 <- sapply(h16$q14, factorise_format)
n15 <- sapply(h16$q15, factorise_format)
h16$n14 <- n14
h16$n15 <- n15

##16:22

n16 <- sapply(h16$q16, factorise_importance)
n17 <- sapply(h16$q17, factorise_importance)
n18 <- sapply(h16$q18, factorise_importance)
n19 <- sapply(h16$q19, factorise_importance)
n20 <- sapply(h16$q20, factorise_importance)
n21 <- sapply(h16$q21, factorise_importance)
n22 <- sapply(h16$q22, factorise_importance)

h16$n16 <- n16
h16$n17 <- n17
h16$n18 <- n18
h16$n19 <- n19
h16$n20 <- n20
h16$n21 <- n21
h16$n22 <- n22

h16 <- mutate(h16, n16_22sum = n16 + n17 + n18 + n19 + n20 + n21 + n22)
group16_22 <- c('n16', 'n17', 'n18', 'n19','n20','n21','n22')
h16 <- h16 %>% 
  mutate(mean16_22 = rowMeans(.[group16_22]), sd16_22 = rowSds(as.matrix(.[group16_22])))

##23:27 - Never To Daily

n23 <- sapply(h16$q23, factorise_likert)
n24 <- sapply(h16$q24, factorise_likert)
n25 <- sapply(h16$q25, factorise_likert)
n26 <- sapply(h16$q26, factorise_likert)
n27 <- sapply(h16$q27, factorise_likert)

h16$n23 <- n23
h16$n24 <- n24
h16$n25 <- n25
h16$n26 <- n26
h16$n27 <- n27

#28:41 - Never To Daily

n28 <- sapply(h16$q28, factorise_likert)
n29 <- sapply(h16$q29, factorise_likert)
n30 <- sapply(h16$q30, factorise_likert)
n31 <- sapply(h16$q31, factorise_likert)
n32 <- sapply(h16$q32, factorise_likert)
n33 <- sapply(h16$q33, factorise_likert)
n34 <- sapply(h16$q34, factorise_likert)
n35 <- sapply(h16$q35, factorise_likert)
n36 <- sapply(h16$q36, factorise_likert)
n37 <- sapply(h16$q37, factorise_likert)
n38 <- sapply(h16$q38, factorise_likert)
n39 <- sapply(h16$q39, factorise_likert)
n40 <- sapply(h16$q40, factorise_likert)
n41 <- sapply(h16$q41, factorise_likert)
n27 <- sapply(h16$q27, factorise_likert)

h16$n28 <- n28
h16$n29 <- n29
h16$n30 <- n30
h16$n31 <- n31
h16$n32 <- n32
h16$n33 <- n33
h16$n34 <- n34
h16$n35 <- n35
h16$n36 <- n36
h16$n37 <- n37
h16$n38 <- n38
h16$n39 <- n39
h16$n40 <- n40
h16$n41 <- n41

h16 <- mutate(h16, n28_41sum = n28 + n29 + n30 + n31 + n32 + n33 + n34 + n35 + n36 + n37 + n38 + n39 + n40 + n41)
group28_41 <- c('n28', 'n29', 'n30', 'n31','n32','n33','n34', 'n35', 'n36', 'n37', 'n38', 'n39', 'n40', 'n41')
h16 <- h16 %>% 
  mutate(mean28_41 = rowMeans(.[group28_41]), sd28_41 = rowSds(as.matrix(.[group28_41])))

#42:48 - Importance

n42 <- sapply(h16$q42, factorise_importance)
n43 <- sapply(h16$q43, factorise_importance)
n44 <- sapply(h16$q44, factorise_importance)
n45 <- sapply(h16$q45, factorise_importance)
n46 <- sapply(h16$q46, factorise_importance)
n47 <- sapply(h16$q47, factorise_importance)
n48 <- sapply(h16$q48, factorise_importance)

h16$n42 <- n42
h16$n43 <- n43
h16$n44 <- n44
h16$n45 <- n45
h16$n46 <- n46
h16$n47 <- n47
h16$n48 <- n48

h16 <- mutate(h16, n42_48sum = n42 + n43 + n44 + n45 + n46 + n47 + n48)
group42_48 <- c('n42', 'n43', 'n44', 'n45','n46','n47','n48')
h16 <- h16 %>% 
  mutate(mean42_48 = rowMeans(.[group42_48]), sd42_48 = rowSds(as.matrix(.[group42_48])))

##49:58 Agree/Disagree

n49 <- sapply(h16$q49, factorise_agree)
n50 <- sapply(h16$q50, factorise_agree)
n51 <- sapply(h16$q51, factorise_agree)
n52 <- sapply(h16$q52, factorise_agree)
n53 <- sapply(h16$q53, factorise_agree)
n54 <- sapply(h16$q54, factorise_agree)
n55 <- sapply(h16$q55, factorise_agree)
n56 <- sapply(h16$q56, factorise_agree)
n57 <- sapply(h16$q57, factorise_agree)
n58 <- sapply(h16$q58, factorise_agree)
n59 <- sapply(h16$q59, factorise_agree)

h16$n49 <- n49
h16$n50 <- n50
h16$n51 <- n51
h16$n52 <- n52
h16$n53 <- n53
h16$n54 <- n54
h16$n55 <- n55
h16$n56 <- n56
h16$n57 <- n57
h16$n58 <- n58
h16$n59 <- n59

h16 <- mutate(h16, n49_59sum = n49 + n50 + n51 + n52 + n53 + n54 + n55 + n56 + n57 + n58 + n59)
group49_59 <- c('n49', 'n50', 'n51', 'n52','n53','n54','n55', 'n56', 'n57', 'n58', 'n59')
h16 <- h16 %>% 
  mutate(mean49_59 = rowMeans(.[group49_59]), sd49_59 = rowSds(as.matrix(.[group49_59])))

#60:75 Agree/Disagree

n60 <- sapply(h16$q60, factorise_agree)
n61 <- sapply(h16$q61, factorise_agree)
n62 <- sapply(h16$q62, factorise_agree)
n63 <- sapply(h16$q63, factorise_agree)
n64 <- sapply(h16$q64, factorise_agree)
n65 <- sapply(h16$q65, factorise_agree)
n66 <- sapply(h16$q66, factorise_agree)
n67 <- sapply(h16$q67, factorise_agree)
n68 <- sapply(h16$q68, factorise_agree)
n69 <- sapply(h16$q69, factorise_agree)
n70 <- sapply(h16$q70, factorise_agree)
n71 <- sapply(h16$q71, factorise_agree)
n72 <- sapply(h16$q72, factorise_agree)
n73 <- sapply(h16$q73, factorise_agree)
n74 <- sapply(h16$q74, factorise_agree)
n75 <- sapply(h16$q75, factorise_agree)

h16$n60 <- n60
h16$n61 <- n61
h16$n62 <- n62
h16$n63 <- n63
h16$n64 <- n64
h16$n65 <- n65
h16$n66 <- n66
h16$n67 <- n67
h16$n68 <- n68
h16$n69 <- n69
h16$n70 <- n70
h16$n71 <- n71
h16$n72 <- n72
h16$n73 <- n73
h16$n74 <- n74
h16$n75 <- n75

h16 <- mutate(h16, n60_75sum = n60 + n61 + n62 + n63 + n64 + n65 + n66 + n67 + n68 + n69 + n70 + n71 + n72 + n73 + n74 + n75)
group60_75 <- c('n60', 'n61', 'n62', 'n63','n64','n65','n66', 'n67', 'n68', 'n69', 'n70', 'n71', 'n72', 'n73', 'n74', 'n75')
h16 <- h16 %>% 
  mutate(mean60_75 = rowMeans(.[group60_75]), sd60_75 = rowSds(as.matrix(.[group60_75])))

#76:90

n76 <- sapply(h16$q76, factorise_agree)
n77 <- sapply(h16$q77, factorise_agree)
n78 <- sapply(h16$q78, factorise_agree)
n79 <- sapply(h16$q79, factorise_agree)
n80 <- sapply(h16$q80, factorise_agree)
n81 <- sapply(h16$q81, factorise_agree)
n82 <- sapply(h16$q82, factorise_agree)
n83 <- sapply(h16$q83, factorise_agree)
n84 <- sapply(h16$q84, factorise_agree)
n85 <- sapply(h16$q85, factorise_agree)
n86 <- sapply(h16$q86, factorise_agree)
n87 <- sapply(h16$q87, factorise_agree)
n88 <- sapply(h16$q88, factorise_agree)
n89 <- sapply(h16$q89, factorise_agree)
n90 <- sapply(h16$q90, factorise_agree)


h16$n76 <- n76
h16$n77 <- n77
h16$n78 <- n78
h16$n79 <- n79
h16$n80 <- n80
h16$n81 <- n81
h16$n82 <- n82
h16$n83 <- n83
h16$n84 <- n84
h16$n85 <- n85
h16$n86 <- n86
h16$n87 <- n87
h16$n88 <- n88
h16$n89 <- n89
h16$n90 <- n90

h16 <- mutate(h16, n76_90sum = n76 + n77 + n78 + n79 + n80 + n81 + n82 + n83 + n84 + n85 + n86 + n87 + n88 + n89 + n90)
group76_90 <- c('n76', 'n77', 'n78', 'n79','n80','n81','n82', 'n83', 'n84', 'n85', 'n86', 'n87', 'n88', 'n89', 'n90')
h16 <- h16 %>% 
  mutate(mean76_90 = rowMeans(.[group76_90]), sd76_90 = rowSds(as.matrix(.[group76_90])))

#create summaries based on groups

h16 <- mutate(h16, total_sum = n6_12sum + n16_22sum + n28_41sum + n42_48sum + n49_59sum + n60_75sum + n76_90sum)
group_sums <- c('n6_12sum', 'n16_22sum', 'n28_41sum', 'n42_48sum', 'n49_59sum', 'n60_75sum', 'n76_90sum')
h16 <- h16 %>% 
  mutate(mean_groups = rowMeans(.[group_sums]), sd_sums = rowSds(as.matrix(.[group_sums])))

#then remove text-based columns

h16$q6 <- NULL
h16$q7 <- NULL
h16$q8 <- NULL
h16$q9 <- NULL
h16$q10 <- NULL
h16$q11 <- NULL
h16$q12 <- NULL
h16$q14 <- NULL
h16$q15 <- NULL
h16$q16 <- NULL
h16$q17 <- NULL
h16$q18 <- NULL
h16$q19 <- NULL
h16$q20 <- NULL
h16$q21 <- NULL
h16$q22 <- NULL
h16$q23 <- NULL
h16$q24 <- NULL
h16$q25 <- NULL
h16$q26 <- NULL
h16$q27 <- NULL
h16$q28 <- NULL
h16$q29 <- NULL
h16$q30 <- NULL
h16$q31 <- NULL
h16$q32 <- NULL
h16$q33 <- NULL
h16$q34 <- NULL
h16$q35 <- NULL
h16$q36 <- NULL
h16$q37 <- NULL
h16$q38 <- NULL
h16$q39 <- NULL
h16$q40 <- NULL
h16$q41 <- NULL
h16$q42 <- NULL
h16$q43 <- NULL
h16$q44 <- NULL
h16$q45 <- NULL
h16$q46 <- NULL
h16$q47 <- NULL
h16$q48 <- NULL
h16$q49 <- NULL
h16$q50 <- NULL
h16$q51 <- NULL
h16$q52 <- NULL
h16$q53 <- NULL
h16$q54 <- NULL
h16$q55 <- NULL
h16$q56 <- NULL
h16$q57 <- NULL
h16$q58 <- NULL
h16$q59 <- NULL
h16$q60 <- NULL
h16$q61 <- NULL
h16$q62 <- NULL
h16$q63 <- NULL
h16$q64 <- NULL
h16$q65 <- NULL
h16$q66 <- NULL
h16$q67 <- NULL
h16$q68 <- NULL
h16$q69 <- NULL
h16$q70 <- NULL
h16$q71 <- NULL
h16$q72 <- NULL
h16$q73 <- NULL
h16$q74 <- NULL
h16$q75 <- NULL
h16$q76 <- NULL
h16$q77 <- NULL
h16$q78 <- NULL
h16$q79 <- NULL
h16$q80 <- NULL
h16$q81 <- NULL
h16$q82 <- NULL
h16$q83 <- NULL
h16$q84 <- NULL
h16$q85 <- NULL
h16$q86 <- NULL
h16$q87 <- NULL
h16$q88 <- NULL
h16$q89 <- NULL
h16$q90 <- NULL


##Create new spreadsheet of encoded data

write.csv(h16, "survey_sample_encoded.csv")



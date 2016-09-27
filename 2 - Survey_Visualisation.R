## - - HARKIVE 2016 MUSIC LISTENING SURVEY - - ##

#Q1: Gender, 
#Q2: Age,
#Q5: Have you contributed a Harkive story (Y/N)
#Q6:12 - Rate frequency you listen using the following (Never to Daily)

# - - Q6: MP3/Digital Files
# - - Q7: Streaming (Spotify, YouTube, Soundcloud, etc)
# - - Q8: Radio (incl. Online, Analogue, Catch-up)
# - - Q9: Podcasts (incl. Mixcloud, etc)
# - - Q10: Physical Formats (CD, Vinyl, Tape, etc)
# - - Q11: Live Music
# - - Q12: Performing/Making Music

#Q14:15 - Of the following, which is your favourite/least favourite

# - - Q14: Your Favourite
# - - Q15: Your Least Favourite

#Q16:22 - Which of the following are important in selecting a format?
#Options: Not Important At All; Unimportant; Somewhat Unimportant; 
# - - - - Neither Important nor Unimportant; 
# - - - - Somewhat Important; Important; Very Important

#Q16: Convenience
#Q17: Cost
#Q18: Portability
#Q19: Sociability/Sharing
#Q20: Sound Quality
#Q21: Collectibility
#Q22: Exclusivity/Rarity

##Load in survey data and assign to a variable

h16 <- read.csv("survey_sample_encoded.csv")

library(magrittr)
library(ggplot2)
library(RColorBrewer)

##Basic plot

h16 %>%
  ggplot() +
  aes(x = n6_12sum, y = sd6_12) +
  geom_point()

##Adding variables - shape according to Gender

h16 %>%
  ggplot() +
  aes(x = n6_12sum, y = sd6_12, shape=q1) +
  geom_point()

##Adding variables - colour according to Age.
##Remove elements ('Other', 'Rather Not Say') from Gender

h16 %>%
  filter(q1 != "Rather Not Say", q1 != "Other") %>%
  ggplot() +
  aes(x = n6_12sum, y = sd6_12, colour=q2, shape=q1) +
  geom_point() 

##Adding Axis Labels, Title, Renaming Legends

h16 %>%
  filter(q1 != "Rather Not Say", q1 != "Other") %>%
  ggplot() +
  aes(x = n6_12sum, y = sd6_12, colour=q2, shape=q1) +
  geom_jitter() +
  ggtitle('Harkive Survey Q6:12 - Multi Format Engagement') +
  xlab('Regularity of Multi Format Engagement') +
  ylab('Volatility') +
  scale_colour_discrete(name  ="Age") +
  scale_shape_discrete(name  ="Gender")

##NB: Trying changing the geom_point command to other options
## line, etc,

##Save Plot to PDF##

pdf(file="h6_12.pdf", 7, 7)
h16 %>%
  filter(q1 != "Rather Not Say", q1 != "Other") %>%
  ggplot() +
  aes(x = n6_12sum, y = sd6_12, colour=q2, shape=q1) +
  geom_point() +
  ggtitle('Harkive Survey Q6:12 - Multi Format Engagement') +
  xlab('Regularity of Multi Format Engagement') +
  ylab('Standard Deviation') +
  scale_colour_discrete(name  ="Age") +
  scale_shape_discrete(name  ="Gender")
dev.off()

##Save Plot to JPeg - cannot specify sizes

jpeg(file="h6_12.jpeg")
h16 %>%
  filter(q1 != "Rather Not Say", q1 != "Other") %>%
  ggplot() +
  aes(x = n6_12sum, y = sd6_12, colour=q2, shape=q1) +
  geom_point() +
  ggtitle('Harkive Survey Q6:12 - Multi Format Engagement') +
  xlab('Regularity of Multi Format Engagement') +
  ylab('Standard Deviation') +
  scale_colour_discrete(name  ="Age") +
  scale_shape_discrete(name  ="Gender")
dev.off()

#Plot Favourite Format

h16 %>%
  filter(q1 != "Rather Not Say", q1 != "Other", n14 != "", n15 != "") %>%
  ggplot() +
  aes(x = n6_12sum, y = sd6_12, colour=n14) +
  geom_point() +
  ggtitle('Harkive Survey Q14 - Multi Format Engagement\nColoured by Favorite Format') +
  xlab('Regularity of Multi Format Engagement') +
  ylab('Standard Deviation') +
  scale_colour_discrete(name  ="Favourite\nFormat",
                        labels=c("MP3","Phy", "Rad", "Liv", "Perf.", "Pods", "Strm"))

#Plot Least Favourite by Age/Format Engagement

h16 %>%
  filter(q1 != "Rather Not Say", q1 != "Other", n14 != "", n14 != "Other") %>%
  ggplot() +
  aes(x = n6_12sum, y = q2, colour=n15) +
  geom_jitter() +
  ggtitle('Harkive Survey Q15 - Multi Format Engagement by Age\nColoured by Least Favourite Format') +
  xlab('Regularity of Multi Format Engagement') +
  ylab('Age') +
  scale_colour_discrete(name  ="Favourite\nFormat",
                        labels=c("MP3","Phy", "Rad", "Liv", "Perf.", "Pods", "Strm"))


##calcultate relationship between SD and SUM(6_12).
##i.e. does 'volatility' affect 'engagement' with multi-formats?
##coloured by favourite format 
h16 <- mutate(h16, n6_12sumsd = n6_12sum/sd6_12)

h16 %>%
  filter(q1 != "Rather Not Say", q1 != "Other", n14 != "", n14 != "Other") %>%
  ggplot() +
  aes(x = n6_12sum, y = n6_12sumsd) +
  geom_jitter(aes(colour=n14)) +
  stat_smooth(method ="lm") +
  ggtitle('Harkive Survey Multi Format Engagement\nColoured by Favourite Format') +
  xlab('Regularity of Multi Format Engagement') +
  ylab('SD/Total') +
  scale_colour_discrete(name  ="Favourite\nFormat",
                        labels=c("MP3","Phy", "Rad", "Liv", "Perf.", "Pods", "Strm"))

#Bringing in Qs 16-22 - Motivations for Listening

h16 %>%
  filter(q1 != "Rather Not Say", q1 != "Other") %>%
  ggplot() +
  aes(x = n16_22sum, y = n6_12sum, colour=n14, shape=q1) +
  geom_jitter() +
  ggtitle('Harkive Survey Q16:22 - Format Choice Motivations') +
  xlab('Importance of Motivations') +
  ylab('Level of Multi-Format Engagement') +
  scale_colour_discrete(name  ="Formats") +
  scale_shape_discrete(name  ="Gender")

##Plot some of the motivational elements: Convenience v Cost

h16 %>%
  filter(q1 != "Rather Not Say", q1 != "Other") %>%
  ggplot() +
  aes(x = n16, y = n17, colour=q2, shape=q1) +
  geom_jitter() + 
  ggtitle('Harkive Survey Q16:17\nFormat Choice Motivations: Convenience v Cost') +
  xlab('Convenience') +
  ylab('Cost') +
  scale_colour_discrete(name  ="Fave Format") +
  scale_shape_discrete(name  ="Gender")

h16 %>%
  filter(q1 != "Rather Not Say", q1 != "Other") %>%
  ggplot() +
  aes(x = n16, y = n22, colour=n14, shape=q5) +
  geom_jitter() + 
  ggtitle('Harkive Survey Q17:1\nFormat Choice Motivations: Convenience v Exlusivity') +
  xlab('Convenience') +
  ylab('Exclusivity') +
  scale_colour_discrete(name  ="Fave Format") +
  scale_shape_discrete(name  ="Yes/No")




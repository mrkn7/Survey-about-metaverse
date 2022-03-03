library(gmodels)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(Hmisc)
library(magrittr)
library(ggstatsplot)
library(sjPlot)
library(readr)
library(caret)
library(wesanderson)
data <- read.csv("cleaningdata.csv", sep = ";", encoding = "UTF-8")
data$X <- NULL
data <- data %>% mutate_all(na_if, " ")
# Deleting the incomplete observations
data <- data %>%
  rownames_to_column() %>%
  na.omit(data)
data$rowname <- NULL
data$gender <- trimws(data$gender, which = c("both"))
data$gender <- factor(data$gender)
data$cybercendrommetaverse <- trimws(data$cybercendrommetaverse, which = c("both"))
data$cybercendrommetaverse <- factor(data$cybercendrommetaverse)
data$compatibility.issues <- trimws(data$compatibility.issues, which = c("both"))
data$compatibility.issues <- factor(data$compatibility.issues)
data$metaversesocioeconomic <- trimws(data$metaversesocioeconomic, which = c("both"))
data$metaversesocioeconomic <- factor(data$metaversesocioeconomic)
data$spend.my.time <- trimws(data$spend.my.time, which = c("both"))
data$spend.my.time <- factor(data$spend.my.time)
data$metaverseethic <- trimws(data$metaverseethic, which = c("both"))
data$metaverseethic <- factor(data$metaverseethic)
data$scares.me <- trimws(data$scares.me, which = c("both"))
data$scares.me <- factor(data$scares.me)
data$achievementshappiness <- trimws(data$achievementshappiness, which = c("both"))
data$achievementshappiness <- factor(data$achievementshappiness)
data$human.rights.change <- trimws(data$human.rights.change, which = c("both"))
data$human.rights.change <- factor(data$human.rights.change)
data$education <- trimws(data$education, which = c("both"))
data$education <- factor(data$education)
data$faculty <- trimws(data$faculty, which = c("both"))
data$faculty <- factor(data$faculty)
data$virtualexperience <- trimws(data$virtualexperience, which = c("both"))
data$virtualexperience <- factor(data$virtualexperience)
data$Metaversethougts <- trimws(data$Metaversethougts, which = c("both"))
data$Metaversethougts <- factor(data$Metaversethougts)
#Correcting capitalization errors#
data[356, 5] <- "Hayır"
data$metaverse.concept <- trimws(data$metaverse.concept, which = c("both"))
data$metaverse.concept <- factor(data$metaverse.concept)
data$metaverseleader <- trimws(data$metaverseleader, which = c("both"))
data$metaverseleader <- factor(data$metaverseleader)
data$laws <- trimws(data$laws, which = c("both"))
data$laws <- factor(data$laws)
data$which.social.media <- trimws(data$which.social.media, which = c("both"))
data$which.social.media <- factor(data$which.social.media)
# 8 or more translated to Turkish
data[23, 12] <- "8 ve daha fazla"
data$socialmediatime <- trimws(data$socialmediatime, which = c("both"))
data$socialmediatime <- factor(data$socialmediatime)
#Development areas translated to Turkish
data[276, 8] <- "Sosyal"
data[81, 8] <- "Tıp"
data[c(36, 40, 352), 8] <- "Sanat"
data$development.area <- trimws(data$development.area, which = c("both"))
data$development.area <- factor(data$development.area)
##
ggpiestats(data, metaverse.concept,title = ("Have you heard about Metaverse concept before?"),
           results.subtitle = FALSE,package = "wesanderson",palette = "Darjeeling2")
p <- ggplot(data , aes(development.area, ..count..)) + xlab("Development Area") + ylab("Frequency") + ggtitle("Area that human beings will develop more in the metaverse") +
  geom_bar(aes(fill = development.area), position = "dodge") +
  scale_fill_brewer(palette = "Set1") + geom_text(stat='count', aes(label=..count..), vjust=-1) 
p + ggtitle("Area that human beings will develop more in the metaverse")
ggplot(data , aes(socialmediatime, ..count..)) + xlab("Hours") + ylab("Frequency") + ggtitle("Hours spent on social media per day") +
  geom_bar(aes(fill = socialmediatime), position = "dodge") +
  scale_fill_brewer(palette = "Dark2") + geom_text(stat='count', aes(label=..count..), vjust=-1) + coord_flip()
## Question_1 
data_r2 <- data %>% slice(-c(66, 70, 81, 157, 319, 330))
r2_chisq <- chisq.test(data_r2$gender, data_r2$scares.me)
ggplot(data_r2, aes(data_r2$scares.me, ..count..)) + geom_bar(aes(fill = data_r2$gender), position = "dodge") +
  scale_color_manual(values = c("#0073C2FF", "#EFC000FF"))+
  scale_fill_manual(values = c("#0073C2FF", "#EFC000FF"))
fisher_r2 <- fisher.test(data_r2$gender, data_r2$scares.me)
ggbarstats(
  data_r2, gender, scares.me,
  results.subtitle = FALSE,
  xlab = "It scares me that something bad might happen in the Metaverse.",
  legend.title = "Gender",
  subtitle = paste0(
    "Fisher's Exact Test", ", p-value = ",
    ifelse(fisher_r2$p.value < 0.001, "< 0.001", round(fisher_r2$p.value, 3))
  )
)
data_2 <- data_r2 %>% select(gender, scares.me)
data_2$gender <- droplevels(data_2$gender)
data_2 %>%
  sjtab(fun = "xtab", var.labels=c("gender", "scares.me"),
        show.row.prc=T, show.col.prc=T, show.summary=T, show.exp=T, show.legend=T)
####QUESTION 2
data_3 <- data %>% select(socialmediatime, spend.my.time)
data_3 %>%
  sjtab(fun = "xtab", var.labels=c("socialmediatime", "spend.my.time"),
        show.row.prc=T, show.col.prc=F, show.summary=T, show.exp=F, show.legend=T)
###QUESTION3
data_r3 <- data %>% slice(-c(68,140,151,156,163,241,253))
data_r3 <- data_r3 %>% mutate_all(na_if, " ")
data_r3 <- data_r3 %>% mutate_all(na_if, "")
data_r3 <- data_r3 %>%
  na.omit(data_r3)
data_r3$human.rights.change <- trimws(data_r3$human.rights.change, which = c("both"))
data_r3$human.rights.change <- factor(data_r3$human.rights.change)
data_r3 %>%
  sjtab(fun = "xtab", var.labels=c("laws", "human.rights.change"),
        show.row.prc=T, show.col.prc=F, show.summary=T, show.exp=F, show.legend=T)
ggplot(data_r3, aes(laws, ..count..)) +
  geom_bar(aes(fill = human.rights.change), position = "dodge") +
  xlab("Who should make the laws and rules?") +
  ylab("Frequency") +
  scale_color_manual(values = c("#0073C2FF", "#EFC000FF","#E69F00", "#56B4E9", "#009E73"))+
  scale_fill_manual(name = "I think laws & human rights & economy etc. would change in the Metaverse positively.",
                    values = c("#0073C2FF", "#EFC000FF", "#E69F00", "#56B4E9", "#009E73")) + geom_text(stat='count', aes(label=..count..), vjust=-1)
#QUESTION4
fisher.test(data$scares.me, data$cybercendrommetaverse,
            simulate.p.value = TRUE)
index <- createDataPartition(data$scares.me, p = .8, list = FALSE)
train <- data[index, ]
test <- data[-index, ]
# Training the model
logistic_model <- glm(scares.me ~ cybercendrommetaverse, family = binomial(), train)
# Checking the model
summary(logistic_model)
exp(-0.04879 )













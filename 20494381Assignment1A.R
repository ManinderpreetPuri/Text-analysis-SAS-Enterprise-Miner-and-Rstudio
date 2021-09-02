# Name: Maninderpreet Singh Puri
# Student number: 20494381

#loading libraries
library(dplyr)
library(xlsx)
library(corrplot)
library(GGally)
library(plotly)
library(ggplot2)
library(rpart)
library(rpart.plot)

# Configuring working directory
setwd("C:/Users/Sazee/Desktop/BUS5CA/Assignment 1/Case 1")

# Loading the dataset.
df <- read.csv(file="news_cleaned_new.csv", header=TRUE, sep=",")

# Exploring the data set
summary(df)

# Getting the top and bottom 20% of News data for Task 2 (third bullet point)
a = round(20 * nrow(df)/  100)
df = df[ order( df[,5], decreasing = TRUE),]
top_df = head(df, a)

df = df[ order( df[,5], decreasing = FALSE),]
bottom_df = head(df, a)

# filtering the document according to the respective article domains
Entertainment <- filter(df, is_Entertainment == "1")
Lifestyle <- filter(df, is_Lifestyle == "1")
Scitech <- filter(df, is_Scitech == "1")
Sports <- filter(df, is_Sports == "1")
World <- filter(df, is_World == "1")

# Saving as separate datasets for each article domain to different Excel sheets  
write.xlsx(Entertainment, "C:\\Users\\Sazee\\Desktop\\BUS5CA\\Assignment 1\\Case 1\\Entertainment.xlsx", row.names = FALSE)
write.xlsx(Lifestyle, "C:\\Users\\Sazee\\Desktop\\BUS5CA\\Assignment 1\\Case 1\\Lifestyle.xlsx", row.names = FALSE)
write.xlsx(Scitech, "C:\\Users\\Sazee\\Desktop\\BUS5CA\\Assignment 1\\Case 1\\Scitech.xlsx", row.names = FALSE)
write.xlsx(Sports, "C:\\Users\\Sazee\\Desktop\\BUS5CA\\Assignment 1\\Case 1\\Sports.xlsx", row.names = FALSE)
write.xlsx(World, "C:\\Users\\Sazee\\Desktop\\BUS5CA\\Assignment 1\\Case 1\\World.xlsx", row.names = FALSE)
write.xlsx(top_df, "C:\\Users\\Sazee\\Desktop\\BUS5CA\\Assignment 1\\Case 1\\top_df.xlsx", row.names = FALSE)
write.xlsx(bottom_df, "C:\\Users\\Sazee\\Desktop\\BUS5CA\\Assignment 1\\Case 1\\bottom_df.xlsx", row.names = FALSE)

# Identifying the articles with a high number of comments in each data channel.
a = round(20 * nrow(Entertainment)/  100)

Entertainment = Entertainment[ order( Entertainment[,5], decreasing = TRUE),]

top_Entertainment = head(Entertainment, a)


a = round(20 * nrow(Lifestyle)/  100)

Lifestyle = Lifestyle[ order( Lifestyle[,5], decreasing = TRUE),]

top_Lifestyle = head(Lifestyle, a)


a = round(20 * nrow(Sports)/  100)

Sports = Sports[ order( Sports[,5], decreasing = TRUE),]

top_Sports = head(Sports, a)


a = round(20 * nrow(Scitech)/  100)

Scitech = Scitech[ order( Scitech[,5], decreasing = TRUE),]

top_Scitech = head(Scitech, a)


a = round(20 * nrow(World)/  100)

World = World[ order( World[,5], decreasing = TRUE),]

top_World = head(World, a)


# Deleting nominal and ordinal variables from all the filtered article domains. Just including 
#"n_comments, n_words_content, n_words_headline, n_words_abstract, n_keywords, is_weekend" parameters.

top_Entertainment = subset(top_Entertainment, select = c(n_comments, n_words_content, n_words_headline, n_words_abstract, n_keywords, is_weekend))
top_Lifestyle = subset(top_Lifestyle, select = c(n_comments, n_words_content, n_words_headline, n_words_abstract, n_keywords, is_weekend))
top_Scitech = subset(top_Scitech, select = c(n_comments, n_words_content, n_words_headline, n_words_abstract, n_keywords, is_weekend))
top_Sports = subset(top_Sports, select = c(n_comments, n_words_content, n_words_headline, n_words_abstract, n_keywords, is_weekend))
top_World = subset(top_World, select = c(n_comments, n_words_content, n_words_headline, n_words_abstract, n_keywords, is_weekend))


#Checking correlation of n_comments with all other variables in all article domains.
M <- data.matrix(top_Entertainment)
corrM <- cor(M, method="spearman")
corrplot(corrM, number.cex = 1, number.font = 2, method = "number")

M <- data.matrix(top_Lifestyle)
corrM <- cor(M, method="spearman")
corrplot(corrM, number.cex = 1, number.font = 2, method = "number")

M <- data.matrix(top_Scitech)
corrM <- cor(M, method="spearman")
corrplot(corrM, number.cex = 1, number.font = 2, method = "number")

M <- data.matrix(top_Sports)
corrM <- cor(M, method="spearman")
corrplot(corrM, number.cex = 1, number.font = 2, method = "number")

M <- data.matrix(top_World)
corrM <- cor(M, method="spearman")
corrplot(corrM, number.cex = 1, number.font = 2, method = "number")

#Plotting variables of interest and the number of comments with fitted lines for the Entertainment category.
ggplot(data=top_Entertainment) + geom_jitter(aes(x=n_words_content, y=n_comments), color = 'blue', size = 1)+ geom_smooth(aes(x=n_words_content, y=n_comments),method = "lm")
ggplot(data=top_Entertainment) + geom_jitter(aes(x=n_words_headline, y=n_comments), color = 'red', size = 1)+ geom_smooth(aes(x=n_words_headline, y=n_comments),method = "lm")
ggplot(data=top_Entertainment) + geom_jitter(aes(x=n_words_abstract, y=n_comments), color = 'orange', size = 1)+ geom_smooth(aes(x=n_words_abstract, y=n_comments),method = "lm")
ggplot(data=top_Entertainment) + geom_jitter(aes(x=n_keywords, y=n_comments), color = 'purple', size = 1) + geom_smooth(aes(x=n_keywords, y=n_comments),method = "lm")
ggplot(data=top_Entertainment) + geom_jitter(aes(is_weekend, n_comments), color = 'green', size = 1)  +  geom_smooth(aes(x = is_weekend, y = n_comments), method = "lm", se = TRUE, alpha = 1)


#Plotting variables of interest and the number of comments with fitted lines for the Lifestyle category .
ggplot(data=top_Lifestyle) + geom_jitter(aes(x=n_words_content, y=n_comments), color = 'blue', size = 1)+ geom_smooth(aes(x=n_words_content, y=n_comments),method = "lm")
ggplot(data=top_Lifestyle) + geom_jitter(aes(x=n_words_headline, y=n_comments), color = 'red', size = 1)+ geom_smooth(aes(x=n_words_headline, y=n_comments),method = "lm")
ggplot(data=top_Lifestyle) + geom_jitter(aes(x=n_words_abstract, y=n_comments), color = 'orange', size = 1)+ geom_smooth(aes(x=n_words_abstract, y=n_comments),method = "lm")
ggplot(data=top_Lifestyle) + geom_jitter(aes(x=n_keywords, y=n_comments), color = 'purple', size = 1) + geom_smooth(aes(x=n_keywords, y=n_comments),method = "lm")
ggplot(data=top_Lifestyle) + geom_jitter(aes(is_weekend, n_comments), color = 'green', size = 1)  +  geom_smooth(aes(x = is_weekend, y = n_comments), method = "lm", se = TRUE, alpha = 1)


#Plotting variables of interest and the number of comments with fitted lines for the Scitech category .
ggplot(data=top_Scitech) + geom_jitter(aes(x=n_words_content, y=n_comments), color = 'blue', size = 1)+ geom_smooth(aes(x=n_words_content, y=n_comments),method = "lm")
ggplot(data=top_Scitech) + geom_jitter(aes(x=n_words_headline, y=n_comments), color = 'red', size = 1)+ geom_smooth(aes(x=n_words_headline, y=n_comments),method = "lm")
ggplot(data=top_Scitech) + geom_jitter(aes(x=n_words_abstract, y=n_comments), color = 'orange', size = 1)+ geom_smooth(aes(x=n_words_abstract, y=n_comments),method = "lm")
ggplot(data=top_Scitech) + geom_jitter(aes(x=n_keywords, y=n_comments), color = 'purple', size = 1) + geom_smooth(aes(x=n_keywords, y=n_comments),method = "lm")
ggplot(data=top_Scitech) + geom_jitter(aes(is_weekend, n_comments), color = 'green', size = 1)  +  geom_smooth(aes(x = is_weekend, y = n_comments), method = "lm", se = TRUE, alpha = 1)


#Plotting variables of interest and the number of comments with fitted lines for the Sports category .
ggplot(data=top_Sports) + geom_jitter(aes(x=n_words_content, y=n_comments), color = 'blue', size = 1)+ geom_smooth(aes(x=n_words_content, y=n_comments),method = "lm")
ggplot(data=top_Sports) + geom_jitter(aes(x=n_words_headline, y=n_comments), color = 'red', size = 1)+ geom_smooth(aes(x=n_words_headline, y=n_comments),method = "lm")
ggplot(data=top_Sports) + geom_jitter(aes(x=n_words_abstract, y=n_comments), color = 'orange', size = 1)+ geom_smooth(aes(x=n_words_abstract, y=n_comments),method = "lm")
ggplot(data=top_Sports) + geom_jitter(aes(x=n_keywords, y=n_comments), color = 'purple', size = 1) + geom_smooth(aes(x=n_keywords, y=n_comments),method = "lm")
ggplot(data=top_Sports) + geom_jitter(aes(is_weekend, n_comments), color = 'green', size = 1)  +  geom_smooth(aes(x = is_weekend, y = n_comments), method = "lm", se = TRUE, alpha = 1)


#Plotting variables of interest and the number of comments with fitted lines for the Entertainment category .
ggplot(data=top_World) + geom_jitter(aes(x=n_words_content, y=n_comments), color = 'blue', size = 1)+ geom_smooth(aes(x=n_words_content, y=n_comments),method = "lm")
ggplot(data=top_World) + geom_jitter(aes(x=n_words_headline, y=n_comments), color = 'red', size = 1)+ geom_smooth(aes(x=n_words_headline, y=n_comments),method = "lm")
ggplot(data=top_World) + geom_jitter(aes(x=n_words_abstract, y=n_comments), color = 'orange', size = 1)+ geom_smooth(aes(x=n_words_abstract, y=n_comments),method = "lm")
ggplot(data=top_World) + geom_jitter(aes(x=n_keywords, y=n_comments), color = 'purple', size = 1) + geom_smooth(aes(x=n_keywords, y=n_comments),method = "lm")
ggplot(data=top_World) + geom_jitter(aes(is_weekend, n_comments), color = 'green', size = 1)  +  geom_smooth(aes(x = is_weekend, y = n_comments), method = "lm", se = TRUE, alpha = 1)


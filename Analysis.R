pars <- read.csv("Participant_Only_Data.csv", stringsAsFactors = FALSE)
data <- read.csv("Games_Data.csv", stringsAsFactors = FALSE)

library(ggplot2)

# Data stats
## Gender split
sum(pars$demographics_app.1.player.gender=="Man" | pars$demographics_app.1.player.gender=="Woman" )
sum(pars$demographics_app.1.player.gender=="Man")/sum(pars$demographics_app.1.player.gender=="Man" | pars$demographics_app.1.player.gender=="Woman" )

# Filter out games with non useful conversations
print(paste0("Num trasnfer dropped for transfer success condition:",sum(data$game.useful_convo ==0 & data$game.RANDOM_TRANSFER_SUCCESS == 1)))
print(paste0("Num trasnfer dropped for transfer fail condition:",sum(data$game.useful_convo ==0 & data$game.RANDOM_TRANSFER_SUCCESS == 0)))
data_filtered <- data[data$game.useful_convo ==1, ]


# game.SELLER_BELIEVE is coded such that 0 = buyer beleives that transfer failed (believes that the buyer believes the them [assuming non altruism])
# game.SELLER_META_BELIEVE is coded such that 0 = believes transfer not successful (believes the seller [assuming non altruism]))

data_filtered$game.BUYER_BELIEVE[data_filtered$game.seller_lied == 1 | is.na(data_filtered$game.seller_lied)] <-
  1 - data_filtered$game.BUYER_BELIEVE[data_filtered$game.seller_lied == 1 | is.na(data_filtered$game.seller_lied)]
data_filtered$game.SELLER_META_BELIEVE[data_filtered$game.seller_lied == 1 | is.na(data_filtered$game.seller_lied)]  <-
  1 - data_filtered$game.SELLER_META_BELIEVE[data_filtered$game.seller_lied == 1 | is.na(data_filtered$game.seller_lied)]
data_filtered$game.BUYER_TRANSFER[!(data_filtered$game.seller_lied == 1 | is.na(data_filtered$game.seller_lied))]  <-
  1 - data_filtered$game.BUYER_TRANSFER[!(data_filtered$game.seller_lied == 1 | is.na(data_filtered$game.seller_lied))]

data_filtered_no_altruism <- data_filtered[data_filtered$game.seller_lied == 1 | is.na(data_filtered$game.seller_lied),]

# Correlation between reported abilities
reg <- lm(pars$introduction_app.1.player.ability_to_detect_lies ~ pars$introduction_app.1.player.ability_to_lie)
summary(reg)

reg <- lm(pars$introduction_app.1.player.ability_to_detect_lies[pars$demographics_app.1.player.gender != ""] ~ pars$demographics_app.1.player.gender[pars$demographics_app.1.player.gender != ""])
summary(reg)

reg <- lm(pars$introduction_app.1.player.ability_to_lie[pars$demographics_app.1.player.gender != ""] ~ pars$demographics_app.1.player.gender[pars$demographics_app.1.player.gender != ""])
summary(reg)

ggplot(pars, aes(x = introduction_app.1.player.ability_to_lie, y = introduction_app.1.player.ability_to_detect_lies))+
  geom_point() +
  geom_smooth(method=lm, se=FALSE, color = "gray") +
  xlab("Reported 'Ability to Lie'") +
  ylab("Reported 'Ability to Detect Lies'") +
  theme_classic()

#### Calculate scores:

for (ii in pars$participant.code) {
  temp <- data_filtered_no_altruism[data_filtered_no_altruism$buyer.id == ii,]
  pars$LIE_ABILITY_SCORE_1[pars$participant.code == ii] <- abs(sum(temp$game.BUYER_TRANSFER) - sum(temp$game.RANDOM_TRANSFER_SUCCESS))  /  dim(temp)[1]
  pars$LIE_ABILITY_SCORE_2[pars$participant.code == ii] <- abs(sum(temp$game.BUYER_BELIEVE) - sum(temp$game.RANDOM_TRANSFER_SUCCESS))  /  dim(temp)[1]
  
  
  temp <- data_filtered_no_altruism[data_filtered_no_altruism$seller.id == ii,]
  pars$DETECTION_ABILITY_SCORE_1[pars$participant.code == ii] <- sum(temp$game.BUYER_TRANSFER)  /  dim(temp)[1]
  pars$DETECTION_ABILITY_SCORE_2[pars$participant.code == ii] <- sum(temp$game.BUYER_BELIEVE)  /  dim(temp)[1]
}

# Rating ability effect on realized ability

reg <- lm(LIE_ABILITY_SCORE_1 ~ introduction_app.1.player.ability_to_lie, pars)
summary(reg)

reg <- lm(LIE_ABILITY_SCORE_2 ~ introduction_app.1.player.ability_to_lie, pars)
summary(reg)


reg <- lm(DETECTION_ABILITY_SCORE_1 ~ introduction_app.1.player.ability_to_detect_lies, pars)
summary(reg)

reg <- lm(DETECTION_ABILITY_SCORE_2 ~ introduction_app.1.player.ability_to_detect_lies, pars)
summary(reg)

  
# Buyer: Altruism VS loss aversion

reg <- lm(game.BUYER_TRANSFER ~ game.BUYER_BELIEVE, data_filtered_no_altruism)
summary(reg)

mean(data_filtered_no_altruism$game.BUYER_TRANSFER[data_filtered_no_altruism$game.BUYER_BELIEVE>0.5])
1- mean(data_filtered_no_altruism$game.BUYER_TRANSFER[data_filtered_no_altruism$game.BUYER_BELIEVE<0.5])
# clear lack of understanding or caring. Or possibly atruistic behavior (since the outcomes are equal)


# CHECK IF THERE'S A DIFFERENCE BETWEEN DECISION FIRST AND BELIEF FIRST
mean(data_filtered_no_altruism$game.BUYER_TRANSFER[data_filtered_no_altruism$game.BUYER_BELIEVE>0.5 & data_filtered_no_altruism$game.BUYER_DECISION_FIRST == 1])
1- mean(data_filtered_no_altruism$game.BUYER_TRANSFER[data_filtered_no_altruism$game.BUYER_BELIEVE<0.5 & data_filtered_no_altruism$game.BUYER_DECISION_FIRST == 1])

mean(data_filtered_no_altruism$game.BUYER_TRANSFER[data_filtered_no_altruism$game.BUYER_BELIEVE>0.5 & data_filtered_no_altruism$game.BUYER_DECISION_FIRST == 0])
1- mean(data_filtered_no_altruism$game.BUYER_TRANSFER[data_filtered_no_altruism$game.BUYER_BELIEVE<0.5 & data_filtered_no_altruism$game.BUYER_DECISION_FIRST == 0])

# Overall, are we able to detect lies:
reg <- lm(game.BUYER_TRANSFER~game.RANDOM_TRANSFER_SUCCESS, data_filtered_no_altruism)
summary(reg)

reg <- lm(game.BUYER_BELIEVE~game.RANDOM_TRANSFER_SUCCESS, data_filtered_no_altruism)
summary(reg)

# Basic cheks similar to gilovich
mean(data_filtered_no_altruism$game.SELLER_META_BELIEVE[data_filtered_no_altruism$game.RANDOM_TRANSFER_SUCCESS==1])

mean(data_filtered_no_altruism$game.BUYER_TRANSFER[data_filtered_no_altruism$game.RANDOM_TRANSFER_SUCCESS==1])
mean(data_filtered_no_altruism$game.BUYER_BELIEVE[data_filtered_no_altruism$game.RANDOM_TRANSFER_SUCCESS==1])

t.test(data_filtered_no_altruism$game.SELLER_META_BELIEVE[data_filtered_no_altruism$game.RANDOM_TRANSFER_SUCCESS==1], data_filtered_no_altruism$game.BUYER_TRANSFER[data_filtered_no_altruism$game.RANDOM_TRANSFER_SUCCESS==1])
t.test(data_filtered_no_altruism$game.SELLER_META_BELIEVE[data_filtered_no_altruism$game.RANDOM_TRANSFER_SUCCESS==1], data_filtered_no_altruism$game.BUYER_BELIEVE[data_filtered_no_altruism$game.RANDOM_TRANSFER_SUCCESS==1])

mean(data_filtered_no_altruism$game.SELLER_META_BELIEVE[data_filtered_no_altruism$game.RANDOM_TRANSFER_SUCCESS==0])


mean(data_filtered_no_altruism$game.BUYER_TRANSFER[data_filtered_no_altruism$game.RANDOM_TRANSFER_SUCCESS==0])
mean(data_filtered_no_altruism$game.BUYER_BELIEVE[data_filtered_no_altruism$game.RANDOM_TRANSFER_SUCCESS==0])

t.test(data_filtered_no_altruism$game.SELLER_META_BELIEVE[data_filtered_no_altruism$game.RANDOM_TRANSFER_SUCCESS==0], data_filtered_no_altruism$game.BUYER_TRANSFER[data_filtered_no_altruism$game.RANDOM_TRANSFER_SUCCESS==0])
t.test(data_filtered_no_altruism$game.SELLER_META_BELIEVE[data_filtered_no_altruism$game.RANDOM_TRANSFER_SUCCESS==0], data_filtered_no_altruism$game.BUYER_BELIEVE[data_filtered_no_altruism$game.RANDOM_TRANSFER_SUCCESS==0])





# Overall main test
reg2 <- lm(game.SELLER_META_BELIEVE ~ game.BUYER_BELIEVE , data_filtered)
summary(reg2)
reg3 <- lm(game.SELLER_META_BELIEVE ~ game.BUYER_TRANSFER , data_filtered)
summary(reg3)






#### 
reg <- lm(game.SELLER_META_BELIEVE ~ game.BUYER_BELIEVE , data_filtered_no_altruism)
summary(reg)
reg <- lm(game.SELLER_META_BELIEVE ~ game.BUYER_TRANSFER , data_filtered_no_altruism)
summary(reg) ### so close
reg <- lm(game.SELLER_META_BELIEVE ~ game.BUYER_BELIEVE*game.RANDOM_TRANSFER_SUCCESS , data_filtered_no_altruism)
summary(reg)
reg <- lm(game.SELLER_META_BELIEVE ~ game.BUYER_TRANSFER*game.RANDOM_TRANSFER_SUCCESS , data_filtered_no_altruism)
summary(reg) #stat sig

ggplot(data_filtered_no_altruism, aes(x = game.SELLER_META_BELIEVE, y = game.BUYER_BELIEVE, shape = factor(game.RANDOM_TRANSFER_SUCCESS, labels = c("Telling the truth", "Lying"))))+
  geom_point() +
  geom_smooth(method=lm, se=FALSE, color = "gray") +
  xlab("Seller's Estimate of Buyer Believing") +
  ylab("Buyer's Actual Belief") +
  guides(shape=guide_legend(title="")) +
  theme_classic() +
  theme(legend.position ="top")



data_filtered_lying_only <- data_filtered_no_altruism[data_filtered_no_altruism$game.RANDOM_TRANSFER_SUCCESS==1,]
reg2 <- lm(game.SELLER_META_BELIEVE ~ game.BUYER_BELIEVE , data_filtered_lying_only)
summary(reg2)
reg3 <- lm(game.SELLER_META_BELIEVE ~ game.BUYER_TRANSFER , data_filtered_lying_only)
summary(reg3)







data_filtered_truth_only <- data_filtered_no_altruism[data_filtered_no_altruism$game.RANDOM_TRANSFER_SUCCESS==0,]
reg2 <- lm(game.SELLER_META_BELIEVE ~ game.BUYER_BELIEVE , data_filtered_truth_only)
summary(reg2)
reg3 <- lm(game.SELLER_META_BELIEVE ~ game.BUYER_TRANSFER , data_filtered_truth_only)
summary(reg3) #stat sig 





# Main test 2



reg <- lm(DETECTION_ABILITY_SCORE_1 ~ LIE_ABILITY_SCORE_1, pars)
summary(reg)

reg <- lm(DETECTION_ABILITY_SCORE_2 ~ LIE_ABILITY_SCORE_1, pars)
summary(reg)

reg <- lm(DETECTION_ABILITY_SCORE_1 ~ LIE_ABILITY_SCORE_2, pars)
summary(reg)

reg <- lm(DETECTION_ABILITY_SCORE_1 ~ LIE_ABILITY_SCORE_2, game)
summary(reg)



reg <- lm(data$seller.participant.ability_to_lie ~ data$game.BUYER_TRANSFER, data_filtered_no_altruism)
summary(reg)
reg <- lm(data$seller.participant.ability_to_lie ~ data$game.BUYER_BELIEVE, data_filtered_no_altruism)
summary(reg)

reg <- lm(data$buyer.participant.ability_to_detect_lies ~ data$game., data_filtered_no_altruism)
summary(reg)
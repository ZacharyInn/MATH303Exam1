#Zachary Inn
#MATH 303
#Exam 1

#PART A

#Q1
#using poisson
q1<-c(0:30)
prob1<-dpois(q1,lambda = 27)

print(1-sum(prob1))

#The probability that more than 30 cases of this rare disease are diagnosed
#in the state over the next year is about 0.2447, or ~24.47%.

################################################################################

#Q2
#using poisson
q2<-c(0:30)
prob2<-dpois(q2,lambda = 35)

print(1-sum(prob2))

#The probability that more than 30 cases of this rare disease are diagnosed
#in the state over the next year is about 0.7731, or ~77.31%.

################################################################################

#PART B

#Q3

print(pnorm(200000,315000,50000,TRUE) - pnorm(175000,315000,50000,TRUE))

#The proportion of homes available to buy within the $175,000 to $200,000 range
#is .0082 or 0.82%.

################################################################################

#Q4

print(qnorm(0.05,315000,50000,FALSE))

#The minimum value that a home can be worth and still be charged the premium
#is $397,242.70.

################################################################################

#Q5

#find z scores of both values

z1<-(289000-315000)/((50000/sqrt(15)))
z2<-(340000-315000)/((50000/sqrt(15)))

q5<-pnorm(z1,0,1,TRUE) + pnorm(z2,0,1,FALSE)

print(q5)

#In a sample of 15 residents, the probability their mean home value will be less
#than $289,000 or more than $340,000 is .0484 or 4.84%.

################################################################################

#PART C

#Q6

###############################################################
                 #              #             #               #
                 #      Inc     #     Dec     #     Total     #
                 #              #             #               #
###############################################################
                 #              #             #               #
#Food Processing #    .455      #     .195    #      .65      #
                 #              #             #               #
###############################################################
                 #              #             #               #
#Other           #     .0525    #     .2975   #      .35      #
                 #              #             #               #
###############################################################
                 #              #             #               #
#Total           #     .5075    #     .4925   #       1       #
                 #              #             #               #
###############################################################

print(.195/.4925)

#The probability that among the goods that have decreased in price, that the
#good was from food processing is .3959 or 39.59%.

################################################################################

#PART D

#Q7

summary(littletown$YRSWRK)
sd(littletown$YRSWRK)

# 131 - 15.0 (4.32), (2.5, 11.8, 14.9, 18.0, 28.5)

summary(littletown$AGESTR)
sd(littletown$AGESTR)

# 131 - 22.1 (2.50), (16.0, 20.5, 22.7, 24.0, 27.3)

agedxOnly <- data.frame(littletown$AGEDX)
agedxOnly <- na.omit(agedxOnly)
summary(agedxOnly$littletown.AGEDX)
sd(agedxOnly$littletown.AGEDX)

# 23 - 64.75 (5.34), (53.8, 62.9, 65.8, 67.5, 76.48)

################################################################################

#Q8

addmargins(table(littletown$SCH, littletown$DX))

#       0   1 Sum
# 0    62   8  70
# 1    25   7  32
# 2    21   8  29
# Sum 108  23 131

print(25/32)

#The probability that a person working the evening shift was also not diagnosed
#with the disease is about .7813 or 78.13%.

################################################################################

#Q9

#person working morning and diagnosed

print(8/70)

#person working evening and diagnosed

print(7/32)

#It is more likely that a person working the evening shift was diagnosed with 
#the disease as opposed to a person working the morning shift.

################################################################################

#Q10

boxplot(littletown$AGESTR[littletown$DX==0],littletown$AGESTR[littletown$DX==1],
        names = c("DX=0","DX=1"))

mean(littletown$AGESTR[littletown$DX==0])
mean(littletown$AGESTR[littletown$DX==1])

summary(littletown$AGESTR[littletown$DX==0])
summary(littletown$AGESTR[littletown$DX==1])

#Based on this box plot, I believe that there is some correlation to how young a
#person starts working at this plant and their chance to be diagnosed with this
#rare cancer. I believe this because the mean age of someone who is diagnosed 
#(21.66 y/o) is a bit younger than the mean age of someone who isn't diagnosed
#(22.24 y/o). Another form of statistical evidence that I base my claim on is
#that of those who are diagnosed, the maximum age is significantly younger 
#(24.20 y/o) than the maximum age of someone who isn't diagnosed (27.30 y/o).
#While one could argue that the minimum age of someone who isn't diagnosed 
#(16.00 y/o) is younger than the minimum age of someone who is diagnosed 
#(16.40 y/o), therefore my claim is invalid. My rebuttal would be that the Q1,
#median, Q3, and max of those diagnosed (20.15, 22.50, 23.45, 24.20) are all 
#younger than those who are not diagnosed (20.50, 22.90, 24.20, 27.30).

################################################################################














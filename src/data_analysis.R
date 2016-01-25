# Opis danych ze strony:
# VARIABLE DESCRIPTIONS:
#   survival        Survival
# (0 = No; 1 = Yes)
# pclass          Passenger Class
# (1 = 1st; 2 = 2nd; 3 = 3rd)
# name            Name
# sex             Sex
# age             Age
# sibsp           Number of Siblings/Spouses Aboard
# parch           Number of Parents/Children Aboard
# ticket          Ticket Number
# fare            Passenger Fare
# cabin           Cabin
# embarked        Port of Embarkation
# (C = Cherbourg; Q = Queenstown; S = Southampton)
# 
# SPECIAL NOTES:
#   Pclass is a proxy for socio-economic status (SES)
# 1st ~ Upper; 2nd ~ Middle; 3rd ~ Lower
# 
# Age is in Years; Fractional if Age less than One (1)
# If the Age is Estimated, it is in the form xx.5
# 
# With respect to the family relation variables (i.e. sibsp and parch)
# some relations were ignored.  The following are the definitions used
# for sibsp and parch.
# 
# Sibling:  Brother, Sister, Stepbrother, or Stepsister of Passenger Aboard Titanic
# Spouse:   Husband or Wife of Passenger Aboard Titanic (Mistresses and Fiances Ignored)
# Parent:   Mother or Father of Passenger Aboard Titanic
# Child:    Son, Daughter, Stepson, or Stepdaughter of Passenger Aboard Titanic
# 
# Other family relatives excluded from this study include cousins,
# nephews/nieces, aunts/uncles, and in-laws.  Some children travelled
# only with a nanny, therefore parch=0 for them.  As well, some
# travelled with very close friends or neighbors in a village, however,
# the definitions do not support such relations.

# nie potrafie zrobic ladnie sciezki wzglednej wiec tu trzeba skonfigurowac
TRAIN.DATA.DIR = "~/mine/projects/mow-titanic/src/data/train.csv"
train <- read.csv(TRAIN.DATA.DIR)

# ANALIZA ZMIENNYCH ############################################
########################## PassagerId ##########################
class(train$PassengerId) # integer - id nie analizujemy

########################## Survived ############################
class(train$Survived) # integer - znacznik, czy przezyl
table(train$Survived)
# 0   1 
# 549 342 

########################## Pclass ##############################
class(train$Pclass) #integer - klasa spoleczna (zmienna objasniajaca)
table(train$Pclass)
# 1   2   3 
# 216 184 491

sum(is.na(train$Pclass)) # nie ma brakow danych

mosaicplot(
  train$Pclass ~ train$Survived,
  main="Survived vs. Pclass",
  color=c("mediumpurple", "gold"),
  xlab="Pclass",
  ylab="Survived",
  off=c(5),
  cex.axis=1.2
)

########################## Name ###############################
class(train$Name) #factor - imie/nazwisko, mozna wyciagnac tytul (potem)

########################## Sex ################################
class(train$Sex) #factor - plec (zmienna objasniajaca)
table(train$Sex)
# female   male 
# 314    577 

sum(is.na(train$Sex)) # nie ma brakow danych

mosaicplot(
  train$Sex ~ train$Survived,
  main="Survived vs. Sex",
  color=c("mediumpurple", "gold"),
  xlab="Sex",
  ylab="Survived",
  off=c(5),
  cex.axis=1.2
)

######################### Age #################################
class(train$Age) # numeric - wiek (zmienna objasniajaca)
summary(train$Age)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0.42   20.12   28.00   29.70   38.00   80.00     177 


# SA BRAKI! - trzeba cos z tym zrobic (az 19%)
hist(
  train$Age,
  col = "mediumpurple",
  xlab = "Age",
  main = "Age histogram"
)

# Co zrobic? Najlepiej to uzupelnic:
# Pomysl 1 - uwzgledniajac rozklad dla plci (losowanie z gestosci lub np mediane)
# Pomysl 2 - uwzgledniajac rozklad dla tytulu ze zmiennej Name (losowanie z gestosci lub np mediane)

plot(
  train$PassengerId,
  train$Age,
  main="Survived vs. Age",
  ylab = "Age",
  xlab = "PassengerId"
)
points(
  train$PassengerId[which(train$Survived==1)],
  train$Age[which(train$Survived==1)],
  col = "mediumpurple",
  pch=21,
  bg = "gold"
)

######################### SibSp ###############################
class(train$SibSp) #integer - liczba rodzenstwa/malzonkow na pokladzie (zmienna objasniajaca)
table(train$SibSp)
# 0   1   2   3   4   5   8 
# 608 209  28  16  18   5   7

mosaicplot(
  train$SibSp ~ train$Survived,
  main="Survived vs. SibSp",
  color=c("mediumpurple", "gold"),
  xlab="SibSp",
  ylab="Survived",
  off=c(5),
  cex.axis=1.2
)

######################### Parch ###############################
class(train$Parch) #integer - liczba dzieci/rodzicow na pokladzie (zmienna objasniajaca)
table(train$Parch)
# 0   1   2   3   4   5   6 
# 678 118  80   5   4   5   1 

mosaicplot(
  train$Parch ~ train$Survived,
  main="Survived vs. Parch",
  color=c("mediumpurple", "gold"),
  xlab="Parch",
  ylab="Survived",
  off=c(5),
  cex.axis=1.2
)

######################### Ticket ##############################
class(train$Ticket) #factor - numer biletu - ROZWAZYC SPOSOB WLACZENIA DO ANALIZY
train$Ticket

######################### Fare ################################
class(train$Fare) #numeric - oplata (zmienna objasniajaca)
summary(train$Fare)
hist(train$Fare)
sum(train$Fare==0) # 15 osob z cena biletu 0 = traktowac jak brak danych
# Co zrobic? Najlepiej to uzupelnic:
# Pomysl 1 - uwzgledniajac rozklad dla klasy (zmienna Pclass) losowanie z gestosci lub np mediane)

plot(
  train$PassengerId,
  train$Fare,
  main="Survived vs. Fare",
  ylab="Fare",
  xlab="PassengerId"
)
points(
  train$PassengerId[which(train$Survived==1)],
  train$Fare[which(train$Survived==1)],
  col="mediumpurple",
  pch=21,
  bg="gold"
)

median(train$Fare[train$Pclass==1 & train$Fare>0])
median(train$Fare[train$Pclass==2 & train$Fare>0])
median(train$Fare[train$Pclass==3 & train$Fare>0])

plot(
  train$PassengerId,
  log(train$Fare),
  main="log(Fare) vs. Pclass",
  ylab="Fare",
  xlab="PassengerId"
)
points(
  train$PassengerId[which(train$Pclass==1 & train$Fare>0)],
  log(train$Fare[which(train$Pclass==1 & train$Fare>0)]),
  col="mediumpurple",
  pch=21,
  bg="red"
)
points(
  train$PassengerId[which(train$Pclass==2 & train$Fare>0)],
  log(train$Fare[which(train$Pclass==2 & train$Fare>0)]),
  col="mediumpurple",
  pch=21,
  bg="green"
)
points(
  train$PassengerId[which(train$Pclass==3 & train$Fare>0)],
  log(train$Fare[which(train$Pclass==3 & train$Fare>0)]),
  col="mediumpurple",
  pch=21,
  bg="blue"
)

######################### Cabin ###############################
class(train$Cabin) #factor - numer kabiny - ROZWAZYC SPOSOB WLACZENIA DO ANALIZY
train$Cabin
sum(train$Cabin=="") # 687 brakow

######################### Embarked ############################
class(train$Embarked) #factor - miejsce wsiascia/wysiascia (zmienna objasniajaca)
table(train$Embarked)
#         C   Q   S 
#     2 168  77 644 

mosaicplot(
  train$Embarked ~ train$Survived,
  main="Survived vs. Embarked",
  color=c("mediumpurple", "gold"), 
  xlab="Embarked",
  ylab="Survived",
  off=c(5),
  cex.axis=1.2
)

######################################
median(train$Fare[train$Pclass==1 & train$Fare>0])
median(train$Fare[train$Pclass==2 & train$Fare>0])
median(train$Fare[train$Pclass==3 & train$Fare>0])

plot(
  train$PassengerId,
  log(train$Fare),
  main="log(Fare) vs. Pclass",
  ylab="Fare",
  xlab="PassengerId"
)
points(
  train$PassengerId[which(train$Pclass==1 & train$Fare>0)],
  log(train$Fare[which(train$Pclass==1 & train$Fare>0)]),
  col="mediumpurple",
  pch=21,
  bg="red"
)
points(
  train$PassengerId[which(train$Pclass==2 & train$Fare>0)],
  log(train$Fare[which(train$Pclass==2 & train$Fare>0)]),
  col="mediumpurple",
  pch=21,
  bg="green"
)
points(
  train$PassengerId[which(train$Pclass==3 & train$Fare>0)],
  log(train$Fare[which(train$Pclass==3 & train$Fare>0)]),
  col="mediumpurple",
  pch=21,
  bg="blue"
)
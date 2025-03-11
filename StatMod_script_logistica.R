
rm(list = ls())

###########################
###########################
#### IMPORTAZIONE DATI ####
###########################
###########################

# Dataset relativo alla presenza di una malattia cardiaca relazionata a 13 covariate
# Fonte: https://archive.ics.uci.edu/dataset/45/heart+disease

# Variabili:
# 1. age (in years)
# 2. sex
#    - 0 = female
#    - 1 = male
# 3. cp - chest pain type
#    - 1 = typical angina 
#    - 2 = atypical angina 
#    - 3 = non-anginal pain 
#    - 4 = asymptomatic 
# 4. trestbps - resting blood pressure (in mm Hg on admission to the hospital) 
# 5. chol - serum cholestoral in mg/dl 
# 6. fbs - fasting blood sugar > 120 mg/dl
#    - 0 = false
#    - 1 = true
# 7. restecg - resting electrocardiographic results
#    - 0 = normal
#    - 1 = having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV) 
#    - 2 = showing probable or definite left ventricular hypertrophy by Estes' criteria 
# 8. thalach - maximum heart rate achieved 
# 9. exang - exercise induced angina
#    - 0 = no
#    - 1 = yes
# 10. oldpeak - ST depression induced by exercise relative to rest 
# 11. slope - the slope of the peak exercise ST segment 
#     - 1 = upsloping
#     - 2 = flat
#     - 3 = downsloping
# 12. ca - number of major vessels (0-3) colored by flourosopy 
# 13. thal
#     - 3 = normal
#     - 6 = fixed defect
#     - 7 = reversable defect 
# 14. num - number of heart disease (response variable)
#     - 0 = absence of heart disease
#     - 1,2,3,4 = presence of heart disease

#heart = read.table(file.choose(), header=F, sep=",", dec=".")
heart = read.table("/Users/lorenzo/Documents/Master/Moduli/Statistical Models/Progetto/heart.csv", header=F, sep=",", dec=".")
colnames(heart)=c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope", "ca", "thal","num")

#### Creazione variabile di risposta binaria ####
library(dplyr)
heart_bin = heart %>% mutate(resp=factor(ifelse(num>0,1,0)))
heart_bin = heart_bin[,-14] #eliminazione variabile "num"

#### Trasformazione delle variabili categoriche da numeriche a fattori ####
heart_bin$sex = factor(heart_bin$sex, levels=c(0,1), labels=c("F","M"))
heart_bin$cp = factor(heart_bin$cp, levels=c(1,2,3,4), labels=c("typical","atypical","no-anginal pain","asymptomatic"))
heart_bin$fbs = factor(heart_bin$fbs, levels=c(0,1), labels=c("no","si"))
heart_bin$restecg = factor(heart_bin$restecg, levels=c(0,1,2), labels=c("normal","abnormal","hypertrophy"))
heart_bin$exang = factor(heart_bin$exang, levels=c(0,1), labels=c("no","si"))
heart_bin$slope = factor(heart_bin$slope, levels=c(1,2,3), labels=c("upsloping","flat","downsloping"))
heart_bin$thal = factor(heart_bin$thal, levels=c(3,6,7), labels=c("normal","fixed","reversable"))
#attach(heart_bin)

head(heart_bin)
str(heart_bin)
sum(is.na(heart_bin)) #numero di missing

#############################
#############################
#### ANALISI PRELIMINARI ####
#############################
#############################

#### Variabili categoriche vs Risposta ####
heart_bin %>% group_by(sex, resp) %>% summarize(n=n()) %>% mutate(perc=round(n/sum(n),4))
heart_bin %>% group_by(cp, resp) %>% summarize(n=n()) %>% mutate(perc=round(n/sum(n),4))
heart_bin %>% group_by(fbs, resp) %>% summarize(n=n()) %>% mutate(perc=round(n/sum(n),4))
heart_bin %>% group_by(restecg, resp) %>% summarize(n=n()) %>% mutate(perc=round(n/sum(n),4))
heart_bin %>% group_by(exang, resp) %>% summarize(n=n()) %>% mutate(perc=round(n/sum(n),4))
heart_bin %>% group_by(slope, resp) %>% summarize(n=n()) %>% mutate(perc=round(n/sum(n),4))
heart_bin %>% group_by(thal, resp) %>% summarize(n=n()) %>% mutate(perc=round(n/sum(n),4))

# Stacked barplot
library(ggplot2)
library(scales)
data_stacked = heart_bin %>% group_by(sex, resp) %>% summarize(n=n()) %>% mutate(perc=round(n/sum(n),4), perc_l=scales::percent(perc))
g1 = ggplot(data_stacked, aes(x=sex, y=perc, fill=resp)) + 
  geom_bar(stat="identity", position="fill") +
  scale_y_continuous(breaks=seq(0, 1, .25), label=percent) +
  geom_text(aes(label=perc_l), size=2.8, position=position_stack(vjust=0.5)) +
  theme_minimal() +
  scale_fill_manual(values=c("0"="lightblue","1"="cornflowerblue")) +
  labs(y="Percentuale")
rm(data_stacked)

data_stacked = heart_bin %>% group_by(cp, resp) %>% summarize(n=n()) %>% mutate(perc=round(n/sum(n),4), perc_l=scales::percent(perc))
g2 = ggplot(data_stacked, aes(x=cp, y=perc, fill=resp)) + 
  geom_bar(stat="identity", position="fill") +
  scale_y_continuous(breaks=seq(0, 1, .25), label=percent) +
  geom_text(aes(label=perc_l), size=2.8, position=position_stack(vjust=0.5)) +
  theme_minimal() +
  scale_fill_manual(values=c("0"="lightblue","1"="cornflowerblue")) +
  labs(y="Percentuale")
rm(data_stacked)

data_stacked = heart_bin %>% group_by(fbs, resp) %>% summarize(n=n()) %>% mutate(perc=round(n/sum(n),4), perc_l=scales::percent(perc))
g3 = ggplot(data_stacked, aes(x=fbs, y=perc, fill=resp)) + 
  geom_bar(stat="identity", position="fill") +
  scale_y_continuous(breaks=seq(0, 1, .25), label=percent) +
  geom_text(aes(label=perc_l), size=2.8, position=position_stack(vjust=0.5)) +
  theme_minimal() +
  scale_fill_manual(values=c("0"="lightblue","1"="cornflowerblue")) +
  labs(y="Percentuale")
rm(data_stacked)

data_stacked = heart_bin %>% group_by(restecg, resp) %>% summarize(n=n()) %>% mutate(perc=round(n/sum(n),4), perc_l=scales::percent(perc))
g4 = ggplot(data_stacked, aes(x=restecg, y=perc, fill=resp)) + 
  geom_bar(stat="identity", position="fill") +
  scale_y_continuous(breaks=seq(0, 1, .25), label=percent) +
  geom_text(aes(label=perc_l), size=2.8, position=position_stack(vjust=0.5)) +
  theme_minimal() +
  scale_fill_manual(values=c("0"="lightblue","1"="cornflowerblue")) +
  labs(y="Percentuale")
rm(data_stacked)

data_stacked = heart_bin %>% group_by(exang, resp) %>% summarize(n=n()) %>% mutate(perc=round(n/sum(n),4), perc_l=scales::percent(perc))
g5 = ggplot(data_stacked, aes(x=exang, y=perc, fill=resp)) + 
  geom_bar(stat="identity", position="fill") +
  scale_y_continuous(breaks=seq(0, 1, .25), label=percent) +
  geom_text(aes(label=perc_l), size=2.8, position=position_stack(vjust=0.5)) +
  theme_minimal() +
  scale_fill_manual(values=c("0"="lightblue","1"="cornflowerblue")) +
  labs(y="Percentuale")
rm(data_stacked)

data_stacked = heart_bin %>% group_by(slope, resp) %>% summarize(n=n()) %>% mutate(perc=round(n/sum(n),4), perc_l=scales::percent(perc))
g6 = ggplot(data_stacked, aes(x=slope, y=perc, fill=resp)) + 
  geom_bar(stat="identity", position="fill") +
  scale_y_continuous(breaks=seq(0, 1, .25), label=percent) +
  geom_text(aes(label=perc_l), size=2.8, position=position_stack(vjust=0.5)) +
  theme_minimal() +
  scale_fill_manual(values=c("0"="lightblue","1"="cornflowerblue")) +
  labs(y="Percentuale")
rm(data_stacked)

data_stacked = heart_bin %>% group_by(thal, resp) %>% summarize(n=n()) %>% mutate(perc=round(n/sum(n),4), perc_l=scales::percent(perc))
g7 = ggplot(data_stacked, aes(x=thal, y=perc, fill=resp)) + 
  geom_bar(stat="identity", position="fill") +
  scale_y_continuous(breaks=seq(0, 1, .25), label=percent) +
  geom_text(aes(label=perc_l), size=2.8, position=position_stack(vjust=0.5)) +
  theme_minimal() +
  scale_fill_manual(values=c("0"="lightblue","1"="cornflowerblue")) +
  labs(y="Percentuale")
rm(data_stacked)

library(gridExtra)
grid.arrange(g1, g2, g3, g4, g5, g6, g7, nrow=4, ncol=2)
rm(g1, g2, g3, g4, g5, g6, g7)

#### Variabili quantitative vs Risposta ####
# Scatterplot
par(mfrow=c(3,2))
col_resp=c("blue","red")
plot(heart_bin$age, col=col_resp[heart_bin$resp], ylab="age", xlab="")
legend("topleft", title="resp", cex = 0.4, legend=levels(heart_bin$resp), fill=col_resp)
plot(heart_bin$trestbps, col=col_resp[heart_bin$resp], ylab="trestbps", xlab="")
legend("topleft", title="resp", cex = 0.4, legend=levels(heart_bin$resp), fill=col_resp)
plot(heart_bin$chol, col=col_resp[heart_bin$resp], ylab="chol", xlab="")
legend("topleft", title="resp", cex = 0.4, legend=levels(heart_bin$resp), fill=col_resp)
plot(heart_bin$thalach, col=col_resp[heart_bin$resp], ylab="thalach", xlab="")
legend("topleft", title="resp", cex = 0.4, legend=levels(heart_bin$resp), fill=col_resp)
plot(heart_bin$oldpeak, col=col_resp[heart_bin$resp], ylab="oldpeak", xlab="")
legend("topleft", title="resp", cex = 0.4, legend=levels(heart_bin$resp), fill=col_resp)
plot(heart_bin$ca, col=col_resp[heart_bin$resp], ylab="ca", xlab="")
legend("topleft", title="resp", cex = 0.4, legend=levels(heart_bin$resp), fill=col_resp)
rm(col_resp)

# Boxplot
g1 = ggplot(heart_bin, aes(x=resp, y=age)) +
  geom_boxplot(fill="lightblue", alpha=0.5, notch=F) +
  theme_minimal()
g2 = ggplot(heart_bin, aes(x=resp, y=chol)) +
  geom_boxplot(fill="lightblue", alpha=0.5, notch=F) +
  theme_minimal()
g3 = ggplot(heart_bin, aes(x=resp, y=trestbps)) +
  geom_boxplot(fill="lightblue", alpha=0.5, notch=F) +
  theme_minimal()
g4 = ggplot(heart_bin, aes(x=resp, y=thalach)) +
  geom_boxplot(fill="lightblue", alpha=0.5, notch=F) +
  theme_minimal()
g5 = ggplot(heart_bin, aes(x=resp, y=oldpeak)) +
  geom_boxplot(fill="lightblue", alpha=0.5, notch=F) +
  theme_minimal()
g6 = ggplot(heart_bin, aes(x=resp, y=ca)) +
  geom_boxplot(fill="lightblue", alpha=0.5, notch=F) +
  theme_minimal()
grid.arrange(g1, g2, g3, g4, g5, g6, nrow=3, ncol=2)
rm(g1, g2, g3, g4, g5, g6)

# Strip plot
g1 = ggplot(heart_bin, aes(y=resp, x=age)) +
  geom_point(color="cornflowerblue", alpha = 0.2) +
  theme_minimal()
g2 = ggplot(heart_bin, aes(y=resp, x=trestbps)) +
  geom_point(color="cornflowerblue", alpha = 0.2) +
  theme_minimal()
g3 = ggplot(heart_bin, aes(y=resp, x=chol)) +
  geom_point(color="cornflowerblue", alpha = 0.2) +
  theme_minimal()
g4 = ggplot(heart_bin, aes(y=resp, x=thalach)) +
  geom_point(color="cornflowerblue", alpha = 0.2) +
  theme_minimal()
g5 = ggplot(heart_bin, aes(y=resp, x=oldpeak)) +
  geom_point(color="cornflowerblue", alpha = 0.2) +
  theme_minimal()
g6 = ggplot(heart_bin, aes(y=resp, x=ca)) +
  geom_point(color="cornflowerblue", alpha = 0.2) +
  theme_minimal()
grid.arrange(g1, g2, g3, g4, g5, g6, nrow=3, ncol=2)
rm(g1, g2, g3, g4, g5, g6)

#### Correlazione variabili quantitative ####
library(ggcorrplot)
matrix_corrplot = round(cor(select_if(heart_bin, is.numeric), method="pearson"),4)
ggcorrplot(matrix_corrplot, hc.order=T, type="lower", lab=T)
rm(matrix_corrplot)

###########################
###########################
#### MODELLO LOGISTICO ####
###########################
###########################

#### Creazione nuove categorie per le variabili restecg e cp ####
heart_bin = heart_bin %>% mutate(restecg_class=factor(ifelse(restecg=="normal",0,1),labels=c("normal","hypertrophy")))
heart_bin = heart_bin %>% mutate(cp_class=factor(ifelse(cp=="asymptomatic",0,1),labels=c("asymptomatic","symptomatic")))

#### Selezione variabili esplicative ####
library(car)
# Selezione manuale - Step 1
mod1 = glm(resp~sex, family=binomial(link=logit), data=heart_bin)
summary(mod1)
#anova(mod1)

# Selezione manuale - Step 2
mod2 = glm(resp~sex+cp_class, family=binomial(link=logit), data=heart_bin)
summary(mod2)
anova(mod1, mod2, test='Chisq')
#Anova(mod2)
AIC(mod1,mod2)
BIC(mod1,mod2)

# Selezione manuale - Step 3
mod3 = glm(resp~sex+cp_class+fbs, family=binomial(link=logit), data=heart_bin)
summary(mod3)
anova(mod2, mod3, test='Chisq')
#Anova(mod3)
AIC(mod2,mod3)
BIC(mod2,mod3)

rm(mod1, mod2, mod3)

# Selezione manuale - Modello finale
mod_manual = glm(resp~sex+cp_class+thal+slope+oldpeak+ca+trestbps, family=binomial(link=logit), data=heart_bin)
summary(mod_manual)
#anova(mod_manual)
Anova(mod_manual)

# Selezione automatica - Modelli backward e forward
library(MASS)
mod_null = glm(resp~1, family=binomial(link=logit), data=heart_bin[,-c(3,7)])
#summary(mod_null)
mod_full = glm(resp~., family=binomial(link=logit), data=heart_bin[,-c(3,7)])
#summary(mod_full)

mod_backward = stepAIC(mod_full, direction="backward")
summary(mod_backward)

mod_forward = stepAIC(mod_null, direction="forward", scope=list(lower=mod_null,upper=mod_full))
summary(mod_forward) #uguale a mod_backward

rm(mod_null, mod_full)

# Confronto modelli di selezione manuale e automatica
anova(mod_manual, mod_backward, test='Chisq') #perché sono nested
#anova(mod_manual, mod_forward, test='Chisq')
AIC(mod_manual, mod_backward, mod_forward)
BIC(mod_manual, mod_backward, mod_forward)

# Interpretazione dei coefficienti e dei rispettivi IC
exp(cbind(coef(mod_manual), confint.default(mod_manual)))
exp(2*coef(mod_manual)[9]) #aumento di 2 vasi sanguigni colorati per la variabile "ca"

#library(epiDisplay)
#logistic.display(mod_manual)
library(sjPlot)
plot_model(mod_manual, sort.est = TRUE)

#### Valutazione del modello ####
# Valori previsti
previsti = predict(mod_manual, type="response")
#previsti = predict.glm(mod_manual, type="response")
#previsti = mod_manual$fitted
#previsti = fitted(mod_manual)
#istruzioni diverse per ottenere i medesimi valori previsti

# Analisi grafica dei valori previsti
par(mfrow=c(2,2))
plot(heart_bin$ca, previsti)
plot(heart_bin$oldpeak, previsti)
plot(heart_bin$trestbps, previsti)

# Analisi grafica dei residui
library(car)
residualPlots(mod_manual)

# Matrice di confusione
previsti0.5 = factor(ifelse(previsti<=0.5,0,1))
table(previsti0.5, heart_bin$resp)

library(caret)
confusionMatrix(previsti0.5, heart_bin$resp, positive="1")

# Curva ROC
library(pROC)
par(mfrow=c(1,1))
roc(heart_bin$resp~previsti, col="red", plot=T, print.auc=T, legacy.axes=T, main="Curva ROC")

#### Utilizzo del modello per nuovi pazienti ####
nuovo_paziente = data.frame(sex = "M",
                            cp_class = "asymptomatic",
                            trestbps = mean(heart_bin$trestbps),
                            oldpeak = mean(heart_bin$oldpeak),
                            slope = "upsloping",
                            ca = 3,
                            thal = "reversable")
predict(mod_manual, type="response", nuovo_paziente)

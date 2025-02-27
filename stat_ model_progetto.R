setwd("C:/Users/Roberto/Desktop/Statistical Models Progetto")

life_heart = read.csv(file="heart.csv", header=FALSE, sep=",")
library(dplyr)
library(ggplot2)
library(ggsci)
library(rgl)
# Variabili rilevate nel dataset
# 1. age 
# 2. sex 
# 3. cp: chest pain type 
# -- Value 1: typical angina 
# -- Value 2: atypical angina 
# -- Value 3: non-anginal pain 
# -- Value 4: asymptomatic 
# 4. trestbps: resting blood pressure (in mm Hg on admission to the hospital) 
# 5. chol: serum cholestoral in mg/dl 
# 6. fbs: (fasting blood sugar > 120 mg/dl) 
# -- Value 0: false
# -- Value 1: true
# 7. restecg: resting electrocardiographic results 
# -- Value 0: normal 
# -- Value 1: having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV) 
# -- Value 2: showing probable or definite left ventricular hypertrophy by Estes' criteria 
# 8. thalach: maximum heart rate achieved 
# 9. exang: exercise induced angina
# -- Value 0: no
# -- Value 1: yes
# 10. oldpeak: ST depression induced by exercise relative to rest 
# 11. slope: the slope of the peak exercise ST segment 
# -- Value 1: upsloping 
# -- Value 2: flat 
# -- Value 3: downsloping 
# 12. ca: number of major vessels (0-3) colored by flourosopy 
# 13. thal: 3 = normal; 6 = fixed defect; 7 = reversable defect 
# 14. num: number of heart disease (the predicted attribute)

#---------------------------------------------------------------------------------#
# 2) Aggiunta del nome delle colonne al dataframe:
names(life_heart) <- c("age", "sex", "cp", "trestbps", "chol","fbs", "restecg","thalach","exang", "oldpeak","slope", "ca", "thal", "num")

############### CHECK DELLA STRUTTURA DEL DATASET:

str(life_heart)

tail(life_heart)

#il dataset presenta delle variabili il cui tipo non è corretto. Variabili cateogirche quali il sesso infatti, sono trattate come interi. 
#Si rende necessaria una conversione in factor delle variabili categoriche in modo da poter iniziare una esplorazione corretta del database.




#######################CONTROLLO DEI VALORI MANCANTI. E' FONDAMENTALE CAPIRE SE TRA LE 14 COLONNE CI SIANO VALORI MANCANTI#####

which(is.na(life_heart)) 

sum(is.na(life_heart))    # sembra non ci siano valori mancanti nel dataset. 

##################### Conversione delle variabili in factor############ 
#usiamo la funzione unique per vedere i valori presi dalla variabile sesso




unique(life_heart$sex)  #funzione fondamentale per ispezionare variabili che risultano con tante categorie o valori 
                        #elimina inoltre i valori che si ripetono permettendo una identificazione piu chiara.

#Altra funzione esplorativa è la funzione distinct

distinct(life_heart,sex,cp,restecg,fbs,exang,slope,thal) #variabili qualitative 


unique(life_heart$cp)

head(life_heart$cp)




#ANALISI  variabilii CATEGORICHE:





#L'idea è di analizzare come si distribuisce il numero di infarti rispetto al sesso

unique(life_heart$sex)
head(life_heart$sex)
table(life_heart$sex)  #nel dataset sono presenti 206 uomini e 97 donne


ggplot(life_heart, aes(x = factor(sex,levels=c(1,0),labels = c("Maschio","Femmina")), fill = factor(num))) + 
  geom_bar(position = "stack") + 
  labs(title = "Distribuzione della malattia cardiaca per sesso", 
       x = "Sesso", y = "Proporzione") + 
  scale_fill_manual(values = c("0" = "#1f77b4",  # Blu
                               "1" = "#ff7f0e",  # Arancione
                               "2" = "#2ca02c",  # Verde
                               "3" = "#d62728",  # Rosso
                               "4" = "#9467bd")) +  # Viola
  theme_minimal()



#vediamo la distribuzione del numero di infarti rispetto al tipo di dolore 
ggplot(life_heart, aes(x = factor(cp, 
                                  levels = c(1, 2, 3, 4),  
                                  labels = c("Dolore toracico", 
                                             "Dolore toracico anomalo", 
                                             "Dolore toracico non dovuto a malattia cardiaca", 
                                             "Asintomatico")), 
                       fill = factor(num))) + 
  geom_bar(position = "stack") +  # Stack per le frequenze assolute
  labs(title = "Distribuzione della malattia cardiaca per tipo di dolore toracico", 
       x = "Tipo di dolore toracico", 
       y = "Frequenza assoluta", 
       fill = "Numero di infarti") + 
  scale_fill_manual(values = c("0" = "#1f77b4",  # Blu
                               "1" = "#ff7f0e",  # Arancione
                               "2" = "#2ca02c",  # Verde
                               "3" = "#d62728",  # Rosso
                               "4" = "#9467bd")) +  # Viola
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  facet_wrap(~ sex, labeller = labeller(sex = c("0" = "Donna", "1" = "Uomo")))  # Aggiungi la divisione per sesso




#GRAFICO 3D
colori=c("red","blue","yellow")
colori_scala <- rainbow(length(unique(colori)))[as.factor(colori)]

plot3d(life_heart$age,life_heart$sex,life_heart$num,col = colori_scala)





life_heart <- life_heart %>%
  mutate(sex_label = factor(sex, levels = c(1, 0), labels = c("Uomo", "Donna")))

# Grafico con facet_grid
ggplot(life_heart, aes(x = age, fill = factor(num))) +
  geom_histogram(binwidth = 5, color = "black", alpha = 0.7) +
  facet_grid(. ~ sex_label) +  # Divide il grafico per sesso
  labs(title = "Distribuzione dell'età per sesso e numero di infarti",
       x = "Età",
       y = "Conteggio",
       fill = "Numero di infarti") +
  scale_fill_manual(values = c("0" = "#1f77b4",  # Blu
                               "1" = "#ff7f0e",  # Arancione
                               "2" = "#2ca02c",  # Verde
                               "3" = "#d62728",  # Rosso
                               "4" = "#9467bd")) +  # Viola
  theme_minimal()




################################### PLOT DEL RISULTATO DELL'ECG RISPETTO AL SESSO.
#convertiamo la variabile in esame: 

life_heart$restecg=factor(life_heart$restecg, levels = c(0,1,2), labels=c("normale","anormale","ipertofia ventricolare"))



#Plottiamo quante persone hanno avuto un ecg normale quanti anormale e quanti invece hanno avuto ipertrofia ventricolare
ggplot(life_heart, aes(x = factor(restecg), fill = factor(sex))) +
  geom_bar(color = "black", position = position_dodge(preserve = "single")) +
  labs(title = "Distribuzione dei risultati ECG a riposo per sesso",
       x = "Tipologia di ECG a riposo",
       y = "Conteggio") +
  theme_minimal()


#Analizziamo il numero di infarti rispetto al valore della glicemia a digiuno:
#valore 1 indica un valore maggiore di 120 mlg, 0 invece un valore inferiore.

ggplot(life_heart, aes(x = factor(fbs, labels=c("<120 ",">120")), fill = factor(num))) + 
  geom_bar(position = "fill") + 
  labs(title = "Distribuzione del numero di infarti in base ai livelli di glicemia", 
       x = "Valori della glicemia a digiuno", y = "Proporzione") + 
  theme_minimal()












#VARIABILI QUANTITATIVE

#CALCOLO DELLA MATRICI DI CORRELAZIONE:

library(ggcorrplot)
df_corr=life_heart %>% dplyr::select(age,trestbps,chol,thalach,oldpeak)
sum(is.na(df_corr))
cor_matrix=cor(df_corr)
print(cor_matrix)


ggcorrplot(cor_matrix, 
           method = "square",  
           type = "lower", 
           hc.order = TRUE,
           lab = TRUE, 
           colors = c("blue", "white", "red"))+
       ggtitle("Matrice di Correlazione delle Variabili Quantitative") +
       theme(plot.title = element_text(hjust = 0.5))






#BOX PLOT PER OUTLIER E POSSIBILI SCOSTAMENTI DALLA NORMALE:


library(ggplot2)



ggplot(life_heart, aes(x = factor(sex, levels=c(0,1), labels = c("Maschio","Femmina")), y = chol, fill = factor(sex))) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 19) +
  labs(title = "Livelli di Colesterolo per Sesso", x = "Sesso", y = "Colesterolo (mg/dl)") +
  scale_x_discrete(labels = c("0" = "Uomo", "1" = "Donna")) +  # Rinomina le categorie
  scale_fill_manual(values = c("lightblue", "pink"), labels = c("Uomini", "Donne")) + 
  theme_minimal()
 


  
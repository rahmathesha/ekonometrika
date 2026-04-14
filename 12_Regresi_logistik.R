library(jtools)
ds = read.csv("kual_log.csv", stringsAsFactors = T)
str(ds)
ds$purchase = as.factor(ds$purchase)

# 1 Pengaruh Variabel Panjelas
# penjelas numerik
reg_num = glm(purchase ~ age, data = ds, 
              family =  binomial("link" = logit))
summ(reg_num)
exp(reg_num$coefficients) #OR

# penjelas kategorik
reg_cat = glm(purchase ~ gender, data = ds, 
              family =  binomial("link" = logit))
summ(reg_cat)
exp(reg_cat$coefficients)

# Model tanpa penjelas
reglog1 = glm(purchase ~ 1, ds, 
             family=binomial("link"=logit))
summ(reglog1)

# Model dengan penjelas
reglog = glm(purchase ~ .,ds, 
             family=binomial("link"=logit))
summ(reglog)

# Pemilihan model tanpa vs dengan penjelas
# LRT/ G: tanpa penjelas vs menggunakan penjelas (semua/pilihan)
reglog_H0 = reglog1
reglog_H1 = reglog
anova(reglog_H0, reglog_H1, test = "LRT") #test = Chisq
qchisq(.95, df= 3) # Tolak H0

#Wald
anova(reglog_H1, test = "LRT") #Chisq
coefficients(summary(reglog_H1)) #atau summary(reglog)
summ(reglog_H1)

# GoF
library(performance)
#H0: model fit vs H1: model tdk fit
performance_hosmer(reglog, n_bins = 10) 
r2_nagelkerke(reglog)

#odds ratio
exp(reglog_H1$coefficients)     

# Proporsi kelas respons
dugaan = round(fitted(reglog),3)
head(data.frame(ds$purchase,dugaan),10)

# Model dengan interaksi
# reglog2 = glm(purchase ~ gender: age + income + age, 
#               ds, family = "binomial")
# summ(reglog2)


# 2 Kemampuan Prediksi
set.seed (1001)
acak <- sample(1:nrow(ds), .75*nrow(ds))
train <- data.frame(ds)[acak,]
test <- data.frame(ds)[-acak,]

dim(train);dim(test)

reg_log = glm(purchase ~ ., train, 
             family=binomial("link"=logit))

summ(reg_log)
pred_rl = predict(reg_log, test, type = 'response')
prediksi = (cbind(test[,4], pred_rl, 
                  ifelse(pred_rl >= 0.5, '1', '0')))


guide <- table(test$purchase, prediksi[,3])
(akurasi <- (guide[1] + guide[4])/(sum(guide))) 
(spes <- (guide[1])/(guide[1]+guide[3])) 
(sensi <- (guide[4])/(guide[2] + guide[4])) 

library(caret)
confusionMatrix(guide, positive = "1")


# ROC: ROC-AUC
# tanpa variabel
reglog_1 <-glm(purchase~1 , train, family = "binomial"(link = logit))
summ(reglog_1)

library(pROC)
roc <- roc(purchase~fitted(reglog_1), data = train)
par(pty = "s") #m = maximum
plot(roc, print.thres="best", bty = 'l', 
     las = 1, col = "#377eb8", 
     lwd = 4, legacy.axes = T, auc.polygon = T)
plot.roc(purchase~fitted(reg_log), data = train, 
         add = T, col = "red", 
         lwd = 4,)
auc(roc)

#
























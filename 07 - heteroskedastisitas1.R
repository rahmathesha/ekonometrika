#Breush-Pagan-Gofrey Test
#data set BPtest.txt
ilus_1 = read.table("BPtest.txt", header = T)
step_1 = lm(Y~X1, data = ilus_1)
#plot(ilus_1$X1, residuals(step_1)^2, xlab = "X", ylab = "e^2")
sisaan = residuals(step_1)
JKS = deviance(step_1) #jumlah kuadrat sisaan (JKS) 
step_2 = JKS/nrow(ilus_1) #(e^2/n) -> MLE

step_3 = sisaan^2/step_2 #nilai pi

ilus_1a = data.frame(ilus_1,step_3)
step_4 = lm(step_3~X1, data = ilus_1a)

JKR = anova(step_4)[[2]][1]

step_5 = JKR/2
step_5 > qchisq(.95, df=1) #nilai tabel chi-square 5% 

#tolak H0 -> terjadi heteroskedastisitas
library(lmtest)
bptest(step_1) #perintah u/ uji BP 

# Heteroskedasticity 
library(readxl)
mesin = read_excel("mesin.xlsx")
str(mesin)

reg_1 = lm(produksi~., data = mesin)
summary(reg_1)

# Grafik sisaan
par(mfrow = c(1,3))
plot(fitted(reg_1), residuals(reg_1), xlab = "dugaan", ylab = 'sisaan',
     pch= 21, bg= "grey", las= 1)
abline(h = 0, col= "red", lwd= 2)
plot(mesin$biaya, residuals(reg_1), xlab = "X1", ylab = "sisaan",
     pch= 21, bg= "grey", las= 1)
abline(h = 0, col= "red", lwd= 2)
plot(fitted(reg_1), residuals(reg_1)^2, xlab = "dugaan", ylab = "sisaan_kuadrat", 
     pch= 21, bg= "grey", las= 1)

#Transformasi variabel respons 
mean_Y = (mean(mesin$produksi))
mean_Y_kuadrat = mean_Y^2 #lebih dekat dengan ragam sisaan
mean_Y_kubik = mean_Y^3

ragam_sisa = sd(residuals(reg_1))^2 #karena nilai Y ada yg 0, maka menggunakan transf log(Y+1)

mean_Y; mean_Y_kuadrat; mean_Y_kubik; ragam_sisa

logY_1 = log(mesin$produksi) 

mesin1 = data.frame(mesin, logY_1)
head(mesin1)
reg_1a = lm(logY_1 ~ biaya, data = mesin1)
summary(reg_1a)

library(jtools)
summ(reg_1a, digits = 4)

par(mfrow = c(1,2))
plot(fitted(reg_1a), residuals(reg_1a), xlab = "dugaan", ylab = 'sisaan',
     pch= 21, bg= "grey", las= 1)
abline(h=0, col= "red", lwd= 2)
plot(fitted(reg_1a), residuals(reg_1a)^2, xlab = "dugaan", ylab = 'sisaan_kuadrat',
     pch= 21, bg= "grey", las= 1)


library(lmtest)
bptest(reg_1)   #ragam sisaan tidak homogen
bptest(reg_1a)  #ragam sisaan homogen


#2. WLS dengan data set mesin.xlsx
mesin_wls = mesin
mesin_wls$mean_Y = rep(mean_Y, nrow(mesin_wls))
wls_1 = lm(produksi~biaya, weights = mean_Y, 
           data = mesin_wls)
summ(wls_1, digits = 4)

plot(fitted(wls_1), residuals(wls_1),
     main = "Hasil WLS dengan pembobot 1/E(X)",
     xlab = "dugaan", ylab = 'sisaan',
     pch= 21, bg= "grey", las= 1)
abline(h=0, col= "red", lwd= 2)
bptest(wls_1)

# Jika 1/(sisaan^2) sebagai pembobot
mesin_wls$s2 = 1/(residuals(reg_1)^2)
wls_s2 = lm(produksi~biaya, weights = s2, 
           data = mesin_wls)
summ(wls_s2, digits = 4)

plot(fitted(wls_s2), residuals(wls_s2),
     main = "Hasil WLS dengan pembobot 1/sisaan^2",
     xlab = "dugaan", ylab = 'sisaan',
     pch= 21, bg= "grey", las= 1)
abline(h=0, col= "red", lwd= 2)
bptest(wls_s2)

#3. White's HC
library(car)
cov1 <- hccm(reg_1, type = "hc0")
model_hc <- coeftest(reg_1, vcov. = cov1)

#atau
summ(reg_1, digits = 4, robust = "HC0") # HC0 -> White HC

#
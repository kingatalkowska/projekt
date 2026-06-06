library(AER)
library(plm)
library(lmtest)
library(sandwich)
library(tseries)
library(car)
library(skedastic)

head(CASchools)

model_1 = lm(math ~ I(students/teachers) + income, data = CASchools)
model_1
summary(model_1)
#Zmienna stosunku studentów do nauczycieli jest nieistotna statystycznie

## Normalność rozkładu reszt
jarque.bera.test(model_1$residuals)  
## p-value = 0.7809 NIE MA PODSTAW DO ODRZUCENIA H0 O NORMALNOŚCI ROZKŁADU RESZT

### badanie homoskedastyczności ----
white(mainlm = model_1, interactions = TRUE) 
## 0.00221 ODRZUCAMY H0, WYSTĘPUJE HETEROSKEDASTYCZNOŚĆ RESZT

coeftest(model_1, vcov. = vcovHC, type = "HC3")

model_2 = lm(math ~ I(students/teachers) + log(income), data = CASchools)
model_2
summary(model_2)
#Zmienna stosunku studentów do nauczycieli jest nieistotna statystycznie
#Wraz ze wzrostem o 1 jednostkę dochodu nastepuje średni wzrost o 3,4917 p.p. zmiennej math


model_3 = lm(math ~ I(students/teachers) + income + I(income^2), data = CASchools)
model_3
summary(model_3)

vif(model_3)
# WYSTEPUJE WSPÓŁLINIOWOŚĆ POMIĘDZY INCOME I INCOME DO KWADRATU, 
# NO ALE TO JEST TA SAMA ZMIENNA PODNIESIONA DO KWADRATU (współliniowość strukturalna)

data("Cigar")
head(Cigar)
ramkaCIGAR = pdata.frame(Cigar, index = c("state", "year"))
model_4 = plm(sales ~ price + ndi,
    data = ramkaCIGAR,
    model = "within")
summary(model_4)
# Wraz ze wzrostem price o jedną jednostkę maleje sales o 0,4128 jednostki
# przy efektach stałych dla jednostek, ceteris paribus

model_5 = plm(sales ~ price + ndi,
    data = ramkaCIGAR,
    model = "within",
    effect = "twoways")

summary(model_5)
# Wraz ze wzrostem price o jedną jednostkę maleje sales o 0.8232 jednostki
# przy efektach stałych dla jednostek ORAZ CZASU, ceteris paribus


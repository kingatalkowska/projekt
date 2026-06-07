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

data("PSID1976")
head(PSID1976)
PSID1976$binarna <- ifelse(PSID1976$participation == "yes", 1, 0)
model_6 <- glm(binarna ~ age + education + youngkids,
    data = PSID1976,
    family = binomial(link = "probit"))
summary(model_6)
# Youngkids istotna statystycznie, wraz ze wzrostem younkids o 1 jednostkę,
# prawdopodobieństwo uczesticzenia w rynku pracy maleje 0 0,86 p.p., ceteris paribus

ramka_danych_6 <- data.frame(
    age = 30,
    education = 12,
    youngkids = 1
) 
prognoza <- predict(model_6, newdata = ramka_danych_6, type = "response")

# 0.4650085 Zgodnie z oszacowanym modelem probitowym, 
# 30-letnia kobieta z 12-letnim okresem edukacji i małym dzieckiem ma około 46,5% szans na uczestnictwo w rynku pracy.

library(datasets)
data("JohnsonJohnson")
head(JohnsonJohnson)
names(JohnsonJohnson)
class(JohnsonJohnson)

ramka_danych_7 <- data.frame(
    y = as.numeric(JohnsonJohnson),
    czas = 1:length(JohnsonJohnson),
    kwartal = as.factor(cycle(JohnsonJohnson))
)

summary(ramka_danych_7)
View(ramka_danych_7)

model_7 <- lm(log(y) ~ kwartal + czas, data=ramka_danych_7)
summary(model_7)
# Z każdym kolejnym kwartałem zysk firmy średnio rośnie o 4,18%,
# ceteris paribus. Rosnący trend wykładniczy

# Brak istotności statystycznej kwartał2
# Zyski w trzecim kwartale są średnio o 9,82% wyższe niż w kwartale pierwszym.
# Zyski w czwartym kwartale są średnio o 17,05% niższe niż w kwartale pierwszym.

bgtest(model_7, order = 4)
# Hipoteza zerowa (H0): Brak autokorelacji reszt rzędu 4
# p MNIEJSZE OD 0.05 - odrzucamy H0, występuje silna, statystycznie istotna autokorelacja reszt.
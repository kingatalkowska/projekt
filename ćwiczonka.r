library(AER)
library(plm)
library(lmtest)
library(sandwich)
library(tseries)
library(car)
library(skedastic)

data("Guns")
model_Guns = lm(log(violent)~ log(income) + prisoners + law, data = Guns)
summary(model_Guns)

#Wszystkie zmienne niezależne sa istotne statystycznie. Wraz ze wzrostem o jedna jednostkę zmiennej prisoners,
średnio rośnie zmienna zależna violent o 1.880e-03%%, ceteris paribus.
#Wraz ze wzrostem o jedna jednostkę zmiennej lawyes,
średnio maleje zmienna zależna violent o 4.815e-01 %%, ceteris paribus.
#Wraz ze wzrostem o 1 % zmiennej income,
średnio rośnie zmienna zależna violent o 5.060e-01 %% , ceteris paribus.

library(AER)
data("Affairs")

# 1. Utworzenie czystej zmiennej binarnej: 1 jeśli zdradził/a (affairs > 0), 0 jeśli nie
Affairs$zdrada <- ifelse(Affairs$affairs > 0, 1, 0)

# 2. Oszacowanie modelu Logitowego
model_zdrada <- glm(zdrada ~ yearsmarried + religiousness + rating, 
                    data = Affairs, 
                    family = binomial(link = "logit"))
summary(model_zdrada)

# 3. Wyliczenie prawdopodobieństwa zdrady dla konkretnego profilu
osoba <- data.frame(yearsmarried = 10, religiousness = 2, rating = 2)
predict(model_zdrada, newdata = osoba, type = "response")

# Pytania do Zadania 2:
# A) Jaki wpływ na szansę zdrady ma ocena szczęścia w małżeństwie (rating)? Zinterpretuj kierunek zależności.
# B) Czy zmienna "religiousness" (poziom religijności od 1 do 5) jest statystycznie istotna? Co oznacza p-value na poziomie 2.6e-05?
# C) Jak zinterpretujesz wynik wyliczony przez funkcję predict() dla przygotowanego profilu?
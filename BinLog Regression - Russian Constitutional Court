# Hastily written and a bit naive code for binary logistic regression.
# Regression model treat as an independent variable Russian Constitutional Court's decisions on political issues regarding human rights. 
# Sample of the decisions is manually coded.

library(readxl)
courts <- read_excel("/Users/viktorledenev/Desktop/EU - courses/Courts/Final/KSRF_database_short.xlsx")
library(lubridate)
library(dplyr)
library(ggplot2)
library(naniar)
library(sjPlot)


vis_miss(courts) # миссингов нет

courts$date_year <- format(as.Date(courts$date), "%Y")
courts$date_month <- month(courts$date)
courts$date_half_year <- ifelse(courts$date_month <= 6, "Half 1", "Half 2")
halves <- character()
for (i in 1:length(courts$date_year)) {
  new_string <- paste0(courts$date_year[i], courts$date_half_year[i])
  halves <- c(halves, new_string)
}
courts$halves <- halves

courts$elections <- dplyr::recode(courts$date_year,
                           "2011" = "1",
                           "2012" = "1",
                           "2013" = "0",
                           "2014" = "0",
                           "2015" = "0",
                           "2016" = "1",
                           "2017" = "0", 
                           "2018" = "1",
                           "2019" = "0",
                           "2020" = "0",
                           "2021" = "1",
                           "2022" = "0",
                           "2023" = "0")
# GD - 2021, 2016, 2011
# President - 2012, 2018

courts$elections_GD <- dplyr::recode(courts$date_year,
                           "2011" = "1",
                           "2012" = "0",
                           "2013" = "0",
                           "2014" = "0",
                           "2015" = "0",
                           "2016" = "1",
                           "2017" = "0", 
                           "2018" = "0",
                           "2019" = "0",
                           "2020" = "0",
                           "2021" = "1",
                           "2022" = "0",
                           "2023" = "0")

courts$elections_pres <- dplyr::recode(courts$date_year,
                           "2011" = "0",
                           "2012" = "1",
                           "2013" = "0",
                           "2014" = "0",
                           "2015" = "0",
                           "2016" = "0",
                           "2017" = "0", 
                           "2018" = "1",
                           "2019" = "0",
                           "2020" = "0",
                           "2021" = "0",
                           "2022" = "0",
                           "2023" = "0")

# Opredeleniya
# 2011 - 1990
# 2012 - 2557
# 2013 - 2278
# 2014 - 3085
# 2015 - 3111
# 2016 - 2888
# 2017 - 3197
# 2018 - 3489
# 2019 - 3640
# 2020 - 3154
# 2021 - 3077
# 2022 - 3690
# 2023 - 836

courts$opred_year <- dplyr::recode(courts$date_year,
                           "2011" = "1990",
                           "2012" = "2557",
                           "2013" = "2278",
                           "2014" = "3085",
                           "2015" = "3111",
                           "2016" = "2888",
                           "2017" = "3197", 
                           "2018" = "3489",
                           "2019" = "3640",
                           "2020" = "3154",
                           "2021" = "3077",
                           "2022" = "3690",
                           "2023" = "836")

# Postanov
# 2011 - 30
# 2012 - 34
# 2013 - 30
# 2014 - 33
# 2015 - 34
# 2016 - 28
# 2017 - 40
# 2018 - 47
# 2019 - 41
# 2020 - 50
# 2021 - 55
# 2022 - 59
# 2023 - 22

courts$post_year <- dplyr::recode(courts$date_year,
                            "2011" = "30",
                            "2012" = "34",
                            "2013" = "30",
                            "2014" = "33",
                            "2015" = "34",
                            "2016" = "28",
                            "2017" = "40", 
                            "2018" = "47",
                            "2019" = "41",
                            "2020" = "50",
                            "2021" = "55",
                            "2022" = "59",
                            "2023" = "22")

courts$opred_year <- as.numeric(courts$opred_year)
courts$post_year <- as.numeric(courts$post_year)

courts <- courts |> group_by(date_year) |> 
  mutate (decis_year = opred_year + post_year)

ggplot(courts, aes (x = date_year))+
  geom_bar() +
  geom_line(aes(y = decis_year))

unique(courts$decis_year)


lapply(courts, class)
# courts$date_year <- as.factor(courts$date_year)
courts$form <- as.factor(courts$form)
courts$plaintiff <- as.factor(courts$plaintiff)
courts$amicus_curia <- as.factor(courts$amicus)
courts$contra_regime <- as.factor(courts$contra_regime)
courts$relaxing_actions <- as.factor(courts$relaxing_actions)
courts$dissenters <- as.factor(courts$dissenters)
courts$key_decision <- as.factor(courts$key_decision)
courts$dokladchik <- as.factor(courts$dokladchik)
courts$regional_legislation <- as.factor(courts$regional_legislation)
courts$area <- as.factor(courts$area)
courts$area_short <- as.factor(courts$area_short)
courts$ordinary <- as.factor(courts$ordinary)
courts$date_year <- as.numeric(courts$date_year)
courts$date_month <- as.numeric(courts$date_month)
courts$elections <- as.factor(courts$elections)



courts$form_bin <- dplyr::recode(courts$form,
                          "0" = "0",
                          "1" = "1",
                          "2" = "1") 
courts$plaintiff_cit <- ifelse(courts$plaintiff == "Citizen", "Citizen", "Not a citizen")
courts$plaintiff_cit <- as.factor(courts$plaintiff_cit)
courts$plaintiff_polit <- dplyr::recode(courts$plaintiff,
                                 "Citizen, Politician" = "Politician",
                                 "Politician" = "Politician",
                                 "Ombudsman" = "Politician", 
                                 "Ombudsman, NGO, Citizen" = "Politician",
                                 "MinJust" = "Politician", 
                                 "President" = "Politician",
                                 "Citizen" = "Not a politician",
                                 "Citizen, NGO" = "Not a politician", 
                                 "NGO" = "Not a politician",
                                 "NGO, Citizen" = "Not a politician", 
                                 "Business" = "Not a politician",
                                 "Citizen, business" = "Not a politician",
                                 "Court" = "Not a politician") 
courts$plaintiff_polit <- as.factor(courts$plaintiff_polit)
courts$contra_regime_bin <- ifelse(courts$contra_regime == "0", "0", "1")
courts$contra_regime_bin <- as.factor(courts$contra_regime_bin)
courts$relaxing_actions_bin <- as.factor(ifelse(courts$relaxing_actions == "0", "0", "1"))
courts$dissenters_bin <- as.factor(ifelse(courts$dissenters == "No", "0", "1"))
courts$representatives_bin <- as.factor(ifelse(courts$representatives == "No", "0", "1"))
courts$dokladchik_bin <- as.factor(ifelse(courts$dokladchik == "No", "0", "1"))


courts <- courts |> group_by(date_year) |> 
  mutate (pol_decis_year = n())



# Распределение во времени
ggplot(courts, aes (x = halves))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90))

ggplot(courts, aes (x = date_year))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_continuous(breaks = unique(courts$date_year))

# z-standardization 
a <- courts |> filter (date_year != 2023)
sd1 <- sd(a$pol_decis_year) # sd = 6.45974
mean1 <- mean(a$pol_decis_year) # 17.01227

courts <- courts |> group_by(date_year) |> 
  mutate(sd_year = (pol_decis_year - mean1) / sd1)

courts |> 
  filter (sd_year > 1.96 | sd_year < -1.96) |> 
  pull(date_year) 
# 2011, 2023 - отклоняющиеся года


# 1. ПРЕДИКТОРЫ РЕШЕНИЯ ЗА ИЛИ ПРОТИВ РЕЖИМА

# 1.1. РАСПРЕДЕЛЕНИЕ ВО ВРЕМЕНИ РЕШЕНИЙ ПО ПОДДЕРЖКЕ РЕЖИМА

ggplot(courts, aes (x = date_year, fill = contra_regime_bin))+
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(breaks = unique(courts$date_year))

library(gmodels)
#CrossTable(as.factor(courts$date_year), courts$contra_regime_bin, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS", simulate.p.value=TRUE)
# слишком мало данных

mod_year<- glm (contra_regime_bin~date_year,courts,family="binomial")

tab_model(mod_year,
          show.ci = T,
          show.r2 = T,
          show.aic = T,
          show.loglik = T,
          dv.labels = c("Year"))

courts$date_year_simplified <- courts$date_year-2000

mod_year_simpl<- glm (contra_regime_bin~date_year_simplified,courts,family="binomial")

tab_model(mod_year_simpl,
          show.ci = T,
          show.r2 = T,
          show.aic = T,
          show.loglik = T,
          dv.labels = c("Year"))


library(effects)
m1.eff<-allEffects(mod_year)
m1.eff

mod0<-glm(contra_regime_bin~1,courts,family="binomial")
library(rcompanion)
nagelkerke(mod_year, null = mod0) 
#McFadden                            0.0381242
#Cox and Snell (ML)                  0.0311287
#Nagelkerke (Cragg and Uhler)        0.0552195

#library(generalhoslem)
#logitgof(courts$contra_regime_bin, fitted(mod_year), g = 2) # This test will not be valid where there is only one or two categorical predictor variables. 

courts$yhat<-predict(mod_year, courts)
residuals(mod_year)

courts$yhat1<-ifelse(courts$yhat < 0.5, 0,1)

library(pROC)
f1 = roc(courts$contra_regime_bin, courts$yhat1)
plot(f1, col="red")
# extremely poor model


# 1.2. ВЛИЯНИЕ ОРДИНАРНОСТИ ЗАКОНОДАТЕЛЬСТВА

ggplot(courts, aes (x = ordinary, fill = contra_regime_bin))+
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))

CrossTable(courts$ordinary, courts$contra_regime_bin, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# Chi^2 =  4.827128     d.f. =  1     p =  0.02801527
# Pearson's Chi-squared test with Yates' continuity correction 
# Chi^2 =  3.792387     d.f. =  1     p =  0.05148618 
# Field: Although this seems like a nice solution to the problem there is a fair bit of evidence that this overcorrects and produces chi-square values that are too small! => 
# Can assume test is significant
# Однако нет статистически значимых остатков.
# Alternative hypothesis: true odds ratio is not equal to 1
# p =  0.02649755 

mod_ordinary <- glm (contra_regime_bin~ordinary,courts,family="binomial")

tab_model(mod_ordinary,
          show.ci = T,
          show.r2 = T,
          show.aic = T,
          show.loglik = T,
          dv.labels = c("Ordinary"))
# Значимый негативный эффект ординарности

m2.eff<-allEffects(mod_ordinary)
m2.eff

# ordinary
# 0          1 
# 0.18181818 0.04545455 

nagelkerke(mod_ordinary, null = mod0)
# McFadden                            0.0427543
# Cox and Snell (ML)                  0.0348426
# Nagelkerke (Cragg and Uhler)        0.0618076


# 1.3. ВЛИЯНИЕ НА РЕШЕНИЕ ФИГУРЫ ЗАЯВИТЕЛЯ

ggplot(courts, aes (x = plaintiff_cit, fill = contra_regime_bin))+
  geom_bar(position = "fill")

ggplot(courts, aes (x = plaintiff_polit, fill = contra_regime_bin))+
  geom_bar(position = "fill")
# Неграждане относительно других категорий менее успешны
# Неполитики относительно политиков менее успешны

# граждане vs неграждане
CrossTable(courts$plaintiff_cit, courts$contra_regime_bin, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# Chi^2 =  5.977944     d.f. =  1     p =  0.01448588 
# Yates
# Chi^2 =  4.75358     d.f. =  1     p =  0.02923741 
# residuals are insignificant

mod_citizen <- glm (contra_regime_bin~plaintiff_cit,courts,family="binomial")

tab_model(mod_citizen,
          show.ci = T,
          show.r2 = T,
          show.aic = T,
          show.loglik = T,
          dv.labels = c("Citizen as Plaintiff"))
# Значимый негативный эффект гражданина как заявителя

m3.1.eff<-allEffects(mod_citizen)
m3.1.eff

# ordinary
# Citizen Not a citizen 
# 0.1093750     0.2702703

nagelkerke(mod_citizen, null = mod0)
#McFadden                            0.0388061
#Cox and Snell (ML)                  0.0316766
#Nagelkerke (Cragg and Uhler)        0.0561914

# политики vs неполитики
CrossTable(courts$plaintiff_polit, courts$contra_regime_bin, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# Fisher is significant: 0.01685552 

mod_politician <- glm (contra_regime_bin~plaintiff_polit,courts,family="binomial")

tab_model(mod_politician,
          show.ci = T,
          show.r2 = T,
          show.aic = T,
          show.loglik = T,
          dv.labels = c("Politician as Plaintiff"))
# Значимый негативный эффект неполитика как заявителя

m3.2.eff<-allEffects(mod_politician)
m3.2.eff

#plaintiff_polit
#Not a politician       Politician 
#0.1241830        0.4166667 

nagelkerke(mod_politician, null = mod0)
#McFadden                            0.0420870
#Cox and Snell (ML)                  0.0343082
#Nagelkerke (Cragg and Uhler)        0.0608597


# 1.4. ВЛИЯНИЕ НА РЕШЕНИЕ НАЛИЧИЕ НЕСОГЛАСНЫХ

ggplot(courts, aes (x = dissenters_bin, fill = contra_regime_bin))+
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))

table(courts$dissenters)
# Aranovsky - 5
# Yaroslavtsev - 5
# Kazantsev - 3

CrossTable(courts$dissenters_bin, courts$contra_regime_bin, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# Fisher is significant: 0.001455181

mod_dissent <- glm (contra_regime_bin~dissenters_bin,courts,family="binomial")

tab_model(mod_dissent,
          show.ci = T,
          show.r2 = T,
          show.aic = T,
          show.loglik = T,
          dv.labels = c("Dissent"))
# Значимый позитивный эффект наличия несогласного судьи

m4.eff<-allEffects(mod_dissent)
m4.eff
# dissenters_bin
# 0         1 
# 0.1168831 0.5454545 


nagelkerke(mod_dissent, null = mod0)
#McFadden                            0.0776022
#Cox and Snell (ML)                  0.0623419
#Nagelkerke (Cragg and Uhler)        0.1105890

# 1.5. ВЛИЯНИЕ НА РЕШЕНИЕ ГОДА ВЫБОРОВ

# ВЛИЯНИЕ НА РЕШЕНИЕ НАЛИЧИЯ ГОЛОСОВАНИЯ

ggplot(courts, aes (x = elections, fill = contra_regime_bin))+
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))

ggplot(courts, aes (x = elections_GD, fill = contra_regime_bin))+
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))

ggplot(courts, aes (x = elections_pres, fill = contra_regime_bin))+
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))

# При выборах в ГД ситуация противоположная гипотезе

CrossTable(courts$elections, courts$contra_regime_bin, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# chi-square is insignificant: p =  0.4845092, p =  0.6531423

CrossTable(courts$elections_GD, courts$contra_regime_bin, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# fisher is insignificant

CrossTable(courts$elections_pres, courts$contra_regime_bin, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
# fisher is insignificant

mod_elections <- glm (contra_regime_bin~elections,courts,family="binomial")

tab_model(mod_elections,
          show.ci = T,
          show.r2 = T,
          show.aic = T,
          show.loglik = T,
          dv.labels = c("Elections"))
# insignificant


# 2 Бинарная логистическая модель



mod_year_simpl<- glm (contra_regime_bin~date_year_simplified,courts,family="binomial")
mod_ordinary <- glm (contra_regime_bin~ordinary,courts,family="binomial")
mod_citizen <- glm (contra_regime_bin~plaintiff_cit,courts,family="binomial")
mod_politician <- glm (contra_regime_bin~plaintiff_polit,courts,family="binomial")
mod_dissent <- glm (contra_regime_bin~dissenters_bin,courts,family="binomial")
mod_elections <- glm (contra_regime_bin~elections,courts,family="binomial")


tab_model(mod_year_simpl, mod_ordinary, mod_citizen, mod_politician, mod_dissent, mod_elections,
          show.ci = T,
          show.r2 = T,
          show.aic = T,
          show.loglik = T,
          dv.labels = c("Year", "Ordinariness", "Citizen", "Politician", "Dissent", "Elections"))


# 2.1. Nested models

mod1 <- glm (contra_regime_bin~date_year_simplified,courts,family="binomial")
mod2 <- glm (contra_regime_bin~date_year_simplified + ordinary,courts,family="binomial")
mod3 <- glm (contra_regime_bin~date_year_simplified + ordinary+plaintiff_cit,courts,family="binomial")
mod4 <- glm (contra_regime_bin~date_year_simplified + ordinary+plaintiff_cit+dissenters_bin,courts,family="binomial")
mod5 <- glm (contra_regime_bin~date_year_simplified + ordinary+plaintiff_cit+dissenters_bin+elections,courts,family="binomial")

tab_model(mod1, mod2, mod3, mod4, mod5,
          show.ci = T,
          show.r2 = T,
          show.aic = T,
          show.loglik = T,
          dv.labels = c("Year", "Ordinariness", "Plaintiff is Citizen", "Dissenters", "Elections"))

nagelkerke(mod1, null = mod0) # 0.0552195
nagelkerke(mod2, null = mod0) # 0.1390740
nagelkerke(mod3, null = mod0) # 0.188555
nagelkerke(mod4, null = mod0) # 0.257160, McFadden - 0.1888114
nagelkerke(mod5, null = mod0) # 0.259515

anova(mod1, mod2, test = "Chisq")
anova(mod2, mod3, test = "Chisq")
anova(mod3, mod4, test = "Chisq")
anova(mod4, mod5, test = "Chisq") 
# Mod5 единственная незначимая - отбрасываем выборы = > работаем с моделью 4.
# Аналогично с AIC. LogLik в пользу модели 5, но он всегда свидетельствует об улучшении модели. Изменение pseudo r^2 минимальное для модели 5. Кроме того, добавление года выборов не дает нам какое-либо изменение в остальных переменных.

tab_model(mod1, mod2, mod3, mod4, mod5,
                 show.ci = F,
                 show.r2 = T,
                 show.aic = T,
                 show.loglik = T,
                 show.se = T, 
                 dv.labels = c("Year", "Ordinariness", "Plaintiff is not a citizen", "Dissent opinions", "Elections"))


plot_model(mod4,ci_method="wald", show.values = TRUE)

eff<-allEffects(mod4)
eff

# Диагностика

library(car) 
vif(mod4) # ок

car::residualPlots(mod4)

library(generalhoslem)
logitgof(courts$contra_regime_bin, fitted(mod4), g = 6) # p-value выше порога чуть-чуть => модель ок

courts$yhat<-predict(mod4, courts)
courts$yhat1<-ifelse(courts$yhat < 0.5, 0,1)
table(courts$contra_regime_bin, courts$yhat1) 

library(caret)
confusionMatrix(as.factor(courts$contra_regime_bin), as.factor(courts$yhat1))
# Accuracy : 0.8727 
# Sensitivity : 0.8750          
# Specificity : 0.8000
# Surprisingly good (since Constitutional court almost always rejects petitions?)

library(pROC)
f = roc(courts$contra_regime_bin, courts$yhat1)
plot(f, col="red")
auc(f)
# 0.5798
# bad fit

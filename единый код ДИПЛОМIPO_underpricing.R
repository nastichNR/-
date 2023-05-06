#####ОБЫЧНАЯ####
# TECH+NOTECH -------------------------------------------------------------
#данные все
data_all <- read.xlsx("DATA_ALL23.xlsx",sheet=2)

data_all_sent <- read.xlsx("DATA_ALL23_sent.xlsx")

data_ALL_sent_final <- merge(data_all,data_all_sent, by = "Ticker")

View(data_ALL_sent_final)

data_ALL_sent_final <- merge(data_ALL_sent_final,part_all, by ="Ticker")
#загрузим доходности индекса для поправки на рынок

library(quantmod)

# Установка начальной и конечной даты
start_date <- as.Date("2000-01-01")
end_date <- as.Date("2021-01-01")

# Загрузка данных из YAHOO FINANCE
getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date)

# Расчет ежедневных доходностей
returns <- data.frame( Date = index(GSPC), returns = dailyReturn(GSPC))

# Проверка результатов
head(returns)

#загрузим данные дня выхода на IPO каждой компании и объединим с существующеф выборкой

data_date <- read.csv("IPO_date.csv",sep = ";")
head(data_date)
data_date$Date <- as.Date(data_date$Date )
#скрепляем 
data_date_market <- merge(data_date,returns, by = "Date")

View(data_date_market)

#скрепляем с данными недооценки
data_ALL_sent_final_market <- merge(data_ALL_sent_final,data_date_market, by ="Ticker")
duplicates <- subset(data_ALL_sent_final_market, duplicated(Ticker) | duplicated(Ticker, fromLast = TRUE))
View(duplicates)

write.xlsx(data_ALL_sent_final_market,"data_ALL_sent_final_market")
# загружаем чистые данные
data_ALL_sent_final_market <- read.xlsx("data_ALL_sent_final_market.xlsx")
View(data_ALL_sent_final_market )
describe()
describe(data_ALL_sent_final_market)

#NOTECH+TECH

###соединяем все дата фреймы в один####

part1 <- read.xlsx("sentiment_part1_all.xlsx")
part2 <- read.xlsx("sentiment_part2_all.xlsx")
part3 <- read.xlsx("sentiment_part3_all.xlsx")
part4 <- read.xlsx("sentiment_part4_all.xlsx")

part12 <- merge(part1,part2, by ="Ticker")
part123 <- merge(part12,part3, by ="Ticker")
part_all <- merge(part123,part4, by ="Ticker")
View(part_all)
data_all <- read.xlsx("DATA_ALL23.xlsx", sheet = 3)

data_all_sent <- read.xlsx("DATA_ALL23_sent.xlsx")

data_ALL_sent_final <- merge(data_all,data_all_sent, by = "Ticker")
data_ALL_sent_final <- merge(data_ALL_sent_final,part_all, by ="Ticker")
#удаляем дупликаты
data_ALL_sent_final <- dplyr::select(data_ALL_sent_final, -sum_row.y.y, -sum_row.x.y)
View(data_ALL_sent_final)


#добавим переменные в данные БЕЗ рынка
data_new <- mutate(data_ALL_sent_final,   NI = ifelse(Net_income>0, 1, 0),  
                   tech = ifelse (Sector == "Tech", 1, 0),
                   underpriced = ifelse(Day_Px_Chng>0,1,0),
                   uncertainty_perc = scale(uncertaint_perc),
                   word_sum = sum_row, positive_percent = scale(positive_perc), 
                   Litigious_percent=scale(Litigious_perc), negative_percent = scale(negative_perc),
                   uncert_part1 = scale(uncert_perc_part1),uncert_part2 = scale(uncert_perc_part2))


describe(data_new)
#удаляем факторные ( в случае изменения на рынок -daily.returns.y,-Date.y,- Date.x)
data_new_new <- dplyr::select(data_new,-IPO.year, -Ticker,-Day_Close ,-Change_Close,-Opening_Price,-Sector2, 
                              -Sector, -IPO_proceed, -sum_row,
                              -Market_Capitalization)
#удаляем пропуски
data_new_new <- na.omit(data_new_new)

#описательная статистика
describe(data_new_new)
summary(data_new_new)
str(data_new_new)
View(data_new_new)
#графики плотности распределения значения недооценки в зависимости от перееменных интереса 







#график
#для двух групп
ggbetweenstats(
  data  = data_new,
  x     = Sector,
  y     = Day_Px_Chng,
  title = "Распределение недооценки по 2-м секторам",
  p.adjust.method = "none",
  bf.message = FALSE,
  xlab = "Сектор",
  ylab = "Изменение цены в первый день торгов"
)
# соотношение неопределенности и недооценки

ggplot(data_new_new, aes(x = uncertaint_perc, y = Day_Px_Chng)) +
  geom_point(aes(color = uncertaint_perc)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Соотношение неопределенности и недооценки все сектора",
       x = "Неопределенность",
       y = "Недооценка")
library(ggstatsplot)
library(AER)
library(ggside)
data("CigarettesB")

str(CigarettesB)
names(CigarettesB)

#диаграмма рассеивания
#неопределенность
library(gridExtra)
library(ggside)
gg1 <- ggscatterstats(
  data  = data_new_new,
  y     = Day_Px_Chng,
  x     = uncert_perc_part1,
  ylab  = "Доходность первого дня",
  xlab  = "Доля неопределенности в первой части проспекта",
  title = "Неопределенность", 
  xfill = "#018abd", 
  yfill = "#97cbdc", results.subtitle =FALSE,
  smooth.line.args = list(size = 1.5, color = "darkblue", method = "lm", formula = y ~ x),
  point.args = list(size = 3, alpha = 0.4, color = "#0093b7", stroke = 0, na.rm = TRUE)
)+
  theme(plot.title = element_text(size = 8))

#судебная тональность
gg2 <- ggscatterstats(
  data  = data_new_new,
  y     = Day_Px_Chng,
  x     = Litigious_perc ,
  ylab  = "Доходность первого дня",
  xlab  = "Доля судебной тональности в четвертой части проспекта",
  title = "Судебный сантимент", 
  xfill = "#018abd", 
  yfill = "#97cbdc", results.subtitle =FALSE,
  smooth.line.args = list(size = 1.5, color = "darkblue", method = "lm", formula = y ~ x),
  point.args = list(size = 3, alpha = 0.4, color = "#0093b7", stroke = 0, na.rm = TRUE)
)+
  theme(plot.title = element_text(size = 8))


#негатив
gg3 <- ggscatterstats(
  data  = data_new_new,
  y     = Day_Px_Chng,
  x     = negative_perc,
  ylab  = "Доходность первого дня",
  xlab  = "Доля негативной тональности в проспекте",
  title = "Негативный сантимент", 
  xfill = "#018abd", 
  yfill = "#97cbdc", results.subtitle =FALSE,
  smooth.line.args = list(size = 1.5, color = "darkblue", method = "lm", formula = y ~ x),
  point.args = list(size = 3, alpha = 0.4, color = "#0093b7", stroke = 0, na.rm = TRUE)
)+ theme(plot.title = element_text(size = 8))


#позитив
gg4 <- ggscatterstats(
  data  = data_new_new,
  y     = Day_Px_Chng,
  x     = positive_perc   ,
  ylab  = "Доходность первого дня",
  xlab  = "Доля позитивной тональности в проспекте",
  title = "Позитивный сантимент", 
  xfill = "#018abd", 
  yfill = "#97cbdc", results.subtitle =FALSE,
  smooth.line.args = list(size = 1.5, color = "darkblue", method = "lm", formula = y ~ x),
  point.args = list(size = 3, alpha = 0.4, color = "#0093b7", stroke = 0, na.rm = TRUE)
)+ theme(plot.title = element_text(size = 8))



# Объединяем графики в таблицу 2x1
grid.arrange(gg1, gg2, gg3, gg4, ncol = 2)

%#график для построения регрессии
model1 <- lm(data = CigarettesB, packs ~ .)

ggcoefstats(model1, 
            title = "Результаты лучшей регрессии", 
            ylab = "Переменные", 
            xlab = "Оценка коэффициента", 
            package = "RColorBrewer",
            palette = "Dark2")



#корреляционная матрица
corrplot(cor(data_new_new_underpriced))
# модель недооценки Technology -------------------------------------------------------
# недооценка обычная регрессия--------------------------------------------------------------
#корреляционная матрица
data_cor <- data_new_new_underpriced[1:32]
View(data_cor)
corrplot(cor(data_cor))

cor(data_new_new$Net_income,data_new_new$NI)
str(abs)
#без преобразований C assets
mod_underpr <- lm(Day_Px_Chng~ Total_assets  + R_D + Net_income+Book_ValperShare
                  +CAPEX + ROE + ROA + EBITDA_margin + Current_ratio
                  + Under_rank  + VC.Dummy + Rollup_dummy + Dual_dummy + Internet_dummy + Float 
                  +Issued+Shareholder_equity
                  + Age + NI + tech + uncertainty_perc + negative_percent + word_sum 
                  +positive_percent +Litigious_percent + uncert_part1 +uncert_part2+
                    Offer_Price + Net_Debt_Ebitda+IPO_num_scoop
                  , data = data_new_new)


summary(mod_underpr)
#м/к
vif(mod_underpr)

#нормальность остатков
plot(mod_underpr , which =2) 
??plot
boxCox(mod_underpr)

#влиятельные наблюдения
cook <- cooks.distance(mod_underpr)
plot(cook,type = "h")
which(cook > 0.4)
influencePlot(mod_underpr)

#степени регрессоров
plot(mod_underpr ,which =1)
crPlots(mod_underpr)
resettest(mod_underpr,power = 2)

#выгружаем начальную модель
stargazer(mod_underpr,title="Модель множественной регрессии без преобразований", 
          type="html",df=FALSE, digits=3,out = "Модель множественной регрессии без преобразований1.html")





#модель с преобразованиями
m0 <- lm(Day_Px_Chng~ log(Total_assets) + log(R_D+1) + Book_ValperShare
         + ROE + ROA + EBITDA_margin + log(Current_ratio)+log(abs(CAPEX))
         + Under_rank  + VC.Dummy + Rollup_dummy + Dual_dummy + Internet_dummy + Float 
         + log(Age+1) + NI + tech +Net_Debt_Ebitda +log(Offer_Price)+ Net_Debt_Ebitda
         + log(word_sum)+ uncertainty_perc + negative_percent
         +positive_percent + Litigious_percent + uncert_part1 +uncert_part2+
           IPO_num_scoop+log(Issued), data = data_new_new)


summary(m0)

#м/к
vif(mod_underpr_new)

#нормальность остатков
plot(mod_underpr_new , which =2) 
boxCox(mod_underpr_new )

#влиятельные наблюдения
cook <- cooks.distance(m0)
plot(cook,type = "h")
which(cook > 0.2)

influencePlot(m0 )

#степени регрессоров
plot(mod_underpr_new ,which =1)
crPlots(mod_underpr_new)
resettest(mod_underpr_new_all)

View(data_new_new)
is.na(data_new_new)
#чистка через AIC
m1  <- stepAIC(mod_underpr_new )
summary(mod_underpr_new_all)
#степени регрессоров
plot(mod_underpr_new_all ,which =1)
crPlots(mod_underpr_new_all)
resettest(mod_underpr_new_all)

#гетероскедастичность
bptest(mod_underpr_new_all)
influencePlot(mod_underpr_new_all)



#удаляем 5 влиятельных наблюдений 
a <- which(rownames(data_new_new) %in% c(297,26,59,141,255))


data_new_new_2 <- data_new_new[-a,]
View(data_new_new_2)
describe(data_new_new_2)

#модель без 5 влиятельных наблюдений
mod_underpr_new_5 <- lm(Day_Px_Chng~ log(Total_assets) + log(R_D+1) + Book_ValperShare
                        + ROE + ROA + EBITDA_margin + log(Current_ratio)+log(abs(CAPEX))
                        + Under_rank  + VC.Dummy + Rollup_dummy + Dual_dummy + Internet_dummy + Float 
                        + log(Age+1) + NI + tech +Net_Debt_Ebitda +log(Offer_Price)+ Net_Debt_Ebitda
                        + log(word_sum)+ uncertainty_perc + negative_percent
                        +positive_percent + Litigious_percent + uncert_part1 +uncert_part2+
                          IPO_num_scoop+log(Issued), data = data_new_new_2)




summary(mod_underpr_new_5 )

#м/к
vif(mod_underpr_new)

#нормальность остатков
plot(mod_underpr_new_5  , which =2) 
boxCox(mod_underpr_new )

#влиятельные наблюдения
cook <- cooks.distance(mod_underpr_new)
plot(cook,type = "h")
which(cook > 0.2)

influencePlot(mod_underpr_new_5)

#степени регрессоров
plot(mod_underpr_new_5  ,which =1)
crPlots(mod_underpr_new_5)
resettest(mod_underpr_new_all)


#чистка через AIC
m5  <- stepAIC(mod_underpr_new_5)
summary(m5)

#степени регрессоров
plot(mod_underpr_new_all ,which =1)
crPlots(mod_underpr_new_all)
resettest(mod_underpr_new_all)

#гетероскедастичность
bptest(m5)


#робастные ошибки
clse = function(mod) { 
  G = length(unique(index(mod,"id")))
  N = length(index(mod,"id"))
  dfa = (G/(G - 1))
  rob = sqrt(diag(dfa*vcovHC(mod, method="arellano", type = "HC1", 
                             cluster = "group")))
  return(rob)
}

#удалим 10-15 влияетльных наблюдений

influencePlot(mod_underpr_new_5)

#удаляем 10 влиятельных наблюдений 
a <- which(rownames(data_new_new) %in% c(297,26,59,295,255,310,127,113,50,27))


data_new_new_2 <- data_new_new[-a,]
View(data_new_new_2)


#модель без 10 влиятельных наблюдений
mod_underpr_new_10 <- lm(Day_Px_Chng~ log(Total_assets) + log(R_D+1) + Book_ValperShare
                         + ROE + ROA + EBITDA_margin + log(Current_ratio)+log(abs(CAPEX))
                         + Under_rank  + VC.Dummy + Rollup_dummy + Dual_dummy + Internet_dummy + Float 
                         + log(Age+1) + NI + tech +Net_Debt_Ebitda +log(Offer_Price) + Net_Debt_Ebitda
                         + log(word_sum)+ uncertainty_perc + negative_percent
                         +positive_percent + Litigious_percent + uncert_part1 +uncert_part2+
                           IPO_num_scoop+log(Issued), data = data_new_new_2)


summary(mod_underpr_new_10)

#м/к
vif(mod_underpr_new_10)

#нормальность остатков
plot(mod_underpr_new , which =2) 
boxCox(mod_underpr_new )

#влиятельные наблюдения
cook <- cooks.distance(mod_underpr_new)
plot(cook,type = "h")
which(cook > 0.2)

influencePlot(mod_underpr_new_10)

#степени регрессоров
plot(mod_underpr_new ,which =1)
crPlots(mod_underpr_new)
resettest(mod_underpr_new_all)


#чистка через AIC
m10  <- stepAIC(mod_underpr_new_10)
summary(m10 )


#гетероскедастичность
bptest(m10)

#удалим 25 влияетльных наблюдений

influencePlot(mod_underpr_new_10)

#удаляем 25 влиятельных наблюдений 
a <- which(rownames(data_new_new) %in% c(297,26,59,295,255,310,127,113,50,27,171,162,
                                         31,172,136,10,240,36,246,179,242,273,228,193,327))


data_new_new_2 <- data_new_new[-a,]
View(data_new_new_2)


#модель без gggg влиятельных наблюдений
mod_underpr_new_25<- lm(Day_Px_Chng~ log(Total_assets) + log(R_D+1) + Book_ValperShare
                        + ROE + ROA + EBITDA_margin + log(Current_ratio)+log(abs(CAPEX))
                        + Under_rank  + VC.Dummy + Rollup_dummy + Dual_dummy + Internet_dummy + Float 
                        + log(Age+1) + NI + tech +Net_Debt_Ebitda +log(Offer_Price)+ Net_Debt_Ebitda
                        + log(word_sum)+ uncertainty_perc + negative_percent
                        +positive_percent + Litigious_percent + uncert_part1 +uncert_part2+
                          IPO_num_scoop+log(Issued), data = data_new_new_2)


summary(mod_underpr_new_25)

#м/к
vif(mod_underpr_new)

#нормальность остатков
plot(mod_underpr_new , which =2) 
boxCox(mod_underpr_new )

#влиятельные наблюдения
cook <- cooks.distance(mod_underpr_new_25)
plot(cook,type = "h")
which(cook > 0.05)

influencePlot(mod_underpr_new_25)

#степени регрессоров
plot(mod_underpr_new ,which =1)
crPlots(mod_underpr_new)
resettest(mod_underpr_new_all)


#чистка через AIC
m25  <- stepAIC(mod_underpr_new_25)
summary(m25 )


#гетероскедастичность
bptest(mod_underpr_new_all_25 )




#выгружаем все модели c спроверкой влиятельных наблюдений


#робастные ошибки
clse = function(mod) { 
  G = length(unique(index(mod,"id")))
  N = length(index(mod,"id"))
  dfa = (G/(G - 1))
  rob = sqrt(diag(dfa*vcovHC(mod, method="arellano", type = "HC1", 
                             cluster = "group")))
  return(rob)
}

stargazer(m0,m5,m10,m25, se=list(clse(m0),clse(m5),clse(m10),clse(m25)),  
          title="Устойчивость результатов к исключению разного количества выбросов (по всем секторам)", type="html",
          df=FALSE, digits=3,out="models1.htm")



#ФИНАЛЬНАЯ МОДЕЛЬ ВСЕ СЕКТОРА БЕЗ УЧЕТА РЫНКА

#удаляем 10 влиятельных наблюдений 
a <- which(rownames(data_new_new) %in% c(297,26,59,295,255,310,127,113,50,27))


data_new_new_2 <- data_new_new[-a,]
data_new_new_2  <- mutate(data_new_new_2 , CAPEX = -CAPEX)
View(data_new_new_2)
describe(data_new_new_2)
summary(data_new_new_2)
#модель без 10 влиятельных наблюдений
mod_underpr_new_10 <- lm(Day_Px_Chng~ log(Total_assets) + log(R_D+1) + Book_ValperShare
                         + ROE + ROA + EBITDA_margin + log(Current_ratio)+log(CAPEX)
                         + Under_rank  + VC.Dummy + Rollup_dummy + Dual_dummy + Internet_dummy + Float 
                         + log(Age+1) + NI + tech +Net_Debt_Ebitda +log(Offer_Price) + Net_Debt_Ebitda
                         + log(word_sum)+ uncertainty_perc + negative_percent
                         +positive_percent + Litigious_percent + uncert_part1 +uncert_part2+
                           IPO_num_scoop+log(Issued), data = data_new_new_2)


summary(mod_underpr_new_10)

#м/к
vif(mod_underpr_new_10)

#нормальность остатков
plot(mod_underpr_new , which =2) 
boxCox(mod_underpr_new )

#влиятельные наблюдения
cook <- cooks.distance(mod_underpr_new)
plot(cook,type = "h")
which(cook > 0.2)

influencePlot(mod_underpr_new_10)

#степени регрессоров
plot(mod_underpr_new_10 ,which =1)
crPlots(mod_underpr_new_10)
resettest(mod_underpr_new_10)


#чистка через AIC
m10_ALL  <- stepAIC(mod_underpr_new_10)
summary(m10_ALL )
vif(m10_ALL )

#сравнение короткая против длинной
waldtest(m10_ALL,mod_underpr_new_10 )


#гетероскедастичность
bptest(m10_ALL)

stargazer(m10_ALL , se=list(clse(m10_ALL)),  
          title="Результаты оценивания регрессии недооценки (по всем секторам)", type="html",
          df=FALSE, digits=3,out="model_all222.htm")


#сравниваем ны выборке рынка


stargazer(m10_ALL_nomarket,m10_ALL , se=list(clse(m10_ALL),clse(m10_ALL_nomarket)),  
          title="Результаты оценивания регрессии недооценки (по всем секторам)", type="html",
          df=FALSE, digits=3,out="model_all222.htm")


cor(log(data_new_new$Total_assets),data_new_new$ROA)
vif(m10_ALL)

#устойчивость результатов

mod_underpr_new1 <- lm(Day_Px_Chng~ uncertainty_perc,data = data_new_new)
summary(mod_underpr_new1)

mod_underpr_new2 <- lm(Day_Px_Chng~ uncertainty_perc+log(Total_assets),
                       data = data_new_new)
summary(mod_underpr_new2)

mod_underpr_new3 <- lm(Day_Px_Chng~ uncertainty_perc+log(Total_assets)+log(Revenue),
                       data = data_new_new)
summary(mod_underpr_new3)
mod_underpr_new4 <- lm(Day_Px_Chng~ uncertainty_perc+log(Total_assets)+log(Revenue)
                       +ROA,
                       data = data_new_new)
summary(mod_underpr_new4)

mod_underpr_new5 <- lm(Day_Px_Chng~ uncertainty_perc+log(Total_assets)+log(Revenue)
                       +ROA+EBITDA_margin,
                       data = data_new_new)
summary(mod_underpr_new5)

mod_underpr_new6 <- lm(Day_Px_Chng~ uncertainty_perc+log(Total_assets)+log(Revenue)
                       +ROA+EBITDA_margin+Rollup_dummy,
                       data = data_new_new)
summary(mod_underpr_new6)

mod_underpr_new7 <- lm(Day_Px_Chng~ uncertainty_perc+log(Total_assets)+log(Revenue)
                       +ROA+EBITDA_margin+Rollup_dummy+tech,
                       data = data_new_new)
summary(mod_underpr_new7)

mod_underpr_new8 <- lm(Day_Px_Chng~ uncertainty_perc+log(Total_assets)+log(Revenue)
                       +ROA+EBITDA_margin+Rollup_dummy+tech+scale(positive_perc),
                       data = data_new_new)
summary(mod_underpr_new8)

mod_underpr_new9 <- lm(Day_Px_Chng~ uncertainty_perc+log(Total_assets)+log(Revenue)
                       +ROA+EBITDA_margin+Rollup_dummy+tech+scale(positive_perc)+log(Offer_Price),
                       data = data_new_new)
summary(mod_underpr_new9)

jtools::export_summs(mod_underpr_new1, 
                     mod_underpr_new2,
                     mod_underpr_new3,
                     mod_underpr_new4,
                     mod_underpr_new5,
                     mod_underpr_new6,
                     mod_underpr_new7,
                     mod_underpr_new8,
                     mod_underpr_new9, scale = TRUE,to.file = "docx", file.name = "test.docx")


cor(data_new_new$uncertainty_sent,data_new_new$Day_Px_Chng)


###модель с поправкой на рынок####

# загружаем чистые данные
data_ALL_sent_final_market <- read.xlsx("data_ALL_sent_final_market.xlsx")
View(data_ALL_sent_final_market )

describe(data_ALL_sent_final_market)



#добавим переменные в данные с рынком
data_new <- mutate(data_ALL_sent_final_market,   NI = ifelse(Net_income>0, 1, 0),  
                   tech = ifelse (Sector == "Technology", 1, 0),
                   underpriced = ifelse(Day_Px_Chng>0,1,0),
                   uncertainty_perc = scale(uncertaint_perc),Day_px_market = (Day_Px_Chng -daily.returns.x),
                   word_sum = sum_row)


summary(data_new)
describe(data_new)
#удаляем факторные ( в случае изменения на рынок -daily.returns.y,-Date.y,- Date.x)
data_new_new_market <- dplyr::select(data_new,-IPO.year, -Ticker,-Day_Close ,-Change_Close,-Opening_Price,-Sector2, 
                                     -Sector, -IPO_proceed, -sum_row,
                                     -Change_Opening, -Market_Capitalization, -daily.returns.y,-Date.y,- Date.x)
#удаляем пропуски
data_new_new_market<- na.omit(data_new_new_market)

#оставим в данных только недооцененные компании
#оставим в данных только недооцененные компании
data_new_new_underpriced<- subset(data_new_new, Day_Px_Chng > 0)
View(data_new_new_underpriced)
data_new_new_underpriced<- subset(data_new_new, Day_Px_Chng > 0)
View(data_new_new_underpriced)
#описательная статистика
describe(data_new_new)
summary(data_new_new)
str(data_new_new)
View(data_new_new)

#график
#для двух групп
ggbetweenstats(
  data  = data_new,
  x     = Sector,
  y     = Day_Px_Chng,
  title = "Распределение недооценки по 2-м секторам",
  p.adjust.method = "none",
  bf.message = FALSE,
  xlab = "Сектор",
  ylab = "Изменение цены в первый день торгов"
)




#корреляционная матрица
corrplot(cor(data_new_new_underpriced))
# модель недооценки Technology -------------------------------------------------------
# недооценка обычная регрессия--------------------------------------------------------------




#без преобразований
mod_underpr <- lm(Day_Px_Chng ~ Total_assets + Revenue + R_D +Net_income+Book_ValperShare
                  +Current_liabilities.x+CAPEX + ROE + ROA + EBITDA_margin + Current_ratio
                  + Under_rank  + VC.Dummy + Rollup_dummy + Dual_dummy + Internet_dummy + Float 
                  + Age + NI + tech + uncertainty_perc + scale(negative_perc)+word_sum 
                  +scale(positive_perc) + Offer_Price + Net_Debt_Ebitda, data_new_new_market)


summary(mod_underpr)
#м/к
vif(mod_underpr)

#нормальность остатков
plot(mod_underpr , which =2) 
boxCox(mod_underpr)

#влиятельные наблюдения
cook <- cooks.distance(mod_underpr)
plot(cook,type = "h")
which(cook > 0.4)
influencePlot(mod_underpr)
data_new_new <- data_new_new[-142,]
data_new_new <- data_new_new[-192,]
data_new_new <- data_new_new[-300,]
data_new_new <- data_new_new[-27,]
data_new_new <- data_new_new[-258,]
data_new_new <- data_new_new[-313,]
data_new_new <- data_new_new[-60,]
summary(data_new_new)
nrow(data_new_new)
#степени регрессоров
plot(mod_underpr ,which =1)
crPlots(mod_underpr)
resettest(mod_underpr,power = 2)


#выгружаем начальную модель
stargazer(mod_underpr,title="Модель множественной регрессии без преобразований", 
          type="html",df=FALSE, digits=3,out = "Модель множественной регрессии без преобразований1.html")

#модель с преобразованиями
mod_underpr_new_market <- lm(Day_px_market~ log(Total_assets) + log(Revenue) + log(R_D) +Book_ValperShare
                             + ROE + ROA + EBITDA_margin + log(Current_ratio)
                             + Under_rank  + VC.Dummy + Rollup_dummy + Dual_dummy + Internet_dummy + Float 
                             + log(Age+1) + NI + tech + uncertainty_perc+ scale(negative_perc)
                             +scale(positive_perc) +Net_Debt_Ebitda +log(Offer_Price)+ Net_Debt_Ebitda
                             + log(word_sum)+daily.returns.x , data =  data_new_new_market)





summary(mod_underpr_new_market)

#м/к
vif(mod_underpr_new)

#нормальность остатков
plot(mod_underpr_new_marketwhich =2) 
boxCox(mod_underpr_new_market)

#влиятельные наблюдения
cook <- cooks.distance(mod_underpr_new_market)
plot(cook,type = "h")
which(cook > 0.3)

influencePlot(mod_underpr_new_market)
data_new_new <- data_new_new[-313,]


#степени регрессоров
plot(mod_underpr_new_market ,which =1)
crPlots(mod_underpr_new_market)
resettest(mod_underpr_new_market)


#чистка через AIC
mod_underpr_new_all_market  <- stepAIC(mod_underpr_new_market )
summary(mod_underpr_new_all_market )
#степени регрессоров
plot(mod_underpr_new_all ,which =1)
crPlots(mod_underpr_new_all)
resettest(mod_underpr_new_all_market )

#гетероскедастичность
bptest(mod_underpr_new_all_market )
influencePlot(mod_underpr_new_all_market )


#робастные ошибки
clse = function(mod) { 
  G = length(unique(index(mod,"id")))
  N = length(index(mod,"id"))
  dfa = (G/(G - 1))
  rob = sqrt(diag(dfa*vcovHC(mod, method="arellano", type = "HC1", 
                             cluster = "group")))
  return(rob)
}

#выгружаем итоговую модель
stargazer(mod_underpr_new_all_market, se=list(clse(mod_underpr_new)),title="Финальная модель все сектора c поправкой на рынок ", type="html", 
          df=FALSE, digits=3,out = "Финальная модель c поправкой на рынок.html")


??index
stargazer(mod_underpr_new_all, se=list(clse(mod_underpr_new)), 
          title="Финальная модель все сектора22 ", type="html",
          df=FALSE, digits=3, out = "Финальная модель TECH NOTECH.html")




#устойчивость результатов ПОПРАВИТЬ НА РЫНОК

mod_underpr_new1 <- lm(Day_Px_Chng~ uncertainty_perc,data = data_new_new)
summary(mod_underpr_new1)

mod_underpr_new2 <- lm(Day_Px_Chng~ uncertainty_perc+log(Total_assets),
                       data = data_new_new)
summary(mod_underpr_new2)

mod_underpr_new3 <- lm(Day_Px_Chng~ uncertainty_perc+log(Total_assets)+log(Revenue),
                       data = data_new_new)
summary(mod_underpr_new3)
mod_underpr_new4 <- lm(Day_Px_Chng~ uncertainty_perc+log(Total_assets)+log(Revenue)
                       +ROA,
                       data = data_new_new)
summary(mod_underpr_new4)

mod_underpr_new5 <- lm(Day_Px_Chng~ uncertainty_perc+log(Total_assets)+log(Revenue)
                       +ROA+EBITDA_margin,
                       data = data_new_new)
summary(mod_underpr_new5)

mod_underpr_new6 <- lm(Day_Px_Chng~ uncertainty_perc+log(Total_assets)+log(Revenue)
                       +ROA+EBITDA_margin+Rollup_dummy,
                       data = data_new_new)
summary(mod_underpr_new6)

mod_underpr_new7 <- lm(Day_Px_Chng~ uncertainty_perc+log(Total_assets)+log(Revenue)
                       +ROA+EBITDA_margin+Rollup_dummy+tech,
                       data = data_new_new)
summary(mod_underpr_new7)

mod_underpr_new8 <- lm(Day_Px_Chng~ uncertainty_perc+log(Total_assets)+log(Revenue)
                       +ROA+EBITDA_margin+Rollup_dummy+tech+scale(positive_perc),
                       data = data_new_new)
summary(mod_underpr_new8)

mod_underpr_new9 <- lm(Day_Px_Chng~ uncertainty_perc+log(Total_assets)+log(Revenue)
                       +ROA+EBITDA_margin+Rollup_dummy+tech+scale(positive_perc)+log(Offer_Price),
                       data = data_new_new)
summary(mod_underpr_new9)

jtools::export_summs(mod_underpr_new1, 
                     mod_underpr_new2,
                     mod_underpr_new3,
                     mod_underpr_new4,
                     mod_underpr_new5,
                     mod_underpr_new6,
                     mod_underpr_new7,
                     mod_underpr_new8,
                     mod_underpr_new9, scale = TRUE,to.file = "docx", file.name = "test.docx")


cor(data_new_new$uncertainty_sent,data_new_new$Day_Px_Chng)






###модель с выборкой только положительная доходность####

####оставим в данных только недооцененные компании####
data_new_new_underpriced<- subset(data_new_new, Day_Px_Chng > 0)
View(data_new_new_underpriced)
#без преобразований
mod_underpr <- lm(Day_Px_Chng ~ Total_assets + Revenue + R_D + Net_income+Book_ValperShare
                  +CAPEX + ROE + ROA + EBITDA_margin + Current_ratio
                  + Under_rank  + VC.Dummy + Rollup_dummy + Dual_dummy + Internet_dummy + Float 
                  +Issued+Shareholder_equity
                  + Age + NI + tech + uncertainty_perc + scale(negative_perc)+word_sum 
                  +scale(positive_perc) +scale(Litigious_perc) + Offer_Price + Net_Debt_Ebitda+IPO_num_scoop, data = data_new_new_underpriced)


summary(mod_underpr)
#м/к
vif(mod_underpr)

#нормальность остатков
plot(mod_underpr , which =2) 
boxCox(mod_underpr)

#влиятельные наблюдения
cook <- cooks.distance(mod_underpr)
plot(cook,type = "h")
which(cook > 0.4)
influencePlot(mod_underpr)

a <- which(rownames(data_new_new_underpriced) %in% c(141,113,59,26,255,310))

data_new_new_2_under <- data_new_new_underpriced[-a,]

summary(data_new_new)
nrow(data_new_new)
#степени регрессоров
plot(mod_underpr ,which =1)
crPlots(mod_underpr)
resettest(mod_underpr,power = 2)


#выгружаем начальную модель
stargazer(mod_underpr,title="Модель множественной регрессии без преобразований", 
          type="html",df=FALSE, digits=3,out = "Модель множественной регрессии без преобразований1.html")

#модель с преобразованиями
mod_underpr_new_positive <- lm(Day_Px_Chng~ log(Revenue) + log(R_D) +Book_ValperShare
                               + ROE + ROA + EBITDA_margin + log(Current_ratio)
                               + Under_rank  + VC.Dummy + Rollup_dummy + Dual_dummy + Internet_dummy + Float 
                               + log(Age+1) + NI + tech + uncertainty_perc+ scale(negative_perc)
                               +scale(positive_perc) +Net_Debt_Ebitda +log(Offer_Price)+ Net_Debt_Ebitda
                               + log(word_sum)+scale(uncert_perc_part1)+scale(uncert_perc_part2)+scale(Litigious_perc)+ 
                                 IPO_num_scoop+log(Issued)+Shareholder_equity, data = data_new_new_2)






summary(mod_underpr_new_positive )

#м/к
vif(mod_underpr_new_positive)

#нормальность остатков
plot(mod_underpr_new_positive , which =2) 
boxCox(mod_underpr_new_positive )

#влиятельные наблюдения
cook <- cooks.distance(mod_underpr_new_positive)
plot(cook,type = "h")
which(cook > 0.3)

influencePlot(mod_underpr_new_positive)
data_new_new <- data_new_new[-313,]


#степени регрессоров
plot(mod_underpr_new_positive ,which =1)
crPlots(mod_underpr_new_positive)
resettest(mod_underpr_new_positive)


#чистка через AIC
mod_underpr_new_all_positive  <- stepAIC(mod_underpr_new_positive )
summary(mod_underpr_new_all_positive )
#степени регрессоров
plot(mod_underpr_new_all_positive  ,which =1)
crPlots(mod_underpr_new_all_positive )
resettest(mod_underpr_new_all_positive )

#гетероскедастичность
bptest(mod_underpr_new_all_positive )
influencePlot(mod_underpr_new_all_positive )


#робастные ошибки
clse = function(mod) { 
  G = length(unique(index(mod,"id")))
  N = length(index(mod,"id"))
  dfa = (G/(G - 1))
  rob = sqrt(diag(dfa*vcovHC(mod, method="arellano", type = "HC1", 
                             cluster = "group")))
  return(rob)
}

#выгружаем итоговую модель
stargazer(mod_underpr_new_all_positive , se=list(clse(mod_underpr_new)),title="Финальная модель все сектора22 ", type="html", 
          df=FALSE, digits=3,out = "Финальная модель только Positive2804.html")





#модель обычна/с поправкой на рынок/с только положительными доходностями
tools::export_summs(mod_underpr_new_all, 
                    mod_underpr_new_all_market,
                    mod_underpr_new_all_positive,
                    scale = TRUE,to.file = "docx", file.name = "test3 models.docx")

cor(data_new_new$Revenue,data_new_new$Total_assets)
#устойчивость результатов

mod_underpr_new1 <- lm(Day_Px_Chng~ uncertainty_perc,data = data_new_new)
summary(mod_underpr_new1)

mod_underpr_new2 <- lm(Day_Px_Chng~ uncertainty_perc+log(Total_assets),
                       data = data_new_new)
summary(mod_underpr_new2)

mod_underpr_new3 <- lm(Day_Px_Chng~ uncertainty_perc+log(Total_assets)+log(Revenue),
                       data = data_new_new)
summary(mod_underpr_new3)
mod_underpr_new4 <- lm(Day_Px_Chng~ uncertainty_perc+log(Total_assets)+log(Revenue)
                       +ROA,
                       data = data_new_new)
summary(mod_underpr_new4)

mod_underpr_new5 <- lm(Day_Px_Chng~ uncertainty_perc+log(Total_assets)+log(Revenue)
                       +ROA+EBITDA_margin,
                       data = data_new_new)
summary(mod_underpr_new5)

mod_underpr_new6 <- lm(Day_Px_Chng~ uncertainty_perc+log(Total_assets)+log(Revenue)
                       +ROA+EBITDA_margin+Rollup_dummy,
                       data = data_new_new)
summary(mod_underpr_new6)

mod_underpr_new7 <- lm(Day_Px_Chng~ uncertainty_perc+log(Total_assets)+log(Revenue)
                       +ROA+EBITDA_margin+Rollup_dummy+tech,
                       data = data_new_new)
summary(mod_underpr_new7)

mod_underpr_new8 <- lm(Day_Px_Chng~ uncertainty_perc+log(Total_assets)+log(Revenue)
                       +ROA+EBITDA_margin+Rollup_dummy+tech+scale(positive_perc),
                       data = data_new_new)
summary(mod_underpr_new8)

mod_underpr_new9 <- lm(Day_Px_Chng~ uncertainty_perc+log(Total_assets)+log(Revenue)
                       +ROA+EBITDA_margin+Rollup_dummy+tech+scale(positive_perc)+log(Offer_Price),
                       data = data_new_new)
summary(mod_underpr_new9)

jtools::export_summs(mod_underpr_new1, 
                     mod_underpr_new2,
                     mod_underpr_new3,
                     mod_underpr_new4,
                     mod_underpr_new5,
                     mod_underpr_new6,
                     mod_underpr_new7,
                     mod_underpr_new8,
                     mod_underpr_new9, scale = TRUE,to.file = "docx", file.name = "test.docx")


cor(data_new_new$uncertainty_sent,data_new_new$Day_Px_Chng)




# недооценка логит регрессия--------------------------------------------------------------

data_new_new_ml <- mutate(data_new_new_ml, CAPEX = ((-CAPEX)+1))
View(data_new_new_ml)
mod_logit <- glm(underpriced~ log(R_D+1) +Book_ValperShare
                 + ROE + EBITDA_margin + log(Current_ratio)+log(CAPEX)
                 + Under_rank  + VC.Dummy + Rollup_dummy+ Internet_dummy +tech + Float 
                 + log(Age+1) + NI  +log(Offer_Price)
                 + log(word_sum)  + Litigious_percent + uncert_part1 +uncert_part2+
                   log(Issued),data = data_new_new_ml,family = binomial, x = TRUE )
summary(mod_logit)


vif(mod_logit)

#чистка через Акаике
mod_logit_aic_all <- stepAIC(mod_logit)
summary(mod_logit_aic_all)
vif(mod_logit_aic_all)
#какая модель лучше?
waldtest(mod_logit_aic_all,mod_logit)


#предельные эффекты для среднего

mod_dop_all <- maBina(mod_logit_aic_all, x.mean = TRUE)
mod_dop[1:3]

#выгрузка общей таблицы с предельных эффектов
stargazer(mod_logit_aic_all, mod_dop_all, 
          title="Финальная логит модель для всех секторов", type="html",
          df=FALSE, digits=3, out = "Общая логит таблица ыsssККК.html")

#качество модели
hitmiss(mod_logit_aic_all)
#оптимальная граница для классификации

#то же самое только на ВЫБОРКЕ БЕЗ 10 ВЫБРОСОВ

#удаляем 10 влиятельных наблюдений 
a <- which(rownames(data_new_new) %in% c(297,26,59,295,255,310,127,113,50,27))


data_new_new_2 <- data_new_new[-a,]
View(data_new_new_2)
summary(data_new_new_2)

mod_logit <- glm(underpriced~ log(R_D+1) + Book_ValperShare
                 + ROE + tech+EBITDA_margin + log(Current_ratio)+log(abs(CAPEX))
                 + Under_rank  + VC.Dummy + Rollup_dummy+ Internet_dummy + Float 
                 + log(Age+1) + NI  +log(Offer_Price)+ Net_Debt_Ebitda
                 + log(word_sum) 
                 +positive_percent + Litigious_percent + uncert_part1 +uncert_part2+
                   IPO_num_scoop+log(Issued),data = data_new_new_2,family = binomial, x = TRUE )
summary(mod_logit)


vif(mod_logit)

#чистка через Акаике
mod_logit_aic_no10 <- stepAIC(mod_logit)
summary(mod_logit_aic_no10)

#какая модель лучше?
wald_table <- waldtest(mod_logit_aic,mod_logit)
# Создание таблицы HTML с помощью функции stargazer
stargazer(wald_table, title = "Wald Test Results", type = "html", out = "wald_test_results.html")

#

#предельные эффекты для среднего

mod_dop_no10 <- maBina(mod_logit_aic_no10, x.mean = TRUE)
mod_dop[1:3]

#выгрузка общей таблицы с выбросами и без 
#предельных эффектов

stargazer(mod_logit_aic_all, mod_logit_aic_no10 ,
          title="Устойчивость результатов логит модели к выбросам", type="html",
          df=FALSE, digits=3, out = "Общая логит таблица устойчивость к выбросам.html")

#предельных эффектов
stargazer(mod_dop_all, mod_dop_no10 ,
          title="Предельные эффекты для среднего для первоначальной модели и модели без выбросов", type="html",
          df=FALSE, digits=3, out = "Предельные эффекты для среднего для первоначальной модели и модели без выбросов.html")


#качество модели
hitmiss(mod_logit_aic_all)

b <- hitmiss(mod_logit, k = S)
str(b)

#визуализация 

Pr <- predict(mod_logit_aic_all, type ="response",data_new_new_ml)
Pr_2 <- ifelse(Pr>0.5,1,0)
Y <- as.factor(data_new_new_ml$underpriced)
Y <- as.numeric(data_new_new_ml$underpriced)
Y_pr <- as.factor(Pr_2 )
mod_caret <- caret::confusionMatrix(Y_pr,Y)

# P-Value [Acc > NIR] : 0.1085  => модели одинакового качества
#если после AIC, то  P-Value [Acc > NIR] : 0.01547  
install.packages("pROC")
library(pROC)
??plotROC
install.packages("lares")
library(lares)

lares::mplot_roc(Y ,Pr_2)
#ROC
pROC::plot.roc(data_new_new_ml$underpriced, fitted(mod_logit))

roc.data <- roc(Y,Pr_2)
ROC_c <- roc( fitted(mod_logit_aic_all), as.factor(Y)) 
qplot(x=roc.data$fpr,y=roc.data$tpr, geom = "line")

# lasso -------------------------------------------------------------------

data <- data.frame(data_new_new$Total_assets,data_new_new$Revenue,data_new_new$Float,data_new_new$Book_ValperShare,data_new_new$EBITDA_margin,data_new_new$R_D,
                   data_new_new$Under_rank,data_new_new$VC.Dummy,data_new_new$Rollup_dummy,data_new_new$Dual_dummy,data_new_new$Internet_dummy,data_new_new$Age,
                   data_new_new$ROE,data_new_new$Current_ratio,data_new_new$NI,data_new_new$uncertainty_perc,
                   data_new_new$tech,data_new_new$Offer_Price)
View(data)
mod_lasso <- glmnet(data,data_new_new$Day_Px_Chng, alpha = 1)
plot(mod_lasso,label =TRUE)

#выбор лямбды через кроссвалидацию

data <- as.matrix(data)
mod_lasso2 <- cv.glmnet(data,data_new_new$Day_Px_Chng, alpha = 1)
plot(mod_lasso2)
par("mar")

#лямбды
mod_lasso2$lambda.min
mod_lasso2$lambda.1se

#подставим в модель

mod_lasso3 <- glmnet(data,data_new_new$Day_Px_Chng, alpha = 1, lambda = mod_lasso2$lambda.1se)
coef(mod_lasso3)

#подбор альфы
TR <- trainControl(method = "repeatedcv", repeats = 10)
mod_lasso4 <- train(Day_Px_Chng ~Total_assets + Revenue + Float+Book_ValperShare + EBITDA_margin +R_D+
                      Under_rank+VC.Dummy+Rollup_dummy + Dual_dummy + Internet_dummy + Age+
                      ROE+ Current_ratio + NI  + uncertainty_perc+
                      tech+Offer_Price ,data =data_new_new, method = "glmnet",trControl =TR)

mod_lasso4$bestTune
coef(mod_lasso4$finalModel,mod_lasso4$bestTune$lambda)


# финальная модель
mod_lasso5 <- glmnet(data,data_new_new$Day_Px_Chng, alpha = mod_lasso4$bestTune$alpha ,lambda = mod_lasso4$bestTune$lambda)
coef(mod_lasso5)
plot(mod_lasso5,label =TRUE)

# PCA ---------------------------------------------------------------------
attach(data_new_new)
data_PCA <- select (data_new_new ,Total_assets,R_D,Net_income,Book_ValperShare,
                    CAPEX,ROE,ROA,EBITDA_margin,Current_ratio,Under_rank,VC.Dummy,Rollup_dummy,Dual_dummy,Internet_dummy,
                    Float,Issued,Shareholder_equity,Age,NI,tech,uncertainty_perc,negative_percent, word_sum, 
                    positive_percent,Litigious_percent,uncert_part1,uncert_part2,
                    Offer_Price,Net_Debt_Ebitda,IPO_num_scoop,Day_Px_Chng, Issued)
PCA_mod_tech <- PCA(data_PCA)
#оптимальное количество главных компонент
fviz_eig(PCA_mod_tech)

#первые две компоненты позволяют объяснить всего лишь около 32% изменения всех данных

#визуализация
fviz_pca_var(PCA_mod_tech, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

#удаляем плохо объясняемые переменные и сильно коррелированные
data_PCA <- select (data_new_new ,EBITDA,Total_assets,
                    CAPEX,VC.Dummy,
                    Age,NI,negative_percent,
                    word_sum,Litigious_percent
                    
)
PCA_mod_tech <- PCA(data_PCA)

#визуализация
fviz_pca_var(PCA_mod_tech, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

#интерпретации главных компонент
corrplot(PCA_mod_tech$var$coord, is.corr = FALSE)

#интерпретация главных компонент через квадраты косинусов (квадраты коэффициентов матрицы нагрузок)
corrplot(PCA_mod_tech$var$cos2, is.corr = FALSE)

# переменные,Которые  удается хорошо приблизить за счет первых двух компонент
fviz_cos2(PCA_mod_tech, choice = "var", axes = 1:2)

#если добавить еще одну
fviz_cos2(PCA_mod_tech, choice = "var", axes = 1:3)

#иерархическая кластеризация
mod_clust <- HCPC(data_PCA)
fviz_dend(mod_clust, cex = 0.6, lwd = 0.8, k = 4,
          rect = TRUE,
          k_colors = "jco",
          rect_border = "jco",
          rect_fill = TRUE,
          type = "phylogenic", repel = TRUE)

plot(mod_clust, choice = "3D.map", angle = 60, ind.names = FALSE)
fviz_dend(mod_clust, cex = 0.7, palette = "jco", rect = TRUE, rect_fill = TRUE, 
          rect_border = "jco", labels_track_height = 0.8)
fviz_cluster(mod_clust, palette = "jco", repel = TRUE, geom = "point")

#график каменистой осыпи
fviz_nbclust(data_PCA, kmeans, "wss")

#иерархическая кластеризация

mod_clust_h <- hclust(dist(data_PCA))
fviz_dend(mod_clust_h)

#а если без главных компонент
mod_clust_h_2 <- hclust(dist(data_PCA))
fviz_dend(mod_clust_h_2)
#что можно достать из объекта?
mod_clust$desc.var
mod_clust$desc.axes
mod_clust$data.clust$clust
table(mod_clust$data.clust$clust)

#модель 3
mod_clust$data.clust
mod_clust$desc.axes
mod_clust$desc.ind



##### регрессия с РЫНКОМ #### 
#данные все на 4 странице итогового файла!!!!
data_all <- read.xlsx("DATA_ALL23.xlsx",sheet=4)

data_all_sent <- read.xlsx("DATA_ALL23_sent.xlsx")

data_ALL_sent_final <- merge(data_all,data_all_sent, by = "Ticker")

View(data_ALL_sent_final)

data_ALL_sent_final <- merge(data_ALL_sent_final,part_all, by ="Ticker")
#загрузим доходности индекса для поправки на рынок

library(quantmod)

# Установка начальной и конечной даты
start_date <- as.Date("2000-01-01")
end_date <- as.Date("2021-01-01")

# Загрузка данных из YAHOO FINANCE
getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date)

# Расчет ежедневных доходностей
returns <- data.frame( Date = index(GSPC), returns = dailyReturn(GSPC))

# Проверка результатов
head(returns)

#загрузим данные дня выхода на IPO каждой компании и объединим с существующеф выборкой

data_date <- read.csv("IPO_date.csv",sep = ";")
head(data_date)
data_date$Date <- as.Date(data_date$Date )
#скрепляем 
data_date_market <- merge(data_date,returns, by = "Date")

View(data_date_market)

#скрепляем с данными недооценки
data_ALL_sent_final_market <- merge(data_ALL_sent_final,data_date_market, by ="Ticker")
duplicates <- subset(data_ALL_sent_final_market, duplicated(Ticker) | duplicated(Ticker, fromLast = TRUE))
View(duplicates)

write.xlsx(data_ALL_sent_final_market,"data_ALL_sent_final_market")
# загружаем чистые данные
data_ALL_sent_final_market <- read.xlsx("data_ALL_sent_final_market.xlsx")
View(data_ALL_sent_final_market )
describe()
describe(data_ALL_sent_final_market)

#NOTECH+TECH

###соединяем все дата фреймы в один####
#данные РЫНКА
data_all <- read.xlsx("DATA_ALL23.xlsx",sheet=4)
#данные тональности по частям
part1 <- read.xlsx("sentiment_part1_all.xlsx")
part2 <- read.xlsx("sentiment_part2_all.xlsx")
part3 <- read.xlsx("sentiment_part3_all.xlsx")
part4 <- read.xlsx("sentiment_part4_all.xlsx")

part12 <- merge(part1,part2, by ="Ticker")
part123 <- merge(part12,part3, by ="Ticker")
part_all <- merge(part123,part4, by ="Ticker")
View(part_all)

#данные тональности по всем документам
data_all_sent <- read.xlsx("DATA_ALL23_sent.xlsx")

#скрепляем с данными тональности всего документа и  частей

data_ALL_sent_final_market <- merge(data_all,data_all_sent, by ="Ticker")
data_ALL_sent_final_market <- merge(data_ALL_sent_final_market,part_all, by ="Ticker")
#удаляем дупликаты
data_ALL_sent_final <- dplyr::select(data_ALL_sent_final, -sum_row.y.y, -sum_row.x.y)
View(data_ALL_sent_final)

#добавим переменные в данные с рынком
data_new <- mutate(data_ALL_sent_final_market,  NI = ifelse(Net_income>0, 1, 0),  
                   tech = ifelse (Sector == "Tech", 1, 0),
                   underpriced = ifelse(Day_Px_Chng>0,1,0),
                   uncertainty_perc = scale(uncertaint_perc),
                   word_sum = sum_row, positive_percent = scale(positive_perc), 
                   Litigious_percent=scale(Litigious_perc), negative_percent = scale(negative_perc),
                   uncert_part1 = scale(uncert_perc_part1),uncert_part2 = scale(uncert_perc_part2),Day_px_market = (Day_Px_Chng - daily.returns) )


summary(data_new)
describe(data_new)
#удаляем факторные ( в случае изменения на рынок -daily.returns.y,-Date.y,- Date.x)
data_new_new <- dplyr::select(data_new,-IPO.year, -Ticker,-Day_Close ,-Change_Close,-Opening_Price,-Sector2, 
                              -Sector, -IPO_proceed, -sum_row,
                              -Market_Capitalization)
#удаляем пропуски
data_new_new <- na.omit(data_new_new)

#описательная статистика
describe(data_new_new)
summary(data_new_new)
str(data_new_new)
View(data_new_new)



#модель с преобразованиями
m0 <- lm(Day_px_market ~ log(Total_assets) + log(R_D+1) + Book_ValperShare
         + ROE + ROA + EBITDA_margin + log(Current_ratio)+log(abs(CAPEX))
         + Under_rank  + VC.Dummy  + Internet_dummy + Float 
         + log(Age+1) + NI + tech +Net_Debt_Ebitda +log(Offer_Price)+ Net_Debt_Ebitda
         + log(word_sum)+ uncertainty_perc + negative_percent
         +positive_percent + Litigious_percent + uncert_part1 +uncert_part2+
           IPO_num_scoop+log(Issued), data = data_new_new)


summary(m0)

#м/к
vif(m0 )

#нормальность остатков
plot(m0  , which =2) 
boxCox(mod_underpr_new )

#влиятельные наблюдения
cook <- cooks.distance(m0)
plot(cook,type = "h")
which(cook > 0.2)

influencePlot(m0 )

#степени регрессоров
plot(mod_underpr_new ,which =1)
crPlots(mod_underpr_new)
resettest(mod_underpr_new_all)

View(data_new_new)
is.na(data_new_new)
#чистка через AIC
m1  <- stepAIC(m0 )
summary(m1)
vif()
#степени регрессоров
plot(mod_underpr_new_all ,which =1)
crPlots(mod_underpr_new_all)
resettest(mod_underpr_new_all)

#гетероскедастичность
bptest(m1)
influencePlot(mod_underpr_new_all)



#удаляем 5 влиятельных наблюдений 
a <- which(rownames(data_new_new) %in% c(296,26,59,309,255))


data_new_new_2 <- data_new_new[-a,]
View(data_new_new_2)
describe(data_new_new_2)

#модель без 5 влиятельных наблюдений
mod_underpr_new_5 <- lm(Day_px_market~ log(Total_assets) + log(R_D+1) + Book_ValperShare
                        + ROE + ROA + EBITDA_margin + log(Current_ratio)+log(abs(CAPEX))
                        + Under_rank  + VC.Dummy  + Dual_dummy + Internet_dummy + Float 
                        + log(Age+1) + NI + tech +Net_Debt_Ebitda +log(Offer_Price)+ Net_Debt_Ebitda
                        + log(word_sum)+ uncertainty_perc + negative_percent
                        +positive_percent + Litigious_percent + uncert_part1 +uncert_part2+
                          IPO_num_scoop+log(Issued), data = data_new_new_2)




summary(mod_underpr_new_5 )

#м/к
vif(mod_underpr_new_5 )


#нормальность остатков
plot(mod_underpr_new_5  , which =2) 
boxCox(mod_underpr_new )

#влиятельные наблюдения
cook <- cooks.distance(mod_underpr_new)
plot(cook,type = "h")
which(cook > 0.2)

influencePlot(mod_underpr_new_5)

#степени регрессоров
plot(mod_underpr_new_5  ,which =1)
crPlots(mod_underpr_new_5)
resettest(mod_underpr_new_all)


#чистка через AIC
m5  <- stepAIC(mod_underpr_new_5)
summary(m5)
vif(m5)

#степени регрессоров
plot(mod_underpr_new_all ,which =1)
crPlots(mod_underpr_new_all)
resettest(mod_underpr_new_all)

#гетероскедастичность
bptest(m5)


#робастные ошибки
clse = function(mod) { 
  G = length(unique(index(mod,"id")))
  N = length(index(mod,"id"))
  dfa = (G/(G - 1))
  rob = sqrt(diag(dfa*vcovHC(mod, method="arellano", type = "HC1", 
                             cluster = "group")))
  return(rob)
}

#удалим 10-15 влияетльных наблюдений

influencePlot(mod_underpr_new_5)

#удаляем 10 влиятельных наблюдений 
a <- which(rownames(data_new_new) %in% c(297,26,59,295,255,310,127,113,50,27))


data_new_new_2 <- data_new_new[-a,]
View(data_new_new_2)
data_new_new_2$Day_px_market
data_new_new$Day_px_market
#модель без 10 влиятельных наблюдений
mod_underpr_new_10 <- lm(Day_px_market~ log(Total_assets) + log(R_D+1) + Book_ValperShare
                         + ROE + ROA + EBITDA_margin + log(Current_ratio)+log(abs(CAPEX))
                         + Under_rank  + VC.Dummy + Rollup_dummy + Dual_dummy + Internet_dummy + Float 
                         + log(Age+1) + NI + tech +Net_Debt_Ebitda +log(Offer_Price) + Net_Debt_Ebitda
                         + log(word_sum)+ uncertainty_perc + negative_percent
                         +positive_percent + Litigious_percent + uncert_part1 +uncert_part2+
                           IPO_num_scoop+log(Issued), data = data_new_new_2)


summary(mod_underpr_new_10)

#м/к
vif(mod_underpr_new_10)

#нормальность остатков
plot(mod_underpr_new , which =2) 
boxCox(mod_underpr_new )

#влиятельные наблюдения
cook <- cooks.distance(mod_underpr_new)
plot(cook,type = "h")
which(cook > 0.2)

influencePlot(mod_underpr_new_10)

#степени регрессоров
plot(mod_underpr_new ,which =1)
crPlots(mod_underpr_new)
resettest(mod_underpr_new_all)


#чистка через AIC
m10  <- stepAIC(mod_underpr_new_10)
summary(m10 )
vif(m10)

#гетероскедастичность
bptest(m10)

#удалим 25 влияетльных наблюдений

influencePlot(mod_underpr_new_10)

#удаляем 25 влиятельных наблюдений 
a <- which(rownames(data_new_new) %in% c(297,26,59,295,255,310,127,113,50,27,171,162,
                                         31,172,136,10,240,36,246,179,242,273,228,193,327))


data_new_new_2 <- data_new_new[-a,]
View(data_new_new_2)


#модель без gggg влиятельных наблюдений
mod_underpr_new_25<- lm(Day_px_market~ log(Total_assets) + log(R_D+1) + Book_ValperShare
                        + ROE + ROA + EBITDA_margin +log(abs(CAPEX))
                        + Under_rank  + VC.Dummy + Rollup_dummy + Dual_dummy + Internet_dummy + Float 
                        + log(Age+1) + NI + tech +Net_Debt_Ebitda +log(Offer_Price)+ Net_Debt_Ebitda
                        + uncertainty_perc 
                        +positive_percent  + uncert_part1 +uncert_part2+
                          IPO_num_scoop+log(Issued), data = data_new_new_2)


summary(mod_underpr_new_25)

#м/к
vif(mod_underpr_new_25)

#нормальность остатков
plot(mod_underpr_new , which =2) 
boxCox(mod_underpr_new )

#влиятельные наблюдения
cook <- cooks.distance(mod_underpr_new_25)
plot(cook,type = "h")
which(cook > 0.05)

influencePlot(mod_underpr_new_25)

#степени регрессоров
plot(mod_underpr_new ,which =1)
crPlots(mod_underpr_new)
resettest(mod_underpr_new_all)


#чистка через AIC
m25  <- stepAIC(mod_underpr_new_25)
summary(m25 )
vif(m25)

#гетероскедастичность
bptest(m25)




#выгружаем все модели c спроверкой влиятельных наблюдений


#робастные ошибки
clse = function(mod) { 
  G = length(unique(index(mod,"id")))
  N = length(index(mod,"id"))
  dfa = (G/(G - 1))
  rob = sqrt(diag(dfa*vcovHC(mod, method="arellano", type = "HC1", 
                             cluster = "group")))
  return(rob)
}

stargazer(m1,m5,m10,m25, se=list(clse(m1),clse(m5),clse(m10),clse(m25)),  
          title="Устойчивость результатов в модели с рынком к исключению разного количества выбросов (по всем секторам)", type="html",
          df=FALSE, digits=3,out="models_market1.htm")



#ФИНАЛЬНАЯ МОДЕЛЬ ВСЕ СЕКТОРА с УЧЕТом РЫНКА
#сравниваем регрессию без учета рынка и с учетом рынка
#ИТОГОВАЯ МОДЕЛЬ С УЧЕТОМ РЫНКА

#удаляем 10 влиятельных наблюдений 
a <- which(rownames(data_new_new) %in% c(297,26,59,295,255,310,127,113,50,27))


data_new_new_2 <- data_new_new[-a,]
View(data_new_new_2)
describe(data_new_new_2)
summary(data_new_new_2)
#БЕЗ РЫНКА
#модель без 10 влиятельных наблюдений
mod_underpr_new_10 <- lm(Day_Px_Chng~ log(Total_assets) + log(R_D+1) + Book_ValperShare
                         + ROE + ROA + EBITDA_margin + log(Current_ratio)+log(abs(CAPEX))
                         + Under_rank  + VC.Dummy + Rollup_dummy + Dual_dummy + Internet_dummy + Float 
                         + log(Age+1) + NI + tech +Net_Debt_Ebitda +log(Offer_Price) + Net_Debt_Ebitda
                         + log(word_sum)+ uncertainty_perc + negative_percent
                         +positive_percent + Litigious_percent + uncert_part1 +uncert_part2+
                           IPO_num_scoop+log(Issued), data = data_new_new_2)


summary(mod_underpr_new_10)

#м/к
vif(mod_underpr_new_10)

#нормальность остатков
plot(mod_underpr_new , which =2) 
boxCox(mod_underpr_new )

#влиятельные наблюдения
cook <- cooks.distance(mod_underpr_new)
plot(cook,type = "h")
which(cook > 0.2)

influencePlot(mod_underpr_new_10)

#степени регрессоров
plot(mod_underpr_new_10 ,which =1)
crPlots(mod_underpr_new_10)
resettest(mod_underpr_new_10)


#чистка через AIC
m10_ALL_nomarket  <- stepAIC(mod_underpr_new_10)
summary(m10_ALL_nomarket  )

#сравнение короткая против длинной
waldtest(m10_ALL,mod_underpr_new_10 )


#гетероскедастичность
bptest(m10_ALL)

stargazer(m10_ALL_nomarket , se=list(clse(m10_ALL_market)),  
          title="Результаты оценивания регрессии недооценки с учетом рынка(по всем секторам)", type="html",
          df=FALSE, digits=3,out="model_all_market2.htm")


#С РЫНКОМ
#модель без 10 влиятельных наблюдений
mod_underpr_new_10 <- lm(Day_px_market~ log(Total_assets) + log(R_D+1) + Book_ValperShare
                         + ROE + ROA + EBITDA_margin + log(Current_ratio)+log(abs(CAPEX))
                         + Under_rank  + VC.Dummy + Rollup_dummy + Dual_dummy + Internet_dummy + Float 
                         + log(Age+1) + NI + tech +Net_Debt_Ebitda +log(Offer_Price) + Net_Debt_Ebitda
                         + log(word_sum)+ uncertainty_perc + negative_percent
                         +positive_percent + Litigious_percent + uncert_part1 +uncert_part2+
                           IPO_num_scoop+log(Issued), data = data_new_new_2)


summary(mod_underpr_new_10)

#м/к
vif(mod_underpr_new_10)

#нормальность остатков
plot(mod_underpr_new , which =2) 
boxCox(mod_underpr_new )

#влиятельные наблюдения
cook <- cooks.distance(mod_underpr_new)
plot(cook,type = "h")
which(cook > 0.2)

influencePlot(mod_underpr_new_10)

#степени регрессоров
plot(mod_underpr_new_10 ,which =1)
crPlots(mod_underpr_new_10)
resettest(mod_underpr_new_10)


#чистка через AIC
m10_ALL_market  <- stepAIC(mod_underpr_new_10)
summary(m10_ALL_market)
vif(m10_ALL_market )

#сравнение короткая против длинной
waldtest(m10_ALL,mod_underpr_new_10 )


#гетероскедастичность
bptest(m10_ALL)

stargazer(m10_ALL_nomarket ,m10_ALL_market, se=list(clse(m10_ALL_nomarket),clse(m10_ALL_market)),  
          title="Сравнение результатов регрессий недооценки с учетом рынка и без (по всем секторам)", type="html",
          df=FALSE, digits=3,out="model_all_market2.htm")

plot_summs(m10_ALL_nomarket,m10_ALL_market ,point.size =6, 
           robust = list("HC0", "HC0"),
           model.names = c("Без учета рынка", "С учетом рынка")) +
  xlab("Оценка коэффициентов") +
  ylab("Зависимая переменная") +
  ggtitle("Результаты регрессий c учетом рынка и без")






#ОСТАВИМ В ВЫБОРКЕ ТОЛЬКО DUAL-dummy=1

#ОСТАВИМ В ВЫБОРКЕ ТОЛЬКО DUAL-dummy=1
data_new_new_DUAL<- subset(data_new_new, Dual_dummy == 1 )
View(data_new_new_DUAL)

#удаляем 10 влиятельных наблюдений 
a <- which(rownames(NOTECH_new_new) %in% c(13,108,100,162,200,73,28,83,150,114))


NOTECH_new_new_2 <- NOTECH_new_new[-a,]
View(NOTECH_new_new_2)

#модель без 10 влиятельных наблюдений
mod_underpr_new_10_dual <- lm(Day_Px_Chng~ log(Total_assets) + log(R_D+1) +Book_ValperShare
                              + ROE + ROA + EBITDA_margin + log(Current_ratio)+log(abs(CAPEX+1))
                              + Under_rank  + VC.Dummy + Rollup_dummy + tech  + Internet_dummy + Float 
                              + log(Age+1) + NI +Net_Debt_Ebitda +log(Offer_Price)+ Net_Debt_Ebitda
                              + log(word_sum)+ uncertainty_perc + negative_percent
                              +positive_percent + uncert_part1 +uncert_part2+
                                IPO_num_scoop+log(Issued), data = data_new_new_DUAL)




summary(mod_underpr_new_10)

#м/к
vif(mod_underpr_new_10)

#нормальность остатков
plot(mod_underpr_new , which =2) 
boxCox(mod_underpr_new )

#влиятельные наблюдения
cook <- cooks.distance(mod_underpr_new)
plot(cook,type = "h")
which(cook > 0.2)

influencePlot(mod_underpr_new_10)

#степени регрессоров
plot(mod_underpr_new_10 ,which =1)
crPlots(mod_underpr_new_10)
resettest(mod_underpr_new_10)


#чистка через AIC
m10_ALL_dual  <- stepAIC(mod_underpr_new_10_dual)
summary(m10_NOTECH)
vif(m10_NOTECH )


#гетероскедастичность
bptest(m10_NOTECH)

stargazer(m10_ALL_dual, se=list(clse(m10_ALL_dual)),  
          title="Результаты оценивания регрессии по выборке двухклассовых IPO по всем секторам", type="html",
          df=FALSE, digits=3,out="DUAL_ALL.htm")
vif(m10_ALL_dual)


####технологический сектор####
###пробуем по отдельности

TECH <- read.xlsx("TECH_ALL23.xlsx",sheet=2)
#добавляем переменные тональности
sent_data_TH <- read.xlsx("sent_data_TH.xlsx")
TECH23_sent <- merge(TECH ,sent_data_TH , by ="Ticker")
part_all <- merge(part123,part4, by ="Ticker")

TECH23_sent <- merge(TECH23_sent, part_all, by = "Ticker")

#удаляем дупликаты

View(TECH23_sent)

#добавим переменные
TECH23_sent_new<- mutate(TECH23_sent, NI = ifelse(Net_income>0, 1, 0),
                         underpriced = ifelse(Day_Px_Chng>0,0,1),
                         uncertainty_perc = scale(uncertaint_perc),
                         word_sum = sum_row, positive_percent = scale(positive_perc), 
                         Litigious_percent=scale(Litigious_perc), negative_percent = scale(negative_perc),
                         uncert_part1 = scale(uncert_perc_part1),uncert_part2 = scale(uncert_perc_part2))


View(TECH23_sent_new)
cor(TECH_sent_new_new)

#удаляем факторные
TECH_sent_new_new <- dplyr::select(TECH23_sent_new,-Ticker,-Day_Close ,-Change_Close,-Opening_Price, -Sector, -IPO_proceed,
                                   -Total_Share_Float,-IPO.year,
                                   -Total_sum_of_shares ,-Sector,-Sector_tech_notech,-Gross_profit ,-COGS)



ggplot(TECH_new_new_underpriced, aes(x = uncertaint_perc, y = Day_Px_Chng)) +
  geom_point(aes(color = uncertaint_perc)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Соотношение неопределенности и недооценки технологический сектор",
       x = "Неопределенность",
       y = "Недооценка")
#удаляем пропуски
TECH_new_new  <- na.omit(TECH_sent_new_new)
View(TECH_new_new)

TECH_new_new_underpriced<- subset(TECH_new_new, Day_Px_Chng > 0)
View(TECH_new_new_underpriced)
#описательная статистика
describe(TECH_new_new )
summary(TECH_new_new )
str(TECH_new_new )
cor(scale(TECH_sent_new_new$Weak_Modal_sent),TECH_sent_new_new$Day_Px_Chng)
#корреляционная матрица
corrplot(cor(TECH_new_new ))

#удаляем выбивающиеся наблюдения


View(TECH_new_new )



#без преобразований C assets
mod_underpr <- lm(Day_Px_Chng~ Total_assets  + R_D + Net_income+Book_ValperShare
                  +CAPEX + ROE + ROA + EBITDA_margin + Current_ratio
                  + Under_rank  + VC.Dummy + Rollup_dummy + Dual_dummy + Internet_dummy + Float 
                  +Issued+Shareholder_equity + Age + NI + uncertainty_perc + negative_percent + word_sum 
                  +positive_percent +Litigious_percent + uncert_part1 +uncert_part2+
                    Offer_Price + Net_Debt_Ebitda+IPO_num_scoop
                  , data = TECH_new_new )


summary(mod_underpr)
#м/к
vif(mod_underpr)

#нормальность остатков
plot(mod_underpr , which =2) 
??plot
boxCox(mod_underpr)

#влиятельные наблюдения
cook <- cooks.distance(mod_underpr)
plot(cook,type = "h")
which(cook > 0.4)
influencePlot(mod_underpr)

#степени регрессоров
plot(mod_underpr ,which =1)
crPlots(mod_underpr)
resettest(mod_underpr,power = 2)

#выгружаем начальную модель
stargazer(mod_underpr,title="Модель множественной регрессии без преобразований", 
          type="html",df=FALSE, digits=3,out = "Модель множественной регрессии без преобразований1.html")





#модель с преобразованиями


mod_underpr_new <- lm(Day_Px_Chng~ log(Total_assets) + log(R_D+1) +  Net_income+Book_ValperShare
                      + ROE + ROA + EBITDA_margin + log(Current_ratio)+log(abs(CAPEX))
                      + Under_rank  + VC.Dummy + Rollup_dummy + Dual_dummy + Internet_dummy + Float 
                      + log(Age+1) + NI +Net_Debt_Ebitda +log(Offer_Price)+ Net_Debt_Ebitda
                      + log(word_sum)+ uncertainty_perc + negative_percent
                      +positive_percent + Litigious_percent + uncert_part1 +uncert_part2+
                        IPO_num_scoop+log(Issued), data =TECH_new_new)


summary(mod_underpr_new)

#м/к
vif(mod_underpr_new)

#нормальность остатков
plot(mod_underpr_new , which =2) 
boxCox(mod_underpr_new )

#влиятельные наблюдения
cook <- cooks.distance(m0)
plot(cook,type = "h")
which(cook > 0.2)

influencePlot(mod_underpr_new )

#степени регрессоров
plot(mod_underpr_new ,which =1)
crPlots(mod_underpr_new)
resettest(mod_underpr_new_all)

View(data_new_new)
is.na(data_new_new)
#чистка через AIC
m1  <- stepAIC(mod_underpr_new)
summary(m1)
#степени регрессоров
plot(mod_underpr_new_all ,which =1)
crPlots(mod_underpr_new_all)
resettest(mod_underpr_new_all)

#гетероскедастичность
bptest(m1 )
influencePlot(mod_underpr_new_all)



#удаляем 5 влиятельных наблюдений 
a <- which(rownames(TECH_new_new) %in% c(73,110,13,60,28))


TECH_new_new_2 <- TECH_new_new[-a,]
View(TECH_new_new_2)
describe(data_new_new_2)

#модель без 5 влиятельных наблюдений
mod_underpr_new_5 <- lm(Day_Px_Chng~ log(Total_assets) + log(R_D+1) +  Net_income+Book_ValperShare
                        + ROE + ROA + EBITDA_margin + log(Current_ratio)+log(abs(CAPEX))
                        + Under_rank  + VC.Dummy + Rollup_dummy + Dual_dummy + Internet_dummy + Float 
                        + log(Age+1) + NI +Net_Debt_Ebitda +log(Offer_Price)+ Net_Debt_Ebitda
                        + log(word_sum)+ uncertainty_perc + negative_percent
                        +positive_percent + Litigious_percent + uncert_part1 +uncert_part2+
                          IPO_num_scoop+log(Issued), data = TECH_new_new_2)




summary(mod_underpr_new_5 )

#м/к
vif(mod_underpr_new)

#нормальность остатков
plot(mod_underpr_new_5  , which =2) 
boxCox(mod_underpr_new )

#влиятельные наблюдения
cook <- cooks.distance(mod_underpr_new)
plot(cook,type = "h")
which(cook > 0.2)

influencePlot(mod_underpr_new_5)

#степени регрессоров
plot(mod_underpr_new_5  ,which =1)
crPlots(mod_underpr_new_5)
resettest(mod_underpr_new_all)


#чистка через AIC
m5  <- stepAIC(mod_underpr_new_5)
summary(m5)

#степени регрессоров
plot(mod_underpr_new_all ,which =1)
crPlots(mod_underpr_new_all)
resettest(mod_underpr_new_all)

#гетероскедастичность
bptest(m5)


#робастные ошибки
clse = function(mod) { 
  G = length(unique(index(mod,"id")))
  N = length(index(mod,"id"))
  dfa = (G/(G - 1))
  rob = sqrt(diag(dfa*vcovHC(mod, method="arellano", type = "HC1", 
                             cluster = "group")))
  return(rob)
}

#удалим 10-15 влияетльных наблюдений

influencePlot(mod_underpr_new_5)

#удаляем 10 влиятельных наблюдений 
a <- which(rownames(TECH_new_new) %in% c(73,110,13,60,28,111,112,52,109,93))


TECH_new_new_2 <- TECH_new_new[-a,]
View(TECH_new_new_2)

#модель без 10 влиятельных наблюдений
mod_underpr_new_10 <- lm(Day_Px_Chng~ log(Total_assets) + log(R_D+1) +  Net_income+Book_ValperShare
                         + ROE + ROA + EBITDA_margin + log(Current_ratio)+log(abs(CAPEX))
                         + Under_rank  + VC.Dummy + Rollup_dummy + Dual_dummy + Internet_dummy + Float 
                         + log(Age+1) + NI +Net_Debt_Ebitda +log(Offer_Price)+ Net_Debt_Ebitda
                         + log(word_sum)+ uncertainty_perc + negative_percent
                         +positive_percent + Litigious_percent + uncert_part1 +uncert_part2+
                           IPO_num_scoop+log(Issued), data = TECH_new_new_2)


summary(mod_underpr_new_10)

#м/к
vif(mod_underpr_new_10)

#нормальность остатков
plot(mod_underpr_new , which =2) 
boxCox(mod_underpr_new )

#влиятельные наблюдения
cook <- cooks.distance(mod_underpr_new)
plot(cook,type = "h")
which(cook > 0.2)

influencePlot(mod_underpr_new_10)

#степени регрессоров
plot(mod_underpr_new ,which =1)
crPlots(mod_underpr_new)
resettest(mod_underpr_new_all)


#чистка через AIC
m10  <- stepAIC(mod_underpr_new_10)
summary(m10 )


#гетероскедастичность
bptest(m10)

#удалим 25 влияетльных наблюдений

influencePlot(mod_underpr_new_10)

#удаляем 25 влиятельных наблюдений 
#удаляем 10 влиятельных наблюдений 

a <- which(rownames(TECH_new_new) %in% c(73,110,13,60,28,111,112,52,109,93,118,101,17,77,58,42,19,7,86,119))


TECH_new_new_2 <- TECH_new_new[-a,]
View(TECH_new_new_2)



#модель без gggg влиятельных наблюдений
mod_underpr_new_25<- lm(Day_Px_Chng~ log(Total_assets) + log(R_D+1) +  Net_income+Book_ValperShare
                        + ROE + ROA + EBITDA_margin + log(Current_ratio)+log(abs(CAPEX))
                        + Under_rank  + VC.Dummy + Rollup_dummy + Dual_dummy + Internet_dummy + Float 
                        + log(Age+1) + NI +Net_Debt_Ebitda +log(Offer_Price)+ Net_Debt_Ebitda
                        + log(word_sum)+ uncertainty_perc + negative_percent
                        +positive_percent + Litigious_percent + uncert_part1 +uncert_part2+
                          IPO_num_scoop+log(Issued), data = TECH_new_new_2)


summary(mod_underpr_new_25)

#м/к
vif(mod_underpr_new)

#нормальность остатков
plot(mod_underpr_new , which =2) 
boxCox(mod_underpr_new )

#влиятельные наблюдения
cook <- cooks.distance(mod_underpr_new_25)
plot(cook,type = "h")
which(cook > 0.05)

influencePlot(mod_underpr_new_25)

#степени регрессоров
plot(mod_underpr_new ,which =1)
crPlots(mod_underpr_new)
resettest(mod_underpr_new_all)


#чистка через AIC
m25  <- stepAIC(mod_underpr_new_25)
summary(m25 )


#гетероскедастичность
bptest(mod_underpr_new_all_25 )




#выгружаем все модели c спроверкой влиятельных наблюдений


#робастные ошибки
clse = function(mod) { 
  G = length(unique(index(mod,"id")))
  N = length(index(mod,"id"))
  dfa = (G/(G - 1))
  rob = sqrt(diag(dfa*vcovHC(mod, method="arellano", type = "HC1", 
                             cluster = "group")))
  return(rob)
}

stargazer(m0,m5,m10,m25, se=list(clse(m0),clse(m5),clse(m10),clse(m25)),  
          title="Устойчивость результатов к исключению разного количества выбросов (тех.сектор)", type="html",
          df=FALSE, digits=3,out="models_Tech1.htm")



#ФИНАЛЬНАЯ МОДЕЛЬ ВСЕ СЕКТОРА БЕЗ УЧЕТА РЫНКА c ВЫБРОСАМИ И БЕЗ 


#удаляем 10 влиятельных наблюдений 
a <- which(rownames(TECH_new_new) %in% c(73,110,13,60,28,111,112,52,109,93))


TECH_new_new_2 <- TECH_new_new[-a,]
View(TECH_new_new_2)

#модель без 10 влиятельных наблюдений
mod_underpr_new_10 <- lm(Day_Px_Chng~ log(Total_assets) + log(R_D+1) +Book_ValperShare
                         + ROE + ROA + EBITDA_margin + log(Current_ratio)+log(abs(CAPEX))
                         + Under_rank  + VC.Dummy + Rollup_dummy+ Internet_dummy + Float 
                         + log(Age+1) + NI +Net_Debt_Ebitda +log(Offer_Price)+ Net_Debt_Ebitda
                         + log(word_sum)+ uncertainty_perc + negative_percent
                         +positive_percent + Litigious_percent + uncert_part1 +uncert_part2+
                           IPO_num_scoop+log(Issued), data = TECH_new_new_2)




summary(mod_underpr_new_10)

#м/к
vif(mod_underpr_new_10)

#нормальность остатков
plot(mod_underpr_new , which =2) 
boxCox(mod_underpr_new )

#влиятельные наблюдения
cook <- cooks.distance(mod_underpr_new)
plot(cook,type = "h")
which(cook > 0.2)

influencePlot(mod_underpr_new_10)

#степени регрессоров
plot(mod_underpr_new_10 ,which =1)
crPlots(mod_underpr_new_10)
resettest(mod_underpr_new_10)


#чистка через AIC
m10_TECH  <- stepAIC(mod_underpr_new_10)
summary(m10_TECH )
vif(m10_TECH)
vif(m10_ALL)
bptest(m10_ALL )
#гетероскедастичность


bptest(m10_TECH )
ggcoefstats(m10_TECH, 
            title = "Результаты лучшей регрессии технологического сектора", 
            ylab = "Переменные", 
            xlab = "Оценка коэффициента", 
            package = "RColorBrewer",
            palette = "Spectral")

my_colors <- c("#1b1b1b", "#800000", "#000080", "#006400","#800080",
               "#8B4513","#2F4F4F","#FF0000","#0000FF","#FF00FF","#008080","#FF4500")
ggcoefstats(m10_TECH, 
            title = "Результаты лучшей регрессии технологического сектора", 
            ylab = "Переменные", 
            xlab = "Оценка коэффициента", 
            package = "RColorBrewer",
            palette = "Set2")


??ggcoefstats
stargazer(m10_TECH , se=list(clse(m10_TECH)),  
          title="Результаты оценивания регрессии недооценки по технологическому сектору", type="html",
          df=FALSE, digits=3,out="model_TECHFINall14.htm")



#ФИНАЛЬНАЯ МОДЕЛЬ БЕЗ ОЧИСТКИ ВЫБРОСОВ



mod_underpr_new_no <- lm(Day_Px_Chng ~ log(Total_assets) + log(R_D+1) +Book_ValperShare
                         + ROE + ROA  + log(Current_ratio)+log(abs(CAPEX))
                         + Under_rank  + VC.Dummy + Rollup_dummy+ Internet_dummy + Float 
                         + log(Age+1) + NI+log(Offer_Price)+ EBITDA_margin
                         +log(word_sum)+ uncertainty_perc + negative_percent
                         +positive_percent + Litigious_percent + uncert_part1 +uncert_part2+
                           IPO_num_scoop, data = TECH_new_new)




summary(mod_underpr_new_no)

#м/к
vif(mod_underpr_new_10)

#нормальность остатков
plot(mod_underpr_new , which =2) 
boxCox(mod_underpr_new )

#влиятельные наблюдения
cook <- cooks.distance(mod_underpr_new)
plot(cook,type = "h")
which(cook > 0.2)

influencePlot(mod_underpr_new_10)

#степени регрессоров
plot(mod_underpr_new_10 ,which =1)
crPlots(mod_underpr_new_10)
resettest(mod_underpr_new_10)


#чистка через AIC
mno_TECH  <- stepAIC(mod_underpr_new_no)
summary(mno_TECH)
vif(m10_TECH)

#гетероскедастичность
bptest(mno_TECH )


#выгружаем и сравниваем две модели без выбросов и с выбросами 
stargazer(mno_TECH, m10_TECH , se=list(clse(mno_TECH),clse(m10_TECH)),  
          title="Сравнение моделей с выбросами и без по технологическому сектору", type="html",
          df=FALSE, digits=3,out="с выбросами и без по технологическому сектору14.htm")


















# модель недооценки Technology -------------------------------------------------------
# недооценка обычная регрессия--------------------------------------------------------------

# недооценка логит регрессия--------------------------------------------------------------
####LOGIT######

mod_logit <- glm(underpriced~ log(R_D+1) +Book_ValperShare
                 + ROE + ROA + log(Current_ratio)+log(abs(CAPEX))
                 + Under_rank   + Rollup_dummy + Dual_dummy + Internet_dummy + Float 
                 + log(Age+1) + NI  +log(Offer_Price)
                 + log(word_sum)+ uncertainty_perc + negative_percent
                 +positive_percent + uncert_part1 +uncert_part2+
                   IPO_num_scoop+log(Issued),data = TECH_new_new,family = binomial, x = TRUE )
summary(mod_logit)


vif(mod_logit)

#чистка через Акаике
mod_logit_aic_tech <- stepAIC(mod_logit)
summary(mod_logit_aic_tech)
vif(mod_logit_aic_tech)
#какая модель лучше?

waldtest(mod_logit_aic_all,mod_logit)

#предельные эффекты для среднего

mod_dop_all <- maBina(mod_logit_aic_all, x.mean = TRUE)
mod_dop[1:3]

#выгрузка общей таблицы с предельных эффектов
stargazer(mod_logit_aic_all, mod_dop_all, 
          title="Финальная логит модель для нетехнологического сектора", type="html",
          df=FALSE, digits=3, out = "Общая логит таблица ыS.html")

#качество модели
hitmiss(mod_logit_aic_all)
#оптимальная граница для классификации


#качество модели
hitmiss(mod_logit_aic_all)

b <- hitmiss(mod_logit, k = S)
str(b)

#визуализация 

Pr <- predict(mod_logit_aic_all, type ="response",NOTECH_new_new_2)
Pr_2 <- ifelse(Pr>0.5,1,0)
Y <- as.factor(NOTECH_new_new_2$underpriced)
Y <- as.numeric(NOTECH_new_new_2$underpriced)
Y_pr <- as.factor(Pr_2 )
mod_caret <- caret::confusionMatrix(Y_pr,Y)

# P-Value [Acc > NIR] : 0.1085  => модели одинакового качества
#если после AIC, то  P-Value [Acc > NIR] : 0.01547  
install.packages("pROC")
library(pROC)
??plotROC
install.packages("lares")
library(lares)

lares::mplot_roc(Y ,Pr_2)
#ROC
pROC::plot.roc(NOTECH_new_new_2$underpriced, fitted(mod_logit))

roc.data <- roc(Y,Pr_2)
ROC_c <- roc( fitted(mod_logit_aic_all), as.factor(Y)) 
qplot(x=roc.data$fpr,y=roc.data$tpr, geom = "line")










# lasso -------------------------------------------------------------------

describe(TECH_new_new)
data <- data.frame(TECH_new_new$Total_assets,TECH_new_new$Revenue,TECH_new_new$Float,TECH_new_new$Book_ValperShare,TECH_new_new$EBITDA,TECH_new_new$R_D,
                   TECH_new_new$Under_rank,TECH_new_new$VC.Dummy,TECH_new_new$Rollup_dummy,TECH_new_new$Dual_dummy,TECH_new_new$Internet_dummy,TECH_new_new$Age,
                   TECH_new_new$ROE,TECH_new_new$ROA,TECH_new_new$NI,TECH_new_new$IPO_num_scoop,
                   TECH_new_new$Offer_Price,TECH_new_new$Current_ratio, TECH_new_new$Net_debt, TECH_new_new$CAPEX,TECH_new_new$Net_Debt_Ebitda,
                   TECH_new_new$negative_percent,TECH_new_new$positive_percent,TECH_new_new$Litigious_percent,
                   TECH_new_new$uncert_part1,TECH_new_new$uncert_part2,
                   TECH_new_new$IPO_num_scoop, TECH_new_new$Issued)
View(data)
mod_lasso <- glmnet(data,TECH_new_new$Day_Px_Chng, alpha = 1)
plot(mod_lasso,label =TRUE)

#выбор лямбды через кроссвалидацию

data <- as.matrix(data)
mod_lasso2 <- cv.glmnet(data,TECH_new_new$Day_Px_Chng, alpha = 1)
plot(mod_lasso2)
par("mar")

#лямбды
mod_lasso2$lambda.min
mod_lasso2$lambda.1se

#подставим в модель

mod_lasso3 <- glmnet(data,TECH_new_new$Day_Px_Chng, alpha = 1, lambda = mod_lasso2$lambda.1se)
coef(mod_lasso3)

#подбор альфы
TR <- trainControl(method = "repeatedcv", repeats = 5)
mod_lasso4 <- train(Day_Px_Chng ~log(R_D+1) +Book_ValperShare
                    + ROE + ROA + EBITDA_margin + log(Current_ratio)+log(abs(CAPEX))
                    + Under_rank  + VC.Dummy + Rollup_dummy+ Internet_dummy + Float 
                    + log(Age+1) + NI  +log(Offer_Price)+ Net_Debt_Ebitda
                    + log(word_sum) + negative_percent
                    +positive_percent + Litigious_percent + uncert_part1 +uncert_part2+
                      IPO_num_scoop+log(Issued) ,data =TECH_new_new, method = "glmnet",trControl =TR)

mod_lasso4$bestTune
coef(mod_lasso4$finalModel,mod_lasso4$bestTune$lambda)


# финальная модель
mod_lasso5 <- glmnet(data,TECH_new_new$Day_Px_Chng, alpha = mod_lasso4$bestTune$alpha ,lambda = mod_lasso4$bestTune$lambda)
coef(mod_lasso5)
plot(mod_lasso5,label =TRUE)














# PCA ---------------------------------------------------------------------
attach(TECH_new_new)
data_PCA <- select (TECH_new_new ,Total_assets,R_D,Net_income,Book_ValperShare,
                    CAPEX,ROE,ROA,EBITDA_margin,Current_ratio,Under_rank,VC.Dummy,Rollup_dummy,Dual_dummy,Internet_dummy,
                    Float,Issued,Shareholder_equity,Age,NI,uncertainty_perc,negative_percent, word_sum, 
                    positive_percent,Litigious_percent,uncert_part1,uncert_part2,
                    Offer_Price,Net_Debt_Ebitda,IPO_num_scoop,Day_Px_Chng, Issued)
PCA_mod_tech <- PCA(data_PCA)
#оптимальное количество главных компонент
# Visualize eigenvalues
fviz_eig(PCA_mod_tech)
fviz_eig(PCA_mod_tech,
         addlabels = TRUE,
         ylim = c(0, 30),
         ggtheme = theme_classic()) + 
  labs(title = "График каменистой осыпи",
       x = "Главная компонента",
       y = "Процент объясненной дисперсии")
??fviz_eig
#первые две компоненты позволяют объяснить всего лишь около 32% изменения всех данных

#визуализация
fviz_pca_var(PCA_mod_tech, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

#удаляем плохо объясняемые переменные и сильно коррелированные
data_PCA <- select (TECH_new_new ,Total_assets,
                    CAPEX,VC.Dummy,ROA,
                    Age,NI,negative_percent,
                    word_sum,Litigious_percent
                    
)
PCA_mod_tech <- PCA(data_PCA)

#визуализация
fviz_pca_var(PCA_mod_tech, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

#интерпретации главных компонент
corrplot(PCA_mod_tech$var$coord, is.corr = FALSE)

#интерпретация главных компонент через квадраты косинусов (квадраты коэффициентов матрицы нагрузок)
corrplot(PCA_mod_tech$var$cos2, is.corr = FALSE)

# переменные,Которые  удается хорошо приблизить за счет первых двух компонент
fviz_cos2(PCA_mod_tech, choice = "var", axes = 1:2)

#если добавить еще одну
fviz_cos2(PCA_mod_tech, choice = "var", axes = 1:3)

#иерархическая кластеризация
mod_clust <- HCPC(data_PCA)

fviz_dend(mod_clust, cex = 0.6, lwd = 0.8, k = 4,
          rect = TRUE,
          k_colors = "jco",
          rect_border = "jco",
          rect_fill = TRUE,
          type = "phylogenic", repel = TRUE)

plot(mod_clust, choice = "3D.map", angle = 60, ind.names = FALSE)
fviz_dend(mod_clust, cex = 0.7, palette = "jco", rect = TRUE, rect_fill = TRUE, 
          rect_border = "jco", labels_track_height = 0.8)
fviz_cluster(mod_clust, palette = "jco", repel = TRUE, geom = "point")

#график каменистой осыпи
fviz_nbclust(data_PCA, kmeans, "wss")

#иерархическая кластеризация

mod_clust_h <- hclust(dist(data_PCA))
fviz_dend(mod_clust_h)

#а если без главных компонент
mod_clust_h_2 <- hclust(dist(data_PCA))
fviz_dend(mod_clust_h_2)
#что можно достать из объекта?
mod_clust$desc.var
mod_clust$desc.axes
mod_clust$data.clust$clust
table(mod_clust$data.clust$clust)

#модель 3
mod_clust$data.clust
mod_clust$desc.axes
mod_clust$desc.ind


library(e1071)
library(mlbench)
library(ggplot2)
library(JOUSBoost)
library(caret)
library(dplyr)
library(randomForest)
library(AER)
library(earth)
library(gbm)
library("earth")
library(RColorBrewer) # Откроем библиотеку RColorBrewer:
display.brewer.all() # Посмотрим, какие в ней имеются палитры

# выберем цвета из палитры Set2 по количеству секторов в круге:
colors = brewer.pal(10,"Set1")

# И используем их при визуализации
par(mar = c(5, 5, 5, 5)) # установим поля
pie(okr$ХимЭкспорт, names2, main = "Доля федеральных округов в экспорте \n продукции химической промышленности", col=colors)

#данные все для ML
data_all <- read.xlsx("DATA_ALL23.xlsx")

data_all_sent <- read.xlsx("DATA_ALL23_sent.xlsx")

data_ALL_sent_final <- merge(data_all,data_all_sent, by = "Ticker")
data_ALL_sent_final <- merge(data_ALL_sent_final,part_all, by ="Ticker")




#добавим переменные
data_new <- mutate(data_ALL_sent_final,   NI = ifelse(Net_income>0, 1, 0),  
                   tech = ifelse (Sector == "Tech", 1, 0),
                   underpriced = ifelse(Day_Px_Chng>0,1,0),
                   uncertainty_perc = scale(uncertaint_perc),
                   word_sum = sum_row, positive_percent = scale(positive_perc), 
                   Litigious_percent=scale(Litigious_perc), negative_percent = scale(negative_perc),
                   uncert_part1 = scale(uncert_perc_part1),uncert_part2 = scale(uncert_perc_part2))

#удаляем факторные и ненужные 
data_new_new_ml <- dplyr::select(data_new,-IPO.year, -Ticker,-Day_Close ,-Change_Close,-Opening_Price,-Sector2, -Sector, 
                                 -IPO_proceed, 
                                 -sum_row,-Change_Opening, -Market_Capitalization, -Day_Px_Chng)


#удаляем ненужные текстовые переменные
data_new_new_ml_ <- data_new_new_ml[,-c(31:39)]
data_new_new_ml_2 <- data_new_new_ml_[,-c(38:45)]
data_new_new_ml_3 <- data_new_new_ml_2 [,-c(45:52)]
data_new_new_ml_4 <- data_new_new_ml_3[,-c(52:59)]
data_new_new_ml_5 <- data_new_new_ml_4[,-c(59:66)]
data_new_new_ml <- data_new_new_ml_5 

#должно быть  75 колонок           




#удаляем пропуски
data_new_new_ml <- na.omit(data_new_new_ml)
#описательная статистика
describe(data_new_new_ml)
summary(data_new_new_ml)
str(data_new_new_ml)
View(data_new_new_ml)
#разделение на две подвыборки
set.seed(391)
# class зависимая переменная, 75 % в выборку обучения
inTraining <- createDataPartition(data_new_new_ml$underpriced , p = .75, list = FALSE)
#sort(sample(1:length(Sonar$Class), size = length(Sonar$Class)/4*3, replace = FALSE))
training <-data_new_new_ml [ inTraining,]
testing  <- data_new_new_ml [-inTraining,]
training$underpriced
#настройка десяти-кратной кросс-валидации
#первый параметр отвечает за то насколько блоков мы разобьем данные (на 9 кусочках обучаем на 10 качество модели измеряем)
#запиши 10 р квадратов и так 10 раз
fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10)

#number - "порядок" кросс-валидации (к)
#repeats - количество повторений k-кратной кросс-валидации

#методы:
#"cv" - кросс-валидация
#"repeatedcv" - повторная кросс-валидация

#метрики "Accuracy", "Rsquared"
#если задавать метрику "ROC" (площадь под ROC кривой), то нужно отдельно 
#оценивать веротяности классов classProbs = TRUE и summaryFunction = twoClassSummary

#обучение модели через бустинг
set.seed(825)
gbmFit1 <- caret::train(underpriced ~ ., data = training, 
                        method = "gbm", 
                        trControl = fitControl,
                        verbose = FALSE)
gbmFit1
gbmFit1$bestTune
gbmFit1$results

#verbose = FALSE - нужно именно для gbm

#можно отдельно задать сетку параметров для перебора
gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 10), 
                        n.trees = (1:15)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

?expand.grid
#обучение
set.seed(981)
gbmFit2 <- caret::train(underpriced  ~ ., data = training, 
                        method = "gbm", 
                        trControl = fitControl, 
                        verbose = FALSE, 
                        tuneGrid = gbmGrid)
gbmFit2
getTrainPerf(gbmFit2)
gbmFit2$bestTune
View(training)
#визаулизируем
plot(gbmFit2)

#собственные цвета
ggplot(gbmFit2) + geom_point(col = "darkblue", size = 0.7) + scale_color_manual(values = c("#550445", "#c96588", "#5f73b3","#006400")) + theme_bw() 
+ theme(legend.position = "bottom")+labs(title = "Название графика")

#другая визуализация
plot(gbmFit2,  plotType = "level",
     scales = list(x = list(rot = 90)))
ggplot(gbmFit2,  plotType = "level")
#scales = list(x = list(rot = 90)) - параметр отображения подписей осей


summary_proxy <- as.data.frame(summary(
  gbmFit2, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2,
))

ggplot(summary_proxy, aes(x = reorder(var, rel.inf), y = rel.inf, fill = rel.inf)) + geom_col(color = "black") + coord_flip() + theme_bw() + ylab("Относительная важность переменной") + xlab("") + labs("Значения") +
  scale_fill_gradient2(low="#d2cdca", mid="#929fb0",
                       high="#4a5060", midpoint=20) + theme(legend.position = "bottom")


ggplot(head(summary_proxy, 10), aes(x = reorder(var, rel.inf), y = rel.inf, fill = rel.inf)) +
  geom_col(color = "black") +
  coord_flip() +
  theme_bw() +
  ylab("Относительная важность переменной") +
  xlab("") +
  labs("Значения") +
  scale_fill_gradient2(low="#FF7F50", mid="#FF4500",
                       high="#DC143C", midpoint=3) +
  theme(legend.position = "bottom")

#scales = list(x = list(rot = 90)) - параметр отображения подписей осей
library(gbm)


#оценим модель через максимизацию площади под ROC кривой

##заменим в данных 1/0 на underpriced/overpriced
#данные все для ML
data_all <- read.xlsx("DATA_ALL23.xlsx")

data_all_sent <- read.xlsx("DATA_ALL23_sent.xlsx")

data_ALL_sent_final <- merge(data_all,data_all_sent, by = "Ticker")
data_ALL_sent_final <- merge(data_ALL_sent_final,part_all, by ="Ticker")




#добавим переменные
data_new <- mutate(data_ALL_sent_final,   NI = ifelse(Net_income>0, 1, 0),  
                   tech = ifelse (Sector == "Tech", 1, 0),
                   underpriced = ifelse(Day_Px_Chng>0,"yes","no"),
                   uncertainty_perc = scale(uncertaint_perc),
                   word_sum = sum_row, positive_percent = scale(positive_perc), 
                   Litigious_percent=scale(Litigious_perc), negative_percent = scale(negative_perc),
                   uncert_part1 = scale(uncert_perc_part1),uncert_part2 = scale(uncert_perc_part2))

#удаляем факторные и ненужные 
data_new_new_ml <- dplyr::select(data_new,-IPO.year, -Ticker,-Day_Close ,-Change_Close,-Opening_Price,-Sector2, -Sector, 
                                 -IPO_proceed, 
                                 -sum_row,-Change_Opening, -Market_Capitalization, -Day_Px_Chng)


#удаляем ненужные текстовые переменные
data_new_new_ml_ <- data_new_new_ml[,-c(31:39)]
data_new_new_ml_2 <- data_new_new_ml_[,-c(38:45)]
data_new_new_ml_3 <- data_new_new_ml_2 [,-c(45:52)]
data_new_new_ml_4 <- data_new_new_ml_3[,-c(52:59)]
data_new_new_ml_5 <- data_new_new_ml_4[,-c(59:66)]
data_new_new_ml <- data_new_new_ml_5 

#должно быть  75 колонок           

#удаляем пропуски
data_new_new_ml <- na.omit(data_new_new_ml)

#разделение на две подвыборки
set.seed(391)
# class зависимая переменная, 75 % в выборку обучения
inTraining <- createDataPartition(data_new_new_ml$underpriced , p = .75, list = FALSE)
#sort(sample(1:length(Sonar$Class), size = length(Sonar$Class)/4*3, replace = FALSE))
training <-data_new_new_ml [ inTraining,]
testing  <- data_new_new_ml [-inTraining,]

fitControl <- trainControl(method = "cv",
                           number = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)
#оценка модели
set.seed(825)


gbmFit3 <- caret::train(underpriced ~ . , data = training, 
                        method = "gbm", 
                        trControl = fitControl, 
                        verbose = FALSE, 
                        tuneGrid = gbmGrid,
                        metric = "ROC")


gbmFit3$bestTune
getTrainPerf(gbmFit3)

#построение прогнозов
Pr1 <- predict(gbmFit3, newdata = training)
Pr1

#построение прогнозируемых вероятностей
Pr2 <- predict(gbmFit3, newdata = training, type = "prob")
Pr2

#оценим через случайный лес
#method = 'rf'

fitControl <- trainControl(method = "cv",
                           number = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)
mod_rf <- caret::train(underpriced ~ ., data = training, trControl = fitControl, 
                       metric = "ROC", method = "rf")
mod_rf

#таблица сопряженности
confusionMatrix.train(mod_rf)

#сравним модели
resamps <- resamples(list(GBM = gbmFit3,
                          RF = mod_rf))
summary(resamps)

#визуализация
bwplot(resamps, layout = c(1, 3))
dotplot(resamps, metric = "Spec")

#поиск различий
difValues <- diff(resamps)
summary(difValues)

#пример обучения моделей без найтроки параметров
fitControl <- trainControl(method = "none", classProbs = TRUE)

#обучение модели
set.seed(825)
gbmFit4 <- caret::train(Class ~ ., data = training, 
                        method = "gbm", 
                        trControl = fitControl, 
                        verbose = FALSE, 
                        tuneGrid = data.frame(interaction.depth = 4,
                                              n.trees = 100,
                                              shrinkage = .1,
                                              n.minobsinnode = 20),
                        metric = "ROC")
gbmFit4
summary(gbmFit4)
#можно делать прогнозы

#######
# SVM #
#######

#method = "svmPoly" - полиномиальное ядро
#method = "svmLinear" - линейное ядро 
#method = "svmRadial" - радиально базисная фнукция
TR <- trainControl(method = "cv",
                   number = 10,
                   classProbs = TRUE,
                   summaryFunction = twoClassSummary)


#логистическая регрессия

mod_log <- caret::train(underpriced ~ ., data = training, trControl = TR,
                        family = "binomial", method = "glm")
mod_log

#lasso

#LASSO 

mod_lasso <- train(underpriced ~ .,data = training, method = "glmnet",trControl =TR)

#случайный лес
mod_log_tr <- caret::train(underpriced ~ ., data = training, trControl = TR,
                           method = "rf", tuneLength = 3)

#gmb
mod_log_gmb <- caret::train(underpriced ~ ., data = training, trControl = TR,
                            method = "gbm", tuneLength = 3, verbose = FALSE)

#svm
mod_log_svm <- caret::train(underpriced ~ ., data = training, trControl = TR,
                            method = "svmRadial", tuneLength = 3, verbose = FALSE)


#сравнение
Comparison <- resamples(list(Logit= mod_log,
                             Lasso = mod_lasso,
                             RF = mod_log_tr,
                             GBM = mod_log_gmb,
                             SVM = mod_log_svm))
bwplot(Comparison)


summary(Comparison)
bwplot(Comparison)
dotplot(Comparison, metric = "Accuracy")


#c цветами 

myColours <- brewer.pal(6,"BuGn")
my.settings <- list(
  superpose.polygon=list(col=myColours[2:5], border="transparent"),
  strip.background=list(col=myColours[6]),
  strip.border=list(col="black")
)
bwplot(Comparison,  col = "black", fill = "#3CB371", par.settings = my.settings, par.strip.text=list(col="white", font=2))
?bwplot
ggplot(Comparison3)
dotplot(Comparison3, par.strip.text=list(col="white", font=2), par.settings = my.settings)









#поиск различий
Comparison2 <- diff(Comparison)
summary(Comparison2)


#какая переменная была самой важной?
ggplot(varImp(mod_log))


varImp(mod_log)
summary(mod_log)

#каково качество модели?
confusionMatrix(mod_log)

#получаем статистику модели
getTrainPerf(mod_log)

#можно ли сразу узнать площадь под ROC?
TR_ROC <- trainControl(method = "cv", number = 10, classProbs = TRUE, 
                       summaryFunction = twoClassSummary)
mod_log <- caret::train(A ~ . - affairs, data = data, trControl = TR_ROC,
                        family = "binomial", method = "glm", metric = "ROC")
mod_log

#для регрессии: регуляризация - добавляем шатрф за
#"ridge" - сумму квадартов коэффициентов
#"lasso" - сумму модлуей коэффициентов







####тоже самое только с зависимой переменной ДОХОДНОСТИ####


#данные все для ML
data_all <- read.xlsx("DATA_ALL23.xlsx")

data_all_sent <- read.xlsx("DATA_ALL23_sent.xlsx")

data_ALL_sent_final <- merge(data_all,data_all_sent, by = "Ticker")
data_ALL_sent_final <- merge(data_ALL_sent_final,part_all, by ="Ticker")




#добавим переменные
data_new <- mutate(data_ALL_sent_final,   NI = ifelse(Net_income>0, 1, 0),  
                   tech = ifelse (Sector == "Tech", 1, 0),
                   underpriced = ifelse(Day_Px_Chng>0,1,0),
                   uncertainty_perc = scale(uncertaint_perc),
                   word_sum = sum_row, positive_percent = scale(positive_perc), 
                   Litigious_percent=scale(Litigious_perc), negative_percent = scale(negative_perc),
                   uncert_part1 = scale(uncert_perc_part1),uncert_part2 = scale(uncert_perc_part2))

#удаляем факторные и ненужные 
data_new_new_ml <- dplyr::select(data_new,-IPO.year, -Ticker,-Day_Close ,-Change_Close,-Opening_Price,-Sector2, -Sector, 
                                 -IPO_proceed, 
                                 -sum_row,-Change_Opening, -Market_Capitalization, -underpriced)


#удаляем ненужные текстовые переменные
data_new_new_ml_ <- data_new_new_ml[,-c(31:39)]
data_new_new_ml_2 <- data_new_new_ml_[,-c(38:45)]
data_new_new_ml_3 <- data_new_new_ml_2 [,-c(45:52)]
data_new_new_ml_4 <- data_new_new_ml_3[,-c(52:59)]
data_new_new_ml_5 <- data_new_new_ml_4[,-c(59:66)]
data_new_new_ml <- data_new_new_ml_5 

#должно быть  74 колонок           




#удаляем пропуски
data_new_new_ml <- na.omit(data_new_new_ml)
#описательная статистика
describe(data_new_new_ml)
summary(data_new_new_ml)
str(data_new_new_ml)
View(data_new_new_ml)
#разделение на две подвыборки
set.seed(998)
# class зависимая переменная, 75 % в выборку обучения
inTraining <- createDataPartition(data_new_new_ml$Day_Px_Chng , p = .75, list = FALSE)
#sort(sample(1:length(Sonar$Class), size = length(Sonar$Class)/4*3, replace = FALSE))
training <-data_new_new_ml [ inTraining,]
testing  <- data_new_new_ml [-inTraining,]

#настройка десяти-кратной кросс-валидации
#первый параметр отвечает за то насколько блоков мы разобьем данные (на 9 кусочках обучаем на 10 качество модели измеряем)
#запиши 10 р квадратов и так 10 раз
fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10)

#number - "порядок" кросс-валидации (к)
#repeats - количество повторений k-кратной кросс-валидации

#методы:
#"cv" - кросс-валидация
#"repeatedcv" - повторная кросс-валидация

#метрики "Accuracy", "Rsquared"
#если задавать метрику "ROC" (площадь под ROC кривой), то нужно отдельно 
#оценивать веротяности классов classProbs = TRUE и summaryFunction = twoClassSummary

#обучение модели через бустинг
set.seed(825)
gbmFit1 <- caret::train(Day_Px_Chng ~ ., data = training, 
                        method = "gbm", 
                        trControl = fitControl,
                        verbose = FALSE)
gbmFit1
gbmFit1$bestTune
gbmFit1$results

#verbose = FALSE - нужно именно для gbm

#можно отдельно задать сетку параметров для перебора
gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 10), 
                        n.trees = (1:15)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

?expand.grid
#обучение
set.seed(825)
gbmFit2 <- caret::train(Day_Px_Chng  ~ ., data = training, 
                        method = "gbm", 
                        trControl = fitControl, 
                        verbose = FALSE, 
                        tuneGrid = gbmGrid)
gbmFit2
getTrainPerf(gbmFit2)
gbmFit2$bestTune
training$Day_Px_Chng 
#визаулизируем
plot(gbmFit2)

#собственные цвета
ggplot(gbmFit2) + geom_point(col = "darkblue", size = 0.7) + scale_color_manual(values = c("#550445", "#c96588", "#5f73b3","#006400")) + theme_bw() 
+ theme(legend.position = "bottom")+labs(title = "Название графика")

#другая визуализация
plot(gbmFit2,  plotType = "level",
     scales = list(x = list(rot = 90)))
ggplot(gbmFit2,  plotType = "level")
#scales = list(x = list(rot = 90)) - параметр отображения подписей осей


summary_proxy <- as.data.frame(summary(
  gbmFit2, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2,
))

ggplot(summary_proxy, aes(x = reorder(var, rel.inf), y = rel.inf, fill = rel.inf)) + geom_col(color = "black") + coord_flip() + theme_bw() + ylab("Относительная важность переменной") + xlab("") + labs("Значения") +
  scale_fill_gradient2(low="#d2cdca", mid="#929fb0",
                       high="#4a5060", midpoint=20) + theme(legend.position = "bottom")


ggplot(head(summary_proxy, 10), aes(x = reorder(var, rel.inf), y = rel.inf, fill = rel.inf)) +
  geom_col(color = "black") +
  coord_flip() +
  theme_bw() +
  ylab("Относительная важность переменной") +
  xlab("") +
  labs("Значения") +
  scale_fill_gradient2(mid="#AFEEEE", low="#E0FFFF",
                       high="#008080", midpoint=5) +
  theme(legend.position = "bottom")

#scales = list(x = list(rot = 90)) - параметр отображения подписей осей
library(gbm)







#оценим модель
fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10)
#оценка модели
set.seed(825)

training$Day_Px_Chng <- as.factor(training$Day_Px_Chng)

gbmFit3 <- train(Day_Px_Chng   ~ . , data = training, 
                 method = "gbm", 
                 trControl = fitControl, 
                 verbose = FALSE, 
                 tuneGrid = gbmGrid)


gbmFit3$bestTune
getTrainPerf(gbmFit3)

#построение прогнозов
Pr1 <- predict(gbmFit3, newdata = training)
Pr1

#построение прогнозируемых вероятностей
Pr2 <- predict(gbmFit3, newdata = training, type = "prob")
Pr2








#оценим через случайный лес 
#method = 'rf'

fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10)

mod_rf <- caret::train(Day_Px_Chng ~ ., data = training, trControl = fitControl, 
                       metric = "Rsquared", method = "rf")
mod_rf

#таблица сопряженности
confusionMatrix.train(mod_rf)

#сравним модели
resamps <- resamples(list(GBM = gbmFit3,
                          RF = mod_rf))
summary(resamps)

#визуализация
bwplot(resamps, layout = c(1, 3))
dotplot(resamps, metric = "Spec")

#поиск различий
difValues <- diff(resamps)
summary(difValues)

#пример обучения моделей без найтроки параметров
fitControl <- trainControl(method = "none", classProbs = TRUE)

#обучение модели
set.seed(825)
gbmFit4 <- caret::train(Class ~ ., data = training, 
                        method = "gbm", 
                        trControl = fitControl, 
                        verbose = FALSE, 
                        tuneGrid = data.frame(interaction.depth = 4,
                                              n.trees = 100,
                                              shrinkage = .1,
                                              n.minobsinnode = 20),
                        metric = "ROC")
gbmFit4
summary(gbmFit4)
#можно делать прогнозы

#######
# SVM #
#######

#method = "svmPoly" - полиномиальное ядро
#method = "svmLinear" - линейное ядро 
#method = "svmRadial" - радиально базисная фнукция

#обычная регрессия
x <- runif(100, -5, 5)
eps <- rnorm(100, 0, 3)
y <- 3 + 5*x + eps
data <- data.frame(y, x)

#10-кратная кросс-валидация
TR <- trainControl(method = "cv", number = 10)
mod_lm <- caret::train(y ~ x, method = "lm", data = data, trControl = TR)
mod_lm

#добавим квадрат и сравним модели
TR <- trainControl(method = "cv", number = 10)
mod_lm_2 <- caret::train(y ~ x + I(x^2), method = "lm", data = data, trControl = TR)

#сравнение
Comparison <- resamples(list(mod_lm, mod_lm_2))
summary(Comparison)
bwplot(Comparison)
dotplot(Comparison, metric = "Rsquared")

#поиск различий
Comparison2 <- diff(Comparison)
summary(Comparison2)

#добавим случайный лес
mod_rf_lm <- caret::train(y ~ x, method = "rf", data = data, trControl = TR)

#сравним
Comparison <- resamples(list(mod_lm, mod_lm_2, mod_rf_lm))
summary(Comparison)
bwplot(Comparison)

#лассо регрессия


mod_lasso <- glmnet(data_new_new_ml[,-25],data_new_new_ml[,25], alpha = 1)
plot(mod_lasso,label =TRUE)

#выбор лямбды через кроссвалидацию

data_new_new_ml <- as.matrix(data_new_new_ml)
mod_lasso2 <- cv.glmnet(data_new_new_ml[,-25],data_new_new_ml[,25], alpha = 1)
plot(mod_lasso2)

par("mar")

#лямбды
mod_lasso2$lambda.min
mod_lasso2$lambda.1se

#подставим в модель

mod_lasso3 <- glmnet(data_new_new_ml[,-25],data_new_new_ml[,25], alpha = 1, lambda = mod_lasso2$lambda.1se)
coef(mod_lasso3)

#подбор альфы
TR <- trainControl(method = "repeatedcv", repeats = 10)
mod_lasso4 <- train(Day_Px_Chng ~. ,data =data_new_new_ml, method = "glmnet",trControl =TR)

mod_lasso4$bestTune
coef(mod_lasso4$finalModel,mod_lasso4$bestTune$lambda)


# финальная модель
mod_lasso5 <- glmnet(data_new_new_ml[,-25],data_new_new_ml[,25], alpha = mod_lasso4$bestTune$alpha ,lambda = mod_lasso4$bestTune$lambda)
coef(mod_lasso5)
plot(mod_lasso5,label =TRUE)


#LASSO 
#подбор альфы
TR <- trainControl(method = "repeatedcv", repeats = 10)
mod_lasso <- train(Day_Px_Chng ~.,data = training, method = "glmnet",trControl =TR, tuneGrid=expand.grid(
  .alpha=1,
  .lambda=seq(0, 0.1, by = 0.01)))

mod_lasso$bestTune
coef(mod_lasso$finalModel,mod_lasso$bestTune$lambda)
View(training)
# финальная модель
mod_lasso_final <- glmnet(training[,-25],training[,25], alpha = mod_lasso$bestTune$alpha ,lambda = mod_lasso$bestTune$lambda)
coef(mod_lasso_final)
plot(mod_lasso_final,label =TRUE)
?trainControl

#обычная регреесия

mod_lm <- train(Day_Px_Chng ~ ., data = training, method = "lm" ,trControl = TR)

#случайный лес
mod_log_tr <- caret::train(Day_Px_Chng ~ ., data = training, trControl = TR,
                           method = "rf", tuneLength = 3)

#gmb
mod_log_gmb <- caret::train(Day_Px_Chng ~ ., data = training, trControl = TR,
                            method = "gbm", tuneLength = 3, verbose = FALSE)

#svm
mod_log_svm <- caret::train(Day_Px_Chng~ ., data = training, trControl = TR,
                            method = "svmRadial", tuneLength = 3, verbose = FALSE)


#сравнение
Comp <- resamples(list(LM= mod_lm,
                       Lasso = mod_lasso,
                       RF = mod_log_tr,
                       GBM = mod_log_gmb,
                       SVM = mod_log_svm))
bwplot(Comp)
bwplot(Comp, palette = "Blues")


#сравнение
Comparison <- resamples(list(LM= mod_lm,
                             Lasso = mod_lasso,
                             RF = mod_log_tr,
                             GBM = mod_log_gmb,
                             SVM = mod_log_svm))
summary(Comparison)
bwplot(Comparison)
dotplot(Comparison, metric = "Rsquared")

#поиск различий
Comparison2 <- diff(Comparison)
summary(Comparison2)
#c цветами 

myColours <- brewer.pal(6,"Blues")
my.settings <- list(
  superpose.polygon=list(col=myColours[2:5], border="transparent"),
  strip.background=list(col=myColours[6]),
  strip.border=list(col="black")
)
bwplot(Comparison,  col = "black", fill = "#4682B4", par.settings = my.settings, par.strip.text=list(col="white", font=2))
?bwplot
ggplot(Comparison)
dotplot(Comparison, par.strip.text=list(col="white", font=2), par.settings = my.settings)




#какая переменная была самой важной?
ggplot(varImp(mod_log))


varImp(mod_log)
summary(mod_log)
?varImp

#каково качество модели?
confusionMatrix(mod_log)

#получаем статистику модели
getTrainPerf(mod_log)

#можно ли сразу узнать площадь под ROC?
TR_ROC <- trainControl(method = "cv", number = 10, classProbs = TRUE, 
                       summaryFunction = twoClassSummary)
mod_log <- caret::train(A ~ . - affairs, data = data, trControl = TR_ROC,
                        family = "binomial", method = "glm", metric = "ROC")
mod_log

#для регрессии: регуляризация - добавляем шатрф за
#"ridge" - сумму квадартов коэффициентов
#"lasso" - сумму модлуей коэффициентов

install.packages("randomForestExplainer")
install.packages("viridis")
library(viridis)
library(randomForestExplainer)
library(randomForest)
library(ggplot2)
?randomFXorest
#регрессия
#####Лес с переменной БИНАРНОЙ на всей выборке####



#данные все
data_all <- read.xlsx("DATA_ALL23.xlsx")

data_all_sent <- read.xlsx("DATA_ALL23_sent.xlsx")

data_ALL_sent_final <- merge(data_all,data_all_sent, by = "Ticker")
data_ALL_sent_final <- merge(data_ALL_sent_final,part_all, by ="Ticker")

#добавим переменные
data_new <- mutate(data_ALL_sent_final,   NI = ifelse(Net_income>0, 1, 0),  
                   tech = ifelse (Sector == "Tech", 1, 0),
                   underpriced = ifelse(Day_Px_Chng>0,1,0),
                   uncertainty_perc = scale(uncertaint_perc),
                   word_sum = sum_row, positive_percent = scale(positive_perc), 
                   Litigious_percent=scale(Litigious_perc), negative_percent = scale(negative_perc),
                   uncert_part1 = scale(uncert_perc_part1),uncert_part2 = scale(uncert_perc_part2))

#удаляем дупликаты
data_ALL_sent_final <- dplyr::select(data_ALL_sent_final, -sum_row.y.y, -sum_row.x.y)
View(data_ALL_sent_final)


#удаляем факторные и ненужные 
data_new_new_ml <- dplyr::select(data_new,-IPO.year, -Ticker,-Day_Close ,-Change_Close,-Opening_Price,-Sector2, -Sector, 
                                 -IPO_proceed, 
                                 -sum_row,-Change_Opening, -Market_Capitalization, -Day_Px_Chng)


#удаляем ненужные текстовые переменные
data_new_new_ml_ <- data_new_new_ml[,-c(32:39)]
data_new_new_ml_2 <- data_new_new_ml_[,-c(38:45)]
data_new_new_ml_3 <- data_new_new_ml_2 [,-c(45:52)]
data_new_new_ml_4 <- data_new_new_ml_3[,-c(52:59)]
data_new_new_ml_5 <- data_new_new_ml_4[,-c(59:66)]
data_new_new_ml <- data_new_new_ml_5 

#должно быть  74 колонок           
#отбираем только переменные интереса



data_new_new_ml <- dplyr::select(data_new_new_ml, Total_assets,R_D,Net_income,Book_ValperShare,CAPEX,ROE,ROA,EBITDA_margin,Current_ratio,
                                 Under_rank,VC.Dummy, Rollup_dummy,Dual_dummy,Internet_dummy,Float ,Net_debt,
                                 Issued,Shareholder_equity,Age,NI,tech,uncertainty_perc,negative_percent,word_sum,positive_percent,
                                 Litigious_percent,uncert_part1,uncert_part2,underpriced, 
                                 Offer_Price,Net_Debt_Ebitda,IPO_num_scoop)




#удаляем пропуски
data_new_new_ml <- na.omit(data_new_new_ml)
#описательная статистика
describe(data_new_new_ml)
summary(data_new_new_ml)
str(data_new_new_ml)
View(data_new_new_ml)
#разделение на две подвыборки
set.seed(998)
# class зависимая переменная, 75 % в выборку обучения
inTraining <- createDataPartition(data_new_new_ml$underpriced , p = .75, list = FALSE)
#sort(sample(1:length(Sonar$Class), size = length(Sonar$Class)/4*3, replace = FALSE))
training <-data_new_new_ml [ inTraining,]
testing  <- data_new_new_ml [-inTraining,]



#проверяем какая по счету зависимая переменная
View(data_new_new_ml)

data_new_new_ml[,29]
data_new_new_ml$underpriced 



#обучим лес
set.seed(223)
mod <- randomForest(underpriced  ~. , data = data_new_new_ml,localImp = TRUE )
mod
plot(mod, main = "Зависимость ошибки от количества деревьев")
which.min(mod$mse)


#число переменных отбираемых в узле (нужно убрать зависимую переменную)
t <- tuneRF(data_new_new_ml[,-29], data_new_new_ml[,29],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry =286,
            trace = TRUE)



#stepFactor - шаг поиска
#ntreeTry - число деревьев
#trace - показывать динамику поиска

#обучаем финальную модель
mod <- randomForest(underpriced~., data = data_new_new_ml, ntree = 500, mtry = 5)
mod
#что важнее всего?
varImpPlot(mod,
           sort = T,
           n.var = 10,
           main = "10 наиболеe важных переменных")

color_scheme <- colorRamp(c("#B0E0E6", "#00008B"))

# Строим график, используя градиент цвета для заполнения кружочков
varImpPlot(mod, sort = T, n.var = 10, main = "10 наиболее важных переменных",
           color = color_scheme(seq(0, 1, length.out = 10)),
           pch = 19, bg = color_scheme(seq(0, 1, length.out = 10)))

??plot_min_depth_distributioт
plot_min_depth_distribution(mod) + 
  xlab("Переменные") +
  ylab("Количество деревьев")+
  scale_fill_viridis(discrete = TRUE, option = "A") + ggtitle("Распределение минимальной глубины и среднего сплита")

??scale_fill_viridis
plot_min_depth_distribution(mod) + 
  xlab("Переменные") +
  ylab("Количество деревьев")+
  scale_fill_manual(values = c("#003f5c", "#2f4b7c", "#665191","#4B0082" , "#a05195", "#d45087", "#f95d6a",   "#FF8C00", "#ff7c43", "#ffa600", "#FFFF00",
                               "#FF4500","#e45149", "#5F9EA0","#b6d2dd", "#40E0D0" ,"#259086","#006400", "#0a463c","#2F4F4F")) + ggtitle("Распределение минимальной глубины и среднего сплита")
hist(treesize(mod),
     main = "No. of Nodes for the Trees",
     col = "green")
partialPlot(mod, data_new_new_ml, R_D)
partialPlot(mod, data_new_new_ml, uncertainty_perc)


#предельные эффекты
partialPlot(rf, train, Offer_Price, "setosa")



#####Лес с переменной первоначальной ДОХОДНОСТИ на огр выборке####


#данные все
data_all <- read.xlsx("DATA_ALL23.xlsx")

data_all_sent <- read.xlsx("DATA_ALL23_sent.xlsx")

data_ALL_sent_final <- merge(data_all,data_all_sent, by = "Ticker")
data_ALL_sent_final <- merge(data_ALL_sent_final,part_all, by ="Ticker")

#добавим переменные
data_new <- mutate(data_ALL_sent_final,   NI = ifelse(Net_income>0, 1, 0),  
                   tech = ifelse (Sector == "Tech", 1, 0),
                   underpriced = ifelse(Day_Px_Chng>0,1,0),
                   uncertainty_perc = scale(uncertaint_perc),
                   word_sum = sum_row, positive_percent = scale(positive_perc), 
                   Litigious_percent=scale(Litigious_perc), negative_percent = scale(negative_perc),
                   uncert_part1 = scale(uncert_perc_part1),uncert_part2 = scale(uncert_perc_part2))

#удаляем дупликаты
data_ALL_sent_final <- dplyr::select(data_ALL_sent_final, -sum_row.y.y, -sum_row.x.y)
View(data_ALL_sent_final)


#удаляем факторные и ненужные 
data_new_new_ml <- dplyr::select(data_new,-IPO.year, -Ticker,-Day_Close ,-Change_Close,-Opening_Price,-Sector2, -Sector, 
                                 -IPO_proceed, 
                                 -sum_row,-Change_Opening, -Market_Capitalization, -underpriced)


#удаляем ненужные текстовые переменные
data_new_new_ml_ <- data_new_new_ml[,-c(32:39)]
data_new_new_ml_2 <- data_new_new_ml_[,-c(38:45)]
data_new_new_ml_3 <- data_new_new_ml_2 [,-c(45:52)]
data_new_new_ml_4 <- data_new_new_ml_3[,-c(52:59)]
data_new_new_ml_5 <- data_new_new_ml_4[,-c(59:66)]
data_new_new_ml <- data_new_new_ml_5 

#должно быть  74 колонок           

data_new_new_ml <- dplyr::select(data_new_new_ml, Total_assets,R_D,Net_income,Book_ValperShare,CAPEX,ROE,ROA,EBITDA_margin,Current_ratio,
                                 Under_rank,VC.Dummy, Rollup_dummy,Dual_dummy,Internet_dummy,Float ,Net_debt,
                                 Issued,Shareholder_equity,Age,NI,tech,uncertainty_perc,negative_percent,word_sum,positive_percent,
                                 Litigious_percent,uncert_part1,uncert_part2,underpriced, 
                                 Offer_Price,Net_Debt_Ebitda,IPO_num_scoop)





#удаляем пропуски
data_new_new_ml <- na.omit(data_new_new_ml)
#описательная статистика
describe(data_new_new_ml)
summary(data_new_new_ml)
str(data_new_new_ml)
View(data_new_new_ml)
#разделение на две подвыборки
set.seed(998)
# class зависимая переменная, 75 % в выборку обучения
inTraining <- createDataPartition(data_new_new_ml$Day_Px_Chng , p = .75, list = FALSE)
#sort(sample(1:length(Sonar$Class), size = length(Sonar$Class)/4*3, replace = FALSE))
training <-data_new_new_ml [ inTraining,]
testing  <- data_new_new_ml [-inTraining,]


#удаляем пропуски
data_new_new_rf <- na.omit(data_new_new_rf)
#описательная статистика

#проверяем какая по счету зависимая переменная
View(data_new_new_ml)


data_new_new_ml[,29]
data_new_new_ml$Day_Px_Chng



#обучим лес
set.seed(948)
mod <- randomForest(Day_Px_Chng ~. , data = data_new_new_ml,localImp = TRUE )
mod
plot(mod)
plot(mod, main = "Зависимость ошибки от количества деревьев", xlabel = "Importance", ylabel = "Features")

which.min(mod$mse)


#число переменных отбираемых в узле (нужно убрать зависимую переменную)
t <- tuneRF(data_new_new_ml[,-29], data_new_new_ml[,29],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry =261,
            trace = TRUE)


#stepFactor - шаг поиска
#ntreeTry - число деревьев
#trace - показывать динамику поиска

#обучаем финальную модель
mod <- randomForest(Day_Px_Chng~., data = data_new_new_ml, ntree = 261, mtry = 10)
mod
#что важнее всего?
varImpPlot(mod,
           sort = T,
           n.var = 10,
           main = "10 наиболее важных переменных")
plot_min_depth_distribution(mod) + 
  xlab("Переменные") +
  ylab("Количество деревьев")+
  scale_fill_viridis(discrete = TRUE, option = "C") + ggtitle("Распределение минимальной глубины и среднего сплита")
??  scale_fill_viridis
hist(treesize(mod),
     main = "No. of Nodes for the Trees",
     col = "green")
partialPlot(mod, data_new_new_ml, R_D)
partialPlot(mod, data_new_new_ml, uncertainty_perc)


#предельные эффекты
partialPlot(mod , data_new_new_ml, Offer_Price)

?partialPlot



pp <- partialPlot(mod, data_new_new_ml,Offer_Price)

# Преобразование данных в формат, который можно использовать в ggplot2
df <- data.frame(x = pp[[1]], y = pp[[2]])

# Создание графика с красивыми цветами
ggplot(df, aes(x = x, y = y)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 2) +
  theme_minimal() +
  xlab("Offer_Price") +
  ylab("Day_Px_Chng") +
  ggtitle("Предельный эффект Offer_Price на Day_Px_Chng") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        panel.grid = element_blank())


library(readtext)
library(pdftools)
library(tidyr)
library(tm)
library(dplyr)
library(tidytext)
library(ggplot2)
library(methods)
library(quanteda)
library(tidyr)
library(Matrix)
library(janeaustenr)
library(quanteda)
library(lexicon)

library(stringr)

# подключение необходимых библиотек
library(tm)
library(dplyr)
library(tidyr)
library(afinn)





pdf_text("ZM.pdf") %>% strsplit(split = "\n") %>% mutate(linenumber = row_number(opinions))

####анализ форм S1

#ZOOM
ZM<- readtext("ZM.pdf")
ZM<- pdf_text("ZM.pdf")
rows<-scan(textConnection(ZM), 
           what="character", sep = "\n")
ZM<- data.frame(ZM)
ZM <- ZM %>% mutate(linenumber = row_number()) %>%unnest_tokens(output = word, input = text)%>% anti_join(stop_words) 
head(ZM)
#наиболее часто встречающиеся слова
ZM %>% count(word, sort = TRUE)
View(ZM)
str(ZM)
#визуализация частот
#оставим только те, которые больше 600
data_books <- ZM %>%
  count(word, sort = TRUE) %>% filter(n > 200) %>%
  mutate(word = reorder(word, n))
data_books
ggplot(data_books, aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() + theme_bw()


#CRWD
CRWD<- readtext("ADPT2.pdf")
CRWD<- data.frame(CRWD)
CRWD <- CRWD %>% mutate(linenumber = row_number()) %>%unnest_tokens(output = word, input = text)%>% anti_join(stop_words) 
View(CRWD)
sum(CRWD)


head(CRWD)

#наиболее часто встречающиеся слова
CRWD <- CRWD %>% count(word, sort = TRUE)
sum(CRWD$n)

#визуализация частот
#оставим только те, которые больше 600
data_books <- CRWD %>%
  count(word, sort = TRUE) %>% filter( n > 00) %>%
  mutate(word = reorder(word, n))
data_books

ggplot(data_books, aes(word, n)) +
  geom_col(aes(fill = word) +
             scale_fill_gradient(low = "white", 
                                 high = "blue"))+xlab(NULL) +coord_flip() + theme_bw()

#BSM
BSM<- readtext("BSM.pdf")



BSM<- data.frame(BSM)
BSM <- BSM %>% mutate(linenumber = row_number()) %>%unnest_tokens(output = word, input = text)%>% anti_join(stop_words) 
View(BSM)

head(BSM)
??str_split
library(stringr)
unnest_sentences(BSM, output = text,
                 input = text, doc_id = NULL,
                 output_id = "sent_id", drop = TRUE)
#наиболее часто встречающиеся слова
BSM %>% count(word, sort = TRUE)

#визуализация частот
#оставим только те, которые больше 600
data_books <- BSM %>%
  count(word, sort = TRUE) %>% filter( n > 300) %>%
  mutate(word = reorder(word, n))
data_books

ggplot(data_books, aes(word, n)) +
  geom_col(aes(fill = word) +
             scale_fill_gradient(low = "white", 
                                 high = "blue"))+xlab(NULL) +coord_flip() + theme_bw()




#######совместные графики#########
#объединим
frequency1 <- bind_rows(mutate(ZM, company = "Zoom Video Communications Inc"),
                        mutate(CRWD, company = "CrowdStrike Holdings Inc")) %>%
  
  #отберем слова
  mutate(word = str_extract(word, "[a-z']+")) %>%
  #сколько раз конкретное слово встречается у конкретного автора
  count(company, word) %>%
  #перейдем к частотам - доля данного слова у каждого автора
  group_by(company) %>%
  mutate(proportion = n / sum(n)) %>%
  #уберем столбец с частотами
  select(-n)
#уберем столбец с частотами

#изменим формат
frequency2 <- frequency1 %>%
  spread(company, proportion)


frequency <- frequency2 %>% gather(company, proportion, `Zoom Video Communications Inc`)

head(frequency)

#визуализация частот
ggplot(frequency, aes(x = proportion, y = `CrowdStrike Holdings Inc`,
                      #красим сильнее те точки, которые сильнее отличаются
                      color = abs(`CrowdStrike Holdings Inc` - proportion))) +
  #добавим биссектрису
  geom_abline(color = "gray40", lty = 2) +
  #добавим небольшой разброс к точкам
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) + 
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) + 
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) + scale_color_gradient(limits = c(0, 0.001),
                                                                  low = "darkslategray4", high = "gray75") +
  facet_wrap(~company, ncol = 2) +
  theme(legend.position="none") + labs(y = "CrowdStrike Holdings Inc", x = NULL)


#посмотрим на корреляцию частот слов в двух произведениях
cor.test(data = frequency[frequency$company == "Zoom Video Communications Inc",],
         ~ proportion + `CrowdStrike Holdings Inc`)

View(frequency11)


#анализ настроений



ZM<- readtext("ZM.pdf")
ZM<- data.frame(ZM)
?edgar::getSentiment()

CRWD<- readtext("CRWD.pdf")
CRWD<- data.frame(CRWD)


forms<- bind_rows(mutate(ZM, company = "Zoom Video Communications Inc"),
                  mutate(CRWD, company = "CrowdStrike Holdings Inc")) %>%
  
  group_by(company) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("summary", ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

View (forms)
#посомтрим на частоту слов, которые означают радость
head(nrc_emotions)

nrcjoy <- nrc_emotions$term[nrc_emotions$joy == 1] %>% data.frame()
colnames(nrcjoy) <- "word"

#inner_join оставляет только общие элементы

#нарежем текст по 80 строк и посомтрим на число негатвиных и позитивных слов
formssentiment <- forms %>%
  inner_join(get_sentiments("bing")) %>%
  #сгруппируем по 80 и посмотрим сколько там позитивных и негативных слов
  count(company, index = linenumber %/% 20, sentiment) %>%
  #выведем в отдельные столбцы
  spread(sentiment, n) %>%
  #вычислим разницу между числом позитивных и негативных слов
  mutate(sentiment = positive - negative)

#count(book, index = linenumber %/% 80, sentiment) - выдает количества всех
#пар: результат деления нацело и positive/negative
#деление нацело
11 %/% 5
head(formssentiment)

#визуализируем
ggplot(formssentiment, aes(index, sentiment, fill = company)) + 
  geom_col(show.legend = FALSE) +
  facet_wrap(~company, ncol = 2, scales = "free_x")
#facet_wrap работает как facet_grid, но можно задать размер матрицы
#scales = "free_x" - у каждого своя шкала

#как изменяется окраска текста с точки зрения интесивности? 
ZM<- forms %>%
  filter(company == "Zoom Video Communications Inc")
afinn <- pride_prejudice %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(index = linenumber %/% 80) %>%
  summarise(sentiment = sum(value)) %>%
  mutate(method = "AFINN")

#визуализируем
ggplot(afinn, aes(index, sentiment, fill = method)) + geom_col(show.legend = FALSE) + 
  facet_wrap(~method, ncol = 1, scales = "free_y")

#облака слов
forms %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))
par(mfrow = c(1,1))
par(mar=c(5.1 ,4.1 ,4.1 ,2.1)) 

#облака негативных и позитивных слов рядом
forms %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"), max.words = 50)
#acast разделяет на два столбца и указывает негативно или позивтино слово
#и сколько раз оно встречалось


TF-IDF
library(dplyr)
library(janeaustenr)
library(tidytext)
library(ggplot2)
library(gutenbergr)


ZM<- readtext("ZM.pdf")

ZM<- data.frame(ZM)


CRWD<- readtext("CRWD.pdf")
CRWD<- data.frame(CRWD)


forms<- bind_rows(mutate(ZM, company = "Zoom Video Communications Inc"),
                  mutate(CRWD, company = "CrowdStrike Holdings Inc")) 


View (forms)

#посдчет слов
forms_words <- forms %>% unnest_tokens(word, text) %>%
  count(company, word, sort = TRUE)

#подсчет слов во всех книгах
total_words <- forms_words %>%
  group_by(company) %>%
  summarize(total = sum(n))
#добавим правый столбец к левому
forms_words <- left_join(forms_words, total_words)

#визуализация частот
ggplot(forms_words, aes(n/total, fill = company)) + 
  geom_histogram(show.legend = FALSE) + xlim(NA, 0.0009) +
  facet_wrap(~company, ncol = 2, scales = "free_y")

#посмотрим на убывание частот: tf = term frequency
freq_by_rank <-forms_words %>%
  group_by(company) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)
freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color = company)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

#убедимся, что закон Ципфа применим
rank_subset <- freq_by_rank %>%
  filter(rank < 500,
         rank > 10)
mod <- lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)
summary(mod)

#построим регрессионную линию
freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color = company)) +
  geom_abline(intercept = -0.62, slope = -1.1, 
              color = "gray50", linetype = 2) + geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()

#найдем слова, которые важны, но не слишком распространены
forms_words <- forms_words %>%
  bind_tf_idf(word, company, n)
forms_words

#ранжируем по убыванию td-idf
A <- forms_words %>%
  dplyr::select(-total) %>%
  arrange(desc(tf_idf))

A[, 3:6] <- A[, 3:6] %>% round(4)
A %>%  head(10)  %>% gt()
??tf_idf
#визуализируем
forms_words%>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% group_by(company) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = company)) + geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~company, ncol = 2, scales = "free") + coord_flip()

#загрузим физические тексты
my_mirror <- "http://mirrors.xmission.com/gutenberg/"
physics <- gutenberg_download(c(37729, 14725, 13476, 5001),
                              meta_fields = "author", mirror = 
                                my_mirror)

#вычислим частоты
physics_words <- physics %>% unnest_tokens(word, text) %>% 
  count(author, word, sort = TRUE) %>% ungroup()
plot_physics <- physics_words %>%
  bind_tf_idf(word, author, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola")))
#визуализируем
plot_physics %>%
  group_by(author) %>%
  top_n(15, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>% ggplot(aes(word, tf_idf, fill = author)) + 
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") + facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip()

#можно искать фразы с соответствующими словами
physics %>%
  filter(str_detect(text, "ray")) %>%
  dplyr::select(text)

#можно добавить собственные стоп-слова
mystopwords <- data.frame(word = c("eq", "co", "rc", "ac", "ak", "bn",
                                   "fig", "file", "cg", "cb", "cm"))
physics_words <- anti_join(physics_words, mystopwords, by = "word")
plot_physics <- physics_words %>%
  bind_tf_idf(word, author, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(author) %>%
  top_n(15, tf_idf) %>%
  ungroup %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola")))

#получим визуализацию
ggplot(plot_physics, aes(word, tf_idf, fill = author)) + geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") + coord_flip()




#####GPT######

#считаем количество слов в каждом документе попытка 2

library(pdftools)
library(tidytext)
library(dplyr)
library(stringr)

#словарь
dictionary <- read.xlsx("LMDictionary.xlsx")

getwd()
# Укажите путь к вашей папке
path <- "/Users/Anastasia/Desktop/S1_FORMS"

# Создаем пустой фрейм данных, в который будут сохраняться результаты
df <- data.frame(doc_name = character(), word = character(), frequency = numeric())

# Получаем список пдф файлов в папке
pdf_files <- list.files(path = path, pattern = "\\.pdf$", full.names = TRUE)

# Цикл для обработки каждого пдф файла
for (i in seq_along(pdf_files)) {
  
  # Имя текущего документа
  doc_name <- basename(pdf_files[i])
  
  # Используем pdftools для извлечения текста из пдф файла
  text <- pdf_text(pdf_files[i])
  
  # Извлекаем слова из текста
  words <- text %>% 
    str_to_lower() %>% 
    str_squish() %>% 
    str_replace_all("[^[:alpha:]]", " ") %>% 
    str_split("\\s+") %>% 
    unlist()
  
  # Удаляем стоп-слова
  words <- words[!words %in% stop_words$word]
  
  # Создаем фрейм данных для текущего документа
  df_temp <- data.frame(doc_name = rep(doc_name, length(words)), word = words, stringsAsFactors = FALSE)
  
  # Считаем частоту каждого слова
  df_temp %>% 
    group_by(doc_name, word) %>% 
    summarise(frequency = n()) %>% 
    ungroup() -> df_temp
  
  # Объединяем фрейм данных для текущего документа с общим фреймом данных
  df <- bind_rows(df, df_temp)
}
library(openxlsx)
# Сортируем фрейм данных по имени документа и частоте слова
df <- df %>% arrange(doc_name, desc(frequency))
dff <- write.xlsx(df, "частота слов без тональности")
df <- read.xlsx("df.xlsx")
# Выводим результаты
View(df)

#визуализируем TF и IDF

plot_df <- df %>%
  bind_tf_idf(word, doc_name, frequency) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  filter(doc_name %in% head(unique(doc_name), 4)) %>%
  group_by(doc_name) %>%
  top_n(15, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf))

library(RColorBrewer)
display.brewer.all()

# определяем палитру
my_palette <- brewer.pal(11, "Spectral")
#визуализируем
plot_df %>%
  group_by(doc_name) %>%
  top_n(15, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>% ggplot(aes(word, tf_idf, fill = doc_name)) + 
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = my_palette) +  # применяем палитру
  labs(x = NULL, y = "tf-idf") + facet_wrap(~doc_name, ncol = 2, scales = "free") +
  coord_flip()


# Преобразование в широкий формат
df <- data.frame(df) 
df %>% select(all_of(df))
wide_data <- pivot_wider(df, id_cols = doc_name, names_from = word, values_from = frequency)
head(wide_data)
# Вывод преобразованных данных
View(wide_data)
#транспонируем
wide_data <- t(wide_data)
#вместо na - 0
wide_data <- ifelse(is.na(wide_data), 0, wide_data)
head(wide_data)
colnames(wide_data) <- as.character(wide_data[1,])
head(wide_data)
# удаляем первую строку (она больше не нужна)
wide_data<- wide_data[-1,]
#добавляем вручную слово word
write.csv(wide_data,"wide_data")

#cчитаем общее количество слов в документе
#сначала превратим числа в числа

wide_data_all <- read.xlsx("wide_data_ALL.xlsx")
wide_data_all[,-1] <- apply(wide_data_all[,-1], 2, as.numeric)
View(wide_data_all)
head(wide_data_all)
str(wide_data_all)
col_sums <- colSums(wide_data_all[,-1])
sum_row <- c(col_sums)
View(col_sums)
str(col_sums)


# добавляем последнюю строку к таблице
wide_data_sum <- rbind(wide_data, sum_row)
View(wide_data_sum)

#скрепляем со словарем

wide_data_new <- read.xlsx("wide_data_ALL.xlsx")


wide_data_new <- data.frame(wide_data_new)
View(wide_data_new)
wide_sentiment <- inner_join(wide_data_new,Dictionary,by = "word")
View(wide_sentiment)
sum_row_dict <- colSums(wide_sentiment[,-1])
sum_row_dict <- c(sum_row_dict)

#подсчитываем тональность 

result_negative2 <- aggregate(wide_sentiment[,-1], by = list(wide_sentiment$Negative), FUN = sum) 


result_positive2 <- aggregate(wide_sentiment[,-1], by = list(wide_sentiment$Positive ), FUN = sum)

result_uncertainty2 <- aggregate(wide_sentiment[,-1], by = list(wide_sentiment$Uncertainty ), FUN = sum)

result_Litigious2 <- aggregate(wide_sentiment[,-1], by = list(wide_sentiment$Litigious), FUN = sum)

result_Strong_Modal2 <- aggregate(wide_sentiment[,-1], by = list(wide_sentiment$Strong_Modal), FUN = sum)

result_Weak_Modal2 <- aggregate(wide_sentiment[,-1], by = list(wide_sentiment$Weak_Modal), FUN = sum)

result_Constraining2 <- aggregate(wide_sentiment[,-1], by = list(wide_sentiment$Constraining), FUN = sum)


#соединяем все колонкии в одну

result_all <- bind_rows(result_negative2,result_positive2,result_uncertainty2,result_Litigious2,
                        
                        result_Strong_Modal2,result_Weak_Modal2 ,result_Constraining2)
View(result_all)

#удаляем нули
result_all <- result_all[result_all$Group.1 != 0,]
View(result_all)

#транспонируем
result_all <- t(result_all)

#присваиваем стобцам имена тональности
colnames(result_all)[c(1:7)] <- c("negative_sent", "positive_sent", "uncertainty_sent","Litigious_sent",
                                  "Strong_Modal_sent","Weak_Modal_sent", "Constraining_sent" )



#удаляем первую строку с единицами

result_all <- result_all[-1,]

# Удаление последние 4 строки
result_all <- result_all[-(215:222),]

sum_row <-  c(col_sums)


# добавляем последнюю строку к таблице
result_all <- cbind(result_all , sum_row)
result_all <- data.frame(result_all)

#считаем степень каждой тональности в документах
result_all<- mutate(result_all, negative_perc = negative_sent/ sum_row,
                    positive_perc = positive_sent/sum_row,
                    uncertaint_perc =  uncertainty_sent/sum_row,
                    Litigious_perc = Litigious_sent/sum_row,
                    Strong_Modal_perc = Strong_Modal_sent/sum_row,
                    Weak_Modal_perc = Weak_Modal_sent/sum_row, 
                    Constraining_perc =  Constraining_sent/sum_row)


View(result_all)

#сохраним в csv
write.csv(result_all, "ИТОГ тональность")


#соединяем столбцы тональности с финансовыми данными

sent_data_NOTH <- read.xlsx("sent_data_NOTH.xlsx")


View(sent_data_NOTH)


####технологический сектор тональности####



# Укажите путь к вашей папке

path <- "/Users/Anastasia/Desktop/S-1 FORMS TECH"

# Создаем пустой фрейм данных, в который будут сохраняться результаты
df <- data.frame(doc_name = character(), word = character(), frequency = numeric())

# Получаем список пдф файлов в папке
pdf_files <- list.files(path = path, pattern = "\\.pdf$", full.names = TRUE)

# Цикл для обработки каждого пдф файла
for (i in seq_along(pdf_files)) {
  
  # Имя текущего документа
  doc_name <- basename(pdf_files[i])
  
  # Используем pdftools для извлечения текста из пдф файла
  text <- pdf_text(pdf_files[i])
  
  # Извлекаем слова из текста
  words <- text %>% 
    str_to_lower() %>% 
    str_squish() %>% 
    str_replace_all("[^[:alpha:]]", " ") %>% 
    str_split("\\s+") %>% 
    unlist()
  
  # Удаляем стоп-слова
  words <- words[!words %in% stop_words$word]
  
  # Создаем фрейм данных для текущего документа
  df_temp <- data.frame(doc_name = rep(doc_name, length(words)), word = words, stringsAsFactors = FALSE)
  
  # Считаем частоту каждого слова
  df_temp %>% 
    group_by(doc_name, word) %>% 
    summarise(frequency = n()) %>% 
    ungroup() -> df_temp
  
  # Объединяем фрейм данных для текущего документа с общим фреймом данных
  df <- bind_rows(df, df_temp)
}

# Сортируем фрейм данных по имени документа и частоте слова
df_tech <- df %>% arrange(doc_name, desc(frequency))
dff <- write.xlsx(df_tech, "частота слов без тональности tech")
df_tech <- read.xlsx("df_tech.xlsx")
# Выводим результаты
View(df_tech)

# Преобразование в широкий формат
df_tech <- data.frame(df_tech) 

wide_tech_data <- pivot_wider(df_tech, id_cols = doc_name, names_from = word, values_from = frequency)

# Вывод преобразованных данных
View(wide_tech_data)
#транспонируем
wide_tech_data <- t(wide_tech_data)
#вместо na - 0
wide_tech_data <- ifelse(is.na(wide_tech_data), 0, wide_tech_data)

colnames(wide_tech_data) <- as.character(wide_tech_data[1,])

# удаляем первую строку (она больше не нужна)
wide_tech_data<- wide_tech_data[-1,]
#добавили слово word
write.csv(wide_tech_data,"wide_tech_data")

#cчитаем общее количество слов в документе
#сначала превратим числа в числа

wide_data_all_tech <- read.xlsx("wide_data_ALL_tech.xlsx")
wide_data_all_tech[,-1] <- apply(wide_data_all_tech[,-1], 2, as.numeric)
View(wide_data_all_tech)


str(wide_data_all_tech)
col_sums_t <- colSums(wide_data_all_tech[,-1])
sum_row <- t(col_sums_t)
View(sum_row)
str(col_sums_t)


# добавляем последнюю строку к таблице
wide_data_sum_t <- rbind(wide_tech_data , sum_row)
View(wide_data_sum_t)

#добавили слово word
write.csv(wide_data_sum_t,"wide_tech_data_")


#скрепляем со словарем

wide_data_new <- read.xlsx("wide_data_ALL_techh.xlsx")


wide_data_new <- data.frame(wide_data_new)
View(wide_data_new)
wide_sentiment <- inner_join(wide_data_new,dictionary,by = "word")
View(wide_sentiment)
#подсчитываем тональность 

result_negative2 <- aggregate(wide_sentiment[,-1], by = list(wide_sentiment$Negative), FUN = sum) 

View(result_negative2)
result_positive2 <- aggregate(wide_sentiment[,-1], by = list(wide_sentiment$Positive ), FUN = sum)

result_uncertainty2 <- aggregate(wide_sentiment[,-1], by = list(wide_sentiment$Uncertainty ), FUN = sum)

result_Litigious2 <- aggregate(wide_sentiment[,-1], by = list(wide_sentiment$Litigious), FUN = sum)

result_Strong_Modal2 <- aggregate(wide_sentiment[,-1], by = list(wide_sentiment$Strong_Modal), FUN = sum)

result_Weak_Modal2 <- aggregate(wide_sentiment[,-1], by = list(wide_sentiment$Weak_Modal), FUN = sum)

result_Constraining2 <- aggregate(wide_sentiment[,-1], by = list(wide_sentiment$Constraining), FUN = sum)


#соединяем все колонкии в одну

result_all <- bind_rows(result_negative2,result_positive2,result_uncertainty2,result_Litigious2,
                        
                        result_Strong_Modal2,result_Weak_Modal2 ,result_Constraining2)
View(result_all)

#удаляем нули
result_all <- result_all[result_all$Group.1 != 0,]
View(result_all)

#транспонируем
result_all <- t(result_all)

#присваиваем стобцам имена тональности
colnames(result_all)[c(1:7)] <- c("negative_sent", "positive_sent", "uncertainty_sent","Litigious_sent",
                                  "Strong_Modal_sent","Weak_Modal_sent", "Constraining_sent" )



#удаляем первую строку с единицами

result_all <- result_all[-1,]

# Удаление последние 4 строки
result_all <- result_all[-(121:128),]

sum_row <-  c(col_sums_t)

# добавляем последнюю строку к таблице
result_all <- cbind(result_all , sum_row)
result_all <- data.frame(result_all)

#считаем степень каждой тональности в документах
result_all<- mutate(result_all, negative_perc = negative_sent/ sum_row,
                    positive_perc = positive_sent/sum_row,
                    uncertaint_perc =  uncertainty_sent/sum_row,
                    Litigious_perc = Litigious_sent/sum_row,
                    Strong_Modal_perc = Strong_Modal_sent/sum_row,
                    Weak_Modal_perc = Weak_Modal_sent/sum_row, 
                    Constraining_perc =  Constraining_sent/sum_row)


View(result_all)

#сохраним в csv
write.csv(result_all, "ИТОГ тональность tech")

####все сектора####
#до словаря

head(df_tech)
head(df)

df_tech$frequency <- as.numeric(df_tech$frequency)
df$frequency <- as.numeric(df$frequency)

df_all <- bind_rows(df,df_tech)

# Преобразование в широкий формат
df_all<- data.frame(df_all) 

wide_all_data_graph <- pivot_wider(df_all, id_cols = doc_name, names_from = word, values_from = frequency)

# Вывод преобразованных данных
View(wide_all_data_graph)
#транспонируем
wide_all_data_graph <- t(wide_all_data_graph)
#вместо na - 0
wide_all_data_graph[is.null(wide_all_data_graph)] <- 0
wide_all_data_graph <- ifelse(is.na(wide_all_data_graph), 0, wide_all_data_graph)
wide_all_data_graph <- data.frame(wide_all_data_graph)
colnames(wide_all_data_graph) <- as.character(wide_all_data_graph[1,])
dim(wide_all_data_graph)
# удаляем первую строку (она больше не нужна)
wide_all_data_graph<- wide_all_data_graph[-1,]
#добавили слово word
write.csv(wide_all_data_graph,"wide_all_data_graph")

#cчитаем общее количество слов в документе
#сначала превратим числа в числа

wide_data_all_tech <- read.xlsx("wide_data_ALL_tech.xlsx")
wide_data_all_tech[,-1] <- apply(wide_data_all_tech[,-1], 2, as.numeric)
View(wide_data_all_tech)


str(wide_data_all_tech)
col_sums_t <- colSums(wide_data_all_tech[,-1])
sum_row <- t(col_sums_t)
View(sum_row)
str(col_sums_t)


# добавляем последнюю строку к таблице
wide_data_sum_t <- rbind(wide_tech_data , sum_row)
View(wide_data_sum_t)

#добавили слово word
write.csv(wide_data_sum_t,"wide_tech_data_")






####Графики####
library(tm)
library(SnowballC)
library(wordcloud)

library(tm)
library(quanteda)
library(wordcloud)

library(pdftools)
library(tidytext)
library(dplyr)
library(wordcloud)
library(extrafont) # загрузка библиотеки extrafont для использования дополнительных шрифтов
loadfonts() # загрузить все шрифты на компьютере

wordcloud(
  words = wide_data_ALL_tech_neww$word,
  freq = wide_data_ALL_tech_neww$freq ,
  max.words = 85,
  random.order = FALSE,
  rot.per = 0.35,
  colors = brewer.pal(9, "Purples"), # изменение цветовой схемы на палитру "PuBuGn"
  scale = c(5, 0.7) # изменение размера облака слов, # случайный выбор шрифта из загруженных шрифтов
  # использование семейства шрифтов "sans
)


# Создание столбчатой диаграммы
library(ggplot2)
# Создай столбчатой диаграмму и выведи на график топ 50 слов по частоте
library(ggplot2)
library(ggplot2)

# Отсортировать данные по убыванию частоты
wide_data_ALL_tech_neww <- wide_data_ALL_tech_neww[order(-wide_data_ALL_tech_neww$freq), ]

# Создать график
# Отсортировать данные по убыванию частоты
wide_data_ALL_tech_neww <- wide_data_ALL_tech_neww[order(-wide_data_ALL_tech_neww$freq), ]

# Создать график с проранжированными словами
ggplot(wide_data_ALL_tech_neww[1:50, ], aes(x = reorder(word, -freq), y = freq, fill = freq)) + 
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  xlab("Термин") +
  ylab("Частота") +
  ggtitle("Частота использования терминов") +
  coord_flip()
library(ggplot2)
library(dplyr)

# Отсортировать данные по убыванию частоты
wide_data_ALL_tech_neww <- wide_data_ALL_tech_neww[order(-wide_data_ALL_tech_neww$freq), ]

# Создать график
ggplot(wide_data_ALL_tech_neww[1:40, ], aes(x = reorder(word, freq), y = freq, fill = freq)) + 
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  xlab("Термин") +
  ylab("Частота") +
  ggtitle("Частота использования терминов") + 
  coord_flip()



#посдчет слов
book_words <- austen_books() %>% unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE) %>% ungroup()

#подсчет слов во всех книгах
total_words <- book_words %>%
  group_by(book) %>%
  summarize(total = sum(n))
#добавим правый столбец к левому
book_words <- left_join(book_words, total_words)

#визуализация частот
ggplot(book_words, aes(n/total, fill = book)) + 
  geom_histogram(show.legend = FALSE) + xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")

#посмотрим на убывание частот: tf = term frequency
freq_by_rank <- book_words %>%
  group_by(book) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)
freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color = book)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

#убедимся, что закон Ципфа применим
rank_subset <- freq_by_rank %>%
  filter(rank < 500,
         rank > 10)
mod <- lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)
summary(mod)

#построим регрессионную линию
freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color = book)) +
  geom_abline(intercept = -0.62, slope = -1.1, 
              color = "gray50", linetype = 2) + geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()

#найдем слова, которые важны, но не слишком распространены
book_words <- book_words %>%
  bind_tf_idf(word, book, n)
book_words

#ранжируем по убыванию td-idf
A <- book_words %>%
  dplyr::select(-total) %>%
  arrange(desc(tf_idf))
A[, 3:6] <- A[, 3:6] %>% round(4)
A %>%  head(10)  %>% gt()

#визуализируем
book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% group_by(book) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = book)) + geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") + coord_flip()

#####тех сектор тест####
path <- "/Users/Anastasia/Desktop/S-1 FORMS TECH"

pdf_files <- list.files(path = path, pattern = "*.pdf", full.names = TRUE)

# Создаем пустой список для хранения строк
corpus_list <- list()

# Цикл для обработки каждого пдф файла
for (i in seq_along(pdf_files)) {
  
  # Имя текущего документа
  doc_name <- basename(pdf_files[i])
  
  # Используем pdftools для извлечения текста из пдф файла
  text <- pdf_text(pdf_files[i])
  
  # Извлекаем слова из текста
  words <- text %>% 
    str_to_lower() %>% 
    str_squish() %>% 
    str_replace_all("[^[:alpha:]]", " ") %>% 
    str_split("\\s+") %>% 
    unlist()
  
  # Удаляем стоп-слова
  words <- words[!words %in% stop_words$word]
  
  # Создаем фрейм данных для текущего документа
  df_temp <- data.frame(doc_name = rep(doc_name, length(words)), word = words, stringsAsFactors = FALSE)
  
  # Добавляем строки из фрейм данных в список
  corpus_list[[i]] <- df_temp
}

# Используем do.call() и data.frame() для объединения строк в фрейм данных
df <- do.call("rbind", corpus_list)
#считаем общее количество слов по каждому документу
View(df)
# Создаем функцию для разделения списка слов на части
split_words <- function(words, num_parts = 4) {
  num_words <- length(words)
  part_size <- ceiling(num_words / num_parts)
  parts <- list()
  for (i in seq_len(num_parts)) {
    start <- (i - 1) * part_size + 1
    end <- min(i * part_size, num_words)
    parts[[i]] <- words[start:end]
  }
  return(parts)
}

# Применяем функцию к каждому документу в списке
corpus_list2 <- lapply(corpus_list, function(df) {
  words <- df$word
  parts <- split_words(words)
  df$part <- rep(seq_along(parts), sapply(parts, length))
  return(df)
})

# Объединяем строки в один фрейм данных
df2 <- do.call("rbind", corpus_list2)
View(df2)

# #общеее количество слов по частям 
#Группируем данные по частям документа и подсчитываем количество слов  
part_df_count <- df2  %>% 
  group_by(doc_name, part) %>% 
  summarise(count = n())

View(part_df_count)

### Группируем данные по документам и частям, фильтруем только первую часть
first_part_count <- df2 %>%
  filter(part == 1) %>%
  group_by(doc_name) %>%
  summarise(total_words = n())

# Выводим результат
View(first_part_count)
write.xlsx(first_part_count,"first_part_count")
sum_row <- c(first_part_count$total_words)
View(sum_row )

#соединяем со словарем 

dictionary <- read.xlsx("LMDictionary.xlsx")

part_sentiment <- inner_join(df2,dictionary,by = "word")
head (part_sentiment )

library(ggplot2)

# Фильтруем строки с неопределенными словами
part_uncertainty <- part_sentiment %>% 
  filter(Uncertainty == 1)

# Группируем данные по частям документа и подсчитываем количество неопределенных слов
part_uncertainty_count <- part_uncertainty %>% 
  group_by(doc_name, part) %>% 
  summarise(count = n())

# Строим график
ggplot(part_uncertainty_count, aes(x = factor(part), y = count, fill = doc_name)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Часть документа") +
  ylab("Количество неопределенных слов") +
  ggtitle("Распределение неопределенных слов по частям документа") +
  theme_minimal()


# Считаем частоту слов по документам, частям и категориям
freq_table <- part_sentiment %>%
  group_by(doc_name, part, word) %>%
  summarise(count = n()) %>%
  ungroup()

# Выводим первые строки таблицы
head(freq_table)
View(freq_table)

#фильтруем и оставляем только первую часть каждого документа

# Оставляем только первую часть слов каждого документа
freq_table_filtered1 <- freq_table %>%
  distinct(doc_name, word, .keep_all = TRUE) %>%
  filter(part == 1)
View(freq_table_filtered1)

# Преобразование в широкий формат
freq_table_filtered1<- data.frame(freq_table_filtered1) 
wide_freq_table_filtered1 <- pivot_wider(freq_table_filtered1, id_cols = doc_name, names_from = word, values_from = count)
View(wide_freq_table_filtered1)
# Вывод преобразованных данных
View(wide_data)
#транспонируем
wide_freq_table_filtered1 <- t(wide_freq_table_filtered1)
#вместо na - 0
wide_freq_table_filtered1 <- ifelse(is.na(wide_freq_table_filtered1), 0, wide_freq_table_filtered1)
head(wide_freq_table_filtered1)
colnames(wide_freq_table_filtered1 ) <- as.character(wide_freq_table_filtered1 [1,])
head(wide_freq_table_filtered1)
# удаляем первую строку (она больше не нужна)
wide_freq_table_filtered1 <- wide_freq_table_filtered1 [-1,]

#добавляем вручную слово word
write.csv(wide_freq_table_filtered1,"wide_freq_table_filtered1")

#cчитаем общее количество слов в документе
#сначала превратим числа в числа

wide_freq_table_filtered1 <- read.xlsx("wide_freq_table_filtered1.xlsx")
wide_data_all[,-1] <- apply(wide_data_all[,-1], 2, as.numeric)
View(wide_freq_table_filtered1)
head(wide_freq_table_filtered1)
str(wide_data_all)

#скрепляем со словарем

sent_part1 <- inner_join(wide_freq_table_filtered1,dictionary,by = "word")
View(sent_part1)
write.xlsx(sent_part1 ,"sent_part1 ")

#подсчитываем тональность 

result_negative1 <- aggregate(sent_part1[,-1], by = list(sent_part1$Negative), FUN = sum) 

View(result_negative1)
result_positive1 <- aggregate(sent_part1[,-1], by = list(sent_part1$Positive ), FUN = sum)

result_uncertainty1 <- aggregate(sent_part1[,-1], by = list(sent_part1$Uncertainty ), FUN = sum)

result_Litigious1 <- aggregate(sent_part1[,-1], by = list(sent_part1$Litigious), FUN = sum)

result_Strong_Modal1 <- aggregate(sent_part1[,-1], by = list(sent_part1$Strong_Modal), FUN = sum)

result_Weak_Modal1 <- aggregate(sent_part1[,-1], by = list(sent_part1$Weak_Modal), FUN = sum)

result_Constraining1 <- aggregate(sent_part1[,-1], by = list(sent_part1$Constraining), FUN = sum)


#соединяем все колонкии в одну

result_all <- bind_rows(result_negative1,result_positive1,result_uncertainty1,result_Litigious1,
                        
                        result_Strong_Modal1,result_Weak_Modal1 ,result_Constraining1)
View(result_all)

#удаляем нули
result_all <- result_all[result_all$Group.1 != 0,]
View(result_all)

#транспонируем
result_all <- t(result_all)

#присваиваем стобцам имена тональности
colnames(result_all)[c(1:7)] <- c("negative_sent_part1", "positive_sent_part1", "uncertainty_sent_part1","Litigious_sent_part1",
                                  "Strong_Modal_sent_part1","Weak_Modal_sent_part1", "Constraining_sent_part1" )



#удаляем первую строку с единицами

result_all <- result_all[-1,]

# Удаление последние 4 строки
result_all <- result_all[-(121:128),]


# добавляем последнюю строку к таблице
result_all <- cbind(result_all , sum_row)
result_all <- data.frame(result_all)

#считаем степень каждой тональности в документах
result_all<- mutate(result_all, neg_perc_part1 = negative_sent_part1/ sum_row,
                    pos_perc_part1 = positive_sent_part1/sum_row,
                    uncert_perc_part1 =  uncertainty_sent_part1/sum_row,
                    litig_perc_part1 = Litigious_sent_part1/sum_row,
                    strong_Mod_perc_part1 = Strong_Modal_sent_part1/sum_row,
                    weak_Mod_perc_part1 = Weak_Modal_sent_part1/sum_row, 
                    const_perc_part1 =  Constraining_sent_part1/sum_row)


View(result_all)

#сохраним в csv
write.csv(result_all, "sentiment_part1_tech.csv")



###я тупая, можно сделать сразу для всех секторов, а потом разбить на два... класс####

path <- "/Users/Anastasia/Desktop/S-1 FORMS ALL"

pdf_files <- list.files(path = path, pattern = "*.pdf", full.names = TRUE)

# Создаем пустой список для хранения строк
corpus_list <- list()

# Цикл для обработки каждого пдф файла
for (i in seq_along(pdf_files)) {
  
  # Имя текущего документа
  doc_name <- basename(pdf_files[i])
  
  # Используем pdftools для извлечения текста из пдф файла
  text <- pdf_text(pdf_files[i])
  
  # Извлекаем слова из текста
  words <- text %>% 
    str_to_lower() %>% 
    str_squish() %>% 
    str_replace_all("[^[:alpha:]]", " ") %>% 
    str_split("\\s+") %>% 
    unlist()
  
  # Удаляем стоп-слова
  words <- words[!words %in% stop_words$word]
  
  # Создаем фрейм данных для текущего документа
  df_temp <- data.frame(doc_name = rep(doc_name, length(words)), word = words, stringsAsFactors = FALSE)
  
  # Добавляем строки из фрейм данных в список
  corpus_list[[i]] <- df_temp
}

# Используем do.call() и data.frame() для объединения строк в фрейм данных
df <- do.call("rbind", corpus_list)
#считаем общее количество слов по каждому документу
View(df)
# Создаем функцию для разделения списка слов на части
split_words <- function(words, num_parts = 4) {
  num_words <- length(words)
  part_size <- ceiling(num_words / num_parts)
  parts <- list()
  for (i in seq_len(num_parts)) {
    start <- (i - 1) * part_size + 1
    end <- min(i * part_size, num_words)
    parts[[i]] <- words[start:end]
  }
  return(parts)
}

# Применяем функцию к каждому документу в списке
corpus_list2 <- lapply(corpus_list, function(df) {
  words <- df$word
  parts <- split_words(words)
  df$part <- rep(seq_along(parts), sapply(parts, length))
  return(df)
})

# Объединяем строки в один фрейм данных
df2 <- do.call("rbind", corpus_list2)
View(df2)

# #общеее количество слов по частям 
#Группируем данные по частям документа и подсчитываем количество слов  
part_df_count <- df2  %>% 
  group_by(doc_name, part) %>% 
  summarise(count = n())

View(part_df_count)

### Группируем данные по документам и частям, фильтруем только первую часть
first_part_count <- df2 %>%
  filter(part == 1) %>%
  group_by(doc_name) %>%
  summarise(total_words = n())

# Выводим результат
View(first_part_count)
write.xlsx(first_part_count,"first_part_count")
sum_row <- c(first_part_count$total_words)
View(sum_row )

#соединяем со словарем 

dictionary <- read.xlsx("LMDictionary.xlsx")

part_sentiment <- inner_join(df2,dictionary,by = "word")
head (part_sentiment)

library(ggplot2)

# Фильтруем строки с неопределенными словами
part_uncertainty <- part_sentiment %>% 
  filter(Uncertainty == 1)

# Группируем данные по частям документа и подсчитываем количество неопределенных слов
part_uncertainty_count <- part_uncertainty %>% 
  group_by(doc_name, part) %>% 
  summarise(count = n())

# Строим график
ggplot(part_uncertainty_count, aes(x = factor(part), y = count, fill = doc_name)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Часть документа") +
  ylab("Количество неопределенных слов") +
  ggtitle("Распределение неопределенных слов по частям документа") +
  theme_minimal()
#без отображения цветов справа
ggplot(part_uncertainty_count, aes(x = factor(part), y = count, fill = doc_name)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Часть документа") +
  ylab("Количество неопределенных слов") +
  ggtitle("Распределение неопределенных слов по частям документа") +
  theme_minimal() +
  theme(legend.position = "none")


#c другими цветами 
library(ggplot2)
# График с 333 красивыми цветами
# Создание палитры с 333 красивыми цветами
my_colors <- viridis(333)
ggplot(part_uncertainty_count, aes(x = factor(part), y = count, fill = doc_name)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Часть документа") +
  ylab("Количество неопределенных слов") +
  ggtitle("Распределение неопределенных слов по частям документа") +
  theme_minimal() +
  theme(legend.position = "none") +
  # Использование 333 красивых цветов
  scale_fill_manual(values = my_colors)



library(ggplot2)
part_sentiment %>%
  group_by(doc_name, part) %>%
  summarise(across(Negative:Constraining, sum)) %>%
  pivot_longer(cols = Negative:Constraining, names_to = "sentiment", values_to = "count") %>%
  ggplot(aes(x = doc_name, y = count, fill = sentiment)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~sentiment)


part_sentiment %>%
  group_by(doc_name, part) %>%
  summarise(across(Negative:Constraining, sum)) %>%
  pivot_longer(cols = Negative:Constraining, names_to = "sentiment", values_to = "count") %>%
  ggplot(aes(x = "", y = count, fill = sentiment)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = brewer.pal(8, "Accent")) +
  facet_wrap(~sentiment)


library(ggplot2)
library(RColorBrewer)

# Вектор с названиями сентиментов на русском языке
sentiment_labels <- c("Сдерживающий", "Судебный", "Негативный", "Позитивный", 
                      "Сильно модальный", "Неопределенный", "Слабо модальный")

part_sentiment %>%
  group_by(doc_name, part) %>%
  summarise(across(Negative:Constraining, sum)) %>%
  pivot_longer(cols = Negative:Constraining, names_to = "sentiment", values_to = "count") %>%
  ggplot(aes(x = "", y = count, fill = sentiment)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis(discrete = TRUE, option = "magma", labels = sentiment_labels) +
  facet_wrap(~sentiment) +
  xlab(NULL) +
  ylab("Количество слов") +
  labs(fill = "Сентимент")



??scale_fill_viridis_d

library(RColorBrewer)

library(viridis)
library(ggplot2)











# Считаем частоту слов по документам, частям и категориям
freq_table <- part_sentiment %>%
  group_by(doc_name, part, word) %>%
  summarise(count = n()) %>%
  ungroup()

# Выводим первые строки таблицы
head(freq_table)
View(freq_table)

#фильтруем и оставляем только первую часть каждого документа

# Оставляем только первую часть слов каждого документа
freq_table_filtered1 <- freq_table %>%
  distinct(doc_name, word, .keep_all = TRUE) %>%
  filter(part == 1)
View(freq_table_filtered1)

# Преобразование в широкий формат
freq_table_filtered1<- data.frame(freq_table_filtered1) 
wide_freq_table_filtered1 <- pivot_wider(freq_table_filtered1, id_cols = doc_name, names_from = word, values_from = count)
View(wide_freq_table_filtered1)
# Вывод преобразованных данных
View(wide_data)
#транспонируем
wide_freq_table_filtered1 <- t(wide_freq_table_filtered1)
#вместо na - 0
wide_freq_table_filtered1 <- ifelse(is.na(wide_freq_table_filtered1), 0, wide_freq_table_filtered1)
head(wide_freq_table_filtered1)
colnames(wide_freq_table_filtered1 ) <- as.character(wide_freq_table_filtered1 [1,])
head(wide_freq_table_filtered1)
# удаляем первую строку (она больше не нужна)
wide_freq_table_filtered1 <- wide_freq_table_filtered1 [-1,]

#добавляем вручную слово word
write.csv(wide_freq_table_filtered1,"wide_freq_table_filtered1_all")

#cчитаем общее количество слов в документе
#сначала превратим числа в числа

wide_freq_table_filtered1 <- read.xlsx("wide_freq_table_filtered1_all.xlsx")
wide_data_all[,-1] <- apply(wide_data_all[,-1], 2, as.numeric)
View(wide_freq_table_filtered1)
head(wide_freq_table_filtered1)
str(wide_data_all)

#скрепляем со словарем

sent_part1 <- inner_join(wide_freq_table_filtered1,dictionary,by = "word")
View(sent_part1)
write.xlsx(sent_part1 ,"sent_part1_all ")

#подсчитываем тональность 

result_negative1 <- aggregate(sent_part1[,-1], by = list(sent_part1$Negative), FUN = sum) 

View(result_negative1)
result_positive1 <- aggregate(sent_part1[,-1], by = list(sent_part1$Positive ), FUN = sum)

result_uncertainty1 <- aggregate(sent_part1[,-1], by = list(sent_part1$Uncertainty ), FUN = sum)

result_Litigious1 <- aggregate(sent_part1[,-1], by = list(sent_part1$Litigious), FUN = sum)

result_Strong_Modal1 <- aggregate(sent_part1[,-1], by = list(sent_part1$Strong_Modal), FUN = sum)

result_Weak_Modal1 <- aggregate(sent_part1[,-1], by = list(sent_part1$Weak_Modal), FUN = sum)

result_Constraining1 <- aggregate(sent_part1[,-1], by = list(sent_part1$Constraining), FUN = sum)


#соединяем все колонкии в одну

result_all <- bind_rows(result_negative1,result_positive1,result_uncertainty1,result_Litigious1,
                        
                        result_Strong_Modal1,result_Weak_Modal1 ,result_Constraining1)
View(result_all)

#удаляем нули
result_all <- result_all[result_all$Group.1 != 0,]
View(result_all)

#транспонируем
result_all <- t(result_all)

#присваиваем стобцам имена тональности
colnames(result_all)[c(1:7)] <- c("negative_sent_part1", "positive_sent_part1", "uncertainty_sent_part1","Litigious_sent_part1",
                                  "Strong_Modal_sent_part1","Weak_Modal_sent_part1", "Constraining_sent_part1" )



#удаляем первую строку с единицами

result_all <- result_all[-1,]

# Удаление последние 4 строки
result_all <- result_all[-(334:341),]


# добавляем последнюю строку к таблице
result_all <- cbind(result_all , sum_row)
result_all <- data.frame(result_all)

#считаем степень каждой тональности в документах
result_all<- mutate(result_all, neg_perc_part1 = negative_sent_part1/ sum_row,
                    pos_perc_part1 = positive_sent_part1/sum_row,
                    uncert_perc_part1 =  uncertainty_sent_part1/sum_row,
                    litig_perc_part1 = Litigious_sent_part1/sum_row,
                    strong_Mod_perc_part1 = Strong_Modal_sent_part1/sum_row,
                    weak_Mod_perc_part1 = Weak_Modal_sent_part1/sum_row, 
                    const_perc_part1 =  Constraining_sent_part1/sum_row)


View(result_all)

#сохраним в csv
write.csv(result_all, "sentiment_part1_all.csv")
#сделали эксель файл вручную

#####аналогично 2 часть документов####

# Объединяем строки в один фрейм данных
df2 <- do.call("rbind", corpus_list2)
View(df2)

# #общеее количество слов по частям 
#Группируем данные по частям документа и подсчитываем количество слов  
part_df_count <- df2  %>% 
  group_by(doc_name, part) %>% 
  summarise(count = n())

View(part_df_count)

### Группируем данные по документам и частям, фильтруем только ВТОРУЮ часть
first_part_count <- df2 %>%
  filter(part == 2) %>%
  group_by(doc_name) %>%
  summarise(total_words = n())

# Выводим результат
View(first_part_count)
write.xlsx(first_part_count,"first_part_count")
sum_row <- c(first_part_count$total_words)
View(sum_row )

#соединяем со словарем 

dictionary <- read.xlsx("LMDictionary.xlsx")

part_sentiment <- inner_join(df2,dictionary,by = "word")
head (part_sentiment)
View(part_sentiment)
library(ggplot2)

# Фильтруем строки с неопределенными словами
part_uncertainty <- part_sentiment %>% 
  filter(Uncertainty == 1)

# Группируем данные по частям документа и подсчитываем количество неопределенных слов
part_uncertainty_count <- part_uncertainty %>% 
  group_by(doc_name, part) %>% 
  summarise(count = n())
View(part_uncertainty_count)
# Строим график
ggplot(part_uncertainty_count, aes(x = factor(part), y = count, fill = doc_name)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Часть документа") +
  ylab("Количество неопределенных слов") +
  ggtitle("Распределение неопределенных слов по частям документа") +
  theme_minimal()
#без отображения цветов справа
ggplot(part_uncertainty_count, aes(x = factor(part), y = count, fill = doc_name)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Часть документа") +
  ylab("Количество неопределенных слов") +
  ggtitle("Распределение неопределенных слов по частям документа") +
  theme_minimal() +
  theme(legend.position = "none")


# Считаем частоту слов по документам, частям и категориям
#ищем ошибку - все ОК
freq_table <- part_sentiment %>%
  group_by(doc_name, part, word) %>%
  summarise(count = n()) %>%
  ungroup()


# Выводим первые строки таблицы
head(freq_table)
View(freq_table)

#фильтруем и оставляем только ВТОРУЮ часть каждого документа

freq_table_filtered1 <- freq_table %>%
  filter(part == 2) %>%
  dplyr::select(-part)
View(freq_table_filtered1)


# Преобразование в широкий формат
freq_table_filtered1<- data.frame(freq_table_filtered1) 
wide_freq_table_filtered1 <- pivot_wider(freq_table_filtered1, id_cols = doc_name, names_from = word, values_from = count)
View(wide_freq_table_filtered1)

#транспонируем
wide_freq_table_filtered1 <- t(wide_freq_table_filtered1)
#вместо na - 0
wide_freq_table_filtered1 <- ifelse(is.na(wide_freq_table_filtered1), 0, wide_freq_table_filtered1)
head(wide_freq_table_filtered1)
colnames(wide_freq_table_filtered1 ) <- as.character(wide_freq_table_filtered1 [1,])
head(wide_freq_table_filtered1)
# удаляем первую строку (она больше не нужна)
wide_freq_table_filtered1 <- wide_freq_table_filtered1 [-1,]

#добавляем вручную слово word
write.csv(wide_freq_table_filtered1,"wide_freq_table_filtered2_all")

#cчитаем общее количество слов в документе
#сначала превратим числа в числа

wide_freq_table_filtered1 <- read.xlsx("wide_freq_table_filtered2_all.xlsx")

View(wide_freq_table_filtered1)
head(wide_freq_table_filtered1)


#скрепляем со словарем

sent_part1 <- inner_join(wide_freq_table_filtered1,dictionary,by = "word")
View(sent_part1)
write.xlsx(sent_part1 ,"sent_part1_all ")

#подсчитываем тональность 

result_negative1 <- aggregate(sent_part1[,-1], by = list(sent_part1$Negative), FUN = sum) 

View(result_negative1)
result_positive1 <- aggregate(sent_part1[,-1], by = list(sent_part1$Positive ), FUN = sum)

result_uncertainty1 <- aggregate(sent_part1[,-1], by = list(sent_part1$Uncertainty ), FUN = sum)
View(result_uncertainty1)
result_Litigious1 <- aggregate(sent_part1[,-1], by = list(sent_part1$Litigious), FUN = sum)

result_Strong_Modal1 <- aggregate(sent_part1[,-1], by = list(sent_part1$Strong_Modal), FUN = sum)

result_Weak_Modal1 <- aggregate(sent_part1[,-1], by = list(sent_part1$Weak_Modal), FUN = sum)

result_Constraining1 <- aggregate(sent_part1[,-1], by = list(sent_part1$Constraining), FUN = sum)


#соединяем все колонкии в одну

result_all <- bind_rows(result_negative1,result_positive1,result_uncertainty1,result_Litigious1,
                        
                        result_Strong_Modal1,result_Weak_Modal1 ,result_Constraining1)
View(result_all)

#удаляем нули
result_all <- result_all[result_all$Group.1 != 0,]
View(result_all)

#транспонируем
result_all <- t(result_all)

#присваиваем стобцам имена тональности
colnames(result_all)[c(1:7)] <- c("negative_sent_part2", "positive_sent_part2", "uncertainty_sent_part2","Litigious_sent_part2",
                                  "Strong_Modal_sent_part2","Weak_Modal_sent_part2", "Constraining_sent_part2" )



#удаляем первую строку с единицами

result_all <- result_all[-1,]

# Удаление последние 4 строки
result_all <- result_all[-(334:341),]


# добавляем последнюю строку к таблице
result_all <- cbind(result_all , sum_row)
result_all <- data.frame(result_all)

#считаем степень каждой тональности в документах
result_all<- mutate(result_all, neg_perc_part2 = negative_sent_part2/ sum_row,
                    pos_perc_part2 = positive_sent_part2/sum_row,
                    uncert_perc_part2 =  uncertainty_sent_part2/sum_row,
                    litig_perc_part2 = Litigious_sent_part2/sum_row,
                    strong_Mod_perc_part2 = Strong_Modal_sent_part2/sum_row,
                    weak_Mod_perc_part2 = Weak_Modal_sent_part2/sum_row, 
                    const_perc_part2 =  Constraining_sent_part2/sum_row)


View(result_all)

#сохраним в csv
write.csv(result_all, "sentiment_part2_all.csv")
#сделали эксель файл вручную

#####аналогично 3 часть документов####
# #общеее количество слов по частям 
#Группируем данные по частям документа и подсчитываем количество слов  
part_df_count <- df2  %>% 
  group_by(doc_name, part) %>% 
  summarise(count = n())

View(part_df_count)

### Группируем данные по документам и частям, фильтруем только ВТОРУЮ часть
first_part_count <- df2 %>%
  filter(part == 3) %>%
  group_by(doc_name) %>%
  summarise(total_words = n())

# Выводим результат
View(first_part_count)
write.xlsx(first_part_count,"first_part_count")
sum_row <- c(first_part_count$total_words)
View(sum_row )


Считаем частоту слов по документам, частям и категориям
#ищем ошибку - все ОК
freq_table <- part_sentiment %>%
  group_by(doc_name, part, word) %>%
  summarise(count = n()) %>%
  ungroup()


# Выводим первые строки таблицы
head(freq_table)
View(freq_table)

#фильтруем и оставляем только ТРЕТЬЮ часть каждого документа

freq_table_filtered1 <- freq_table %>%
  filter(part == 3) %>%
  dplyr::select(-part)

View(freq_table_filtered1)


# Преобразование в широкий формат
freq_table_filtered1<- data.frame(freq_table_filtered1) 
wide_freq_table_filtered1 <- pivot_wider(freq_table_filtered1, id_cols = doc_name, names_from = word, values_from = count)
View(wide_freq_table_filtered1)

#транспонируем
wide_freq_table_filtered1 <- t(wide_freq_table_filtered1)
#вместо na - 0
wide_freq_table_filtered1 <- ifelse(is.na(wide_freq_table_filtered1), 0, wide_freq_table_filtered1)
head(wide_freq_table_filtered1)
colnames(wide_freq_table_filtered1 ) <- as.character(wide_freq_table_filtered1 [1,])
head(wide_freq_table_filtered1)
# удаляем первую строку (она больше не нужна)
wide_freq_table_filtered1 <- wide_freq_table_filtered1 [-1,]

#добавляем вручную слово word
write.csv(wide_freq_table_filtered1,"wide_freq_table_filtered3_all")

#cчитаем общее количество слов в документе
#сначала превратим числа в числа

wide_freq_table_filtered1 <- read.xlsx("wide_freq_table_filtered3_all.xlsx")

View(wide_freq_table_filtered1)
head(wide_freq_table_filtered1)


#скрепляем со словарем

sent_part1 <- inner_join(wide_freq_table_filtered1,dictionary,by = "word")
View(sent_part1)
write.xlsx(sent_part1 ,"sent_part1_all ")

#подсчитываем тональность 

result_negative1 <- aggregate(sent_part1[,-1], by = list(sent_part1$Negative), FUN = sum) 

View(result_negative1)
result_positive1 <- aggregate(sent_part1[,-1], by = list(sent_part1$Positive ), FUN = sum)

result_uncertainty1 <- aggregate(sent_part1[,-1], by = list(sent_part1$Uncertainty ), FUN = sum)
View(result_uncertainty1)
result_Litigious1 <- aggregate(sent_part1[,-1], by = list(sent_part1$Litigious), FUN = sum)

result_Strong_Modal1 <- aggregate(sent_part1[,-1], by = list(sent_part1$Strong_Modal), FUN = sum)

result_Weak_Modal1 <- aggregate(sent_part1[,-1], by = list(sent_part1$Weak_Modal), FUN = sum)

result_Constraining1 <- aggregate(sent_part1[,-1], by = list(sent_part1$Constraining), FUN = sum)


#соединяем все колонкии в одну

result_all <- bind_rows(result_negative1,result_positive1,result_uncertainty1,result_Litigious1,
                        
                        result_Strong_Modal1,result_Weak_Modal1 ,result_Constraining1)
View(result_all)

#удаляем нули
result_all <- result_all[result_all$Group.1 != 0,]
View(result_all)

#транспонируем
result_all <- t(result_all)

#присваиваем стобцам имена тональности
colnames(result_all)[c(1:7)] <- c("negative_sent_part3", "positive_sent_part3", "uncertainty_sent_part3","Litigious_sent_part3",
                                  "Strong_Modal_sent_part3","Weak_Modal_sent_part3", "Constraining_sent_part3" )



#удаляем первую строку с единицами

result_all <- result_all[-1,]

# Удаление последние 4 строки
result_all <- result_all[-(334:341),]


# добавляем последнюю строку к таблице
result_all <- cbind(result_all , sum_row)
result_all <- data.frame(result_all)

#считаем степень каждой тональности в документах
result_all<- mutate(result_all, neg_perc_part3 = negative_sent_part3/ sum_row,
                    pos_perc_part3 = positive_sent_part3/sum_row,
                    uncert_perc_part3 =  uncertainty_sent_part3/sum_row,
                    litig_perc_part3 = Litigious_sent_part3/sum_row,
                    strong_Mod_perc_part3 = Strong_Modal_sent_part3/sum_row,
                    weak_Mod_perc_part3 = Weak_Modal_sent_part3/sum_row, 
                    const_perc_part3 =  Constraining_sent_part3/sum_row)


View(result_all)

#сохраним в csv
write.csv(result_all, "sentiment_part3_all.csv")
#сделали эксель файл вручную

#####аналогично 4 часть документов####
# #общеее количество слов по частям 
#Группируем данные по частям документа и подсчитываем количество слов  
part_df_count <- df2  %>% 
  group_by(doc_name, part) %>% 
  summarise(count = n())

View(part_df_count)

### Группируем данные по документам и частям, фильтруем только ВТОРУЮ часть
first_part_count <- df2 %>%
  filter(part == 4) %>%
  group_by(doc_name) %>%
  summarise(total_words = n())

# Выводим результат
View(first_part_count)
write.xlsx(first_part_count,"first_part_count")
sum_row <- c(first_part_count$total_words)
View(sum_row )


#Считаем частоту слов по документам, частям и категориям
#ищем ошибку - все ОК
freq_table <- part_sentiment %>%
  group_by(doc_name, part, word) %>%
  summarise(count = n()) %>%
  ungroup()


# Выводим первые строки таблицы
head(freq_table)
View(freq_table)

#фильтруем и оставляем только ТРЕТЬЮ часть каждого документа

freq_table_filtered1 <- freq_table %>%
  filter(part == 4) %>%
  dplyr::select(-part)

View(freq_table_filtered1)


# Преобразование в широкий формат
freq_table_filtered1<- data.frame(freq_table_filtered1) 
wide_freq_table_filtered1 <- pivot_wider(freq_table_filtered1, id_cols = doc_name, names_from = word, values_from = count)
View(wide_freq_table_filtered1)

#транспонируем
wide_freq_table_filtered1 <- t(wide_freq_table_filtered1)
#вместо na - 0
wide_freq_table_filtered1 <- ifelse(is.na(wide_freq_table_filtered1), 0, wide_freq_table_filtered1)
head(wide_freq_table_filtered1)
colnames(wide_freq_table_filtered1 ) <- as.character(wide_freq_table_filtered1 [1,])
head(wide_freq_table_filtered1)
# удаляем первую строку (она больше не нужна)
wide_freq_table_filtered1 <- wide_freq_table_filtered1 [-1,]

#добавляем вручную слово word
write.csv(wide_freq_table_filtered1,"wide_freq_table_filtered4_all")

#cчитаем общее количество слов в документе
#сначала превратим числа в числа

wide_freq_table_filtered1 <- read.xlsx("wide_freq_table_filtered4_all.xlsx")

View(wide_freq_table_filtered1)
head(wide_freq_table_filtered1)


#скрепляем со словарем

sent_part1 <- inner_join(wide_freq_table_filtered1,dictionary,by = "word")
View(sent_part1)
write.xlsx(sent_part1 ,"sent_part1_all ")

#подсчитываем тональность 

result_negative1 <- aggregate(sent_part1[,-1], by = list(sent_part1$Negative), FUN = sum) 

View(result_negative1)
result_positive1 <- aggregate(sent_part1[,-1], by = list(sent_part1$Positive ), FUN = sum)

result_uncertainty1 <- aggregate(sent_part1[,-1], by = list(sent_part1$Uncertainty ), FUN = sum)
View(result_uncertainty1)
result_Litigious1 <- aggregate(sent_part1[,-1], by = list(sent_part1$Litigious), FUN = sum)

result_Strong_Modal1 <- aggregate(sent_part1[,-1], by = list(sent_part1$Strong_Modal), FUN = sum)

result_Weak_Modal1 <- aggregate(sent_part1[,-1], by = list(sent_part1$Weak_Modal), FUN = sum)

result_Constraining1 <- aggregate(sent_part1[,-1], by = list(sent_part1$Constraining), FUN = sum)


#соединяем все колонкии в одну

result_all <- bind_rows(result_negative1,result_positive1,result_uncertainty1,result_Litigious1,
                        
                        result_Strong_Modal1,result_Weak_Modal1 ,result_Constraining1)
View(result_all)

#удаляем нули
result_all <- result_all[result_all$Group.1 != 0,]
View(result_all)

#транспонируем
result_all <- t(result_all)

#присваиваем стобцам имена тональности
colnames(result_all)[c(1:7)] <- c("negative_sent_part4", "positive_sent_part4", "uncertainty_sent_part4","Litigious_sent_part4",
                                  "Strong_Modal_sent_part4","Weak_Modal_sent_part4", "Constraining_sent_part4" )



#удаляем первую строку с единицами

result_all <- result_all[-1,]

# Удаление последние 4 строки
result_all <- result_all[-(334:341),]


# добавляем последнюю строку к таблице
result_all <- cbind(result_all , sum_row)
result_all <- data.frame(result_all)

#считаем степень каждой тональности в документах
result_all<- mutate(result_all, neg_perc_part4 = negative_sent_part4/ sum_row,
                    pos_perc_part4 = positive_sent_part4/sum_row,
                    uncert_perc_part4 =  uncertainty_sent_part4/sum_row,
                    litig_perc_part4 = Litigious_sent_part4/sum_row,
                    strong_Mod_perc_part4 = Strong_Modal_sent_part4/sum_row,
                    weak_Mod_perc_part4 = Weak_Modal_sent_part4/sum_row, 
                    const_perc_part4 =  Constraining_sent_part4/sum_row)


View(result_all)

#сохраним в csv
write.csv(result_all, "sentiment_part4_all.csv")
#сделали эксель файл вручную




###соединяем все дата фреймы в один####

part1 <- read.xlsx("sentiment_part1_all.xlsx")
part2 <- read.xlsx("sentiment_part2_all.xlsx")
part3 <- read.xlsx("sentiment_part3_all.xlsx")
part4 <- read.xlsx("sentiment_part4_all.xlsx")



part12 <- merge(part1,part2, by ="Ticker")
View(part12)

part123 <- merge(part12,part3, by ="Ticker")

part_all <- merge(part123,part4, by ="Ticker")
View(part_all)

library(ggplot2)

# Создаем таблицу частотности слов
freq_table <- part_sentiment %>%
  group_by(doc_name, part, sentiment) %>%
  summarise(count = n()) %>%
  ungroup()
View(part_sentiment)
head(part_sentiment)
# Создаем график
ggplot(freq_table, aes(x = doc_name, y = count, fill = sentiment)) +
  geom_col(position = "dodge") +
  facet_wrap(~part) +
  labs(title = "Частотность слов в документах",
       x = "Документы",
       y = "Частотность",
       fill = "Sentiment") +
  theme_bw()


part_sentiment %>%
  group_by(doc_name, part) %>%
  summarise(across(Negative:Constraining, sum)) %>%
  pivot_longer(cols = Negative:Constraining, names_to = "sentiment", values_to = "count") %>%
  ggplot(aes(x = doc_name, y = count, fill = sentiment)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~sentiment)

library(tidyr)
library(ggplot2)

part_sentiment %>%
  pivot_longer(cols = c(Negative, Positive, Uncertainty, Litigious, Weak_Modal),
               names_to = "sentiment", values_to = "count") %>%
  ggplot(aes(x = sentiment, y = count, fill = as.factor(part))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~doc_name) +
  labs(x = "Sentiment", y = "Count", fill = "Part") +
  theme_minimal()

??"pdf_text"
# Set the path to the folder containing the PDF files
path <-   "/Users/Anastasia/Desktop/S-1 FORMS TECH"

pdf_files <- list.files(path = path, pattern = "*.pdf", full.names = TRUE)

# Создаем пустой список для хранения строк
corpus_list <- list()

# Цикл для обработки каждого пдф файла
for (i in seq_along(pdf_files)) {
  
  # Имя текущего документа
  doc_name <- basename(pdf_files[i])
  
  # Используем pdftools для извлечения текста из пдф файла
  text <- pdf_text(pdf_files[i])
  
  # Извлекаем слова из текста
  words <- text %>% 
    str_to_lower() %>% 
    str_squish() %>% 
    str_replace_all("[^[:alpha:]]", " ") %>% 
    str_split("\\s+") %>% 
    unlist()
  
  # Удаляем стоп-слова
  words <- words[!words %in% stop_words$word]
  
  # Создаем фрейм данных для текущего документа
  df_temp <- data.frame(doc_name = rep(doc_name, length(words)), word = words, stringsAsFactors = FALSE)
  
  # Добавляем строки из фрейм данных в список
  corpus_list[[i]] <- df_temp
}

# Используем do.call() и data.frame() для объединения строк в фрейм данных
df <- do.call("rbind", corpus_list)
View(df)
write.csv(df,"no_lines.csv")



#####первый столбец - имя документа, второй очищенный текст

# Создаем пустой список для хранения строк
corpus_list <- list()

# Цикл для обработки каждого пдф файла
for (i in seq_along(pdf_files)) {
  
  # Имя текущего документа
  doc_name <- basename(pdf_files[i])
  
  # Используем pdftools для извлечения текста из пдф файла
  text <- pdf_text(pdf_files[i])
  
  # Извлекаем слова из текста
  words <- text %>% 
    str_to_lower() %>% 
    str_squish() %>% 
    str_replace_all("[^[:alpha:]]", " ") %>% 
    str_split("\\s+") %>% 
    unlist()
  
  # Удаляем стоп-слова
  words <- words[!words %in% stop_words$word]
  
  # Объединяем слова в строку
  doc_text <- paste(words, collapse = " ")
  
  # Создаем фрейм данных для текущего документа
  df_temp <- data.frame(doc_name = doc_name, doc_text = doc_text, stringsAsFactors = FALSE)
  
  # Добавляем строки из фрейм данных в список
  corpus_list[[i]] <- df_temp
}

# Используем do.call() и data.frame() для объединения строк в фрейм данных
df <- do.call("rbind", corpus_list)


dim(df)
#для того, чтобы не лагали модели отбираем первые 55 документов
df <- head(df, 60)
df
# Переименовываем столбцы
colnames(df) <- c("doc_id", "text")

# Создаем корпус из датафрейма df
corpus_s <- Corpus(DataframeSource(df),)

corpus_s <- VCorpus(VectorSource(df[,2]))
# Устанавливаем некоторые параметры корпуса
corpus_s <- corpus %>%
  tm_map(removeNumbers) %>% # Удаление чисел
  tm_map(removePunctuation) %>% # Удаление знаков пунктуации
  tm_map(stripWhitespace) %>% # Удаление лишних пробелов
  tm_map(content_transformer(tolower)) # Приведение к нижнему регистру



#DTM матрица (Document Term Matrix)
dtm_all <- DocumentTermMatrix(corpus_s)


#DTM матрица с весами по tf-idf
dtm_tf <- tm::DocumentTermMatrix(corpus_s, control = list(weighting =
                                                            weightTfIdf))
dtm_tf <- dtm_tf[, sort(colSums(dtm_tf), decreasing = TRUE)]
View(dtm_tf)
findFreqTerms(dtm_tf, lowfreq = 0, highfreq = 1)

#поиск коррелированных слов - ставим минимальную корреляцию
findAssocs(dtm_tf, "stock", corlimit = 0.5)
#можно сразу искать для веткора слов

#интересная визуализация коррелированных слов
#с частотой выше заднной
correlation.limit <- 0.8
freqency.terms.dtm <- findFreqTerms(dtm_all, lowfreq = 55000)
plot(dtm_all, term = freqency.terms.dtm,
     corThreshold=correlation.limit)


#визаулизация через дендрограмму: первые 50 слов по tf-idf
dendogram <- dist(t(dtm_tf[,1:50]), method = "euclidian")
dendogram.fit <- hclust(d = dendogram, method = "ward.D")
plot(dendogram.fit, hang = -1)

#пример словаря для анализа сантиментов
dict <- lexicon_loughran()

#выделю самые частые слова и посмотрю как они меняются по документам
N <- findFreqTerms(dtm_all, lowfreq = 1000)
N1 <- colnames(dtm_all) %in% N
S <- dtm_all[1:10, N1] %>% as.matrix() 
t(S) %>% heatmap(Rowv = NA,
                 Colv = NA)
#что-то с цветами, ведь тут только 1 и 0

#другая визуализация
heatmaply(S, dendrogram = F)
?heatmaply
#### Поиск крайних новостей ####
#зададим эталонные новости  (273 - негатив, 145 - позитив)

#указываем ту, которая должна быть правой и левой
#левая "меньше"
#например, можно правой выбрать самую негативную
#а левую - позитивную
#он расставит остальные в таком порядке
writeLines(as.character(corpus_s[[58]]))
writeLines(as.character(corpus_s[[28]]))

#построим модель
G <- corpus(corpus_s)
D <- dfm(tokens(G))
mod <- textmodel_wordfish(D, dir = c(58,28))
summary(mod)

#визуализируем
textplot_scale1d(mod)

#какие похожи?
S <- predict(mod) %>% sort(decreasing = TRUE)
names(S)[S > 1]
writeLines(as.character(corpus[[11]]))

#здесь визуализированы словы, которые используются чаще
#в крайних сообщениях
textplot_scale1d(mod, margin="features")


#### Wordfish - поиск крайностей####
#источник https://tutorials.quanteda.io/machine-learning/wordfish/
#Идея: надо расставить новости по некоторой шкале
#выбираем один положительный текст, один отрицательный (сами)
#остальные он расставляет сам и сообщает им интенсивность
toks_irish <- tokens(data_corpus_irishbudget2010, remove_punct = TRUE)
dfmat_irish <- dfm(toks_irish)

#определим негавтиную и позитивную новость (на листе скрипт диплом итог)
#для остальных он определит сам
#негативная (273 - негатив, 145 - позитив)
writeLines(as.character(toks_irish[[273]]))
#позитивная
writeLines(as.character(toks_irish[[145]]))

#модель
tmod_wf <- textmodel_wordfish(D, dir = c(12, 2))
summary(tmod_wf)

#визуализируем
textplot_scale1d(tmod_wf)

#с группировкой
textplot_scale1d(tmod_wf, groups = D )

#слова с подсветкой
textplot_scale1d(tmod_wf, margin = "features", 
                 highlighted = c("government", "global", "company", 
                                 "financial", "risk"))

#### Correspondence analysis - визуализация в R2 ####
# источник https://tutorials.quanteda.io/machine-learning/ca/
#визуализируем новости в двумерном пространстве
#чем они ближе, тем более похожи
tmod_ca <- textmodel_ca(D)

#одномерная визуализация
textplot_scale1d(tmod_ca)

#определяем координаты
dat_ca <- data.frame(dim1 = coef(tmod_ca, doc_dim = 1)$coef_document, 
                     dim2 = coef(tmod_ca, doc_dim = 2)$coef_document)

#визуализируем
plot(1, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", 
     xlab = "Dimension 1", ylab = "Dimension 2")
grid()
text(dat_ca$dim1, dat_ca$dim2, 
     labels = rownames(dat_ca), 
     cex = 0.8, col = rgb(0, 0, 0, 0.7))


#### Latent semantic scaling - поиск сантиментов по теме ####
# источник: https://tutorials.quanteda.io/machine-learning/lss/
# devtools::install_github("quanteda/quanteda.corpora")
# идея - даем список позитивных и негативных слов,
# алгротим сам расставляет веса для ВСЕХ (!) слов по близости
# к данным позитивным и негативным
library(quanteda)
library(quanteda.corpora)
remotes::install_github("quanteda/quanteda.corpora")
library(LSX)
corp_news <- download("data_corpus_guardian")
corp_news <- corp_news[1:1000]

#превратим в предложения и токенезируем
corp_sent <- corpus_reshape(corp_news, to =  "sentences")
toks_sent <- corp_sent %>% 
  tokens(remove_punct = TRUE, remove_symbols = TRUE, 
         remove_numbers = TRUE, remove_url = TRUE) %>% 
  tokens_remove(stopwords("en", source = "marimo")) %>%
  tokens_remove(c("*-time", "*-timeUpdated", "GMT", "BST", "*.com"))  

#выделим самые частые слова
dfmat_sent <- toks_sent %>% 
  dfm() %>% 
  dfm_remove(pattern = "") %>% 
  dfm_trim(min_termfreq = 100)
topfeatures(dfmat_sent, 20)

G <- corpus(corpus_s)
dfmat_sent <- dfm(tokens(G))
#зададим примеры позитивных и негативных слов
seed <- as.seedwords(data_dictionary_sentiment)
print(seed)

toks_sent <- tokens(G)
#отбор слов по некоторой теме
eco <- char_context(toks_sent, pattern = "uncertainty*", p = 0.05)

#обучим модель
#он сам отберет позитивные и негативные слова по теме
#и покажет их интенсивность
tmod_lss <- textmodel_lss(dfmat_sent, seeds = seed,
                          terms = eco, k = 20, cache = TRUE)
head(coef(tmod_lss), 20)
#самые позитивные

#самые негативные
tail(coef(tmod_lss), 20)

#посветим негативные слова из словаря
textplot_terms(tmod_lss, data_dictionary_LSD2015["negative"])

#сделаем прогнозы сантиментов текста по модели


# Выбираем столбцы Ticker и IPO.year из TECH23_sent
selected_cols <- select(TECH23_sent, Ticker, IPO.year)

# Создаем отдельный датафрейм c годами
new_df <- data.frame(selected_cols)
new_df <- head(new_df,60)

dfmat_doc <- dfm_group(dfmat_sent)
dfmat_doc <- as.matrix.data.frame(dfmat_doc)
View(dfmat_doc)
#скрепляем с датами
dat <- cbind(new_df,dfmat_doc)
#удаляем лишний столбец
dat <- dplyr::select(dat,- Ticker)
View(dat)
dat <- as.dfm(dat)
dfmat_doc$fit <- predict(tmod_lss, newdata = dfmat_doc)


#сглаживание результатов
# Convert the IPO.year column to a date variable
dat$IPO.date <- as.Date(paste0(dat$IPO.year, "-01-01"))

# Smooth the lss_var (dfmat_doc$fit) with the IPO.date variable as the date_var
dfmat_doc_smooth <- smooth_lss(dat, engine = "locfit", lss_var = "fit", date_var = "IPO.date")

#визуализируем
plot(dat$IPO.year, dfmat_doc$fit, col = rgb(0, 0, 0, 0.05), pch = 16, ylim = c(-0.5, 0.5),
     xlab = "Time", ylab = "Economic sentiment")
lines(dat_smooth$date, dfmat_doc_smooth$fit, type = "l")
lines(dat_smooth$date, dfmat_doc_smooth$fit + dfmat_doc_smooth$se.fit * 1.96, type = "l", lty = 3)
lines(dat_smooth$date, dfmat_doc_smooth$fit - dfmat_doc_smooth$se.fit * 1.96, type = "l", lty = 3)
abline(h = 0, lty = c(1, 2))

#### Topic LDA - словари по Дерихле ####
# install.packages("seededlda")
library(seededlda)
library(lubridate)

#загрузим данные
corp_news <- download("data_corpus_guardian")
corp_news_2016 <- corpus_subset(corp_news, year(date) == 2016)
ndoc(corp_news_2016)

#превратим в DFM
toks_news <- tokens(corp_news_2016, remove_punct = TRUE, remove_numbers = TRUE, remove_symbol = TRUE)
toks_news <- tokens_remove(toks_news, pattern = c(stopwords("en"), "*-time", "updated-*", "gmt", "bst"))
dfmat_news <- dfm(toks_news) %>% 
  dfm_trim(min_termfreq = 0.8, termfreq_type = "quantile",
           max_docfreq = 0.1, docfreq_type = "prop")

#LDA (работает долго)
tmod_lda <- textmodel_lda(dfmat_news, k = 10)
#что входит в каждый топик?
terms(tmod_lda, 10)

#из какого топика скорее какой документ
head(topics(tmod_lda), 20)

#сколько раз какой топик встречается как самый вероятный
dfmat_news$topic <- topics(tmod_lda)
table(dfmat_news$topic)

#по топикам
head(tmod_lda$theta)

#### Дерихле по подготовленным словарям ####
G <- corpus(corpus_s)
dfmat_sent <- dfm(tokens(G))

#модель без первоначальных словарей
lda <- textmodel_lda(dfmat_sent, 6)
terms(lda)
topics(lda)

#модель с первоначальными словарями
dict <- dictionary(list(financials = c("financial", "revenues", "income","million"),
                        stocks = c("common", "shares", "stock","class","market"),
                        development = c("products*", "development*", "agreement*","services"),
                        risks = c("debt", "risk*", "tax","losses", "uncertain"),
                        special = c("clinical*", "technology*", "industry")))
slda <- textmodel_seededlda(dfmat_sent, dict, residual = TRUE, min_termfreq = 10)
terms(slda)
topics(slda)

#вероятности
head(slda$theta)

#
#### Поиск схожих текстов или слов ####
#источник https://tutorials.quanteda.io/statistical-analysis/dist/
library(quanteda.textstats)
toks_inaug <- tokens(data_corpus_inaugural)
dfmat_inaug <- dfm(toks_inaug, remove = stopwords("en"))

#кластеризация
tstat_dist <- as.dist(textstat_dist(dfmat_inaug))
clust <- hclust(tstat_dist)
plot(clust, xlab = "Distance", ylab = NULL)

#разметим кластеры
N <- cutree(clust, k = 3)
N

#### RELATIVE FREQUENCY ANALYSIS ####
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(quanteda.corpora)
library(lubridate)
#можно сравнивать по каким словам два множества текстов
#сильнее всего отличаются (например, до и после определенной даты)
#источник https://tutorials.quanteda.io/statistical-analysis/keyness/
#загрузка данных
corp_news <- download("data_corpus_guardian")
corp_news <- corp_news[1:500]

#сравнение данных до и после определенной даты
toks_news <- tokens(corp_news, remove_punct = TRUE) 
dfmat_news <- dfm(toks_news)
tstat_key <- textstat_keyness(dfmat_news, 
                              target = year(dfmat_news$date) >= 2016)
textplot_keyness(tstat_key)

#### Collocation - совместное употребление слов ####
#источник https://tutorials.quanteda.io/statistical-analysis/collocation/
#определение слов, которые часто идут рядом
#друг с другом
corp_news <- download("data_corpus_guardian")
corp_news <- corp_news[1:500]

#токенизация
toks_news <- tokens(corp_news, remove_punct = TRUE)

#какие слова возникают часто вместе?
tstat_col_caps <- tokens_select(toks_news, pattern = "^[A-Z]", 
                                valuetype = "regex", 
                                case_insensitive = FALSE, 
                                padding = TRUE) %>% 
  textstat_collocations(min_count = 10)
tstat_col_caps


#### Анализ сантиментов ####
# remotes::install_github("quanteda/quanteda.sentiment")
library("quanteda.sentiment")

#словарь позитивных и негативных слов
print(data_dictionary_geninqposneg, max_nval = 8)

#вычисление сантимента по ckjdfh,
tail(data_corpus_inaugural) |>
  textstat_polarity(dictionary = data_dictionary_geninqposneg)

#другой словарь
print(data_dictionary_ANEW, max_nval = 8)

#валетность каждого слова в словаре
lapply(valence(data_dictionary_ANEW), head, 8)

#вычисляем значение pleuasre для каждого текста
tail(data_corpus_inaugural) |>
  textstat_valence(dictionary = data_dictionary_ANEW["pleasure"])


#вычисление полярности текста
data("data_dictionary_LSD2015", package = "quanteda.sentiment")
sent_pol <- tail(data_corpus_inaugural, 25) |>
  textstat_polarity(dictionary = data_dictionary_LSD2015)
sent_pol <- dplyr::mutate(sent_pol, polarity = sentiment)
sent_val <- tail(data_corpus_inaugural, 25) |>
  textstat_valence(dictionary = data_dictionary_AFINN)


#создание совоего словаря с валетностью
dict <- dictionary(list(quality = c("bad", "awful", "horrific",
                                    "good", "great", "amazing")))
dict
valence(dict) <- list(quality = c(amazing = 2.2, awful = -1.5, bad = -1, 
                                  horrific = -2, good = 1, great = 1.7))
valence(dict)

#задание своего словаря с позитивной и негативной окраской
dict <- dictionary(list(neg = c("bad", "awful", "horrific", 
                                "not", "ugly"),
                        pos = c("good", "great", "amazing")))
valence(dict) <- list(neg = -1, pos = 1)

#применение словаря
txt <- c(doc1 = "This is a fantastic, wonderful example.",
         doc2 = "The settlement was not amiable.",
         doc3 = "The good, the bad, and the ugly.")
toks <- tokens(txt)
toks  |>
  textstat_valence(dictionary = dict)

#замена каждого слова на окраску
tokens_lookup(toks, data_dictionary_LSD2015, nested_scope = "dictionary", 
              exclusive = FALSE)

#определяем полярность
textstat_polarity(toks, data_dictionary_LSD2015)
textstat_valence(toks, data_dictionary_AFINN)

#источник
# https://rdrr.io/github/quanteda/quanteda.sentiment/f/vignettes/sentiment_analysis.Rmd

# виды словарей 
# https://github.com/quanteda/quanteda.sentiment
# другой пакет https://cran.r-project.org/web/packages/syuzhet/vignettes/syuzhet-vignette.html
#### LDA (ДЕРИХЛЕ) с интересной визуализацией ####
# источник https://github.com/cran/sentopics
# devtools::install_github("odelmarcelle/sentopics") 
library(xts)
library(sentopics)
print(ECB_press_conferences_tokens, 2)

#уменьшу число документов
lda <- LDA(ECB_press_conferences_tokens[1:500],
           K = 3, alpha = .1)
lda <- grow(lda, 100)
lda

#распределение текстов по топикам
head(lda$theta) 

#какие слова входят в топики
topWords(lda, output = "matrix") 

#визуализация
plot(lda)

#усреднение сантимента по временным периодам (тут месяцы)
proportion_topics(lda, period = "month") |> head(2)

#визуализация (видимо нормированная доля словарей)
plot_sentiment_breakdown(lda, period = "quarter", 
                         rolling_window = 3)



#### Поиск слов связанных с данными словами ####
# источник https://tutorials.quanteda.io/advanced-operations/target-word-collocations/
library(quanteda)
library(quanteda.textstats)
library(quanteda.corpora)

corp_news <- download("data_corpus_guardian")
toks_news <- tokens(corp_news[1:500], remove_punct = TRUE)

#выделем целевые слова
eu <- c("EU", "europ*", "european union")

#оставим слова, которые находятся не дальше, чем 10 слов
#от данного (выделение целевых слов)
toks_inside <- tokens_keep(toks_news, pattern = eu, window = 10)

#удалим сами целевые слова
toks_inside <- tokens_remove(toks_inside, pattern = eu)

#оставим только слова, которые вне окна в 10 слов 
#от целеых
toks_outside <- tokens_remove(toks_news,
                              pattern = eu, window = 10)

#dfm матрицы
dfmat_inside <- dfm(toks_inside)
dfmat_outside <- dfm(toks_outside)

#ищем схожие слова
tstat_key_inside <- textstat_keyness(rbind(dfmat_inside, dfmat_outside), 
                                     target = seq_len(ndoc(dfmat_inside)))
head(tstat_key_inside, 50)
#чем выше слово, тем больше связь
#### Базовые операции с корпусами текстов ####
# Источник: https://tutorials.quanteda.io/basic-operations/


brary(e1071)
library(mlbench)
library(ggplot2)
library(JOUSBoost)
library(caret)
library(dplyr)
library(randomForest)
library(AER)
library(earth)
library(gbm)
library("earth")
library(RColorBrewer) # Откроем библиотеку RColorBrewer:
display.brewer.all() # Посмотрим, какие в ней имеются палитры

# выберем цвета из палитры Set2 по количеству секторов в круге:
colors = brewer.pal(10,"Set1")

# И используем их при визуализации
par(mar = c(5, 5, 5, 5)) # установим поля
pie(okr$ХимЭкспорт, names2, main = "Доля федеральных округов в экспорте \n продукции химической промышленности", col=colors)

#данные все для ML
data_all <- read.xlsx("DATA_ALL23.xlsx")

data_all_sent <- read.xlsx("DATA_ALL23_sent.xlsx")

data_ALL_sent_final <- merge(data_all,data_all_sent, by = "Ticker")
data_ALL_sent_final <- merge(data_ALL_sent_final,part_all, by ="Ticker")




#добавим переменные
data_new <- mutate(data_ALL_sent_final,   NI = ifelse(Net_income>0, 1, 0),  
                   tech = ifelse (Sector == "Tech", 1, 0),
                   underpriced = ifelse(Day_Px_Chng>0,1,0),
                   uncertainty_perc = scale(uncertaint_perc),
                   word_sum = sum_row, positive_percent = scale(positive_perc), 
                   Litigious_percent=scale(Litigious_perc), negative_percent = scale(negative_perc),
                   uncert_part1 = scale(uncert_perc_part1),uncert_part2 = scale(uncert_perc_part2))


#удаляем факторные и ненужные 
data_new_new_ml <- dplyr::select(data_new,-IPO.year, -Ticker,-Day_Close ,-Change_Close,-Opening_Price,-Sector2, -Sector, 
                                 -IPO_proceed, 
                                 -sum_row,-Change_Opening, -Market_Capitalization, -Day_Px_Chng)


#удаляем ненужные текстовые переменные
data_new_new_ml_ <- data_new_new_ml[,-c(32:39)]
data_new_new_ml_2 <- data_new_new_ml_[,-c(38:45)]
data_new_new_ml_3 <- data_new_new_ml_2 [,-c(45:52)]
data_new_new_ml_4 <- data_new_new_ml_3[,-c(52:59)]
data_new_new_ml_5 <- data_new_new_ml_4[,-c(59:66)]
data_new_new_ml <- data_new_new_ml_5 

#отбираем только переменные интереса



data_new_new_ml <- dplyr::select(data_new_new_ml, Total_assets,R_D,Net_income,Book_ValperShare,CAPEX,ROE,ROA,EBITDA_margin,Current_ratio,
                                 Under_rank,VC.Dummy, Rollup_dummy,Dual_dummy,Internet_dummy,Float ,Net_debt,
                                 Issued,Shareholder_equity,Age,NI,tech,uncertainty_perc,negative_percent,word_sum,positive_percent,
                                 Litigious_percent,uncert_part1,uncert_part2,underpriced, 
                                 Offer_Price,Net_Debt_Ebitda,IPO_num_scoop)


#должно быть  75 колонок           




#удаляем пропуски
data_new_new_ml <- na.omit(data_new_new_ml)
#описательная статистика
describe(data_new_new_ml)
summary(data_new_new_ml)
str(data_new_new_ml)
View(data_new_new_ml)
#разделение на две подвыборки
set.seed(391)
# class зависимая переменная, 75 % в выборку обучения
inTraining <- createDataPartition(data_new_new_ml$underpriced , p = .75, list = FALSE)
#sort(sample(1:length(Sonar$Class), size = length(Sonar$Class)/4*3, replace = FALSE))
training <-data_new_new_ml [ inTraining,]
testing  <- data_new_new_ml [-inTraining,]
training$underpriced
#настройка десяти-кратной кросс-валидации
#первый параметр отвечает за то насколько блоков мы разобьем данные (на 9 кусочках обучаем на 10 качество модели измеряем)
#запиши 10 р квадратов и так 10 раз
fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10)

#number - "порядок" кросс-валидации (к)
#repeats - количество повторений k-кратной кросс-валидации

#методы:
#"cv" - кросс-валидация
#"repeatedcv" - повторная кросс-валидация

#метрики "Accuracy", "Rsquared"
#если задавать метрику "ROC" (площадь под ROC кривой), то нужно отдельно 
#оценивать веротяности классов classProbs = TRUE и summaryFunction = twoClassSummary

#обучение модели через бустинг
set.seed(825)
gbmFit1 <- caret::train(underpriced ~ ., data = training, 
                        method = "gbm", 
                        trControl = fitControl,
                        verbose = FALSE)
gbmFit1
gbmFit1$bestTune
gbmFit1$results
View(data_new_new_ml)
#verbose = FALSE - нужно именно для gbm

#можно отдельно задать сетку параметров для перебора
gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 10), 
                        n.trees = (1:15)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

?expand.grid
#обучение
set.seed(981)
gbmFit2 <- caret::train(underpriced  ~ ., data = data_new_new_ml, 
                        method = "gbm", 
                        trControl = fitControl, 
                        verbose = FALSE, 
                        tuneGrid = gbmGrid)
gbmFit2
getTrainPerf(gbmFit2)
gbmFit2$bestTune

#визаулизируем
plot(gbmFit2)

#собственные цвета
ggplot(gbmFit2) + geom_point(col = "darkblue", size = 0.7) + scale_color_manual(values = c("#550445", "#c96588", "#5f73b3","#006400")) + theme_bw() 
+ theme(legend.position = "bottom")+labs(title = "Название графика")

#другая визуализация
plot(gbmFit2,  plotType = "level",
     scales = list(x = list(rot = 90)))
ggplot(gbmFit2,  plotType = "level")
#scales = list(x = list(rot = 90)) - параметр отображения подписей осей


summary_proxy <- as.data.frame(summary(
  gbmFit2, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2,
))

ggplot(summary_proxy, aes(x = reorder(var, rel.inf), y = rel.inf, fill = rel.inf)) + geom_col(color = "black") + coord_flip() + theme_bw() + ylab("Относительная важность переменной") + xlab("") + labs("Значения") +
  scale_fill_gradient2(low="#d2cdca", mid="#929fb0",
                       high="#4a5060", midpoint=20) + theme(legend.position = "bottom")


ggplot(head(summary_proxy, 10), aes(x = reorder(var, rel.inf), y = rel.inf, fill = rel.inf)) +
  geom_col(color = "black") +
  coord_flip() +
  theme_bw() +
  ylab("Относительная важность переменной") +
  xlab("") +
  labs("Значения") +
  scale_fill_gradient2(low="#FF7F50", mid="#FF4500",
                       high="#DC143C", midpoint=3) +
  theme(legend.position = "bottom")

#scales = list(x = list(rot = 90)) - параметр отображения подписей осей
library(gbm)


#оценим модель через максимизацию площади под ROC кривой

##заменим в данных 1/0 на underpriced/overpriced
#данные все для ML
data_all <- read.xlsx("DATA_ALL23.xlsx")

data_all_sent <- read.xlsx("DATA_ALL23_sent.xlsx")

data_ALL_sent_final <- merge(data_all,data_all_sent, by = "Ticker")
data_ALL_sent_final <- merge(data_ALL_sent_final,part_all, by ="Ticker")




#добавим переменные
data_new <- mutate(data_ALL_sent_final,   NI = ifelse(Net_income>0, 1, 0),  
                   tech = ifelse (Sector == "Tech", 1, 0),
                   underpriced = ifelse(Day_Px_Chng>0,"yes","no"),
                   uncertainty_perc = scale(uncertaint_perc),
                   word_sum = sum_row, positive_percent = scale(positive_perc), 
                   Litigious_percent=scale(Litigious_perc), negative_percent = scale(negative_perc),
                   uncert_part1 = scale(uncert_perc_part1),uncert_part2 = scale(uncert_perc_part2))

#удаляем факторные и ненужные 
data_new_new_ml <- dplyr::select(data_new,-IPO.year, -Ticker,-Day_Close ,-Change_Close,-Opening_Price,-Sector2, -Sector, 
                                 -IPO_proceed, 
                                 -sum_row,-Change_Opening, -Market_Capitalization, -Day_Px_Chng)


#удаляем ненужные текстовые переменные
data_new_new_ml_ <- data_new_new_ml[,-c(32:39)]
data_new_new_ml_2 <- data_new_new_ml_[,-c(38:45)]
data_new_new_ml_3 <- data_new_new_ml_2 [,-c(45:52)]
data_new_new_ml_4 <- data_new_new_ml_3[,-c(52:59)]
data_new_new_ml_5 <- data_new_new_ml_4[,-c(59:66)]
data_new_new_ml <- data_new_new_ml_5 

#должно быть  75 колонок   


#отбираем только переменные интереса

data_new_new_ml <- dplyr::select(data_new_new_ml, Total_assets,R_D,Net_income,Book_ValperShare,CAPEX,ROE,ROA,EBITDA_margin,Current_ratio,
                                 Under_rank,VC.Dummy, Rollup_dummy,Dual_dummy,Internet_dummy,Float ,Net_debt,
                                 Issued,Shareholder_equity,Age,NI,tech,uncertainty_perc,negative_percent,word_sum,positive_percent,
                                 Litigious_percent,uncert_part1,uncert_part2,underpriced, 
                                 Offer_Price,Net_Debt_Ebitda,IPO_num_scoop)


#удаляем пропуски
data_new_new_ml <- na.omit(data_new_new_ml)

#разделение на две подвыборки
set.seed(391)
# class зависимая переменная, 75 % в выборку обучения
inTraining <- createDataPartition(data_new_new_ml$underpriced , p = .75, list = FALSE)
#sort(sample(1:length(Sonar$Class), size = length(Sonar$Class)/4*3, replace = FALSE))
training <-data_new_new_ml [ inTraining,]
testing  <- data_new_new_ml [-inTraining,]

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)
#оценка модели
set.seed(825)


gbmFit3 <- caret::train(underpriced ~ . , data = data_new_new_ml, 
                        method = "gbm", 
                        trControl = fitControl, 
                        verbose = FALSE, 
                        tuneGrid = gbmGrid,
                        metric = "ROC")


gbmFit3$bestTune
getTrainPerf(gbmFit3)


#оценим через случайный лес
#method = 'rf'

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)


rfFit3 <- caret::train(underpriced ~ . , data = data_new_new_ml, trControl = TR,
                       method = "rf", tuneLength = 3,
                       metric = "ROC")

#сравним модели
resamps <- resamples(list(GBM = gbmFit3,
                          RF = rfFit3))
summary(resamps)

#визуализация
bwplot(resamps, layout = c(1, 3))
dotplot(resamps, metric = "Spec")


#c цветами 

myColours <- brewer.pal(6,"BuPu")
my.settings <- list(
  superpose.polygon=list(col=myColours[2:5], border="transparent"),
  strip.background=list(col=myColours[6]),
  strip.border=list(col="black")
)
bwplot(resamps,  col = "black", fill = "#8B008B", par.settings = my.settings, par.strip.text=list(col="white", font=2))


#ФИНАЛЬНАЯ МОДЕЛЬ БУСТИНГ 
data_new_new_ml <- as.data.frame(data_new_new_ml)
gbm.fit_cv_proxy <- gbm(
  formula = underpriced ~ ., data = data_new_new_ml,
  n.trees = 150,
  interaction.depth = 1,
  shrinkage = 0.1,
  n.minobsinnode = 20,
  verbose = FALSE
)  
summary_cv_proxy <- as.data.frame(summary(
  gbm.fit_cv_proxy, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
))

par(mar = c(5, 8, 1, 1))
summary_cv_proxy <- as.data.frame(summary(
  gbm.fit_cv_proxy, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
))

ggplot(summary_cv_proxy, aes(x = reorder(var, rel.inf), y = rel.inf, fill = rel.inf)) + geom_col(color = "black") + coord_flip() + theme_bw() + ylab("Относительная важность переменной") + xlab("") + labs("Значения") +
  scale_fill_gradient2(low="#d2cdca", mid="#929fb0",
                       high="#4a5060", midpoint=15) + theme(legend.position = "bottom")

ggplot(gbmFit2, plotType = "level") + scale_fill_gradient2(low="#f7e8e3", mid="#005ec4",
                                                           high="#02062f", midpoint=1.05) + theme_bw() + theme(legend.position = "bottom") + ylab("Глубина дерева") + xlab("Количество деревьев")
install.packages("pdp")
library(pdp)
gbm.fit_cv_proxy %>%
  pdp::partial(pred.var = "Offer_Price", n.trees = gbm.fit_cv_proxy$n.trees) %>%
  autoplot(rug = TRUE, train =data_new_new_ml, col = "darkblue", size = 1) + theme_bw() + 
  ylab("Прогноз доходности в первый день")

explainer_gbm_proxy <- lime::lime(
  x              = data_sum3, 
  model          = gbm_cv_proxy1, 
  bin_continuous = FALSE
)








#можно делать прогнозы

#######
# SVM #
#######

#method = "svmPoly" - полиномиальное ядро
#method = "svmLinear" - линейное ядро 
#method = "svmRadial" - радиально базисная фнукция
TR <- trainControl(method = "repeatedcv",
                   number = 10,
                   repeats = 10,
                   classProbs = TRUE,
                   summaryFunction = twoClassSummary)


#логистическая регрессия

mod_log <- caret::train(underpriced ~ ., data = data_new_new_ml, trControl = TR,
                        family = "binomial", method = "glm")
mod_log

#lasso

#LASSO 

mod_lasso <- train(underpriced ~ .,data = data_new_new_ml, method = "glmnet",trControl =TR,tuneGrid=expand.grid(
  .alpha=1,
  .lambda=seq(0, 0.1, by = 0.01)))

#случайный лес
mod_log_tr <- caret::train(underpriced ~ ., data = data_new_new_ml, trControl = TR,
                           method = "rf", tuneLength = 3)

#gmb
mod_log_gmb <- caret::train(underpriced ~ ., data = data_new_new_ml, trControl = TR,
                            method = "gbm", tuneLength = 3, verbose = FALSE)

#svm
mod_log_svm <- caret::train(underpriced ~ ., data = data_new_new_ml, trControl = TR,
                            method = "svmRadial", tuneLength = 3, verbose = FALSE)


#сравнение
Comparison <- resamples(list(Logit= mod_log,
                             Lasso = mod_lasso,
                             RF = mod_log_tr,
                             GBM = mod_log_gmb,
                             SVM = mod_log_svm))
bwplot(Comparison)


summary(Comparison)
bwplot(Comparison)
dotplot(Comparison, metric = "Accuracy")


#c цветами 

myColours <- brewer.pal(6,"BuGn")
my.settings <- list(
  superpose.polygon=list(col=myColours[2:5], border="transparent"),
  strip.background=list(col=myColours[6]),
  strip.border=list(col="black")
)
bwplot(Comparison,  col = "black", fill = "#3CB371", par.settings = my.settings, par.strip.text=list(col="white", font=2))
?bwplot
ggplot(Comparison3)
dotplot(Comparison, par.strip.text=list(col="white", font=2), par.settings = my.settings)









#поиск различий
Comparison2 <- diff(Comparison)
summary(Comparison2)


#какая переменная была самой важной?
ggplot(varImp(mod_log))


varImp(mod_log)
summary(mod_log)

#каково качество модели?
confusionMatrix(mod_log)

#получаем статистику модели
getTrainPerf(mod_log)

#можно ли сразу узнать площадь под ROC?
TR_ROC <- trainControl(method = "cv", number = 10, classProbs = TRUE, 
                       summaryFunction = twoClassSummary)
mod_log <- caret::train(A ~ . - affairs, data = data, trControl = TR_ROC,
                        family = "binomial", method = "glm", metric = "ROC")
mod_log

#для регрессии: регуляризация - добавляем шатрф за
#"ridge" - сумму квадартов коэффициентов
#"lasso" - сумму модлуей коэффициентов







####тоже самое только с зависимой переменной ДОХОДНОСТИ####


#данные все для ML
data_all <- read.xlsx("DATA_ALL23.xlsx")

data_all_sent <- read.xlsx("DATA_ALL23_sent.xlsx")

data_ALL_sent_final <- merge(data_all,data_all_sent, by = "Ticker")
data_ALL_sent_final <- merge(data_ALL_sent_final,part_all, by ="Ticker")




#добавим переменные
data_new <- mutate(data_ALL_sent_final,   NI = ifelse(Net_income>0, 1, 0),  
                   tech = ifelse (Sector == "Tech", 1, 0),
                   underpriced = ifelse(Day_Px_Chng>0,1,0),
                   uncertainty_perc = scale(uncertaint_perc),
                   word_sum = sum_row, positive_percent = scale(positive_perc), 
                   Litigious_percent=scale(Litigious_perc), negative_percent = scale(negative_perc),
                   uncert_part1 = scale(uncert_perc_part1),uncert_part2 = scale(uncert_perc_part2))

#удаляем факторные и ненужные 
data_new_new_ml <- dplyr::select(data_new,-IPO.year, -Ticker,-Day_Close ,-Change_Close,-Opening_Price,-Sector2, -Sector, 
                                 -IPO_proceed, 
                                 -sum_row,-Change_Opening, -Market_Capitalization, -underpriced)


#удаляем ненужные текстовые переменные
data_new_new_ml_ <- data_new_new_ml[,-c(32:39)]
data_new_new_ml_2 <- data_new_new_ml_[,-c(38:45)]
data_new_new_ml_3 <- data_new_new_ml_2 [,-c(45:52)]
data_new_new_ml_4 <- data_new_new_ml_3[,-c(52:59)]
data_new_new_ml_5 <- data_new_new_ml_4[,-c(59:66)]
data_new_new_ml <- data_new_new_ml_5 

#должно быть  74 колонок           
#отбираем только переменные интереса



data_new_new_ml <- dplyr::select(data_new_new_ml, Total_assets,R_D,Net_income,Book_ValperShare,CAPEX,ROE,ROA,EBITDA_margin,Current_ratio,
                                 Under_rank,VC.Dummy, Rollup_dummy,Dual_dummy,Internet_dummy,Float ,Net_debt,
                                 Issued,Shareholder_equity,Age,NI,tech,uncertainty_perc,negative_percent,word_sum,positive_percent,
                                 Litigious_percent,uncert_part1,uncert_part2,Day_Px_Chng, 
                                 Offer_Price,Net_Debt_Ebitda,IPO_num_scoop)





#удаляем пропуски
data_new_new_ml <- na.omit(data_new_new_ml)
#описательная статистика
describe(data_new_new_ml)
summary(data_new_new_ml)
str(data_new_new_ml)
View(data_new_new_ml)
#разделение на две подвыборки
set.seed(998)
# class зависимая переменная, 75 % в выборку обучения
inTraining <- createDataPartition(data_new_new_ml$Day_Px_Chng , p = .75, list = FALSE)
#sort(sample(1:length(Sonar$Class), size = length(Sonar$Class)/4*3, replace = FALSE))
training <-data_new_new_ml [ inTraining,]
testing  <- data_new_new_ml [-inTraining,]

#настройка десяти-кратной кросс-валидации
#первый параметр отвечает за то насколько блоков мы разобьем данные (на 9 кусочках обучаем на 10 качество модели измеряем)
#запиши 10 р квадратов и так 10 раз
fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10)

#number - "порядок" кросс-валидации (к)
#repeats - количество повторений k-кратной кросс-валидации

#методы:
#"cv" - кросс-валидация
#"repeatedcv" - повторная кросс-валидация

#метрики "Accuracy", "Rsquared"
#если задавать метрику "ROC" (площадь под ROC кривой), то нужно отдельно 
#оценивать веротяности классов classProbs = TRUE и summaryFunction = twoClassSummary

#обучение модели через бустинг
set.seed(825)
gbmFit1 <- caret::train(Day_Px_Chng ~ ., data = data_new_new_ml, 
                        method = "gbm", 
                        trControl = fitControl,
                        verbose = FALSE)
gbmFit1
gbmFit1$bestTune
gbmFit1$results

#verbose = FALSE - нужно именно для gbm

#можно отдельно задать сетку параметров для перебора
gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 10), 
                        n.trees = (1:15)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

?expand.grid
#обучение
set.seed(825)
gbmFit2 <- caret::train(Day_Px_Chng  ~ ., data = data_new_new_ml, 
                        method = "gbm", 
                        trControl = fitControl, 
                        verbose = FALSE, 
                        tuneGrid = gbmGrid)
gbmFit2
getTrainPerf(gbmFit2)
gbmFit2$bestTune
training$Day_Px_Chng 
#визаулизируем
plot(gbmFit2)

#собственные цвета
ggplot(gbmFit2) + geom_point(col = "darkblue", size = 0.7) + scale_color_manual(values = c("#550445", "#c96588", "#5f73b3","#006400")) + theme_bw() 
+ theme(legend.position = "bottom")+labs(title = "Название графика")

#другая визуализация
plot(gbmFit2,  plotType = "level",
     scales = list(x = list(rot = 90)))
ggplot(gbmFit2,  plotType = "level")

# График, используя ggplot2 красивый с палитрой своей
ggplot(gbmFit2, plotType = "level",aes(x = interaction.depth, y = n.trees, fill = cv.error)) +
  geom_tile() +
  scale_fill_gradient(low = "#E0FFFF", high = "darkblue") +
  labs(title = "Зависимость ошибки RMSE от гиперпараметров",
       x = "Количество деревьев ", y = "Глубина деревьев")


#ВАЖНОСТЬ ПЕРЕМЕННЫХ
library(ggplot2)
library(gbm)

#scales = list(x = list(rot = 90)) - параметр отображения подписей осей


summary_proxy <- as.data.frame(summary(
  gbmFit2, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2,
))

ggplot(summary_proxy, aes(x = reorder(var, rel.inf), y = rel.inf, fill = rel.inf)) + geom_col(color = "black") + coord_flip() + theme_bw() + ylab("Относительная важность переменной") + xlab("") + labs("Значения") +
  scale_fill_gradient2(low="#d2cdca", mid="#929fb0",
                       high="#4a5060", midpoint=20) + theme(legend.position = "bottom")


ggplot(head(summary_proxy, 10), aes(x = reorder(var, rel.inf), y = rel.inf, fill = rel.inf)) +
  geom_col(color = "black") +
  coord_flip() +
  theme_bw() +
  ylab("Относительная важность переменной") +
  xlab("") +
  labs("Значения") +
  scale_fill_gradient2(mid="#AFEEEE", low="#E0FFFF",
                       high="#008080", midpoint=5) +
  theme(legend.position = "bottom")

#scales = list(x = list(rot = 90)) - параметр отображения подписей осей
library(gbm)


#оценим модель
fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10)
#оценка модели
set.seed(825)

training$Day_Px_Chng <- as.factor(training$Day_Px_Chng)

gbmFit3 <- train(Day_Px_Chng   ~ . , data = training, 
                 method = "gbm", 
                 trControl = fitControl, 
                 verbose = FALSE, 
                 tuneGrid = gbmGrid)


gbmFit3$bestTune
getTrainPerf(gbmFit3)


#оценим через случайный лес 
#method = 'rf'

fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10)

mod_rf <- caret::train(Day_Px_Chng ~ ., data = data_new_new_ml, trControl = fitControl, 
                       metric = "Rsquared", method = "rf")
mod_rf

#таблица сопряженности
confusionMatrix.train(mod_rf)

#сравним модели
resamps <- resamples(list(GBM = gbmFit2,
                          RF = mod_rf))
summary(resamps)

#визуализация
bwplot(resamps, layout = c(1, 3))
dotplot(resamps, metric = "Spec")

#поиск различий
difValues <- diff(resamps)
summary(difValues)

#пример обучения моделей без найтроки параметров
fitControl <- trainControl(method = "none", classProbs = TRUE)

#обучение модели
set.seed(825)
gbmFit4 <- caret::train(Class ~ ., data = training, 
                        method = "gbm", 
                        trControl = fitControl, 
                        verbose = FALSE, 
                        tuneGrid = data.frame(interaction.depth = 4,
                                              n.trees = 100,
                                              shrinkage = .1,
                                              n.minobsinnode = 20),
                        metric = "ROC")
gbmFit4
summary(gbmFit4)
#можно делать прогнозы

#######
# SVM #
#######

#method = "svmPoly" - полиномиальное ядро
#method = "svmLinear" - линейное ядро 
#method = "svmRadial" - радиально базисная фнукция

#ФИНАЛЬНАЯ МОДЕЛЬ БУСТИНГ 
data_new_new_ml <- as.data.frame(data_new_new_ml)
gbm.fit_cv_proxy <- gbm(
  formula = Day_Px_Chng ~ ., data = data_new_new_ml,
  n.trees = 100,
  interaction.depth = 5,
  shrinkage = 0.1,
  n.minobsinnode = 20,
  verbose = FALSE
)  
summary_cv_proxy <- as.data.frame(summary(
  gbm.fit_cv_proxy, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
))

par(mar = c(5, 8, 1, 1))
summary_cv_proxy <- as.data.frame(summary(
  gbm.fit_cv_proxy, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
))

ggplot(summary_cv_proxy, aes(x = reorder(var, rel.inf), y = rel.inf, fill = rel.inf)) + geom_col(color = "black") + coord_flip() + theme_bw() + ylab("Относительная важность переменной") + xlab("") + labs("Значения") +
  scale_fill_gradient2(low="#d2cdca", mid="#929fb0",
                       high="#4a5060", midpoint=15) + theme(legend.position = "bottom")

ggplot(gbmFit2, plotType = "level") + scale_fill_gradient2(low="#f7e8e3", mid="#005ec4",
                                                           high="#02062f", midpoint=1.05) + theme_bw() + theme(legend.position = "bottom") + ylab("Глубина дерева") + xlab("Количество деревьев")
install.packages("pdp")
library(pdp)
gbm.fit_cv_proxy %>%
  pdp::partial(pred.var = "Offer_Price", n.trees = gbm.fit_cv_proxy$n.trees) %>%
  autoplot(rug = TRUE, train =data_new_new_ml, col = "darkblue", size = 1) + theme_bw() + 
  ylab("Прогноз доходности в первый день")

explainer_gbm_proxy <- lime::lime(
  x              = data_sum3, 
  model          = gbm_cv_proxy1, 
  bin_continuous = FALSE
)




















#лассо регрессия

data_new_new_ml <- data.frame(data_new_new_ml)
mod_lasso <- glmnet(data_new_new_ml[,-29],data_new_new_ml[,29], alpha = 1)
plot(mod_lasso,label =TRUE)


# Построить график без легенды
plot(mod_lasso, label = TRUE)

mod_lasso <- glmnet(data_new_new_ml[,-29], data_new_new_ml[,29], alpha = 1)
plot(mod_lasso, label = TRUE)
library(Matrix)
coef_df <- as.data.frame.matrix(coef(mod_lasso$finalModel,mod_lasso$bestTune$lambda))
coef_df <- tail(coef_df, -1)
coef_df$variable <- rownames(coef_df)
colnames(coef_df)[1] <- "Intercept"
coef_df$num <- seq(1, nrow(coef_df))

# вывод таблицы на экран
print(coef_df[, c("num", "variable", "Intercept")])
colnames(coef_df) <- c("Значение коэффициента", "Переменная","Номер")

# оформление таблицы
coef_df %>%
  kable(format = "html", digits = 5, align = "c", caption = "Коэффициенты модели LASSO") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)


library(htmlwidgets)

coef_df %>%
  kable(format = "html", digits = 4, align = "c", caption = "Коэффициенты модели LASSO") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
  as_widget() %>%
  saveWidget(file = "coef_table.html")


# Добавить легенду
plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")
legend(x = "topright", y = 0.5, legend = var.names, col = 1:length(var.names), lty = 1:length(var.names), bty = "n")

legend("right", inset=c(0,0.05), legend = var.names, col = 1:length(var.names), lty = 1:length(var.names), bty = "n")


#выбор лямбды через кроссвалидацию
data_new_new_ml <- as.matrix(data_new_new_ml)
mod_lasso2 <- cv.glmnet(data_new_new_ml[,-29],data_new_new_ml[,29], alpha = 1)
plot(mod_lasso2)

par("mar")

#лямбды
mod_lasso2$lambda.min
mod_lasso2$lambda.1se

#подставим в модель

mod_lasso3 <- glmnet(data_new_new_ml[,-29],data_new_new_ml[,29], alpha = 1, lambda = mod_lasso2$lambda.1se)
coef(mod_lasso3)


#LASSO ФИНАЛ
#подбор альфы
TR <- trainControl(method = "repeatedcv", number = 10,repeats = 10)
mod_lasso <- train(Day_Px_Chng ~.,data =data_new_new_ml, method = "glmnet",trControl =TR, tuneGrid=expand.grid(
  .alpha=1,
  .lambda=seq(0, 0.1, by = 0.01)))

mod_lasso$bestTune

mod_lasso$finalModel
coef(mod_lasso$finalModel,mod_lasso$bestTune$lambda)
View(training)

library(Matrix)

# Получение коэффициентов модели
coef_mat <- coef(mod_lasso$finalModel, mod_lasso$bestTune$lambda)

# Преобразование матрицы в data.frame
coef_df <- as.data.frame(as.matrix(coef_mat))

# Добавление имени переменной в data.frame
coef_df$variable <- rownames(coef_df)

# Переименование столбцов
names(coef_df) <- c("coefficient", "variable")

# Упорядочивание по убыванию величины коэффициентов
coef_df <- coef_df[order(-coef_df$coefficient), ]

library(ggplot2)

# Отбор ненулевых коэффициентов
non_zero_coef <- coef_df[abs(coef_df$coefficient) > 0.001,]

# Построение графика
ggplot(data = non_zero_coef, aes(x = variable, y = coefficient)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  xlab("Переменные") +
  ylab("Коэффициенты") +
  ggtitle("Ненулевые коэффициенты модели лассо-регрессии")


# финальная модель
mod_lasso_final <- glmnet(training[,-29],training[,29], alpha = mod_lasso$bestTune$alpha ,lambda = mod_lasso$bestTune$lambda)
coef(mod_lasso_final)

plot(mod_lasso_final,label =TRUE)







#обычная регреесия

mod_lm <- train(Day_Px_Chng ~ ., data = data_new_new_ml, method = "lm" ,trControl = TR)

#случайный лес
mod_log_tr <- caret::train(Day_Px_Chng ~ ., data = data_new_new_ml, trControl = TR,
                           method = "rf", tuneLength = 3)

#gmb
mod_log_gmb <- caret::train(Day_Px_Chng ~ ., data = data_new_new_ml, trControl = TR,
                            method = "gbm", tuneLength = 3, verbose = FALSE)

#svm
mod_log_svm <- caret::train(Day_Px_Chng~ ., data = data_new_new_ml, trControl = TR,
                            method = "svmRadial", tuneLength = 3, verbose = FALSE)


#сравнение
Comp <- resamples(list(LM= mod_lm,
                       Lasso = mod_lasso,
                       RF = mod_log_tr,
                       GBM = mod_log_gmb,
                       SVM = mod_log_svm))
bwplot(Comp)
bwplot(Comp, palette = "Blues")


#сравнение
Comparison <- resamples(list(LM= mod_lm,
                             Lasso = mod_lasso,
                             RF = mod_log_tr,
                             GBM = mod_log_gmb,
                             SVM = mod_log_svm))
summary(Comparison)
bwplot(Comparison)
dotplot(Comparison, metric = "Rsquared")

#поиск различий
Comparison2 <- diff(Comparison)
summary(Comparison2)
#c цветами 
par(mfrow = c(1,3))
myColours <- brewer.pal(6,"Blues")
my.settings <- list(
  superpose.polygon=list(col=myColours[2:5], border="transparent"),
  strip.background=list(col=myColours[6]),
  strip.border=list(col="black")
)
bwplot(Comparison,  col = "black", fill = "#4682B4", par.settings = my.settings, par.strip.text=list(col="white", font=2))
?bwplot
ggplot(Comparison)
dotplot(Comparison, par.strip.text=list(col="white", font=2), par.settings = my.settings)




#какая переменная была самой важной?
ggplot(varImp(mod_log))


varImp(mod_log)
summary(mod_log)
?varImp

#каково качество модели?
confusionMatrix(mod_log)

#получаем статистику модели
getTrainPerf(mod_log)

#можно ли сразу узнать площадь под ROC?
TR_ROC <- trainControl(method = "cv", number = 10, classProbs = TRUE, 
                       summaryFunction = twoClassSummary)
mod_log <- caret::train(A ~ . - affairs, data = data, trControl = TR_ROC,
                        family = "binomial", method = "glm", metric = "ROC")
mod_log

#для регрессии: регуляризация - добавляем шатрф за
#"ridge" - сумму квадартов коэффициентов
#"lasso" - сумму модлуей коэффициентов



#ЕСЛИ ЗАДАТЬ ДЛЯ МОДЕЛИ ЛУЧШИЕ ЕЕ ПАРАМЕТРЫ








































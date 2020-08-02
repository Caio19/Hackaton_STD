#### Pacotes utilizados ####

library(tidyverse)
library(forecast)
library(readxl)
library(lubridate)
library(pastecs)
library(fitdistrplus)

#### Lendo a base de dados ####

#fonte original: http://www.dadosefatos.turismo.gov.br/2016-02-04-11-53-05.html?limitstart=0

dados = read_excel("C:\\Users\\caioc\\Documents\\hackaton\\Dados.turistas.xlsx")


### Tratamento dos dados ####

glimpse(dados)


# Convertendo o formato da data

dados = dados%>%
  mutate_at("Data", ymd)

#Removendo duplicatas (Erro de coleta)

dados = dados%>%distinct()%>%
  arrange(Data)

#Definindo a série temporal

TS = ts(dados$`Total Turistas`, frequency = 12)


#### Análise gráfica ####

ggplot(dados, aes(x = Data, y= `Total Turistas` ))+geom_line()+geom_smooth(method = "lm")
#Aparentemente os dados possuem sazonalidade e tendência 

plot.ts(TS)

hist(dados$`Total Turistas`)

boxplot(dados$`Total Turistas`)


#### Teste de hipoteses para tendencia ####

trend.test(TS)

#Conclusão: Havia tendência significativa de crescimento no volume de turistas anual 

# (P-valor < 0.05)

### Analise de Séries temporais #### 

plot.ts(diff(TS))
trend.test(diff(TS))
#Uma diferenciacao remove a tendencia

acf(diff(TS))
#ordem MA: Possivelmente até 13 

pacf(diff(TS))
#ordem autorregrassiva: Possivelmente até 10 


#Ajuste do melhor modelo com base no AIC

fit = auto.arima(TS, max.p = 10, max.q = 13,ic = "aic")

summary(fit)
fit


#Qualidade do ajuste

hist(fit$residuals)

res.fit = fitdist(as.numeric(fit$residuals), "norm")
plot(res.fit)
#Ajuste aparentemente razoavel de normalidade dos residuos
#Mas o teste de shapiro aponta mal ajuste
shapiro.test(fit$residuals)


#### Ajuste 2 - transformação logarítma ####

TSlog = log(TS)

plot.ts(diff(TSlog))
trend.test(diff(TSlog))
#Uma diferenciacao remove a tendencia

acf(diff(TSlog))
#ordem MA: Possivelmente até 14

pacf(diff(TSlog))
#ordem autorregrassiva: Possivelmente até 10 


#Ajuste do melhor modelo com base no AIC

fitlog = auto.arima(TSlog, max.p = 10, max.q = 14,ic = "aic")

summary(fitlog)
fitlog


#Qualidade do ajuste

hist(fitlog$residuals)

res.fitlog = fitdist(as.numeric(fitlog$residuals), "norm")
plot(res.fitlog)
shapiro.test(fitlog$residuals)

#Visualmente o ajuste e melhor mas ainda temos evidencias de que os residuos nao 
# sao normais, contudo devido ao baixo desvio observado nos graficos
# utilizaremos o modelo mesmo assim

### Predição para todos os meses de 2020 #### 

plot(forecast(fitlog, h = 12))

previsao.2020 = forecast(fitlog, h = 12)
previsao.2020

previsao.2020.df = as.data.frame(previsao.2020)
previsao.2020.df = previsao.2020.df%>%
  mutate_all(exp)

#Corrigindo os nomes das datas
previsao.2020.df$Data=seq.Date(as.Date("2020-01-01"), as.Date("2020-12-01"), "month")


#Unindo os dados na mesma base

dados.prev = bind_rows(dados, previsao.2020.df)

####Visualizando os resultados####

ggplot(dados.prev, aes(x= Data, y = `Total Turistas`))+geom_line(color = "blue4" , size = 0.5)+
  geom_point(size = 1)+
  geom_line(aes(y = `Point Forecast`, fill = "Previsto"), color = "green4", size =0.5, linetype = "dashed")+
  geom_point(aes(y = `Point Forecast`),size = 1)+
  theme_bw()+
  labs(title =  "Total de Turistas - Registrado e Previsto")


# Total previsto nos meses de pandemia 

previsao.2020.df%>%
  filter(Data >= as.Date("2020-03-01"), Data <= as.Date("2020-07-01"))%>%
  summarise(Total = sum(`Point Forecast`), 
            Media = mean(`Point Forecast`))

# 2,25 MM














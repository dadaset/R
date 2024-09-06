series_pol_risk

analysis_data <- merge(BRENT_AGGR, series_pol_risk, by = "year")
colnames(analysis_data)[2] <- "BRENT Prices"

analysis_data<-merge(US_AGGR,analysis_data, by = "year")
colnames(analysis_data)[2] <- "US Gas Prices"

analysis_data<-merge(WTI_AGGR,analysis_data, by = "year")
colnames(analysis_data)[2] <- "WTI Prices"


modelo_libia <- lm(`BRENT Prices` ~ Libya, data = analysis_data)
summary(modelo_libia)
# Regressão Linear para a Nigéria
modelo_nigeria <- lm(`BRENT Prices` ~ Nigeria, data = analysis_data)
summary(modelo_nigeria)

# Regressão Linear para a Venezuela
modelo_venezuela <- lm(`BRENT Prices` ~ Venezuela, data = analysis_data)
summary(modelo_venezuela)


# Gráfico para a Líbia
ggplot(analysis_data, aes(x = Libya, y = `BRENT Prices`)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = TRUE, level = 0.1) +
  labs(title = "Relação entre Índice de Risco Político da Líbia e Preços do Brent",
       x = "Índice de Risco Político (Líbia)",
       y = "Preços do Brent") +
  theme_minimal()

# Gráfico para a Nigéria
ggplot(analysis_data, aes(x = Nigeria, y = `BRENT Prices`)) +
  geom_point() +
  geom_smooth(method = "lm", color = "green", se = T) +
  labs(title = "Relação entre Índice de Risco Político da Nigéria e Preços do Brent",
       x = "Índice de Risco Político (Nigéria)",
       y = "Preços do Brent") +
  theme_minimal()



# Gráfico para a Venezuela
ggplot(analysis_data, aes(x = Venezuela, y = `BRENT Prices`)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relação entre Índice de Risco Político da Venezuela e Preços do Brent",
       x = "Índice de Risco Político (Venezuela)",
       y = "Preços do Brent") +
  theme_minimal()

#------------------ lets try VAR --------------------------


data_var <- analysis_data[, c("BRENT Prices", "Libya", "Nigeria", "Venezuela")]
data_ts <- ts(data_var, start = c(2000), frequency = 1)

modelo_var <- VAR(data_ts, ic = "AIC")

summary(modelo_var)
VARselect(data_ts)
stability(modelo_var)
causality(modelo_var, cause = "Libya")
causality(modelo_var, cause = "BRENT.Prices")
causality(modelo_var, cause = "Venezuela")
causality(modelo_var, cause = "Nigeria")


model_summary_xtable <- summary(modelo_var)
#-------------------- now for US gas prices

data_var <- analysis_data[, c("US Gas Prices", "Libya", "Nigeria", "Venezuela")]
data_ts <- ts(data_var, start = c(2000), frequency = 1)

modelo_var <- VAR(data_ts, ic = "AIC")
summary(modelo_var)
VARselect(data_ts)


tidy_var <- tidy(modelo_var)
print(tidy_var)

#-------------------- now for US gas prices

data_var <- analysis_data[, c("WTI Prices", "Libya", "Nigeria", "Venezuela")]
data_ts <- ts(data_var, start = c(2000), frequency = 1)

modelo_var <- VAR(data_ts, ic = "AIC")
summary(modelo_var)
VARselect(data_ts)


tidy_var <- tidy(modelo_var)
print(tidy_var)


analysis_data <- merge(analysis_data, regulatory_index, by = "year")

colnames(analysis_data)[6] <- "Libya Risk"
colnames(analysis_data)[7] <- "Nigeria Risk"
colnames(analysis_data)[8] <- "Venezuela Risk"
colnames(analysis_data)[9] <- "Libya Institutions"
colnames(analysis_data)[10] <- "Nigeria Institutions"
colnames(analysis_data)[11] <- "Venezuela Institutions"

data_var <- analysis_data[, c("WTI Prices", "Libya Risk", "Nigeria Risk", "Venezuela Risk", 'Libya Institutions',"Nigeria Institutions", "Venezuela Institutions" )]
data_ts <- ts(data_var, start = c(2000), frequency = 1)
modelo_var <- VAR(data_ts, ic = "AIC")
summary(modelo_var)
VARselect(data_ts)








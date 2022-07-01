rm(list=ls())
setwd("C:/Users/dadaset/Desktop/Fundace/Dados")
library(tidyverse)
library(cagedExplorer)
library(snakecase)
library(stringi)

# Reading full dataset --------------------------------------------------------
snis <- readxl::read_excel('Desagregado-20220530111922.xlsx', range = 'A1:IA639')
colnames(snis) <- snakecase::to_snake_case(colnames(snis))
colnames(snis)<-stri_trans_general(str = colnames(snis), 
                                   id = "Latin-ASCII")

#read.csv("haha.csv", encoding = "UTF-8")

# padronizando os nomes das colunas

names(snis)[names(snis) == 'codigo_do_municipio'] = "codigo_municipio"
names(snis)[names(snis) == 'sigla_do_prestador'] = "sigla_prestador"
names(snis)[names(snis) == 'tipo_de_servico'] = "tipo_servico"
names(snis)[names(snis) == "'tipo_servico"] = "tipo_servico"


# Keeping relevant features only ----------------------------------------------
# mapear quais sao essas variaveis no dataframe atual, supostamente só mudou o lugar do underscore


snis2 <- snis %>% 
  select(
    codigo_municipio, prestador, sigla_prestador, tipo_servico,
    natureza_juridica, starts_with('pop_'),
    starts_with('ag_001_'), starts_with('ag_026_'),
    starts_with('es_001_'), starts_with('es_026_'),
    starts_with('fn_017_'),
    # Investimentos prestador de serviços
    starts_with('fn_023'), starts_with('fn_024'),
    starts_with('fn_025'), starts_with('fn_033'),
    # Investimentos município
    starts_with('fn_042'), starts_with('fn_043'),
    starts_with('fn_044'), starts_with('fn_048'),
    # Investimentos estado
    starts_with('fn_052'), starts_with('fn_053'),
    starts_with('fn_054'), starts_with('fn_058'),
    # Índices
    starts_with('in_003'), "in_004_tarifa_media_praticada",
    "in_005_tarifa_media_de_agua", "in_006_tarifa_media_de_esgoto",
    starts_with('in_008'), starts_with('in_012'), starts_with('in_013'),
    starts_with('in_015'), starts_with('in_016'), starts_with('in_022'),
    starts_with('in_024'), starts_with('in_046'), starts_with('in_047'),
    starts_with('in_055'), starts_with('in_056')
  )


# loading Rdarta file - city level

load(file = 'municipios_sp.rda')

# Adding city-level data from an auxiliary dataset ----------------------------
snis3 <- municipios_sp %>% # Dataset from 'cagedExplorer' package
  select(Codmun7, codigo, municipio, municipio_clean,
         regiao_administrativa, regiao_governo) %>% 
  left_join(snis2 %>% 
              mutate(codigo_municipio = as.integer(codigo_municipio)),
            by = c('codigo' = 'codigo_municipio')) 
snis3 %>% select(
  Codmun7, codigo, municipio, municipio_clean, regiao_administrativa,
  regiao_governo, pop_tot_populacao_total_do_municipio_do_ano_de_referencia_fonte_ibge,
  pop_urb_populacao_urbana_do_municipio_do_ano_de_referencia_fonte_ibge,
  prestador, sigla_prestador, tipo_servico, natureza_juridica,
  ag_001_populacao_total_atendida_com_abastecimento_de_agua:in_056_indice_de_atendimento_total_de_esgoto_referido_aos_municipios_atendidos_com_agua
)


# precisa atualizar os dados de população total e população urbana

# New feature: simplified version of 'natureza_jurica' ------------------------
snis4 <- snis3 %>% 
  mutate(natureza_juridica = case_when(
    is.na(natureza_juridica) ~ 'Sem dados',
    str_detect(natureza_juridica,'economia') ~ 
      'Soc. de Econ. Mista com Adm. Pública',
    TRUE ~ natureza_juridica
  )) %>% 
  mutate(nat_jur_simplified = case_when(
    natureza_juridica %in% c('Administração pública direta',
                             'Autarquia',
                             'Empresa pública') ~ 'Administração pública',
    TRUE ~ natureza_juridica
  ) %>% fct_relevel('Soc. de Econ. Mista com Adm. Pública',
                    'Administração pública',
                    'Empresa privada',
                    'Sem dados'))



##----- CODIGO ORIGINAL      ###---- 
pib_muni<-readxl::read_xlsx("pib-municipios-2017.xlsx")

snis5 <- snis4 %>% 
  left_join(readxl::read_excel('pib-municipios-2017.xlsx') %>% 
              select(Codmun7 = codigo, pib2017 = pib) %>% 
              mutate(Codmun7 = as.integer(Codmun7)),
            by = 'Codmun7') %>% 
  left_join(readxl::read_excel('estimativa-populacao-municipios-2017.xlsx') %>%
              select(Codmun7, pop2017) %>% 
              mutate(Codmun7 = as.integer(Codmun7)),
            by = 'Codmun7') %>% 
  mutate(pib_per_capita2017 = pib2017/ pop2017)

#------

# agora adicionar um data frame que tenha a relação pib os codigos municipio
# depois adicionar dataframe com popuçaçãp


# adionando o pop que estava no sheets1

#sheet1 <- readxl::read_excel('Dados Municipios SP.xlsx', sheet = 1)
#pop<-cbind(sheet1[,1],sheet1[,11])
#colnames(pop)[1] <- "Codmun7"
#colnames(pop)[2] <- "pop2020"
##
## criando df com pib ficticio simplesmente multiplicando a coluna de pop por 10
#times_ten<-function(a) {
  a*10
}
#pib2020 <- pop
#pib2020[2]<- lapply(pib2020[2], times_ten)
#colnames(pib2020)[2] <- "pib2020"
#pop$Codmun7<-as.numeric(pop$Codmun7)
#pib2020$Codmun7<-as.numeric(pib2020$Codmun7)
#
#pop$pop2020<-as.numeric(pop$pop2020)
#pib2020$pib2020<-as.numeric(pib2020$pib2020)
#



# Dealing with special cases: cities with two service providers ---------------
# Creating dataframe with only these special cases
snis_2prestadores <- snis3 %>% 
  filter(municipio_clean %in% c('MAUA', 'SALTO', 'SANTA MARIA DA SERRA')) %>%
  group_by(municipio_clean) %>% # Grouping by city
  mutate(
    # Total investment is the sum of values indicated for each provider
    fn_033_investimentos_totais_realizados_pelo_prestador_de_servicos =
      sum(fn_033_investimentos_totais_realizados_pelo_prestador_de_servicos,
          na.rm = TRUE),
    fn_048_investimentos_totais_realizados_pelo_municipio =
      sum(fn_048_investimentos_totais_realizados_pelo_s_municipio_s,
          na.rm = TRUE),
    fn_058_investimentos_totais_realizados_pelo_estado =
      sum(fn_058_investimentos_totais_realizados_pelo_estado,
          na.rm = TRUE),
    # Overall performance indicator as average of the providers' individual scores
    in_012_indicador_de_desempenho_financeiro = 
      mean(in_012_indicador_de_desempenho_financeiro)
  ) %>% 
  ungroup()

#Overall fees (water + sewage) should also be the average of the providers'
# fees, within each city. Let' do that manually to the cities of 'MAUA' and 'SALTO':

snis_2prestadores$in_004_tarifa_media_praticada[snis_2prestadores$municipio_clean == 'MAUA'] <- 
  (snis_2prestadores$in_005_tarifa_media_de_agua[snis_2prestadores$municipio_clean == 'MAUA' 
                                                & snis_2prestadores$tipo_servico == 'Água'] +
     snis_2prestadores$in_006_tarifa_media_de_esgoto[snis_2prestadores$municipio_clean == 'MAUA' 
                                                    & snis_2prestadores$tipo_servico == 'Esgotos']) / 2

snis_2prestadores$in_004_tarifa_media_praticada[snis_2prestadores$municipio_clean == 'SALTO'] <- 
  (snis_2prestadores$in_005_tarifa_media_de_agua[snis_2prestadores$municipio_clean == 'SALTO' 
                                                & snis_2prestadores$tipo_servico == 'Água'] +
     snis_2prestadores$in_006_tarifa_media_de_esgoto[snis_2prestadores$municipio_clean == 'SALTO' 
                                                    & snis_2prestadores$tipo_servico == 'Esgotos']) / 2
#
## For 'SANTA MARIA DA SERRA', we set overall fees equal to water fees, since we have no data on sewage fees.
snis_2prestadores$in_004_tarifa_media_praticada[snis_2prestadores$municipio_clean == 'SANTA MARIA DA SERRA'] <- 
 snis_2prestadores$in_005_tarifa_media_de_agua[snis_2prestadores$municipio_clean == 'SANTA MARIA DA SERRA' 
                                               & snis_2prestadores$tipo_servico == 'Água']

# Replacing lines in the full dataframe with updated values
snis6 <- snis5 %>% 
  filter(!municipio_clean %in% c('MAUA', 'SALTO', 'SANTA MARIA DA SERRA')) %>% 
  bind_rows(snis_2prestadores)

# Per capita investment -------------------------------------------------------
snis_inv <- snis6 %>%
  rename(inv_prest =
           fn_033_investimentos_totais_realizados_pelo_prestador_de_servicos,
         inv_est = fn_058_investimentos_totais_realizados_pelo_estado,
         inv_mun = fn_048_investimentos_totais_realizados_pelo_municipio) %>%
  mutate(na_inv_prest = is.na(inv_prest),
         na_inv_est = is.na(inv_est),
         na_inv_mun = is.na(inv_mun)) %>%
  mutate_at(.vars = vars(inv_prest, inv_est, inv_mun),
            .funs = function(x) ifelse(is.na(x), 0, x)) %>%
  mutate(inv_total =
           ifelse(na_inv_prest & na_inv_est & na_inv_mun,
                  NA_real_,
                  inv_prest + inv_est + inv_mun)) %>%
  mutate(inv_per_capita = inv_total / pop_tot_populacao_total_do_municipio_do_ano_de_referencia_fonte_ibge)

# Adding per capita investment feature
snis7 <- snis6 %>%
  left_join(snis_inv %>% select(municipio_clean, tipo_servico, inv_per_capita),
            by = c('municipio_clean', 'tipo_servico'))

# Saving ----------------------------------------------------------------------
saveRDS(snis7, 'snis-cleanhouse.rds')

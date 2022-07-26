library(tmap)
setwd("C:/Users/dadaset/Desktop/Fundace/Saneamento/Dados")

#### ----ATENÇÃO!: para carregar o polygons o melhor jeito foi clicar no rda e abrir com R---####


#polygons_municipios_sp<-load(file='Polygons/polygons_municipios_sp.rda')

# definindo função

add_geometry_municipios <- function(df) {
  polygons_municipios_sp %>%
    select(-NM_MUNICIP) %>%
    right_join(df,
               by = 'Codmun7')
}

ranking2<-readxl::read_xlsx('mapas.xlsx',sheet = 2)
ranking<-ranking[order(ranking$Município), ]

#deixar o ranking com o nome padronizado
library(stringi)
ranking2$municipio_clean<-stri_trans_general(str = ranking2$Município, 
                                    id = "Latin-ASCII")
ranking2$municipio_clean<-toupper(ranking2$municipio_clean)

ranking2[1]<- NULL


# fazer uma junção 

join<-left_join(snis_agua, ranking2, by = "municipio_clean") #esse aqui ta com 648 linhas, esperaria 645, talvez essas 3 sejam duplicatas (era mesmo)


n_occur <- data.frame(table(join$municipio_clean))
n_occur[n_occur$Freq > 1,]

library(dplyr)

join<-join[-c(648, 646, 644),]

# mapa
# ele nao deixou mexer no nome impresso da variável, apesar de ter podido alterar a variavel que entra

mapa_atendimento <- join %>% 
  add_geometry_municipios() %>% 
  rename(`Índice de atendimento` =
          RANKING) %>% 
  tm_shape() +
  tm_style(
    "beaver",
    legend.format = list(fun = function(x) str_c(round(x), '%'),
                         text.separator = " a ")
  ) +
  tm_fill(
    'Índice de atendimento',
    palette = 'Blues',
    style = 'quantile',
    n = 6,
    alpha = 1,
    id = "municipio_clean",
    textNA = "Sem dados",
    colorNA = '#fff7bc'
  ) +
  tm_layout(main.title = 
              'Ranking com média móvel de investimento') +
  custom_map_settings ; mapa_atendimento


tmap_save(mapa_atendimento, height = 6, width = 6,
          filename = 'mapa-ranking-media-investimento.png')



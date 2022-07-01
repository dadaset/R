indices<-readxl::read_xlsx("Dados Municipios SP 2 (1).xlsx", sheet = 8)

#criar a coluna municipios clean

library(stringi)
municipio_clean<-stri_trans_general(str = indices$Município, 
                                   id = "Latin-ASCII")
municipio_clean<-toupper(municipio_clean)
indices$municipio_clean <- municipio_clean

indices$Codmun7<-snis$Codmun7

#jogar o indice do municipio na base do snis
snis_agua
indices

snis_agua2<-inner_join(snis_agua, indices, by = "municipio_clean")

mapa_atendimento_agua <- snis_agua2 %>% 
  add_geometry_municipios() %>% 
  rename(`Ranking` =
           RANKING) %>% 
  tm_shape() +
  tm_style(
    "beaver",
    legend.format = list(fun = function(x) str_c(round(x)),
                         text.separator = " a ")
  ) +
  tm_fill(
    'Ranking',
    palette = 'Blues',
    style = 'quantile',
    n = 6,
    alpha = 1,
    id = "municipio_clean",
    textNA = "Sem dados",
    colorNA = '#fff7bc'
  ) +
  tm_layout(main.title = 
              'Índice do Ranking',
            attr.outside = TRUE) +
  custom_map_settings ; mapa_atendimento_agua

# Saving
tmap_save(mapa_atendimento_agua, height = 6, width = 6,
          filename = 'mapa-indice.png')







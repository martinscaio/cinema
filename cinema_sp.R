# Biblioteca ----------------------------------------------------------------------------------------------------
library(tidyverse)
library(data.table)
library(tmaptools)
library(geobr)
library(tmap)
library(mapview)
library(sf)
library(tidygeocoder)
library(ggmap)
library(leaflet)

# Dados ----------------------------------------------------------------------------------------------------



eliminar_colunas <- c("PAGINA_ELETRONICA_COMPLEXO", "COMPLEMENTO_ENDERECO", "COMPLEXO_ITINERANTE", "OPERACAO_USUAL", 
                      "EXIBIDOR", "REGISTRO_EXIBIDOR", "CNPJ_EXIBIDOR", "GRUPO_EXIBIDOR")



base_dados <- fread("C:\\Users\\mcaio\\Desktop\\Nova pasta\\Salas_cinemas.csv",
                    encoding = "Latin-1",
                    drop = eliminar_colunas)




# Limpeza dados ----------------------------------------------------------------------------------------------------


base_sp <- base_dados %>% 
  filter(MUNICIPIO_COMPLEXO == "SÃO PAULO" & SITUACAO_EXIBIDOR == "REGULAR") %>%
  unite(col = "endereco", c(ENDERECO_COMPLEXO, NUMERO_ENDERECO_COMPLEXO, BAIRRO_COMPLEXO), sep = " - " )
  




cinema_sp <- base_sp %>% mutate(endereco = stringr::str_remove_all(endereco, ","),
                                  endereco = abjutils::rm_accent(endereco),
                                  endereco = stringr::str_to_upper(endereco),
                                  endereco = stringr::str_replace_all(endereco, "^AV.", "AVENIDA "),
                                  endereco = stringr:: str_replace_all(endereco, "ROD", "RODOVIA "),
                                  endereco = stringr::str_replace_all(endereco, "AVENIDANIDA", "AVENIDA "),
                                  endereco = stringr::str_replace_all(endereco, "AVENIDA NIDA", "AVENIDA "))
             





# ajustes manuais em alguns endereços
endereco_cinema <- cinema_sp %>% 
  select(REGISTRO_COMPLEXO, endereco) %>% 
  distinct(REGISTRO_COMPLEXO, .keep_all = TRUE) %>% 
  mutate(endereco = dplyr::case_when(endereco == "RUA FREI CANECA - 569 - CERQUEIRA CESAR" ~ "RUA FREI CANECA - 569",
                                     endereco == "AVENIDA  ANTARTICA - 408 - AGUA BRANCA" ~ "AVENIDA FRANCISCO MATARAZZO - 1350",
                                     endereco == "AVENIDA  RAIMUNDO PEREIRA MAGALHAES - 1465 - JARDIM IRIS" ~ "AV. RAIMUNDO PEREIRA DE MAGALHAES, 1465 - PIQUERI, São Paulo - SP",
                                     endereco == "AVENIDA  RAIMUNDO PEREIRA DE MAGALHAES - 11001 - JARDIM PIRITUBA" ~ "AV. RAIMUNDO PEREIRA DE MAGALHAES, 11001 - PARADA DE TAIPAS, São Paulo - SP",
                                     endereco == "RUA DOMINGOS DE MORAES - 2564 - VILA MARIANA" ~ "RUA DOMINGOS DE MORAIS - 2564 - VILA MARIANA",
                                     endereco == "AVENIDA  CRUZEIRO DO SUL - 1100 - PONTE PEQUENA" ~ "AVENIDA  CRUZEIRO DO SUL - 1100 - CANINDE",
                                     endereco == "AVENIDA  DR. CHUCRI ZAIDAN - 902 - VILA CORDEIRO" ~ "AVENIDA  DR. CHUCRI ZAIDAN - 902",
                                     endereco == "AVENIDA  DR. FRANCISCO MESQUITA - 1000 - IPIRANGA." ~ "AVENIDA DOUTOR FRANCISCO MESQUITA - 1000",
                                     endereco == "AVENIDA  INTERLAGOS - 2255 - VILA INGLESA" ~ "AVENIDA  INTERLAGOS - 2255",
                                     endereco == "AVENIDA  DAS NACOES UNIDAS - 4777 - LAPA" ~ "AVENIDA  DAS NACOES UNIDAS - 4777 - VILA OLIMPIA",
                                     endereco == "AVENIDA  PRESIDENTE JUSCELINO KUBITCHECK - 2041 - VILA NOVA CONCEICAO" ~ "AVENIDA  PRESIDENTE JUSCELINO KUBITSCHECK - 2041",
                                     endereco == "AVENIDA  JOSE PINHEIRO BORGES S/N - 0 - VILA CAMPANELA" ~ "AVENIDA  JOSE PINHEIRO BORGES  - 354 - ITAQUERA",
                                     endereco == "AVENIDA  DRª RUTH CARDOSO - 4777 - VILA HAMBURGUESA" ~ "AVENIDA DOUTORA RUTH CARDOSO - 4777",
                                     endereco == "RUA ENGENHEIRO HEITOR ANTONIO EIRAS GARCIA - 1700 - JARDIM ESMERALDA" ~ "RUA ENGENHEIRO HEITOR ANTONIO EIRAS GARCIA - 1870",
                                     endereco == "RUA ENGENHEIRO ARMANDO DE ARRUDA PEREIRA - 5241 - JABAQUARA" ~ "RUA ENGENHEIRO ARMANDO DE ARRUDA PEREIRA - 5241",
                                     endereco == "RUA FEITICO DA VILA - 399 - CHACARA SANTA CLARA - CAPAO REDONDO" ~ "RUA FEITICO DA VILA - 399 - CAPAO REDONDO",
                                     endereco == "AVENIDA  JOSE PINHEIRO - 60 - GUAIANAZES" ~ "AVENIDA  JOSE PINHEIRO BORGES - 60",
                                     endereco == "RUA BERNARDO JOSE DE LORENA - 0 - PIRITUBA" ~ "RUA BERNARDO JOSE DE LORENA - PERUS",
                                     endereco == "RUA DOM JOSE DE BARROS - 306 - CENTRO" ~ "RUA DOM JOSE DE BARROS - 306 - REPUBLICA",
                                     endereco == "ESTRADA DE CAMPO LIMPO - 459 - VILA PREL" ~ "ESTRADA DO CAMPO LIMPO - 459",
                                     endereco == "RUA REGENTE FEIJO - 1759 - TATUAPE" ~ "AVENIDA REGENTE FEIJO - 1759",
                                     endereco == "AVENIDA  GIOVANNI GRONCHI - 5819 - JD. GUEDALA" ~ "AVENIDA  GIOVANNI GRONCHI - 5819 - VILA ANDRADE",
                                     endereco == "RUA FRANCISCA ESPOSITO TONETTI - 105 - JARDIM GUAPIRA" ~ "RUA COSTA BRITO - 46", 
                                     endereco == "AVENIDA LUIZ IMPARATO - 564 - PARQUE CISPER" ~ "AVENIDA  LUIZ IMPARATO - 564",
                                     endereco == "RUA MARIA ANTONIA - 294 - VILA BUARQUE" ~ "RUA MARIA ANTONIA - 294",
                                     endereco == "AVENIDA  DR. ANTONIO MARIA LAET - 566 - PARADA INGLESA" ~ "AVENIDA  DOUTOR ANTÔNIO MARIA LAET - 566 - TUCURUVI",
                                     endereco == "AVENIDA  DAS NACOES UNIDAS - 14401 - VILA GERTRUDES" ~ "AVENIDA  DAS NACOES UNIDAS - 14401" ,
                                     endereco == "PRACA FRANKLIN ROOSEVELT - 172 - CONSOLACAO" ~ "PRACA FRANKLIN ROOSEVELT - 172",
                                     endereco == "AVENIDA  LUIZ IMPARATO - 564 - PARQUE CISPER" ~ "AVENIDA  LUIS IMPARATO - 564",
                                     endereco == "AVENIDA SAO JOAO - 473 - CENTRO" ~ "AVENIDA  SAO JOAO - 473 - SAO PAULO",
                                     endereco == "RUA TREZE DE MAIO - 1947 - BELA VISTA" ~ "RUA 13 DE MAIO - 1947 - BELA VISTA - SAO PAULO",
                                     endereco == "AVENIDA  PRESIDENTE JUSCELINO KUBITSCHECK - 2041" ~ "Av. Pres. Juscelino Kubitschek, 2041 - Itaim Bibi",
                                     endereco == "RUA FREI CANECA - 569" ~ "RUA FREI CANECA - 569- BELA VISTA", 
                                     endereco == "RUA GONCALVES CRESPO S/N. - 420 - TATUAPE" ~ "RUA GONÇALVES CRESPO - 420 - TATUAPE",
                                     endereco == "AVENIDA  INACIO MONTEIRO - 6900 - CIDADE TIRADENTES" ~ "RUA INACIO MONTEIRO - 6900 - CIDADE TIRADENTES",
                                     endereco == "AVENIDA  ARICANDUVA - 5555 - VILA MATILDE" ~ "AVENIDA ARICANDUVA, 5555 -  JARDIM SANTA TEREZINHA",
                                     endereco == "RUA CONSELHEIRO MOREIRA DE BARROS - 2780 - LAUZANE PAULISTA" ~ "RUA CONSELHEIRO MOREIRA DE BARROS, 2580 - Lauzane Paulista, São Paulo - SP, 02430-001",
                                     endereco == "AVENIDA  HIGIENOPOLIS - 646 - HIGIENOPOLIS" ~ "Avenida Higienópolis - 618 - Higienópolis",
                                     endereco == "AVENIDA   PAULISTA - 2424 - BELA VISTA" ~ "Avenida Paulista, 2424 - Consolação, São Paulo - SP, 01310-300",
                                     endereco == "RUA PAMPLONA - 1704 - JARDIM PAULISTA" ~ "R. Pamplona, 1704 - Jardim Paulista, São Paulo - SP, 01405-002",
                                     endereco == "RUA JOAQUIM FLORIANO - 462 - ITAIM BIBI" ~ "Rua Joaquim Floriano 462 - Vila Olímpia - Itaim Bibi",
                                     endereco == "RODOVIA  RAPOSO TAVARES - 1002 - JARDIM BOA VISTA" ~ "Rua Sebastião Gonçalves - Jardim Boa Vista",
                                     endereco == "AVENIDA  MAGALHAES DE CASTRO - 12000 - MORUMBI" ~ "Rua Joapé - Cidade Jardim, São Paulo - SP",TRUE ~ endereco))



endereco_cinema<- endereco_cinema %>% mutate(endereco = case_when(endereco == "AVENIDA  PRESIDENTE JUSCELINO KUBITSCHECK - 2041" ~ "Rua são marun, Vila Olímpia, Itaim Bibi, São Paulo",
                                                                  endereco == "AVENIDA  SAO JOAO - 473 - CENTRO" ~ "AVENIDA  SAO JOAO - 473 - SAO PAULO",
                                                                  endereco == "RUA TREZE DE MAIO - 1947 - PARAISO" ~ "RUA 13 DE MAIO - 1947 - BELA VISTA - SAO PAULO",
                                                                  endereco == "RUA FREI CANECA - 569" ~ "RUA FREI CANECA - 569- BELA VISTA",
                                                                  endereco == "RUA ENGENHEIRO ARMANDO DE ARRUDA PEREIRA - 5241" ~ "AVENIDA ENGENHEIRO ARMANDO DE ARRUDA PEREIRA - 5241, 5241 - Vila do Encontro, São Paulo",
                                                                  endereco == "RUA COSTA BRITO - 46" ~ "Rua Mário Lago, Jardim Guapira",TRUE ~ endereco))




# Pegando as Latitudes e Longitudes----------------------------------------------------------------------------------------------------




lugares <- geocode_OSM(endereco_cinema$endereco, projection = 4326, as.sf = T)





# mapas interativos----------------------------------------------------------------------------------------------------



lugares %>% mapview()



leaflet(lugares) %>% addTiles() %>% addMarkers() 




# Arquivo shape com os distritos de sp----------------------------------------------------------------------------------------------------



distrito_sp <- st_read("C:\\Users\\mcaio\\Desktop\\cinema\\LAYER_DISTRITO\\DEINFO_DISTRITO.shp")




# mapa dos distritos de sp com a localização dos cinemas



ggplot(distrito_sp) + 
  geom_sf() + 
  stat_sf_coordinates(data = lugares, size = 2.4, colour = "Blue")+
  ylab(NULL)+
  xlab(NULL)+
  ggtitle("Salas de Exibição e Complexos em SP")+
  theme_bw()




# Modo diferente com outras funcoes


qtm(mun_sp) +
  tm_legend(show = FALSE)

tm_shape(distrito_sp) +
  tm_fill() +
  tm_shape(lugares) +
  tm_bubbles(col = "red", size = 0.15) 




# tabela


tabela <- cinema_sp %>% 
  select(NOME_SALA, SITUACAO_SALA, NOME_COMPLEXO, endereco, REGISTRO_COMPLEXO) %>% 
  distinct(REGISTRO_COMPLEXO, .keep_all = TRUE) %>% 
  kableExtra::kbl %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))




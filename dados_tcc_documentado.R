#carregando bibliotecas necessárias para executar os códigos
library(RSQLite)
library(tidyverse)
library(zoo)
library(readxl)
library(abjutils)
library(stringr)
library(ca)
library(factoextra)
library(FactoMineR)
library(ggrepel)

#conectando ao banco de dados da plataforma JF Salvando Todos
conn <- dbConnect(SQLite(), "~/Lucas/UFJF/TCC/jfsalvandotodos.db")

#banco de dados com dados de confirmados e mortes para cada municipio
data_municipios <- tbl(conn, "data_municipios")

#banco de dados com informacoes de codigo e nome de cada municipio
municipios <- tbl(conn, "municipios")

#banco de dados com informacoes de codigo e nome de cada microrregioes
microrregioes <- tbl(conn, "microrregioes")

#banco de dados com informacoes de codigo e nome de cada mesorregioes
mesorregioes <- tbl(conn, "mesorregioes")

#banco de dados com informacoes de codigo e nome de cada uf
ufs <- tbl(conn, "ufs")

#transformando os banco de dados com formato tbl para o formato dataframe
data_municipios_df <- data.frame(data_municipios)

municipios_df <- data.frame(municipios)

microrregioes_df <- data.frame(microrregioes)

mesorregioes_df <- data.frame(mesorregioes)

ufs_df <- data.frame(ufs)

#selecionando as variáveis que serão utilizadas para as análises e para mesclar com outros dados
municipiosAux <- municipios_df %>% select(population, id, name, id_micro, id_meso, id_uf)

microrregioesAux <- microrregioes_df %>% select(name, id)

mesorregioesAux <- mesorregioes_df %>% select(name, id)

ufsAux <- ufs_df %>% select(name, id)

#realizando a mesclagem dos dados dos municípios com as microrregiões que eles pertencem
dados_municip_micro <- right_join(municipiosAux, microrregioesAux, by = c("id_micro" = "id"))

#realizando a mesclagem dos dados dos municípios com as mesorregiões que eles pertencem
dados_municip_micro_meso <- right_join(dados_municip_micro, mesorregioesAux, by = c("id_meso" = "id"))

#realizando a mesclagem dos dados dos municípios com as regiões que eles pertencem
dados_municip_micro_meso_uf <- right_join(dados_municip_micro_meso, ufsAux, by = c("id_uf" = "id"))

#selecionado apenas as colunas que iremos utilizar adiante
dados_names <- dados_municip_micro_meso_uf %>% select(1,2,3,7,8,9)

#passando os dados para o formato de dataframe
dados_names_df <- data.frame(dados_names)

#renomeando as colunas do dataframe
colnames(dados_names_df) <- c("pop", "id", "city", "micro", "meso", "uf")

#vetor auxiliar com as siglas de cada estado
estados <- c("AC","AL", "AP", "AM","BA", "CE", "DF", "ES", "GO",
             "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI",
             "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")

#vetor auxiliar com os nomes por extenso de cada estado
aux <- unique(dados_names_df$uf)[order(unique(dados_names_df$uf))]

#trocando o nome por extenso por siglas, tratando alguns casos especiais na sequencia
str_replace_all(dados_names_df$uf, setNames(estados, aux)) %>% 
  str_replace_all(., setNames("PB", "PAiba")) %>% 
  str_replace_all(., setNames("PR", "PAna")) -> vetEstados

#transformando todas as letras em maiusculas para facilitar o merge adiante
cidadeEstado <- paste(toupper(dados_names_df$city), " (", vetEstados, ")", sep = "")

#mudando a variavel estado para o novo formato que inclui o estado entre parenteses. Retirando tambem a variavel uf
dados_names_df %>% 
  mutate(cidades = city) %>% 
  mutate(city = cidadeEstado) %>% 
  select(-uf) -> dados_names_df

#retirando a acentuação e colocando todas as letras em maiúsculo, para padronizar as variáveis
dados_names_df$micro <- rm_accent(dados_names_df$micro)
dados_names_df$micro <- toupper(dados_names_df$micro)

dados_names_df$meso <- rm_accent(dados_names_df$meso)
dados_names_df$meso <- toupper(dados_names_df$meso)

dados_names_df$city <- rm_accent(dados_names_df$city)
dados_names_df$city <- toupper(dados_names_df$city)

dados_names_df$cidades <- rm_accent(dados_names_df$cidades)
dados_names_df$cidades <- toupper(dados_names_df$cidades)

#criando a variável de distância seguindo alguns critérios geogáficos
dados_names_df$distance <- "OUTROS"

dados_names_df$distance[dados_names_df$micro == "ENTORNO DE BRASILIA"] <- "ENTORNO DE BRASILIA" 

dados_names_df$distance[dados_names_df$meso == "ZONA DA MATA"] <- "ZONA DA MATA"

dados_names_df$distance[dados_names_df$city == "JUIZ DE FORA (MG)"] <- "JUIZ DE FORA"

dados_names_df$distance[dados_names_df$city == "GOVERNADOR VALADARES (MG)"] <- "GOVERNADOR VALADARES"

dados_names_distance_df <- dados_names_df %>% select(c(pop, id, city, distance, cidades))

#selecionando algumas variáveis e em sequência unindo dois banco de dados
data_municipios_df %>% select(c(date, id, confirmed, deaths)) %>% 
  right_join(., dados_names_distance_df, by = "id") -> dados_confirmados

#formatando a variável de data 
dados_confirmados$mesAno <- format(as.Date(dados_confirmados$date, "%Y-%m-%d"), "%Y/%m")

#retirando meses que não faram parte da análise
dados_confirmados %>% 
  filter(!mesAno %in% c("2020/02", "2020/03", "2020/04", "2020/05", "2022/06")) -> dados_confirmados

#calculando o valor mensal de algumas variáveis, pois estavam expresso de forma acumulada 
dados_confirmados %>% group_by(city, mesAno) %>%
  slice_tail() %>%
  group_by(city) %>% 
  mutate(diff_prev = confirmed - lag(confirmed, 1),
         confirmados = ifelse(is.na(diff_prev), 0, diff_prev),
         diff_prev = deaths - lag(deaths, 1),
         mortes = ifelse(is.na(diff_prev), 0, diff_prev)) %>% 
  select(pop, city, distance, mesAno, confirmados, mortes, cidades) -> dados_aux

#tratando alguns casos especiais
dados_aux$confirmados[dados_aux$confirmados < 0] = 0
dados_aux$mortes[dados_aux$mortes < 0] = 0

#carregando os dados de acessos a Plataforma JF Salvando Todos
dados_acessos <- read.table("~/Lucas/UFJF/TCC/TCC2022/Analytics.csv", sep = ",", dec = ".", header = T, comment.char="#")

#alterando o formato da data para que fique o ano e o mes apenas
dados_acessos$`Mês do ano` <- format(as.Date(paste(dados_acessos$Mês.do.ano,1,sep="-"),"%Y%m-%d"), "%Y/%m")

#retirando o acento e colocando os nomes todo em maiúsculo
dados_acessos$Cidade <- rm_accent(dados_acessos$Cidade)
dados_acessos$Cidade <- toupper(dados_acessos$Cidade)

#criando um vetor que armazena as cidades que obtiveram dados de acessos
cidades_acessos <- unique(dados_acessos$Cidade)


# cidades_acessos <- rm_accent(cidades_acessos)
# cidades_acessos <- toupper(cidades_acessos)


#fitlrando cidades que tiveram acesso a Plataforma no data frame de casos e mortes
#Obs. O Google Analytics apresenta somente o nome da cidade. Foi realizada uma busca manual para identificar o estado 
#Alguns nomes de cidade existem em mais de um estado, então esses casos foram tratados manualmente

cidadesDuplicadas <- c("SANTA LUZIA (MA)", "SANTA LUZIA (PB)", "SANTA LUZIA (BA)", "BELEM (AL)", "BELEM (PB)", "OURO BRANCO (AL)",
                       "OURO BRANCO (RN)", "SANTA CRUZ (PE)", "SANTA CRUZ (PB)", "AMPARO (PB)", "BOA VISTA (PB)", "CAMPO GRANDE (AL)",
                       "CASCAVEL (CE)", "GOIANA (MG)", "IGUATU (PR)", "LAGOA SANTA (GO)", "PALMAS (PR)", "PARNAMIRIM (PE)",
                       "PRAIA GRANDE (SC)", "RIO BRANCO (MT)", "SANTA RITA (PB)", "SANTO ANDRE (PB)", "SAO CARLOS (SC)",
                       "SAO SEBASTIAO (AL)", "SAO VICENTE (RN)", "NOVA SANTA RITA (PI)")

dados_confirmados_mortes_distance_pop %>% filter(!city %in% cidadesDuplicadas) -> dados_confirmados_mortes_distance_pop

dados_confirmados_mortes_distance_pop$cidades %in% cidades_acessos -> casosMortesCidades

dados_confirmados_mortes_distance_pop[casosMortesCidades,] -> dados_confirmados_mortes_distance_pop_acess


#trocando o nome das variaveis
colnames(dados_acessos) <- c("cidades", "Mês.do.ano", "usuarios", "novos.usuários", "sessoes", "mesAno")

#selecionando algumas variaveis e criando um novo dataframe
dados_acessos %>% 
  select(cidades, sessoes, mesAno) -> dados_acessos_final

#unindo dois banco de dados diferentes
dados_confirmados_mortes_distance_pop_acess <- merge(dados_confirmados_mortes_distance_pop_acess, dados_acessos_final, by = c("cidades", "mesAno"), all.x = T)

#substituindo os valores NA por zero
dados_confirmados_mortes_distance_pop_acess[is.na(dados_confirmados_mortes_distance_pop_acess)] <- 0

#carregando os dados do inventario
inventario <- read.csv("~/Lucas/UFJF/TCC/inventario.csv")

#selecionando apenas as colunas de interesse
inventario %>% select(Data, Suporte.Midiático) -> inventario

#modificando a data para se adequar ao padrao de ano e mes
inventario$date <- format(as.Date(inventario$Data, "%d/%m/%Y"), "%Y/%m")

#calculando o total de noticias para cada mes
inventario %>% group_by(date, Suporte.Midiático) %>% 
  summarise(noticias = n()) %>%
  group_by(date) %>% 
  summarise(noticias = sum(noticias)) -> noticias

#retirando meses que não faram parte da análise
noticias %>% 
  filter(!date %in% c("2020/02", "2020/03", "2020/04", "2020/05", "2022/06")) -> noticias

#unindo dois banco de dados diferentes
dados_confirmados_mortes_distance_pop_acess_news <- merge(dados_confirmados_mortes_distance_pop_acess, noticias, by.x = "mesAno", by.y = "date", all = T)

#tratando um caso especifico
dados_confirmados_mortes_distance_pop_acess_news$city[dados_confirmados_mortes_distance_pop_acess_news$city == "CAMPO GRANDE (MT DO SUL)"] <- "CAMPO GRANDE (MS)"

#obtendo o vetor com as cidades presente no banco de dados
cidades_acessos <- unique(dados_confirmados_mortes_distance_pop_acess_news$city)

#Iniciando a extração dos dados vacinais
#vetor auxiliar com as iniciais de cada estado
estados <- c("AC","AL","AP","AM","BA", "CE", "DF", "ES", "GO",
             "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI",
             "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")

#funcao iterativa para tratar os dados vacinais 
for(estado in estados){
  #carregando os dados do estado
  vacina <- read.csv(paste("~/Lucas/UFJF/TCC/vacina/processed_", estado, ".csv", sep = ""))
  
  #transformando a data para ano e mes, apenas
  vacina$date <- format(as.Date(vacina$date,"%Y-%m-%d"), "%Y/%m")
  
  #tratando os nomes das cidades
  vacina$city <- gsub('.{3}$', '', vacina$city)
  vacina$city <- rm_accent(vacina$city)
  
  #transformando todas as letras em maiusculas para facilitar o merge adiante
  vacina$city <- paste(toupper(vacina$city), " (", estado, ")", sep = "")
  
  #agregando os dados vacinais para cidade, mes do ano e dose utilizada
  vacina %>% 
    group_by(date, city, dose) %>% 
    summarise(vacina = sum(count)) -> vacina
  
  #criando um data frame apenas com os dados para primeira dose
  vacina %>% filter(dose == 1) %>% 
    na.omit() -> primeiraDose
  
  #criando um data frame apenas com os dados para segunda dose e dose unica (completamente vacinados)
  vacina %>% filter(dose == "0" | dose == "2") %>% 
    group_by(date, city) %>% 
    summarise(vacina = sum(vacina)) %>% 
    na.omit()  -> segundaDose
  
  #criando um data frame apenas com os dados para doses de reforco
  vacina %>% filter(dose > 2) %>% 
    group_by(date, city) %>% 
    summarise(vacina = sum(vacina)) %>% 
    na.omit() -> doseReforco
  
  #unindo os data frames de cada variavel acima
  merge(primeiraDose, segundaDose, by = c("date", "city"), all = T, suffixes = c(1,2)) %>% 
    merge(., doseReforco, by = c("date", "city"), all = T, suffixes = c(2,3)) %>% 
    select(-dose)-> vacinaCompleto
  
  #substituindo NA por 0
  vacinaCompleto[is.na(vacinaCompleto)] <- 0
  
  #alterando os nomes das colunas para padronizar o data frame
  colnames(vacinaCompleto) <- c("date", "city", "primeiraDose", "segundaDose", "doseReforco")
  
  #salvando o data frame com os dados de cada dose para cada cidade em cada mes
  vacinaCompleto %>% filter(city %in% cidades_acessos) %>% 
    write.table(., paste("~/Lucas/UFJF/TCC/vacina/processed_", estado, ".txt", sep = ""))
}

#criando um data frame que contem todas as cidades, de cada estado, que acessaram a Plataforma
vacinaCidadesAcessos <- data.frame()
for(estado in estados){
  aux <- read.table(paste("~/Lucas/UFJF/TCC/vacina/processed_", estado, ".txt", sep = ""))
  if(nrow(aux) > 1){
    vacinaCidadesAcessos <- bind_rows(vacinaCidadesAcessos, aux)
  }
}

#unindo dois bancos de dados diferentes
dados_confirmados_mortes_distance_pop_acess_news_vac <- merge(dados_confirmados_mortes_distance_pop_acess_news, vacinaCidadesAcessos,
                                                              by.x = c("city", "mesAno"), by.y = c("city", "date"), all.x = T)

#atribuindo valor zero aos locais onde havia o valor NA
dados_confirmados_mortes_distance_pop_acess_news_vac[is.na(dados_confirmados_mortes_distance_pop_acess_news_vac)] <- 0

#lendo os dados de idhm
IDHM2010 <- read_xlsx("~/Lucas/UFJF/TCC/idhm.xlsx")

#selecionando somente as colunas que nos interessa
IDHM2010 <- IDHM2010 %>% select(Territorialidade, IDHM)

#tratando os assentos presentes
IDHM2010$Territorialidade <- rm_accent(IDHM2010$Territorialidade)

#transformando todas as letras em maiusculas para facilitar o merge adiante
IDHM2010$Territorialidade <- toupper(IDHM2010$Territorialidade)

#mudando o nome das colunas para facilitar o merge
colnames(IDHM2010) <- c("city", "IDHM2010")

#realizando o merge, agregando assim o idhm dos municipios
dados_confirmados_mortes_distance_pop_acess_news_vac_idh<- merge(dados_confirmados_mortes_distance_pop_acess_news_vac, IDHM2010, by = "city", all.x = T)

#separando as datas em trimestres 
dados_confirmados_mortes_distance_pop_acess_news_vac_idh$trim[dados_confirmados_mortes_distance_pop_acess_news_vac_idh$mesAno %in% c("2020/06", "2020/07", "2020/08")] <- "1º"

dados_confirmados_mortes_distance_pop_acess_news_vac_idh$trim[dados_confirmados_mortes_distance_pop_acess_news_vac_idh$mesAno %in% c("2020/09", "2020/10", "2020/11")] <- "2º"

dados_confirmados_mortes_distance_pop_acess_news_vac_idh$trim[dados_confirmados_mortes_distance_pop_acess_news_vac_idh$mesAno %in% c("2020/12", "2021/01", "2021/02")] <- "3º"

dados_confirmados_mortes_distance_pop_acess_news_vac_idh$trim[dados_confirmados_mortes_distance_pop_acess_news_vac_idh$mesAno %in% c("2021/03", "2021/04", "2021/05")] <- "4º"

dados_confirmados_mortes_distance_pop_acess_news_vac_idh$trim[dados_confirmados_mortes_distance_pop_acess_news_vac_idh$mesAno %in% c("2021/06", "2021/07", "2021/08")] <- "5º"

dados_confirmados_mortes_distance_pop_acess_news_vac_idh$trim[dados_confirmados_mortes_distance_pop_acess_news_vac_idh$mesAno %in% c("2021/09", "2021/10", "2021/11")] <- "6º"

dados_confirmados_mortes_distance_pop_acess_news_vac_idh$trim[dados_confirmados_mortes_distance_pop_acess_news_vac_idh$mesAno %in% c("2021/12", "2022/01", "2022/02")] <- "7º"

dados_confirmados_mortes_distance_pop_acess_news_vac_idh$trim[dados_confirmados_mortes_distance_pop_acess_news_vac_idh$mesAno %in% c("2022/03", "2022/04", "2022/05")] <- "8º"

#realizando a agregação dos dados por trimestre e/ou localidade, algumas variáveis são somadas, outras são realizadas as médias
dados_confirmados_mortes_distance_pop_acess_news_vac_idh %>% 
  select(pop, distance, city) %>% 
  distinct(city, .keep_all = T)-> population

dados_confirmados_mortes_distance_pop_acess_news_vac_idh %>% 
  group_by(distance, trim) %>% 
  mutate(confirmados = sum(confirmados),
         mortes = sum(mortes),
         primeiraDose = sum(primeiraDose),
         segundaDose = sum(segundaDose),
         doseReforco = sum(doseReforco),
         acessos = sum(sessoes),
         idhm2010 = mean(IDHM2010)) %>% 
  select(-c(mesAno, city, pop, IDHM2010, sessoes)) %>% 
  distinct(trim, distance, .keep_all = T) -> dados_trim_confirmados_mortes_distance_pop_acess_news_vac_idh

population %>% 
  group_by(distance) %>% 
  mutate(pop = sum(pop))%>%
  distinct(distance, .keep_all = T) %>% 
  select(-city) -> pop_dist

#unindo dois banco de dados diferentes
dados_trim_pop_dist <- merge(dados_trim_confirmados_mortes_distance_pop_acess_news_vac_idh, pop_dist, by = "distance", all.x = T)

#retirando a variavel cidade, pois estamos utilizando a localidade
dados_trim_pop_dist %>% 
  select(-cidades) -> dados_trim_pop_dist

#realizando a padronização de algumas variáveis para a escala de valor/100 mil habitantes
dados_trim_pop_dist$auxPop <- dados_trim_pop_dist$pop/100000

dados_trim_pop_dist$confirmados <- dados_trim_pop_dist$confirmados / dados_trim_pop_dist$auxPop
dados_trim_pop_dist$mortes <- dados_trim_pop_dist$mortes / dados_trim_pop_dist$auxPop
dados_trim_pop_dist$primeiraDose <- dados_trim_pop_dist$primeiraDose / dados_trim_pop_dist$auxPop
dados_trim_pop_dist$segundaDose <- dados_trim_pop_dist$segundaDose / dados_trim_pop_dist$auxPop
dados_trim_pop_dist$doseReforco <- dados_trim_pop_dist$doseReforco / dados_trim_pop_dist$auxPop

#retirando algumas variaveis auxiliares e noticias que será adicionada posteriormente
dados_trim_pop_dist %>% 
  select(- c(auxPop, noticias)) -> dados_trim_pop_dist

#preprando os dados de noticias, calculando a quantidade delas para adicioná-lo ao banco de dados
noticias$trim[noticias$date %in% c("2020/06", "2020/07", "2020/08")] <- "1º"

noticias$trim[noticias$date %in% c("2020/09", "2020/10", "2020/11")] <- "2º"

noticias$trim[noticias$date %in% c("2020/12", "2021/01", "2021/02")] <- "3º"

noticias$trim[noticias$date %in% c("2021/03", "2021/04", "2021/05")] <- "4º"

noticias$trim[noticias$date %in% c("2021/06", "2021/07", "2021/08")] <- "5º"

noticias$trim[noticias$date %in% c("2021/09", "2021/10", "2021/11")] <- "6º"

noticias$trim[noticias$date %in% c("2021/12", "2022/01", "2022/02")] <- "7º"

noticias$trim[noticias$date %in% c("2022/03", "2022/04", "2022/05")] <- "8º"

noticias %>% 
  group_by(trim) %>% 
  mutate(noticias = sum(noticias))%>% 
  select(-date) %>% 
  distinct(trim, .keep_all = T) -> noticias_trim

#unindo as variáveis de notícias no banco de dados 
dados_trim_final <- merge(dados_trim_pop_dist, noticias_trim, by = "trim", all.x = T)


#normalizando cada uma das variaveis
dados_trim_final %>%
  as_tibble() %>%
  mutate(across(where(is.numeric),  ~ scale(.)[,1])) -> dados_norm_final

#definindo o valor minimo de cada variavel quantitativa como zero
for(i in 3:ncol(dados_norm_final)){
  
  minimo <- min(dados_norm_final[,i])
  
  for(j in 1:nrow(dados_norm_final)){
    
    dados_norm_final[j,i] <- dados_norm_final[j,i] - minimo
    
  }
}

#montando o numero das linhas unindo o trimestre à localidade
nomesDasLinhas <- paste(dados_norm_final$trim, dados_norm_final$distance)

#transformando um banco de dados para o formato dataframe 
dados_norm_final <- data.frame(dados_norm_final)

#defindo o nome das linhas com a variavel criada um pouco acima
rownames(dados_norm_final) <- nomesDasLinhas

#retirando as variaveis trim e distance, pois seus valores estao armazenados na linha 
dados_norm_final %>% 
  select(-c(trim, distance)) -> dados_norm_final

#realizando a analise de correspondencia, deixando as colunas 7 e 8 (Pop e IDHM) como suplementar
res.ca <- ca(dados_norm_final, supcol = 7:8)

#gerando um gráfico com a analise de correspondência criada acima
graph <- plot(res.ca, mass = TRUE, contrib = "absolute", map =
               "rowprincipal", arrows = c(FALSE, TRUE))

#retirando as informações das coordenadas de linhas e colunas do gráfico
data <- data.frame(rbind(graph$rows,graph$cols))

#tratando os dados retirando do gráfico de analise de correspondência
data$local <- toupper(str_replace_all(rownames(data), "[:digit:]|º", ""))
data$trim <- str_replace_all(rownames(data), "[^0-9.º]", "")
data$variable <- rep(c("row", "col"), c(40,9))
data$size <- rep(c(2,3), c(40,9))

#susbstituindo algumas variaveis apos o tratamento dos dados
data$trim[41:49] <- data$local[41:49]
data$local[41:49] <- rep("VARIÁVEIS",9)

#criando dataframes auxiliares
data1 <- data[1:40,]
data2 <- data[c(41:46,49),]

data2aux <- data[47:48,]
data2aux$local <- rep("VARIÁVEIS SUP", 2)

data3 <- data[c(1:5, 36:40),]
data4 <- rbind(data2, data2aux, data3, data[21:25,])

#criando o gráfico de análise de correspondência 
ggplot(data1, aes(x = Dim1, y = Dim2,
                       col = local,
                       label = trim)) +
  geom_vline(xintercept = 0, lty = "dashed", alpha = .5) +
  geom_hline(yintercept = 0, lty = "dashed", alpha = .5) +
  geom_path(size = 1) +
  geom_point(data = data2, aes(Dim1, Dim2), size = 3, shape = 19) +
  geom_point(data = data2aux, aes(Dim1, Dim2), size = 3, shape = 19) + 
  scale_x_continuous(limits = range(data$Dim1) + c(diff(range(data$Dim1)) * -0.2,
                                                   diff(range(data$Dim1)) * 0.2)) +
  scale_y_continuous(limits = range(data$Dim2) + c(diff(range(data$Dim2)) * -0.2,
                                                   diff(range(data$Dim2)) * 0.2)) +
  geom_label_repel(data = data4,  show.legend = F, segment.alpha = .5, point.padding = unit(5, "points"), size = 4,
                   max.overlaps = Inf,
                   force = 5,
                   force_pull = 5) +
  guides(colour = guide_legend(override.aes = list(size = 4))) +
  labs(x = paste0("Dimension 1 (40,5%)"),
       y = paste0("Dimension 2 (25,0%)"),
       col = "", shape = c(1,1,1,1,1,4,6),
       title = "") +
  theme_minimal() +
  scale_colour_manual(values=c("#706F70", "#377EB8", "#4DAF4A", "#984EA3",
                               "#FF7F00", "#000000", "#E41A1C"),
                      labels=c("Entorno de Brasília","Governador Valadares","Juiz de Fora",
                               "Outros","Zona da Mata","Variáveis","Variáveis suplementar")) +
  guides(color = guide_legend(
    override.aes=list(linetype = c(1,1,1,1,1,0,0),
                      shape = c(NA, NA, NA, NA, NA, 19, 16)))) +
  theme(legend.text=element_text(size=14),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        title = element_text(size = 16))

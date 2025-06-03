# Associa os domicílios do CNEFE 2022 em hexógonos urbanos validados do projeto 
# GeoReDUS às Zonas OD 2023. Para ser considerado válido, um domicílio do CNEFE:
# 



library('tidyverse')
library('tidylog')
library('sf')
library('arrow')

# Estrutura de pastas
pasta_dados       <- "../../yellow_dados"
dados_originais     <- sprintf("%s/00_dados_originais/GeoReDUS", pasta_dados)
pasta_aop_2024_2028 <- sprintf("%s/14_aop_2024_2028", pasta_dados)
pasta_dasimetria    <- sprintf("%s/03_dasimetria", pasta_aop_2024_2028)


# ------------------------------------------------------------------------------
# Associar ids CNEFE às Zonas OD
# ------------------------------------------------------------------------------

# Este passo é melhor ser feito no QGIS.
#
# 1. Abrir no QGIS as camadas:
# (a) /yellow_dados/00_dados_originais/GeoReDUS/35_cnefe_ids_domicilios.gpkg (*)
# (b) /yellow_dados/00_dados_originais/Metro/Zonas_OD_2023.gpkg
#
# 2. Menu > Vector > Geoprocessing Tools > Intersection:
#         - Input layer: 35_cnefe_ids_domicilios.gpkg
#         - Overlay layer: Zonas_OD_2023 (filtrar "NumeroMuni" = 36)
#         - Input fields to keep: id_geral
#         - Overlay fields to keep: NumeroZona
#
# 3. Exportar como CSV: 02_intersecao_cnefe_todos_zonaOD.csv na pasta 
# /yellow_dados/14_aop_2024_2028/03_ttmatrix_2024_2028/

# (*) esta camada é uma exportação do arquivo 35_CNEFE_2022.parquet (na mesma 
# pasta), em .gpkg, com a coluna id_geral além do geometry


# ------------------------------------------------------------------------------
# Pegar IDs CNEFE urbanos já validados no processo GeoReDUS
# ------------------------------------------------------------------------------

# A lógica é:
# 1. Linha do CNEFE foi associada a um hexágono urbano? Se não foi, descartar;
# 2. Se sim, o código de setor censitário (CD_SETOR) presente nela é o mesmo
# existente no hexágono? Se sim, tudo ok;
# 3. Se não é mas a atribuição dela tenha sido feita por meio dos centroides
# de face de quadra, há uma divergência entre as bases do IBGE. Especificamente,
# alguns logradouros estão marcados com o CD_SETOR de um setor mas estão fora
# dele quando considerados os shapefiles dos setores censitários. Nesses casos,
# vamos considerá-los como válidos, mas o seu CD_SETOR será considerado o
# que veio da base do CNEFE, pois é este número que bate com o dos resultados
# do universo.

# CNEFEs urbanos válidos
cnefe_hex_setor <- '../../georedus/Dados/00_TMP_APAGAR/03_TMP_CNEFE_INT/05_CNEFE_HEX_2022/35_cnefe_setor_hexagono_urbanos.csv'
cnefe_hex_setor <- read_delim(cnefe_hex_setor, delim = ';', col_types = 'ccc') %>% distinct()
out_file <- sprintf('%s/35_cnefe_setor_hexagono_urbanos.parquet', dados_originais)
write_parquet(cnefe_hex_setor, out_file)

# Filtrar somente município de São Paulo e jorgar na pasta de trabalho
cnefe_hex_setor <- cnefe_hex_setor %>% filter(str_starts(CD_SETOR, '3550308'))
head(cnefe_hex_setor)

out_file <- sprintf('%s/01_intersecao_cnefe_urbanos_validos_setor_hexagono.parquet', pasta_dasimetria)
write_parquet(cnefe_hex_setor, out_file)


# ------------------------------------------------------------------------------
# Validar associação dos domicílios CNEFE com setores censitários e hexágonos
# ------------------------------------------------------------------------------

# Associação entre IDs de domicílios urbanos válidos do CNEFE, hexágonos e setor censitário (SP)
cnefe_hex_setor <- sprintf('%s/01_intersecao_cnefe_urbanos_validos_setor_hexagono.parquet', pasta_dasimetria)
cnefe_hex_setor <- open_dataset(cnefe_hex_setor)
cnefe_hex_setor <- cnefe_hex_setor %>% collect()
head(cnefe_hex_setor)


# Associação entre IDs de domicílios do CNEFE (todos) e zonas OD)
hex_od <- sprintf('%s/02_intersecao_cnefe_todos_zonaOD.csv', pasta_dasimetria)
hex_od <- read_delim(hex_od, delim = ';', col_types = 'ci')
hex_od <- hex_od %>% rename(zona_OD = NumeroZona)
head(hex_od)


# Associar somente de domicílios urbanos válidos às zonas OD
cnefe_hex_setor_zona <- cnefe_hex_setor %>% left_join(hex_od, by = 'id_geral')
cnefe_hex_setor_zona <- cnefe_hex_setor_zona %>% filter(!is.na(zona_OD))
head(cnefe_hex_setor_zona)


# Quais hexágonos ficam de fora? Olhando no mapa, são hexágonos de áreas de fato
# não habitadas, como áreas verdes, parques, shopping centers etc.
hex_sp <- read_sf(sprintf("%s/../IPEA/aop_hex_grid_v2.gpkg", dados_originais))
hex_sp <- hex_sp %>% filter(abbrev_muni == 'spo') %>% select(-c(abbrev_muni, name_muni, code_muni))
hex_sp <- hex_sp %>% filter(!id_hex %in% cnefe_hex_setor_zona$h3_address)
out_teste <- sprintf('%s/hexagonos_que_ficam_de_fora.gpkg', pasta_dasimetria)
st_write(hex_sp, out_teste, driver = 'GPKG', append = FALSE)


dom_validados_zona_hex <-
  cnefe_hex_setor_zona %>%
  group_by(zona_OD, h3_address) %>%
  summarise(dom_urbanos_validos = n()) %>% 
  ungroup() %>% 
  group_by(zona_OD) %>% 
  mutate(perc_zona = dom_urbanos_validos / sum(dom_urbanos_validos)) %>% 
  ungroup()

dom_validados_zona_hex %>% filter(zona_OD == 1) %>% select(dom_urbanos_validos) %>% sum() # 1996
dom_validados_zona_hex %>% filter(zona_OD == 1) %>% select(perc_zona) %>% sum() # 1

# Renomear a coluna, só para deixar mais claro a que ela se refere: domicílios
# do CNEFE validados no âmbito do projeto GeoReDUS
dom_validados_zona_hex <- dom_validados_zona_hex %>% rename(dom_urbanos_validados = dom_urbanos_validos)

# Na OD 2023, temos 4.458.951 domicílios em SP - com quantos ficamos vindos do CNEFE?
sum(dom_validados_zona_hex$dom_urbanos_validados) # 4.961.420


out_file <- sprintf('%s/03_domicilios_validados_cnefe_por_hexagono_e_zona_OD.csv', pasta_dasimetria)
write_delim(dom_validados_zona_hex, out_file, delim = ';')


# ------------------------------------------------------------------------------
# Domicílios válidos por fonte: Censo Básico, CNEFE, Pesquisa OD 2023
# ------------------------------------------------------------------------------

# censo <- '/home/livre/Desktop/Base_GtsRegionais/GitLab/georedus/Dados/02_UNIVERSO/2022/01_basico.parquet'
# censo <- open_dataset(censo)
# censo <- censo %>% filter(cd_mun == '3550308') %>% select(cd_setor, situacao, matches('^V0')) %>% collect()
# 
# # Domicílios em SP: Censo: 4.996.529 / Pesquisa OD: 4.458.951
# censo %>% select(V0002) %>% sum()
# # Urbanos: 4.987.923
# censo %>% filter(situacao == 'Urbana') %>% select(V0002) %>% sum()
# # Particulares urbanos: 4.983.616
# censo %>% filter(situacao == 'Urbana') %>% select(V0003) %>% sum()


# # Domicílios CNEFE considerados válidos no município de SP: 4.962.013
# dasimetria <- '/home/livre/Desktop/Base_GtsRegionais/GitLab/georedus/Dados/00_TMP_APAGAR/03_TMP_CNEFE_INT/06_CNEFE_URBANOS_2022/cnefe_setor_hexagono_urbanos_agrupados_2022.parquet'
# dasimetria <- open_dataset(dasimetria)
# dasimetria <- dasimetria %>% collect()
# dasimetria <- dasimetria %>% filter(str_starts(CD_SETOR, '3550308'))
# dasimetria %>% ungroup() %>% select(dom_urbanos_validos) %>% sum()




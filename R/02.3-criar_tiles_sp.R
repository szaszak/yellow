# Configura o Valhalla para a cidade de São Paulo, parte 2 - cria os tiles com elevação
# e os arquivos de configuração necessários para rodar

# carregar bibliotecas
source('fun/setup.R')

# Estrutura de pastas
pasta_geral_tiles    <- "/home/livre/Desktop/Base_GtsRegionais/GitLab/yellow/valhalla_tiles_sp"
pasta_valhalla_conf  <- sprintf("%s/conf", pasta_geral_tiles)
pasta_valhalla_pbf   <- sprintf("%s/pbf", pasta_geral_tiles)
pasta_valhalla_elev  <- sprintf("%s/elevation_tiles", pasta_geral_tiles)
pasta_valhalla_tiles <- sprintf("%s/valhalla_tiles", pasta_geral_tiles)
out_pbf_file <- sprintf('%s/20220216_sao_paulo_edited_20220915.osm.pbf', pasta_valhalla_pbf)

# ----------------------------------------
# 1. Configurar tiles para SP com elevação
# ----------------------------------------

# Os arquivos de elevação já estão configurados, mas segue aqui o passo a passo:

# Build the elevation tiles
# Para pegar os bounding boxes (no caso, de São Paulo), usar o site 
# https://boundingbox.klokantech.com/ - buscar por "São Paulo" na caixa de busca 
# e escolher o formato .CSV para ver as coordenadas abaixo. As coordenadas são 
# similares às extraídas pelo script do IPEA, então vamos usá-las.

# Visualizar elevação no mapa: https://onthegomap.com/#/create
# https://elevation-tiles-prod.s3.amazonaws.com/index.html#15/-23.6036/-46.6907
# https://valhalla.github.io/demos/elevation/#loc=18,-23.609234,-46.694260&shape=[{%22lat%22:%2247.235422%22,%22lon%22:%229.287224%22},{%22lat%22:%2247.221434%22,%22lon%22:%229.288254%22},{%22lat%22:%2247.205575%22,%22lon%22:%229.269371%22},{%22lat%22:%2247.189246%22,%22lon%22:%229.247398%22},{%22lat%22:%2247.167076%22,%22lon%22:%229.261818%22},{%22lat%22:%2247.181079%22,%22lon%22:%229.284821%22},{%22lat%22:%2247.187613%22,%22lon%22:%229.314003%22},{%22lat%22:%2247.194145%22,%22lon%22:%229.358292%22},{%22lat%22:%2247.190412%22,%22lon%22:%229.408760%22},{%22lat%22:%2247.190179%22,%22lon%22:%229.438629%22},{%22lat%22:%2247.199744%22,%22lon%22:%229.476738%22},{%22lat%22:%2247.209774%22,%22lon%22:%229.500084%22},{%22lat%22:%2247.189712%22,%22lon%22:%229.332542%22},{%22lat%22:%22-23.607536%22,%22lon%22:%22-46.693686%22},{%22lat%22:%22-23.612539%22,%22lon%22:%22-46.695607%22},{%22lat%22:%22-23.610219%22,%22lon%22:%22-46.694673%22},{%22lat%22:%22-23.608253%22,%22lon%22:%22-46.693954%22},{%22lat%22:%22-23.610465%22,%22lon%22:%22-46.694781%22},{%22lat%22:%22-23.609644%22,%22lon%22:%22-46.694475%22},{%22lat%22:%22-23.611350%22,%22lon%22:%22-46.695092%22},{%22lat%22:%22-23.610475%22,%22lon%22:%22-46.694775%22},{%22lat%22:%22-23.609895%22,%22lon%22:%22-46.694566%22},{%22lat%22:%22-23.609531%22,%22lon%22:%22-46.694410%22},{%22lat%22:%22-23.609030%22,%22lon%22:%22-46.694239%22}]&resample_distance=97050

# cd ~/valhalla/00_sao_paulo/
# valhalla_build_elevation -46.826409 -24.007 -46.36509 -23.357 ${PWD}/elevation_tiles


# Refazer o arquivo de configuração do Valhalla:
valhalla_build_config_path <- sprintf("/usr/bin/valhalla_build_config")
arg_vc1 <- sprintf('--mjolnir-tile-dir %s', pasta_valhalla_tiles)
arg_vc2 <- sprintf('--mjolnir-tile-extract %s/valhalla_tiles.tar', pasta_geral_tiles)
arg_vc3 <- sprintf('--mjolnir-timezone %s/timezones.sqlite', pasta_valhalla_tiles)
arg_vc4 <- sprintf('--mjolnir-admin %s/admins.sqlite', pasta_valhalla_tiles)
arg_vc5 <- sprintf('--additional-data-elevation %s', pasta_valhalla_elev)
arg_vc6 <- sprintf(' > %s/valhalla.json', pasta_valhalla_conf)
system2(command = valhalla_build_config_path, 
        args = c(arg_vc1, arg_vc2, arg_vc3, arg_vc4, arg_vc5, arg_vc6))


# Refazer os arquivos de admin areas:
valhalla_build_admins_path <- sprintf("/usr/bin/valhalla_build_admins")
arg_va1 <- sprintf('--config %s/valhalla.json %s', pasta_valhalla_conf, out_pbf_file)
system2(command = valhalla_build_admins_path, args = c(arg_va1))


# Refazer os arquivos de time zones:
valhalla_build_timezones_path <- sprintf("/usr/bin/valhalla_build_timezones")
arg_vtz1 <- sprintf(' > %s/timezones.sqlite', pasta_valhalla_tiles)
system2(command = valhalla_build_timezones_path, args = c(arg_vtz1))


# Refazer os arquivos de tiles, agora com elevação - ATENÇÃO: ele vai usar
# a pasta de 'elevation_tiles' que havia sido criada previamente:
valhalla_build_tiles_path <- sprintf("/usr/bin/valhalla_build_tiles")
arg_vt1 <- sprintf('-c %s/valhalla.json %s', pasta_valhalla_conf, out_pbf_file)
system2(command = valhalla_build_tiles_path, args = c(arg_vt1))


# Consolidar e compactar os arquivos dos tiles
find_path <- sprintf("/usr/bin/find")
arg_find <- sprintf('%s | sort -n | tar -cf "%s/valhalla_tiles.tar" --no-recursion -T -', pasta_valhalla_tiles, pasta_geral_tiles)
system2(command = find_path, args = c(arg_find))



# ----------------------------------------
# 2. Testar o funcionamento do Valhalla
# ----------------------------------------
# Para testar - abrir outro terminal e iniciar o valhalla:
# valhalla_service /home/livre/valhalla/00_sao_paulo/conf/valhalla.json 2
valhalla_service_path <- sprintf("/usr/bin/valhalla_service")
arg_vs1 <- sprintf('%s/valhalla.json 2 &', pasta_valhalla_conf)
system2(command = valhalla_service_path, args = c(arg_vs1))

# Testar buscar dados de elevação com alguns latlong de SP
curl_path <- sprintf("/usr/bin/curl")
arg_c1 <- "-X POST http://localhost:8002/height"
arg_c2 <- "-H 'Content-Type: application/json'"
arg_c3 <- "-H 'cache-control: no-cache'"
arg_c4 <- "-d '{\"range\": true, \"shape\": ["
arg_c5 <- "{\"lat\": -23.55585, \"lon\": -46.70469},"
arg_c6 <- "{\"lat\": -23.55584, \"lon\": -46.70469}, "
arg_c7 <- "{\"lat\": -23.59349, \"lon\": -46.65145}"
arg_c8 <- "]}' | jq '.'"
system2(command = curl_path, args = c(arg_c1, arg_c2, arg_c3, arg_c4, arg_c5, arg_c6, arg_c7, arg_c8))


# Encerrar o processo do Valhalla, para não ficar rodando de fundo
killall_path <- sprintf("/usr/bin/killall")
arg_k1 <- 'valhalla_service'
system2(command = killall_path, args = c(arg_k1))

# E em outro terminal - o resultado deve ser um JSON curto com as altitudes:
# curl -X POST \
# http://localhost:8002/height \
# -H 'Content-Type: application/json' \
# -H 'cache-control: no-cache' \
# -d '{
#     "range": true,
#     "shape": [
#                 {
#         "lat": -23.55585,
#         "lon": -46.70469
#         },
#         {
#         "lat": -23.55584,
#         "lon": -46.70469
#         },
#         {
#             "lat": -23.59349,
#             "lon": -46.65145
#         }
#     ]
# }' | jq '.'
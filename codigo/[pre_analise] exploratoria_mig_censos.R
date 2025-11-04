#'---------------------------------------------------------
#'@projeto Influencia da migracao para a autodeclaracao racial no Brasil
#'@coordenacao Thiago Cordeiro-Almeida e Leonardo Silveira
#'@script Thiago Cordeiro Almeida (CED/Espanha)
#'@ultima-atualizacao 2025-11-04
#'@dados Censo demográfico (Sidra)
#'@script Importacao e tratamento das variaveis para explorar números dos censos 2010
#'---------------------------------------------------------

# configuracoes gerais ----------------------------------------------------

options(scipen = 99999)
rm(list = ls())
invisible(gc())

# pacotes -----------------------------------------------------------------

library(pacman)
p_load(tidyverse, arrow, readxl, sidrar)

# Parametros --------------------------------------------------------------


# Importacao de tabelas ---------------------------------------------------



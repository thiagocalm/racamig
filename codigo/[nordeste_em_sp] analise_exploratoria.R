#'---------------------------------------------------------
#'@projeto RacaMigRaca - migrantes em rmsp e a composicao racial
#'@coordenacao Thiago Cordeiro-Almeida e Leonardo Silveira
#'@script Thiago Cordeiro Almeida (CED/Espanha)
#'@ultima-atualizacao 2026-03-18
#'@dados Censo demográfico 2010
#'@script Importacao e tratamento das variaveis para explorar números dos censos 2010
#'---------------------------------------------------------

# configuracoes gerais ----------------------------------------------------

options(scipen = 99999)
rm(list = ls())
invisible(gc())

# pacotes -----------------------------------------------------------------

library(pacman)
p_load(tidyverse, arrow, readxl, censobr, geobr)


# importar dados ----------------------------------------------------------

# dicionario
censobr::data_dictionary(2010, "emigration")
censobr::data_dictionary(2010, "population")

# variaveis que precisamos
vars <- c(
  "code_muni", "code_state", "abbrev_state", "name_state", "code_region",
  "name_region", "code_weighting","V0001","V0002", "V0011", "V0300","V0010",
  "V1001","V1004","V1006","V0504","V0601","V6036","V0606","V0618","V0619",
  "V0622","V6222","V0623","V0624","V0625","V6252","V6254","V0626","V6262","V6264"
)

# importacao de dados para variaveis necessarias
df <- censobr::read_population(2010,as_data_frame = TRUE, columns = vars)

# reduzindo base para otimizar analises -----------------------------------

df <- df |>
  filter(V1004 == 20) # restricao para RMSP

invisible(gc())

# renomeando variaveis ----------------------------------------------------

df <- df |>
  rename(
    peso = V1001,
    ordem = V0504,
    sexo = V0601,
    idade = V6036,
    raca = V0606,
    ind_nasc_mun = V0618,
    ind_nasc_uf = V0619,
    cod_nasc_uf = V0622,
    dur_mun = V0624,
    dur_uf = V0623,
    ind_ult_tipo = V0625,
    cod_ult_uf = V6252,
    cod_ult_mun = V6254,
    ind_fix_tipo = V0626,
    cod_fix_uf = V6262,
    cod_fix_mun = V6264
  )


# criando variaveis de identificacao de migrante --------------------------

# ajustando variaveis para codigo da uf de residencia anterior
df <- df |>
  # ajustando variaveis primeiro
  mutate(
    cod_ult_uf = str_sub(cod_ult_uf, 0,2),
    cod_fix_uf = str_sub(cod_fix_uf, 0,2)
  ) |>
  #criando regioes de origem
  mutate(
    cod_ult_reg = str_sub(cod_ult_uf,0,1),
    cod_fix_reg = str_sub(cod_fix_uf,0,1)
  )


df <- df |>
  mutate(
    ind_mig_ult = case_when(ind_ult_tipo == 1 ~ 1, TRUE ~ 0),
    ind_mig_fix = case_when(ind_fix_tipo == 1 ~ 1, TRUE ~ 0),
    ind_mig_ult_ne = case_when(ind_ult_tipo == 1 & cod_ult_reg == 2 ~ 1, TRUE ~ 0),
    ind_mig_fix_ne = case_when(ind_fix_tipo == 1 & cod_fix_reg == 2 ~ 1, TRUE ~ 0),
  )

# temos os migrantes para cada tipo de definicao migratoria

df |>
  summarise(
    N_ult = sum(ind_mig_ult),
    N_fix = sum(ind_mig_fix),
    N_ult_ne = sum(ind_mig_ult_ne),
    N_fix_ne = sum(ind_mig_fix_ne)
  ) |>
  mutate(
    prop_ult_ne = N_ult_ne / N_ult,
    prop_fix_ne = N_fix_ne / N_fix
  )

# transformando variaveis em numericas

df <- df |>
  mutate(across(8:37, ~ as.numeric(.x)))

# analises ----------------------------------------------------------------

###
# 1. Numero de migrantes segundo forma de captacao
###

t1 <- df |>
  summarise(
    N_ult = sum(peso[ind_mig_ult == 1]),
    N_fix = sum(peso [ind_mig_fix == 1]),
    N_ult_ne = sum(peso [ind_mig_ult_ne == 1]),
    N_fix_ne = sum(peso [ind_mig_fix_ne == 1])
  ) |>
  mutate(
    prop_ult_ne = N_ult_ne / N_ult,
    prop_fix_ne = N_fix_ne / N_fix,
    sexo = "Total"
  ) |>
  bind_rows(
    df |>
      summarise(
        N_ult = sum(peso[ind_mig_ult == 1]),
        N_fix = sum(peso [ind_mig_fix == 1]),
        N_ult_ne = sum(peso [ind_mig_ult_ne == 1]),
        N_fix_ne = sum(peso [ind_mig_fix_ne == 1]),
        .by = sexo
      ) |>
      mutate(
        prop_ult_ne = N_ult_ne / N_ult,
        prop_fix_ne = N_fix_ne / N_fix,
        sexo = factor(sexo, levels = c(1,2), labels = c("Mulheres","Homens"))
      )
  ) |>
  select(sexo, everything())

t1

###
# 2. Distribuicao por raca segundo status migratorio
###

t2 <- df |>
  filter(raca != 9) |>
  summarise(
    N_ult = sum(peso[ind_mig_ult == 1]),
    N_fix = sum(peso [ind_mig_fix == 1]),
    N_ult_ne = sum(peso [ind_mig_ult_ne == 1]),
    N_fix_ne = sum(peso [ind_mig_fix_ne == 1]),
    .by = c(raca)
  ) |>
  mutate(
    prop_ult_ne = N_ult_ne / N_ult,
    prop_fix_ne = N_fix_ne / N_fix,
    perc_ult = N_ult / sum(N_ult),
    perc_fix = N_fix / sum(N_fix),
    perc_ult_ne = N_ult_ne / sum(N_ult_ne),
    perc_fix_ne = N_fix_ne / sum(N_fix_ne),
    raca = factor(
      raca,
      levels = c(1,2,3,4,5),
      labels = c("Branca","Preta","Amarela","Parda","Indigena")
    )
  )

t2

###
# 3. Distribuicao dos migrantes de ultima etapa segundo tempo desde migracao
###

t3 <- df |>
  filter(! is.na(dur_mun)) |>
  mutate(
    dur_mun = case_when(dur_mun >= 10 ~ 10, TRUE ~ dur_mun)
  ) |>
  summarise(
    N = sum(peso[ind_mig_ult == 1]),
    .by = c(dur_mun)
  ) |>
  arrange(dur_mun) |>
  mutate(
    perc = N / sum(N)
  )

t3

###
# 4. Distribuicao dos migrantes de ultima etapa segundo tempo desde migracao e raca
###

t4 <- df |>
  filter(
    !is.na(dur_mun),
    raca != 9
  ) |>
  mutate(
    dur_mun = case_when(dur_mun >= 10 ~ 10, TRUE ~ dur_mun)
  ) |>
  summarise(
    N = sum(peso[ind_mig_ult == 1]),
    .by = c(raca, dur_mun)
  ) |>
  arrange(dur_mun) |>
  mutate(
    perc = N / sum(N),
    .by = raca
  ) |>
  mutate(
    raca = factor(
      raca,
      levels = c(1,2,3,4,5),
      labels = c("Branca","Preta","Amarela","Parda","Indigena")
    )
  )

t4 |>
  ggplot() +
  aes(x = dur_mun, y = perc, color = raca) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 5, alpha = .5)

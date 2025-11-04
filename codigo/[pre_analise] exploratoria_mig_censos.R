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

# tabela 2010
sidra10 <- read_csv2(file.path("input","sidra_2010_datafixa_munic_sex.csv"))

# tabela 2022
sidra22 <- read_csv2(file.path("input","sidra_2022_datafixa_munic_sex.csv"))

# juncao de tabelas

sidra_munic <- sidra10 |>
  bind_rows(sidra22) |>
  filter(
    !is.na(ano),
    nome != "Brasil"
  ) |>
  janitor::clean_names()

# tabela auxiliar

url <- "https://github.com/thiagocalm/tca_utils/raw/refs/heads/master/from-to%20tables/DTB_2024/RELATORIO_DTB_BRASIL_2024_MUNICIPIOS.xls"

temp <- tempfile(fileext = ".xlsx")
download.file(url, destfile = temp, mode = "wb")
data <- read_excel(temp)

relacao_munic <- read_xls(
  temp,
  sheet = 1,
  skip = 5
)

# Tratamentos -------------------------------------------------------------

# extracao da UF

sidra_munic <- sidra_munic |>
  mutate(
    uf = substr(nome, nchar(nome) - 3 + 1, nchar(nome)-1),
    nome = substr(nome, 1, nchar(nome)-5)
  ) |>
  select(nome, uf, ano, sexo, n)

# juntando informacoes dos municipios

sidra_munic <- sidra_munic |>
  left_join(
    relacao_munic |> select("cod_munic" = `Código Município Completo`, "nome" = `Nome_Município`),
    by = join_by(nome)
  ) |>
  filter(!is.na(cod_munic))

# extraindo regioes

sidra_munic <- sidra_munic |>
  mutate(
    regiao = substr(cod_munic,1,1),
    regiao = factor(regiao, levels = c(1:5), labels = c("Norte","Nordeste","Sudeste","Sul","Centro-Oeste"))
  )

# calculando rankings

sidra_munic <- sidra_munic |>
  filter(sexo == "Total") |>
  arrange(ano, desc(n)) |>
  mutate(
    ranking_maior = row_number(),
    .by = ano
  ) |>
  arrange(ano, n) |>
  mutate(
    ranking_menor = row_number(),
    .by = ano
  ) |>
  pivot_longer(
    ranking_maior:ranking_menor,
    names_to = "ranking",
    values_to = "position"
  )

# Graficos ----------------------------------------------------------------

# pop total - maiores
sidra_munic |>
  filter(position %in% 1:50, ranking == "ranking_maior") |>
  ggplot() +
  aes(
    x = reorder(nome, n),
    y = n,
    color = regiao
  ) +
  geom_point(size = 5, alpha = .6) +
  lemon::facet_rep_grid(. ~ ano, repeat.tick.labels = TRUE, scales = "free_y") +
  coord_flip() +
  theme_light() +
  labs(
    title = "Maiores recebedores de migrantes de data fixa",
    x = "Nome dos município",
    y = "Número de migrantes que não residiam lá 5 anos antes",
    color = "Região"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = .5, vjust = .5),
    legend.position = "bottom"
  )

# pop total - menores
sidra_munic |>
  filter(position %in% 1:50, ranking == "ranking_menor") |>
  ggplot() +
  aes(
    x = reorder(nome, n),
    y = n,
    color = regiao
  ) +
  geom_point(size = 5, alpha = .4) +
  lemon::facet_rep_grid(. ~ ano, repeat.tick.labels = TRUE, scales = "free_y") +
  coord_flip() +
  theme_light() +
  labs(
    title = "Menores recebedores de migrantes de data fixa",
    x = "Nome dos município",
    y = "Número de migrantes que não residiam lá 5 anos antes",
    color = "Região"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = .5, vjust = .5),
    legend.position = "bottom"
  )


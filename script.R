
# ==============================================================================
# SCRIPT DE AULA 3: VISUALIZAÇÃO AVANÇADA DE DADOS COM GGPLOT2
# Conteúdo: Introdução às Camadas, Customizações, Posições e Facetas
# Dados utilizados: alunos.txt e desempenho.csv (Aula 2)
# ==============================================================================

# ------------------------------------------------------------------------------
# PASSO 0: CONFIGURAÇÃO DO AMBIENTE E IMPORTAÇÃO
# ------------------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(scales) # Necessário para formatação de eixos (porcentagem, etc.)

# Importação dos dados da aula anterior ajustando os nulos (NAs)
alunos <- read_delim("dados/alunos.txt", delim = " ", na = c("NA", ""), show_col_types = FALSE)
desempenho <- read_csv("dados/desempenho.csv", na = c("NA", ""), show_col_types = FALSE)

# Preparando uma base unificada para os exemplos multivariados
alunos_desempenho_completo <- alunos |>
  left_join(desempenho, by = "id_aluno") |>
  drop_na(modalidade, periodo_letivo)


# ==============================================================================
# INTRODUÇÃO DIDÁTICA: CONSTRUÇÃO CUMULATIVA EM CAMADAS (A IMAGEM DAS CAMADAS)
# ==============================================================================
# Vamos construir um gráfico passo a passo, acumulando as camadas da imagem
# para entender o papel de cada uma delas no código.

# 1. CAMADA: DATA (Os dados brutos)
# O código apenas inicializa o gráfico com a base de dados. A tela fica cinza/vazia.
ggplot(data = alunos_desempenho_completo)

# 2. CAMADA: MAPPING (Mapeamento estético - aes)
# Definimos quem será o eixo X, o eixo Y e como as cores serão divididas.
# Note que os eixos aparecem na tela, mas ainda não há nenhum dado desenhado.
ggplot(data = alunos_desempenho_completo, 
       mapping = aes(x = horas_estudo, y = prova_1, color = modalidade))

# 3. CAMADA: LAYERS (As formas geométricas - geom_*)
# Adicionamos a geometria de pontos. Agora os dados finalmente aparecem na tela.
ggplot(data = alunos_desempenho_completo, 
       mapping = aes(x = horas_estudo, y = prova_1, color = modalidade)) +
  geom_point(alpha = 0.6, size = 2)

# 4. CAMADA: SCALES (Escalas de cores e eixos)
# Controlamos como os dados são mapeados na tela. Aqui mudamos a paleta de cores.
ggplot(data = alunos_desempenho_completo, 
       mapping = aes(x = horas_estudo, y = prova_1, color = modalidade)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_color_brewer(palette = "Set1")

# 5. CAMADA: FACETS (Subgráficos)
# Dividimos o gráfico em múltiplos painéis com base em uma variável (periodo_letivo).
ggplot(data = alunos_desempenho_completo, 
       mapping = aes(x = horas_estudo, y = prova_1, color = modalidade)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(~ periodo_letivo)

# 6. CAMADA: COORDINATES (Sistemas de coordenadas e limites)
# Controlamos a área de visualização. Aqui limitamos o eixo X entre 0 e 30 horas.
ggplot(data = alunos_desempenho_completo, 
       mapping = aes(x = horas_estudo, y = prova_1, color = modalidade)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(~ periodo_letivo) +
  coord_cartesian(xlim = c(0, 30))

# 7. CAMADA: THEME (Aparência estética não-relacionada aos dados)
# Ajustamos o visual de fundo, fontes e a posição da legenda na tela.
ggplot(data = alunos_desempenho_completo, 
       mapping = aes(x = horas_estudo, y = prova_1, color = modalidade)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(~ periodo_letivo) +
  coord_cartesian(xlim = c(0, 30)) +
  theme_minimal() +
  theme(legend.position = "bottom")


# ==============================================================================
# PARTE 1: ENTENDENDO O PROCESSO DE CONTAGEM (geom_bar vs geom_col)
# ==============================================================================

# 1.1 Gráfico de Barras Horizontal (coord_flip)
ggplot(alunos, aes(x = modalidade)) +
  geom_bar(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Alunos por Modalidade (Horizontal)",
    x = "Modalidade",
    y = "Quantidade"
  ) +
  theme_minimal()

# 1.2 Barras com valores pré-calculados (geom_col)
# Diferença conceitual: dados resumidos fora do ggplot
alunos_modalidade <- alunos |>
  count(modalidade)

ggplot(alunos_modalidade, aes(x = modalidade, y = n)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Quantidade de Alunos por Modalidade (geom_col)",
    x = "Modalidade",
    y = "Quantidade (n)"
  ) +
  theme_minimal()


# ------------------------------------------------------------------------------
# PARTE 2: VISUALIZANDO DISTRIBUIÇÕES NUMÉRICAS
# ------------------------------------------------------------------------------

# 2.1 Gráfico de Densidade Simples (Alternativa ao Histograma)
ggplot(desempenho, aes(x = horas_estudo)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  labs(
    title = "Distribuição das Horas de Estudo (Densidade)",
    x = "Horas de Estudo",
    y = "Densidade"
  ) +
  theme_minimal()

# 2.2 Densidade por Categorias (Comparação de Grupos)
ggplot(
  alunos_desempenho_completo,
  aes(x = horas_estudo, fill = modalidade)
) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Distribuição das Horas de Estudo por Modalidade",
    x = "Horas de Estudo",
    y = "Densidade",
    fill = "Modalidade"
  ) +
  theme_minimal()

# 2.3 Violin Plot
ggplot(desempenho, aes(x = periodo_letivo, y = prova_1)) +
  geom_violin(fill = "steelblue", alpha = 0.6) +
  labs(
    title = "Distribuição das Notas da Prova 1 por Período (Violin)",
    x = "Período Letivo",
    y = "Nota"
  ) +
  theme_minimal()

# 2.4 Combinando Camadas (Layers): Violin + Boxplot
ggplot(desempenho, aes(x = periodo_letivo, y = prova_1)) +
  geom_violin(fill = "steelblue", alpha = 0.5) +
  geom_boxplot(width = 0.15, fill = "white", outlier.color = "red") +
  labs(
    title = "Distribuição das Notas por Período (Sobreposição)",
    subtitle = "Camada combinada de Violin plot e Boxplot",
    x = "Período Letivo",
    y = "Nota"
  ) +
  theme_minimal()


# ------------------------------------------------------------------------------
# PARTE 3: RELAÇÃO E DISPERSÃO (TRATANDO OVERPLOTTING)
# ------------------------------------------------------------------------------

# 3.1 Gráfico de Pontos com Jitter
# Evita a sobreposição de pontos idênticos em variáveis categóricas
ggplot(desempenho, aes(x = periodo_letivo, y = prova_1)) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "darkblue") +
  labs(
    title = "Notas dos Alunos por Período (Jitter)",
    subtitle = "geom_jitter() dispersa pontos sobrepostos artificialmente",
    x = "Período",
    y = "Nota"
  ) +
  theme_minimal()


ggplot(desempenho, aes(x = horas_estudo, y = prova_1, color = horas_estudo >= 20)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(
    values = c("TRUE" = "#2ca02c", "FALSE" = "#7f7f7f"),
    labels = c("TRUE" = "Sim (20h ou mais)", "FALSE" = "Não (Menos de 20h)")
  ) +
  labs(
    title = "Identificação de Alunos com Alta Dedicação",
    x = "Horas de Estudo Semanais",
    y = "Nota na Prova 1",
    color = "Estudou 20h ou mais?"
  ) +
  theme_minimal()

# ------------------------------------------------------------------------------
# PARTE 4: COMPORTAMENTOS TEMPORAIS E ÁREAS
# ------------------------------------------------------------------------------

# 4.1 Evolução das Médias (Gráfico de Linha + Pontos)
media_periodo_linha <- desempenho |>
  group_by(periodo_letivo) |>
  summarise(media_nota = mean(prova_1, na.rm = TRUE))

ggplot(media_periodo_linha, aes(x = periodo_letivo, y = media_nota, group = 1)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(size = 3, color = "darkblue") +
  labs(
    title = "Evolução da Média das Notas por Período Letivo",
    x = "Período Letivo",
    y = "Média de Notas"
  ) +
  theme_minimal()



# ------------------------------------------------------------------------------
# PARTE 5: DUAS VARIÁVEIS CATEGÓRICAS (Ajustando Position)
# ------------------------------------------------------------------------------

# 5.1 Barras Empilhadas (Stack - Padrão)
ggplot(alunos, aes(x = modalidade, fill = sexo)) +
  geom_bar(position = "stack") +
  labs(title = "Alunos por Modalidade e Sexo (Stack)", x = "Modalidade", fill = "Sexo") +
  theme_minimal()

# 5.2 Barras Lado a Lado (Dodge)
ggplot(alunos, aes(x = modalidade, fill = sexo)) +
  geom_bar(position = "dodge") +
  labs(title = "Alunos por Modalidade e Sexo (Dodge)", x = "Modalidade", fill = "Sexo") +
  theme_minimal()


# 5.3 Barras Proporcionais/Percentuais (Fill) adicionando os valores dentro

proporcao_sexo <- alunos |>
  count(modalidade, sexo) |>
  group_by(modalidade) |>
  mutate(
    proporcao = n / sum(n)
  )

ggplot(proporcao_sexo, aes(x = modalidade, y = proporcao, fill = sexo)) +
  geom_col() +
  geom_text(
    aes(label = scales::percent(proporcao, accuracy = 1)),
    position = position_stack(vjust = 0.5)
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proporção de Gênero por Modalidade",
    x = "Modalidade",
    y = "Proporção",
    fill = "Sexo"
  ) +
  theme_minimal()

# 5.4 Barras Proporcionais/Percentuais (Fill) adicionando os valores com o dodge

ggplot(proporcao_sexo, aes(x = modalidade, y = proporcao, fill = sexo)) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = scales::percent(proporcao, accuracy = 1)),
    position = position_dodge(width = 0.9),
    vjust = -0.5
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

# ------------------------------------------------------------------------------
# PARTE 6: VISUALIZAÇÃO ESTATÍSTICA (Pontos Agregados e Barras de Erro)
# ------------------------------------------------------------------------------

# 6.1 Gráfico de Pontos Agregados (Médias Simples)
media_periodo <- desempenho |>
  group_by(periodo_letivo) |>
  summarise(media = mean(prova_1, na.rm = TRUE))

ggplot(media_periodo, aes(x = periodo_letivo, y = media)) +
  geom_point(size = 4, color = "firebrick") +
  labs(title = "Média da Prova 1 por Período", x = "Período", y = "Nota Média") +
  theme_minimal()

# 6.2 Gráfico de Erro (Média +/- Desvio Padrão)
resumo_estatistico <- desempenho |>
  group_by(periodo_letivo) |>
  summarise(
    media = mean(prova_1, na.rm = TRUE),
    desvio = sd(prova_1, na.rm = TRUE)
  )

ggplot(resumo_estatistico, aes(x = periodo_letivo, y = media)) +
  geom_point(size = 3, color = "darkblue") +
  geom_errorbar(
    aes(ymin = media - desvio, ymax = media + desvio),
    width = 0.2, color = "darkblue", linewidth = 0.8
  ) +
  labs(
    title = "Média de Desempenho e Dispersão (Desvio Padrão)",
    x = "Período Letivo",
    y = "Nota (Média +/- Desvio)"
  ) +
  theme_minimal()



# ------------------------------------------------------------------------------
# PARTE 7: VISUALIZAÇÃO MULTIVARIADA COM FACETAS
# ------------------------------------------------------------------------------
notas_longo <- desempenho |>
  select(id_aluno, prova_1, prova_2, trabalho, projeto) |>
  pivot_longer(
    cols = c(prova_1, prova_2, trabalho, projeto),
    names_to = "tipo_avaliacao",
    values_to = "nota"
  ) |> drop_na(nota)  


# 8.1 facet_wrap (Unidirecional / Painéis em sequência)
ggplot(notas_longo, aes(x = nota, fill = tipo_avaliacao)) +
  geom_histogram(binwidth = 1, color = "white", alpha = 0.7) +
  facet_wrap(~ tipo_avaliacao, ncol = 2) + 
  theme_minimal() +
  theme(legend.position = "none")


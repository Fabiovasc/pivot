# pivot

#exericio REGEX
#Utilizando os dados disponíveis em words crie RegEx capaz de encontrar palavras que:

#Comece com “y”.
install.packages('words')
library(words)

# Filtra palavras que começam com "y"
palavras_com_y <- words[grepl("^y", words)]
print(palavras_com_y)

# Filtra palavras que não começam com "y"
palavras_sem_y <- words[grepl("^[^y]", words)]
print(palavras_sem_y)


# Filtra palavras que terminamm com ''x''
final_x <- words[grepl('x$', words)]
print(final_x)

# Filtra palavras que têm exatamente três letras
palavras_três_letras <- words[grepl("^[a-zA-Z]{3}$", words)]
print(palavras_três_letras)

# Filtra palavras que têm sete letras ou mais
palavras_sete_ou_mais <- words[grepl("^.{7,}$", words)]
print(palavras_sete_ou_mais)

TB <- fread('dados rstudio/TB (4).csv')

# Usando o pipe (%>%) do dplyr para encadear operações
TB1 <- TB %>% 
  pivot_longer( 
    # A função pivot_longer transforma dados de formato largo para longo
    cols = -c(1:4), 
    # Aqui, estamos especificando que queremos transformar todas as colunas, exceto as 4 primeiras.
    names_to = "chave", 
    # Os nomes das colunas que estão sendo pivotadas (transformadas) serão armazenados na nova coluna chamada "chave".
    values_to = "casos", 
    # Os valores das colunas pivotadas serão armazenados na nova coluna chamada "casos".
    values_drop_na = TRUE 
    # Esta opção remove quaisquer valores NA (faltantes) da nova coluna "casos" resultante.
  )

TB1

library(magrittr)
# Usando o pipe (%>%) para encadear operações
TB1 %>% 
  count(chave) 
# A função 'count' do dplyr conta o número de ocorrências de cada valor na coluna 'chave'.
# Isso resulta em um novo data frame que contém duas colunas:
# 1. 'chave' - os valores únicos da coluna original 'chave' em TB1.
# 2. 'n' - a contagem de quantas vezes cada valor aparece na coluna 'chave'.

# O operador %<>% é uma combinação do operador %>% e da atribuição (<-), 
# o que significa que estamos atribuindo o resultado de volta a TB1.
TB1 %<>% 
  filter(chave %like% "^new") 
# A função 'filter' do dplyr é usada para selecionar linhas do data frame com base em uma condição.
# Aqui, estamos usando a condição 'chave %like% "^new"' para filtrar as linhas.
# O operador %like% (que é do pacote 'data.table') verifica se os valores na coluna 'chave' 
# começam com a string "new". O símbolo "^" indica que a correspondência deve ocorrer 
# no início da string.
# Portanto, apenas as linhas onde 'chave' começa com "new" serão mantidas em TB1.


TB2 <- TB1 %>% 
  mutate(chave = stringr::str_replace(chave, "newrel", "new_rel"))
TB2

TB3 <- TB2 %>% 
  separate(chave, c("new", "type", "sexage"), 
           sep = "_")
TB3

TB4 <- TB3 %>% 
  select(-new, -iso2, -iso3)

TB5 <- TB4 %>% 
  separate(sexage, c("sexo", "idade"), sep = 1)
TB5

TB_wide <- TB5 %>%
  group_by(country, year) %>%            # Agrupa os dados por país e ano
  summarise(total_casos = sum(casos, na.rm = TRUE), .groups = 'drop') %>%  # Soma os casos por grupo
  pivot_wider(                       # Transforma os dados para o formato wide
    names_from = year,               # Os nomes das colunas virão da coluna 'ano'
    values_from = total_casos,      # Os valores nas novas colunas serão os totais de casos
    names_prefix = "Ano_"            # Prefixo para as colunas de ano
  )

# Exibindo a tabela resultante
TB_wide %>% 
  select(country, Ano_2016)


# Carregando as bibliotecas necessárias
library(dplyr)      # Para manipulação de dados
library(tidyr)      # Para transformar os dados em wide

# Criando a tabela no formato wide
TB_wide <- TB5 %>%
  group_by(country, year, idade, sexo) %>%  # Agrupa os dados por país, ano, idade e sexo
  summarise(total_casos = sum(casos, na.rm = TRUE), .groups = 'drop') %>%  # Soma os casos por grupo
  pivot_wider(                        # Transforma os dados para o formato wide
    names_from = c(year, sexo),     # Cria novas colunas a partir das combinações de idade e sexo
    values_from = total_casos,        # Os valores nas novas colunas serão os totais de casos
    values_fn = list(total_casos = sum),  # Especifica que, se houver múltiplos valores, devemos somá-los
    names_prefix = "Idade_Sexo_"       # Prefixo para as colunas resultantes
  )

# Exibindo a tabela resultante
TB_wide



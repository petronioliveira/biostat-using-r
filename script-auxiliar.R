cirurgia <- readxl::read_excel ("dados/dadosCirurgia.xlsx")

cirurgia$tempo <- cut(cirurgia$tempohosp, 
                      breaks = c(1, 20.75, 27.50, 42.00, 245),
                      labels = c("<= 21","22-28", "29-42",">42"),
                      right = FALSE, 
                      include.lowest = TRUE)

tab1 <- table (cirurgia$tempo)
tab1

cirurgia$infec <- factor(cirurgia$infec, levels = c("sim", "não"))
table(cirurgia$infec)

tab2 <- with(data = cirurgia, table(tempo, infec))
addmargins(tab2)

tab3 <- summarytools::ctable(cirurgia$tempo, cirurgia$infec,
                             prop = "r",
                             headings = FALSE)

summarytools::ctable(cirurgia$tempo, cirurgia$infec,
                     prop = "r",
                     chisq = TRUE,
                     headings = FALSE)

coin::lbl_test (cirurgia$tempo ~ cirurgia$infec)


emerg <- c (1,1,2,2,2,2,2,1,1,1,1,1,1,1,1,2)
controle <- c (1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2)

dadosAsma <- data.frame(emerg, controle)

dadosAsma$emerg <- factor (dadosAsma$emerg,
                           levels = c (1,2),
                           labels = c ('consultou', 'não consultou'))
dadosAsma$controle <- factor (dadosAsma$controle, 
                          levels = c (1,2),
                          labels = c ('asma controlada', 'não controlada'))

tab3 <- with(data = dadosAsma, table(controle, emerg))
addmargins(tab3)


#| label: tbl-rafisher
#| echo: FALSE
#| message: FALSE
#| warning: FALSE
#| tbl-cap: "Consulta à emergência e controle da asma (tab3)"

Controle = c("Asma controlada", "Não controlada", "Total")
Emergência_Sim = c(2, 8, 10)
Emergência_Não = c(5, 1, 6)
Total = c(7, 9, 16)

df <- data.frame(Controle, Emergência_Sim, Emergência_Não, Total)
pacman::p_load(dplyr, flextable)
minha_tab <- flextable(df) %>%
  #set_header_labels(
  #controle = "Controle",
  #emergencia_sim = "Emergência_Sim",
  #emergencia_nao = "Emergência_Não",
  #total = "Total") %>%
  separate_header()  %>%  
  align(align = "center", part = "all") %>% 
  autofit() %>%
  theme_booktabs() %>%
  #align(align = "center", part = "header") %>% 
  #align(align = "center", part = "body") %>%   
  bold(part = "header") %>% 
  bold(j = "Controle", part = "body") 
minha_tab



#| label: tbl-tabmcnemar
#| echo: FALSE
#| message: FALSE
#| warning: FALSE
#| tbl-cap: "Consulta à emergência e controle da asma (tab3)"

Pré_teste <-c("Sim", "Não", "Total")
Pós_teste_Sim <- c("20 (a)", "22 (c)", "42")
Pós_teste_Não <- c("8 (b)", "150 (d)", "158")
Total <- c(28, 172, 200)

df <- data.frame(Pré_teste, Pós_teste_Sim, Pós_teste_Não, Total)

minha_tab <- flextable(df) %>%
  #set_header_labels(
  #pre = "Pré-teste",
  #pos_Sim = "Pós-teste_Sim",
  #pos_Nao = "Pós-teste_Não",
  #total = "Total") %>%
  #separate_header()  %>%  
  #align(align = "center", part = "all") %>% 
  autofit() %>%
  theme_booktabs() %>%
  align(align = "center", part = "header") %>% 
  align(align = "center", part = "body") %>%   
  bold(part = "header") %>% 
  bold(j = "Pré_teste", part = "body") 
minha_tab



#| label: tbl-apgar1
#| echo: FALSE
#| message: FALSE
#| warning: FALSE
#| tbl-cap: "Construção dos postos"

apgar1 <-c(4, 5, 7, 8, 8, 8, 9, 9, 10, 10)
ordem <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
posto <- c(1, 2, 3, 5, 5, 5, 8, 8, 9.5, 9.5)

df <- data.frame(apgar1, ordem, posto)

minha_tab <- flextable(df) %>%
  #set_header_labels(
  #pre = "Pré-teste",
  #pos_Sim = "Pós-teste_Sim",
  #pos_Nao = "Pós-teste_Não",
  #total = "Total") %>%
  #separate_header()  %>%  
  #align(align = "center", part = "all") %>% 
  autofit() %>%
  theme_booktabs() %>%
  align(align = "center", part = "header") %>% 
  align(align = "center", part = "body") %>%   
  bold(part = "header") %>% 
  bold(j = "Pré_teste", part = "body") 
minha_tab

pacman::p_load (coin,
                confintr,
                flextable,
                ggpubr,
                ggsci,
                kableExtra,
                knitr,
                readxl,
                rstatix,
                tidyverse)

cirurgia <- readxl::read_excel ("dados/dadosCirurgia.xlsx")
cirurgia$infec <- factor(cirurgia$infec, levels = c("sim", "não"))

resumo <- cirurgia %>% 
  dplyr::group_by(infec) %>% 
  dplyr::summarise(n = n(),
                   mediana = median (tempohosp, na.rm = TRUE),
                   p25=quantile(tempohosp, probs = 0.25, na.rm = TRUE),
                   p75=quantile(tempohosp, probs = 0.75, na.rm = TRUE))
resumo

dif

infectado <- dplyr::filter(cirurgia, infec == "sim") %>% 
  select(tempohosp)
sem_infec <- dplyr::filter(cirurgia, infec == "não") %>% 
  select(tempohosp)
median(infectado$tempohosp)
median(sem_infec$tempohosp)

dif_mediana <- median(infectado$tempohosp)- median(sem_infec$tempohosp)
dif_mediana <- resumo[1,3] - resumo[2,3]
ci_quantile_diff(cirurgia$infec == "sim", cirurgia$infec == "não")



#| label: tbl-wilcoxef
#| echo: FALSE
#| message: FALSE
#| warning: FALSE
#| tbl-cap: "Interpretação do valor r (sem considerar o sinal)"

magnitude <-c("pequeno", "médio", "grande")
r <- c("0,10 < 0,30", "0,30 < 0,50", ">= 0,50")

df <- data.frame(r, magnitude)

minha_tab <- flextable(df) %>%
  #set_header_labels(
  #pre = "Pré-teste",
  #pos_Sim = "Pós-teste_Sim",
  #pos_Nao = "Pós-teste_Não",
  #total = "Total") %>%
  #separate_header()  %>%  
  #align(align = "center", part = "all") %>% 
  autofit() %>%
  theme_booktabs() %>%
  align(align = "center", part = "header") %>% 
  align(align = "center", part = "body") %>%   
  bold(part = "header") %>% 
  bold(j = "Pré_teste", part = "body") 
minha_tab

names(tabela)

tabela <- read.csv('dados/tabela.csv', sep = ";",  fileEncoding = "latin1")

#| label: tbl-var
#| echo: FALSE
#| message: FALSE
#| warning: FALSE
#| tbl-cap: "Variáveis e dados"

minha_tab <- flextable(tabela) %>%
  set_header_labels(
  id = "Id",
  nome = "Nome",
  idade = "Idade",
  sexo = "Sexo",
  p_sist = "PAS",
  p_diast = "PAD",
  e_geral = "Estado Geral") %>%
  autofit() %>%
  theme_booktabs() %>%
  align(align = "center", part = "header") %>% 
  align(align = "center", part = "body") %>%   
  bold(part = "header") 
minha_tab

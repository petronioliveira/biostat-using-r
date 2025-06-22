pacman::p_load(DescTools,
               dplyr,
               flextable,
               ggplot2, 
               ggpubr, 
               ggsci, 
               grDevices, 
               Hmisc, 
               kableExtra, 
               knitr, 
               plotrix, 
               readxl, 
               scales
               tidyr)


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

#| label: tbl-vpp
#| echo: FALSE
#| fig.align: "center"
#| out.width: '70%'
#| message: FALSE
#| warning: FALSE
#| tbl-cap: "Prevalencia e valor preditivo"



#| tbl-cap: "Distribuição de frequência de drogadição em parturientes, Hospital Geral, Caxias do Sul, RS, 2008"
#| echo: false
#| message: FALSE
#| warning: FALSE

df1 <- data.frame(Droga = c("Nenhuma", "Medicamentos", "Álcool", 
                            "Crack", "Cocaína", "Total"),
                  f = c(904, 23, 17, 2, 1, 947),
                  fr = c(0.955, 0.024, 0.018, 0.002, 0.001, 1.00),
                  fp = c(95.5, 2.4, 1.8, 0.2, 0.1, 100),
                  Fp = c(95.5, 97.9, 99.7, 99.9, 100, ''))

minha_tab <- flextable(df1) %>%
  autofit() %>%
  theme_booktabs() %>%
  align(align = "center", part = "header") %>%
  align(align = "center", part = "body") %>%
  align(j = 1, align = "left", part = "body") %>%
  bold(part = "header")

minha_tab

mater <- readxl::read_excel("dados/dadosMater.xlsx") %>% 
  select(idadeMae, altura, peso, anosEst, fumo, 
         para, ig, sexo, pesoRN, compRN, utiNeo)

mater <- mater %>%
  mutate(categIdade = case_when(
    idadeMae < 20 ~ "< 20 anos",
    idadeMae >= 20 & idadeMae <= 35 ~ "20 a 35 anos",
    idadeMae > 35 ~ "> 35 anos")) %>%
  mutate(categIdade = factor(categIdade, levels = c("< 20 anos", "20 a 35 anos", "> 35 anos")))

### Parte substituída no Cap 6 - Tabelas de frequência
# Um pacote adicional, `kableExtra` @zhu2021construct, permite opções 
# de formatação simples, melhorando o aspecto da tabela, utilizando-se 
# o operador pipe `%>%`. O pacote `kableExtra` foi projetado para estender 
# a funcionalidade básica das tabelas produzidas usando `knitr::kable()`. 
# Podem ser acrescentadas várias das suas funções como `kable_styling()` 
# ou `kable_classic()` para especificar estilos à tabela, como extensão 
# da tabela, alinhamento, tipo e tamanho da fonte



f_abs <- table (mater$categIdade)
f_rel <- round(prop.table(f_abs), 3)
f_perc <- round(f_rel*100, 2)
f_abs <- c (f_abs, sum(f_abs))
f_rel <- c (f_rel, sum (f_rel))
f_perc <- c (f_perc, sum (f_perc))
tab1 <- cbind(f_abs,
              f_rel ,
              f_perc)
tab1 <- as.data.frame(tab1)
row.names(tab1)[4] <-  "Total"
colnames(tab1) <- c("Faixa etária", "f", "fr", "fp (%)")
tab1



#| tbl-cap: "Estado nutricional pré-gestacional das parturientes"
#| echo: false
#| message: FALSE
#| warning: FALSE
df <- data.frame(estNutri = c("Baixo Peso", "Peso adequado", "Sobrepeso", "Obesidade","Total"),
                 f = c(67, 799, 327, 175, 1368),
                 fr = c(0.049, 0.584, 0.239, 0.128, 1.000),
                 fp = c(4.9, 58.4, 23.9, 12.8, 100),
                 Fp = c(4.9, 63.3, 87.2, 100, ''))

minha_tab <- flextable(df) %>%
  autofit() %>%
  set_header_labels(
    estNutri = "Estado Nutricional",
    f = "f",
    fr = "fr",
    fp = "fp (%)",
    Fp = "Fp (%)") %>%
  theme_booktabs() %>%
  width(j = "estNutri", width = 1.5) %>%
  width(j = 2:5, width = 1) %>%
  align(align = "center", part = "header") %>%
  align(align = "center", part = "body") %>%
  align(j = 1, align = "left", part = "body") %>%
  bold(i = 5, part = "body") %>%
  bold(part = "header") %>%
  hline(i = 4, part = "body", 
        border = fp_border_default(color = "black", width = 1.5)) %>% 
  add_footer_lines(value = "FONTE:Hospital Geral, Caxias do Sul, RS, 2008") %>%
  align(align = "right", part = "footer") %>% 
  fontsize(size = 9, part = "footer")

minha_tab


mater <- readxl::read_excel("dados/dadosMater.xlsx") %>%
  mutate(imc = peso/altura^2) %>% 
  select(idadeMae, altura, peso, anosEst, fumo, 
         para, ig, sexo, pesoRN, compRN, utiNeo, imc)

mater <- mater %>% 
  mutate (estNutri = case_when(
    imc < 18.5 ~ "Baixo Peso",
    imc >= 18.5 & imc < 25 ~ "Peso adequado", 
    imc >= 25 & imc < 30 ~ "Sobrepeso",
    imc >= 30 ~ "Obesidade")) %>%
  mutate(estNutri = factor(estNutri, 
                           levels = c("Baixo Peso", "Peso adequado", 
                                      "Sobrepeso", "Obesidade")))

f.abs <- table (mater$estNutri)
f.rel <- round(prop.table(f.abs), 3)
f.perc <- round(f.rel*100, 2)

f.abs <- c (f.abs, sum(f.abs))
f.rel <- c (f.rel, sum (f.rel))
f.perc <- c (f.perc, sum (f.perc))

tab2 <- cbind(f.abs,
              f.rel ,
              f.perc)

tab2 <- as.data.frame(tab2)
row.names(tab2)[5] <-  "Total"
colnames(tab2) <- c("f", "fr", "fp")
tab2

#| label: tbl-pizza
#| tbl-cap: "Sentimento dos alunos do curso de Medicina em relação ao gráfico de pizza"
#| echo: FALSE
#| message: FALSE
#| warning: FALSE
df2 <- data.frame(sent = c("Odeiam", "Não gostam",
                                 "Indiferentes", "Amam", "Total"),
                  f = c(6, 12, 14, 8, 40),
                  fr = c(0.15, 0.30, 0.35, 0.20, 1.00),
                  fp = c(15, 30, 35, 20, 100),
                  Fp = c(15, 45, 80, 100, NA))

minha_tab <- flextable(df2) %>%
  autofit() %>%
  set_header_labels(
    sent = "Sentimento",
    f = "f",
    fr = "fr",
    fp = "fp (%)",
    Fp = "Fp (%)") %>%
  theme_booktabs() %>%
  width(j = "sent", width = 1.8) %>%
  width(j = 2:5, width = 1) %>%
  align(align = "center", part = "header") %>%
  align(align = "center", part = "body") %>%
  align(j = 1, align = "left", part = "body") %>%
  bold(i = 5, part = "body") %>%
  bold(part = "header") %>%
  hline(i = 4, part = "body", 
        border = fp_border_default(color = "black", width = 1.5)) %>% 
  add_footer_lines(value = "FONTE: Universidade de Caxias do Sul, 2012") %>%
  align(align = "right", part = "footer") %>% 
  fontsize(size = 9, part = "footer")

minha_tab


#| label: tbl-effectsize
#| tbl-cap: "Interpretação do Coeficiente de Assimetria"
#| echo: FALSE
#| message: FALSE
#| warning: FALSE

res = c("0.01", "0,06", "0,14")
effect = c("pequeno","médio","grande")

df <- data.frame(res, effect)

minha_tab <- flextable(df) %>%
  set_header_labels(
    res = "Resultado",
    effect = "Tamanho do Efeito") %>%
  autofit() %>%
  theme_booktabs() %>%
  width(j = 1, width = 1) %>%
  width(j = 2, width = 1.7) %>%
  align(align = "left", part = "header") %>%
  align(align = "left", part = "body") %>%
  bold(part = "header") 

minha_tab


#| label: tbl-tab2x3
#| tbl-cap: "Distribuição do Consumo de Álcool e o Sexo"
#| echo: FALSE
#| message: FALSE
#| warning: FALSE

sexo = c("Feminino", "Masculino")
nenhum = c(8, 8)
tres = c(8,8)
seis = c(8,8)

df <- data.frame(sexo, nenhum, tres, seis)

minha_tab <- flextable(df) %>%
  set_header_labels(sexo = "Sexo",
                    nenhum = "Nenhum",
                    tres = "Três latas",
                    seis = "Seis latas") %>%
  autofit() %>%
  theme_booktabs() %>%
  width(j = 1:4, width = 1) %>%
  align(align = "center", part = "header") %>%
  align(align = "center", part = "body") %>%
  align(j = 1, align = "left", part = "body") %>%
  bold(part = "header") %>% 
  add_footer_lines(value = "* Tamanho médio da lata = 350 ml") %>%
  align(align = "left", part = "footer") %>% 
  fontsize(size = 9, part = "footer")

minha_tab


#| label: tbl-result
#| tbl-cap: "Efeito do Álcool* sobre a Memória de acordo com o sexo **"
#| echo: FALSE
#| message: FALSE
#| warning: FALSE

sexo = c("Feminino", "Masculino", "Valor P")
nenhum = c("60,6 (5,0)", "66,9 (10,3)", "0,145")
um = c("62,5 (6,6)", "66,9 (12,5)", "0,396")
dois = c("57,5 (7,1)", "35,6 (10,8)", "0,0003")

df <- data.frame(sexo, nenhum, um, dois)

minha_tab <- flextable(df) %>%
  set_header_labels(sexo = "Sexo",
                    nenhum = "Nenhum",
                    um = "Um litro",
                    dois = "Dois litros") %>%
  autofit() %>%
  theme_booktabs() %>%
  width(j = 1:4, width = 1.3) %>%
  align(align = "center", part = "header") %>%
  align(align = "center", part = "body") %>%
  align(j = 1, align = "left", part = "body") %>%
  bold(part = "header") %>%  
  hline(i = 2, part = "body", 
      border = fp_border_default(color = "black", width = 1.3)) %>% 
  add_footer_lines(value = "* Um litro de cerveja (4,5%) = 5 unidades de alcool") %>%
  align(align = "left", part = "footer") %>% 
  fontsize(size = 9, part = "footer") %>% 
  add_footer_lines(value = "** Escore médio(desvio padrão)") %>%
  align(align = "left", part = "footer") %>% 
  fontsize(size = 9, part = "footer")

minha_tab

library(rstatix)
autoestima <- readxl::read_excel("dados/dadosAutoestima2.xlsx")
autoestimaL <- autoestima %>% 
  tidyr::pivot_longer(cols = c(t1, t2, t3),
                      names_to = "tempo",
                      values_to = "escores") %>% 
  convert_as_factor(id, tratamento, tempo)

mod.anova2 <- rstatix::anova_test(data = autoestimaL, 
                                  dv = escores,
                                  wid = id,
                                  within = tempo, 
                                  between = tratamento,
                                  type = 3)

#| label: tbl-tabanova2
#| tbl-cap: "Tabela da Anova Corrigida (GGe)"
#| echo: FALSE
#| message: FALSE
#| warning: FALSE

tab_anova2 <- rstatix::get_anova_table(mod.anova2, correction = "GG")

minha_tab <- flextable(tab_anova2) %>%
  autofit() %>%
  theme_booktabs() %>%
  bold(part = "header") 

minha_tab


#| label: tbl-coefr
#| tbl-cap: "Tamanho amostral para regressão de acordo com o r de Pearson e o número de variáveis"
#| echo: FALSE
#| message: FALSE
#| warning: FALSE

r_pearson <-c(0.2, 0.3,0.4)
uma <- c(190, 80, 45)
duas <- c(230, 100, 55)
tres <- c(265, 115, 65)
quatro <- c(290, 125, 70)

df <- data.frame(r_pearson, uma, duas, tres, quatro)

minha_tab <- flextable(df) %>%
  set_header_labels(
    r_pearson = "r de Pearson",
    uma = "Uma",
    duas = "Duas",
    tres = "Três",
    quatro = "Quatro") %>%
  autofit() %>%
  add_header_row(values = c("", "Número de Variáveis"),
                 colwidths = c(1, 4)) %>% 
  theme_booktabs() %>%
  width(j = 1, width = 1.5) %>%
  width(j = 2:4, width = 1.5) %>%
  align(align = "center", part = "header") %>%
  align(align = "center", part = "body") %>%
  bold(part = "header") 

minha_tab


fumo <-c("Sim", "Não", "Total")
baixoPeso <- c(18, 22, 40)
sem_bp <- c(51, 209, 260)
Total <- c(69, 260, 300)

df <- data.frame(fumo, baixoPeso, sem_bp, Total)

minha_tab <- flextable(df) %>%
  set_header_labels(
    fumo = "Tabagismo",
    baixoPeso = "Baixo Peso",
    sem_bp = "Sem Baixo Peso",
    Total = "Total") %>%
  autofit() %>%
  add_footer_lines(value = "* Baixo Peso = peso ao nascer < 2500g") %>%
  align(align = "left", part = "footer") %>% 
  fontsize(size = 9, part = "footer") %>% 
    theme_booktabs() %>%
  align(align = "center", part = "header") %>% 
  align(align = "center", part = "body") %>%   
  bold(part = "header") %>%
  bold(i = ~ fumo == "Total", part = "body")

minha_tab


Controle = c("Asma controlada", "Não controlada", "Total")
Emerg_sim = c(2, 8, 10)
Emerg_nao = c(5, 1, 6)
Total = c(7, 9, 16)

df <- data.frame(Controle, Emerg_sim, Emerg_nao, Total)

minha_tab <- flextable(df) %>%
  set_header_labels(
    Controle = "Controle",
    Emerg_sim = "Sim",
    Emerg_nao = "Não",
    Total = "Total") %>%
  autofit() %>%
  add_header_row(values = c("", "Visita à Emergência", ""),
                 colwidths = c(1, 2, 1)) %>% 
  theme_booktabs() %>%
  width(j = 1, width = 2) %>%
  width(j = 2:4, width = 1.2) %>%
  align(align = "center", part = "header") %>%
  align(align = "center", part = "body") %>%
  align(j = 1, align = "left", part = "body") %>%
  bold(part = "header") %>% 
  bold(j = 1, part = "body") %>%
  hline(i = 2, part = "body", 
        border = fp_border_default(color = "black", width = 1.3)) 

minha_tab



pre <-c("Sim", "Não", "Total")
pos_sim <- c("20 (a)", "22 (c)", "42")
pos_nao <- c("8 (b)", "150 (d)", "158")
total <- c(28, 172, 200)

df <- data.frame(pre, pos_sim, pos_nao, total)

minha_tab <- flextable(df) %>%
  autofit() %>%
  set_header_labels(
    pre = "Pré-Teste",
    pos_sim = "Sim",
    pos_nao = "Não",
    total = "Total") %>%
  autofit() %>%
  add_header_row(values = c("", "Pós-teste", ""),
                 colwidths = c(1, 2, 1)) %>% 
  theme_booktabs() %>%
  width(j = 1, width = 1.2) %>%
  width(j = 2:4, width = 1.2) %>%
  align(align = "center", part = "header") %>% 
  align(align = "center", part = "body") %>%   
  bold(part = "header") %>% 
  bold(j = 1, part = "body") %>% 
  hline(i = 2, part = "body", 
        border = fp_border_default(color = "black", width = 1.5))
minha_tab

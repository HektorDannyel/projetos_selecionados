# Pacotes necessários -----------------------------------------------------

library(tidyverse)
library(readxl)
library(stringi)
library(scales)
library(utilidades)
library(kableExtra)
library(grid)
library(gridExtra)
library(htmltools)

# Lendo dados -------------------------------------------------------------

cadav_0 <- read_rds("dados/dados.rds")

total_tx <- read_rds("dados/tx_total.rds")

media_sf36 <- read_rds("dados/media_sf36.rds") |> 
  mutate(dominio = str_to_lower(dominio))

# Preparação ---------------------------------------------------------------------

cadav <- cadav_0 |> 
  rename_all(~ . |> str_to_lower() |> str_replace_all("\\s|-", "_") |> stri_trans_general("Latin-ASCII")) |> 
  mutate_if(is.character, ~ . |> str_to_lower() |> stri_trans_general("Latin-ASCII")) |> 
  mutate(tempo_seguimento = 2018 - ano) |> 
  mutate_at(vars(meld), ~ . |> str_remove_all("[a-z\\s]*") |> as.numeric()) |> 
  mutate(isquemia = hour(`isquemia_total_(hrs)`) + minute(`isquemia_total_(hrs)`)/60,
         etiologia = ifelse(etiologia == "alcoll", "alcool", etiologia),
         transfusao_io = str_replace(transfusao_io, "uplasma", "plasma"),
         complicacoes_agudas = str_remove(complicacoes_agudas, " \\(pulsoterapia\\)"),
         complicacoes_tardias = str_remove(complicacoes_tardias, " \\(pulsoterapia\\)"),
         causa_reop = ifelse(str_detect(causa_reop, "estenose"), "estenose coledoco", causa_reop),
         rejeicao = case_when(
           
           rejeicao == "aguda e cronica" ~ "aguda, cronica",
           str_detect(rejeicao, "aguda") ~ "aguda",
           str_detect(rejeicao, "cronica") ~ "cronica",
           TRUE ~ rejeicao
           
         ))

total_cadav <- dim(cadav)[1]

# Análise Descritiva Dados Clínicos ------------------------------------------------------

## Transplantados vs Sobreviventes

total_vs_sobrev <- total_tx |> 
  count(ano, name = "total") |> 
  left_join(cadav |> 
              count(ano, name = "vivos")) |> 
  pivot_longer(-ano) |> 
  mutate(value = ifelse(is.na(value), 0, value),
         name = ifelse(name == "total",
                       "Número de pacientes transplantados de 1991 a 2009",
                       "Número de pacientes ainda vivos após transplante")) |> 
  ggplot(aes(x = factor(ano), y = value, fill = name)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = value), position = position_dodge(.9), vjust = -.5) +
  xlab("Ano") + ylab("Total") +
  labs(title = "Total de transplantados vs sobreviventes") +
  theme(plot.title = element_text(hjust = .5), legend.position = "bottom", legend.title = element_blank())

## Variáveis numéricas

### Resumo

caracteristicas_numericas <- cadav |> 
  summarise_at(vars(idade_hoje, idade_no_tx, tempo_seguimento, imc, meld, tempo_cx = `tempo_cx_(min)`, isquemia, uti = `tempo_uti_(dias)`, internamento = `tempo_internado_(dias)`),
               list(~mean(., na.rm = TRUE), ~sd(., na.rm = TRUE), ~median(., na.rm = TRUE), ~min(., na.rm = TRUE), ~max(., na.rm = TRUE))) |> 
  pivot_longer(everything()) |> 
  mutate(name = str_replace(name, "(_)([a-z]+$)", ".\\2")) |> 
  separate(name, into = c("variavel", "estatistica"), sep = "\\.") |> 
  pivot_wider(names_from = estatistica, values_from = value) |> 
  rename(media = mean, dp = sd, mediana = median)

### Dispersão das variáveis temporais

plot_tempo <- cadav |> 
  select(`Idade hoje` = idade_hoje, `Idade no transplante` = idade_no_tx, `Tempo seguimento` = tempo_seguimento) |> 
  pivot_longer(everything()) |> 
  ggplot(aes(x = name, y = value, color = name)) +
  geom_boxplot() +
  geom_jitter(width = .2) +
  facet_wrap(~name, scales = "free") +
  xlab("") + ylab("Anos") +
  labs(title = "Dispersão das variáveis temporais") +
  theme(plot.title = element_text(hjust = .5), legend.position = "none")

### Dispersão IMC

plot_imc <- cadav |> 
  ggplot(aes(x = 1, y = imc)) +
  geom_hline(yintercept = c(18.5, 25, 30, 35, 40)) +
  geom_boxplot() +
  geom_jitter(width = .2) +
  annotate("text", x = .6, y = c(18.5, 25, 30, 35, 40) + 0.5, label = c(18.5, 25, 30, 35, 40)) +
  xlab("") + ylab("kg/m²") +
  labs(title = "IMC", caption = "Valores de referência de acordo com o Ministério da Saúde \n<https://www.gov.br/conitec/pt-br/midias/protocolos/resumidos/PCDTResumidodeSobrepesoObesidade.pdf.pdf>") +
  theme(plot.title = element_text(hjust = .5), axis.ticks.x = element_blank(), axis.text.x = element_blank(), plot.caption = element_text(hjust = .5))

imc_cat <- cadav |> 
  select(imc) |> 
  filter(!is.na(imc)) |> 
  mutate(imc_cat = case_when(
    
    imc < 18.5 ~ "Abaixo do peso",
    imc < 25 ~ "Eutrófico",
    imc < 30 ~ "Sobrepeso",
    imc < 35 ~ "Obesidade grau I",
    imc < 40 ~ "Obesidade grau II",
    TRUE ~ "Obesidade grau III"
    
  ),
  faixa = case_when(
    
    imc < 18.5 ~ "Abaixo de 18.5",
    imc < 25 ~ "18.5 - 24.9",
    imc < 30 ~ "25 - 29.9",
    imc < 35 ~ "30 - 34.9",
    imc < 40 ~ "35 - 39.9",
    TRUE ~ "40 ou mais"
    
  ),
  imc_cat = factor(imc_cat, levels = c("Abaixo do peso", "Eutrófico", "Sobrepeso", "Obesidade grau I", "Obesidade grau II", "Obesidade grau III"))) |> 
  count(imc_cat, faixa) |> 
  mutate(p = n/sum(n),
         P = cumsum(p),
         Q = rev(cumsum(rev(p))))

### Dispersão MELD

plot_meld <- cadav |> 
  ggplot(aes(x = 1, y = meld)) +
  geom_boxplot() +
  geom_jitter(width = .2) +
  xlab("") + ylab("Score MELD") +
  labs(title = "MELD") +
  theme(plot.title = element_text(hjust = .5), axis.ticks.x = element_blank(), axis.text.x = element_blank())

### Dispersão tempo de cirurgia

plot_tempo_cx <- cadav |> 
  ggplot(aes(x = 1, y = `tempo_cx_(min)`)) +
  geom_boxplot() +
  geom_jitter(width = .2) +
  xlab("") + ylab("Minutos") +
  labs(title = "Tempo de cirurgia") +
  theme(plot.title = element_text(hjust = .5), axis.ticks.x = element_blank(), axis.text.x = element_blank())

quant_tempo_cx <- cadav |> 
  reframe(valor = quantile(`tempo_cx_(min)`, na.rm = TRUE)) |> 
  add_column(quartil = c("min", "0.25", "0.5", "0.75", "max"), .before = "valor")

### Dispersão tempo de isquemia

plot_isquemia <- cadav |> 
  ggplot(aes(x = 1, y = isquemia)) +
  geom_boxplot() +
  geom_jitter(width = .2) +
  xlab("") + ylab("Horas") +
  labs(title = "Tempo de isquemia") +
  theme(plot.title = element_text(hjust = .5), axis.ticks.x = element_blank(), axis.text.x = element_blank())

### Dispersão tempo de internamento UTI

plot_uti <- cadav |> 
  ggplot(aes(x = 1, y = `tempo_uti_(dias)`)) +
  geom_boxplot() +
  geom_jitter(width = .2) +
  xlab("") + ylab("Dias") +
  labs(title = "Tempo de internamento em UTI") +
  theme(plot.title = element_text(hjust = .5), axis.ticks.x = element_blank(), axis.text.x = element_blank())

### Dispersão tempo de internamento comum

plot_internamento <- cadav |> 
  ggplot(aes(x = 1, y = `tempo_internado_(dias)`)) +
  geom_boxplot() +
  geom_jitter(width = .2) +
  xlab("") + ylab("Dias") + 
  labs(title = "Tempo de internamento") +
  theme(plot.title = element_text(hjust = .5), axis.ticks.x = element_blank(), axis.text.x = element_blank())

## Variáveis discretas

### Dispersão de gênero

genero <- cadav |> 
  count(sexo) |> 
  mutate(total = sum(n),
         p = n/total)

plot_genero <- genero |> 
  mutate(sexo = str_to_upper(sexo),
         tt = paste(n, "\n", percent(p, .1))) |> 
  ggplot(aes(x = sexo, y = n, fill = sexo)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = tt), vjust = 2, size = 6) +
  xlab("Gênero") + ylab("Total") +
  labs(title = "Distribuição de gênero") +
  theme(plot.title = element_text(hjust = .5), legend.position = "none")

### Dispersão score Child-Pugh
  
child_pugh <- cadav |> 
  count(child) |> 
  filter(!is.na(child)) |> 
  mutate(total = sum(n),
         p = n/total)

plot_child_pugh <- child_pugh |> 
  mutate(child = str_to_upper(child),
         tt = paste(n, "\n", percent(p, .1))) |> 
  ggplot(aes(x = child, y = n, fill = child)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = tt), vjust = 2, size = 6) +
  xlab("Categoria") + ylab("Total") +
  labs(title = "Distribuição do Score Child-Pugh") +
  theme(plot.title = element_text(hjust = .5), legend.position = "none")

### Dispersão tipo sanguíneo

tipo_sang <- cadav |> 
  count(abo) |> 
  filter(!is.na(abo)) |> 
  mutate(total = sum(n),
         p = n/total)

plot_tipo_sang <- tipo_sang |> 
  mutate(abo = str_to_upper(abo),
         tt = paste(n, "\n", percent(p, .1)),
         vjust = ifelse(p < .1, -1, 2)) |> 
  ggplot(aes(x = abo, y = n, fill = abo)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = tt, vjust = vjust), size = 6) +
  xlab("Tipo") + ylab("Total") +
  labs(title = "Distribuição do tipo sanguíneo") +
  theme(plot.title = element_text(hjust = .5), legend.position = "none")

### Dispersão Etiologia

etiologia_total <- cadav |> 
  select(etiologia) |> 
  separate_rows(etiologia, sep = " \\+ ") |> 
  count(etiologia, sort = TRUE) |> 
  mutate(total = total_cadav,
         p = n/total)

etiologia_outro <- cadav |> 
  select(etiologia) |> 
  separate_rows(etiologia, sep = " \\+ ") |> 
  count(etiologia, sort = TRUE) |> 
  mutate(etiologia = ifelse(etiologia == "alcoll", "alcool",
                            ifelse(n == 1, "outro", etiologia))) |> 
  group_by(etiologia) |> 
  summarise(n = sum(n)) |> 
  arrange(desc(n)) |> 
  mutate(total = total_cadav,
         p = n/total)

### Dispersão comorbidade prévia

comorb_pre_total <- cadav |> 
  select(comorb_pre) |> 
  separate_rows(comorb_pre, sep = "\\s*,\\s*") |>
  count(comorb_pre, sort = TRUE) |> 
  mutate(total = total_cadav,
         p = n/total)

comorb_pre_sim <- cadav |> 
  select(comorb_pre) |> 
  filter(!comorb_pre == "sc") |> 
  mutate(total = length(comorb_pre)) |> 
  separate_rows(comorb_pre, sep = "\\s*,\\s*") |>
  group_by(total) |> 
  count(comorb_pre, sort = TRUE) |> 
  ungroup() |> 
  mutate(p = n/total)

comorb_pre_sim_outro <- comorb_pre_sim |> 
  mutate(comorb_pre = ifelse(n == 1, "outro", comorb_pre)) |> 
  group_by(comorb_pre, total) |> 
  summarise(n = sum(n)) |> 
  ungroup() |> 
  mutate(p = n/total)

### Dispersão complicações cirrose

complicacoes_cirrose <- cadav |> 
  select(complicacoes_cirrose) |> 
  separate_rows(complicacoes_cirrose, sep = "\\s*,\\s*") |>
  count(complicacoes_cirrose, sort = TRUE) |> 
  mutate(total = total_cadav,
         p = n/total)

complicacoes_cirrose_sim <- cadav |> 
  select(complicacoes_cirrose) |> 
  filter(!complicacoes_cirrose == "sc") |> 
  mutate(total = length(complicacoes_cirrose)) |> 
  separate_rows(complicacoes_cirrose, sep = "\\s*,\\s*") |>
  group_by(total) |> 
  count(complicacoes_cirrose, sort = TRUE) |> 
  ungroup() |> 
  mutate(p = n/total)

complicacoes_cirrose_sim_outro <- complicacoes_cirrose_sim |> 
  mutate(complicacoes_cirrose = ifelse(n == 1, "outro", complicacoes_cirrose)) |> 
  group_by(complicacoes_cirrose, total) |> 
  summarise(n = sum(n)) |> 
  ungroup() |> 
  mutate(p = n/total)

### Dispersão comorbidades posteriores

comorb_pos_total <- cadav |> 
  select(comorb_pos) |> 
  separate_rows(comorb_pos, sep = "\\s*,\\s*") |>
  count(comorb_pos, sort = TRUE) |> 
  mutate(total = total_cadav,
         p = n/total)

comorb_pos_sim <- cadav |> 
  select(comorb_pos) |> 
  filter(!comorb_pos == "sc") |> 
  mutate(total = length(comorb_pos)) |> 
  separate_rows(comorb_pos, sep = "\\s*,\\s*") |>
  group_by(total) |> 
  count(comorb_pos, sort = TRUE) |> 
  ungroup() |> 
  mutate(p = n/total)

comorb_pos_sim_outro <- comorb_pos_sim |> 
  mutate(comorb_pos = ifelse(n == 1, "outro", comorb_pos)) |> 
  group_by(comorb_pos, total) |> 
  summarise(n = sum(n)) |> 
  ungroup() |> 
  mutate(p = n/total)

### Dispersão de necessidade de transfusão

transfusao_freq <- cadav |> 
  select(transfusao_io) |> 
  mutate(transfusao_io = ifelse(!is.na(transfusao_io) & !transfusao_io == "nao", "sim", transfusao_io)) |> 
  count(transfusao_io) |> 
  mutate(total = sum(n),
         p = n/total,
         na = is.na(transfusao_io)) |> 
  group_by(na) |> 
  mutate(total_sem_na = ifelse(is.na(transfusao_io), NA, sum(n)),
         p_sem_na = n/total_sem_na) |> 
  ungroup() |> 
  select(-na)

transfusao_freq_plot <- transfusao_freq |> 
  filter(!is.na(transfusao_io)) |> 
  mutate(transfusao_io = ifelse(transfusao_io == "sim", "Sim", "Não"),
         tt = paste(total_sem_na, "\n", percent(p_sem_na, .1)),
         vjust = ifelse(p < .1, -1, 2)) |> 
  ggplot(aes(x = transfusao_io, y = n, fill = transfusao_io)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = tt, vjust = vjust), size = 6) +
  xlab(" ") + ylab("Total") +
  labs(title = "Necessidade de transfusão") +
  theme(plot.title = element_text(hjust = .5), legend.position = "none")

### Dispersão do tipo de transfusão

transfusao_0 <- cadav |> 
  select(transfusao_io) |> 
  filter(!is.na(transfusao_io)) |> 
  separate_rows(transfusao_io, sep = "\\s*\\+\\s*") |> 
  mutate(transfusao_io = str_replace(transfusao_io, "([0-9]+)([a-z]+)", "\\1_\\2")) |> 
  separate(transfusao_io, sep = "_", into = c("qtd", "tipo")) |> 
  filter(!is.na(tipo)) |> 
  mutate(qtd = replace_na(as.numeric(qtd), 0))

transfusao <- transfusao_0 |> 
  group_by(tipo) |> 
  summarise(media = mean(qtd),
            dp = sd(qtd), 
            mediana = median(qtd),
            min = min(qtd),
            max = max(qtd),
            contagem = n()) |> 
  ungroup()

plot_transfusao <- transfusao_0 |> 
  mutate(tipo = case_when(
    
    tipo == "ch" ~ "Hemácias",
    tipo == "crio" ~ "Crioprecipitado",
    tipo == "plaq" ~ "Plaquetas",
    tipo == "plasma" ~ "Plasma"
    
  )) |> 
  ggplot(aes(x = tipo, y = qtd, color = tipo)) +
  geom_boxplot() +
  geom_jitter(width = .2) +
  xlab("Tipo") + ylab("Unidades") +
  labs(title = "Unidades de transfusão") +
  theme(plot.title = element_text(hjust = .5), legend.position = "none")

### Dispersão de complicações agudas

complicacoes_agudas_total <- cadav |> 
  select(complicacoes_agudas) |> 
  mutate(total = total_cadav) |> 
  separate_rows(complicacoes_agudas, sep = "\\s*,\\s*") |> 
  count(complicacoes_agudas, total, sort = TRUE) |> 
  mutate(p = n/total)

complicacoes_agudas_total_sim <- cadav |> 
  select(complicacoes_agudas) |> 
  filter(!complicacoes_agudas == "nao") |> 
  mutate(total = length(complicacoes_agudas)) |> 
  separate_rows(complicacoes_agudas, sep = "\\s*,\\s*") |>
  group_by(total) |> 
  count(complicacoes_agudas, sort = TRUE) |> 
  ungroup() |> 
  mutate(p = n/total)

complicacoes_agudas_total_sim_outro <- complicacoes_agudas_total_sim |> 
  mutate(complicacoes_agudas = ifelse(n == 1, "outro", complicacoes_agudas)) |> 
  group_by(complicacoes_agudas, total) |> 
  summarise(n = sum(n),
            p = sum(p)) |> 
  ungroup()

### Dispersão de necessidade de reoperação

reop <- cadav |> 
  count(reop, causa_reop) |> 
  mutate(total = total_cadav,
         p = n/total) |> 
  group_by(reop) |> 
  mutate(total_sim = sum(n),
         p_sim = n/total_sim) |> 
  ungroup()

### Dispersão de complicações tardias

complicacoes_tardias <- cadav |> 
  select(complicacoes_tardias) |> 
  separate_rows(complicacoes_tardias, sep = "\\s*,\\s*") |> 
  count(complicacoes_tardias) |> 
  mutate(complicacoes_tardias = ifelse(complicacoes_tardias == "cronica", "rejeicao cronica", complicacoes_tardias)) |> 
  group_by(complicacoes_tardias) |> 
  summarise(n = sum(n)) |> 
  arrange(desc(n)) |> 
  mutate(total = total_cadav,
         p = n/total)

complicacoes_tardias_sim <- cadav |> 
  select(complicacoes_tardias) |> 
  filter(!complicacoes_tardias == "nao") |>
  mutate(total = length(complicacoes_tardias)) |> 
  separate_rows(complicacoes_tardias, sep = "\\s*,\\s*") |> 
  count(complicacoes_tardias, total, sort = TRUE) |> 
  mutate(p = n/total)

complicacoes_tardias_sim_outro <- complicacoes_tardias_sim |> 
  mutate(complicacoes_tardias = ifelse(n == 1, "outro", complicacoes_tardias)) |> 
  group_by(complicacoes_tardias, total) |> 
  summarise(n = sum(n),
            p = sum(p)) |> 
  ungroup()

### Dispersão de rejeições

rejeicao <- cadav |> 
  select(rejeicao) |> 
  separate_rows(rejeicao, sep = "\\s*,\\s*") |> 
  count(rejeicao) |> 
  mutate(total = total_cadav,
         p = n/total,
         sim = !rejeicao == "nao") |> 
  group_by(sim) |> 
  mutate(total_sim = ifelse(sim, sum(n), NA),
         p_sim = n/total_sim) |> 
  ungroup()

plot_rejeicao <- rejeicao |> 
  transmute(
    Rejeição = case_when(
      
      rejeicao == "aguda" ~ "Aguda",
      rejeicao == "cronica" ~ "Crônica",
      TRUE ~ "Não"
      
    ),
    Total = n,
    tt = paste0(n, "\n", percent(p, .01))
  ) |> 
  ggplot(aes(x = Rejeição, y = Total, fill = Rejeição)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = tt), vjust = 2) +
  xlab("") + ylab("Total") +
  labs(title = "Rejeição") +
  theme(plot.title = element_text(hjust = .5), legend.position = "none")

### Dispersão de re-transplante

re_tx <- cadav |> 
  count(re_tx, causa_re_tx) |> 
  mutate(total = total_cadav,
         p = n/total) |> 
  group_by(re_tx) |> 
  mutate(total_sim = ifelse(re_tx == "sim", sum(n), NA),
         p_sim = n/total_sim) |> 
  ungroup()


# Análise Descritiva Qualidade de Vida ------------------------------------

## Resumo

sumario_qv <- cadav |> 
  summarise_at(vars(egs, cf, af, ae, as, dor, vt, sm),
               list(~mean(., na.rm = TRUE), ~sd(., na.rm = TRUE), ~median(., na.rm = TRUE), ~min(., na.rm = TRUE), ~max(., na.rm = TRUE))) |> 
  pivot_longer(everything()) |> 
  mutate(name = str_replace(name, "(_)([a-z]+$)", ".\\2")) |> 
  separate(name, into = c("dominio", "estatistica"), sep = "\\.") |> 
  pivot_wider(names_from = estatistica, values_from = value) |> 
  rename(media = mean, dp = sd, mediana = median)

plot_qv <- cadav |> 
  select(egs, cf, af, ae, as, dor, vt, sm) |> 
  pivot_longer(everything()) |> 
  mutate(name = ifelse(name == "dor", "Dor", str_to_upper(name)),
         name = factor(name, levels = unique(name))) |> 
  ggplot(aes(x = name, y = value, color = name)) +
  geom_boxplot() +
  geom_jitter(width = .1) +
  xlab("Domínio") + ylab("Score") +
  labs(title = "Distribuição domínios SF-36") +
  theme(plot.title = element_text(hjust = .5), legend.position = "none")

## QV vs Idade

plot_qv_idade <- cadav |> 
  mutate(idade_cat = ifelse(idade_no_tx < 50, "< 50", ">= 50")) |> 
  select(idade_cat, egs, cf, af, ae, as, dor, vt, sm) |> 
  pivot_longer(-idade_cat) |> 
  mutate(name = ifelse(name == "dor", "Dor", str_to_upper(name))) |> 
  ggplot(aes(x = idade_cat, y = value, color = idade_cat)) +
  geom_boxplot() +
  geom_jitter(width = .1) +
  facet_wrap(~name, scales = "free") + 
  xlab("Recorte de idade") + ylab("Score") +
  labs(title = "Distribuição domínios SF-36", subtitle = "Por recorte de idade") +
  theme(plot.title = element_text(hjust = .5), plot.subtitle = element_text(hjust = .5), legend.position = "none") +
  scale_y_continuous(limits = c(-1, 101), breaks = seq(0, 100, 20))

sumario_idade_0 <- cadav |> 
  mutate(idade_cat = ifelse(idade_no_tx < 50, "< 50", ">= 50")) |> 
  group_by(idade_cat) |> 
  summarise_at(vars(egs, cf, af, ae, as, dor, vt, sm),
               list(~mean(., na.rm = TRUE), ~sd(., na.rm = TRUE), ~median(., na.rm = TRUE), ~min(., na.rm = TRUE), ~max(., na.rm = TRUE))) |> 
  pivot_longer(-idade_cat) |> 
  mutate(name = str_replace(name, "(_)([a-z]+$)", ".\\2")) |> 
  separate(name, into = c("dominio", "estatistica"), sep = "\\.") |> 
  pivot_wider(names_from = estatistica, values_from = value)

sumario_idade <- sumario_idade_0 |> 
  filter(idade_cat == "< 50") |> 
  select(-idade_cat) |> 
  left_join(sumario_idade_0 |> 
              filter(idade_cat == ">= 50") |> 
              select(-idade_cat) |> 
              rename_at(vars(-dominio), ~paste0(., "_50")), by = "dominio") |> 
  select(dominio, mean, mean_50, sd, sd_50, median, median_50, min, min_50, max, max_50) |> 
  pivot_longer(-dominio) |> 
  separate(name, into = c("medida", "recorte"), sep = "_") |> 
  mutate(recorte = ifelse(is.na(recorte), "< 50", ">= 50"),
         recorte = case_when(
           
           medida == "sd" ~ paste0(" ", recorte, " "),
           medida == "median" ~ paste0("  ", recorte, "  "),
           medida == "min" ~ paste0("   ", recorte, "   "),
           medida == "max" ~ paste0("     ", recorte, "     "),
           TRUE ~ recorte
           
         ))
  

## QV vs Gênero

plot_qv_genero <- cadav |> 
  select(sexo, egs, cf, af, ae, as, dor, vt, sm) |> 
  pivot_longer(-sexo) |> 
  mutate(name = ifelse(name == "dor", "Dor", str_to_upper(name)) ,
         sexo = str_to_upper(sexo)) |> 
  ggplot(aes(x = sexo, y = value, color = sexo)) +
  geom_boxplot() +
  geom_jitter(width = .1) +
  facet_wrap(~name, scales = "free") + 
  xlab("Gênero") + ylab("Score") +
  labs(title = "Distribuição domínios SF-36", subtitle = "Por gênero") +
  theme(plot.title = element_text(hjust = .5), plot.subtitle = element_text(hjust = .5), legend.position = "none")

sumario_genero_0 <- cadav |> 
  group_by(sexo) |> 
  summarise_at(vars(egs, cf, af, ae, as, dor, vt, sm),
               list(~mean(., na.rm = TRUE), ~sd(., na.rm = TRUE), ~median(., na.rm = TRUE), ~min(., na.rm = TRUE), ~max(., na.rm = TRUE))) |> 
  pivot_longer(-sexo) |> 
  mutate(name = str_replace(name, "(_)([a-z]+$)", ".\\2")) |> 
  separate(name, into = c("dominio", "estatistica"), sep = "\\.") |> 
  pivot_wider(names_from = estatistica, values_from = value)

sumario_genero <- sumario_genero_0 |> 
  filter(sexo == "f") |> 
  select(-sexo) |> 
  left_join(sumario_genero_0 |> 
              filter(sexo == "m") |> 
              select(-sexo) |> 
              rename_at(vars(-dominio), ~paste0(., "_M")), by = "dominio") |> 
  select(dominio, mean, mean_M, sd, sd_M, median, median_M, min, min_M, max, max_M) |> 
  pivot_longer(-dominio) |> 
  separate(name, into = c("medida", "genero"), sep = "_") |> 
  mutate(genero = ifelse(is.na(genero), "F", "M"),
         genero = case_when(
           
           medida == "sd" ~ paste0(" ", genero, " "),
           medida == "median" ~ paste0("  ", genero, "  "),
           medida == "min" ~ paste0("   ", genero, "   "),
           medida == "max" ~ paste0("     ", genero, "     "),
           TRUE ~ genero
           
         ))

## QV vs Complicações Tardias

plot_qv_complicacoes_tardias <- cadav |> 
  mutate(complicacoes_tardias_cat = ifelse(complicacoes_tardias == "nao", "Não", "Sim")) |> 
  select(complicacoes_tardias_cat, egs, cf, af, ae, as, dor, vt, sm) |> 
  pivot_longer(-complicacoes_tardias_cat) |> 
  mutate(name = ifelse(name == "dor", "Dor", str_to_upper(name))) |> 
  ggplot(aes(x = complicacoes_tardias_cat, y = value, color = complicacoes_tardias_cat)) +
  geom_boxplot() +
  geom_jitter(width = .1) +
  facet_wrap(~name, scales = "free") + 
  xlab("Presença de complicações") + ylab("Score") +
  labs(title = "Distribuição domínios SF-36", subtitle = "Por presença de complicações tardias") +
  theme(plot.title = element_text(hjust = .5), plot.subtitle = element_text(hjust = .5), legend.position = "none")

sumario_complicacoes_tardias_0 <- cadav |> 
  mutate(complicacoes_tardias_cat = ifelse(complicacoes_tardias == "nao", "Não", "Sim")) |> 
  group_by(complicacoes_tardias_cat) |> 
  summarise_at(vars(egs, cf, af, ae, as, dor, vt, sm),
               list(~mean(., na.rm = TRUE), ~sd(., na.rm = TRUE), ~median(., na.rm = TRUE), ~min(., na.rm = TRUE), ~max(., na.rm = TRUE))) |> 
  pivot_longer(-complicacoes_tardias_cat) |> 
  mutate(name = str_replace(name, "(_)([a-z]+$)", ".\\2")) |> 
  separate(name, into = c("dominio", "estatistica"), sep = "\\.") |> 
  pivot_wider(names_from = estatistica, values_from = value)

sumario_complicacoes_tardias <- sumario_complicacoes_tardias_0 |> 
  filter(complicacoes_tardias_cat == "Sim") |> 
  select(-complicacoes_tardias_cat) |> 
  left_join(sumario_complicacoes_tardias_0 |> 
              filter(complicacoes_tardias_cat == "Não") |> 
              select(-complicacoes_tardias_cat) |> 
              rename_at(vars(-dominio), ~paste0(., "_nao")), by = "dominio") |> 
  select(dominio, mean, mean_nao, sd, sd_nao, median, median_nao, min, min_nao, max, max_nao) |> 
  pivot_longer(-dominio) |> 
  separate(name, into = c("medida", "complicacoes"), sep = "_") |> 
  mutate(complicacoes = ifelse(is.na(complicacoes), "Sim", "Não"),
         complicacoes = case_when(
           
           medida == "sd" ~ paste0(" ", complicacoes, " "),
           medida == "median" ~ paste0("  ", complicacoes, "  "),
           medida == "min" ~ paste0("   ", complicacoes, "   "),
           medida == "max" ~ paste0("     ", complicacoes, "     "),
           TRUE ~ complicacoes
           
         ))

# Testes Qualidade de Vida ------------------------------------------------

## QV pacientes vs Médias

comparacao_qv <- sumario_qv |> 
  select(dominio, media) |> 
  left_join(media_sf36) |> 
  mutate(p_valor_br = mapply(function(dom, m) t.test(eval(parse(text = paste0("cadav$", dom))), mu = m, alternative = "two.sided")$p.value,
                             dominio, media_br),
         p_valor_sul = mapply(function(dom, m) t.test(eval(parse(text = paste0("cadav$", dom))), mu = m, alternative = "two.sided")$p.value,
                              dominio, media_sul))

## QV Recorte de Idade

idade_50 <- cadav |> 
  filter(idade_no_tx < 50) |> 
  select(egs, cf, af, ae, as, dor, vt, sm)

idade_50_ <- cadav |> 
  filter(idade_no_tx >= 50) |> 
  select(egs, cf, af, ae, as, dor, vt, sm)

comparacao_idade <- sumario_idade_0 |> 
  mutate(stat = paste0(number(mean, .1), " ± ", number(sd, .1))) |> 
  select(-c(mean, sd, median, min, max)) |> 
  pivot_wider(names_from = idade_cat, values_from = stat) |> 
  mutate(p_valor = sapply(dominio, function(x) t.test(eval(parse(text = paste0("idade_50$", x))),
                                                      eval(parse(text = paste0("idade_50_$", x))),
                                                      alternative = "two.sided")$p.value))

## QV Gênero

genero_m <- cadav |> 
  filter(sexo == "m") |> 
  select(egs, cf, af, ae, as, dor, vt, sm)

genero_f <- cadav |> 
  filter(sexo == "f") |> 
  select(egs, cf, af, ae, as, dor, vt, sm)

comparacao_genero <- sumario_genero_0 |> 
  mutate(stat = paste0(number(mean, .1), " ± ", number(sd, .1)),
         sexo = str_to_upper(sexo)) |> 
  select(-c(mean, sd, median, min, max)) |> 
  pivot_wider(names_from = sexo, values_from = stat) |> 
  mutate(p_valor = sapply(dominio, function(x) t.test(eval(parse(text = paste0("genero_m$", x))),
                                                      eval(parse(text = paste0("genero_f$", x))),
                                                      alternative = "two.sided")$p.value))

## QV Complicações Tardias

complicacoes_tardias_teste_sim <- cadav |> 
  filter(!complicacoes_tardias == "nao") |> 
  select(egs, cf, af, ae, as, dor, vt, sm)

complicacoes_tardias_teste_nao <- cadav |> 
  filter(complicacoes_tardias == "nao") |> 
  select(egs, cf, af, ae, as, dor, vt, sm)

comparacao_complicacoes_tardias <- sumario_complicacoes_tardias_0 |> 
  mutate(stat = paste0(number(mean, .1), " ± ", number(sd, .1))) |> 
  select(-c(mean, sd, median, min, max)) |> 
  pivot_wider(names_from = complicacoes_tardias_cat, values_from = stat) |> 
  mutate(p_valor = sapply(dominio, function(x) t.test(eval(parse(text = paste0("complicacoes_tardias_teste_sim$", x))),
                                                      eval(parse(text = paste0("complicacoes_tardias_teste_nao$", x))),
                                                      alternative = "two.sided")$p.value))
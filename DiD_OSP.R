# 1 - TRATAMENTO DOS DADOS

library(dplyr)
library(did)
library(zoo)
library(tidyr)
library(ggplot2)
library(lubridate)
library(purrr)
library(stringr)

# 1.1) Importar dados do ISP.
url_crimes  <- "https://www.ispdados.rj.gov.br/Arquivos/BaseDPEvolucaoMensalCisp.csv"
dados_isp   <- read.csv(url_crimes, header = TRUE, stringsAsFactors = FALSE , sep=';')

url_pop   <- "https://www.ispdados.rj.gov.br/Arquivos/PopulacaoEvolucaoMensalCisp.csv"
dados_pop <- read.csv(url_pop, header = TRUE, stringsAsFactors = FALSE , sep=';')

# url_area    <- "https://www.ispdados.rj.gov.br/Arquivos/AreasemKm.csv"
# dados_area  <- read.csv(url_area, header = TRUE, stringsAsFactors = FALSE , sep=';')

# 1.2) Ajustar coluna de datas e selecionar recorte temporal desejado.
dados_isp <- dados_isp %>% mutate(mes_ano = as.Date(as.yearmon(as.character(mes_ano), "%Ym%m")), cisp = as.integer(cisp))
df <- dados_isp %>% filter(mes_ano >= as.Date("2010-01-01") & as.Date(mes_ano) < "2022-7-31")

# 1.3) Organizar dados de população e área (Km2).
# dados_area <- dados_area %>% mutate(area_cisp_num = str_replace_all(area_cisp_km2, ",", "."),
#                                    area_cisp_num = as.numeric(area_cisp_num)) %>%  select(cisp, area = area_cisp_num)

dados_pop <- dados_pop %>% mutate(pop_circ_num = str_replace_all(pop_circ, ",", "."),
                                  pop_circ_num = as.numeric(pop_circ_num))

dados_pop <- dados_pop %>% select(cisp = circ, mes, ano, pop = pop_circ_num)

# 1.4) Integrar bases de crime, população e área.
df <- df %>% left_join(dados_pop, by = c("cisp", "mes", "ano")) %>% mutate(pop = as.integer(pop))
         #%>% left_join(dados_area, by = "cisp")
         

# 1.5) Selecionar colunas desejadas.
df <- df %>% select(cisp , mes_ano, roubo_rua, roubo_veiculo, total_roubos, total_furtos, pop)

# 1.6) Balancear painel atribuindo 'NA' para meses em que CISPs ainda não existiam.
todos_meses <- seq.Date(min(df$mes_ano), max(df$mes_ano), by = "month")
df <- df %>% group_by(cisp) %>% complete(mes_ano = todos_meses) %>%  ungroup() %>% arrange(cisp, mes_ano)

# 1.7) Criar índice t para cada mês analisado.
calend <- tibble::tibble(mes_ano = sort(unique(df$mes_ano))) %>%  mutate(t = dplyr::row_number())
df <- df %>% left_join(calend, by = "mes_ano") %>% arrange(cisp, mes_ano)

# 1.8) Agregar dados de regiões/bairros que se dividem em múltiplas CISP.
# São três casos: CISP 12 + 13 = Copacabana / CISP 65 + 66 = Magé / CISP 79 + 81 = Niterói (Região Oceânica).
# A partir de agora: dados da CISP 12 representam dados das CISP 12 + 13, etc.
df <- df %>% mutate(cisp = dplyr::case_when(cisp %in% c(12L, 13L) ~ 12L,
      cisp %in% c(65L, 66L) ~ 65L, cisp %in% c(79L, 81L) ~ 79L,TRUE ~ cisp))

df <- df %>% group_by(cisp, mes_ano, t) %>% summarise(
    roubo_rua = { x <- roubo_rua; if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE) },
    roubo_veiculo = { x <- roubo_veiculo; if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE) },
    total_roubos = { x <- total_roubos; if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE) },
    total_furtos = { x <- total_furtos; if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE) },
    pop = { x <- pop; if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE) },
#    area = { x <- area; if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE) },
    .groups = "drop"
  ) %>%  arrange(cisp, t)

# 1.9) Montar tabela com CISPs tratadas e datas de início do tratamento.
# Há CISPs com mais de uma base do OSP. Nesses casos, usamos data da primeira inauguração.
cisps = c(1, 5, 9, 10, 12, 14, 15, 16, 18, 20, 23, 34, 35, 37, 42, 44, 48, 50,
          51, 52, 53, 55, 57, 58, 59, 63, 65, 67, 71, 72, 76, 77, 79, 88, 93,
          96, 105, 108, 110, 123, 134)

datas_inaug = c("2016-07-04", "2014-01-01", "2015-12-01", "2019-10-04",
                "2017-12-03", "2018-12-20", "2015-12-01", "2019-11-29",
                "2019-01-03", "2019-12-20", "2015-12-01", "2019-09-20",
                "2024-12-06", "2023-12-01", "2019-12-19", "2024-08-15",
                "2021-11-22", "2020-08-12", "2021-04-07", "2019-08-16",
                "2023-05-29", "2022-01-14", "2023-01-12", "2019-10-30",
                "2019-11-14", "2021-06-01", "2020-08-12", "2024-12-09",
                "2022-01-14", "2020-01-24", "2017-12-15", "2017-12-15",
                "2020-10-22", "2022-06-02", "2022-06-02", "2024-03-22",
                "2023-11-01", "2022-03-30", "2022-03-30", "2022-03-25",
                "2022-01-15")

datas_inicio <- tibble::tibble(cisp = cisps, inicio = as.Date(datas_inaug))
datas_inicio <- datas_inicio %>% mutate(inicio = floor_date(as.Date(inicio), unit='month'))

# 1.10) Mapear datas de início g em períodos de tempo t.
# CISPs nunca tratadas recebem g = 0.
map_t <- setNames(calend$t, calend$mes_ano)
datas_inicio <- datas_inicio %>% mutate(g = as.integer(map_t[as.character(inicio)]),
                                        g = dplyr::coalesce(g, 0L))

df <- df %>% dplyr::left_join(datas_inicio %>% dplyr::select(cisp, g), by = "cisp") %>%
  mutate(g = dplyr::coalesce(g, 0L)) %>% arrange(cisp, t)

# 1.11) Excluir dados de CISPs com NA
cisp_com_na <- df %>% filter(is.na(total_roubos)) %>% pull(cisp) %>% unique()
df <- df %>% filter(!cisp %in% cisp_com_na)
colSums(is.na(df))

# 1.12) Transformamos variáveis em logaritmo
df_log <- df %>% mutate(roubo_rua = log(roubo_rua + 1), roubo_veiculo = log(roubo_veiculo + 1),
    total_roubos = log(total_roubos + 1), total_furtos = log(total_furtos + 1) ,pop = log(pop))
# area = log(area)




# 2 - ESTIMAÇÃO DO MODELO DIFF IN DIFF ESCALONADO - CALLAWAY & SANTANNA (2021)

# 2.1) Rodar Diff-in-Diff
ctrl <- "nevertreated" # pode ser "nevertreated" ou "notyettreated"
est  <- "reg"

out_roubo <- att_gt(
  yname="total_roubos", tname="t", idname="cisp", gname="g", xformla=~1, data=df_log, est_method=est,
  control_group = ctrl, panel=TRUE)

out_furto <- att_gt(
  yname="total_furtos", tname="t", idname="cisp", gname="g", xformla=~1, data=df_log, est_method=est,
  control_group = ctrl, panel=TRUE
)

out_roubo_rua <- att_gt(
  yname="roubo_rua", tname="t", idname="cisp", gname="g", xformla=~1, data=df_log, est_method=est, 
  control_group = ctrl, panel=TRUE
)

out_roubo_veiculo <- att_gt(
  yname="roubo_veiculo", tname="t", idname="cisp", gname="g", xformla=~1, data=df_log, est_method=est, 
  control_group = ctrl, panel=TRUE
)

# 2.2) Preparar estudos de evento
t_min = -36;
t_max = 60;
es_roubo <- aggte(out_roubo, type="dynamic", min_e=t_min, max_e=t_max)
es_furto <- aggte(out_furto, type="dynamic", min_e=t_min, max_e=t_max)
es_roubo_rua  <- aggte(out_roubo_rua, type="dynamic", min_e=t_min, max_e=t_max)
es_roubo_veiculo  <- aggte(out_roubo_veiculo, type="dynamic", min_e=t_min, max_e=t_max)

# 2.3) Extrair dados para montagem dos gráficos
df_roubo <- tibble(e = es_roubo$egt, att = es_roubo$att.egt, se  = es_roubo$se.egt, serie = "Total de Roubos") %>%
            filter(e >= t_min, e <= t_max) %>% mutate(lwr = att - 1.96*se, upr = att + 1.96*se)

df_furto <- tibble(e = es_furto$egt, att = es_furto$att.egt, se  = es_furto$se.egt, serie = "Total de Furtos") %>%
            filter(e >= t_min, e <= t_max) %>% mutate(lwr = att - 1.96*se, upr = att + 1.96*se)

df_roubo_rua <- tibble(e = es_roubo_rua$egt, att = es_roubo_rua$att.egt, se  = es_roubo_rua$se.egt, serie = "Roubos de Rua") %>%
  filter(e >= t_min, e <= t_max) %>% mutate(lwr = att - 1.96*se, upr = att + 1.96*se)

df_roubo_veiculo <- tibble(e = es_roubo_veiculo$egt, att = es_roubo_veiculo$att.egt, se  = es_roubo_veiculo$se.egt, serie = "Roubos de Veículo") %>%
  filter(e >= t_min, e <= t_max) %>% mutate(lwr = att - 1.96*se, upr = att + 1.96*se)

# 3 - APRESENTAÇÃO DE RESULTADOS
# 3.1) Plot dos gráficos dos estudos de evento
df_roubo2 <- df_roubo %>%
  dplyr::mutate(periodo = ifelse(e < 0, "Pré", "Pós"))

ggplot(df_roubo2, aes(x = e, y = att, color = periodo)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 3) +
  geom_linerange(aes(ymin = lwr, ymax = upr), linewidth = 0.6, alpha = 0.7) + 
  geom_point(size = 1.8) + 
  scale_x_continuous(breaks = seq(t_min, t_max, by = 6)) +
  scale_color_manual(values = c("Pré" = "#2C7FB8", "Pós" = "#D95F0E")) +
  labs(x = "Meses desde o tratamento", y = "ATT", title = "Total de Roubos") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        legend.title = element_blank())

df_furto2 <- df_furto %>%
  dplyr::mutate(periodo = ifelse(e < 0, "Pré", "Pós"))

ggplot(df_furto2, aes(x = e, y = att, color = periodo)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 3) +
  geom_linerange(aes(ymin = lwr, ymax = upr), linewidth = 0.6, alpha = 0.7) +
  geom_point(size = 1.8) +  
  scale_x_continuous(breaks = seq(t_min, t_max, by = 6)) +
  scale_color_manual(values = c("Pré" = "#2C7FB8", "Pós" = "#D95F0E")) +
  labs(x = "Meses desde o tratamento", y = "ATT", title = "Total de Furtos") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        legend.title = element_blank())

df_roubo_rua2 <- df_roubo_rua %>%
  dplyr::mutate(periodo = ifelse(e < 0, "Pré", "Pós"))

ggplot(df_roubo_rua2, aes(x = e, y = att, color = periodo)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 3) +
  geom_linerange(aes(ymin = lwr, ymax = upr), linewidth = 0.6, alpha = 0.7) + 
  geom_point(size = 1.8) +  
  scale_x_continuous(breaks = seq(t_min, t_max, by = 6)) +
  scale_color_manual(values = c("Pré" = "#2C7FB8", "Pós" = "#D95F0E")) +
  labs(x = "Meses desde o tratamento", y = "ATT", title = "Roubos de Rua") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        legend.title = element_blank())

df_roubo_veiculo2 <- df_roubo_veiculo %>%
  dplyr::mutate(periodo = ifelse(e < 0, "Pré", "Pós"))

ggplot(df_roubo_veiculo2, aes(x = e, y = att, color = periodo)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 3) +
  geom_linerange(aes(ymin = lwr, ymax = upr), linewidth = 0.6, alpha = 0.7) +  
  geom_point(size = 1.8) +  
  scale_x_continuous(breaks = seq(t_min, t_max, by = 6)) +
  scale_color_manual(values = c("Pré" = "#2C7FB8", "Pós" = "#D95F0E")) +
  labs(x = "Meses desde o tratamento", y = "ATT", title = "Roubos de Veículos") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        legend.title = element_blank())


# 3.2) Cálculo de significância do ATT médio para diferentes períodos
es_roubo <- aggte(out_roubo, type = "dynamic", min_e = 1, max_e = t_max)
summary(es_roubo) 

es_furto <- aggte(out_furto, type = "dynamic", min_e = 1, max_e = t_max)
summary(es_furto)

es_roubo_rua <- aggte(out_roubo_rua, type = "dynamic", min_e = 1, max_e = t_max)
summary(es_roubo_rua)

es_roubo_veiculo <- aggte(out_roubo_veiculo, type = "dynamic", min_e = 1, max_e = t_max)
summary(es_roubo_veiculo)

# Magnitude econômica dos efeitos
calcular_crimes_evitados <- function(att_pct, crime_medio_pre, n_cisps, meses) {
  crimes_evitados_mes <- crime_medio_pre * abs(att_pct) * n_cisps
  crimes_evitados_total <- crimes_evitados_mes * meses
  list(por_mes = crimes_evitados_mes, total = crimes_evitados_total)
}

crime_pre_medio <- df %>%
  filter(g > 0 & g > 108) %>%  # Coorte 2019+ (onde há efeitos)
  left_join(datas_inicio %>% select(cisp, inicio), by = "cisp") %>%
  filter(mes_ano < inicio) %>%
  summarise(
    rr = mean(roubo_rua, na.rm = TRUE),
    rv = mean(roubo_veiculo, na.rm = TRUE),
    tr = mean(total_roubos, na.rm = TRUE)
  )

n_cisps_pos2019 <- sum(df$g > 108 & df$g > 0, na.rm = TRUE) / 151  # 151 períodos

mag_rr_ano1 <- calcular_crimes_evitados(-0.09, crime_pre_medio$rr, n_cisps_pos2019, 12)
mag_rv_ano1 <- calcular_crimes_evitados(-0.12, crime_pre_medio$rv, n_cisps_pos2019, 12)
mag_tr_ano1 <- calcular_crimes_evitados(-0.10, crime_pre_medio$tr, n_cisps_pos2019, 12)

mag_rr_2anos <- calcular_crimes_evitados(-0.09, crime_pre_medio$rr, n_cisps_pos2019, 24)
mag_rv_2anos <- calcular_crimes_evitados(-0.12, crime_pre_medio$rv, n_cisps_pos2019, 24)

# 4 - SEPARAÇÃO POR COORTES
# Vamos separar a análise para dois coortes: um que compreende as unidades inauguradas até 2019
# e outro que compreende as unidades implementadas de 2019 em diante.

# 4.1) Definimos os coortes (tratados até 2018 + nunca tratados) e (tratados após 2018 + nunca tratados)
g_limite <- 108
df_pre2019 <- subset(df_log, g == 0 | (g > 0 & g <= g_limite))
df_pos2019 <- subset(df_log, g == 0 | g > g_limite)

# 4.2) Rodamos o modelo para cada um dos crimes e cada um dos coortes

# 4.2.1) Roubos de Rua
out_pre2019_rua <- att_gt(yname="roubo_rua", tname="t", idname="cisp", gname="g",xformla=~1,
                          data=df_pre2019, est_method="reg", control_group="nevertreated", panel=TRUE)

out_pos2019_rua <- att_gt(yname="roubo_rua", tname="t", idname="cisp", gname="g", xformla=~1,
                          data=df_pos2019, est_method="reg",  control_group="nevertreated", panel=TRUE)

# 4.2.2) Roubos de Veíulo
out_pre2019_veiculo <- att_gt(yname="roubo_veiculo", tname="t", idname="cisp", gname="g", xformla=~1,
                              data=df_pre2019, est_method="reg", control_group="nevertreated", panel=TRUE)

out_pos2019_veiculo <- att_gt(yname="roubo_veiculo", tname="t", idname="cisp", gname="g", xformla=~1,
                              data=df_pos2019, est_method="reg",  control_group="nevertreated", panel=TRUE)

# 4.2.3) Total de Roubos
out_pre2019_roubos <- att_gt(yname="total_roubos", tname="t", idname="cisp", gname="g", xformla=~1,
                             data=df_pre2019, est_method="reg", control_group="nevertreated", panel=TRUE)

out_pos2019_roubos <- att_gt(yname="total_roubos", tname="t", idname="cisp", gname="g", xformla=,
                             data=df_pos2019, est_method="reg", control_group="nevertreated", panel=TRUE)

# 4.2.4) Total de Furtos
out_pre2019_furtos <- att_gt(yname="total_furtos", tname="t", idname="cisp", gname="g", xformla=~1,
                             data=df_pre2019, est_method="reg", control_group="nevertreated", panel=TRUE)

out_pos2019_furtos <- att_gt(yname="total_furtos", tname="t", idname="cisp", gname="g", xformla=~1,
                             data=df_pos2019, est_method="reg", control_group="nevertreated", panel=TRUE)

# 4.5.5) Comparar características observáveis dos coortes
df_comp <- df %>%
  left_join(datas_inicio %>% select(cisp, inicio), by = "cisp") %>%
  mutate(coorte = case_when(
    is.na(inicio) ~ "Controle",
    floor_date(inicio, "month") <= as.Date("2018-12-01") ~ "Até 2018",
    TRUE ~ "2019+"
  ))

stats_cisp <- df_comp %>%
  filter(is.na(inicio) | mes_ano < inicio) %>%
  group_by(cisp, coorte) %>%
  summarise(
    pop = mean(pop, na.rm = TRUE),
    rr = mean(roubo_rua, na.rm = TRUE),
    rv = mean(roubo_veiculo, na.rm = TRUE),
    tr = mean(total_roubos, na.rm = TRUE),
    tf = mean(total_furtos, na.rm = TRUE),
    .groups = "drop"
  )

tab_coortes <- stats_cisp %>%
  group_by(coorte) %>%
  summarise(N = n(), across(pop:tf, ~mean(., na.rm = TRUE)), .groups = "drop") %>%
  mutate(across(where(is.numeric) & !N, ~round(., 1)), pop = pop/1000)

ate18 <- stats_cisp %>% filter(coorte == "Até 2018")
pos19 <- stats_cisp %>% filter(coorte == "2019+")

tt <- function(x, y) tryCatch(t.test(x, y)$p.value, error = function(e) NA)

pvals <- tibble(
  var = c("pop", "rr", "rv", "tr", "tf"),
  p = c(tt(ate18$pop, pos19$pop), tt(ate18$rr, pos19$rr), 
        tt(ate18$rv, pos19$rv), tt(ate18$tr, pos19$tr), tt(ate18$tf, pos19$tf))
) %>%
  mutate(sig = case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ ""))

tab_final <- tab_coortes %>%
  bind_rows(tibble(coorte = "p-valor", N = NA, pop = pvals$p[1], rr = pvals$p[2], 
                   rv = pvals$p[3], tr = pvals$p[4], tf = pvals$p[5]))

print(tab_final)
print(pvals)


# 4.3) Montamos os Estudos de Evento
es_pre2019_rua  <- aggte(out_pre2019_rua, type="dynamic", min_e=t_min, max_e=36)
es_pos2019_rua  <- aggte(out_pos2019_rua,  type="dynamic", min_e=t_min, max_e=36)

es_pre2019_veiculo  <- aggte(out_pre2019_veiculo, type="dynamic", min_e=t_min, max_e=36)
es_pos2019_veiculo  <- aggte(out_pos2019_veiculo, type="dynamic", min_e=t_min, max_e=36)

es_pre2019_roubos  <- aggte(out_pre2019_roubos, type="dynamic", min_e=t_min, max_e=36)
es_pos2019_roubos  <- aggte(out_pos2019_roubos, type="dynamic", min_e=t_min, max_e=36)

es_pre2019_furtos  <- aggte(out_pre2019_furtos, type="dynamic", min_e=t_min, max_e=36)
es_pos2019_furtos  <- aggte(out_pos2019_furtos, type="dynamic", min_e=t_min, max_e=36)

# 4.4) Ordenamos os dataframes para produção dos gráficos
df_rua <- dplyr::bind_rows( tibble::tibble(e = es_pre2019_rua$egt, att = es_pre2019_rua$att.egt,
                 se  = es_pre2019_rua$se.egt,coorte = "Até 2018"), tibble::tibble(e = es_pos2019_rua$egt,
                 att = es_pos2019_rua$att.egt, se  = es_pos2019_rua$se.egt, coorte = "2019 em diante")) |>
                 dplyr::mutate(lwr = att - 1.96*se,upr = att + 1.96*se)

df_veiculo <- dplyr::bind_rows(tibble::tibble(e = es_pre2019_veiculo$egt, att = es_pre2019_veiculo$att.egt,
                 se  = es_pre2019_veiculo$se.egt, coorte = "Até 2018"), tibble::tibble(e = es_pos2019_veiculo$egt,
                 att = es_pos2019_veiculo$att.egt, se  = es_pos2019_veiculo$se.egt, coorte = "2019 em diante")) |>
                 dplyr::mutate(lwr = att - 1.96*se, upr = att + 1.96*se)

df_total_roubos <- dplyr::bind_rows(tibble::tibble(e = es_pre2019_roubos$egt, att = es_pre2019_roubos$att.egt,
                se  = es_pre2019_roubos$se.egt, coorte = "Até 2018"), tibble::tibble(e = es_pos2019_roubos$egt, att = es_pos2019_roubos$att.egt,
                se  = es_pos2019_roubos$se.egt, coorte = "2019 em diante")) |>
                dplyr::mutate(lwr = att - 1.96*se,upr = att + 1.96*se)

df_total_furtos <- dplyr::bind_rows(tibble::tibble(e = es_pre2019_furtos$egt, att = es_pre2019_furtos$att.egt,
                 se  = es_pre2019_furtos$se.egt, coorte = "Até 2018"), tibble::tibble(e = es_pos2019_furtos$egt,
                 att = es_pos2019_furtos$att.egt, se  = es_pos2019_furtos$se.egt, coorte = "2019 em diante")) |>
                 dplyr::mutate(lwr = att - 1.96*se, upr = att + 1.96*se)

# 4.5) Gráficos das análises por coorte

# 4.5.1) Roubos de Rua
ggplot(df_rua, aes(x = e, y = att, color = coorte, fill = coorte)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 3) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = .15, color = NA) +
  geom_line(linewidth = .8) +
  geom_point(size = 1.6) +
  scale_x_continuous(breaks = seq(t_min, t_max, by = 6)) +
  labs(x = "Meses desde o tratamento", y = "ATT",
       title = "Roubos de rua – comparação de coortes") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = .5),
        legend.title = element_blank(),
        legend.position = "bottom")

# 4.5.2) Roubos de Veículos
ggplot(df_veiculo, aes(x = e, y = att, color = coorte, fill = coorte)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 3) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = .15, color = NA) +
  geom_line(linewidth = .8) +
  geom_point(size = 1.6) +
  scale_x_continuous(breaks = seq(t_min, t_max, by = 6)) +
  labs(x = "Meses desde o tratamento", y = "ATT",
       title = "Roubos de veículo – comparação de coortes") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = .5),
        legend.title = element_blank(),
        legend.position = "bottom")

# 4.5.3) Total de Roubos
ggplot(df_total_roubos, aes(x = e, y = att, color = coorte, fill = coorte)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 3) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = .15, color = NA) +
  geom_line(linewidth = .8) +
  geom_point(size = 1.6) +
  scale_x_continuous(breaks = seq(t_min, t_max, by = 6)) +
  labs(x = "Meses desde o tratamento", y = "ATT",
       title = "Total de roubos – comparação de coortes") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = .5),
        legend.title = element_blank(),
        legend.position = "bottom")

# 4.5.4) Total de Furtos
ggplot(df_total_furtos, aes(x = e, y = att, color = coorte, fill = coorte)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 3) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = .15, color = NA) +
  geom_line(size = .8) +
  geom_point(size = 1.6) +
  scale_x_continuous(breaks = seq(t_min, t_max, by = 6)) +
  labs(x = "Meses desde o tratamento", y = "ATT",
       title = "Total de furtos – comparação de coortes") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = .5),
        legend.title = element_blank(),
        legend.position = "bottom")



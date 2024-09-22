
# Cargar paquetes----
library(car)
library(MASS)
library(ggstats)
library(tidyverse)
library(ggpubr)
library(readxl)
library(lmerTest)
library(emmeans)
library(knitr)
library(kableExtra)
library(performance)
library(GGally)
library(scales) 
library(factoextra)
library(FactoMineR)
library(gtools)
library(bbmle)
library(effectsize)

# Investigadoras/es

## Milena Vásquez-Amézquita - https://orcid.org/0000-0001-7317-8430 
## Andrés Castellanos-Chacón - https://orcid.org/0000-0003-1684-9319 
## Wendy Medina-Sarmiento - NA
## Valentina Cepeda - NA
## Marina Begoña Martínez-González - https://orcid.org/0000-0002-5840-6383
## Juan David Leongómez - https://orcid.org/0000-0002-0092-6298


#### CREAR FUNCION PARA REPORTAR MODELOS INCLIYENDO R²!!!!!!!

# Functions----
corr.stars <- function(x) {
  require(Hmisc)
  x <- as.matrix(x)
  R <- rcorr(x)$r
  p <- rcorr(x)$P
  # define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, 
                    paste0("\\textbf{", round(R, 2), "***}"), 
                    ifelse(p < .01, 
                           paste0("\\textbf{", round(R, 2), "**}"), 
                           ifelse(p < .05, 
                                  paste0("\\textbf{", round(R, 2), "*}"),
                                  ifelse(p < .10,
                                         paste0(round(R, 2), "$^{\\dagger}$"),
                                         format(round(R, 2), nsmall = 2)))))
  # build a new matrix that includes the correlations with their appropriate stars
  Rnew <- matrix(mystars, 
                 ncol = ncol(x))
  diag(Rnew) <- paste(diag(R), " ", 
                      sep = "")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", 
                          sep = "")
  # remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew)
  # remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew) - 1])
  return(Rnew)
}

# External stimuli evaluation----
ext_val <- read_excel("Data/Evaluacion Manipulación Rostros.xlsx") 

masc_dat <- ext_val |> 
  select(ResponseId, 
         contains("M", ignore.case = FALSE), 
         -Menstruacion) |> 
  pivot_longer(cols = contains("M", ignore.case = FALSE),
               names_to = "Stimulus",
               values_to = "Masculinity") |> 
  mutate(Sexual_dimorphism = ifelse(grepl("f_1", Stimulus), "Feminine", "Masculine")) |> 
  mutate(Stimulus = str_sub(Stimulus, end = 3))

masc_dat |> 
  group_by(Stimulus) |> 
  summarise(t = t.test(Masculinity ~ Sexual_dimorphism)$statistic,
            p = t.test(Masculinity ~ Sexual_dimorphism)$p.value) |> 
  ungroup() |> 
  mutate(p.signif = stars.pval(p))

age_dat <- ext_val |> 
  select(ResponseId, 
         contains("E", ignore.case = FALSE)) |> 
  select(-c(2:5)) |> 
  pivot_longer(cols = contains("E", ignore.case = FALSE),
               names_to = "Stimulus",
               values_to = "Age") |> 
  mutate(Sexual_dimorphism = ifelse(grepl("f_1", Stimulus), "Feminine", "Masculine")) |> 
  mutate(Stimulus = str_sub(Stimulus, end = 3))

age_dat |> 
  summarise(Mean = mean(Age, na.rm = TRUE),
            SD = sd(Age, na.rm = TRUE),
            Min = min(Age, na.rm = TRUE),
            Max = max(Age, na.rm = TRUE))

ggplot(age_dat, aes(x = Age)) +
  geom_histogram(bins = 26, fill = "grey", color = "black") +
  labs(x = "Estimated Age",
       y = "Count") +
  scale_x_continuous(breaks = seq(15, 40, 5))

# Cargar datos----

## Eye-tracking----
dat_et <- read_excel("Data/BD-ET-CUC-UB.xlsx", 
                     sheet = "CUC-UB") |> 
  select(-c(Participant, Condicion, TOI, Interval, Media_respuesta, AOI, 
            AOI_Global, Respuesta, Number_of_mouse_clicks...17, 
            Time_to_first_mouse_click...18, AOI_respuesta)) |> 
  rename(ID = Recording,
         University = UNIVERSIDAD,
         Stimulus = Media,
         Condition = Condición,
         Relationship = Contexto,
         Sexual_dimorphism = Rostro,
         TFD = Total_duration_of_whole_fixations,
         NF = Number_of_whole_fixations,
         TFF = Time_to_first_whole_fixation,
         NMC = Number_of_mouse_clicks...21,
         TFMC = Time_to_first_mouse_click...22,
         DFF = Duration_first_fixation) |> 
  mutate(across(where(is.character), as.factor)) |>
  mutate(Condition = fct_recode(Condition, 
                                "Low" = "BAJA",
                                "High" = "ALTA"),
         Relationship = fct_recode(Relationship, 
                                   "Short term" = "CP",
                                   "Long term" = "LP"),
         Sexual_dimorphism = fct_recode(Sexual_dimorphism,
                                        "Feminized" = "Feminizado",
                                        "Masculinized" = "Masculinizado")) |>
  mutate(Stimulus = ifelse(Sexual_dimorphism == "Feminized", paste0(str_sub(str_replace(Stimulus, ".* - ", ""), 1, 2), "F"), 
                           ifelse(Sexual_dimorphism == "Masculinized", paste0(str_sub(str_replace(Stimulus, ".* - ", ""), 1, 2), "M"), 
                                  Stimulus)),
         Choice = ifelse(NMC == 0, "No", "Yes"))

## Cuestionarios----
### Sin calcular puntajes totales de instrumentos, para ver consistencia interna
quests <- read_excel("Data/Cuestionario Datos Sociodemográficos  (Disponibilidad) (respuestas) (1).xlsx", 
                     sheet = "Respuestas de formulario 1") |> 
  select(-c(Invitado, `Servicios ayuda`, `Correos cierre`)) |>
  rename(Date = Fecha,
         Age = edad,
         City = Ciudad,
         Education = Escolaridad,
         Ethnicity = Etnia,
         Gender = Sexo,
         Sex = Genero,
         Sexual_orientation = OS,
         Relationship_current = "Pareja actual",
         Relationship_duration = DuracionR,
         Relationship_status = EstadoR,
         Partner_sex = SexoParejaActual,
         Partner_masculinity = Masculinidad_pareja,
         Partner_dominance = Dominancia_pareja,
         Partner_attractiveness = Atractivo_pareja,
         Number_of_children = NumHijos,
         Hormonal_contraception = "Anticonceptivos hormonales",
         Contraceptive = Cual_anticonceptivo,
         Last_mentruation = "Ultima menstruacion",
         Currently_pregnant = "Embarazo actual",
         Sexual_abuse = "Experiencia abuso sexual",
         Comments = comentarios1,
         Medical_history = "antecedentes medicos",
         SP_happiness = "AP felicidad",
         SP_financial_security = "AP seguridad economica",
         SP_money_control = "AP control dinero",
         SP_attractiveness = "AP atractivo",
         SP_self_confidence = "AP autoconfianza",
         SP_self_esteem = "AP autoestima",              
         SP_health = "AP salud",
         Electricity = "SB electricidad",
         Internet_access = "SB internet",
         TV = "SB television",
         Internet_use = "Fr acceso internet",
         Hospital_access = "Acceso hospital",
         Freq_illness = "Fr enfermedades",
         Socioeconomic_level = "Estrato socioeconomico",
         Neighborhood = "Barrio de residencia",
         Perceived_neighborhood_safety= "Seguridad barrio",
         Perceived_city_safety = "Seguridad ciudad",
         Perceived_home_safety = "Seguridad hogar",
         Perceived_country_safety = "Seguridad país",
         Freq_robery = "Fr de robos",
         Men_perceived_as_danger_to_children = "Hombres peligrosos hijos",
         Men_perceived_as_danger_to_partner = "Hombres peligrosos pareja",
         Partner_physical_violence = "VP fisica",
         Freq_partner_physical_violence = "Fr VP fisica",
         Partner_sexual_violence = "VP sexual",
         Freq_partner_sexual_violence = "Fr VP sexual",
         Partner_infidelity = "Infidelidad",
         Freq_partner_infidelity = "Fr infidelidad",
         Victim_of_violence = "Victima de alguna violencia",
         Violence_type = "Tipo violencia",
         Victim_of_gender_violence = "Victima violencia género",
         Victim_of_armed_conflict = "Victima conflicto armado",
         Control_question_1 = "Sin leer",
         Control_question_2 = "Broma") |> 
  mutate(Education = factor(Education, levels = c("Primaria", 
                                                  "Bachillerato",
                                                  "Universitario", 
                                                  "Posgrado")),
         Sexual_orientation = factor(Sexual_orientation, 
                                     levels = c("Exclusivamente heterosexual",
                                                "Principalmente heterosexual, con contactos homosexuales esporádicos",
                                                "Predominantemente heterosexual, aunque con contactos homosexuales más que esporádicos",
                                                "Bisexual",                                                                             
                                                "Pansexual",                                                                            
                                                "Demisexual")), 
         Relationship_status = factor(Relationship_status,
                                      levels = c("Soltero sin contactos sexuales en el último año",
                                                 "Soltero con contactos sexuales en el último año",
                                                 "Relación exclusiva o matrimonio - viven juntos",
                                                 "Relación exclusiva - no viven juntos",
                                                 "Relación no exclusiva - contactos sexuales con otras personas")),
         Internet_use = factor(Internet_use,
                               levels = c("Cada día",
                                          "Cada mes",
                                          "Cada año")),
         Socioeconomic_level = as.factor(Socioeconomic_level),
         City = ifelse(City == "Bogotá D.C." | 
                         City == "Madrid, Cundinamarca" | 
                         City == "Zipaquirá, Cundinamarca" |
                         City == "Zipaquirá" | 
                         City == "Mosquera, cundinamarca" | 
                         City == "Mosquera" | 
                         City == "FUNZA, CUNDINAMARCA" |
                         City == "Madrid Cundinamarca" | 
                         City == "Une- Cundinamarca", 
                       "Bogota Region",
                       ifelse(City == "Soledad" | 
                                City == "Barranquilla" | 
                                City == "BARRANQUILLA" | 
                                City == "Soledad, Atlantico" |
                                City == "Costa Atlantica" |
                                City == "Corozal", 
                              "Atlantico Region", 
                              "Other"))) |> 
  mutate(Education = recode(Education,
                            "Primaria" =  "Primary school",
                            "Bachillerato" = "High school",
                            "Universitario" = "University",
                            "Posgrado" = "Postgraduate")) |> 
  mutate(Sexual_orientation = recode(Sexual_orientation,
                                     "Exclusivamente heterosexual" = "Exclusively heterosexual",
                                     "Principalmente heterosexual, con contactos homosexuales esporádicos" = "Predominantly heterosexual",
                                     "Predominantemente heterosexual, aunque con contactos homosexuales más que esporádicos" = "Predominantly heterosexual, but more than incidentally homosexual",
                                     "Bisexual" = "Bisexual",                                                                             
                                     "Pansexual" = "Pansexual",                                                                            
                                     "Demisexual" = "Demisexual")) |> 
  mutate(Relationship_status = recode(Relationship_status,
                                      "Soltero sin contactos sexuales en el último año" = "Single without sexual contacts",
                                      "Soltero con contactos sexuales en el último año" = "Single with sexual contacts",
                                      "Relación exclusiva o matrimonio - viven juntos" = "Exclusive relationship - cohabitating",
                                      "Relación exclusiva - no viven juntos" = "Exclusive relationship - not cohabitating",
                                      "Relación no exclusiva - contactos sexuales con otras personas" = "Non-exclusive relationship")) |> 
  mutate(Internet_use = recode(Internet_use,
                               "Cada día" = "Daily",
                               "Cada mes" = "Monthly",
                               "Cada año" = "Yearly")) |>
  mutate(across(starts_with("Men_perceived_as_danger_to_"),
                ~recode(.,
                        "Completamente en desacuerdo" =  1,
                        "Ligeramente en desacuerdo" = 2,
                        "Ni de acuerdo ni en desacuerdo" = 3,
                        "Ligeramente deacuerdo" = 4,
                        "Completamente deacuerdo" = 5))) |> 
  mutate(across(where(is.character), ~replace(., . == "Si" , "Yes"))) |>  
  mutate(across(where(is.character), ~replace(., . == "Sí" , "Yes"))) |> 
  mutate(across(where(is.character), ~replace(., . == "No quiero responder" , "Prefer not to answer"))) |> 
  mutate(across(where(is.character), ~replace(., . == "Mujer" , "Woman"))) |> 
  mutate(across(where(is.character), ~replace(., . == "Hombre" , "Man"))) |> 
  mutate(across(where(is.character), ~replace(., . == "Femenino" , "Female"))) |> 
  mutate(across(where(is.character), ~replace(., . == "Masculino" , "Male"))) |> 
  mutate(across(where(is.character), ~replace(., . == "Sin pareja actual" , "Single"))) |> 
  mutate(across(where(is.character), ~replace(., . == "Sí, una vez en la adultez" , "Once as adult"))) |> 
  mutate(across(where(is.character), ~replace(., . == "Sí, tanto en la infancia como en la adultez" , "Both as child and adult"))) |> 
  mutate(across(where(is.character), ~replace(., . == "Sí, más de una vez en mi infancia" , "More than once as child"))) |> 
  mutate(across(where(is.character), ~replace(., . ==  "Sí, una vez e mi infancia" , "Once as child"))) |> 
  mutate(across(where(is.character), ~replace(., . ==  "Afrocolombiano" , "Afrocolombian"))) |> 
  mutate(across(where(is.character), ~replace(., . ==  "Desplazado conflicto armado" , "Undetermined"))) |> 
  mutate(across(where(is.character), ~replace(., . ==  "Ninguna" , "Undetermined"))) |> 
  mutate(across(where(is.character), ~replace(., . ==  "Comunidad negra" , "Afrocolombian"))) |> 
  mutate(across(where(is.character), ~replace(., . ==  "Raizal del Archipiélago de San Andrés, Providencia y Santa Catalina" , "Raizal"))) |> 
  mutate(across(where(is.character), ~replace(., . ==  "Patos" , "Indigenous"))) |> 
  mutate(across(where(is.character), ~replace(., . ==  "Indígena" , "Indigenous"))) |> 
  mutate(across(where(is.character), ~replace(., . ==  "No estoy segura" , "Unsure")))

### PCA----
#### Socio-ecological factors----
quests_pca_gen <- quests |> 
  select(ID, 
         Men_perceived_as_danger_to_partner, 
         Men_perceived_as_danger_to_children,
         Freq_partner_physical_violence,
         Freq_partner_sexual_violence,
         Freq_partner_infidelity,
         Perceived_home_safety) |> 
  #rename_with(~str_replace_all(., "Men_perceived_as_danger_to_")) |> 
  rename_with(~str_replace_all(., "Freq_", "Frequency of")) |>
  rename_with(~str_replace_all(., "_", " ")) |> 
  rename_with(~str_to_sentence(.))

pca_sef <- PCA(quests_pca_gen[,-1], graph = FALSE)
summary(pca_sef)

ggarrange(fviz_eig(pca_sef, addlabels = TRUE, barfill = "#00AFBB") +
            labs(title = "PCA: Socio-ecological factors",
                 subtitle = "Scree plot"),
          fviz_pca_var(pca_sef, 
                       col.var = "#00AFBB",
                       repel = TRUE) +
            labs(title = NULL,
                 subtitle = "Loadings"))

#### Men perceived as dangerous----
quests_pca <- quests |> 
  select(ID, 
         Men_perceived_as_danger_to_partner, 
         Men_perceived_as_danger_to_children) |> 
  rename_with(~str_remove_all(., "Men_perceived_as_danger_to_")) |>
  rename_with(~str_to_sentence(.))

pca_mpd <- PCA(quests_pca[,-1], graph = FALSE)
summary(pca_mpd)

mpd_scores <- data.frame(pca_mpd$ind$coord)$Dim.1

ggarrange(fviz_eig(pca_mpd, addlabels = TRUE, barfill = "#00AFBB") +
            labs(title = "PCA: Men perceived as danger to...",
                 subtitle = "Scree plot"),
          fviz_pca_var(pca_mpd, 
                       col.var = "#00AFBB",
                       repel = TRUE) +
            labs(title = NULL,
                 subtitle = "Loadings"))

### Con puntajes totales de instrumentos, menos columnas----
quests_clean <- quests |>  
  mutate(across(starts_with("Escasez alimentaria"),
                ~recode(.,
                        "Nunca" =  0,
                        "Rara vez/algunas veces" = 1,
                        "Casi siempre" = 2))) |> 
  rowwise() |> 
  mutate(Self_esteem = sum(autoestima_I1, 5-autoestima_I2, autoestima_I3, autoestima_I4,
                           autoestima_I5, 5-autoestima_I6, autoestima_I7, 5-autoestima_I8, 
                           5-autoestima_I9, autoestima_I10),
         Self_perception = sum(across(starts_with("SP_"))),
         Perceived_safety = sum(across(ends_with("_safety"))),
         Food_insecurity = sum(across(starts_with("Escasez alimentaria")))) |> 
  select(!starts_with("autoestima_")) |> 
  mutate(across(where(is.character), as.factor)) |> 
  bind_cols(Men_perceived_as_dangerous = mpd_scores)

## Evaluación subjestiva de rostros----
### Formato ancho
eval <- read_excel("Data/Evaluación subjetiva rostros (Respuestas).xlsx") |> 
  select(-c(123:124)) |> 
  rowwise() |> 
  mutate(Masculinity_masculinized = sum(across(ends_with("M Mas"))),
         Masculinity_feminized = sum(across(ends_with("F Mas"))),
         Attractiveness_masculinized = sum(across(ends_with("M Atr"))),
         Attractiveness_feminized = sum(across(ends_with("F Atr")))) |> 
  rename(Date = "Marca temporal",
         ID = "Escribe tu código de participante")
### Formato largo
eval_long <- left_join(eval |>
                         select(-c(123:126)) |> 
                         select(!ends_with(" Mas")) |> 
                         pivot_longer(cols = ends_with("Atr"),
                                      names_to = "Stimulus",
                                      values_to = "Attractiveness") |> 
                         mutate(Stimulus = str_remove_all(Stimulus, " Atr")), 
                       eval |> 
                         select(-c(123:126)) |> 
                         select(!ends_with(" Atr")) |> 
                         pivot_longer(cols = ends_with("Mas"),
                                      names_to = "Stimulus",
                                      values_to = "Masculinity") |> 
                         mutate(Stimulus = str_remove_all(Stimulus, " Mas")))

## Disponibilidad de recursos----
reg <- rbind(read_excel("Data/3Registro Participantes Disponibilidad de Recursos-corregido.xlsx", 
                        sheet = "UB") |> 
               mutate(University = "UB"),
             read_excel("Data/3Registro Participantes Disponibilidad de Recursos-corregido.xlsx", 
                        sheet = "CUC") |> 
               mutate(University = "CUC")) |> 
  select(-c(Grupo, `Entrega de kit`, `Protocolo de bioseguridad`, `Requisitos previos al registro`, Consentimiento,
            `Código de evaluador`:`Código auxiliar que reclutó`)) |>
  rename(Date = "Fecha de registro",
         ID = "Codigo del Participante",
         Condition = "Condicion",
         Calibration = "Calibración",
         Gaze_perc = "% Gaze",
         Condition_happiness = "Q Feliz",
         Condition_physical_safety = "Q Segura físicamente",
         Condition_healthy = "Q Saludable",
         Condition_economic_security = "Q Segura económicamente",
         Body_temperature = "Temperatura",
         Ovulating = "Test de ovulación",
         Saliva_pre = "Recolección de saliva pre",
         Saliva_pre_time = "Hora...18",
         Eye_tracking = "Rastreo Ocular",
         Subjective_evaluation = "Evaluación subjetiva",
         Sociodemographic_questionnaire = "Cuestionario sociodemográfico",
         Saliva_post = "Recolección de saliva post",
         Saliva_post_time = "Hora...23",
         Notes = "Observaciones") |>
  mutate(Condition = fct_recode(Condition, 
                                "Low" = "Baja",
                                "High" = "Alta"),
         Calibration = fct_recode(Calibration, 
                                  "<=0.5" = "<0.5 (menor a 0.5)",
                                  ">0.5" = ">0.5 (mayor a 0.5)",
                                  "<=0.5" = "0.5 (igual a 0.5)",
                                  NULL = "Selecciona"),
         Ovulating = fct_recode(as.factor(Ovulating),
                                "No" = "0",
                                "Yes" = "1")) |> 
  mutate_all(~str_replace_all(., "SI", "Yes")) |> 
  mutate_all(~str_replace_all(., "NO", "No")) |> 
  mutate_all(~str_replace_all(., "INCOMPLETO", "No")) |>
  mutate_all(~str_replace_all(., "Recuperado", "Data recovered")) |> 
  mutate_all(~str_replace_all(., "RECUPERADO", "Data recovered")) |> 
  mutate_all(~na_if(., "Selecciona")) |> 
  mutate_all(~na_if(., "N/A")) |> 
  mutate(across(starts_with("Condition_"), as.numeric))

# Base de datos final----
## Integrada----
dat_int <- dat_et |> 
  left_join(quests_clean, by = c("ID"), multiple = "all") |>
  left_join(eval_long, by = c("ID", "Stimulus"), multiple = "all") |> 
  left_join(reg, by = c("ID", "University", "Condition"), multiple = "all") 

### Tamaño de muestra----
n_recolectado <- dat_int |> 
  summarise(n = n_distinct(ID))

## Base de datos filtrada----
dat <- dat_int |> 
  filter(Control_question_1 == "No" 
         & Control_question_2 == "No" 
         & Ovulating != "Yes" 
         & Sexual_orientation == "Exclusively heterosexual") |> 
  mutate(Stimulus = str_remove_all(Stimulus, "F")) |> 
  mutate(Stimulus = str_remove_all(Stimulus, "M")) |> 
  ungroup()

## Base de datos diferencia masculinizado - feminizado----
    # Por participante, estímulo y relación
dat_dif <- dat |> 
  group_by(ID, 
           Stimulus, 
           Condition, 
           Relationship,
           Sexual_orientation,
           Freq_partner_physical_violence,
           Freq_partner_sexual_violence,
           Freq_partner_infidelity,
           Relationship_current,
           Men_perceived_as_dangerous) |> 
  summarise(DFF_dif = DFF[Sexual_dimorphism == "Masculinized"] - DFF[Sexual_dimorphism == "Feminized"],
            TFD_dif = TFD[Sexual_dimorphism == "Masculinized"] - TFD[Sexual_dimorphism == "Feminized"],
            NF_dif = NF[Sexual_dimorphism == "Masculinized"] - NF[Sexual_dimorphism == "Feminized"],
            Attr_dif = Attractiveness[Sexual_dimorphism == "Masculinized"] - Attractiveness[Sexual_dimorphism == "Feminized"],
            Masc_dif = Masculinity[Sexual_dimorphism == "Masculinized"] - Masculinity[Sexual_dimorphism == "Feminized"]) |> 
  ungroup() |> 
  mutate(across(where(is.character), as.factor))
  
### Tamaño de muestra----
n_filtrado <- dat |> 
  summarise(n = n_distinct(ID))

n_edad <- dat |> 
  group_by(ID) |> 
  summarise(Age = first(Age),
            Condition = first(Condition)) |> 
  ungroup() |> 
  group_by(Condition) |> 
  summarise(n = n_distinct(ID),
            Mean = mean(Age, na.rm = TRUE),
            SD = sd(Age, na.rm = TRUE),
            Min = min(Age, na.rm = TRUE),
            Max = max(Age, na.rm = TRUE))

## Bases filtradas individuales----
### Disponibilidad de recursos----
reg_fin <- reg |> 
  left_join(quests_clean, by = c("ID")) |> 
  filter(ID %in% unique(dat$ID)) 

## Cuestionarios---
quests_fin <- quests_clean |> 
  filter(ID %in% unique(dat$ID)) 

# Descriptivos----
## Sociodemographic----
desc_quest <- quests_fin |> 
  left_join(reg, by = c("ID")) |> 
  select(ID, Condition, Age, City, Education, Ethnicity, Sexual_orientation, Relationship_current, 
         Relationship_status:Hormonal_contraception, Sexual_abuse,
         SP_happiness:Socioeconomic_level, 
         Perceived_country_safety:Freq_robery,
         Victim_of_violence,
         Victim_of_gender_violence:Victim_of_armed_conflict,
         Self_esteem:Men_perceived_as_dangerous,
         Freq_partner_physical_violence, Freq_partner_infidelity,
         Partner_physical_violence, Partner_sexual_violence,
         Freq_partner_sexual_violence,
         "Escasez alimentaria1":"Escasez alimentaria5") |> 
  mutate(across(starts_with("Escasez alimentaria"),
                ~recode(.,
                        "0" = "Never",
                        "1" = "Rarely/sometimes",
                        "2" = "Almost always"))) |> 
  mutate(across(where(is.character), as.factor)) |> 
  mutate(across(starts_with("Escasez alimentaria"),
                ~factor(.,
                        levels = c("Never",
                                   "Rarely/sometimes",
                                   "Almost always"))))
### Sociodemographic numeric----
desc_quest |> 
  select(ID, Condition, where(is.numeric)) |> 
  pivot_longer(where(is.numeric),
               names_to = "Variable",
               values_to = "Value") |> 
  mutate(Variable = str_replace_all(Variable, "_", " ")) |>
  mutate(Variable = str_replace_all(Variable, "Freq", "Frequency of")) |>
  mutate(Variable = str_replace_all(Variable, "SP", "Self-perceived")) |>
  ggplot(aes(x = Value, fill = Condition, color = Condition)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~Variable, scales = "free") +
  stat_summary(aes(xintercept = after_stat(x), y = 0),
               fun = mean, geom = "vline", orientation = "y") +
  labs(x = NULL, y = "Density")

### Sociodemographic categorical----
desc_quest |> 
  select(ID, Condition, where(is.factor), City, -starts_with("Escasez alimentaria")) |> 
  pivot_longer(City:Victim_of_armed_conflict,
               names_to = "Variable",
               values_to = "Value") |> 
  mutate(Variable = str_replace_all(Variable, "_", " ")) |> 
  ggplot(aes(y = Value, fill = Condition, color = Condition)) +
  geom_bar(alpha = 0.3, position = position_dodge()) +
  geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 0.1)),
            vjust = "inward",
            position = position_dodge(.9),
            stat = "prop",
            color = "black",
            size = 2.5) +
  facet_wrap(~Variable, scales = "free") +
  scale_y_discrete(labels = label_wrap(20)) +
  theme(axis.text.y = element_text(size = 8)) +
  labs(x = NULL, y = "Count")

## Sociodemographic factors----
ggarrange(desc_quest |>
            select(ID, Condition, Age, Number_of_children) |>
            pivot_longer(where(is.numeric),
                         names_to = "Variable",
                         values_to = "Value") |> 
            mutate(Variable = str_replace_all(Variable, "_", " ")) |>
            ggplot(aes(x = Value, fill = Condition, color = Condition)) +
            geom_density(alpha = 0.3) +
            facet_wrap(~Variable, scales = "free", ncol = 1) +
            stat_summary(aes(xintercept = after_stat(x), y = 0),
                         fun = mean, geom = "vline", orientation = "y") +
            labs(x = NULL, y = NULL),
          desc_quest |> 
            select(ID, Condition, Sexual_orientation, City, Ethnicity, 
                   Education, Relationship_current, Relationship_status) |> 
            pivot_longer(Sexual_orientation:Relationship_status,
                         names_to = "Variable",
                         values_to = "Value") |> 
            mutate(Variable = str_replace_all(Variable, "_", " ")) |> 
            ggplot(aes(y = Value, fill = Condition, color = Condition)) +
            geom_bar(alpha = 0.3, position = position_dodge()) +
            geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 0.1)),
                      vjust = "inward",
                      position = position_dodge(.9),
                      stat = "prop",
                      color = "black",
                      size = 2.5) +
            facet_wrap(~Variable, scales = "free") +
            scale_y_discrete(labels = label_wrap(20)) +
            theme(axis.text.y = element_text(size = 8)) +
            labs(x = NULL, y = NULL),
          widths = c(1, 3),
          common.legend = TRUE,
          legend = "bottom",
          labels = "auto")
 
## Access to resources factors----
ggarrange(desc_quest |> 
            select(ID, Condition, Socioeconomic_level, Electricity, Internet_access, Internet_use,
                   TV, Hospital_access) |> 
            pivot_longer(Socioeconomic_level:Hospital_access,
                         names_to = "Variable",
                         values_to = "Value") |> 
            mutate(Variable = str_replace_all(Variable, "_", " ")) |> 
            ggplot(aes(y = Value, fill = Condition, color = Condition)) +
            geom_bar(alpha = 0.3, position = position_dodge()) +
            geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 0.1)),
                      vjust = "inward",
                      position = position_dodge(.9),
                      stat = "prop",
                      color = "black",
                      size = 2.5) +
            facet_wrap(~Variable, scales = "free") +
            scale_y_discrete(labels = label_wrap(20)) +
            theme(axis.text.y = element_text(size = 8)) +
            labs(x = NULL, y = NULL),
          widths = c(1, 3),
          common.legend = TRUE,
          legend = "bottom")       

## Health-related factors----
ggarrange(desc_quest |>
            select(ID, Condition, Freq_illness, SP_health) |>
            pivot_longer(Freq_illness:SP_health,
                         names_to = "Variable",
                         values_to = "Value") |> 
            mutate(Variable = str_replace_all(Variable, "_", " ")) |>
            mutate(Variable = str_replace_all(Variable, "Freq", "Frequency of")) |>
            mutate(Variable = str_replace_all(Variable, "SP", "Self-perceived")) |>
            mutate(Value = as.numeric(Value)) |> 
            ggplot(aes(x = Value, fill = Condition, color = Condition)) +
            geom_density(alpha = 0.3) +
            facet_wrap(~Variable) +
            stat_summary(aes(xintercept = after_stat(x), y = 0),
                         fun = mean, geom = "vline", orientation = "y") +
            labs(x = NULL, y = NULL),
          widths = c(2, 1),
          common.legend = TRUE,
          legend = "bottom")

## Food security factors----
ggarrange(desc_quest |>
            select(ID, Condition, "Escasez alimentaria1":"Escasez alimentaria5") |> 
            pivot_longer("Escasez alimentaria1":"Escasez alimentaria5",
                         names_to = "Variable",
                         values_to = "Value") |> 
            mutate(Variable = str_replace_all(Variable, "Escasez alimentaria", "")) |> 
            mutate(Variable = str_replace_all(Variable, "1", "1. Smaller food portions")) |>
            mutate(Variable = str_replace_all(Variable, "2", "2. Reduced number of meals")) |>
            mutate(Variable = str_replace_all(Variable, "3", "3. Food scarcity at home")) |>
            mutate(Variable = str_replace_all(Variable, "4", "4. Sleeping with hunger")) |>
            mutate(Variable = str_replace_all(Variable, "5", "5. Day and night without eating")) |>
            ggplot(aes(y = Value, fill = Condition, color = Condition)) +
            geom_bar(alpha = 0.3, position = position_dodge()) +
            geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 0.1)),
                      vjust = "inward",
                      position = position_dodge(.9),
                      stat = "prop",
                      color = "black",
                      size = 2.5) +
            facet_wrap(~Variable, scales = "free") +
            scale_y_discrete(labels = label_wrap(20)) +
            theme(axis.text.y = element_text(size = 8)) +
            labs(x = NULL, y = NULL, title = "Items"),
          desc_quest |>
            select(ID, Condition, Food_insecurity) |>
            pivot_longer(Food_insecurity,
                         names_to = "Variable",
                         values_to = "Value") |> 
            mutate(Value = as.numeric(Value)) |> 
            mutate(Variable = str_replace_all(Variable, "_", " ")) |>
            ggplot(aes(x = Value, fill = Condition, color = Condition)) +
            geom_density(alpha = 0.3) +
            facet_wrap(~Variable) +
            stat_summary(aes(xintercept = after_stat(x), y = 0),
                         fun = mean, geom = "vline", orientation = "y") +
            labs(x = NULL, y = NULL, title = "Total"),
          widths = c(3, 1),
          common.legend = TRUE,
          legend = "bottom",
          labels = "auto")

## Hormonal factors----
ggarrange(reg_fin |>
            select(ID, Condition, Body_temperature) |>
            pivot_longer(Body_temperature,
                         names_to = "Variable",
                         values_to = "Value") |> 
            mutate(Value = as.numeric(Value)) |> 
            ggplot(aes(x = Value, fill = Condition, color = Condition)) +
            geom_density(alpha = 0.3) +
            facet_wrap(~Variable) +
            stat_summary(aes(xintercept = after_stat(x), y = 0),
                         fun = mean, geom = "vline", orientation = "y") +
            labs(x = NULL, y = NULL),
          reg_fin |> 
            left_join(desc_quest, by = c("ID", "Condition", "Hormonal_contraception")) |> 
            select(ID, Condition, Ovulating, Hormonal_contraception) |> 
            pivot_longer(Ovulating:Hormonal_contraception,
                         names_to = "Variable",
                         values_to = "Value") |> 
            mutate(Variable = str_replace_all(Variable, "_", " ")) |> 
            ggplot(aes(y = Value, fill = Condition, color = Condition)) +
            geom_bar(alpha = 0.3, position = position_dodge()) +
            geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 0.1)),
                      vjust = "inward",
                      position = position_dodge(.9),
                      stat = "prop",
                      color = "black",
                      size = 2.5) +
            facet_wrap(~Variable, scales = "free") +
            scale_y_discrete(labels = label_wrap(20)) +
            theme(axis.text.y = element_text(size = 8)) +
            labs(x = NULL, y = NULL),
          widths = c(1, 3),
          common.legend = TRUE,
          legend = "bottom",
          labels = "auto")

## Psychological factors----
desc_quest |>
  select(ID, Condition, starts_with("SP_"), -SP_health) |>
  pivot_longer(where(is.numeric),
               names_to = "Variable",
               values_to = "Value") |> 
  mutate(Variable = str_replace_all(Variable, "SP_", "")) |>
  mutate(Variable = str_replace_all(Variable, "self_", "self-")) |>
  mutate(Variable = str_replace_all(Variable, "_", " ")) |>
  mutate(Variable = str_to_sentence(Variable)) |> 
  ggplot(aes(x = Value, fill = Condition, color = Condition)) +
  geom_density(alpha = 0.3) +
  labs(title = "Self-perceived conditions") +
  facet_wrap(~Variable, scales = "free") +
  stat_summary(aes(xintercept = after_stat(x), y = 0),
               fun = mean, geom = "vline", orientation = "y") +
  labs(x = NULL, y = NULL)

## Last partner perception----
desc_quest |>
  select(ID, Condition, Partner_masculinity, Partner_dominance, 
         Partner_attractiveness) |>
  pivot_longer(where(is.numeric),
               names_to = "Variable",
               values_to = "Value") |> 
  mutate(Variable = str_replace_all(Variable, "Partner_", "")) |>
  mutate(Variable = str_to_sentence(Variable)) |> 
  ggplot(aes(x = Value, fill = Condition, color = Condition)) +
  geom_density(alpha = 0.3) +
  labs(title = "Current/last partner perception") +
  facet_wrap(~Variable, scales = "free") +
  stat_summary(aes(xintercept = after_stat(x), y = 0),
               fun = mean, geom = "vline", orientation = "y") +
  labs(x = NULL, y = NULL)

## Factors related con context violence----
ggarrange(desc_quest |>
            select(ID, Condition, ends_with("_safety"), Freq_robery) |>
            pivot_longer(where(is.numeric),
                         names_to = "Variable",
                         values_to = "Value") |> 
            mutate(Value = as.numeric(Value)) |> 
            mutate(Variable = str_replace_all(Variable, "_safety", "")) |>
            mutate(Variable = str_replace_all(Variable, "Perceived_", "")) |>
            mutate(Variable = str_replace_all(Variable, "Freq_", "Frequency of ")) |>
            mutate(Variable = str_replace_all(Variable, "Perceived", "General perception")) |>
            mutate(Variable = str_to_sentence(Variable)) |> 
            ggplot(aes(x = Value, fill = Condition, color = Condition)) +
            geom_density(alpha = 0.3) +
            labs(title = "Safety perception") +
            facet_wrap(~factor(Variable, c("Country", "City", "Neighborhood", "Home",
                                           "Frequency of robery", "General perception")),
                               scales = "free") +
            stat_summary(aes(xintercept = after_stat(x), y = 0),
                         fun = mean, geom = "vline", orientation = "y") +
            labs(x = NULL, y = NULL),
          ggarrange(desc_quest |>
                      select(ID, Condition, 
                             Men_perceived_as_dangerous) |> 
                      pivot_longer(Men_perceived_as_dangerous,
                                   names_to = "Variable",
                                   values_to = "Value") |> 
                      mutate(Variable = str_replace_all(Variable, 
                                                        "_", " ")) |> 
                      mutate(Variable = str_to_sentence(Variable)) |> 
                      ggplot(aes(x = Value, fill = Condition, color = Condition)) +
                      geom_density(alpha = 0.3) +
                      labs(title = "Men perceived as dangerous") +
                      facet_wrap(~Variable, scales = "free") +
                      #scale_y_discrete(labels = label_wrap(20)) +
                      theme(axis.text.y = element_text(size = 8)) +
                      labs(x = NULL, y = NULL),
                    desc_quest |>
                      select(ID, Condition, Victim_of_armed_conflict) |>
                      pivot_longer(Victim_of_armed_conflict,
                                   names_to = "Variable",
                                   values_to = "Value") |> 
                      mutate(Variable = str_replace_all(Variable, 
                                                        "_", " ")) |>
                      ggplot(aes(y = Value, fill = Condition, color = Condition)) +
                      geom_bar(alpha = 0.3, position = position_dodge()) +
                      geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 0.1)),
                                vjust = "inward",
                                position = position_dodge(.9),
                                stat = "prop",
                                color = "black",
                                size = 2.5) +
                      labs(title = "Victim of armed conflict") +
                      facet_wrap(~Variable, scales = "free") +
                      scale_y_discrete(labels = label_wrap(20)) +
                      theme(axis.text.y = element_text(size = 8)) +
                      labs(x = NULL, y = NULL),
                    ncol = 1,
                    labels = c("", "c")),
          #widths = c(2, 1),
          common.legend = TRUE,
          legend = "bottom",
          labels = "auto")

## Factors related con gender and partner violence----
ggarrange(desc_quest |>
            select(ID, Condition, Freq_partner_physical_violence,
                   , Freq_partner_sexual_violence, Freq_partner_infidelity) |>
            pivot_longer(where(is.numeric),
                         names_to = "Variable",
                         values_to = "Value") |> 
            mutate(Value = as.numeric(Value)) |> 
            mutate(Variable = str_replace_all(Variable, "Freq_partner_", "")) |>
            mutate(Variable = str_replace_all(Variable, "_", " ")) |>
            mutate(Variable = str_to_sentence(Variable)) |> 
            ggplot(aes(x = Value, fill = Condition, color = Condition)) +
            geom_density(alpha = 0.3) +
            facet_wrap(~factor(Variable, c("Physical violence",
                                           "Sexual violence",
                                           "Infidelity")), 
                       scales = "free", ncol = 1) +
            stat_summary(aes(xintercept = after_stat(x), y = 0),
                         fun = mean, geom = "vline", orientation = "y") +
            labs(x = NULL, y = NULL),
          desc_quest |> 
            select(ID, Condition, 
                   Victim_of_gender_violence, 
                   Partner_physical_violence,
                   Partner_sexual_violence,
                   Sexual_abuse) |> 
            pivot_longer(Victim_of_gender_violence:Sexual_abuse,
                         names_to = "Variable",
                         values_to = "Value") |> 
            mutate(Value = as.factor(Value)) |> 
            mutate(Variable = str_replace_all(Variable, 
                                              "_", " ")) |> 
            mutate(Variable = str_to_sentence(Variable)) |> 
            ggplot(aes(y = Value, fill = Condition, color = Condition)) +
            geom_bar(alpha = 0.3, position = position_dodge()) +
            geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 0.1)),
                      vjust = "inward",
                      position = position_dodge(.9),
                      stat = "prop",
                      color = "black",
                      size = 2.5) +
            facet_wrap(~Variable, 
                               scales = "free") +
            scale_y_discrete(labels = label_wrap(20)) +
            theme(axis.text.y = element_text(size = 8)) +
            labs(x = NULL, y = NULL),
          widths = c(1, 2),
          common.legend = TRUE,
          legend = "bottom",
          labels = "auto")

## Subjective evaluation----
eval_long |> 
  left_join(reg, by = c("ID")) |> 
  filter(ID %in% unique(dat$ID)) |> 
  rowwise() |> 
  mutate(Sexual_dimorphism = ifelse(grepl("F", Stimulus), "Feminine", "Masculine")) |> 
  select(Condition, Sexual_dimorphism, Attractiveness, Masculinity) |> 
  pivot_longer(Attractiveness:Masculinity,
               names_to = "Variable",
               values_to = "Value") |>
  ggplot(aes(x = Value, fill = Sexual_dimorphism, color = Sexual_dimorphism)) +
  geom_density(alpha = 0.3) +
  facet_grid(Condition~Variable, scales = "free") +
  stat_summary(aes(xintercept = after_stat(x), y = 0),
               fun = mean, geom = "vline", orientation = "y")


## Correlations----
### Correlations in partner perceptions----
quests_fin |> 
  select(Freq_partner_physical_violence, 
         Freq_partner_sexual_violence, 
         Freq_partner_infidelity,
         Partner_masculinity,
         Partner_dominance,
         Partner_attractiveness) |>
  ggcorr(label = TRUE,
         label_round = 2)

### Correlations table (general)----
desc_quest |> 
  left_join(reg_fin |> 
              select(ID, Body_temperature), 
            by = c("ID")) |>
  select(Age,
         Freq_illness,
         starts_with("SP_"),
         Partner_masculinity, Partner_dominance, Partner_attractiveness,
         ends_with("_safety"), Freq_robery,
         Freq_partner_physical_violence, 
         Freq_partner_sexual_violence, 
         Freq_partner_infidelity) |> 
  rename_with(~str_replace_all(., "_", " ")) |> 
  rename_with(~str_replace_all(., "Freq", "Frequency of")) |>
  rename_with(~str_replace_all(., "Frequency of partner", "Partner")) |>
  rename_with(~str_replace_all(., "SP ", "")) |>
  rename_with(~str_replace_all(., "Perceived ", "")) |>
  rename_with(~str_to_sentence(.)) |>
  corr.stars() |>
  rownames_to_column(var = " ") |> 
  slice(-1) |> 
  kable(digits = 2,
        booktabs = TRUE,
        align = c("l", rep("c", 20)),
        linesep = "",
        caption = "Correlations between XXXXXX",
        escape = FALSE) |>
  kable_styling(latex_options = c("HOLD_position"),
                font_size = 5) |>
  column_spec(1, width = "1.2cm") |>
  column_spec(2:21, width = "0.7cm") |>
  add_header_above(c(" ", 
                     "Age" = 1, 
                     "Health" = 1,
                     "Self-perceived conditions" = 7,
                     "Current/last partner\nperception" = 3,
                     "Perceived context\nviolence" = 6,
                     "Frequency of partner\nviolence" = 2),
                   bold = TRUE) |> 
  footnote(general = paste0("Values represent Pearson correlation coefficients ($r$). ",
                            "For significance, $^{\\\\dagger}p$ < 0.1, *$p$ < 0.05, ",
                            "**$p$ < 0.01, ***$p$ < 0.001. ",
                            "Significant correlations are in bold."),
           threeparttable = TRUE,
           footnote_as_chunk = TRUE,
           escape = FALSE) |>
  landscape()

### Correlations table (fixations, subjective evaluations and violence)----
dat_main_corr <- dat |> 
  select(ID, Condition, Relationship,
         TFD, DFF, NF,
         Masculinity, Attractiveness,
         Partner_attractiveness:Partner_masculinity,
         ends_with("_safety"), Freq_robery,
         starts_with("Freq_partner_")) |> 
  group_by(ID, Condition, Relationship) |> 
  summarise_all(mean)

dat_main_corr |> 
  filter(Condition == "High") |> 
  ungroup() |> 
  select(TFD:Freq_partner_infidelity) |> 
  rename_with(~str_replace_all(., "_", " ")) |> 
  rename_with(~str_replace_all(., "Freq", "Frequency of")) |>
  rename_with(~str_replace_all(., "Frequency of partner", "Partner")) |>
  corr.stars() |>
  rownames_to_column(var = " ") |> 
  slice(-1) |> 
  kable(digits = 2,
        booktabs = TRUE,
        align = c("l", rep("c", 20)),
        linesep = "",
        caption = "Correlations between XXXXXX",
        escape = FALSE) |>
  kable_styling(latex_options = c("HOLD_position")) |>
  column_spec(1, width = "1.2cm") |>
  column_spec(2:21, width = "0.7cm") |>
  add_header_above(c(" ", 
                     "Age" = 1, 
                     "Health" = 1,
                     "Self-perceived conditions" = 7,
                     "Current/last partner\nperception" = 3,
                     "Perceived context\nviolence" = 6,
                     "Frequency of partner\nviolence" = 2),
                   bold = TRUE) |> 
  footnote(general = paste0("Values represent Pearson correlation coefficients ($r$). ",
                            "For significance, $^{\\\\dagger}p$ < 0.1, *$p$ < 0.05, ",
                            "**$p$ < 0.01, ***$p$ < 0.001. ",
                            "Significant correlations are in bold."),
           threeparttable = TRUE,
           footnote_as_chunk = TRUE,
           escape = FALSE) |>
  landscape()

### Correlations between sociocontextual factors and fixations----
dat_choice_dif <- dat |> 
  mutate(Choice = as.numeric(recode(Choice,
                                    "Yes" =  "1",
                                    "No" = "0"))) |> 
  group_by(ID, Sexual_dimorphism, Relationship, Condition) |> 
  summarise(Choice = sum(Choice)) |> 
  group_by(ID, Sexual_dimorphism, Relationship, Condition) |> 
  summarise(Choice_count = sum(Choice)) |> 
  ungroup() |> 
  group_by(ID, Relationship, Condition) |> 
  summarise(Choice_dif = Choice_count[Sexual_dimorphism == "Masculinized"] - 
              Choice_count[Sexual_dimorphism == "Feminized"])

dat_dif_short <- dat_dif |> 
  group_by(ID, Relationship, Condition) |> 
  summarise(Freq_partner_physical_violence = mean(Freq_partner_physical_violence),
            Freq_partner_sexual_violence = mean(Freq_partner_sexual_violence), 
            Freq_partner_infidelity = mean(Freq_partner_infidelity),
            Men_perceived_as_dangerous = mean(Men_perceived_as_dangerous),
            DFF_dif = mean(DFF_dif),
            TFD_dif = mean(TFD_dif),
            NF_dif = mean(NF_dif),
            Attr_dif = mean(Attr_dif),
            Masc_dif = mean(Masc_dif)) |> 
  left_join(dat_choice_dif |> 
              select(ID, Relationship, Condition, Choice_dif), 
            by = c("ID", "Relationship", "Condition")) |> 
  rename("PPV" = "Freq_partner_physical_violence",
         "PSV" = "Freq_partner_sexual_violence",      
         "PI" =  "Freq_partner_infidelity",
         "MPD" = "Men_perceived_as_dangerous",
         "DFF" = "DFF_dif",
         "TFD" = "TFD_dif",
         "NF" =  "NF_dif",
         "Attr." = "Attr_dif",
         "Masc." = "Masc_dif",
         "Choice" = "Choice_dif") 
  
Rst_Cl <- dat_dif_short |> 
  select(where(is.numeric), Relationship, Condition) |> 
  filter(Relationship == "Short term" & Condition == "Low")

Rst_Ch <- dat_dif_short |> 
  select(where(is.numeric), Relationship, Condition) |> 
  filter(Relationship == "Short term" & Condition == "High")

Rlt_Cl <- dat_dif_short |> 
  select(where(is.numeric), Relationship, Condition) |> 
  filter(Relationship == "Long term" & Condition == "Low")

Rlt_Ch <- dat_dif_short |> 
  select(where(is.numeric), Relationship, Condition) |> 
  filter(Relationship == "Long term" & Condition == "High")

ggarrange(ggcorr(Rst_Cl[,1:11],
                 geom = "blank", label = TRUE, label_size = 2,
                 hjust = 0.75, label_round = 3) +
            geom_point(size = 10, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.5)) +
            scale_alpha_manual(values = c("TRUE" = 0.3, "FALSE" = 0)) +
            guides(color = FALSE, alpha = FALSE) +
            labs(subtitle = "Low condition, short-term"),
          ggcorr(Rst_Ch[,1:11], 
                 geom = "blank", label = TRUE, label_size = 2,
                 hjust = 0.75, label_round = 3) +
            geom_point(size = 10, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.5)) +
            scale_alpha_manual(values = c("TRUE" = 0.3, "FALSE" = 0)) +
            guides(color = FALSE, alpha = FALSE) +
            labs(subtitle = "High condition, short-term"),
          ggcorr(Rlt_Cl[,1:11],
                 geom = "blank", label = TRUE, label_size = 2,
                 hjust = 0.75, label_round = 3) +
            geom_point(size = 10, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.5)) +
            scale_alpha_manual(values = c("TRUE" = 0.3, "FALSE" = 0)) +
            guides(color = FALSE, alpha = FALSE) +
            labs(subtitle = "Low condition, long-term"),
          ggcorr(Rlt_Ch[,1:11],
                 geom = "blank", label = TRUE, label_size = 2,
                 hjust = 0.75, label_round = 3) +
            geom_point(size = 10, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.5)) +
            scale_alpha_manual(values = c("TRUE" = 0.3, "FALSE" = 0)) +
            guides(color = FALSE, alpha = FALSE) +
            labs(subtitle = "High condition, long-term"),
          common.legend = TRUE,
          legend = "bottom")

# Manipulation check----

## Condition: resource availability----
reg_fin |> 
  select(starts_with("Condition")) |> 
  pivot_longer(cols = contains("_"),
               names_to = "Dimension",
               values_to = "Score") |> 
  group_by(Dimension) |> 
  summarise("Mean (Low)" = mean(Score[reg_fin$Condition == "Low"]),
            "Mean (High)" = mean(Score[reg_fin$Condition == "High"]),
            t = t.test(Score ~ Condition)$statistic,
            p = t.test(Score ~ Condition)$p.value,
            g = hedges_g(Score ~ Condition)$Hedges_g) |> 
  ungroup() |> 
  mutate(p.signif = stars.pval(p))

## Sexual dimorphism----
eval_desc <- dat |>
  group_by(ID, Sexual_dimorphism, Condition) |> 
  summarise(Masculinity = mean(Masculinity),
            Attractiveness = mean(Attractiveness))
### Masculinity----
mod_masc <- lmer(Masculinity ~ Sexual_dimorphism * Condition + (1 | ID), data = eval_desc)
anova(mod_masc)
contr_mod_masc <- as.data.frame(pairs(emmeans(mod_masc,
                                              ~ Sexual_dimorphism | Condition))) |> 
  separate(contrast, c("group1", "group2"), " - ") |> 
  mutate(p.signif = stars.pval(p.value))

p_mancheck_masc <- ggplot(eval_desc, aes(x = Sexual_dimorphism, y = Masculinity, color = Sexual_dimorphism)) +
  geom_jitter(alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot",
               color = "black") +
  stat_pvalue_manual(contr_mod_masc, label = "p.signif",
                     y.position = 10.5,
                     hide.ns = TRUE,
                     tip.length = 0) +
  labs(x = NULL,
       color = "Sexual dimorphism") +
  facet_wrap(~Condition)

### Attractiveness----
mod_attr <- lmer(Attractiveness ~ Sexual_dimorphism * Condition + (1 | ID), data = eval_desc)
anova(mod_attr)
contr_mod_attr <- as.data.frame(pairs(emmeans(mod_attr,
                                              ~ Sexual_dimorphism | Condition))) |> 
  separate(contrast, c("group1", "group2"), " - ") |> 
  mutate(p.signif = stars.pval(p.value))

p_mancheck_attr <- ggplot(eval_desc, aes(x = Sexual_dimorphism, y = Attractiveness, color = Sexual_dimorphism)) +
  geom_jitter(alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot",
               color = "black") +
  stat_pvalue_manual(contr_mod_attr, label = "p.signif",
                     y.position = 10,
                     hide.ns = TRUE,
                     tip.length = 0) +
  labs(x = NULL,
       color = "Sexual dimorphism") +
  facet_wrap(~Condition)

#### Plot----
ggarrange(p_mancheck_masc, p_mancheck_attr,
          legend = "none",
          labels = "auto")  |> 
  annotate_figure(bottom = text_grob("Sexual dimorphism",
                                  size = 11))

# Model 1: DFF----

## Data----
dat_m1 <- dat |> 
  select(DFF, Condition, Relationship, Sexual_dimorphism,
         ID, Stimulus,
         Freq_partner_physical_violence, Freq_partner_sexual_violence,
         Freq_partner_infidelity, Men_perceived_as_dangerous,
         Perceived_home_safety) |> 
  filter(DFF >= 100 & DFF <= 1000) |> 
  drop_na()

## Model 1: DFF---- 
mod1 <- lmer(DFF ~ Condition * Relationship * Sexual_dimorphism +
               (1 + Sexual_dimorphism| ID) + (1 | Stimulus), 
             data = dat_m1)
anova(mod1)

### Contrastes post-hoc----

#### Efecto principal: Sexual_dimorphism----
emmeans(mod1, pairwise ~ Sexual_dimorphism)

#### Diseño completo----
emmeans(mod1, pairwise ~ Sexual_dimorphism | Relationship + Condition)

#### Plot----
cond_labs <- c("Condition: High", "Condition: Low")
names(cond_labs) <- c("High", "Low")

emms_m1 <- as.data.frame(emmeans(mod1,
                                 ~ Sexual_dimorphism + Condition + Relationship))

contr_m1 <- as.data.frame(pairs(emmeans(mod1,
                                        ~ Sexual_dimorphism | Condition + Relationship))) |> 
  separate(contrast, c("group1", "group2"), " - ") |> 
  mutate(p.signif = stars.pval(p.value))

pm1 <- ggplot(emms_m1, aes(y = emmean, x = Sexual_dimorphism, color = Relationship)) +
  geom_errorbar(aes(ymin = emmean-SE,
                    ymax = emmean+SE,
                    group = Relationship), 
                color = "black",
                width=.1,
                position = position_dodge(0.9)) +
  geom_point(position = position_dodge(0.9), size = 1) +
  geom_line(aes(group = Relationship),
            position = position_dodge(0.9)) + 
  stat_pvalue_manual(contr_m1, 
                     label = "p.signif", 
                     y.position = c(266, NA, NA, 268),
                     color = "Relationship", hide.ns = TRUE,
                     position = position_dodge(),
                     tip.length = 0) +
  labs(y = "Duration of First Fixation (DFF)",
       x = NULL) +
  facet_grid(~ Condition,
             labeller = labeller(Condition = cond_labs)) 

# Model 2: TFD----

## Data----
dat_m2 <- dat |> 
  select(TFD, Condition, Relationship, Sexual_dimorphism,
         ID, Stimulus, 
         Freq_partner_physical_violence, Freq_partner_sexual_violence,
         Freq_partner_infidelity, Men_perceived_as_dangerous,
         Perceived_home_safety) |> 
  group_by(ID, Stimulus, Relationship) |> 
  filter(!(sum(TFD[Sexual_dimorphism == "Masculinized"] == 0) > 0 & 
             sum(TFD[Sexual_dimorphism == "Feminized"] == 0) > 0)) |>
  ungroup() |> 
  drop_na()

## Model 2: TFD----
mod2 <- lmer(TFD ~ Condition * Relationship * Sexual_dimorphism +
               (1 + Sexual_dimorphism | ID) + (1 | Stimulus), 
             data = dat_m2,
             control = lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))
anova(mod2)

### Contrastes post-hoc----

#### Efecto principal: Sexual_dimorphism----
emmeans(mod2, pairwise ~ Sexual_dimorphism)

#### Interacción: Condition:Sexual_dimorphism----
#emmeans(mod2, pairwise ~ Sexual_dimorphism | Condition)

#### Interacción: Relationship:Sexual_dimorphism---- 
emmeans(mod2, pairwise ~ Sexual_dimorphism | Relationship)

#### Diseño completo----
emmeans(mod2, pairwise ~ Sexual_dimorphism | Relationship + Condition)

#### Plot----
emms_m2 <- as.data.frame(emmeans(mod2,
                                 ~ Sexual_dimorphism + Condition + Relationship))

contr_m2 <- as.data.frame(pairs(emmeans(mod2,
                                        ~ Sexual_dimorphism | Condition + Relationship))) |> 
  separate(contrast, c("group1", "group2"), " - ") |> 
  mutate(p.signif = stars.pval(p.value))

pm2 <- ggplot(emms_m2, aes(y = emmean, x = Sexual_dimorphism, color = Relationship)) +
  geom_errorbar(aes(ymin = emmean-SE,
                    ymax = emmean+SE,
                    group = Relationship), 
                color = "black",
                width=.1,
                position = position_dodge(0.9)) +
  geom_point(position = position_dodge(0.9), size = 1) +
  geom_line(aes(group = Relationship),
            position = position_dodge(0.9)) + 
  stat_pvalue_manual(contr_m2, 
                     label = "p.signif", 
                     y.position = c(980, 990, 990, 1000),
                     color = "Relationship", hide.ns = TRUE,
                     position = position_dodge(),
                     tip.length = 0) +
  labs(y = "Total Fixation Duration  (TFD)",
       x = NULL) +
  facet_grid(~ Condition,
             labeller = labeller(Condition = cond_labs))

# Model 3: NF----

## Data----
dat_m3 <- dat |> 
  select(NF, Condition, Relationship, Sexual_dimorphism,
         ID, Stimulus, 
         Freq_partner_physical_violence, Freq_partner_sexual_violence,
         Freq_partner_infidelity, Men_perceived_as_dangerous,
         Perceived_home_safety) |> 
  group_by(ID, Stimulus, Relationship) |> 
  filter(!(sum(NF[Sexual_dimorphism == "Masculinized"] == 0) > 0 & 
             sum(NF[Sexual_dimorphism == "Feminized"] == 0) > 0)) |>
  ungroup() |> 
  drop_na()

## Model 3: NF----
mod3 <- lmer(NF ~ Condition * Relationship * Sexual_dimorphism +
                (1 + Sexual_dimorphism | ID) + (1 | Stimulus), 
              data = dat_m3)

anova(mod3)

### Contrastes post-hoc----

#### Efecto principal: Sexual_dimorphism----
emmeans(mod3, pairwise ~ Sexual_dimorphism)

#### Interacción: Condition:Relationship---- 
emmeans(mod3, pairwise ~ Relationship | Condition)

#### Interacción: Relationship:Sexual_dimorphism---- 
emmeans(mod3, pairwise ~ Sexual_dimorphism | Relationship)

#### Diseño completo----
emmeans(mod3, pairwise ~ Sexual_dimorphism | Relationship + Condition)

#### Plot----
emms_m3 <- as.data.frame(emmeans(mod3,
                                 ~ Sexual_dimorphism + Condition + Relationship))

contr_m3 <- as.data.frame(pairs(emmeans(mod3,
                                        ~ Sexual_dimorphism | Condition + Relationship))) |> 
  separate(contrast, c("group1", "group2"), " - ") |> 
  mutate(p.signif = stars.pval(p.value))

pm3 <- ggplot(emms_m3, aes(y = emmean, x = Sexual_dimorphism, color = Relationship)) +
  geom_errorbar(aes(ymin = emmean-SE,
                    ymax = emmean+SE,
                    group = Relationship), 
                color = "black",
                width=.1,
                position = position_dodge(0.9)) +
  geom_point(position = position_dodge(0.9), size = 1) +
  geom_line(aes(group = Relationship),
            position = position_dodge(0.9)) + 
  stat_pvalue_manual(contr_m3, 
                     label = "p.signif", 
                     y.position = c(3.75, 3.75, 3.77, 3.77),
                     color = "Relationship", hide.ns = TRUE,
                     position = position_dodge(),
                     tip.length = 0) +
  labs(y = "Number of Fixations  (NF)",
       x = NULL) +
  facet_grid(~ Condition,
             labeller = labeller(Condition = cond_labs))

# Model 4: Choice----

## Data----
dat_m4  <- dat |> 
  mutate(Choice = as.numeric(recode(Choice,
                                    "Yes" =  "1",
                                    "No" = "0"))) |> 
  group_by(ID, Stimulus, Relationship) |> 
  filter(!(sum(Choice[Sexual_dimorphism == "Masculinized"] == 0) > 0 & 
             sum(Choice[Sexual_dimorphism == "Feminized"] == 0) > 0)) |>
  ungroup() |> 
  group_by(ID, Sexual_dimorphism, Relationship, Condition,
           Freq_partner_physical_violence,
           Freq_partner_sexual_violence,
           Freq_partner_infidelity,
           Men_perceived_as_dangerous,
           Perceived_home_safety) |> 
  summarise(Choice = sum(Choice)) |> 
  ungroup() |> 
  mutate(Choice_prop = Choice/60) |> 
  drop_na()

## Model 4: Choice----
mod4 <- lm(Choice_prop ~ Condition * Relationship * Sexual_dimorphism,
            data = dat_m4)

anova(mod4)

#check_model(mod4)
#check_distribution(mod4)

### Contrastes post-hoc----

#### Efecto principal: Sexual_dimorphism----
emmeans(mod4, pairwise ~ Sexual_dimorphism)

#### Interacción: Relationship:Sexual_dimorphism---- 
emmeans(mod4, pairwise ~ Sexual_dimorphism | Relationship)

#### Diseño completo----
emmeans(mod4, pairwise ~ Sexual_dimorphism | Relationship + Condition)

#### Plot----
emms_m4 <- as.data.frame(emmeans(mod4,
                                 ~ Sexual_dimorphism + Condition + Relationship))

contr_m4 <- as.data.frame(pairs(emmeans(mod4,
                                        ~ Sexual_dimorphism | Condition + Relationship))) |> 
  separate(contrast, c("group1", "group2"), " - ") |> 
  mutate(p.signif = stars.pval(p.value))

pm4 <- ggplot(emms_m4, aes(y = emmean, x = Sexual_dimorphism, color = Relationship)) +
  geom_errorbar(aes(ymin = emmean-SE,
                    ymax = emmean+SE,
                    group = Relationship), 
                color = "black",
                width=.1,
                position = position_dodge(0.9)) +
  geom_point(position = position_dodge(0.9), size = 1) +
  geom_line(aes(group = Relationship),
            position = position_dodge(0.9)) + 
  stat_pvalue_manual(contr_m4, 
                     label = "p.signif", 
                     y.position = c(0.38, 0.38, 0.4, 0.4),
                     color = "Relationship", hide.ns = TRUE,
                     position = position_dodge(),
                     tip.length = 0) +
  labs(y = "Stimuli Choice (proportion)",
       x = NULL) +
  facet_grid(~ Condition,
             labeller = labeller(Condition = cond_labs))

## Final plots----
ggarrange(pm1, pm2, pm3, pm4,
          common.legend = TRUE,
          legend = "right",
          labels = "auto") |> 
  annotate_figure(bottom = text_grob("Sexual dimorphism",
                                    size = 12))

# Covariates and model selection----

## DFF----
### Models----
mod1a <- lmer(DFF ~
                Condition * Relationship * Sexual_dimorphism * Men_perceived_as_dangerous +
                (1 + Sexual_dimorphism | ID) + (1 | Stimulus), 
              data = dat_m1)

mod1b <- lmer(DFF ~
                Condition * Relationship * Sexual_dimorphism * Freq_partner_physical_violence +
                (1 + Sexual_dimorphism | ID) + (1 | Stimulus), 
              data = dat_m1)

mod1c <- lmer(DFF ~
                Condition * Relationship * Sexual_dimorphism * Freq_partner_sexual_violence +
                (1 + Sexual_dimorphism | ID) + (1 | Stimulus), 
              data = dat_m1)

mod1d <- lmer(DFF ~
                Condition * Relationship * Sexual_dimorphism * Freq_partner_infidelity +
                (1 + Sexual_dimorphism | ID) + (1 | Stimulus), 
              data = dat_m1)

mod1e <- lmer(DFF ~
                Condition * Relationship * Sexual_dimorphism * Perceived_home_safety +
                (1 + Sexual_dimorphism | ID) + (1 | Stimulus), 
              data = dat_m1)

### Best model----

aic_m1 <- AICctab(mod1, mod1a, mod1b, mod1c, mod1d, mod1e,
                  base = TRUE, weights = TRUE)
aic_m1

best_m1 <- eval(parse(text = rownames(data.frame(aic_m1))[1]))

anova(best_m1)

#### Plot best model----
covar_best_m1 <- best_m1@frame |> 
  select(where(is.numeric)) |> 
  select(-1)

covar_name <- colnames(covar_best_m1)

covar_best_m1_levels <- c(min(covar_best_m1[,1]),
                          #median(covar_best_m1[,1]),
                          max(covar_best_m1[,1]))
covar_labs <- paste0(c("Min = ", "Max = "),
                     round(covar_best_m1_levels, 3))
names(covar_labs) <- covar_best_m1_levels

cond_labs <- c("Condition: High", "Condition: Low")
names(cond_labs) <- c("High", "Low")

emms_best_m1 <- as.data.frame(emmeans(best_m1,
                                      ~ Sexual_dimorphism + Condition + Relationship + Freq_partner_physical_violence,
                             at = list(Freq_partner_physical_violence = covar_best_m1_levels))) |> 
  mutate(Freq_partner_physical_violence = round(Freq_partner_physical_violence, 3))

contr_best_m1 <- as.data.frame(pairs(emmeans(best_m1,
                                  ~ Sexual_dimorphism | Condition + Relationship + Freq_partner_physical_violence,
                                  at = list(Freq_partner_physical_violence = covar_best_m1_levels)))) |> 
  separate(contrast, c("group1", "group2"), " - ") |> 
  mutate(p.signif = stars.pval(p.value))

pm1f <- ggplot(emms_best_m1, aes(y = emmean, x = Sexual_dimorphism, color = Relationship)) +
  geom_errorbar(aes(ymin = emmean-SE,
                    ymax = emmean+SE,
                    group = Relationship), 
                color = "black",
                width=.1,
                position = position_dodge(0.9)) +
  geom_point(position = position_dodge(0.9), size = 1) +
  geom_line(aes(group = Relationship),
            position = position_dodge(0.9)) + 
  stat_pvalue_manual(contr_best_m1, label = "p.signif", 
                     y.position = c(300, NA, 280, 300,
                                    NA, NA, 320, NA),
                     color = "Relationship", hide.ns = TRUE,
                     position = position_dodge(),
                     tip.length = 0) +
  labs(y = "Duration of First Fixation (DFF)",
       x = NULL) +
  facet_grid(Condition ~ Freq_partner_physical_violence,
             labeller = labeller(Condition = cond_labs,
                                 Freq_partner_physical_violence = covar_labs)) 


## TFD----
### Model----

### Models----
mod2a <- lmer(TFD ~
                Condition * Relationship * Sexual_dimorphism * Men_perceived_as_dangerous +
                (1 + Sexual_dimorphism | ID) + (1 | Stimulus), 
              data = dat_m2)

mod2b <- lmer(TFD ~
                Condition * Relationship * Sexual_dimorphism * Freq_partner_physical_violence +
                (1 + Sexual_dimorphism | ID) + (1 | Stimulus), 
              data = dat_m2)

mod2c <- lmer(TFD ~
                Condition * Relationship * Sexual_dimorphism * Freq_partner_sexual_violence +
                (1 + Sexual_dimorphism | ID) + (1 | Stimulus), 
              data = dat_m2)

mod2d <- lmer(TFD ~
                Condition * Relationship * Sexual_dimorphism * Freq_partner_infidelity +
                (1 + Sexual_dimorphism | ID) + (1 | Stimulus), 
              data = dat_m2)

mod2e <- lmer(TFD ~
                Condition * Relationship * Sexual_dimorphism * Perceived_home_safety +
                (1 + Sexual_dimorphism | ID) + (1 | Stimulus), 
              data = dat_m2)

### Best model----

aic_m2 <- AICctab(mod2, mod2a, mod2b, mod2c, mod2d, mod2e,
                  base = TRUE, weights = TRUE)
aic_m2

best_m2 <- eval(parse(text = rownames(data.frame(aic_m2))[1]))

anova(best_m2)

#### Plot best model----
covar_best_m2 <- best_m2@frame |> 
  select(where(is.numeric)) |> 
  select(-1)

covar_name <- colnames(covar_best_m2)

covar_best_m2_levels <- c(min(covar_best_m2[,1]),
                          #median(covar_best_m1[,1]),
                          max(covar_best_m2[,1]))
covar_labs <- paste0(c("Min = ", "Max = "),
                     round(covar_best_m2_levels, 3))
names(covar_labs) <- covar_best_m2_levels

cond_labs <- c("Condition: High", "Condition: Low")
names(cond_labs) <- c("High", "Low")

emms_best_m2 <- as.data.frame(emmeans(best_m2,
                                      ~ Sexual_dimorphism + Condition + Relationship + Freq_partner_physical_violence,
                                      at = list(Freq_partner_physical_violence = covar_best_m1_levels))) |> 
  mutate(Freq_partner_physical_violence = round(Freq_partner_physical_violence, 3))

contr_best_m2 <- as.data.frame(pairs(emmeans(best_m2,
                                             ~ Sexual_dimorphism | Condition + Relationship + Freq_partner_physical_violence,
                                             at = list(Freq_partner_physical_violence = covar_best_m2_levels)))) |> 
  separate(contrast, c("group1", "group2"), " - ") |> 
  mutate(p.signif = stars.pval(p.value))

pm2f <- ggplot(emms_best_m2, aes(y = emmean, x = Sexual_dimorphism, color = Relationship)) +
  geom_errorbar(aes(ymin = emmean-SE,
                    ymax = emmean+SE,
                    group = Relationship), 
                color = "black",
                width=.1,
                position = position_dodge(0.9)) +
  geom_point(position = position_dodge(0.9), size = 1) +
  geom_line(aes(group = Relationship),
            position = position_dodge(0.9)) + 
  stat_pvalue_manual(contr_best_m2, label = "p.signif", y.position = c(1000, 1000, 1100, 1100,
                                                                       1500, 1400, 1600, 1500),
                     color = "Relationship", hide.ns = TRUE,
                     position = position_dodge(),
                     tip.length = 0) +
  labs(y = "Total Fixation Duration (TFD)",
       x = NULL) +
  facet_grid(Condition ~ Freq_partner_physical_violence,
             labeller = labeller(Condition = cond_labs,
                                 Freq_partner_physical_violence = covar_labs)) 

## NF----

### Models----
mod3a <- lmer(NF ~
                Condition * Relationship * Sexual_dimorphism * Men_perceived_as_dangerous +
                (1 + Sexual_dimorphism | ID) + (1 | Stimulus), 
              data = dat_m3)

mod3b <- lmer(NF ~
                Condition * Relationship * Sexual_dimorphism * Freq_partner_physical_violence +
                (1 + Sexual_dimorphism | ID) + (1 | Stimulus), 
              data = dat_m3)

mod3c <- lmer(NF ~
                Condition * Relationship * Sexual_dimorphism * Freq_partner_sexual_violence +
                (1 + Sexual_dimorphism | ID) + (1 | Stimulus), 
              data = dat_m3)

mod3d <- lmer(NF ~
                Condition * Relationship * Sexual_dimorphism * Freq_partner_infidelity +
                (1 + Sexual_dimorphism | ID) + (1 | Stimulus), 
              data = dat_m3)

mod3e <- lmer(NF ~
                Condition * Relationship * Sexual_dimorphism * Perceived_home_safety +
                (1 + Sexual_dimorphism | ID) + (1 | Stimulus), 
              data = dat_m3)

### Best model----

aic_m3 <- AICctab(mod3, mod3a, mod3b, mod3c, mod3d, mod3e,
                  base = TRUE, weights = TRUE)
aic_m3

best_m3 <- eval(parse(text = rownames(data.frame(aic_m3))[1]))

anova(best_m3)

#### Plot best model----
covar_best_m3 <- best_m3@frame |> 
  select(where(is.numeric)) |> 
  select(-1)

covar_name <- colnames(covar_best_m3)

covar_best_m3_levels <- c(min(covar_best_m3[,1]),
                          #median(covar_best_m1[,1]),
                          max(covar_best_m3[,1]))
covar_labs <- paste0(c("Min = ", "Max = "),
                     round(covar_best_m3_levels, 3))
names(covar_labs) <- covar_best_m3_levels

cond_labs <- c("Condition: High", "Condition: Low")
names(cond_labs) <- c("High", "Low")

emms_best_m3 <- as.data.frame(emmeans(best_m3,
                                      ~ Sexual_dimorphism + Condition + Relationship + Freq_partner_physical_violence,
                                      at = list(Freq_partner_physical_violence = covar_best_m3_levels))) |> 
  mutate(Freq_partner_physical_violence = round(Freq_partner_physical_violence, 3))

contr_best_m3 <- as.data.frame(pairs(emmeans(best_m3,
                                             ~ Sexual_dimorphism | Condition + Relationship + Freq_partner_physical_violence,
                                             at = list(Freq_partner_physical_violence = covar_best_m3_levels)))) |> 
  separate(contrast, c("group1", "group2"), " - ") |> 
  mutate(p.signif = stars.pval(p.value))

pm3f <- ggplot(emms_best_m3, aes(y = emmean, x = Sexual_dimorphism, color = Relationship)) +
  geom_errorbar(aes(ymin = emmean-SE,
                    ymax = emmean+SE,
                    group = Relationship), 
                color = "black",
                width=.1,
                position = position_dodge(0.9)) +
  geom_point(position = position_dodge(0.9), size = 1) +
  geom_line(aes(group = Relationship),
            position = position_dodge(0.9)) + 
  stat_pvalue_manual(contr_best_m3, label = "p.signif", y.position = c(4, 4.5, 4.5, 4,
                                                                       6.3, 6.5, 5.8, 7),
                     color = "Relationship", hide.ns = TRUE,
                     position = position_dodge(),
                     tip.length = 0) +
  labs(y = "Number of Fixations (NF)",
       x = NULL) +
  facet_grid(Condition ~ Freq_partner_physical_violence,
             labeller = labeller(Condition = cond_labs,
                                 Freq_partner_physical_violence = covar_labs)) 

## Choice----
### Models----
mod4a <- lm(Choice_prop ~
              Condition * Relationship * Sexual_dimorphism * Men_perceived_as_dangerous,
            data = dat_m4)

mod4b <- lm(Choice_prop ~
              Condition * Relationship * Sexual_dimorphism * Freq_partner_physical_violence,
            data = dat_m4)

mod4c <- lm(Choice_prop ~
              Condition * Relationship * Sexual_dimorphism * Freq_partner_sexual_violence,
            data = dat_m4)

mod4d <- lm(Choice_prop ~
              Condition * Relationship * Sexual_dimorphism * Freq_partner_infidelity,
            data = dat_m4)

mod4e <- lm(Choice_prop ~
              Condition * Relationship * Sexual_dimorphism * Perceived_home_safety,
            data = dat_m4)

### Best model----

aic_m4 <- AICctab(mod4, mod4a, mod4b, mod4c, mod4d, mod4e,
                  base = TRUE, weights = TRUE)
aic_m4

best_m4 <- eval(parse(text = rownames(data.frame(aic_m4))[1]))

anova(best_m4)

#### Plot best model----
covar_best_m4 <- best_m4$model |> 
  select(where(is.numeric)) |> 
  select(-1)

covar_name <- colnames(covar_best_m4)

covar_best_m4_levels <- c(min(covar_best_m4[,1]),
                          #median(covar_best_m1[,1]),
                          max(covar_best_m4[,1]))
covar_labs <- paste0(c("Min = ", "Max = "),
                     round(covar_best_m4_levels, 3))
names(covar_labs) <- covar_best_m4_levels

cond_labs <- c("Condition: High", "Condition: Low")
names(cond_labs) <- c("High", "Low")

emms_best_m4 <- as.data.frame(emmeans(best_m4,
                                      ~ Sexual_dimorphism + Condition + Relationship + Freq_partner_physical_violence,
                                      at = list(Freq_partner_physical_violence = covar_best_m4_levels))) |> 
  mutate(Freq_partner_physical_violence = round(Freq_partner_physical_violence, 3))

contr_best_m4 <- as.data.frame(pairs(emmeans(best_m4,
                                             ~ Sexual_dimorphism | Condition + Relationship + Freq_partner_physical_violence,
                                             at = list(Freq_partner_physical_violence = covar_best_m4_levels)))) |> 
  separate(contrast, c("group1", "group2"), " - ") |> 
  mutate(p.signif = stars.pval(p.value))

pm4f <- ggplot(emms_best_m4, aes(y = emmean, x = Sexual_dimorphism, color = Relationship)) +
  geom_errorbar(aes(ymin = emmean-SE,
                    ymax = emmean+SE,
                    group = Relationship), 
                color = "black",
                width=.1,
                position = position_dodge(0.9)) +
  geom_point(position = position_dodge(0.9), size = 1) +
  geom_line(aes(group = Relationship),
            position = position_dodge(0.9)) + 
  stat_pvalue_manual(contr_best_m4, label = "p.signif", y.position = c(0.45, 0.45, 0.55, 0.55,
                                                                       NA, 0.65, 0.55, NA),
                     color = "Relationship", hide.ns = TRUE,
                     position = position_dodge(),
                     tip.length = 0) +
  labs(y = "Stimuli Choice (proportion)",
       x = NULL) +
  facet_grid(Condition ~ Freq_partner_physical_violence,
             labeller = labeller(Condition = cond_labs,
                                 Freq_partner_physical_violence = covar_labs)) 

## Final plots----
ggarrange(pm1f, pm2f, pm3f, pm4f,
          common.legend = TRUE,
          legend = "right",
          labels = "auto") |> 
  annotate_figure(top = text_grob("Interaction with frequency of partner physical violence",
                                  size = 12),
                  bottom = text_grob("Sexual dimorphism",
                                     size = 12))

## Plot corr TFD and Choice
dat_corr_choice_TFD_dif <- dat_dif |> 
  group_by(ID, Relationship, Condition) |> 
  summarise(TFD_dif = mean(TFD_dif)) |> 
  left_join(dat_m4 |> 
              select(ID, Relationship, Condition, Choice_dif),
            by = c("ID", "Relationship", "Condition"))

ggplot(dat_corr_choice_TFD_dif, aes(x = Choice_dif, y = TFD_dif)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_grid(Condition ~ Relationship) +
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01, cor.coef.name = "rho", colour = "black") +
  stat_regline_equation(label.y = 800, colour = "black", size = 3)

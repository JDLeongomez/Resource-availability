# Cargar paquetes----
library(car)
library(ggstats)
library(tidyverse)
library(ggpubr)
library(readxl)
library(lmerTest)
library(emmeans)
library(knitr)
library(kableExtra)
library(performance)
library(MuMIn)
library(tictoc)
library(GGally)
library(scales) 

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

# Cargar datos----

## Eye-tracking----
dat_et <- read_excel("Datos/BD-ET-CUC-UB.xlsx", 
                     sheet = "CUC-UB") |> 
  select(-c(Participant, Condicion, TOI, Interval, Media_respuesta, AOI, AOI_Global, Respuesta, Number_of_mouse_clicks...17, Time_to_first_mouse_click...18, AOI_respuesta)) |> 
  rename(ID = Recording,
         University = UNIVERSIDAD,
         Stimulus = Media,
         Condition = Condición,
         Relationship = Contexto,
         Sexual_dimorphism = Rostro,
         TDF = Total_duration_of_whole_fixations,
         NF = Number_of_whole_fixations,
         TFF = Time_to_first_whole_fixation,
         NMC = Number_of_mouse_clicks...21,
         TFMC = Time_to_first_mouse_click...22) |> 
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
quests <- read_excel("Datos/Cuestionario Datos Sociodemográficos  (Disponibilidad) (respuestas) (1).xlsx", 
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

### Con puntajes totales de instrumentos, menos columnas
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
  mutate(across(where(is.character), as.factor)) 
  
## Evaluación subjestiva de rostros----
### Formato ancho
eval <- read_excel("Datos/Evaluación subjetiva rostros (Respuestas).xlsx") |> 
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
reg <- rbind(read_excel("Datos/3Registro Participantes Disponibilidad de Recursos-corregido.xlsx", 
                        sheet = "UB") |> 
               mutate(University = "UB"),
             read_excel("Datos/3Registro Participantes Disponibilidad de Recursos-corregido.xlsx", 
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
  filter(Control_question_1 == "No" & Control_question_2 == "No") |> 
  filter(Sexual_orientation %in% 
           c("Exclusively heterosexual",  
             "Predominantly heterosexual"))

### Tamaño de muestra----
n_filtrado <- dat |> 
  summarise(n = n_distinct(ID))

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
         Men_perceived_as_danger_to_children:Victim_of_violence,
         Victim_of_gender_violence:Victim_of_armed_conflict,
         Self_esteem:Food_insecurity) |> 
  mutate(across(where(is.character), as.factor))

### Sociodemographic numeric----
desc_quest |> 
  select(ID, Condition, where(is.numeric)) |> 
  pivot_longer(where(is.numeric),
               names_to = "Variable",
               values_to = "Value") |> 
  mutate(Variable = str_replace_all(Variable, "_", " ")) |>
  mutate(Variable = str_replace_all(Variable, "Freq", "Frequency of")) |>
  mutate(Variable = str_replace_all(Variable, "SP", "Self-perceived")) |>
  #mutate(Variable = case_when(str_detect(Variable, "_safety") ~ str_replace_all(Variable, "Self-perceived", "Perceived"))) |>
  ggplot(aes(x = Value, fill = Condition, color = Condition)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~Variable, scales = "free") +
  stat_summary(aes(xintercept = after_stat(x), y = 0),
               fun = mean, geom = "vline", orientation = "y") +
  labs(x = NULL, y = "Density")

### Sociodemographic categorical----
desc_quest |> 
  select(ID, Condition, where(is.factor), City) |> 
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
            select(ID, Condition, Freq_illness) |>
            pivot_longer(Freq_illness,
                         names_to = "Variable",
                         values_to = "Value") |> 
            mutate(Variable = str_replace_all(Variable, "_", " ")) |>
            mutate(Variable = str_replace_all(Variable, "Freq", "Frequency of")) |> 
            ggplot(aes(x = Value, fill = Condition, color = Condition)) +
            geom_density(alpha = 0.3) +
            facet_wrap(~Variable,
                       labeller = labeller(c("Frequency of illness"))) +
            stat_summary(aes(xintercept = after_stat(x), y = 0),
                         fun = mean, geom = "vline", orientation = "y") +
            labs(x = NULL, y = NULL),
          desc_quest |> 
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
  select(ID, Condition, starts_with("SP_")) |>
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
                             Men_perceived_as_danger_to_children,
                             Men_perceived_as_danger_to_partner) |> 
                      pivot_longer(Men_perceived_as_danger_to_children:Men_perceived_as_danger_to_partner,
                                   names_to = "Variable",
                                   values_to = "Value") |> 
                      mutate(Value = as.factor(Value)) |> 
                      mutate(Variable = str_replace_all(Variable, 
                                                        "Men_perceived_as_danger_to_", "")) |> 
                      mutate(Variable = str_to_sentence(Variable)) |> 
                      ggplot(aes(y = Value, fill = Condition, color = Condition)) +
                      geom_bar(alpha = 0.3, position = position_dodge()) +
                      geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 0.1)),
                                vjust = "inward",
                                position = position_dodge(.9),
                                stat = "prop",
                                color = "black",
                                size = 2.5) +
                      labs(title = "Men perceived as danger to...") +
                      facet_wrap(~Variable, scales = "free") +
                      scale_y_discrete(labels = label_wrap(20)) +
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
            select(ID, Condition, Freq_partner_physical_violence:Freq_partner_infidelity) |>
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

### Correlations table----
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
        align = c("l", rep("c", 22)),
        linesep = "",
        caption = "Correlations between XXXXXX",
        escape = FALSE) |>
  kable_styling(latex_options = c("HOLD_position"),
                font_size = 5) |>
  column_spec(1, width = "1.2cm") |>
  column_spec(2:23, width = "0.7cm") |>
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


# Manipulation check----
## Resource availability----

### Happiness----
mod_happ <- lm(Condition_happiness ~ Condition * Relationship_current, data = reg_fin) 
Anova(mod_happ, type = 3)
### Physical safety----
mod_phys_safety <- lm(Condition_physical_safety ~ Condition * Relationship_current, data = reg_fin) 
Anova(mod_phys_safety, type = 3)
### Health----
mod_health <- lm(Condition_healthy ~ Condition * Relationship_current, data = reg_fin) 
Anova(mod_health, type = 3)
### Economic security----
mod_econ_sec <- lm(Condition_economic_security ~ Condition * Relationship_current, data = reg_fin) 
Anova(mod_econ_sec, type = 3)

## Sexual dimorphism----
eval_desc <- eval_long |> 
  left_join(quests_fin, by = c("ID")) |> 
  mutate(Sexual_dimorphism = ifelse(grepl("F", Stimulus), "Feminine", "Masculine")) |> 
  group_by(ID, Sexual_dimorphism, Relationship_current) |> 
  summarise(Masculinity = mean(Masculinity),
            Attractiveness = mean(Attractiveness))
### Masculinity----
mod_masc <- lm(Masculinity ~ Sexual_dimorphism * Relationship_current, data = eval_desc)
Anova(mod_masc, type = 3)
### Attractiveness----
mod_attr <- lm(Attractiveness ~ Sexual_dimorphism * Relationship_current, data = eval_desc)
Anova(mod_attr, type = 3)

# Model 1: TFF----

## Data----
dat_m1 <- dat |> 
  select(TFF, Condition, Relationship, Sexual_dimorphism,
         Ovulating, Sexual_orientation, ID, Stimulus, Relationship_current) |> 
  drop_na()
### Partnered participants----
dat_m1_ptnr <- dat_m1 |>
  filter(Relationship_current == "Sí")
### Single participants----
dat_m1_sngl <- dat_m1 |>
  filter(Relationship_current == "No")





## Model 1: TFF General---- 
mod1 <- lmer(TFF ~ Condition * Relationship * Sexual_dimorphism + 
               Ovulating + Sexual_orientation +
               (1 | ID) + (1 | Stimulus), 
             data = dat_m1,
             na.action = "na.fail")
anova(mod1)

## Model 1: TFF Partnered---- 

# opt_mod1_ptnr <- allFit(mod1_ptnr)
# opt_mod1_ptnr_OK <- opt_mod1_ptnr[sapply(opt_mod1_ptnr, is, "merMod")]
# lapply(opt_mod1_ptnr_OK, function(x) x@optinfo$conv$lme4$messages)

mod1_ptnr <- lmer(TFF ~ Condition * Relationship * Sexual_dimorphism + 
                    Ovulating + Sexual_orientation +
                    (1 | ID) + (1 | Stimulus), 
                  data = dat_m1_ptnr,
                  control = lmerControl(optimizer = "bobyqa"))

anova(mod1_ptnr)

## Model 1: TFF Single---- 
mod1_sngl <- lmer(TFF ~ Condition * Relationship * Sexual_dimorphism + 
                    Ovulating + Sexual_orientation +
                    (1 | ID) + (1 | Stimulus), 
                  data = dat_m1_sngl)

anova(mod1_sngl)

# Model 2: TDF----

## Data----
dat_m2 <- dat |> 
  select(TDF, Condition, Relationship, Sexual_dimorphism,
         Ovulating, Sexual_orientation, ID, Stimulus, Relationship_current,
         Freq_partner_physical_violence, Freq_partner_sexual_violence,
         Freq_partner_infidelity) |> 
  drop_na()
### Partnered participants----
dat_m2_ptnr <- dat_m2 |>
  filter(Relationship_current == "Sí")
### Single participants----
dat_m2_sngl <- dat_m2 |>
  filter(Relationship_current == "No")

## Model 2: TDF General----
mod2 <- lmer(TDF ~ Condition * Relationship * Sexual_dimorphism + 
               Ovulating + Sexual_orientation +
               (1 | ID) + (1 | Stimulus), 
             data = dat_m2,
             na.action = "na.fail")
anova(mod2)

### Contrastes post-hoc----

#### Efecto principal: Sexual_dimorphism----
emmeans(mod2, pairwise ~ Sexual_dimorphism)
emmip(mod2, ~ Sexual_dimorphism, CIs = TRUE, type = "response")

#### Interacción: Condition:Sexual_dimorphism ----
emmeans(mod2, pairwise ~ Sexual_dimorphism | Condition)
emmip(mod2, ~ Sexual_dimorphism | Condition, CIs = TRUE, type = "response")

#### Interacción: Condition:Sexual_orientation ----
emmeans(mod2, pairwise ~ Sexual_orientation)
emmip(mod2, ~ Sexual_orientation, CIs = TRUE, type = "response")

#### Interacción: Relationship:Sexual_dimorphism----
emmeans(mod2, pairwise ~ Relationship | Sexual_dimorphism)
emmip(mod2, ~ Relationship | Sexual_dimorphism, CIs = TRUE, type = "response")

## Model 2: TDF Partnered----
mod2_ptnr <- lmer(TDF ~ Condition * Relationship * Sexual_dimorphism + 
                    Ovulating + Sexual_orientation +
                    (1 | ID) + (1 | Stimulus), 
                  data = dat_m2_ptnr,
                  na.action = "na.fail")
anova(mod2_ptnr)

### FALTA AGREGAR OTROS POST-HOCS!!----

## Model 2: TDF Single----
mod2_sngl <- lmer(TDF ~ Condition * Relationship * Sexual_dimorphism + 
                    Ovulating + Sexual_orientation +
                    (1 | ID) + (1 | Stimulus), 
                  data = dat_m2_sngl,
                  na.action = "na.fail")
anova(mod2_sngl)

### FALTA AGREGAR OTROS POST-HOCS!!----

# Model 3: NF----

## Data----
dat_m3 <- dat |> 
  select(NF, Condition, Relationship, Sexual_dimorphism,
         Ovulating, Sexual_orientation, ID, Stimulus, Relationship_current) |> 
  drop_na()
### Partnered participants----
dat_m3_ptnr <- dat_m3 |>
  filter(Relationship_current == "Sí")
### Single participants----
dat_m3_sngl <- dat_m3 |>
  filter(Relationship_current == "No")

## Model 3: NF General----
mod3 <- glmer(NF ~ Condition * Relationship * Sexual_dimorphism + 
                Ovulating + Sexual_orientation +
                (1 | ID) + (1 | Stimulus), 
              family = "poisson",
              data = dat_m3)

cbind(anova(mod3), 
      Anova(mod3, type = 3) |> 
        slice(-1))

check_model(mod3)

### Contrastes post-hoc----

#### Interacción: Relationship:Sexual_dimorphism----
emmeans(mod3, pairwise ~ Relationship | Sexual_dimorphism, type = "response")
emmip(mod3, ~ Relationship | Sexual_dimorphism, CIs = TRUE, type = "response")

## Model 3: NF Partnered---- 
mod3_ptnr <- glmer(NF ~ Condition * Relationship * Sexual_dimorphism + 
                     Ovulating + Sexual_orientation +
                     (1 | ID) + (1 | Stimulus), 
                   family = "poisson", 
                   data = dat_m3_ptnr)

cbind(anova(mod3_ptnr), 
      Anova(mod3_ptnr, type = 3) |> 
        slice(-1))

## Model 3: NF Single---- 
mod3_sngl <- glmer(NF ~ Condition * Relationship * Sexual_dimorphism + 
                     Ovulating + Sexual_orientation +
                     (1 | ID) + (1 | Stimulus), 
                   family = "poisson", 
                   data = dat_m3_sngl)

cbind(anova(mod3_sngl), 
      Anova(mod3_sngl, type = 3) |> 
        slice(-1))

# Model 4: Choice----

## Data----
dat_choice_yes  <- dat |> 
  filter(Choice == "Yes") |> 
  group_by(ID, Sexual_dimorphism, Relationship, Condition, Choice, 
           Relationship_current, Ovulating, Sexual_orientation) |> 
  summarise(Choice = n()) |> 
  group_by(ID, Sexual_dimorphism, Relationship, Condition, 
           Relationship_current, Ovulating, Sexual_orientation) |> 
  summarise(Choice_count = sum(Choice)) |> 
  drop_na()

### Partnered participants----
dat_choice_yes_ptnr <- dat_choice_yes |>
  filter(Relationship_current == "Sí")
### Single participants----
dat_choice_yes_sngl <- dat_choice_yes |>
  filter(Relationship_current == "No")

## Model 4: Choice General----
mod4 <- glm(Choice_count ~ Condition * Relationship * Sexual_dimorphism +
              Ovulating + Sexual_orientation, 
            family = "poisson",
            data = dat_choice_yes,
            na.action = "na.fail")

Anova(mod4, type = 3)

### Contrastes post-hoc----

#### Efecto principal: Relationship----
emmeans(mod4, pairwise ~ Relationship)
emmip(mod4, ~ Relationship, CIs = TRUE, type = "response")

#### Efecto principal: Sexual_dimorphism ----
emmeans(mod4, pairwise ~ Sexual_dimorphism)
emmip(mod4, ~ Sexual_dimorphism, CIs = TRUE, type = "response")

#### Interacción: Condition:Sexual_dimorphism----
emmeans(mod4, pairwise ~ Condition | Sexual_dimorphism)
emmip(mod4, ~ Condition | Sexual_dimorphism, CIs = TRUE, type = "response")

#### Interacción: Relationship:Sexual_dimorphism----
emmeans(mod4, pairwise ~ Relationship | Sexual_dimorphism)
emmip(mod4, ~ Relationship | Sexual_dimorphism, CIs = TRUE, type = "response")

#### Interacción: Condition:Relationship:Sexual_dimorphism----
emmeans(mod4, pairwise ~ Condition | Relationship + Sexual_dimorphism)
emmip(mod4, ~ Condition | Relationship + Sexual_dimorphism, CIs = TRUE, type = "response")

## Model 4: Choice Partnered---- 
mod4_ptnr <- glm(Choice_count ~ Condition * Relationship * Sexual_dimorphism +
                   Ovulating + Sexual_orientation, 
                 family = "poisson",
                 data = dat_choice_yes_ptnr,
                 na.action = "na.fail")

Anova(mod4_ptnr, type = 3)

### FALTA AGREGAR POST-HOCS!!----

## Model 4: Choice Single---- 
mod4_sngl <- glm(Choice_count ~ Condition * Relationship * Sexual_dimorphism +
                   Ovulating + Sexual_orientation, 
                 family = "poisson",
                 data = dat_choice_yes_sngl,
                 na.action = "na.fail")

Anova(mod4_sngl, type = 3)

### FALTA AGREGAR OTROS POST-HOCS!!----

#### Interacción: Condition:Relationship:Sexual_dimorphism----
emmeans(mod4_sngl, pairwise ~ Relationship | Sexual_dimorphism + Condition)
emmip(mod4_sngl, Sexual_dimorphism ~ Relationship | Condition, CIs = TRUE, type = "response")

# Covariates and model selection----

## Choice----
### Data----
dat_choice_yes_comp  <- dat |> 
  filter(Choice == "Yes") |> 
  group_by(ID, Sexual_dimorphism, Relationship, Condition, Choice, 
           Freq_partner_physical_violence,
           Freq_partner_sexual_violence,
           Freq_partner_infidelity,
           Relationship_current,
           Men_perceived_as_danger_to_partner) |> 
  summarise(Choice = n()) |> 
  group_by(ID, Sexual_dimorphism, Relationship, Condition,
           Freq_partner_physical_violence,
           Freq_partner_sexual_violence,
           Freq_partner_infidelity,
           Relationship_current) |> 
  summarise(Choice_count = sum(Choice))

dat_choice_yes_comp_yes <- dat_choice_yes_comp |> 
  filter(Relationship_current == "Sí")

dat_choice_yes_comp_no <- dat_choice_yes_comp |> 
  filter(Relationship_current == "No")

### Model----
mod4a <- glm(Choice_count ~ 
               Condition * Relationship * Sexual_dimorphism * Freq_partner_physical_violence +
               Condition * Relationship * Sexual_dimorphism * Freq_partner_sexual_violence +
               Condition * Relationship * Sexual_dimorphism * Freq_partner_infidelity, 
             family = "poisson",
             data = dat_choice_yes_comp,
             na.action = "na.fail")
### Dredge---
#tic()
#dr_m4a <- dredge(mod4a,
#                 fixed = ~Condition * Relationship * Sexual_dimorphism,
#                 trace = 2)
#toc()
#plot(dr_m4a)

### Model partnered----
mod4a_yes <- glm(Choice_count ~ 
                   Condition * Relationship * Sexual_dimorphism * Freq_partner_physical_violence +
                   Condition * Relationship * Sexual_dimorphism * Freq_partner_sexual_violence +
                   Condition * Relationship * Sexual_dimorphism * Freq_partner_infidelity, 
                 family = "poisson",
                 data = dat_choice_yes_comp_yes,
                 na.action = "na.fail")

mod4_yes <- glm(Choice_count ~ 
                  Condition * Relationship * Sexual_dimorphism, 
                family = "poisson",
                data = dat_choice_yes_comp_yes,
                na.action = "na.fail")
Anova(mod4_yes)

### Dredge---
#dr_m4a_yes <- dredge(mod4a_yes,
#                     fixed = ~Condition * Relationship * Sexual_dimorphism,
#                     trace = 2)
#
#plot(dr_m4a_yes)

### Model single----
mod4a_no <- glm(Choice_count ~ 
                  Condition * Relationship * Sexual_dimorphism * Freq_partner_physical_violence +
                  Condition * Relationship * Sexual_dimorphism * Freq_partner_sexual_violence +
                  Condition * Relationship * Sexual_dimorphism * Freq_partner_infidelity, 
                family = "poisson",
                data = dat_choice_yes_comp_no,
                na.action = "na.fail")

mod4_no <- glm(Choice_count ~ 
                 Condition * Relationship * Sexual_dimorphism, 
               family = "poisson",
               data = dat_choice_yes_comp_no,
               na.action = "na.fail")
Anova(mod4_no)

### Dredge---
#tic()
#dr_m4a_no <- dredge(mod4a_no,
#                    fixed = ~Condition * Relationship * Sexual_dimorphism,
#                    trace = 2)
#toc()
#plot(dr_m4a_no)





## TDF----
### Model----
tic()
mod5a <- lmer(TDF ~ Condition * Relationship * Sexual_dimorphism * Freq_partner_physical_violence +
                Condition * Relationship * Sexual_dimorphism * Freq_partner_sexual_violence +
                Condition * Relationship * Sexual_dimorphism * Freq_partner_infidelity + 
                #Ovulating + Sexual_orientation +
                (1 | ID) + (1 | Stimulus), 
              data = dat_m2,
              na.action = "na.fail")
toc()

### Dredge---
#tic()
#dr_m5a <- dredge(mod5a,
#                 fixed = ~Condition * Relationship * Sexual_dimorphism,
#                 trace = 2)
#toc()
#plot(dr_m5a)


mod2 <- lmer(TDF ~ Condition * Relationship * Sexual_dimorphism + 
               Ovulating + Sexual_orientation +
               (1 | ID) + (1 | Stimulus), 
             data = dat_m2,
             na.action = "na.fail")
anova(mod2)
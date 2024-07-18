# Cargar paquetes----
library(car)
library(tidyverse)
library(readxl)
library(lmerTest)
library(emmeans)

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
         Sex = Sexo,
         Gender = Genero,
         Sexual_orientation = OS,
         Relationship_current = "Pareja actual",
         Relationship_duration = DuracionR,
         Relationship_status = EstadoR,
         Partner_sex = SexoParejaActual,
         Partner_masculinity = Masculinidad_pareja,
         Parner_dominance = Dominancia_pareja,
         Parner_attractiveness = Atractivo_pareja,
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
         SP_selfconfidence = "AP autoconfianza",
         SP_self_esteem = "AP autoestima",              
         SP_health = "AP salud",
         Electricity = "SB electricidad",
         Internet = "SB internet",
         TV = "SB television",
         Freq_internet_use = "Fr acceso internet",
         Hospital_access = "Acceso hospital",
         Freq_illness = "Fr enfermedades",
         Socioeconomic_level = "Estrato socioeconomico",
         Neighborhood = "Barrio de residencia",
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
         Control_question_2 = "Broma")

### Con puntajes totales de instrumentos, menos columnas
quests_fin <- quests %>% 
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
         Perceived_security = sum(across(starts_with("Seguridad "))),
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
reg <- rbind(read_excel("Datos/Registro Participantes Disponibilidad de Recursos.xlsx", 
                        sheet = "UB") |> 
               mutate(University = "UB"),
             read_excel("Datos/Registro Participantes Disponibilidad de Recursos.xlsx", 
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
         Ovulation_test = "Test de ovulación",
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
                                  NULL = "Selecciona")) |> 
  mutate_all(~str_replace_all(., "SI", "Yes")) |> 
  mutate_all(~str_replace_all(., "NO", "No")) |> 
  mutate_all(~str_replace_all(., "INCOMPLETO", "No")) |>
  mutate_all(~str_replace_all(., "Recuperado", "Data recovered")) |> 
  mutate_all(~str_replace_all(., "RECUPERADO", "Data recovered")) |> 
  mutate_all(~na_if(., "Selecciona")) |> 
  mutate_all(~na_if(., "N/A"))

# Base de datos final----
dat <- dat_et |> 
  left_join(quests_fin, by = c("ID"), multiple = "all") |>
  left_join(eval_long, by = c("ID", "Stimulus"), multiple = "all") |> 
  left_join(reg, by = c("ID", "University", "Condition"), multiple = "all") 

# Model 1: TDF----

mod1 <- lmer(TDF ~ Condition * Relationship * Sexual_dimorphism + (1 | ID) + (1 | Stimulus), data = dat)
anova(mod1)

## Contrastes post-hoc----

### Efecto principal: Sexual_dimorphism----
emmeans(mod1, pairwise ~ Sexual_dimorphism)
emmip(mod1, ~ Sexual_dimorphism, CIs = TRUE)

### Interacción: Condition:Sexual_dimorphism ----
emmeans(mod1, pairwise ~ Sexual_dimorphism | Condition)
emmip(mod1, ~ Sexual_dimorphism | Condition, CIs = TRUE)

### Interacción: Relationship:Sexual_dimorphism----
emmeans(mod1, pairwise ~ Relationship | Sexual_dimorphism)
emmip(mod1, ~ Relationship | Sexual_dimorphism, CIs = TRUE)


# Model 2: Choice----

# Selecionar participantes con al menos 80% (48) respuestas 
select80 <- dat |> 
  filter(Choice == "Yes") |> 
  group_by(ID, Sexual_dimorphism, Relationship, Condition, Choice) |> 
  summarise(Choice = n()) |> 
  group_by(ID) |> 
  summarise(Choice_count = sum(Choice)) |> 
  filter(Choice_count >= 48)

dat_choice_yes <- dat |> 
  filter(Choice == "Yes") |> 
  group_by(ID, Sexual_dimorphism, Relationship, Condition, Choice) |> 
  summarise(Choice = n()) |> 
  filter(ID %in% select80$ID)

mod2 <- lm(Choice ~ Condition * Relationship * Sexual_dimorphism, data = dat_choice_yes)
Anova(mod2, type = 3)

## Contrastes post-hoc----

### Efecto principal: Relationship----
emmeans(mod2, pairwise ~ Relationship)
emmip(mod2, ~ Relationship, CIs = TRUE)

### Interacción: Sexual_dimorphism ----
emmeans(mod2, pairwise ~ Sexual_dimorphism)
emmip(mod2, ~ Sexual_dimorphism, CIs = TRUE)

### Interacción: Relationship:Sexual_dimorphism----
emmeans(mod2, pairwise ~ Relationship | Sexual_dimorphism)
emmip(mod2, ~ Relationship | Sexual_dimorphism, CIs = TRUE)



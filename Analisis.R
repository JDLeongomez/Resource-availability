library(tidyverse)
library(readxl)


# Cargar datos
dat_et <- read_excel("Datos/BD-ET-CUC-UB.xlsx", 
                     sheet = "CUC-UB") |> 
  select(-c(Participant, Condicion, TOI, Interval, AOI, AOI_Global, Respuesta, Number_of_mouse_clicks...17, Time_to_first_mouse_click...18, AOI_respuesta)) |> 
  rename(ID = Recording,
         University = UNIVERSIDAD,
         Stimulus = Media,
         Condition = Condición,
         Relationship = Contexto,
         Sexual_dimorphism = Rostro,
         TDF = Total_duration_of_whole_fixations,
         NF = Number_of_whole_fixations,
         TFF = Time_to_first_whole_fixation,
         Response_code = Media_respuesta,
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
                                        "Masculinized" = "Masculinizado"))

# Sin calcular puntajes totales de instrumentos, para ver consistencia interna
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
         Relationship = "Pareja actual",
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

quests_fin <- quests |> 
  mutate_at(vars(starts_with("Escasez alimentaria")),
            ~recode(.,
                    "Nunca" =  0,
                    "Rara vez/algunas veces" = 1,
                    "Casi siempre" = 2)) |> 
  rowwise() |> 
  mutate(Self_esteem = sum(autoestima_I1, 5-autoestima_I2, autoestima_I3, autoestima_I4,
                           autoestima_I5, 5-autoestima_I6, autoestima_I7, 5-autoestima_I8, 
                           5-autoestima_I9, autoestima_I10),
         Self_perception = sum(across(starts_with("SP_"))),
         Perceived_security = sum(across(starts_with("Seguridad "))),
         Food_insecurity = sum(across(starts_with("Escasez alimentaria")))) |> 
  select(!starts_with("autoestima_"))
  

eval <- read_excel("Datos/Evaluación subjetiva rostros (Respuestas).xlsx") |> 
  select(-c(123:124)) |> 
  rowwise() |> 
  mutate(Masculinity_masculinized = sum(across(ends_with("M Mas"))),
         Masculinity_feminized = sum(across(ends_with("F Mas"))),
         Attractiveness_masculinized = sum(across(ends_with("M Atr"))),
         Attractiveness_feminized = sum(across(ends_with("F Atr")))) |> 
  rename(Date = "Marca temporal",
         ID = "Escribe tu código de participante")


eval_atr <- eval |> 
  select(-c(123:126)) |> 
  select(!ends_with(" Mas")) |> 
  pivot_longer(cols = ends_with("Atr"),
               names_to = "Stimulus",
               values_to = "Attractiveness") |> 
  mutate(Stimulus = str_remove_all(Stimulus, " Atr"))

eval_mas <- eval |> 
  select(-c(123:126)) |> 
  select(!ends_with(" Atr")) |> 
  pivot_longer(cols = ends_with("Mas"),
               names_to = "Stimulus",
               values_to = "Masculinity") |> 
  mutate(Stimulus = str_remove_all(Stimulus, " Mas"))

eval_long <- left_join(eval_atr, eval_mas)

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
  select(-c(Invitado, `Servicios ayuda`)) |>
  
  

quests_fin <- quests |> 
  rowwise() |> 
  mutate(Self_esteem = sum(autoestima_I1, 5-autoestima_I2, autoestima_I3, autoestima_I4,
                           autoestima_I5, 5-autoestima_I6, autoestima_I7, 5-autoestima_I8, 
                           5-autoestima_I9, autoestima_I10),
         Self_perception = sum(across(starts_with("AP")))) |> 
  select(!starts_with("autoestima_"))
  




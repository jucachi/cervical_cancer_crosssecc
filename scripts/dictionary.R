# Description: Dictionary for the variables in the dataset

data_factors_0 <-
  data_factors |>
  dplyr::mutate(
    edad = ff_label(edad, "Age (years)"),
    edad.c = case_when(edad < 38 ~ "< 38", edad >= 38 & edad <= 50 ~ "38-50", edad > 50 ~ "> 50") |>
      fct_relevel("< 38", "38-50", "> 50") |>
      ff_label("Age (years)"),
    
    e_marital = factor(e_marital) |> ###
      fct_recode(
        "Married" = "casada",
        "Cohabiting" = "conviviente",
        "Single" = "soltera",
        "Widowed" = "viuda"
      ) |>
      fct_relevel("Married", "Cohabiting", "Single", "Widowed") |>
      ff_label("Marital status"),
    
    n_educacion = factor(n_educacion) |>
      fct_recode(
        "Primary or Secundary" = "primaria",
        "Primary or Secundary" = "secundaria",
        "Higher than Secondary" = "superior"
      ) |>
      fct_relevel("Primary or Secundary", "Higher than Secondary") |>
      ff_label("Education level"),
    
    religion = factor(religion) |> ###
      fct_recode(
        "None" = "ninguna",
        "Catholic" = "catolico",
        "Evangelical" = "evangelista"
      ) |>
      fct_relevel("None", "Catholic", "Evangelical") |>
      ff_label("Religion"),
    
    etnia = factor(etnia) |> ###
      fct_recode(
        "Mestizo" = "mestizo",
        "White" = "blanco",
        "Other" = "otro"
      ) |>
      fct_relevel("Mestizo", "White", "Other") |>
      ff_label("Ethnicity"),
    
    procedencia = factor(procedencia) |> ###
      fct_recode("Urban" = "urbano", "Rural" = "rural") |>
      fct_relevel("Urban", "Rural") |>
      ff_label("Origin"),
    
    ocupacion = factor(ocupacion) |> ###
      fct_recode(
        "Student" = "estudiante",
        "Employed" = "empleada",
        "Homemaker" = "ama de casa",
        "Unemployed" = "sin empleo",
        "Other" = "otro",
      ) |>
      fct_relevel("Student", "Employed", "Homemaker", "Unemployed", "Other") |>
      ff_label("Occupation"),
    
    ocupacion_convi = factor(ocupacion_convi) |> ###
      fct_recode(
        "Student" = "estudiante",
        "Employed" = "empleado",
        "Unemployed" = "sin empleo",
        "Other" = "otro"
      ) |>
      fct_relevel("Student", "Employed", "Unemployed", "Other") |>
      ff_label("Partner's occupation"),
    
    antec_fam = factor(antec_fam) |> ###
      fct_recode("Yes" = "si", "No" = "no") |>
      fct_relevel("Yes", "No") |>
      ff_label("Family history"),
    
    edad_relacion_sexual.c = case_when(
      edad_relacion_sexual <= 18  ~ "18 or younger",
      edad_relacion_sexual >= 19 &
        edad_relacion_sexual <= 21 ~ "19-21",
      edad_relacion_sexual >= 22 ~ "22 or older"
    ) |>
      fct_relevel("18 or younger", "19-21", "22 or older") |>
      ff_label("Age at first sexual intercourse"),
    
    parejas_sex.c = case_when(
      parejas_sex <= 1 ~ "0 or 1",
      parejas_sex == 2 ~ "2",
      parejas_sex == 3 ~ "3",
      parejas_sex >= 4 ~ "4 or more"
    ) |>
      fct_relevel("0 or 1", "2", "3", "4 or more") |>
      ff_label("Number of sexual partners"),
    
    num_hijos.c = case_when(
      ### Changed
      num_hijos == 0 ~ "0",
      num_hijos == 1 ~ "1",
      num_hijos == 2 ~ "2",
      num_hijos >= 3 ~ "3 or more"
    ) |>
      fct_relevel("0", "1", "2", "3 or more") |>
      ff_label("Number of children"),
    
    met_anticoncep = factor(met_anticoncep) |> # Changed
      fct_recode(
        "Not using" = "no uso",
        "Oral contraceptives" = "aco",
        "Injection" = "ampolla de 3 meses",
        "Injection" = "ampolla mes",
        "IUD" = "diu",
        "Subdermal implant" = "implante_sd"
      )  |>
      fct_relevel(
        "Not using",
        "Oral contraceptives",
        "Injection",
        "IUD",
        "Subdermal implant"
      ) |>
      ff_label("Contraceptive method"),
    
    antec_ets = factor(antec_ets) |> ###
      fct_recode("Yes" = "si", "No" = "no") |>
      fct_relevel("Yes", "No") |>
      ff_label("History of STIs"),
    
    conocimiento = case_when(
      conocimiento == "no conoce" |
        conocimiento == "bajo" ~ "Low/No knowledge",
      conocimiento == "medio" ~ "Medium" ,
      TRUE ~ "High"
    ) |>
      fct_relevel("Low/No knowledge", "Medium", "High") |>
      ff_label("Knowledge level"),
    
    conocimiento_dico = case_when(
      conocimiento == "Low/No knowledge" |
        conocimiento == "medio" ~ "Medium to No knowledge",
      TRUE ~ "High"
    ) |>
      fct_relevel("Medium to No knowledge", "High") |>
      ff_label("Knowledge level"),
    
    actitud = case_when(actitud == "negativa" ~ "Negative", TRUE ~ "Positive") |>
      fct_relevel("Negative", "Positive") |>
      ff_label("Attitude"),
    
    practica = case_when(practica == "incorrecta" ~ "Incorrect", TRUE ~ "Correct") |>
      fct_relevel("Incorrect", "Correct") |>
      ff_label("Practices")
  )

# Recode and relabel attitude data
attitudes <- attitudes_data |>
  janitor::clean_names() |>
  dplyr::mutate(across(
    everything(),
    ~ case_when(
      . == "No estoy segura" ~ "Not sure",
      . == "No estoy seguro" ~ "Not sure",
      . == "Si" ~ "Yes",
      TRUE ~ "No"
    ) |>
      factor(levels = c("No", "Not sure", "Yes"))
  ))

# Recode and relabel practices data
practices <- practices_data |>
  janitor::clean_names() |>
  dplyr::mutate(across(
    everything(),
    ~ case_when(. == "Si" ~ "Yes", TRUE ~ "No")))


# Recode and relabel knowledge data
knowledges <- knowledges_data |>
  janitor::clean_names() |>
  dplyr::mutate(across(
    everything(),
    ~ case_when(. == "No sé" ~ "Don't know", . == "Si" ~ "Yes", TRUE ~ "No") |>
      factor(levels = c("No", "Don't know", "Yes"))
  ))

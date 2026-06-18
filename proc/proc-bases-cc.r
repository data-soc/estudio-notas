# ---- Procesamiento bases centradas en cursos ----

pacman::p_load(rio, digest, stargazer, sjPlot, codebook, summarytools, dplyr, tidyr, stringr,
 tidyLPA, lme4, ggplot2, ggeffects, skimr, rlang, table1, patchwork, here, kableExtra, ggthemes)


options(scipen = 999)

notas25 <- read.csv("input/data/raw_data/notas-cohorte-2025.csv")
notas24 <- read.csv("input/data/raw_data/notas-cohorte-2024.csv")
notas23 <- read.csv("input/data/raw_data/notas-cohorte-2023.csv")
notas22 <- read.csv("input/data/raw_data/notas-cohorte-2022.csv")
notas21 <- read.csv("input/data/raw_data/notas-cohorte-2021.csv")
notas20 <- read.csv("input/data/raw_data/notas-cohorte-2020.csv")
notas19 <- read.csv("input/data/raw_data/notas-cohorte-2019.csv")
notas18 <- read.csv("input/data/raw_data/notas-cohorte-2018.csv")


# ---- Diagnóstico ----

# Los casos se repiten al contabilizarse en la carrera y la licenciatura -> revisando algunos casos nos dimos
# cuenta que repiten la misma información (cursos y nota) -> nos quedamos sólo con el título para simplificar el processing

# Los años de ingreso de algunos casos no coinciden con el año en que se tomaron algún curso 
# pero pareciera ser más fiable el semestre del ramo antes que el año de ingreso para estos casos
# debido a que los códigos de los cursos son distintos, dando a entender que son personas de cohortes maś antiguas

 

## Armonización

procesar_notas <- function(df) {
  
  moda <- function(x) {
    ux <- na.omit(unique(x))
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  niveles_semestre <- c("Primer semestre", "Segundo semestre", "Tercer semestre",
                        "Cuarto semestre", "Quinto semestre", "Sexto semestre",
                        "Séptimo semestre", "Octavo semestre", "Noveno semestre",
                        "Décimo semestre")
  
  df %>%
    select(RUT.Alumno, Género.Alumno, COLEGIO, NOTA,
           SEMESTRE, CODIGO, SECCION, ASIGNATURA, CARRERA, Año.de.Ingreso) %>%
    rename(
      rut     = RUT.Alumno,
      sexo    = Género.Alumno,
      colegio = COLEGIO,
      nota    = NOTA,
      ano     = SEMESTRE,
      codigo  = CODIGO,
      seccion = SECCION,
      curso   = ASIGNATURA,
      carrera = CARRERA,
      cohorte = Año.de.Ingreso
    ) %>%
    mutate(
      carrera = if_else(str_detect(carrera, "^Antropología"), "Antropología", carrera)
    ) %>%
    filter(carrera %in% c("Sociología", "Psicología", "Antropología",
                          "Trabajo Social", "Pedagogía en Educación Parvularia")) %>%
    mutate(
      cohorte        = as.numeric(cohorte) * 10,
      semestre_malla = ano - cohorte
    ) %>%
    group_by(codigo) %>%
    mutate(semestre_moda = moda(semestre_malla)) %>%
    ungroup() %>%
    mutate(
      semestre_label = case_when(
        semestre_moda == 1  ~ "Primer semestre",
        semestre_moda == 2  ~ "Segundo semestre",
        semestre_moda == 11 ~ "Tercer semestre",
        semestre_moda == 12 ~ "Cuarto semestre",
        semestre_moda == 21 ~ "Quinto semestre",
        semestre_moda == 22 ~ "Sexto semestre",
        semestre_moda == 31 ~ "Séptimo semestre",
        semestre_moda == 32 ~ "Octavo semestre",
        semestre_moda == 41 ~ "Noveno semestre",
        semestre_moda == 42 ~ "Décimo semestre",
        TRUE ~ NA_character_
      ),
      semestre_label = factor(semestre_label, levels = niveles_semestre, ordered = TRUE)
    )
}

notas25_proc <- procesar_notas(notas25)
notas24_proc <- procesar_notas(notas24)
notas23_proc <- procesar_notas(notas23)
notas22_proc <- procesar_notas(notas22)
notas21_proc <- procesar_notas(notas21)
notas20_proc <- procesar_notas(notas20)
notas19_proc <- procesar_notas(notas19)
notas18_proc <- procesar_notas(notas18)


bases <- list(notas18_proc, notas19_proc, notas20_proc, notas21_proc, notas22_proc, notas23_proc, notas24_proc)


cc_cursos <- bind_rows(bases)


cc_cursos <- cc_cursos %>%
  mutate(disonancia = ano - cohorte) %>% # eliminación de casos disonantes
  filter(disonancia >= 0)

prefijos <- c("ANT", "PS", "SOC", "TS", "PV") # creación de prefijos para el filtrado posterior

cc_cursos <- cc_cursos %>%
  filter(str_detect(codigo, paste0("^(", paste(prefijos, collapse = "|"), ")"))) # filtrar solamente por los cursos de carrera identificando por el código

cc_cursos <- cc_cursos %>%
  mutate(
    sexo = recode(sexo,
                  "Masculino" = 0,
                  "Femenino" = 1) |> as.integer() # recodificación de la variable sexo
  )


cc_cursos_group <- cc_cursos %>%
  group_by(curso, codigo, ano, seccion, semestre_label) %>%
  summarise(
    semestre_moda         = first(semestre_moda),
    nota_promedio = mean(nota, na.rm = TRUE),
    n_estudiantes = n(),
    desv_std = sd(nota),
    n_aprobados = sum(nota >= 4, na.rm = TRUE),
    n_reprobados = sum(nota < 4, na.rm = TRUE),
    porcentaje_aprobacion = (n_aprobados / n_estudiantes) * 100,
    n_hombres = sum(sexo == 0, na.rm = TRUE),
    n_mujeres = sum(sexo == 1, na.rm = TRUE),
    nota_min = min(nota, na.rm = TRUE),
    nota_max = max(nota, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(curso, ano)

cc_cursos_section <- cc_cursos %>%
  group_by(curso, codigo, ano, semestre_label) %>%  # se elimina 'seccion'
  summarise(
    semestre_moda         = first(semestre_moda),
    
    # Promedio de notas de todos los individuos (todas las secciones juntas)
    nota_promedio         = mean(nota, na.rm = TRUE),
    
    # Conteo total de estudiantes de todas las secciones
    n_estudiantes         = n(),
    
    # Desviación estándar de todos los individuos juntos
    desv_std              = sd(nota, na.rm = TRUE),
    
    # Conteos calculados directamente sobre individuos
    n_aprobados           = sum(nota >= 4, na.rm = TRUE),
    n_reprobados          = sum(nota < 4, na.rm = TRUE),
    porcentaje_aprobacion = (sum(nota >= 4, na.rm = TRUE) / n()) * 100,
    n_hombres             = sum(sexo == 0, na.rm = TRUE),
    n_mujeres             = sum(sexo == 1, na.rm = TRUE),
    
    nota_min              = min(nota, na.rm = TRUE),
    nota_max              = max(nota, na.rm = TRUE),
    
    .groups = 'drop'
  ) %>%
  arrange(curso, ano)


cc_cursos_group <- cc_cursos_group %>% # creación de la variable carrera a partir del código del curso
  mutate(carrera = case_when(
    str_starts(codigo, "SOC") ~ "Sociología",
    str_starts(codigo, "PS") ~ "Psicología",
    str_starts(codigo, "ANT") ~ "Antropología",  
    str_starts(codigo, "TS") ~ "Trabajo Social",   
    str_starts(codigo, "PV") ~ "Educación Parvularia",       
    TRUE ~ NA_character_
  ))

cc_cursos_group <- cc_cursos_group %>% # se ajusta la variable año
  mutate(
    ano = as.character(ano),
    ano = str_sub(ano, 1, -2)
  )

cc_cursos_section <- cc_cursos_section %>%
  mutate(carrera = case_when(
    str_starts(codigo, "SOC") ~ "Sociología",
    str_starts(codigo, "PS") ~ "Psicología",
    str_starts(codigo, "ANT") ~ "Antropología",  
    str_starts(codigo, "TS") ~ "Trabajo Social",   
    str_starts(codigo, "PV") ~ "Educación Parvularia",       
    TRUE ~ NA_character_
  ))

cc_cursos_section <- cc_cursos_section %>%
  mutate(
    ano = as.character(ano),
    ano = str_sub(ano, 1, -2)
  )
  

# Creación de variable carácter: distinción de cursos obligatorio/electivo

## Psico


psico <- cc_cursos_group %>%
  filter(carrera == "Psicología")


malla_caracter <- tibble::tibble(
  curso = c(
    "Filosofía","Psicología","Métodos de la Investigación Social","Historia Social de Chile","Psicobiología",
    "Curso Transversal FACSO I","Inglés I",
    "Epistemología de las Ciencias Sociales","Psicología de la Personalidad","Procesos Psicológicos Básicos",
    "Estadística 1","Procesos Básicos de Aprendizaje","CFG","Inglés II",
    "Teorías y Sistemas Psicológicos","Psicopatología","Psicología del Desarrollo I","Estadística 2",
    "Psicología Social I","Neurofisiología","Curso Transversal FACSO II","Inglés III",
    "Introducción a la Evaluación Psicológica","Psiquiatría","Psicología del Desarrollo II",
    "Metodología Cualitativa","Psicología Social II","Neurociencia Cognitiva",
    "Curso Artístico /Deportivo","Inglés IV",
    "Psicología del Trabajo y las Organizaciones","Psicología Clínica","Psicología Educacional",
    "Psicología Jurídica","Psicología Comunitaria",
    "Cursos Optativos","Curso Transversal FACSO III",
    "Seminario de Grado I","Curso Transversal FACSO IV","Seminario de Grado II",
    "Práctica Profesional","Seminario de Práctica I","Cursos de Formación Profesional",
    "Práctica Profesional II","Seminario de Práctica II"
  ),
  caracter = c(
    "Obligatorio","Obligatorio","Obligatorio","Obligatorio","Obligatorio",
    "Electivo","Obligatorio",
    "Obligatorio","Obligatorio","Obligatorio","Obligatorio","Obligatorio","Electivo","Obligatorio",
    "Obligatorio","Obligatorio","Obligatorio","Obligatorio","Obligatorio","Obligatorio","Electivo","Obligatorio",
    "Obligatorio","Obligatorio","Obligatorio","Obligatorio","Obligatorio","Obligatorio",
    "Libre","Obligatorio",
    "Obligatorio","Obligatorio","Obligatorio","Obligatorio","Obligatorio",
    "Electivo","Electivo",
    "Electivo","Electivo","Electivo",
    "Obligatorio","Obligatorio","Electivo",
    "Obligatorio","Obligatorio"
  )
)

malla_psico <- tibble(
  codigo = c(
    "PSIPPPCCC11",
    "PSIPPPCCC25",
    "PSIPPPCH24",
    "PSIPPPCP22",
    "PSIPPPCS23",
    "PSIPPPE19",
    "PSIPPPEN26",
    "PSIPPPEN27",
    "PSIPPPJ21",
    "PSIPPPSC20",
    "PSIPPPTO21",
    "PSIPPPC2",
    "PSIPPPCCC18",
    "PSIPPPCH17",
    "PSIPPPCP15",
    "PSIPPPCS16",
    "PSIPPPE11",
    "PSIPPPEN19",
    "PSIPPPEN28",
    "PSIPPPJ13",
    "PSIPPPS12",
    "PSIPPPSC29",
    "PSIPPPTO14",
    "PSIPPPE9",
    "PSIPPPE8",
    "PSIPPPE7",
    "PSIPPPE6",
    "PSIPPPE1",
    "PSIPPPE10",
    "PSIPPPE4",
    "PSIPPPE3",
    "PSIPPPE5"
  ),
  caracter = c(
    "Obligatorio",  # Fundamentos Históricos...
    "Obligatorio",  # Filosofía
    "Obligatorio",  # Historia social contemporánea
    "Obligatorio",  # Teoría Sociológica I
    "Obligatorio",  # Metodología I
    "Obligatorio",  # Inglés I
    "Obligatorio",  # Curso Transversal
    "Obligatorio",  # Curso Transversal
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",  # Fundamentos Históricos...
    "Obligatorio",  # Filosofía
    "Obligatorio",  # Historia social contemporánea
    "Obligatorio",  # Teoría Sociológica I
    "Obligatorio",  # Metodología I
    "Obligatorio",  # Inglés I
    "Obligatorio",  # Curso Transversal
    "Obligatorio",  # Curso Transversal
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio"
  )
)

psico <- psico %>%  left_join(
    malla_caracter,
    by = "curso"
  ) %>%
  
  left_join(
    malla_psico %>%
      rename(caracter_codigo = caracter),
    by = "codigo"
  ) %>%
  
  mutate(
    caracter = coalesce(caracter, caracter_codigo)
  )

# revisar faltantes ANTES de imputar
psico %>%
  filter(is.na(caracter)) %>%
  distinct(curso)

# imputar
psico <- psico %>%
  mutate(
    caracter = tidyr::replace_na(caracter, "Electivo")
  ) %>%
  select(-caracter_codigo)




## Trabajo 


trabajo <- cc_cursos_group %>%
  filter(carrera == "Trabajo Social")



flexibilidad_df <- tibble(
  curso = c(
    "Fundamentos Históricos y Políticos del Trabajo Social",
    "Filosofía",
    "Historia Social Contemporánea",
    "Teoría Sociológica I",
    "Metodología de la Investigación Social I",
    "Inglés I",
    "Curso Transversal Facultad",
    "Análisis Comparado de Políticas Sociales",
    "Análisis Social del Territorio",
    "Evaluación Social de Programas y Proyectos",
    "Indicadores Sociales", 
    "Innovación Social y Transferencia", 
    "Justicia Social, Derechos y Ciudadanía",
    "Metodología de la Investigación Social III",
    "NUCLEO DE TITULACIÓN I+D II",
    "Normativas Jurídicas de la Intervención Social",
    "Núcleo I + D - Disciplinar I: Metodología para la Intervención Social", 
    "Núcleo I + D - Disciplinar II: Intervención en Fenómenos Sociales Extremos", 
    "Núcleo I + D - Disciplinar III: Cuestión Social y Desigualdades", 
    "Núcleo I + D - Disciplinar IV: Cosmopolitismo Diversidad e Intervención Social",
    "Sustentabilidad e Impacto Social",
    "Teoría Sociológica II",
    "Teorías de Género",
    "Planificación y Gestión Social",
    "Políticas Sociales",
    "Sujetos y Movimientos Sociales",
    "Enfoques Críticos en Trabajo Social",
    "Estado y Sociedad Contemporánea",
    "Historia Social de Chile",
    "Estadística I",
    "Inglés II",
    "Justicia Social y Desigualdades",
    "Epistemología de las Ciencias Sociales",
    "Psicología",
    "Economía I",
    "Estadística II",
    "Inglés III",
    "Deportivo o Artístico",
    "Fundamentos de la Intervención Social",
    "Ética Social",
    "Antropología",
    "Economía II",
    "Metodología de la Investigación Social II",
    "Inglés IV",
    "Núcleo Temático Electivo I+D I:",
    "Núcleo Temático Electivo I+D II:",
    "NÚCLEO TEMÁTICO ELECTIVO I+D II:",
    "NÚCLEO TEMÁTICO ELECTIVO I+D IV",
    "NÚCLEO TEMÁTICO ELECTIVO I+D IV:",
    "Núcleo Temático Electivo I+D III",
    "Núcleo Temático Electivo I+D IV",
    "Asignatura Diplomado o Magíster"
  ),
  caracter = c(
    "Obligatorio",  # Fundamentos Históricos...
    "Obligatorio",  # Filosofía
    "Obligatorio",  # Historia social contemporánea
    "Obligatorio",  # Teoría Sociológica I
    "Obligatorio",  # Metodología I
    "Obligatorio",  # Inglés I
    "Obligatorio",  # Curso Transversal
    "Obligatorio",  # Curso Transversal
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio", 
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio", 
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",  # Enfoques críticos
    "Obligatorio",  # Estado y sociedad
    "Obligatorio",  # Historia social Chile
    "Obligatorio",  # Estadística I
    "Obligatorio",  # Inglés II
    "Obligatorio",  # Justicia Social
    "Obligatorio",  # Epistemología
    "Obligatorio",  # Psicología
    "Obligatorio",  # Economía I
    "Obligatorio",  # Estadística II
    "Obligatorio",  # Inglés III
    "Obligatorio",  # Deportivo/Artístico
    "Obligatorio",  # Fundamentos intervención
    "Obligatorio",  # Ética Social
    "Obligatorio",  # Antropología
    "Obligatorio",  # Economía II
    "Obligatorio",  # Metodología II
    "Obligatorio",  # Inglés IV
    "Electivo",     # Núcleo Electivo I+D I
    "Electivo",     # Núcleo Electivo I+D II
    "Electivo",     # Núcleo Electivo I+D III
    "Electivo",     # Núcleo Electivo I+D IV
    "Electivo",
    "Electivo",
    "Electivo",
    "Electivo"      # Diplomado/Magíster
  )
)

malla_trabajo <- tibble(
  codigo = c(
    "TS201021",
    "TS201025",
    "TS201029",
    "TS201033",
    "TS201034",
    "TS201070",
    "TS201074",
    "TS201073",
    "TS201076"

  ),
  caracter = c(
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio"
  )
)




trabajo <- trabajo %>%
  
  # join por curso
  left_join(
    flexibilidad_df,
    by = "curso"
  ) %>%
  
  # join por código
  left_join(
    malla_trabajo %>%
      rename(caracter_codigo = caracter),
    by = "codigo"
  ) %>%
  
  # unificar ambas columnas
  mutate(
    caracter = coalesce(caracter, caracter_codigo)
  ) %>%
  
  # eliminar auxiliar
  select(-caracter_codigo)

trabajo <- trabajo %>%
  mutate(caracter = ifelse(is.na(caracter), "Electivo", caracter))

trabajo %>%
  filter(is.na(caracter)) %>%
  distinct(curso)


# Socio


socio <- cc_cursos_group %>%
  filter(carrera == "Sociología")




malla_caracter_socio <- tibble(
  curso = c(
    "Filosofía Social",
    "Historia de la Sociedad Moderna",
    "Introducción a la Sociología",
    "Antropología",
    "Psicología Social",
    "Inglés I",
    "Curso Transversal de Facultad",
    "Teoría Sociológica Clásica",
    "Historia Social de América Latina",
    "Epistemología",
    "Diseños de Investigación",
    "Población y sociedad",
    "Inglés II",
    "Teorías Sociológicas de la Sociedad Moderna",
    "Historia Social de Chile",
    "Estrategias de Investigación Cualitativa",
    "Estadística Descriptiva",
    "Economía",
    "Inglés III",
    "Deportivo / Artístico",
    "Teorías Sociológicas Contemporáneas",
    "Sociología Política",
    "Análisis de Información Cualitativa",
    "Estadística Correlacional",
    "Estrategias de Investigación Cuantitativa",
    "Inglés IV",
    "Curso Transversal de Facultad",
    "Desigualdades y Estratificación Social",
    "Sociología de la Cultura",
    "Sociología del Género",
    "Estadística Multivariada",
    "Créditos electivos",
    "CFG o Deportivo / Artístico",
    "Teoría y Sociedad Latinoamericana",
    "Sociología Económica",
    "Sociología de las Políticas Públicas",
    "Créditos Electivos",
    "Transformaciones Sociales del Chile Contemporáneo",
    "Investigación Evaluativa",
    "Créditos Electivos",
    "Seminario de Grado",
    "Créditos Electivos",
    "Seminario de Título I",
    "Seminario de Título II",
    "Práctica Profesional"
  ),
  caracter = c(
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Electivo",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Electivo",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Electivo",
    "Electivo",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Electivo",
    "Obligatorio",
    "Obligatorio",
    "Electivo",
    "Obligatorio",
    "Electivo",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio"
  )
)



socio <- socio %>%
  left_join(malla_caracter_socio, by = "curso")

socio <- socio %>%
  mutate(caracter = ifelse(is.na(caracter), "Electivo", caracter))

socio %>%
  filter(is.na(caracter)) %>%
  distinct(curso)

socio <- socio %>%
  filter(codigo != "SOC01060")


## Educación Parvularia


educa <- cc_cursos_group %>%
  filter(carrera == "Educación Parvularia")


malla_caracter_parv <- tibble(
  curso = c(
    # Semestre I
    "Bases del Desarrollo y Aprendizaje",
    "Construcciones Sociales sobre la Infancia",
    "Historia y Fundamentos de la Educación",
    "Salud y Cuidado del Niño",
    "Filosofía y Epistemología de las Ciencias Sociales",
    "Práctica 1: Aproximaciones al Campo Profesional",
    
    # Semestre II
    "Teorías del Desarrollo y Aprendizaje",
    "Metodología de la Investigación",
    "Pedagogía de la Diversidad e Inclusión Social",
    "Currículo y Didáctica en la Educación Parvularia",
    "Familia, Comunidad y Territorio",
    "Práctica 2: Aproximaciones al Escenario Pedagógico",
    
    # Semestre III
    "Saberes Pedagógicos de la Psicomotricidad, 1 Ciclo",
    "Saberes Pedagógicos del Desarrollo Personal y Social, 1 Ciclo",
    "Análisis de Procesos Evaluativos en Educación Parvularia, 1 Ciclo",
    "Investigación Educativa",
    "Práctica 3: Problematización de Experiencias de Aprendizaje, 1 Ciclo",
    "Inglés I",
    
    # Semestre IV
    "Saberes Pedagógicos de la Psicomotricidad, 2 Ciclo",
    "Saberes Pedagógicos del Desarrollo Personal y Social, 2 Ciclo",
    "Construcción Curricular",
    "Análisis de Procesos Evaluativos en Educación Parvularia, 2 Ciclo",
    "Práctica 4: Problematización de Experiencias de Aprendizaje, 2 Ciclo",
    "Inglés II",
    
    # Semestre V
    "Saberes Pedagógicos del Lenguaje y la Comunicación, 1 Ciclo",
    "Creatividad y Expresiones Infantiles, 1 Ciclo",
    "Proyectos Educativos",
    "Gestión Curricular",
    "Práctica 5: Desarrollo de Experiencias de Aprendizaje 1 Ciclo",
    "Inglés III",
    
    # Semestre VI
    "Saberes Pedagógicos del Lenguaje y la Comunicación, 2 Ciclo",
    "Creatividad y Expresiones Infantiles, 2 Ciclo",
    "Electivo",
    "Liderazgo Pedagógico, Redes Profesionales y Recursos Comunicativos",
    "Práctica 6: Desarrollo de Experiencias de Aprendizaje, 2 Ciclo",
    "Inglés IV",
    
    # Semestre VII
    "Saberes Pedagógicos del Razonamiento Lógico Matemático, 1 Ciclo",
    "Saberes Pedagógicos del Medio Natural",
    "Pedagogía para las Infancias: Transiciones y Articulación Curricular",
    "Electivo",
    "Práctica 7: Desarrollo de Experiencias de Aprendizaje en Contextos Diversos I",
    "Curso Formación general",
    "Curso deportivo",
    
    # Semestre VIII
    "Saberes Pedagógicos del Razonamiento Lógico Matemático, 2 Ciclo",
    "Saberes Pedagógicos del Medio Social y Cultural",
    "Seminario Temático de Investigación",
    "Práctica 8: Desarrollo de Experiencias de Aprendizaje en Contextos Diversos II",
    "Formación general",
    "Curso deportivo",
    
    # Semestre IX
    "Seminario I",
    "Electivo",
    "Práctica Profesional 1 Ciclo",
    "Taller de Práctica",
    
    # Semestre X
    "Seminario II",
    "Electivo",
    "Práctica Profesional 2 Ciclo",
    "Taller de Práctica"
  ),
  caracter = c(
    # I
    "Obligatorio","Obligatorio","Obligatorio","Obligatorio","Obligatorio","Obligatorio",
    
    # II
    "Obligatorio","Obligatorio","Obligatorio","Obligatorio","Obligatorio","Obligatorio",
    
    # III
    "Obligatorio","Obligatorio","Obligatorio","Obligatorio","Obligatorio","Obligatorio",
    
    # IV
    "Obligatorio","Obligatorio","Obligatorio","Obligatorio","Obligatorio","Obligatorio",
    
    # V
    "Obligatorio","Obligatorio","Obligatorio","Obligatorio","Obligatorio","Obligatorio",
    
    # VI
    "Obligatorio","Obligatorio","Electivo","Obligatorio","Obligatorio","Obligatorio",
    
    # VII
    "Obligatorio","Obligatorio","Electivo","Electivo","Obligatorio","Obligatorio","Obligatorio",
    
    # VIII
    "Obligatorio","Obligatorio","Obligatorio","Obligatorio","Obligatorio","Obligatorio",
    
    # IX
    "Obligatorio","Electivo","Obligatorio","Obligatorio",
    
    # X
    "Obligatorio","Electivo","Obligatorio","Obligatorio"
  )
)

malla_educa <- tibble(
  codigo = c(
    "PV00039",
    "PV00042",
    "PV00041",
    "PV00044"


  ),
  caracter = c(
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio"
  )
)


educa <- educa %>%
  
  # join por curso
  left_join(
    malla_caracter_parv,
    by = "curso"
  ) %>%
  
  # join por código
  left_join(
    malla_educa %>%
      rename(caracter_codigo = caracter),
    by = "codigo"
  ) %>%
  
  # unificar ambas columnas
  mutate(
    caracter = coalesce(caracter, caracter_codigo)
  ) %>%
  
  # eliminar auxiliar
  select(-caracter_codigo)

educa <- educa %>%
  mutate(caracter = ifelse(is.na(caracter), "Electivo", caracter))

educa %>%
  filter(is.na(caracter)) %>%
  distinct(curso)



antropo <- cc_cursos_group %>%
  filter(carrera == "Antropología")




malla_antropologia <- tibble(
  curso = c(
    
    # Semestre 1
    "Evolución Humana",
    "Formación de la Sociedad Moderna",
    "Problemas Fundamentales de la Antropología I",
    "Filosofía de las Ciencias Sociales",
    "Taller I: El quehacer del antropólogo",
    "Curso Transversal de Facultad",
    "Inglés I",
    
    # Semestre 2
    "Cazadores Recolectores",
    "Formación de la Sociedad Chilena",
    "Problemas Fundamentales de la Antropología II",
    "Teoría Social",
    "Taller II: Etnografía y Archivo",
    "Deportivo/Artístico/Cultural",
    "Inglés II",
    
    # Semestre 3
    "Sociedades Complejas",
    "Problemas Contemporáneos de la Antropología",
    "Problemas Fundamentales de la Antropología III",
    "Estadística I",
    "Taller III: Materialidad y Bioantropología",
    "CFG",
    "Inglés III",
    
    # Semestre 4
    "Electivo de Especialización",
    "Problemas Fundamentales de la Antropología IV",
    "Estadística II",
    "Taller IV: El Proceso de Investigación",
    "Curso Transversal de Facultad",
    "Inglés IV",
    
    # Arqueología - Semestre 5
    "Etnologías y Estudios Interculturales I",
    "Teoría Arqueológica I",
    "Problemas de Prehistoria I",
    "Laboratorio I: Lítica",
    "Laboratorio II: Zooarqueología",
    "Métodos y Técnicas de Terreno I",
    "Taller de Investigación Social Aplicada",
    
    # Antropología Física - Semestre 5
    "Antropología Física I:",
    "Anatomía",
    "Genética",
    "Método Científico",
    
    # Antropología Social - Semestre 5
    "Problemas Teóricos en Antropología Social I",
    "Antropologías Aplicadas I:  Antropología Rural I",
    "Métodos y Técnicas de Investigación Social I",
    "Cursos Electivos",
    
    # Arqueología - Semestre 6
    "Etnologías y Estudios Interculturales II",
    "Teoría Arqueológica II",
    "Problemas de Prehistoria II",
    "Laboratorio II: Cerámica",
    "Laboratorio IV: Electro",
    "Métodos y Técnicas de Terreno II",
    "Taller de Arqueología Cuantitativa",
    
    # Antropología Física - Semestre 6
    "Antropología Física II: Fisiopatología",
    "Genética de Poblaciones",
    "Taller Opcional Antropología Física",
    
    # Antropología Social - Semestre 6
    "Problemas Teóricos en Antropología Social II",
    "Antropologías Aplicadas II",
    "Métodos y Técnicas de Investigación Social II",
    
    # Arqueología - Semestre 7
    "Etnologías y Estudios Interculturales III",
    "Teoría Arqueológica III",
    "Problemas de Prehistoria III",
    "Laboratorio VI: Bioarqueología",
    "Manejo de Colecciones",
    "Métodos y Técnicas de Terreno III",
    "Taller de Investigación I",
    
    # Antropología Física - Semestre 7
    "Bioarqueología",
    "Crecimiento y Desarrollo",
    "Evolución II",
    "Cursos Electivos de Especialidad",
    
    # Antropología Social - Semestre 7
    "Problemas Teóricos en Antropología Social III",
    "Antropologías Aplicadas III",
    "Métodos y Técnicas de Investigación Social III",
    
    # Arqueología - Semestre 8
    "Problemas Contemporáneos",
    "Teoría Arqueológica IV",
    "Problemas de Prehistoria IV",
    "Legislación y Patrimonio",
    "Métodos y Técnicas de Terreno IV",
    "Taller de Investigación II",
    
    # Antropología Física - Semestre 8
    "Antropología Forense",
    "Ecología Humana",
    
    # Antropología Social - Semestre 8
    "Problemas Teóricos en Antropología Social IV",
    "Antropologías Aplicadas IV",
    "Métodos y Técnicas de Investigación Social IV",
    
    # Semestre 9
    "Taller de Memoria I",
    "Práctica Profesional",
    
    # Semestre 10
    "Taller de Memoria II"
    
  ),
  
  caracter = c(
    
    # Semestre 1
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    
    # Semestre 2
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    
    # Semestre 3
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    
    # Semestre 4
    "Electivo",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    
    # Arqueología - Semestre 5
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    
    # Antropología Física - Semestre 5
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    
    # Antropología Social - Semestre 5
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Electivo",
    
    # Arqueología - Semestre 6
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Electivo",
    "Obligatorio",
    "Obligatorio",
    
    # Antropología Física - Semestre 6
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    
    # Antropología Social - Semestre 6
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    
    # Arqueología - Semestre 7
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    
    # Antropología Física - Semestre 7
    "Obligatorio",
    "Electivo",
    "Obligatorio",
    "Electivo",
    
    # Antropología Social - Semestre 7
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    
    # Arqueología - Semestre 8
    "Electivo",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    
    # Antropología Física - Semestre 8
    "Obligatorio",
    "Electivo",
    
    # Antropología Social - Semestre 8
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    
    # Semestre 9
    "Obligatorio",
    "Obligatorio",
    
    # Semestre 10
    "Obligatorio"
  )
)

antropo_cod <- tibble(
  codigo = c(
    "ANT00200",
    "ANT00036",
    "ANT00038",
    "ANT00034",
    "ANT00146",
    "ANT00078",
    "ANT00081",
    "ANT00079",
    "ANT00080",
    "ANT00077",
    "ANT00131",
    "ANT00120",
    "ANT00070",
    "ANT00068",
    "ANT00030",
    "ANT00033",
    "ANT00032",
    "ANT00075",
    "ANT00031",
    "ANT00126",
    "ANT00125",
    "ANT00063",
    "ANT00060",
    "ANT00163",
    "ANT00157",
    "ANT00130",
    "ANT00089",
    "ANT00088",
    "ANT00091",
    "ANT00112",
    "ANT00093",
    "ANT00111",
    "ANT00164",
    "ANT00054",
    "ANT00101",
    "ANT00155",
    "ANT00045",
    "ANT00043",
    "ANT00044",
    "ANT00162",
    "ANT00085",
    "ANT00082",
    "ANT00084",
    "ANT00049",
    "ANT00087",
    "ANT00104",
    "ANT00105",
    "ANT00062",
    "ANT00051",
    "ANT00027",
    "ANT00028",
    "ANT00145",
    "ANT00165",
    "ANT00166",
    "ANT00095",
    "ANT00099",
    "ANT00042",
    "ANT00040",
    "ANT00097",
    "ANT00106",
    "ANT00039",
    "ANT00041",
    "ANT00096",
    "ANT00138",
    "ANT00151",
    "ANT00127",
    "ANT00005",
    "ANT00015",
    "ANT00010",
    "ANT00018",
    "ANT00061",
    "ANT00161",
    "ANT00170",
    "ANT00178",
    "ANT00074",
    "ANT00071",
    "ANT00153",
    "ANT00172",
    "ANT00113",
    "ANT00064",
    "ANT00117",
    "ANT00114",
    "ANT00065",
    "ANT00118",
    "ANT00050",
    "ANT00115",
    "ANT00058",
    "ANT00149",
    "ANT00148",
    "ANT00116",
    "ANT00100",
    "ANT00119",
    "ANT00135",
    "ANT00132",
    "ANT00179",
    "ANT00182",
    "ANT00180",
    "ANT00057"

  ),
  caracter = c(
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio",
    "Obligatorio"
  )
)



antropo_p <- antropo %>%
    left_join(
    malla_antropologia,
    by = "curso"
  ) %>%
  
  # join por código
  left_join(
    antropo_cod %>%
      rename(caracter_codigo = caracter),
    by = "codigo"
  ) %>%
  
  # unificar ambas columnas
  mutate(
    caracter = coalesce(caracter, caracter_codigo)
  ) %>%
  
  # eliminar auxiliar
  select(-caracter_codigo)




antropo_p <- antropo_p %>%
  mutate(caracter = ifelse(is.na(caracter), "Electivo", caracter))

antropo_p %>%
  filter(is.na(caracter)) %>%
  distinct(curso)

## Bases sin secciones

psico_s <- cc_cursos_section %>%
  filter(carrera == "Psicología")

psico_s <- psico_s %>%
  
  # join por curso
  left_join(
    malla_caracter,
    by = "curso"
  ) %>%
  
  # join por código
  left_join(
    malla_psico %>%
      rename(caracter_codigo = caracter),
    by = "codigo"
  ) %>%
  
  # unificar ambas columnas
  mutate(
    caracter = coalesce(caracter, caracter_codigo)
  ) %>%
  
  # eliminar auxiliar
  select(-caracter_codigo)

psico_s <- psico_s %>%
  mutate(caracter = ifelse(is.na(caracter), "Electivo", caracter))

socio_s <- cc_cursos_section %>%
  filter(carrera == "Sociología")

socio_s <- socio_s %>%
  left_join(malla_caracter_socio, by = "curso")

socio_s <- socio_s %>%
  mutate(caracter = ifelse(is.na(caracter), "Electivo", caracter))

ts_s <- cc_cursos_section %>%
  filter(carrera == "Trabajo Social")

ts_s <- ts_s %>%
  
  # join por curso
  left_join(
    flexibilidad_df,
    by = "curso"
  ) %>%
  
  # join por código
  left_join(
    malla_trabajo %>%
      rename(caracter_codigo = caracter),
    by = "codigo"
  ) %>%
  
  # unificar ambas columnas
  mutate(
    caracter = coalesce(caracter, caracter_codigo)
  ) %>%
  
  # eliminar auxiliar
  select(-caracter_codigo)

ts_s <- ts_s %>%
  mutate(caracter = ifelse(is.na(caracter), "Electivo", caracter))

educa_s <- cc_cursos_section %>%
  filter(carrera == "Educación Parvularia")

educa_s <- educa_s %>%
  
  # join por curso
  left_join(
    malla_caracter_parv,
    by = "curso"
  ) %>%
  
  # join por código
  left_join(
    malla_educa %>%
      rename(caracter_codigo = caracter),
    by = "codigo"
  ) %>%
  
  # unificar ambas columnas
  mutate(
    caracter = coalesce(caracter, caracter_codigo)
  ) %>%
  
  # eliminar auxiliar
  select(-caracter_codigo)

educa_s <- educa_s %>%
  mutate(caracter = ifelse(is.na(caracter), "Electivo", caracter))


antropo_s <- cc_cursos_section %>%
  filter(carrera == "Antropología")

antropo_s <- antropo_s %>%
  
  # join por curso
  left_join(
    malla_antropologia,
    by = "curso"
  ) %>%
  
  # join por código
  left_join(
    antropo_cod %>%
      rename(caracter_codigo = caracter),
    by = "codigo"
  ) %>%
  
  # unificar ambas columnas
  mutate(
    caracter = coalesce(caracter, caracter_codigo)
  ) %>%
  
  # eliminar auxiliar
  select(-caracter_codigo)

antropo_s <- antropo_s %>%
  mutate(caracter = ifelse(is.na(caracter), "Electivo", caracter))



bases_semestres <- list(psico, antropo_p, socio, trabajo, educa)
bases_semestres_s <- list(psico_s, socio_s, ts_s, educa_s, antropo_s)

cc_cursos_sem <- bind_rows(bases_semestres)
cc_cursos_sem_s <- bind_rows(bases_semestres_s)


## Disonancia

# notas18_proc <- notas18 %>%
#   select(RUT.Alumno, Género.Alumno, COLEGIO, NOTA,
#          SEMESTRE, CODIGO, SECCION, ASIGNATURA, CARRERA, Año.de.Ingreso) %>%
#   rename(rut = RUT.Alumno, sexo = Género.Alumno, colegio = COLEGIO, nota = NOTA, ano = SEMESTRE,
#          codigo = CODIGO, seccion = SECCION, curso = ASIGNATURA, carrera = CARRERA, cohorte = Año.de.Ingreso)

# notas18_proc <- notas18_proc %>%
#   mutate(carrera = if_else(str_detect(carrera, "^Antropología"),
#                            "Antropología",
#                            carrera))

# notas18_proc <- notas18_proc %>%
#   filter(carrera %in% c("Sociología", "Psicología", "Antropología", "Trabajo Social", "Pedagogía en Educación Parvularia"))

# notas18_proc$ano <- str_sub(notas18_proc$ano, 1, -2)

# notas18_proc <- notas18_proc %>%
#   mutate(ano = as.integer(ano))

# notas18_proc <- notas18_proc %>%
#   mutate(disonancia = ano - cohorte)

# sum(notas18_proc$disonancia < 0, na.rm = TRUE) 

# Disonancia Base 24: 3.6%
# Disonancia Base 23: 2.1%
# Disonancia Base 22: 1.8%
# Disonancia Base 21: 2.8%
# Disonancia Base 20: 3.6%
# Disonancia Base 19: 2.4%
# Disonancia Base 18: 2.5%



# ── 1. Eliminar cursos duplicados exactos ─────────────────────────────────────────────
duplicados <- cc_cursos_sem_s %>%
  group_by(codigo, curso, ano, semestre_moda) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1)

cc_sin_duplicados <- cc_cursos_sem_s %>%
  group_by(codigo, curso, ano, semestre_moda) %>%
  summarise(
    nota_promedio        = weighted.mean(nota_promedio, w = n_estudiantes, na.rm = TRUE),
    desv_std             = weighted.mean(desv_std,      w = n_estudiantes, na.rm = TRUE),
    n_estudiantes        = sum(n_estudiantes, na.rm = TRUE),
    n_aprobados          = sum(n_aprobados,   na.rm = TRUE),
    n_reprobados         = sum(n_reprobados,  na.rm = TRUE),
    n_hombres            = sum(n_hombres,     na.rm = TRUE),
    n_mujeres            = sum(n_mujeres,     na.rm = TRUE),
    nota_min             = min(nota_min,      na.rm = TRUE),
    nota_max             = max(nota_max,      na.rm = TRUE),
    porcentaje_aprobacion = (sum(n_aprobados, na.rm = TRUE) /
                             sum(n_estudiantes, na.rm = TRUE)) * 100,
    carrera              = first(carrera),
    caracter             = first(caracter),
    .groups = "drop"
  )

# ── 2. Identificar inconsistentes ──────────────────────────────────────────────
inconsistentes <- cc_sin_duplicados %>%
  group_by(codigo, curso, ano) %>%
  summarise(n_semestres = n_distinct(semestre_moda), .groups = "drop") %>%
  filter(n_semestres > 1)

# ── 3. Diagnóstico: verificar tipos de columnas clave ─────────────────────────
# Si hay diferencias de tipo, el join falla silenciosamente
stopifnot(
  class(cc_sin_duplicados$codigo) == class(inconsistentes$codigo),
  class(cc_sin_duplicados$ano)    == class(inconsistentes$ano)
)

# ── 4. Marcar inconsistentes con flag en la misma base ────────────────────────
# Esto evita depender de joins entre data frames de distinta estructura
cc_sin_duplicados <- cc_sin_duplicados %>%
  left_join(
    inconsistentes %>% select(codigo, curso, ano) %>% mutate(es_inconsistente = TRUE),
    by = c("codigo", "curso", "ano")
  ) %>%
  mutate(es_inconsistente = tidyr::replace_na(es_inconsistente, FALSE))

# ── 5. Separar y procesar inconsistentes ──────────────────────────────────────
inconsistentes_procesados <- cc_sin_duplicados %>%
  filter(es_inconsistente) %>%
  group_by(codigo, curso, ano) %>%
  summarise(
    semestre_moda         = semestre_moda[which.max(n_estudiantes)],
    nota_promedio         = weighted.mean(nota_promedio, w = n_estudiantes, na.rm = TRUE),
    desv_std              = weighted.mean(desv_std,      w = n_estudiantes, na.rm = TRUE),
    n_estudiantes         = sum(n_estudiantes, na.rm = TRUE),
    n_aprobados           = sum(n_aprobados,   na.rm = TRUE),
    n_reprobados          = sum(n_reprobados,  na.rm = TRUE),
    n_hombres             = sum(n_hombres,     na.rm = TRUE),
    n_mujeres             = sum(n_mujeres,     na.rm = TRUE),
    porcentaje_aprobacion = (sum(n_aprobados, na.rm = TRUE) /
                             sum(n_estudiantes, na.rm = TRUE)) * 100,
    nota_min              = min(nota_min,      na.rm = TRUE),
    nota_max              = max(nota_max,      na.rm = TRUE),
    carrera               = first(carrera),
    caracter              = first(caracter),
    .groups = "drop"
  )

# ── 6. Conservar solo los no-inconsistentes y unir ────────────────────────────
base_sin_inconsistentes <- cc_sin_duplicados %>%
  filter(!es_inconsistente) %>%
  select(-es_inconsistente)          # limpiar columna auxiliar

base_final <- bind_rows(
  base_sin_inconsistentes,
  inconsistentes_procesados
)

# ── 7. Verificación ───────────────────────────────────────────────────────────
n_esperado <- n_distinct(cc_sin_duplicados[, c("codigo", "curso", "ano")])
n_obtenido <- n_distinct(base_final[,        c("codigo", "curso", "ano")])

stopifnot(n_esperado == n_obtenido)
message("✔ base_final contiene ", nrow(base_final), " filas — todos los cursos están presentes.")








# creación variable semestres curriculares

niveles_semestre <- c("Primer semestre", "Segundo semestre", "Tercer semestre",
                        "Cuarto semestre", "Quinto semestre", "Sexto semestre",
                        "Séptimo semestre", "Octavo semestre", "Noveno semestre",
                        "Décimo semestre")


base_final <- base_final %>%
  
  mutate(
    
    semestre_cv = case_when(
      semestre_moda == 1  ~ "Primer semestre",
      semestre_moda == 2  ~ "Segundo semestre",
      semestre_moda == 11 ~ "Tercer semestre",
      semestre_moda == 12 ~ "Cuarto semestre",
      semestre_moda == 21 ~ "Quinto semestre",
      semestre_moda == 22 ~ "Sexto semestre",
      semestre_moda == 31 ~ "Séptimo semestre",
      semestre_moda == 32 ~ "Octavo semestre",
      semestre_moda == 41 ~ "Noveno semestre",
      semestre_moda == 42 ~ "Décimo semestre",
      TRUE ~ NA_character_
    ),
    
    semestre_cv = factor(
      semestre_cv,
      levels = niveles_semestre,
      ordered = TRUE
    )
    
  )

saveRDS(base_final,
  here("input", "data", "proc_data", "cc_nosec.rds"),
  compress = FALSE)

save(
  base_final,
  file = here::here("input", "data", "proc_data", "cc_nosec.RData")
)






# PROCESAMIENTO BASE CON SECCIONES
# =========================================================
# 1. DETECTAR DUPLICADOS
# =========================================================

duplicados <- cc_cursos_sem %>%
  group_by(codigo, curso, ano, semestre_moda, seccion) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1)


# =========================================================
# 2. CONSOLIDAR DUPLICADOS
# =========================================================

cc_sin_duplicados <- cc_cursos_sem %>%
  group_by(codigo, curso, ano, semestre_moda, seccion) %>%
  summarise(
    nota_promedio         = weighted.mean(nota_promedio, w = n_estudiantes, na.rm = TRUE),
    desv_std              = weighted.mean(desv_std,      w = n_estudiantes, na.rm = TRUE),
    n_estudiantes         = sum(n_estudiantes, na.rm = TRUE),
    n_aprobados           = sum(n_aprobados,   na.rm = TRUE),
    n_reprobados          = sum(n_reprobados,  na.rm = TRUE),
    n_hombres             = sum(n_hombres,     na.rm = TRUE),
    n_mujeres             = sum(n_mujeres,     na.rm = TRUE),
    nota_min              = min(nota_min,      na.rm = TRUE),
    nota_max              = max(nota_max,      na.rm = TRUE),
    porcentaje_aprobacion = (sum(n_aprobados, na.rm = TRUE) /
                             sum(n_estudiantes, na.rm = TRUE)) * 100,
    carrera               = first(carrera),
    caracter              = first(caracter),
    .groups = "drop"
  )


# =========================================================
# 3. DETECTAR INCONSISTENTES
# =========================================================

inconsistentes <- cc_sin_duplicados %>%
  group_by(codigo, curso, ano, seccion) %>%
  summarise(n_semestres = n_distinct(semestre_moda), .groups = "drop") %>%
  filter(n_semestres > 1)


# =========================================================
# 4. MARCAR INCONSISTENTES CON FLAG
# =========================================================

cc_sin_duplicados <- cc_sin_duplicados %>%
  left_join(
    inconsistentes %>%
      select(codigo, curso, ano, seccion) %>%
      mutate(es_inconsistente = TRUE),
    by = c("codigo", "curso", "ano", "seccion")
  ) %>%
  mutate(es_inconsistente = tidyr::replace_na(es_inconsistente, FALSE))


# =========================================================
# 5. CONSOLIDAR INCONSISTENTES
# =========================================================

inconsistentes_procesados <- cc_sin_duplicados %>%
  filter(es_inconsistente) %>%
  group_by(codigo, curso, ano, seccion) %>%
  summarise(
    semestre_moda         = semestre_moda[which.max(n_estudiantes)],
    nota_promedio         = weighted.mean(nota_promedio, w = n_estudiantes, na.rm = TRUE),
    desv_std              = weighted.mean(desv_std,      w = n_estudiantes, na.rm = TRUE),
    n_estudiantes         = sum(n_estudiantes, na.rm = TRUE),
    n_aprobados           = sum(n_aprobados,   na.rm = TRUE),
    n_reprobados          = sum(n_reprobados,  na.rm = TRUE),
    n_hombres             = sum(n_hombres,     na.rm = TRUE),
    n_mujeres             = sum(n_mujeres,     na.rm = TRUE),
    porcentaje_aprobacion = (sum(n_aprobados, na.rm = TRUE) /
                             sum(n_estudiantes, na.rm = TRUE)) * 100,
    nota_min              = min(nota_min,      na.rm = TRUE),
    nota_max              = max(nota_max,      na.rm = TRUE),
    carrera               = first(carrera),
    caracter              = first(caracter),
    .groups = "drop"
  )


# =========================================================
# 6. CONSERVAR NO-INCONSISTENTES Y UNIR
# =========================================================

base_sin_inconsistentes <- cc_sin_duplicados %>%
  filter(!es_inconsistente) %>%
  select(-es_inconsistente)

base_final <- bind_rows(
  base_sin_inconsistentes,
  inconsistentes_procesados
)


# =========================================================
# 7. VERIFICACIÓN
# =========================================================

n_esperado <- n_distinct(cc_sin_duplicados[, c("codigo", "curso", "ano", "seccion")])
n_obtenido <- n_distinct(base_final[,        c("codigo", "curso", "ano", "seccion")])

stopifnot(n_esperado == n_obtenido)
message("✔ base_final contiene ", nrow(base_final), " filas — todos los cursos están presentes.")

base_final <- base_final %>%
  
  mutate(
    
    semestre_cv = case_when(
      semestre_moda == 1  ~ "Primer semestre",
      semestre_moda == 2  ~ "Segundo semestre",
      semestre_moda == 11 ~ "Tercer semestre",
      semestre_moda == 12 ~ "Cuarto semestre",
      semestre_moda == 21 ~ "Quinto semestre",
      semestre_moda == 22 ~ "Sexto semestre",
      semestre_moda == 31 ~ "Séptimo semestre",
      semestre_moda == 32 ~ "Octavo semestre",
      semestre_moda == 41 ~ "Noveno semestre",
      semestre_moda == 42 ~ "Décimo semestre",
      TRUE ~ NA_character_
    ),
    
    semestre_cv = factor(
      semestre_cv,
      levels = niveles_semestre,
      ordered = TRUE
    )
    
  )

saveRDS(base_final,
  here("input", "data", "proc_data", "cc_sec.rds"),
  compress = FALSE)

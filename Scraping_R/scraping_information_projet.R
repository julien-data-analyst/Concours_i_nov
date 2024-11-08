#################################-
# Auteur : Julien RENOULT
# Sujet : concours i-nov (scraping PDF des informations de chaque projet)
# Date : 21/09/2024 - 30/09/2024
################################-

# ---- Chargement des librairies ----
library(tabulapdf) # à la place de tabulate
library(dplyr) # fonctions de mutate, rename, group_by, summarise, etc
library(stringr) # fonctions pour gérer les chaînes de caractères
library(data.table) # fonction de fusion (rbindlist)
options(java.parameters = c("-Xmx8000m"))
####################################-

# ---- Importation des données ----

# Prise en compte de la structure
readLines(con = "../Data/i_nov_vag_1_10_somm.csv", n=10)

# Application de l'importation
dataset_i_nov = read.csv(file = "../Data/i_nov_vag_1_10_somm.csv", 
                         sep = ";",
                         quote = "\"",
                         header = TRUE)

# Vérification du succès de l'opération
head(dataset_i_nov)
tail(dataset_i_nov)
str(dataset_i_nov)
###################################-

# ---- Récupération des informations de chaque projet (1/2) ----
# Objectif : récupérer les informations de chaque projet (montant, aide) à l'aide du numéro de page et de l'URL du fichier d'origine
# En regardant les fichier PDF d'origine, on s'aperçoit que :
# - les six premiers fichiers ont le bloc d'information situé à gauche
# !!! Dont les trois premiers sont situées sur la même ligne tandis que les trois autres sont sur deux lignes !!!
# - les quatre derniers fichiers ont le bloc d'information situé au centre
# Il faut donc récupérer deux types de coordonnées

# Prenons par exemple les vague 1 à 3 ####
i_nov_1_3 = dataset_i_nov %>% filter(VAGUE %in% c(1, 2, 3, 4, 5, 6))

# Vérification du succès de l'opération
head(i_nov_1_3)
tail(i_nov_1_3)
str(i_nov_1_3)

# Regardons pour le premier projet ####
# La plus grande infobox
premier_projet = filter(i_nov_1_3, PAGE == 103)
#i_nov_1[1, ]
#filter(i_nov_1, PAGE == 26)
## Localisation de l'infobox du projet
localisation_info_box_prem = locate_areas(premier_projet$URL_FICH_ORIGINE, pages= premier_projet$PAGE)
localisation_info_box_deux = locate_areas("../Data/vague_1_6/i-nov-palmar-s-vague-2-19084.pdf", pages= 36)
localisation_info_box_prem = list(c(200, 44, 657, 177))

# Vérification du succès de l'opération
localisation_info_box_prem

## Récupérons les informations de ce projet
infobox_prem = extract_tables(premier_projet$URL_FICH_ORIGINE, 
               pages = 101,
               area = localisation_info_box_prem,
               guess = FALSE,
               output = "matrix")

infobox_deux = extract_tables("../Data/vague_1_6/i-nov-palmar-s-vague-2-19084.pdf", 
                              pages = 36,
                              area = localisation_info_box_prem,
                              guess = FALSE,
                              output = "matrix")

print(infobox_prem)
premier_projet$PAGE

## petite préparation des données

# Intitialisation
dataset_i_nov_1 = data.frame()
# Détecter le pattern : "MOT(S) > MOT(S) €*"
lign_donn = matrix(nrow = nrow(infobox_prem[[1]]))

# Dans le cas de plusieurs lignes
multiple_lignes = FALSE # savoir s'il y a plusieurs lignes
row_multiple = 0 # la ligne où on va concaténer

# Utilisation de pattern ####
for(i in 1:nrow(infobox_prem[[1]])){
  
  # Récupération du pattern
  lign = str_detect(infobox_prem[[1]][i, 1], "[\\w\\-]+(\\s[\\w\\-]+)*\\s*>\\s*[\\w\\-]+(\\s[\\w\\-]+)*\\s*€*[^,]$")
  
  # Dans le cas de sur deux lignes
  lign_2 = ifelse(!is.na(str_match(infobox_prem[[1]][i, 1], "\\w+(\\s\\w+)*\\s*>\\s*(\\w+,\\s*|$)")[1, 1]), 
                TRUE, FALSE)
  #print(lign)
  if(lign){
    lign_donn[i, 1] = infobox_prem[[1]][i, 1]
    multiple_lignes = FALSE
  }
  if(multiple_lignes & !lign & !lign_2){
    lign_donn[row_multiple, 1] = paste(lign_donn[row_multiple, 1], infobox_prem[[1]][i, 1])
  }
  if(lign_2){
    lign_donn[i, 1] = infobox_prem[[1]][i, 1]
    multiple_lignes = TRUE
    row_multiple = i
  }
}


# Transformation en dataframe
infobox_proj = as.data.frame(lign_donn)

infobox_proj %>% 
  filter(!is.na(V1)) %>% 
  mutate(
    colonne = str_split_i(V1, " > ", 1),
    valeur = str_split_i(V1, " > ", 2)
  ) %>% 
  select(-V1) %>% 
  t(.) %>% 
  as.data.frame(.) -> infobox_data


colnames(infobox_data) = infobox_data[1, ]
infobox_data = infobox_data[-1, ]
infobox_data$PAGE =premier_projet$PAGE

infobox_data_2 = infobox_proj
infobox_data_2 %>% 
  mutate(
    colonne = str_split_i(V1, " > ", 1),
    valeur = str_split_i(V1, " > ", 2)
  ) %>% 
  select(-V1) %>%  
  t(.) %>% 
  as.data.frame(.) -> test_data

colnames(test_data) = c("test", "RÉALISATION", "MONTANT DU PROJET ")
test_data = test_data[-1, ]

# ignorer les colonnes pas communes
dataset_i_nov_1 = bind_rows(dataset_i_nov_1, infobox_data)
#dataset_i_nov_1 = bind_rows(dataset_i_nov_1, test_data) # à supprimer dans la boucle for

# Utilisation de la boucle while ####
# Initialisation des valeurs conditionnelles
i = 1
presence_contact = FALSE

# Initalisation des lignes de données récupérées
lign_donn = matrix(nrow = nrow(infobox_prem[[1]]))
nb_lign_donnee = 0

while(i <= nrow(infobox_prem[[1]]) & !presence_contact){
  # Si présence ">", alors ajouter une nouvelle ligne de donnée
  if(str_detect(infobox_prem[[1]][i, 1], ">")){
    nb_lign_donnee = nb_lign_donnee + 1
    lign_donn[nb_lign_donnee, 1] = infobox_prem[[1]][i, 1]
  }
  
  else{
  # Si pésence de ces caractères, fin de la boucle while
  if(str_detect(infobox_prem[[1]][i, 1], "CONTACT") | 
     str_detect(infobox_prem[[1]][i, 1], "©")){
    presence_contact = TRUE
  }
  else{
    # Concaténation avec la ligne de donnée actuelle
    lign_donn[nb_lign_donnee, 1] = paste(lign_donn[nb_lign_donnee, 1], 
                                         infobox_prem[[1]][i, 1]) 
  }
  }
  i = i + 1
}

# Transformation en dataframe
infobox_proj = as.data.frame(lign_donn)

# Préparation des données
infobox_proj %>% 
  filter(!is.na(V1)) %>% 
  mutate(
    colonne = str_split_i(V1, " > ", 1),
    valeur = str_split_i(V1, " > ", 2)
  ) %>% 
  select(-V1) %>% 
  t(.) %>% 
  as.data.frame(.) -> infobox_data


colnames(infobox_data) = infobox_data[1, ]
infobox_data = infobox_data[-1, ]
infobox_data$PAGE =premier_projet$PAGE
# Ici, on a fait le traitement pour récupérer les informations précises du projet de la vague 1
# Nous allons maintenant faire une boucle for pour récupérer des informations précises pour chaque projet des vagues 1 à 3


# Application pour les six premières vagues ####
## Pour les six premières vagues
localisation_info_box_prem = list(c(200, 44, 657, 177))
localisation_info_box_deux = list(c(200, 43, 448, 168))
dataset_inf_proj_1_3 = data.frame()

gc()
localisation_actuelle = list(c())
for(lign in 1:nrow(i_nov_1_3)){
  if(i_nov_1_3[lign, ]$VAGUE == 1){
    localisation_actuelle = localisation_info_box_prem
  }
  else{
    localisation_actuelle = localisation_info_box_deux
  }
  ## Récupérons les informations de ce projet
  infobox_prem = extract_tables(i_nov_1_3[lign, ]$URL_FICH_ORIGINE, 
                                pages = i_nov_1_3[lign, ]$PAGE,
                                area = localisation_actuelle,
                                guess = FALSE,
                                output = "matrix")
  
  ## Regardons et récupérons les informations utiles du projet
  lign_donn = matrix(nrow = nrow(infobox_prem[[1]]))
  # Initialisation des valeurs conditionnelles
  i = 1
  presence_contact = FALSE
  
  # Initalisation des lignes de données récupérées
  lign_donn = matrix(nrow = nrow(infobox_prem[[1]]))
  nb_lign_donnee = 0
  
  while(i <= nrow(infobox_prem[[1]]) & !presence_contact){
    # Si présence ">", alors ajouter une nouvelle ligne de donnée
    if((str_detect(infobox_prem[[1]][i, 1], ">") & i_nov_1_3[lign, ]$VAGUE == 1) | 
       (str_detect(infobox_prem[[1]][i, 1], ">", negate = TRUE) & i_nov_1_3[lign, ]$VAGUE %in% c(2, 3, 4, 5, 6)) & 
       str_detect(infobox_prem[[1]][i, 1], "CONTACT|©", negate = TRUE)){
      nb_lign_donnee = nb_lign_donnee + 1
      lign_donn[nb_lign_donnee, 1] = infobox_prem[[1]][i, 1]
    }
    
    else{
      # Si pésence de ces caractères, fin de la boucle while
      if(str_detect(infobox_prem[[1]][i, 1], "CONTACT") | 
         str_detect(infobox_prem[[1]][i, 1], "©")){
        presence_contact = TRUE
      }
      else{
        # Concaténation avec la ligne de donnée actuelle
        lign_donn[nb_lign_donnee, 1] = paste(lign_donn[nb_lign_donnee, 1], 
                                             infobox_prem[[1]][i, 1]) 
      }
    }
    i = i + 1
  }
  
  gc()
  ## Préparation des données
  # Transformation en dataframe
  infobox_proj = as.data.frame(lign_donn)
  
  infobox_proj %>% 
    filter(!is.na(V1)) %>% 
    mutate(
      colonne = str_split_i(V1, ">", 1),
      valeur = str_split_i(V1, ">", 2)
    ) %>% 
    select(-V1) %>% 
    t(.) %>% 
    as.data.frame(.) -> infobox_data
  
  
  colnames(infobox_data) = str_trim(infobox_data[1, ])
  infobox_data = infobox_data[-1, ]
  infobox_data$PAGE =i_nov_1_3[lign, ]$PAGE
  infobox_data$VAGUE = i_nov_1_3[lign, ]$VAGUE
  ## Ajout de la ligne de données
  dataset_inf_proj_1_3 = bind_rows(dataset_inf_proj_1_3, infobox_data)
}

# Préparation des données + fusion et exportation des trois premiers projets

## Sélection des colonnes + création de nouvelles colonnes
dataset_inf_proj_1_3 %>% 
  select(LOCALISATION, RÉALISATION, `MONTANT DU PROJET`, PAGE, VAGUE, `DONT AIDE PIA`, `FINANCÉ PAR`, `PILOTÉ PAR`) %>% 
  mutate(
    REALISATION_DEBUT = sapply(str_extract_all(RÉALISATION, "\\d{4}"), function(x) x[1]),
    REALISATION_FIN = sapply(str_extract_all(RÉALISATION, "\\d{4}"), function(x) x[2]),
    Montant_aide = sapply(str_extract_all(`FINANCÉ PAR`, "\\d{3,4} K\\€"), function(x) if(length(x) >= 1) x[1]),
    Montant_proj = sapply(str_extract_all(`FINANCÉ PAR`, "\\d{3,4} K\\€"), function(x) if(length(x) >= 2) x[2]),
    `DONT AIDE PIA` = ifelse(is.na(`DONT AIDE PIA`), Montant_aide, `DONT AIDE PIA`),
    `MONTANT DU PROJET` = ifelse(is.na(`MONTANT DU PROJET`), ifelse(!is.na(Montant_proj), Montant_proj, `MONTANT DU PROJE`), `MONTANT DU PROJET`)
   ) %>% select(LOCALISATION, `MONTANT DU PROJET`, PAGE, VAGUE, `DONT AIDE PIA`, REALISATION_DEBUT,
                REALISATION_FIN) -> data_proj_1_3

# Conversion en nombre des montants + d'aides
data_proj_1_3 %>% mutate(
  MONTANT = str_remove_all(`MONTANT DU PROJET`, " "),
  MONTANT = str_remove_all(`MONTANT`, "€"),
  MONTANT = ifelse(MONTANT == "NULL", NA, MONTANT),
  MONTANT = ifelse(substr(MONTANT, nchar(MONTANT), nchar(MONTANT))=="K", 
         as.integer(substr(MONTANT, 1, nchar(MONTANT) - 1)) * 1000,
         as.integer(MONTANT)),
  
  AIDE = str_remove_all(`DONT AIDE PIA`, " "),
  AIDE = str_remove_all(`AIDE`, "€"),
  AIDE = ifelse(AIDE == "NULL", NA, AIDE),
  AIDE = ifelse(substr(AIDE, nchar(AIDE), nchar(AIDE))=="K", 
                   as.integer(substr(AIDE, 1, nchar(AIDE) - 1)) * 1000,
                   as.integer(AIDE))
) %>% select(-c(`MONTANT DU PROJET`, `DONT AIDE PIA`)) -> data_proj_1_3

# Extraction des numéros de départements (localisation)
data_proj_1_3 %>% 
  mutate(
    localisation_prep = sapply(str_extract_all(LOCALISATION, "\\(\\w{2}"), function(x) str_flatten(x, " ")),
    localisation_prep = str_replace_all(localisation_prep, "\\(", ""),
    localisation_prep = ifelse(str_detect(LOCALISATION, "\\bBOUCHES[-\\s]DU[-\\s]RHÔNE\\b"), "13", localisation_prep),
    localisation_prep = ifelse(str_detect(LOCALISATION, "ALPES[-\\s]DE[-\\s]HAUTE[\\s-]*"), "04", localisation_prep),
    localisation_prep = ifelse(str_detect(LOCALISATION, "PAYS DE LA LOIRE"), "44", localisation_prep),
    DEPT = ifelse(str_detect(LOCALISATION, "PYRÉNÉES-ATLANTIQUE"), "64", localisation_prep)
  ) %>% 
  select(-c(LOCALISATION, localisation_prep))-> data_proj_1_3_V2

# Vérification du succès de l'opération
data_proj_1_3_V2 %>% 
  filter(DEPT == "")

# Préparer montant du projet : convertir en nombre (K : 1000, M : 1000000)
# Préparer les localisations : sapply(str_extract_all(data_proj_1_3$LOCALISATION, "\\d{2}"), function(x) str_flatten(x, " "))
# Cas particuliers des localisations : BOUCHES DU RHÔNE == 13,  ALPES-/ DE-/ HAUTE-/ PROVENCE == 04, PYRÉNÉES-ATLANTIQUE == 64 (à gérer avec une expression regex)
# Pays de la loire

# Fusionner i_nov_1_3 avec data_proj_1_3_V2 (VAGUE, PAGE)
dataset_i_nov_1_6_final = inner_join(i_nov_1_3, data_proj_1_3_V2, by=c("VAGUE", "PAGE"))

dataset_i_nov_1_6_final %>% filter(is.na(MONTANT)) # Les deux premiers sont NA car on pas leurs montants d'indiquées.
# Modification de la ligne 205, 215, 113 et 399 
dataset_i_nov_1_6_final[205, ]$MONTANT = 4400000000000
dataset_i_nov_1_6_final[215, ]$MONTANT = 632876
dataset_i_nov_1_6_final[113, ]$REALISATION_FIN = dataset_i_nov_1_6_final[113, ]$REALISATION_DEBUT
dataset_i_nov_1_6_final[399, ]$MONTANT = 900778
# 4 400 000 M € (VAGUE : 3, PAGE : 24)

# Vérification du succès de l'opération
dataset_i_nov_1_6_final %>% filter(is.na(MONTANT))

# Regardons pour les quatres dernières vagues (7 à 10) ####

# Prenons un exemple de projet dans l'une de ces vagues 
i_nov_7_10 = dataset_i_nov %>% filter(VAGUE %in% c(7, 8, 9, 10))

# Détection éventuelle de doublons au niveau des pages et vagues
i_nov_7_10 %>% select(PAGE, VAGUE) %>% filter(duplicated(.))

# Doublonc au niveau de :
# Page : 84, Vague : 7
# Page : 55, Vague : 10
# Page 47, Vague : 10

# Regardons les lignes concernées
ligns_doublons = i_nov_7_10 %>% 
  filter((VAGUE == 7 & PAGE == 84) | (VAGUE == 10 & PAGE %in% c(55, 47))) 
# !!! Attention, erreur dans le sommaire pour la ligne 14 (Page est égale à 35 non à 32),
# pour la ligne 53 (Page est égale à 83 et non à 84)
# !!!
i_nov_7_10[14, ]$PAGE = 35
i_nov_7_10[53, ]$PAGE = 83
i_nov_7_10[208, ]$PAGE = 54
i_nov_7_10[202, ]$PAGE = 49

# Préparation à l'extraction
premier_projet = i_nov_7_10[1, ]
localisation_info_box_prem = locate_areas(premier_projet$URL_FICH_ORIGINE, pages= premier_projet$PAGE)
localisation_info_box_prem = list(c(179, 111, 248, 253))

# Extraction des données
extraction_7_10 = extract_tables(premier_projet$URL_FICH_ORIGINE, 
                                pages = premier_projet$PAGE,
                                area = localisation_info_box_prem,
                                guess = FALSE,
                                col_names = FALSE)

# Transposition des données
data_extract = transpose(extraction_7_10[[1]])

# Renommer les colonnes
colnames(data_extract) = data_extract[1, ]
data_extract = data_extract[-1, ]

# Vérification du succès de l'opération
data_extract

# Application de l'extraction sur les quatre derniers fichiers PDF
dataset_inf_proj_7_10 = data.frame()

gc()
localisation_actuelle = list(c())
localisation_actuelle = localisation_info_box_prem
for(lign in 1:nrow(i_nov_7_10)){
  if(i_nov_7_10[lign, ]$PROJETS == "VIZZIA"){
    i_nov_7_10[lign, ]$PAGE = 55
  }
  ## Récupérons les informations de ce projet
  extraction_7_10 = extract_tables(i_nov_7_10[lign, ]$URL_FICH_ORIGINE, 
                                pages = i_nov_7_10[lign, ]$PAGE,
                                area = localisation_actuelle,
                                guess = FALSE,
                                col_names = FALSE)
  print(extraction_7_10)
  print(i_nov_7_10[lign, ])
  print(i_nov_7_10[lign, ]$URL_FICH_ORIGINE)
  print(i_nov_7_10[lign, ]$PAGE)
  
  # Transposition des données
  data_extract = transpose(extraction_7_10[[1]])
  
  # Renommer les colonnes
  colnames(data_extract) = data_extract[1, ]
  data_extract = data_extract[-1, ]
  
  # Ajout des colonnes Page et Vague
  data_extract$PAGE = i_nov_7_10[lign, ]$PAGE
  data_extract$VAGUE = i_nov_7_10[lign, ]$VAGUE
  
  dataset_inf_proj_7_10 = bind_rows(dataset_inf_proj_7_10, data_extract)
  gc()
}

# Vérification de doublons
dataset_inf_proj_7_10 %>% select(PAGE, VAGUE) %>% filter(duplicated(.))

# Préparation des données récupérées

# Modification de la ligne 214
dataset_inf_proj_7_10[214, ]$Réalisation = "2023 - 2025"

# Séparation entre les années de réalisation (REALISATION_DEBUT et REALISATION_FIN)
# Conversion en numérique des montants (MONTANT et AIDE)
# Gestion des départements (DEPT)
dataset_inf_proj_7_10 %>% 
  mutate(
         REALISATION_DEBUT = sapply(str_extract_all(Réalisation, "\\d{4}"), function(x) x[1]),
         REALISATION_FIN = sapply(str_extract_all(Réalisation, "\\d{4}"), function(x) x[2]),
         REALISATION_FIN = ifelse(is.na(REALISATION_FIN), REALISATION_DEBUT, REALISATION_FIN),
         MONTANT = as.numeric(str_remove_all(`Montant du projet`, "(\\s|\\€)")),
         AIDE = as.numeric(str_remove_all(`Aide accordée`, "(\\s|\\€)")),
         DEPT = str_remove_all(Localisation, "(et|\\-|\\&)"),
         DEPT = str_remove(DEPT, "\\s")
         ) %>% 
  select(-c(Réalisation, `Montant du projet`, `Aide accordée`, Localisation)) -> data_inf_proj_7_10

# Fusion entre i_nov_7_10 et data_inf_proj_7_10 (VAGUE, PAGE)
dataset_i_nov_7_10_final = inner_join(i_nov_7_10, data_inf_proj_7_10, by=c("VAGUE", "PAGE"))

# Vérification du succès de l'opération
head(dataset_i_nov_7_10_final)
tail(dataset_i_nov_7_10_final)
str(dataset_i_nov_7_10_final)

# Fusion et exportation de nos données préparées pour de futures analyses  ####
dataset_final_somm_inf = rbind(dataset_i_nov_1_6_final, dataset_i_nov_7_10_final)

# Vérification du succès de l'opération
head(dataset_final_somm_inf)
tail(dataset_final_somm_inf)
str(dataset_final_somm_inf)

# Exportation des données dans un fichier CSV
write.csv2(dataset_final_somm_inf, file = "../Data/i_nov_vag_1_10_somm_inf.csv",
           row.names = FALSE, fileEncoding = "utf-8")
##########################################################-
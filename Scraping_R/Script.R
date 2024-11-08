#################################-
# Auteur : Julien RENOULT
# Sujet : concours i-nov (scraping PDF du sommaire/index)
# Date : 20/09/2024 - 21/09/2024
################################-

# ---- Chargement des librairies ----
library(tabulapdf) # à la place de tabulate
library(dplyr) # fonctions de mutate, rename, group_by, summarise, etc
library(stringr) # fonctions pour gérer les chaînes de caractères
library(ggplot2) # fonctions pour faire des graphique
library(data.table) # fonction de fusion (rbindlist)
options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx8192m"))
####################################-

# ---- Importation des données (1/2) ----

# ---- Extraction sur un fichier PDF ----

# Indiquons les colonnes et lignes du tableau PDF à extraire pour les six premières vagues ####
#location_table_vg1_6 = locate_areas("../Data/vague_1_6/i-nov-palmar-s-vague-1-19087.pdf",
#                           pages= c(4, 5))

# Vérification du succès de l'opération
location_table_vg1_6 = list(c(89, 106, 771, 509), c(89, 106, 498, 509))
print(location_table_vg1_6)

# Extraire l'index/le sommaire des entreprises pour les premières vagues ####
i_nov_1 <- extract_tables("../Data/vague_1_6/i-nov-palmar-s-vague-1-19087.pdf", 
                      pages = c(4, 5),
                      area = location_table_vg1_6,
                      guess = FALSE)


# Vérification du succès de l'opération
i_nov_1
dataframe_1 = i_nov_1[[1]]
dataframe_2 = i_nov_1[[2]]
head(dataframe_1)
head(dataframe_2)

# Fusion des deux tableaux 
dataset_i_nov = rbindlist(i_nov_1)

# Indiquons les colonnes et lignes du tableau PDF à extraire pour les quatres dernières vagues ####

# Contexte : pour récupérer les données facilement, nous allons utiliser l'interface de la fonction locate_areas
# pour récupérer d'abord la thématique et ensuite les données associées en matrice.

# Ci-dessous on trouvera un exemple d'extraction d'une thématique et de ses données : ####

# Indiquons la localisation de la thématique
location_table_vg7_10_the = locate_areas("../Data/vague_7_10/i-nov-palmar-s-vague-10-28601.pdf",
                                    pages= 18)

# Indiquons la localisation des données associées à la thématique
location_table_vg7_10_donn = locate_areas("../Data/vague_7_10/i-nov-palmar-s-vague-10-28601.pdf",
                                          pages= 18)

# Vérification du succès de l'opération
print(location_table_vg7_10_the)
print(location_table_vg7_10_donn)

# Extraire la thématique et les données associées ####
i_nov_10_the <- extract_tables("../Data/vague_7_10/i-nov-palmar-s-vague-10-28601.pdf", 
                          pages = 18,
                          area = location_table_vg7_10_the,
                          guess = FALSE,
                          output = "matrix")

i_nov_10_the

i_nov_10_donn <- extract_tables("../Data/vague_7_10/i-nov-palmar-s-vague-10-28601.pdf", 
                               pages = 18,
                               area = location_table_vg7_10_donn,
                               guess = FALSE,
                               output = "matrix")
i_nov_10_donn

# Préparation de la petite table de donnée

## Transformation en DataFrame
i_nov_10_data = data.frame(i_nov_10_donn)

## Renommer les noms de colonnes + Ajout de la thématique

## Initialisation de la thématique
str_theme = str_replace(i_nov_10_the[[1]][1,1], "Thématique\\s*[:|\\-]*\\s*", "")

## Application des modifications
i_nov_10_data %>% 
  rename(NOM_ENTREPRISE = X1,
         NOM_PROJET = X2,
         PAGE = X3) %>% 
  mutate(
    NOM_PROJET = str_replace(NOM_PROJET, "Projet ", ""),
    THEMATIQUE = str_theme,
    PAGE = as.numeric(PAGE)
  ) -> i_nov_10_data

## Vérification du succès de l'opération
head(i_nov_10_data)
str(i_nov_10_data)

# Dans le cas d'une thématique à deux lignes ####
# Indiquons la localisation de la thématique
location_table_vg8_the = locate_areas("../Data/vague_7_10/i-nov-palmar-s-vague-8-19069.pdf",
                                         pages= 22)

# Indiquons la localisation des données associées à la thématique
location_table_vg8_donn = locate_areas("../Data/vague_7_10/i-nov-palmar-s-vague-8-19069.pdf",
                                          pages= 22)

i_nov_8_the <- extract_tables("../Data/vague_7_10/i-nov-palmar-s-vague-8-19069.pdf", 
                               pages = 22,
                               area = location_table_vg8_the,
                               guess = FALSE,
                               output = "matrix")

## Création de la condition sur le nombre de lignes
if(nrow(i_nov_8_the[[1]]) == 2){
  i_nov_8_the[[1]][1, 1] = paste(i_nov_8_the[[1]][1, 1], i_nov_8_the[[1]][2, 1])
  i_nov_8_the[[1]] = i_nov_8_the[[1]][-2, ]
} else{
  i_nov_8_the[[1]] = c(i_nov_8_the[[1]][1], i_nov_8_the[[1]][2])
}
# Commentaire :
## Il faut maintenant maximiser l'automatisation de l'extraction de tableaux PDF
## pour qu'on puisse chercher en détail les différentes informations 
## de chaque projet à l'aide du numéro de page.
#########################################-


# ---- Importation des données (2/2) ----
# ---- Extraction automatique de chaque fichier PDF ----
liste_fich = list.files(path="../Data/vague_1_6")
liste_fich = paste("../Data/vague_1_6/", liste_fich, sep = "")

# Application de l'extraction pour les six premiers fichiers PDF
# Pour les 6 premières vagues : #######
liste_loc_nov_1_6 = list()
liste_inf_nov_1_6 = list(list(), list(), list(), 
                         list(), list(), list())

# Récupération des coordonnées pour l'extraction des trois fichiers
#for(i in 1:3){
#  print(paste0("Pour le fichier de la vague ", i))
#  liste_loc_nov_1_6[[i]] = locate_areas(liste_fich[i],
#                                      pages= c(4, 5))
#}

# Les coordonnées des trois premiers fichiers
liste_loc_nov_1_6 = list(list(c(89, 105, 772, 512), c(89, 104, 500, 509)), 
                         list(c(99, 88, 748, 523), c(99, 89, 491, 521)), 
                         list(c(105, 86, 747, 533), c(105, 91, 388, 540)),
                         list(list(c(86, 87, 793, 211), c(78, 88, 583, 210)), 
                              list(c(94, 215, 797, 312), c(76, 218, 588, 314)),
                              list(c(89, 312, 793, 511), c(77, 316, 577, 513)),
                              list(c(93, 506, 792, 537), c(77, 507, 595, 537))
                              ))

# Application de l'extraction pour les trois premiers fichiers
for(i in 1:3){
  print(paste0("Pour le fichier de la vague ", i))
  liste_inf_nov_1_6[[i]] = extract_tables(liste_fich[i], 
                            pages = c(4, 5),
                            area = liste_loc_nov_1_6[[i]],
                            guess = FALSE)
}

# Vérification du succès de l'opération
liste_inf_nov_1_6

## Pour les vagues 4 à 6, on va récupérer chaque localisation de colonne (ENTREPRISE, PROJET, PAGE)
## Indiquer chaque colonne avec ses données et conservation de ses coordonnées
#print(paste0("Pour le fichier de la vague ", 5))
  
# Récupération de la colonne ENTREPRISE
#liste_loc_nov_1_6[[4]][[1]] = locate_areas(liste_fich[5],
#                                        pages= c(4, 5))
  
# Récupération de la colonne PROJET
#liste_loc_nov_1_6[[4]][[2]] = locate_areas(liste_fich[5],
#                                        pages= c(4, 5))
  
# Récupération de la colonne THÉMATIQUE
#liste_loc_nov_1_6[[4]][[3]] = locate_areas(liste_fich[5],
#                                        pages = c(4, 5))

# Récupération de la colonne PAGES
#liste_loc_nov_1_6[[4]][[4]] = locate_areas(liste_fich[5],
#                                           pages = c(4, 5))

# Application de l'extraction de chaque fichier en utilisant les coordonnées récupérées
for(i in 4:6){
  print(paste0("Pour le fichier de la vague ", i))
  
  # Pour la colonne ENTREPRISE
  liste_inf_nov_1_6[[i]][[1]] = extract_tables(liste_fich[i], 
                                          pages = c(4, 5),
                                          guess = FALSE,
                                          area = liste_loc_nov_1_6[[4]][[1]],
                                          output = "tibble")
  
  # Pour la colonne PROJET
  liste_inf_nov_1_6[[i]][[2]] = extract_tables(liste_fich[i], 
                                               pages = c(4, 5),
                                               guess = FALSE,
                                               area = liste_loc_nov_1_6[[4]][[2]],
                                               output = "tibble")
  # Pour la colonne THEMATIQUE
  liste_inf_nov_1_6[[i]][[3]] = extract_tables(liste_fich[i], 
                                               pages = c(4, 5),
                                               guess = FALSE,
                                               area = liste_loc_nov_1_6[[4]][[3]],
                                               output = "tibble")
  # Pour la colonne PAGES
  liste_inf_nov_1_6[[i]][[4]] = extract_tables(liste_fich[i], 
                                               pages = c(4, 5),
                                               guess = FALSE,
                                               area = liste_loc_nov_1_6[[4]][[4]],
                                               output = "tibble")
}
## Vérification du succès de l'opération
liste_inf_nov_1_6[[4]]
liste_inf_nov_1_6[[5]]
liste_inf_nov_1_6[[6]]

## Création des DataFrames pour les six premiers fichiers PDF

### Les trois premiers vont juste à avoir faire une fusion ####
for(i in 1:3){
  liste_inf_nov_1_6[[i]]= rbindlist(liste_inf_nov_1_6[[i]])
}

# Vérification du succès de l'opération
str(liste_inf_nov_1_6[[1]])
str(liste_inf_nov_1_6[[2]])
str(liste_inf_nov_1_6[[3]])

### Pour les trois derniers, il y aura plusieurs étapes ####
### on doit faire d'abord une fusion de nos colonnes,
### puis une création de dataframes avec ces colonnes

# Dans le quatrième fichier, on peut observer :
# - deux noms d'entreprises qui se faient sur deux lignes (CONSTRUCTION COMPOSITES BOIS \n LIGNOROC,
# LA COMPAGNIE DES MOBILITÉS \n Geovelo)

# Dans le cinquième fichier, on a :
# - un nom de projet sur deux lignes (Hydrogen Carbon Components)

# Dans le sixième fichier, on a :
# - un nom d'entreprise sur deux lignes (METAVERSE TECHNOLOGIES France)
# - un nom de thématique sur deux lignes (Adaption des territoires 
# au changement climatique et métrologie des expositions environnementales)
# - un nom de projet sur deux lignes (BOURRELET COUPE FEU ECOCONCU)

# Il faut les mettre en une seule ligne pour pouvoir faire la création de nos dataframes

# Pour le quatrième fichier PDF (exemple) ####

# Colonne ENTREPRISES ####
entreprises = rbindlist(liste_inf_nov_1_6[[4]][[1]])

# Vérification du succès de l'opération
head(entreprises)
str(entreprises)

# Régler les problèmes de valeurs sur deux lignes
# Le cas de CONSTRUCTION et Geovelo
entreprises[15, ] = paste(entreprises[15, ], entreprises[16, ], sep = " ")
entreprises[32, ] = paste(entreprises[32, ], entreprises[33, ], sep = " ")
entreprises = entreprises[c(-16, -33, -64), ] # 64 car il représente une partie de l'index (inutile)

# Vérification du succès de l'opération
entreprises[15, ]
entreprises[31, ]

# Colonne PROJETS ####
projets = rbindlist(liste_inf_nov_1_6[[4]][[2]])
projets = projets[-62, ] 
# 61 car il représente une partie de l'index (inutile)

# Vérification du succès de l'opération
tail(projets)
str(projets)

# Colonne THEMATIQUE ####
thematique = rbindlist(liste_inf_nov_1_6[[4]][[3]])

# Vérification du succès de l'opération
tail(thematique)
str(thematique)

# Colonne PAGES ####
pages = rbindlist(liste_inf_nov_1_6[[4]][[4]])

# Vérification du succès de l'opération
tail(pages)
str(pages)

# Création du dataframe
#liste_inf_nov_1_6[[4]] = data.frame(list(entreprises, projets, 
#                            thematique, pages))

# Vérification du succès de l'opération
#head(liste_inf_nov_1_6[[4]])
#str(liste_inf_nov_1_6[[4]])

# Pour les vagues de 4 à 6 ####
for(i in 4:6){
  
  # Fusion des colonnes
  entreprises = rbindlist(liste_inf_nov_1_6[[i]][[1]])
  projets = rbindlist(liste_inf_nov_1_6[[i]][[2]])
  thematique = rbindlist(liste_inf_nov_1_6[[i]][[3]])
  pages = rbindlist(liste_inf_nov_1_6[[i]][[4]])
  
  if(i == 4){
  # Régler les problèmes de valeurs sur deux lignes
  # Le cas de CONSTRUCTION et Geovelo
  entreprises[15, ] = paste(entreprises[15, ], entreprises[16, ], sep = " ")
  entreprises[32, ] = paste(entreprises[32, ], entreprises[33, ], sep = " ")
  entreprises = entreprises[c(-16, -33, -64), ] # 64 car il représente une partie de l'index (inutile)
  
  projets = projets[-62, ] # index inutile
  }
  if(i == 5){
    # Le cas du projet Hydrogen Carbon Components
    projets[42, ] = paste(str_replace(projets[42, ], "-", ""), projets[43, ], sep = " ")
    projets = projets[-43, ]
  }
  if(i == 6){
    # Le cas de METAVERSE
    entreprises[28, ] = paste(entreprises[28, ], entreprises[29, ], sep = " ")
    entreprises = entreprises[c(-29, -53), ] # 53 car il représente une partie de l'index (inutile)
    
    # Le cas de BOURRELET COUPE FEU ECOCONCU
    projets[16, ] =  paste(str_replace(projets[16, ], "-", ""), projets[17, ], sep = " ")
    projets = projets[c(-17, -53), ] # -53 index inutile
    
    # Le cas de la thématique : Adaptation des territoires au changement climatique et \n métrologie des expositions environnementales
    thematique[4, ] = paste(thematique[4, ], thematique[5, ], sep = " ")
    thematique[32, ] = paste(thematique[32, ], thematique[33, ], sep = " ")
    thematique[34, ] = paste(thematique[34, ], thematique[35, ], sep = " ")
    thematique[45, ] = paste(thematique[45, ], thematique[46, ], sep = " ")
    thematique[53, ] = paste(thematique[53, ], thematique[54, ], sep = " ")
    thematique[55, ] = paste(thematique[55, ], thematique[56, ], sep = " ")
    thematique = thematique[c(-5, -33, -35, -46, -54, -56), ]
  }
  
  # Vérification du succès de l'opération
  print("------------------------------------------------")
  print(paste0("Structure du fichier de la vague ", i))
  str(data.frame(list(entreprises, projets, 
                               thematique, pages)))
  
  # Création du dataframe en question
  liste_inf_nov_1_6[[i]] = data.frame(list(entreprises, projets, 
                                           thematique, pages))
}


# Pour les 4 dernières vagues : #####
liste_fich_2 = list.files(path="../Data/vague_7_10")
liste_fich_2 = paste("../Data/vague_7_10/", liste_fich_2, sep = "")

# Les coordonnées pour le fichier 10
liste_coord_fich_10 = list(list(c(153, 85, 166, 485), c(168, 87, 301, 481)),
                   list(c(301, 85, 313, 485), c(313, 88, 650, 488)),
                   list(c(153, 87, 164, 481), c(167, 92, 286, 491)),
                   list(c(288, 90, 298, 485), c(301, 92, 543, 483)))

liste_coord_fich_7 = list(list(c(153, 110, 167, 482), c(167, 113, 212, 484)),
                          list(c(212, 111, 222, 486), c(226, 111, 286, 487)),
                          list(c(286, 110, 301, 493), c(303, 111, 420, 492)),
                          list(c(420, 111, 435, 484), c(436, 111, 631, 493))
                          ) # à compléter plus tard

## Création de la fonction d'extraction
extraction_pdf_vag_7_10 = function(chemin, pages, vc_nb_them, coord=list()){
  # Fonction : extraire la/les thématiques avec ses données associées réunis dans un seul tableau
  
  # Arguments :
  ## chemin : chemin du fichier
  ## pages : vecteur numérique représentant les pages où regardées
  ## vc_nb_them : vecteur numérique représentant pour chacune des pages, le nombre de thématiques apparaissant
  
  # Utilisation :
  ## locate_area : les coordonnées permettant de trouver la thématique et les valeurs associées
  ## extract_table : application de l'extraction
  
  # Situation :
  ## Données thématiques sur une autre page : demander à l'utilisateur si la thématique en question possède des données dans l'autre page,
  ## si oui, on va aller les chercher dans les deux pages (locate puis extract), sinon juste sur la page existante
  ## Thématique sur deux lignes : le mettre sur une seule ligne (voir exemple)
  
  # création du boucle for sur le nombre de pages
  ## Initialisation des trois valeurs de retours (coordonnées, thématiques et les données/dataframes)
  liste_coord = vector(mode = "list", length = sum(vc_nb_them))
  the_det = vector(mode = "list", length = sum(vc_nb_them))
  data_the = vector(mode = "list", length = sum(vc_nb_them))
  decalage = 1
  for(ind_1 in 1:length(pages)){
    # récupération du nombre de pages et du nombre de thématique
    page = pages[ind_1]
    nb_them = vc_nb_them[ind_1]
    
    # création du boucle for sur le nombre de thèmes
    for(ind_2 in 1:nb_them){
      
      # Indiquons la localisation de la thématique
      plus_pages = FALSE
      if(length(coord) == 0){
        print(paste0("Localisation de la thématique numéro ", ind_2))
        location_table_vg7_10_the = locate_areas(chemin,
                                                 pages= page)
        
        # Question posé à l'utilisateur si les données se trouve en plus sur la page à côté
        plus_pages = ifelse(readline(prompt = "Est-ce que la thématique encadrée à des données situées sur l'autre page ? [y/n]") == "y", 
                            TRUE, FALSE)
        #print(plus_pages)
        
        if(plus_pages){
          vector_page = c(page, page + 1)
        }else{
          vector_page = c(page)
        }
        #print(vector_page)
        
        # Indiquons la localisation des données associées à la thématique
        print(paste0("Localisation des données associées au numéro ", ind_2))
        location_table_vg7_10_donn = locate_areas(chemin,
                                                  pages= vector_page)
      }else{
        plus_pages = FALSE
        vector_page = c(page)
        location_table_vg7_10_the = list(coord[[decalage]][[1]])
        location_table_vg7_10_donn = list(coord[[decalage]][[2]])
      }
      
      # Extraction des données
      print(paste0("Extraction de la thématique ", ind_2))
      print(location_table_vg7_10_the)
      
      i_nov_the <- extract_tables(chemin, 
                                     pages = vector_page,
                                     area = location_table_vg7_10_the[1],
                                     guess = FALSE,
                                     output = "matrix")
      
      # Préparation des résultats du thème
      if(nrow(i_nov_the[[1]]) == 2){
        i_nov_the[[1]][1, 1] = paste(i_nov_the[[1]][1, 1], i_nov_the[[1]][2, 1])
        i_nov_the[[1]] = i_nov_the[[1]][-2, ]
      } else{
        i_nov_the[[1]] = c(i_nov_the[[1]][1], i_nov_the[[1]][2])
      }
      
      print(paste0("Extraction des données de la thématique ", ind_2))
      print(location_table_vg7_10_donn)
      
      i_nov_donn <- extract_tables(chemin, 
                                      pages = vector_page,
                                      area = location_table_vg7_10_donn,
                                      guess = FALSE,
                                      output = "matrix")
      #print(i_nov_donn)
      
      ## Dans le cas où on a cherché les données dans deux feuilles différentes
      if(plus_pages){
          i_nov_donn = rbind(i_nov_donn[[1]], i_nov_donn[[2]])
      }
      
      ## Transformation en DataFrame
      i_nov_data = data.frame(i_nov_donn)
      
      ## Virer la présence de Thématique (avec Regex)
      str_theme = str_replace(i_nov_the[[1]][1], "Thématique\\s*[:|\\-]*\\s*", "")
      i_nov_the[[1]][1] = str_theme
      
      ## Application des modifications
      i_nov_data %>% 
        rename(NOM_ENTREPRISE = X1,
               NOM_PROJET = X2,
               PAGE = X3) %>% 
        mutate(
          NOM_PROJET = str_replace(NOM_PROJET, "Projet ", ""),
          THEMATIQUE = str_theme,
          PAGE = as.numeric(PAGE)
        ) -> i_nov_data
      
      # Enregistrement des résultats d'extractions
      the_det[[decalage]] = i_nov_the
      data_the[[decalage]] = i_nov_data
      
      # Enregistrement des coordonnées (pour les sauvegarder)
      liste_coord[[decalage]][1] = location_table_vg7_10_the[1]
      liste_coord[[decalage]][2] = location_table_vg7_10_donn
      
      # Ajout de +1 au décalage
      decalage = decalage + 1
    }
  }
  
  return(list(list(data_the), list(the_det), list(liste_coord)))
}

# Application de la fonction ####
data_fich_10 = extraction_pdf_vag_7_10(liste_fich_2[1], c(18, 19), c(2, 2), liste_coord_fich_10)
data_fich_7 = extraction_pdf_vag_7_10(liste_fich_2[2], c(12, 13), c(5, 3)) # c(12, 13), c(5, 3)
data_fich_8 = extraction_pdf_vag_7_10(liste_fich_2[3], c(22, 23), c(7, 3))
data_fich_9 = extraction_pdf_vag_7_10(liste_fich_2[4], c(18, 19), c(3, 1))

# Fusion des différents datasets de chaque thématique ####
liste_inf_nov_7_10 = vector(mode = "list", length = 4)
liste_inf_nov_7_10[1] = list(rbindlist(data_fich_7[[1]][[1]]))
liste_inf_nov_7_10[2] = list(rbindlist(data_fich_8[[1]][[1]]))
liste_inf_nov_7_10[3] = list(rbindlist(data_fich_9[[1]][[1]]))
liste_inf_nov_7_10[4] = list(rbindlist(data_fich_10[[1]][[1]]))

# Ajout des vagues + url du fichier d'origine pour chaque dataset et les fusionner ####

# Ajout d'informations
for(i in 1:6){
  liste_inf_nov_1_6[[i]] %>% 
    mutate(VAGUE = i,
           URL_FICH_ORIGINE = liste_fich[i]) -> liste_inf_nov_1_6[[i]]
}

for(i in 1:4){
  liste_inf_nov_7_10[[i]] %>% 
    mutate(VAGUE = i + 6,
           URL_FICH_ORIGINE = liste_fich_2[i]) -> liste_inf_nov_7_10[[i]]
}

# Fusion des datasets
dataset_i_nov_1_6 = rbindlist(liste_inf_nov_1_6)
dataset_i_nov_7_10 = rbindlist(liste_inf_nov_7_10)

# RENOMMER LES COLONNES
dataset_i_nov_7_10 %>% 
  rename(ENTREPRISES = NOM_ENTREPRISE,
         PROJETS = NOM_PROJET, 
         THÉMATIQUE = THEMATIQUE) %>% 
  mutate(THÉMATIQUE = str_to_upper(THÉMATIQUE)) -> dataset_i_nov_7_10

# Vérification du succès de l'opération
head(dataset_i_nov_1_6)
tail(dataset_i_nov_1_6)
str(dataset_i_nov_1_6)

head(dataset_i_nov_7_10)
tail(dataset_i_nov_7_10)
str(dataset_i_nov_7_10)

# Fusion des deux derniers datasets
dataset_final = rbind(dataset_i_nov_1_6, dataset_i_nov_7_10)

# Vérification du succès de l'opération
head(dataset_final)
tail(dataset_final)
str(dataset_final)

# Envoie de ces données dans un fichier CSV
write.csv2(dataset_final, file = "../Data/i_nov_vag_1_10.csv",
           row.names = FALSE, fileEncoding = "utf-8")
#########################################-
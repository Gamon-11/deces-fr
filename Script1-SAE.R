#' ---
#' title : "Analyse des décès de 2019 à 2021"
#' author : "BOUCHER Nathan, DIOP Mandir,GAMONDELE Maxime"
#' date : "23 février 2024"
#' ---

# ============================================================================= #
# IUT GON - (SSTI206) Analyse de données, reporting et datavisualisation
# Données : Données décès de la France de 2019 à 2022
# source : https://www.insee.fr/fr/information/6800675
# Source : https://www.data.gouv.fr/fr/datasets/fichier-des-personnes-decedees/
# ============================================================================= #

# Début du script

# ======== Lecture et préparation des données ======== # 

#---- Chargement des données ----#

widths <- c(80, 1, 8, 5, 30, 30, 8, 5, 9)

# Fichier 1 : deces-2019.txt

année_2019=read.fwf(file="../Data/deces-2019.txt",
              widths = widths,
              col.names = c("Nom.Prénom", "Sexe", "Date.naissance",
                            "Code.lieu.naissance", "Commune.naissance", 
                            "Pays.naissance", "Date.décès", "Code.lieu.décès", 
                            "Numéro.acte.décès"),
              header = FALSE)

# Fichier 2 : deces-2020.txt

année_2020=read.fwf(file="../Data/deces-2020.txt",
                    widths = widths,
                    col.names = c("Nom.Prénom", "Sexe", "Date.naissance", 
                                  "Code.lieu.naissance", "Commune.naissance", 
                                  "Pays.naissance", "Date.décès", 
                                  "Code.lieu.décès", "Numéro.acte.décès"),
                    header = FALSE)

# Fichier 3 : deces-2021.txt

année_2021=read.fwf(file="../Data/deces-2021.txt",
                    widths = widths,
                    col.names = c("Nom.Prénom", "Sexe", "Date.naissance", 
                                  "Code.lieu.naissance", "Commune.naissance", 
                                  "Pays.naissance", "Date.décès",
                                  "Code.lieu.décès", "Numéro.acte.décès"),
                    header = FALSE)

# Fichier 4 : deces-2022.txt

année_2022=read.fwf(file="../Data/deces-2022.txt",
                    widths = widths,
                    col.names = c("Nom.Prénom", "Sexe", "Date.naissance", 
                                  "Code.lieu.naissance", "Commune.naissance", 
                                  "Pays.naissance", "Date.décès", 
                                  "Code.lieu.décès", "Numéro.acte.décès"),
                    header = FALSE)


# Agrégation des fichiers dans une liste

liste <- list("2019"=année_2019,
                "2020"=année_2020,
                "2021"=année_2021,
                "2022"=année_2022)
str(liste,
    max.level = 1)



# Importation des données du fichier v_departement_2023.csv 
# dans une table departement


departement = read.table(file = "../Data/v_departement_2023.csv",
                         sep = ",",
                         quote = "/",
                         header = TRUE)
str(departement)

# Importation des données du fichier v_region_2023.csv dans une table region

region = read.table(file = "../Data/v_region_2023.csv",
                    sep = ",",
                    quote = "/",
                    header = TRUE)
str(region)

# Agrégation des données dans un objet data à l'aide de do.call() et rbind

data <- do.call(rbind,
                liste)
str(data)

dataset <- within(data,
                  {
                    Sexe <- factor(Sexe, levels = c("1", "2"),
                                   labels = c("Masculin","Féminin"))
                    Date.naissance <- as.Date(as.character(Date.naissance),
                                              format = "%Y%m%d")
                    Code.lieu.naissance <- as.character(Code.lieu.naissance)
                    Date.décès <- as.Date(as.character(Date.décès),
                                          format = "%Y%m%d")
                    Age.décès <- round((difftime(Date.décès,
                                                           Date.naissance,
                                                           units = "days"))/365.25,2)
                    Code.lieu.décès <- as.character(Code.lieu.décès)
                    Code.département.décès <- substr(as.character
                                                     (Code.lieu.décès),1,2)
                    Code.département.naissance <- substr(as.character
                                                         (Code.lieu.naissance),1,2)
                  })

str(dataset)

# Sélection des variables dans dataset avec un ordre précis

dataset[, c("Sexe","Date.naissance","Code.lieu.naissance",
            "Date.décès","Code.lieu.décès","Age.décès",
            "Code.département.décès","Code.département.naissance")] -> dataset

str(dataset)


# Préparation des données (jointure et nettoyage) 

INSEE <- merge(departement,
               region,
               by = "REG")
str(INSEE)

# Utilisation de within() pour nettoyer et transformer les données

INSEE <- within(INSEE, {
  Code.département <- substr(as.character(DEP),1,3)
  Code.région <- (as.integer(REG))
  Région <- as.character(LIBELLE.y)
  Département <- as.character(LIBELLE.x)
})
str(INSEE)

# Sélection des 4 variables

subset(INSEE,
       select = c(Code.département,Code.région,Région,Département))-> INSEE
str(INSEE)

#Jointure de INSEE et dataset

# Première jointure, celle-ci nous permettra d'avoir des noms de départements
# à la place de codes département dans le dataset final

fusion1 <- merge(dataset,
                 INSEE,
                 by.x = "Code.département.décès",
                 by.y = "Code.département",
                 all.x = TRUE)
str(fusion1)

# Seconde jointure,celle-ci nous permettra d'avoir des régions
# à la place de code région dans le dataset final

fusion2 <- merge(fusion1,
                 INSEE,
                 by.x = "Code.département.naissance",
                 by.y = "Code.département",
                 all.x = TRUE)
str(fusion2)

# Dans les deux jointures, nous cherchons à garder toutes les lignes de dataset,
# c'est pour cela que nous utilisons l'argument "all.x = TRUE"

# Extrait des 8 variables d'intéressement
datasetfinal <- subset(fusion2,
                       select = c("Sexe","Date.naissance","Date.décès",
                                  "Age.décès","Région.y","Département.y",
                                  "Région.x","Département.x"))
str(datasetfinal)

# Modification du nom des variables

names(datasetfinal) <- c("Sexe", "Date.naissance", "Date.décès", "Age.décès",
                         "Région.naissance", "Département.naissance",
                         "Région.décès", "Département.décès")

datasetfinal <- within(datasetfinal,{
  Région.naissance <- factor(Région.naissance)
  Département.naissance <- factor(Département.naissance)
  Région.décès <- factor(Région.décès)
  Département.décès <- factor(Département.décès)
})

str(datasetfinal)

# Enregistrement du datasetfinal dans un fichier "deces-2019_to_2022.csv"
# Sauvegarder le DataFrame en tant qu'objet RDS

saveRDS(datasetfinal,
        file = "Décès.RDS")

# Enregistrer le DataFrame dans un fichier CSV

write.csv(datasetfinal,
          file = "deces-2019_to_2022.csv",
          row.names = FALSE)



# -*- coding: utf-8 -*-
"""
Éditeur de Spyder

Ceci est un script temporaire.
"""
import json
import csv

resultatFinal = []

try:
    # Lecture du fichier JSON
    projetPollution = open("concentrations-polluants-dans-lair-ambiant.json", "r", encoding="utf-8")
    donnees = json.load(projetPollution)
except FileNotFoundError:
    print("Erreur : Fichier introuvable.")
    donnees = []
finally:
    # On ferme le fichier proprement
    if 'projetPollution' in locals() and not projetPollution.closed:
        projetPollution.close()

# "utf-8-sig" pour les accents Excel, "newline=''" pour ne pas sauter de lignes
fichierCSV = open("mon_resultat.csv", "w", encoding="utf-8-sig", newline='')
ecritCSV = csv.writer(fichierCSV, delimiter=";")

# Écriture des titres/ colonnes dans excel
ecritCSV.writerow(["Nom_Station", "Latitude", "Longitude", "Date de prélèvement", "Date de fin", "Heure", "Nom_Polluant", "Valeur", "Unité"])


nbligne = 0

for i in range(len(donnees)): 
   
    try:
        # Récupération des infos 
        
        station = donnees[i]["fields"]["nom_station"]
        latitude = donnees[i]["fields"]["geo_point"][0]
        longitude = donnees[i]["fields"]["geo_point"][1]
        
        # 1. Date de Début (Format français)
        dateTemp = donnees[i]["fields"]["date_debut"]     
        dateSplit = dateTemp.split("-")                 
        dateFR = dateSplit[2] + "/" + dateSplit[1] + "/" + dateSplit[0]
        
        # 2. Date de Fin (Format français)
        dateFinTemp = donnees[i]["fields"]["date_fin"]
        dateFinSplit = dateFinTemp.split("-")
        dateFinFR = dateFinSplit[2] + "/" + dateFinSplit[1] + "/" + dateFinSplit[0]
        
        # 3. Heure 
        timestamp = donnees[i]["record_timestamp"]
        heureTemp = timestamp.split("T")[1] 
        heure = heureTemp.split(".")[0]
        
        #4. Autres
        polluant = donnees[i]["fields"]["nom_poll"]
        valeur = donnees[i]["fields"]["valeur"]
        unite = donnees[i]["fields"]["unite"]

        # Écriture de la ligne complète
        ecritCSV.writerow([station, latitude, longitude, dateFR, dateFinFR, heure, polluant, valeur, unite])
        nbligne += 1
    
    except KeyError:
        # Si une info manque, on passe
        pass

fichierCSV.close()
print(f"Terminé ! Le fichier 'mon_resultat.csv' a été créé avec {nbligne} lignes.")

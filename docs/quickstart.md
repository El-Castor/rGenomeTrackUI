# Guide de démarrage rapide — rGenomeTrackUI

## 1. Lancer l'application

```bash
# Depuis le dossier racine du projet
bash scripts/run_app.sh
```

L'application s'ouvre dans votre navigateur à l'adresse `http://127.0.0.1:3838`.

> **Important** : N'utilisez pas `Rscript app.R` directement. Le script `run_app.sh` exporte
> les variables d'environnement conda nécessaires (PATH, DYLD_LIBRARY_PATH, etc.).

---

## 2. Créer un projet

1. Allez dans l'onglet **Projets**
2. Renseignez : nom, chemin du dossier, label du génome (ex: `hg38`)
3. Cliquez sur **Créer le projet**

Un dossier est créé avec la structure suivante :
```
mon_projet/
  data/           # Fichiers de données importés
  runs/           # Dossiers de chaque analyse
  project.yaml    # Config du projet
  file_registry.csv
```

---

## 3. Importer des fichiers

Dans l'onglet **Inputs** :

| Méthode | Quand l'utiliser |
|---|---|
| Upload | Fichiers < 30 Mb depuis votre ordinateur |
| Chemin local | Fichiers volumineux déjà sur le serveur |

Consultez l'onglet **Formats & templates** pour vérifier le format attendu et télécharger un template.

---

## 4. Configurer les tracks

Dans l'onglet **Track Builder** :

1. Sélectionnez un **type de track** (BigWig, BED, GTF…)
2. Donnez-lui un **nom** descriptif
3. Associez un **fichier** depuis le registre (filtré automatiquement par compatibilité)
4. Ajustez les **paramètres** (couleur, hauteur, style…)
5. Utilisez **↑/↓** pour réordonner, et **Activer/Désactiver** pour exclure temporairement

---

## 5. Définir les régions

Dans l'onglet **Régions & Figure** :

- **Région unique** : tapez `chr1:1000000-1250000`
- **Multi-régions** : uploadez un fichier BED (une ligne = une figure)

Ajustez ensuite les paramètres de figure (format, taille, DPI).

---

## 6. Vérifier et lancer

1. Onglet **Aperçu config** : vérifiez la checklist de validation et prévisualisez les fichiers générés
2. Onglet **Run** : nommez le run, cliquez **Préparer**, puis **Lancer**
3. Onglet **Résultats** : consultez et téléchargez les figures

---

## Commandes utiles

```bash
# Vérifier l'environnement conda
conda activate rgenometrackui
conda list | grep -E "pygenometracks|bedtools"

# Vérifier les dépendances R
Rscript scripts/check_r_dependencies.R

# Tester pyGenomeTracks en ligne de commande
pyGenomeTracks --tracks tracks.ini --region chr1:1000000-1250000 --outFileName test.png
```

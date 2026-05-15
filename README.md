# rGenomeTrackUI

![rGenomeTrackUI](assets/banniere.png)

**Interface graphique locale pour la génération de figures de genomic tracks via rGenomeTracks et pyGenomeTracks.**

[![Phase](https://img.shields.io/badge/phase-v0.4%20dark%20theme-blueviolet)](https://github.com)
[![Tests](https://img.shields.io/badge/tests-434%20passed-brightgreen)](tests/testthat)
[![License](https://img.shields.io/badge/license-GPL--3.0-green)](LICENSE)

---

## Table des matières

- [Description](#description)
- [Fonctionnalités](#fonctionnalités)
- [Formats de fichiers supportés](#formats-de-fichiers-supportés)
- [Prérequis](#prérequis)
- [Installation rapide (recommandée)](#installation-rapide-recommandée)
- [Installation manuelle](#installation-manuelle)
- [Lancer l'application](#lancer-lapplication)
- [Workflow typique](#workflow-typique)
- [Projet démo](#projet-démo)
- [Lancer les tests](#lancer-les-tests)
- [Vérifier l'environnement](#vérifier-lenvironnement)
- [Architecture du projet](#architecture-du-projet)
- [Phases de développement](#phases-de-développement)
- [Diagnostic des erreurs fréquentes](#diagnostic-des-erreurs-fréquentes)

---

## Description

**rGenomeTrackUI** est une application R Shiny locale permettant de configurer, générer et exécuter des figures de genomic tracks sans écrire manuellement les fichiers de configuration.

Elle s'appuie sur :
- **[rGenomeTracks](https://bioconductor.org/packages/rGenomeTracks/)** (Bioconductor) — wrapper R de pyGenomeTracks
- **[pyGenomeTracks](https://pygenometracks.readthedocs.io/)** (deeptools) — moteur de rendu des tracks

---

## Fonctionnalités

- **Gestion de projets** — créez plusieurs projets isolés (projets/runs séparés)
- **Track Builder** — ajoutez, réordonnez, activez/désactivez chaque track via une UI schéma-driven
- **16 types de tracks** — bedgraph, bigwig, BED, GTF, narrowPeak, links, domaines, HiC, etc.
- **5 templates** — configurations prédéfinies pour des usages courants
- **Génération automatique** — `tracks.ini`, `run_rGenomeTracks.R`, `run_pyGenomeTracks.sh`
- **Exécution intégrée** — lance rGenomeTracks ou pyGenomeTracks depuis l'interface, avec logs en temps réel
- **Historique des runs** — navigation, duplication, suppression
- **Sélection guidée de région** — analyse les chromosomes depuis vos fichiers (BigWig, GFF, BED), propose un sélecteur avec les vraies tailles, avertit en cas d'incompatibilité de noms entre fichiers
- **Gene picker** — sélectionnez un gène depuis les fichiers GFF/GTF et générez automatiquement la région avec marges
- **Parcourir les fichiers** — bouton « Parcourir » (shinyFiles) pour les chemins locaux
- **Thème dark** — interface sombre soignée (bslib Flatly dark)
- **Projet démo** — données d'exemple chr1 chargées en un clic
- **Tests unitaires** — 434 tests couvrant toutes les fonctions core via testthat

---

## Formats de fichiers supportés

| Format | Extension(s) | Track type |
|--------|-------------|------------|
| BEDGraph | `.bedgraph`, `.bg` | Signal continu |
| BigWig | `.bw`, `.bigwig` | Signal (indexé) |
| BED | `.bed` | Intervalles |
| GTF | `.gtf`, `.gff`, `.gff3` | Gènes/transcripts |
| narrowPeak | `.narrowPeak` | Pics ChIP/ATAC |
| BEDPE (liens) | `.bedpe` | Interactions 3D |
| BEDGraph matrix | `.bedgraph` | Matrices (heatmap) |
| HiC | `.h5`, `.hic`, `.cool` | Matrice de contact |
| Domaines | `.bed` | TADs/domaines |
| Epilogos | `.bedgraph` | Etats chromatine |
| Décorations | — | spacer, x_axis, scalebar, lignes |

---

- **Conda** (Miniconda ou Anaconda) installé et dans le `PATH`
  → https://docs.conda.io/en/latest/miniconda.html
- **Git** (pour cloner le dépôt)
- macOS / Linux (testé sur macOS et Ubuntu)
- Connexion internet (pour l'installation initiale des paquets)

> [!NOTE]
> L'application doit **toujours** être lancée depuis l'environnement Conda `rgenometrackui`.
> Ne jamais utiliser le R ou Python système/global.

---

## Installation rapide (recommandée)

```bash
git clone <repo-url> rGenomeTrackUI
cd rGenomeTrackUI
bash scripts/setup_conda.sh
conda activate rgenometrackui
Rscript app.R
```

Le script `setup_conda.sh` :
1. Détecte `mamba` ou `conda`
2. Crée l'environnement depuis `environment.yml`
3. Lance `Rscript install.R` (installe rGenomeTracks via Bioconductor)
4. Vérifie l'environnement avec tous les scripts de check
5. Affiche un résumé `OK / WARNING / ERROR`

---

## Installation manuelle

Si vous préférez contrôler chaque étape :

### 1. Créer l'environnement Conda

```bash
conda env create -f environment.yml
```

> Avec mamba (plus rapide) :
> ```bash
> mamba env create -f environment.yml
> ```

### 2. Activer l'environnement

```bash
conda activate rgenometrackui
```

### 3. Installer les packages R (Bioconductor)

```bash
Rscript install.R
```

Ce script installe notamment `rGenomeTracks` via `BiocManager::install("rGenomeTracks")`.

### 4. Vérifier les dépendances R

```bash
Rscript scripts/check_r_dependencies.R
```

Le rapport est écrit dans `logs/dependency_check_r.txt`.

### 5. Vérifier pyGenomeTracks

```bash
bash scripts/check_pygenometracks.sh
```

### 6. Vérifier l'environnement Conda complet

```bash
bash scripts/check_conda_env.sh
```

---

## Lancer l'application

```bash
# Méthode recommandée — utilise le Rscript conda (pas le R système)
bash scripts/run_app.sh
```

Ou en spécifiant le Rscript conda directement :

```bash
$(conda info --base)/envs/rgenometrackui/bin/Rscript app.R
```

> [!WARNING]
> **Ne pas utiliser** `Rscript app.R` ni `conda run -n rgenometrackui Rscript app.R` sur macOS
> si `/usr/local/bin` précède le conda env dans votre PATH (cas commun sur macOS avec R installé
> via `.pkg`). Dans ce cas, c'est le R système qui est utilisé et `rGenomeTracks` ne sera pas trouvé.

L'application se lance sur `http://localhost:3838` (ou le port configuré dans `config/app_config.yaml`).

---

## Workflow typique

1. **Dashboard** — vérifier que toutes les dépendances sont OK (pastilles vertes)
2. **Projets** — créer un nouveau projet ou ouvrir un existant
3. **Fichiers d'entrée** — importer ou lier les fichiers de données (bedgraph, bigwig, GTF, etc.)
4. **Tracks** — ajouter et configurer les tracks via le Track Builder ; appliquer un template si besoin
5. **Région & Figure** — définir la région génomique (ex : `chr1:1000-5000`) ou charger un BED de régions
6. **Prévisualisation** — inspecter le `tracks.ini` généré, le script R et le script shell
7. **Exécution** — préparer puis lancer le run ; suivre les logs en direct
8. **Résultats** — visualiser la figure PNG, télécharger tous les fichiers
9. **Historique** — retrouver, dupliquer ou re-lancer des runs passés

---

## Projet démo

Cliquez sur **"Charger un projet démo"** depuis le Dashboard pour créer automatiquement un projet avec :

| Fichier | Type | Description |
|---------|------|-------------|
| `mini_signal.bedgraph` | BEDGraph | Signal sur chr1:1000-10000 |
| `mini_genes.gtf` | GTF | 2 gènes fictifs |
| `mini_peaks.narrowPeak` | narrowPeak | 2 pics |
| `mini_links.bedpe` | BEDPE | 2 interactions 3D |
| `mini_regions.bed` | BED | 2 régions cibles |

Les données couvrent `chr1:1000-10000`.

---

## Lancer les tests

```bash
# Avec le Rscript conda (recommandé)
$(conda info --base)/envs/rgenometrackui/bin/Rscript -e "testthat::test_dir('tests/testthat')"
```

Ou avec devtools si le projet est structuré comme un package :

```bash
Rscript -e "devtools::test()"
```

Les fichiers de tests couvrent :
- `test_slug.R` — utils_slug : slugify, validate_slug, sanitize_name
- `test_project_manager.R` — création, chargement, liste de projets
- `test_run_manager.R` — création de runs, métadonnées, liste
- `test_region_validator.R` — validation et parsing de régions UCSC
- `test_file_validator.R` — validation de fichiers, détection de type, registre
- `test_ini_generator.R` — génération du fichier tracks.ini
- `test_r_script_generator.R` — génération du script run_rGenomeTracks.R
- `test_config_generator.R` — intégration : prepare_run_files
- `test_templates.R` — liste, chargement, application de templates
- `test_genome_index.R` — parseur BigWig binaire, inspecteurs GFF/BED, index chromosomique, cache, gene picker

---

### Vérification complète

```bash
conda activate rgenometrackui
bash scripts/check_conda_env.sh
Rscript scripts/check_r_dependencies.R
bash scripts/check_pygenometracks.sh
```

### Vérification rapide individuelle

```bash
# Vérifier pyGenomeTracks
conda run -n rgenometrackui pyGenomeTracks --version

# Vérifier que R de Conda est actif
conda run -n rgenometrackui R --version

# Vérifier rGenomeTracks dans R
conda run -n rgenometrackui Rscript -e "library(rGenomeTracks); cat('OK\n')"

# Vérifier BEDTools
conda run -n rgenometrackui bedtools --version

# Vérifier que R de Shiny est bien celui de Conda
which R   # doit pointer vers ~/.conda/envs/rgenometrackui/bin/R

# Vérifier que pyGenomeTracks est bien celui de Conda
which pyGenomeTracks   # doit pointer vers ~/.conda/envs/rgenometrackui/bin/pyGenomeTracks
```

---

## Mettre à jour l'environnement

### Mettre à jour les packages conda

```bash
conda activate rgenometrackui
conda env update -f environment.yml --prune
```

### Mettre à jour rGenomeTracks (Bioconductor)

```bash
conda activate rgenometrackui
Rscript -e "BiocManager::install('rGenomeTracks')"
```

### Mettre à jour pyGenomeTracks uniquement

```bash
conda activate rgenometrackui
conda update -c bioconda pygenometracks
```

---

## Recréer l'environnement

Pour repartir d'un état propre :

```bash
conda deactivate
conda env remove -n rgenometrackui
conda env create -f environment.yml
conda activate rgenometrackui
Rscript install.R
```

---

## Supprimer l'environnement

```bash
conda deactivate
conda env remove -n rgenometrackui
```

> [!WARNING]
> Cette opération est irréversible. Toutes les données de l'environnement (packages installés) seront supprimées.
> Les projets, runs et fichiers de données dans `projects/` ne sont **pas** affectés.

---

## Diagnostic des erreurs fréquentes

### `conda: command not found`

Conda n'est pas dans le PATH. Ajoutez-le :

```bash
# Miniconda (exemple macOS/Linux)
export PATH="$HOME/miniconda3/bin:$PATH"
# Puis relancez le terminal ou :
source ~/.bashrc  # ou ~/.zshrc
```

---

### `conda env create` échoue avec des conflits de dépendances

Essayez avec mamba (résolution plus robuste) :

```bash
conda install -n base -c conda-forge mamba
mamba env create -f environment.yml
```

Si le problème persiste, vérifiez la version de conda :
```bash
conda update -n base conda
```

---

### `imager` introuvable ou erreur `libX11.dylib` / `libSM.dylib`

`imager` est une dépendance de `rGenomeTracks` qui nécessite X11. Sur macOS sans XQuartz, le binaire CRAN échoue. Solution : installer `r-imager` via conda (embarque ses propres librairies X11) :

```bash
conda install -n rgenometrackui -c conda-forge r-imager -y
```

Puis vérifier que `rGenomeTracks` se charge :

```bash
/Users/clpichot/miniconda3/envs/rgenometrackui/bin/Rscript -e "library(rGenomeTracks); cat('OK\n')"
```

---

### `BiocManager::install("rGenomeTracks")` échoue

Vérifiez que vous êtes dans le bon environnement :

```bash
which R  # doit pointer vers l'env rgenometrackui
conda activate rgenometrackui
Rscript -e "BiocManager::install('rGenomeTracks', ask=FALSE)"
```

Si le problème vient des certificats SSL :
```bash
Rscript -e "options(download.file.method='libcurl'); BiocManager::install('rGenomeTracks')"
```

---

### `rGenomeTracks` chargé mais erreur pyGenomeTracks non trouvé

rGenomeTracks utilise `reticulate` pour appeler pyGenomeTracks. Assurez-vous que :
1. L'env Conda est activé : `conda activate rgenometrackui`
2. pyGenomeTracks est dans l'env : `which pyGenomeTracks`

Si nécessaire, dans R :
```r
library(rGenomeTracks)
install_pyGenomeTracks()  # installe pyGenomeTracks dans l'env conda actif
```

---

### `pyGenomeTracks` échoue avec des erreurs matplotlib / display

En environnement sans affichage (serveur, CI), définissez :

```bash
export MPLBACKEND=Agg
conda activate rgenometrackui
pyGenomeTracks --tracks tracks.ini --region chr1:1000-5000 --outFileName out.png
```

L'application rGenomeTrackUI configure automatiquement `MPLBACKEND=Agg` pour les rendus headless.

---

### L'application ne trouve pas le bon R (R système utilisé à la place)

Vérifiez lequel est utilisé :
```bash
which R
R --version
```

Si ce n'est pas le R de Conda :
```bash
conda activate rgenometrackui
which R  # doit pointer vers .../envs/rgenometrackui/bin/R
```

Assurez-vous que l'activation est effective. Dans certains shells (fish, etc.) :
```bash
conda run -n rgenometrackui Rscript app.R
```

---

### `bedtools: command not found` dans un script généré

Les scripts générés par l'application doivent être lancés depuis l'environnement Conda :

```bash
conda activate rgenometrackui
bash projects/<project>/runs/<run>/scripts/run_pyGenomeTracks.sh
```

Ou modifiez le script pour inclure `conda run -n rgenometrackui` avant chaque commande externe.

---

## Architecture du projet

```
rGenomeTrackUI/
├── app.R                          # Point d'entrée de l'application Shiny
├── environment.yml                # Définition de l'environnement Conda
├── install.R                      # Installation des packages R (Bioconductor)
├── README.md                      # Ce fichier
│
├── config/
│   ├── app_config.yaml            # Configuration générale de l'application
│   ├── dependencies.yaml          # Spécification des dépendances (machine-readable)
│   └── track_schema.yaml          # Schéma des types de tracks et paramètres (Phase 1)
│
├── scripts/
│   ├── setup_conda.sh             # Setup automatisé de l'env Conda
│   ├── check_conda_env.sh         # Vérification de l'env Conda
│   ├── check_r_dependencies.R     # Vérification des packages R
│   └── check_pygenometracks.sh    # Vérification de pyGenomeTracks
│
├── assets/
│   └── banniere.png               # Bannière du projet
│
├── R/
│   ├── modules/                   # Modules Shiny (Phase 3+)
│   └── core/                      # Logique métier (Phase 1+)
│       └── genome_index.R         # Index chromosomique (BigWig/GFF/BED parser + cache)
│
├── templates/                     # Templates de configurations de tracks (Phase 5)
├── example_data/                  # Données d'exemple pour le projet démo (Phase 1)
├── www/                           # Assets statiques (CSS, images)
├── tests/                         # Tests unitaires testthat (Phase 1+)
├── logs/                          # Logs de setup et validation
└── projects/                      # Projets utilisateur (créé à l'usage)
```

---

## Notes sur les dépendances

### rGenomeTracks

- Package Bioconductor (pas disponible sur conda)
- Installe pyGenomeTracks en interne via `reticulate` si besoin
- 15 types de tracks disponibles : `track_bed()`, `track_bigwig()`, `track_gtf()`, `track_bedgraph()`, `track_bedgraph_matrix()`, `track_hlines()`, `track_vlines()`, `track_spacer()`, `track_epilogos()`, `track_narrowPeak()`, `track_domains()`, `track_hic_matrix()`, `track_links()`, `track_scalebar()`, `track_x_axis()`
- Les tracks se combinent avec `+` puis se rendent avec `plot_gtracks()`

### pyGenomeTracks

- Package Python (bioconda)
- Moteur de rendu utilisé par rGenomeTracks en interne
- Également utilisable directement en CLI pour les scripts reproductibles
- Requiert BEDTools >= 2.30 depuis la version 3.5
- Requiert matplotlib `>= 3.1.1, <= 3.6.2`

### Environnement isolé

Toutes les commandes de cette documentation supposent que l'environnement `rgenometrackui` est activé. Ne jamais installer de packages dans l'environnement global ou système.

---

## Phases de développement

| Phase | Statut | Description |
|-------|--------|-------------|
| **Phase 0** | ✅ Complète | Environnement Conda reproductible, scripts de vérification |
| **Phase 1** | ✅ Complète | Architecture, schéma des tracks, project/run manager |
| **Phase 2** | ✅ Complète | Génération de configuration (tracks.ini, script R, script shell) |
| **Phase 3** | ✅ Complète | Interface Shiny — 10 modules (dashboard, projets, fichiers, tracks, région, préview, run, résultats, historique, settings) |
| **Phase 4** | ✅ Complète | Exécution des runs via processx, logs, visualisation des figures |
| **Phase 5** | ✅ Complète | Historique, templates (5 prédéfinis), duplication de runs, sauvegarde de template |
| **Phase 6** | ✅ Complète | Données d'exemple, tests unitaires (9 fichiers), projet démo, documentation complète |
| **v0.4** | 🔄 En cours | Thème dark, sélection guidée de région (index chromosomique), gene picker, parcourir fichiers locaux |

---

## Licence

GPL-3.0 — voir [LICENSE](LICENSE)

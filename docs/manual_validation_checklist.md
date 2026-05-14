# Checklist de validation manuelle — rGenomeTrackUI

> **Date de création :** 2026-05-14
> **Version :** MVP Phase 6

Ce document décrit les deux scénarios de validation manuelle principaux à exécuter après chaque mise à jour significative du code.

---

## Prérequis

```bash
conda activate rgenometrackui
bash scripts/run_app.sh
# Application accessible sur http://localhost:3838
```

---

## Scénario 1 — Projet démo (validation rapide)

Ce scénario vérifie que le flux complet fonctionne sans interaction manuelle de configuration.

### 1.1 Chargement du projet démo

- [ ] Aller sur l'onglet **Dashboard**
- [ ] Cliquer sur **"Charger un projet démo"**
- [ ] Vérifier : aucune erreur rouge dans la console R (stdout)
- [ ] Vérifier : notification de succès affichée
- [ ] Vérifier : navigation automatique vers **Track Builder**

### 1.2 Track Builder

- [ ] 4 tracks sont visibles et activées : Signal (bedgraph), Gènes (GTF), Pics (narrowPeak), Liens (BEDPE)
- [ ] Aucun message d'erreur

### 1.3 Prévisualisation (onglet Preview)

- [ ] Aller sur l'onglet **Prévisualisation**
- [ ] Onglet **tracks.ini** : contenu INI visible (pas d'erreur `/preview/scripts`)
- [ ] Onglet **Script R** : contenu du script R visible (chemin de sortie = dossier projet, pas `/preview`)
- [ ] Onglet **Script Shell** : contenu visible
- [ ] Boutons de téléchargement fonctionnels (téléchargent un fichier)

### 1.4 Région & Figure

- [ ] Aller sur l'onglet **Région & Figure**
- [ ] La région `chr1:1000-10000` est pré-remplie
- [ ] Le renderer est sélectionné (`pyGenomeTracks` par défaut)

### 1.5 Run (exécution)

- [ ] Aller sur l'onglet **Exécuter**
- [ ] Checklist pré-run : 6 items visibles
  - [x] Projet actif ✓
  - [x] Région(s) ✓
  - [x] Track(s) activée(s) ✓
  - [x] Renderer configuré ✓
  - [x] **Renderer disponible** ✓ (nouveau — pyGenomeTracks détecté)
  - [x] **Fichiers sources** ✓ (nouveau — tous les fichiers présents)
- [ ] Cliquer **"Préparer le run"** → notification succès
- [ ] Cliquer **"Lancer l'analyse"** → statut passe en "En cours…" puis "Terminé"
- [ ] Bouton **"Voir les résultats"** apparaît après succès
- [ ] `run_debug.log` présent dans `<run_path>/logs/run_debug.log`
- [ ] Ligne ajoutée dans `logs/run_debug.log` global

### 1.6 Résultats

- [ ] Cliquer **"Voir les résultats"** → navigation automatique vers l'onglet Résultats
- [ ] Figure PNG affichée dans le panneau principal
- [ ] Bouton **"Figure (PNG/PDF)"** actif (grisé si pas de PNG)
- [ ] Bouton **"tracks.ini"** actif
- [ ] Bouton **"Script Shell"** actif
- [ ] Bouton **"Archive ZIP complète"** actif

### 1.7 Historique

- [ ] Aller sur l'onglet **Historique**
- [ ] Le run démo apparaît dans la table
- [ ] Sélectionner le run → boutons Voir/Dupliquer/Relancer/Supprimer activés
- [ ] **"Voir"** → navigate vers Résultats avec le run sélectionné
- [ ] **"Dupliquer config"** → nouveau run visible dans la table (statut "duplicated")
- [ ] **"Relancer"** → run relancé, notification "Consultez 'Résultats'" affichée, `last_run_path` mis à jour
- [ ] **"Supprimer"** → modal de confirmation affiché, run supprimé après confirmation

---

## Scénario 2 — Projet utilisateur minimal

Ce scénario valide la création et l'utilisation d'un projet utilisateur entier.

### 2.1 Création d'un projet

- [ ] Aller sur l'onglet **Projets**
- [ ] Cliquer **"Nouveau projet"**
- [ ] Saisir un nom (ex : `MonTest`)
- [ ] Confirmer → projet créé, rechargé automatiquement
- [ ] Projet visible dans la liste

### 2.2 Import d'un fichier

- [ ] Aller sur l'onglet **Fichiers d'entrée**
- [ ] Bouton d'ajout actif seulement si un projet est actif
- [ ] Uploader un fichier bedgraph (ex : `example_data/mini_signal.bedgraph`)
- [ ] Fichier visible dans la table du registre avec le bon type et le bon nom
- [ ] Uploader un fichier > 100 MB → vérifier que la limite 2 Go est bien appliquée (pas de blocage à 5 MB)

### 2.3 Configuration des tracks

- [ ] Aller sur **Track Builder**
- [ ] Ajouter une track bedgraph → l'assigner au fichier importé
- [ ] Ajouter une track x_axis
- [ ] Réordonner par glisser-déposer
- [ ] Appliquer un template → modal "Remplacer tout / Ajouter à la suite"

### 2.4 Région & Figure

- [ ] Saisir une région (`chr1:1000-10000`) ou charger un BED
- [ ] Sélectionner un renderer
- [ ] Sauvegarder la configuration

### 2.5 Run

- [ ] Checklist : vérifier que le 5e item (renderer disponible) est vert
- [ ] Vérifier que le 6e item (fichiers sources) est vert
- [ ] Préparer + lancer
- [ ] En cas d'échec : consulter les logs stdout/stderr dans l'interface

### 2.6 Résultats & téléchargements

- [ ] Figure affichée ou message "Aucune figure PNG" si exécution échoue
- [ ] Tous les boutons download reflètent l'état réel des fichiers
- [ ] Télécharger l'archive ZIP → vérifier que tous les fichiers y sont

---

## Points de régression à vérifier systématiquement

| ID  | Test                                                             | Attendu                                      |
|-----|------------------------------------------------------------------|----------------------------------------------|
| R1  | `shiny::small()` → `shiny::tags$small()`                        | Pas d'erreur "not an exported object"        |
| R2  | Preview sans projet actif                                        | Message "Configurez un projet" (pas crash)   |
| R3  | Preview sans région                                              | Message "Définissez au moins une région"     |
| R4  | Run sans renderer                                               | Checklist rouge, lancement bloqué            |
| R5  | Run avec fichier source supprimé                                 | Checklist rouge "Introuvable(s)"             |
| R6  | Renderer pyGenomeTracks absent du PATH                           | Checklist rouge "introuvable — vérifiez Conda" |
| R7  | Boutons download avant tout run                                  | Tous grisés                                  |
| R8  | Résultats après run réussi                                       | dl_figure actif, figure visible              |
| R9  | Historique: Supprimer → run absent de la liste                   | OK                                           |
| R10 | Historique: Relancer → app_state$last_run_path mis à jour        | Résultats accessibles depuis le module       |

---

## Commande de test automatisé (avant validation manuelle)

```bash
$(conda info --base)/envs/rgenometrackui/bin/Rscript \
  -e "testthat::test_dir('tests/testthat')"
```

Résultat attendu : `FAIL 0 | WARN ≤ 50 | PASS ≥ 120`

# Dépannage

## Problèmes de lancement

### pyGenomeTracks introuvable

**Symptôme** : Le bouton "Lancer" est grisé ou l'erreur `pygenometracks: command not found` apparaît.

**Solutions** :
1. Vérifiez que l'environnement conda est actif dans app.R :
   ```bash
   source ~/.bashrc && conda activate rgenometrackui
   ```
2. Lancez via le script fourni : `bash scripts/run_app.sh` (il exporte les bons PATH)
3. Vérifiez le chemin dans le tableau de bord → section "Dépendances"

---

### L'application ne démarre pas

```bash
# Vérifier l'environnement
conda activate rgenometrackui
Rscript -e "library(shiny); runApp('app.R', port=3838)"
```

Consultez les logs dans `logs/`.

---

## Problèmes de fichiers

### Fichier rejeté à l'import

- Vérifiez que l'extension correspond au format attendu (ex : `.bed`, `.bw`)
- Les fichiers BigWig doivent être des fichiers binaires valides — ne les ouvrez pas dans un éditeur texte
- Taille max 500 Mb par défaut ; modifiez `options(shiny.maxRequestSize = ...)` dans `app.R` si besoin

### Noms de chromosomes incompatibles

Erreur pyGenomeTracks : `chromosome not found` ou figure vide.

Les noms de chromosomes doivent être **cohérents** entre tous les fichiers :
- Soit `chr1, chr2, ...` (UCSC)
- Soit `1, 2, ...` (Ensembl)

Conversion rapide :
```bash
# Ajouter "chr" si absent
awk 'BEGIN{OFS="\t"} {$1="chr"$1; print}' fichier.bed > fichier_chr.bed
```

---

## Problèmes de configuration

### La région ne s'affiche pas correctement

Format attendu : `chr:start-end` (ex : `chr1:1000000-2000000`).

- Pas d'espaces dans la chaîne
- `start` doit être inférieur à `end`
- Les coordonnées doivent correspondre au génome de référence utilisé

### Track vide dans la figure

Causes fréquentes :
1. La région ne chevauche pas les données du fichier
2. Le mauvais fichier est associé au track
3. Le BedGraph contient des intervalles vides dans cette région

---

## Problèmes de rendu

### Erreur pyGenomeTracks lors du rendu

Le stderr est affiché dans l'onglet "Erreurs" après exécution.

Messages courants :
- `[E] Error reading file`: format incorrect ou colonnes manquantes
- `[W] No valid intervals`: région sans données — vérifiez chr et coordonnées
- `Permission denied`: vérifiez les droits sur le dossier de sortie

### La figure est trop grande ou illisible

Ajustez la hauteur des tracks dans le constructeur de tracks (paramètre `height`). Réduisez la région à visualiser.

---

## Problèmes de performance

### Import très lent pour les BigWig

Les fichiers BigWig sont indexés et ne nécessitent pas d'être chargés entièrement — l'affichage lent est souvent lié à la taille de la région sélectionnée.

### L'application est lente avec beaucoup de tracks

Réduisez le nombre de tracks actifs ou utilisez des fichiers BigWig plutôt que BedGraph.

---

## Réinitialisation

Pour repartir de zéro :
```bash
# Supprimer le projet courant (les fichiers sources ne sont pas supprimés)
# Rechargez simplement l'application dans le navigateur (F5)
```

Les projets et leur configuration sont stockés dans `projects/`.

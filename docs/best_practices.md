# Bonnes pratiques

## Organisation des fichiers

- Regroupez tous vos fichiers d'entrée dans un dossier dédié au projet avant d'importer
- Nommez vos fichiers de façon explicite : `chipseq_h3k27ac_rep1.bw` plutôt que `data1.bw`
- Évitez les espaces et caractères spéciaux dans les noms de fichiers — préférez underscores `_`

---

## Cohérence des chromosomes

Assurez-vous que tous les fichiers utilisent la **même convention de nommage** des chromosomes :

| Convention | Exemple |
|---|---|
| UCSC | `chr1`, `chrX`, `chrM` |
| Ensembl | `1`, `X`, `MT` |

Ne mélangez pas les deux dans un même projet.

---

## Choix du format signal

| Situation | Format recommandé |
|---|---|
| Fichier < 50 Mb, éditable | BedGraph |
| Fichier > 50 Mb ou partage | BigWig |
| Pics discrets | narrowPeak |

---

## Régions à visualiser

- Commencez par des régions larges (≥ 50 kb) pour valider que les données sont présentes
- Précisez ensuite en zoomant sur la région d'intérêt
- Pour les loci complexes, créez un fichier BED de régions pour automatiser le rendu multi-régions

---

## Ordre des tracks

Un ordre logique facilite la lecture :
1. Track signal principal (ChIP, ATAC...)
2. Appels de pics correspondants
3. Gènes / annotation
4. Interactions (Hi-C, BEDPE)
5. Marqueurs (lignes verticales, régions)

---

## Reproductibilité

- Téléchargez le fichier `.ini` généré et conservez-le avec vos données
- Le script R (`.R`) et shell (`.sh`) permettent de régénérer la figure exactement
- Versionnez vos configurations avec Git si vous travaillez en équipe

---

## Performance

- Fermez les onglets "Preview fichier" inutiles — ils lisent les fichiers en temps réel
- Pour les projets avec > 10 tracks, limitez la région à < 1 Mb pour un rendu rapide
- Utilisez toujours BigWig pour les signaux de séquençage profond

---

## Validation avant rendu

Utilisez le panneau **Aperçu & Validation** pour vérifier :
- ✔ Projet défini
- ✔ Au moins une région configurée
- ✔ Au moins un track actif
- ✔ Tous les fichiers de tracks accessibles
- ✔ pyGenomeTracks disponible

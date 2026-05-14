# Cas d'usage

## 1. Visualisation ChIP-seq (H3K27ac)

**Objectif** : Comparer le signal histone entre deux conditions sur un locus cible.

**Fichiers nécessaires** :
- `condition_A_h3k27ac.bw` — BigWig signal
- `condition_B_h3k27ac.bw` — BigWig signal
- `condition_A_peaks.narrowPeak` — Pics MACS2
- `genes.gtf` — Annotation Ensembl

**Configuration** :
1. Créez un projet `chipseq_h3k27ac`
2. Importez les 4 fichiers
3. Ajoutez 2 tracks BigWig (un par condition), 1 track narrowPeak, 1 track GTF
4. Définissez la région : `chr1:1000000-1200000`
5. Lancez le rendu

---

## 2. Carte Hi-C + domaines TAD

**Objectif** : Visualiser la matrice de contact Hi-C avec les TADs et gènes annotés.

**Fichiers nécessaires** :
- `hic_matrix.cool` — Matrice Hi-C (format cool, si supporté)
- `tad_domains.bed` — Domaines TAD
- `genes.gtf` — Annotation

**Configuration** :
1. Importez les fichiers BED domaines et GTF
2. Ajoutez track Domains (TAD) + track GTF
3. Ajoutez des lignes verticales aux frontières de TAD connues
4. Région large recommandée : `chr1:10000000-15000000`

---

## 3. RNA-seq : signal et épissage

**Objectif** : Visualiser la couverture RNA-seq et les variants d'épissage.

**Fichiers nécessaires** :
- `rnaseq_coverage.bw` — Couverture BigWig
- `transcriptome.gtf` — Annotation avec isoformes

**Configuration** :
1. Track BigWig pour la couverture
2. Track GTF avec `prefered_name = transcript_id` pour distinguer les isoformes
3. Région : locus du gène d'intérêt (récupérez les coordonnées depuis Ensembl)

---

## 4. Multi-régions automatisé

**Objectif** : Générer une figure par locus à partir d'une liste de régions.

**Fichiers nécessaires** :
- Données tracks habituelles
- `loci_of_interest.bed` — Liste de régions (une par ligne)

**Configuration** :
1. Importez le fichier BED de régions dans l'onglet "Régions & Settings"
2. Cliquez "Charger depuis BED" — chaque ligne devient une région
3. Le moteur génère une figure par région automatiquement
4. Toutes les sorties sont dans le dossier de projet

---

## 5. Variants / SNPs ponctuels

**Objectif** : Marquer des positions génomiques spécifiques (SNPs, CTCF sites...) sur une figure.

**Fichiers nécessaires** :
- Tracks standards (signal, gènes)
- `snp_positions.tsv` — Fichier 3 colonnes `chr position label`

**Configuration** :
1. Importez le fichier TSV
2. Ajoutez un track "Lignes verticales" associé au fichier TSV
3. Les positions seront matérialisées par des lignes verticales annotées
4. Combinez avec un track signal pour valider l'enrichissement aux SNPs

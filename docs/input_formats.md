# Formats de fichiers supportés

## Vue d'ensemble

| Format | Extensions | Binaire | Types de track | Description |
|---|---|---|---|---|
| BED | `.bed` | Non | bed, domains, regions | Intervalles génomiques |
| BedGraph | `.bedgraph`, `.bg` | Non | bedgraph | Signal continu (texte) |
| BigWig | `.bw`, `.bigwig` | Oui | bigwig | Signal continu (binaire indexé) |
| GTF/GFF | `.gtf`, `.gff`, `.gff3` | Non | gtf | Annotation de gènes |
| narrowPeak | `.narrowPeak` | Non | narrowPeak | Appels de pics (ChIP-seq) |
| BEDPE | `.bedpe`, `.links` | Non | links | Paires d'intervalles (interactions) |
| Domains | `.bed`, `.domains` | Non | domains | Domaines génomiques (TADs) |
| Regions BED | `.bed` | Non | — | Liste de régions à visualiser |
| Lignes verticales | `.tsv` | Non | vlines | Marqueurs de positions |
| Lignes horizontales | `.tsv` | Non | hlines | Seuils de signal |

---

## BED

**Extensions** : `.bed`  
**Colonnes requises** : `chrom`, `start`, `end`

```
chr1	1000	2000	feature_A	500	+
chr1	3500	4200	feature_B	300	-
```

**Notes** :
- Coordonnées en base 0 (half-open) : `start` inclus, `end` exclu
- Les colonnes 4 (name), 5 (score), 6 (strand) sont optionnelles
- Les noms de chromosomes doivent être cohérents avec les autres fichiers

---

## BedGraph

**Extensions** : `.bedgraph`, `.bg`  
**Colonnes requises** : `chrom`, `start`, `end`, `value`

```
chr1	1000	1100	2.5
chr1	1100	1200	3.1
```

**Notes** :
- Intervalles non chevauchants sur le même chromosome
- La valeur peut être négative (ex : log2-fold-change)
- Pour de gros fichiers (> 100 Mb), préférez BigWig

---

## BigWig

**Extensions** : `.bw`, `.bigwig`  
**Format binaire** — ne peut pas être édité manuellement.

**Conversion depuis BAM** (deeptools) :
```bash
bamCoverage -b sample.bam -o sample.bw --normalizeUsing RPKM
```

---

## GTF / GFF

**Extensions** : `.gtf`, `.gff`, `.gff3`  
**Colonnes** : 9 colonnes séparées par tabulations (format standard)

```
chr1	Ensembl	gene	1000	5000	.	+	.	gene_id "GENE1"; gene_name "GENE1";
chr1	Ensembl	transcript	1000	5000	.	+	.	gene_id "GENE1"; transcript_id "TX1";
chr1	Ensembl	exon	1000	1300	.	+	.	gene_id "GENE1"; transcript_id "TX1";
```

**Notes** :
- Coordonnées en base 1 (contrairement à BED)
- Requis : features `gene`, `transcript`, `exon`
- Attributs `gene_id` et `transcript_id` nécessaires pour le rendu

---

## narrowPeak

**Extensions** : `.narrowPeak`  
Produit par MACS2 ou d'autres peak callers.

```
chr1	1000	1200	peak_1	100	.	10.5	3.2	1.5	100
```

Colonnes : `chrom start end name score strand signalValue pValue qValue peak`

---

## BEDPE / Links

**Extensions** : `.bedpe`, `.links`  
6 colonnes minimum : `chrom1 start1 end1 chrom2 start2 end2`

```
chr1	1000	1500	chr1	5000	5500	link_1	10
```

Utilisé pour les interactions chromatiniennes (Hi-C, ChIA-PET, PLAC-seq).

---

## Regions BED

Format BED standard utilisé pour les **régions** à visualiser.  
Chaque ligne génère une figure distincte.

```
chr1	1000	20000	region_promoteur
chr2	5000	50000	region_enhancer
```

---

## Lignes verticales

Format TSV 2-3 colonnes :

```
chr1	2500	TSS_GENE1
chr1	7000	mutation_rs12345
```

---

## Lignes horizontales

Format TSV 1-2 colonnes :

```
0	baseline
5	seuil
```

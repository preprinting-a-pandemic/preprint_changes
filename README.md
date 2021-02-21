This repository contains code and datasets for the study "Preprints in motion: tracking changes between posting and publication" available from [bioRxv](https://www.biorxiv.org/content/10.1101/2021.02.20.432090v1).

This README contains an overview of the different files and datasets contained in this repository. The full methodology is documented in the above linked preprint.

# Files:

- `compute_abstract_changeratio.ipynb`: This notebook computes the changeratio between two abstract versions (input file: `all_pairs.tsv` in the `data` folder), using either the Python library `difflib` or the output of Microsoft Word track changes (available [here](data/msword_compared_abstracts.txt)). It produces a change-ratio score for each abstract. We used this in the preliminary phase of our study to determine which track change algorithm was more suited to our work.

- `extract_annotations.ipynb`: given an [input file](outputs/reconciled_annotations.txt) of reconciled annotations derived from Microsoft Word, this notebook structures them in a [.tsv](outputs/extracted_reconciled_annotations.tsv) identifying for each annotation the annotator, the label, related comments etc.

- `structure_annotations_and_scores.ipynb`: Finally, this notebook uses the tab-sepatated file as an input for generating a [complete overview](outputs/final_reconciled_annotations.csv) of each annotation. 

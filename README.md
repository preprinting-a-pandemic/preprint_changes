This repository contains code and datasets for the study "..." [currently available from bioRxiv: ...].

This README contains an overview of the different files and datasets contained in this repository. The full methodology is documented in the above linked preprint.

## Scripts:

`compute_abstract_changeratio.ipynb`: This script computes the changeratio between two abstract versions (input file: `all_pairs.tsv` in the `data` folder), using either the Python library `difflib` or the output of Microsoft Word track changes (available in `data/msword_compared_abstracts.txt`). It produces a change-ratio score for each abstract. We used this in the preliminary phase of our study to determine which track change algorithm was more suited for our work.

`extract_annotations.ipynb`: given an input file of reconciled annotations (`outputs/reconciled_annotations.txt` - to know more about this procedure, see the paper) derived from Microsoft Word, we structure them in a .tsv identifying for each annotation the annotator, the label, related comments etc (`outputs/extracted_reconciled_annotations.tsv`).

`structure_annotations_and_scores.ipynb`: Finally, we use this tab-sepatated file as an input for generating a [complete overview](outputs/final_reconciled_annotations.csv) of each annotation. 

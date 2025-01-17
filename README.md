### This repository holds the source code for a custom HPO term annotation system. 

To use the system, run the `app.R` file in [RStudio](https://posit.co/download/rstudio-desktop/). You will need to download the [hpo.obo file](https://hpo.jax.org/data/ontology) before the first use. 

To annotate other data, you will need to recreate th PEStrings data structure:
```r
glimpse(PEStrings)
#Rows: 1,497
#Columns: 8
#$ StringID             <chr> "5fc446c80c3a7ae7be11a6ee1137ea3b", "0e487a51b4d9c50b0376bb06206332c6", "54566ce4c5188cb088e7702518feea3e", …
#$ System               <chr> "ABDOMEN", "ABDOMEN", "ABDOMEN", …
#$ OriginalText         <chr> "+umbilical hernia", "Abdominal fullness without palpable liver edge or spleen tip. No fluid wave or shifting", "distended abdomen", …
#$ Text                 <chr> "ABDOMEN: +umbilical hernia", "ABDOMEN: Abdominal fullness without palpable liver edge or spleen tip. No fluid wave or shifting", "ABDOMEN: distended abdomen", …
#$ STUDY_ID             <list> [NA], [NA], [NA], …
#$ SuggestedTerms       <list> ["HP:0001537", "HP:0100790"], ["HP:0003270", "HP:0002240"], ["HP:0003270"], …
#$ Terms                <list> [NA], [NA], [NA], …
#$ ProcessedSuggestions <list> [NA], [NA], [NA], …
```

The annotated data can be retreived from the `$Terms` column. 

#### Unfortunately, we are unable to provide any support for this system.

For more information, see our pre-print here: [PhenoID, a language model normalizer of physical examinations from genetics clinical notes](https://www.medrxiv.org/content/10.1101/2023.10.16.23296894v2)

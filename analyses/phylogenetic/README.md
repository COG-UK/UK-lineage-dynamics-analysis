# Phylogenetic Analysis
# 
```
phylogenetic
├── README.md
├── results
└── xml
    ├── dta 
    │   ├── A-DTA-20200818.xml
    │   ├── B-DTA-20200818.xml
    │   ├── B.1.1-DTA-20200818.xml
    │   ├── B.1.X-DTA-20200818.xml
    │   └── B.1.pruned-DTA-20200818.xml
    ├── preliminary_analysis.xml
    └── timetrees
        ├── A.fixedRootPrior.skygrid-20200720.xml
        ├── B.1.1.fixedRootPrior.skygrid-20200720.xml
        ├── B.1.X.fixedRootPrior.skygrid-20200720.xml
        ├── B.1.pruned.fixedRootPrior.skygrid-20200720.xml
        └── B.fixedRootPrior.skygrid-20200720.xml
```
## Overview
The ids for sequences  used in these analyses can be found in `data/phylogenetic/metadata.csv` with the appropriate acknowledgements in `data/phylogenetic/GISAID_acknowledgements.csv`. The sequences themselves can be downloaded from COG-UK and GISAID. The phylogenetic trees that are used as data in the 'timetree' analysis can be found in `data/phylogenetic/dataTrees` and were estimated using the COG-UK phylogenetic pipeline - grapevine commit 11bff38 (https://github.com/COG-UK/grapevine). 


### BEAST analysis
The analysis used in the preliminary clock-rate analysis as well as those which estimated timetrees and reconstructed discrete traits (dta) can be reproduced with the provided xmls and the developmental BEAST branch `approximateTreeLikelihood@9eac53`. 
*TODO push update commit up one for logspace DTA*

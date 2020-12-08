# Phylogenetic Analysis

## Overview

This directory contains the scripts, XML files and RMarkdown notebooks needed to: 

- Estimate time-calibrated trees in BEAST
- Estimate state transitions between UK and non-UK branches in the trees in BEAST (DTA)
- Extract UK transmission lineages
- Create related figures and tables

Minimal results are also included. Some of the scripts may need some adjustment depending on the local setup.

The structure of the phylogenetic analysis directory is shown below:

```
phylogenetic/
├── reports
├── results
│   ├── combined_beast_dta
│   └── xml
│       ├── dta
│       │   ├── A-DTA-20200818.xml
│       │   ├── B-DTA-20200818.xml
│       │   ├── B.1.1-DTA-20200818.xml
│       │   ├── B.1.X-DTA-20200818.xml
│       │   └── B.1.pruned-DTA-20200818.xml
│       ├── timetrees
│       │   ├── A.fixedRootPrior.skygrid-20200720.xml
│       │   ├── B.1.1.fixedRootPrior.skygrid-20200720.xml
│       │   ├── B.1.X.fixedRootPrior.skygrid-20200720.xml
│       │   ├── B.1.pruned.fixedRootPrior.skygrid-20200720.xml
│       │   └── B.fixedRootPrior.skygrid-20200720.xml
│       └── preliminary_analysis.xml
├── scripts
└── README.md

```


## Input data
Sequence ids for genomes used in these analyses can be found in [`data/phylogenetic/metadata.csv`](../../data/phylogenetic/metadata.csv) with the appropriate acknowledgements in [`data/phylogenetic/GISAID_acknowledgements.csv`](../../data/phylogenetic/GISAID_acknowledgements.csv). The sequences themselves can be downloaded from [COG-UK](https://www.cogconsortium.uk/data/) and [GISAID](http://www.gisaid.org). 

The phylogenetic trees that are used as data in the 'timetree' analysis can be found in [`data/phylogenetic/`](../../data/phylogenetic/) and were estimated using the COG-UK phylogenetic pipeline - grapevine commit 11bff38 ([https://github.com/COG-UK/grapevine]()). 


## BEAST analysis

1. **Preliminary analysis:** Run [`preliminary_analysis.xml`](results/xml/preliminary_analysis.xml)
2. **Time trees:** Run XML files in [`timetrees/`](results/xml/timetrees/)
3. **DTA:** Run XML files in [`dta`](results/xml/dta/)

XML files should be run using the developmental [BEAST](https://github.com/beast-dev/beast-mcmc) branch `approximateTreeLikelihood` (commit [c8cc55d4](https://github.com/beast-dev/beast-mcmc/tree/c8cc55d4fe9d8c6c802c2cbb71936a2c4ccc381e)). 


## Reports

Run the RMarkDown notebooks below to extract UK transmission lineages and reproduce figures and tables. 

1. [`extractLineages.Rmd`](reports/extractLineages.md): Extract TMRCAs and other summary statistics of the UK transmission lineages across all posterior trees from the BEAST analyses.
2. [`extractLineagesMCC.Rmd`](reports/extractLineagesMCC.md): Extract TMRCAs and other summary statistics of the UK transmission lineages from the BEAST MCC trees.
3. [`lineageSummary.Rmd`](reports/lineageSummary.pdf): Plot summary statistics and figures of the UK transmission lineages extracted from the BEAST DTA analyses.
4. [`importationSummary.Rmd`](reports/importationSummary.pdf): Plot figures about the dataset, infections in different countries and inbound travellers. Also apply the importation lag model to the UK transmission lineage TMRCAs and plot figures with lineage importations. 
5. [`lineageSimilarity.Rmd`](reports/lineageSimilarity.md): Compare similarity of lineage assignments across posterior trees and the MCC tree using the Jaccard index. 
6. [`lineageBreakdown.Rmd`](reports/lineageBreakdown.pdf): Plot breakdowns of UK transmission lineages over time (using only the assignment on the MCC trees).


## Output

- **DTA output:** Log files, MCC trees and subsampled posterior tree files **(thinned to only 200 posterior trees)** can be found in [`results/xml/dta/output/`](results/xml/dta/output/).
- **UK transmission lineages:** The extracted UK transmission lineages can be found in [`results/combined_beast_data/`](results/combined_beast_dta/). Only the summary `.csv` files are provided on this repository. 
	- `clusters_DTA.csv`: Summary statistics of UK transmission lineages across all **2000** posterior trees.
	- `clusterSamples_DTA.csv`: Assignment of UK genomes to transmission lineages across all **2000** posterior trees.
	- `clusters_DTA_MCC_0.5`: Summary statistics of UK transmission lineages on the MCC trees using a posterior probability threshold of 0.5. _**Note that the assignment of genomes to transmission lineages is based on the MCC (summary) tree(s), and therefore does  not include the statistical uncertainty that is present in the posterior set of trees.**_
	- `clusters_DTA_MCC_0.5_shifted`: As above, but including importation lag.
	- `clusterSamples_DTA_MCC_0.5.csv`: Assignment of UK genomes to transmission lineages on the MCC trees using a posterior probability threshold of 0.5. _**Note that the assignment of genomes to transmission lineages is based on the MCC (summary) tree(s), and therefore does  not include the statistical uncertainty that is present in the posterior set of trees.**_
- **Figures:** Output figures are stored in [`results/combined_beast_data/figures/`](results/combined_beast_dta/figures/).



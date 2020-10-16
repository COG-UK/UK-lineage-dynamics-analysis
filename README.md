# SARS-CoV-2 Introductions into the United Kingdom

This repository contains the data and code used to generate the results
presented in *PAPER DOI*.

## Usage and structure

The structure of this repository is shown in the following tree output.

```
uk-intros-analyses/
├── analyses
│   ├── epidemiological
│   │   ├── README.md
│   │   └── results
│   │       └── figure.svg
│   ├── phylogenetic
│   │   ├── README.md
│   │   ├── results
│   │   │   └── figure.svg
│   │   └── xml
│   │       ├── dta
│   │       │   ├── A-DTA-20200818.xml
│   │       │   ├── B.1.1-DTA-20200818.xml
│   │       │   ├── B.1.pruned-DTA-20200818.xml
│   │       │   ├── B.1.X-DTA-20200818.xml
│   │       │   └── B-DTA-20200818.xml
│   │       ├── preliminary_analysis.xml
│   │       └── timetrees
│   │           ├── A.fixedRootPrior.skygrid-20200720.xml
│   │           ├── B.1.1.fixedRootPrior.skygrid-20200720.xml
│   │           ├── B.1.pruned.fixedRootPrior.skygrid-20200720.xml
│   │           ├── B.1.X.fixedRootPrior.skygrid-20200720.xml
│   │           └── B.fixedRootPrior.skygrid-20200720.xml
│   └── spatial
│       ├── Preparing_adm2.ipynb
│       ├── README.md
│       └── results
│           └── figure.svg
├── data
│   ├── phylogenetic
│   │   ├── A.tree
│   │   ├── B.1.1.tree
│   │   ├── B.1.pruned.tree
│   │   ├── B.1.tree
│   │   ├── B.1.X.tree
│   │   ├── B.tree
│   │   ├── GISAID_acknowledgements.csv
│   │   └── metadata.csv
│   ├── spatial
│   │   ├── adm2_cleaning.csv
│   │   ├── adm2_region_conversion_epi_data.txt
│   │   ├── All_locs_raw_data_over_time.tsv
│   │   ├── epi_data_conversion_adm2_region.txt
│   │   ├── final_list_adm2_used.tsv
│   │   └── MCC_lineage_summaries.csv
│   ├── spatial_data
│   │   └── map_files
│   │       ├── channel_islands.json
│   │       ├── NI_counties.geojson
│   │       └── UK.json
│   └── xxx.csv
├── LICENSE
└── README.md
```

- The `analyses` directory contains directories containing the details of each
  type of analysis.
  + The `epidemiological` directory contains a `README.md` file describing how
    to run the included scripts to carry out the epidemiological analysis. The
    output goes into a `results` directory.
  + The `phylogenetic` and `spatial` directories follow the same layout as the
    `epidemiological` directory.
- The `data` directory contains all of the raw data for the analyses which we
  are able to make public.


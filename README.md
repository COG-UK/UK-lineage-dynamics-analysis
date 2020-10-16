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
│   │   ├── script1.R
│   │   ├── script2.R
│   │   └── results
│   │       └── figure.svg
│   ├── phylogenetic
│   │   ├── README.md
│   │   └── results
│   │       └── figure.svg
│   └── spatial
│       ├── README.md
│       └── results
│           └── figure.svg
├── data
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


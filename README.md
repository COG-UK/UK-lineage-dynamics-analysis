# Establishment & lineage dynamics of the SARS-CoV-2 epidemic in the UK

**Louis du Plessis**, **John T. McCrone**, **Alexander E. Zarebski**, **Verity Hill**,  Christopher Ruis, Bernardo Gutierrez, Jayna Raghwani, Jordan Ashworth, Rachel Colquhoun, Thomas R. Connor, Nuno R. Faria, Ben Jackson, Nicholas J. Loman, Áine O’Toole, Samuel M. Nicholls, Kris V. Parag, Emily Scher, Tetyana I. Vasylyeva, Erik M. Volz, Alexander Watts, Isaac I. Bogoch, Kamran Khan, the COVID-19 Genomics UK (COG-UK) Consortium, David M. Aanensen, **Moritz U. G. Kraemer**, **Andrew Rambaut**, **Oliver G. Pybus**

---

This repository contains the data and code used to generate the results
presented in *PAPER DOI*. Some of the scripts may need some adjustment depending on the local setup.

Note that because of the GISAID [terms of use](https://www.gisaid.org/registration/terms-of-use/) genomic sequences cannot be shared in this repository. Instead, we make the GISAID accessions available and provide a table of acknowledgements. Note also that we cannot make administrative level two (adm2) metadata for genomic sequences available. All genomic sequences produced by COG-UK are available [here](https://www.cogconsortium.uk/data/).


## Abstract

_The UK’s COVID-19 epidemic during early 2020 was one of world’s largest and unusually well represented by virus genomic sampling. Here we reveal the fine-scale genetic lineage structure of this epidemic through analysis of 50,887 SARS-CoV-2 genomes, including 26,181 from the UK sampled throughout the country’s first wave of infection. Using large-scale phylogenetic analyses, combined with epidemiological and travel data, we quantify the size, spatio-temporal origins and persistence of genetically-distinct UK transmission lineages. Rapid fluctuations in virus importation rates resulted in >1000 lineages; those introduced prior to national lockdown were larger and more dispersed. Lineage importation and regional lineage diversity declined after lockdown, whilst lineage elimination was size-dependent. We discuss the implications of our genetic perspective on transmission dynamics for COVID-19 epidemiology and control._


## Repository usage and structure

The structure of this repository is shown below:

```
uk-intros-analyses/
├── analyses
│   ├── epidemiological
│   ├── phylogenetic
│   ├── spatial
│   └── README.md
├── data
│   ├── epidemiological
│   ├── phylogenetic
│   ├── spatial
│   └── README.md
├── LICENSE.md
├── LICENSE.gpl.md
└── README.md
```

### Input data

All input data that we are able to share publicly are stored in the [`data`](data/) directory.

### Analyses

The [`analyses`](analyses/) directory contains sub-directories containing the details of each type of analysis:

- **Epidemiological analyses:** The `epidemiological` directory contains a [`README.org`](analyses/epidemiological/README.org) file describing how
    to run the included scripts to carry out the epidemiological analysis. The
    output goes into the [`results`](analyses/epidemiological/results/) directory.
- **Phylogenetic analyses:** The `phylogenetic` directory contains a [`README.md`](analyses/phylogenetic/README.md) file describing how to run the analyses. Minimal output is included in the [`results`](analyses/phylogenetic/results/) directory.
- **Spatial analyses:** The `spatial` directory constains a [`README.md`](analyses/spatial/README.md) file describing how to process adm2 regions and output files stored in the [`results`](analyses/spatial/results/) directory.



## License

Except where otherwise noted the content of this project is licensed under the [Creative Commons Attribution 4.0 International License](https://creativecommons.org/licenses/by/4.0/), and all source code (unless otherwise noted) is licensed under the [GNU General Public License v3.0](https://choosealicense.com/licenses/gpl-3.0/).

---
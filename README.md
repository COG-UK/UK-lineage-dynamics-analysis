# Establishment & lineage dynamics of the SARS-CoV-2 epidemic in the UK

**Louis du Plessis**, **John T. McCrone**, **Alexander E. Zarebski**, **Verity Hill**,  Christopher Ruis, Bernardo Gutierrez, Jayna Raghwani, Jordan Ashworth, Rachel Colquhoun, Thomas R. Connor, Nuno R. Faria, Ben Jackson, Nicholas J. Loman, Áine O’Toole, Samuel M. Nicholls, Kris V. Parag, Emily Scher, Tetyana I. Vasylyeva, Erik M. Volz, Alexander Watts, Isaac I. Bogoch, Kamran Khan, the COVID-19 Genomics UK (COG-UK) Consortium, David M. Aanensen, **Moritz U. G. Kraemer**, **Andrew Rambaut**, **Oliver G. Pybus**

---

This repository contains the data and code used to generate the results
presented in *PAPER DOI*. 

Note that because of the GISAID [terms of use](https://www.gisaid.org/registration/terms-of-use/) genomic sequences cannot be shared in this repository. Instead, we make the GISAID accessions available and provide a table of acknowledgements. All genomic sequences produced by COG-UK are available [here](https://www.cogconsortium.uk/data/).

## Abstract

_The UK’s COVID-19 epidemic during early 2020 was one of world’s largest and unusually well represented by virus genomic sampling. Here we reveal the fine-scale genetic lineage structure of this epidemic through analysis of 50,887 SARS-CoV-2 genomes, including 26,181 from the UK sampled throughout the country’s first wave of infection. Using large-scale phylogenetic analyses, combined with epidemiological and travel data, we quantify the size, spatio-temporal origins and persistence of genetically-distinct UK transmission lineages. Rapid fluctuations in virus importation rates resulted in >1000 lineages; those introduced prior to national lockdown were larger and more dispersed. Lineage importation and regional lineage diversity declined after lockdown, whilst lineage elimination was size-dependent. We discuss the implications of our genetic perspective on transmission dynamics for COVID-19 epidemiology and control._


## Repository usage and structure

The structure of this repository is shown in the following tree output.

```
uk-intros-analyses/
├── analyses
│   ├── epidemiological
│   ├── phylogenetic
│   └── spatial
├── data
│   ├── epidemiological
│   ├── phylogenetic
│   └── spatial
├── LICENSE
└── README.md
```

- **Input data**: All input data that we are able to share publicly are stored in the [`data`](data/) directory.
- **Analyses:** The [`analyses`](analyses/) directory contains directories containing the details of each
  type of analysis.
  + **Epidemiological analyses:** The `epidemiological` directory contains a [`README.org`](analyses/epidemiological/README.org) file describing how
    to run the included scripts to carry out the epidemiological analysis. The
    output goes into a `results` directory.
  + **Phylogenetic analyses:**
  + **Spatial analyses:**


- The `data` directory contains all of the input data for the analyses which we
  are able to share publicly.
- 
  + The `epidemiological` directory contains a `README.md` file describing how
    to run the included scripts to carry out the epidemiological analysis. The
    output goes into a `results` directory.
  + The `phylogenetic` and `spatial` directories follow the same layout as the
    `epidemiological` directory.



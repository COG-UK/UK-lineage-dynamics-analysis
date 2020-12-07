# Epidemiological data

Epidemiological input data used in the analyses.


## Numbers of reported COVID-19 cases in the UK

- [`data_2020-Aug-31_UKtotal.csv`](data_2020-Aug-31_UKtotal.csv)
- [`data_2020-Aug-31-UKnations.csv`](data_2020-Aug-31_UKnations.csv)

Reported cases in the whole UK and each nation, ordered by specimen date.

Data downloaded from [https://coronavirus.data.gov.uk/cases](https://coronavirus.data.gov.uk/cases) on 1 September 2020.


## Number of reported COVID-19 deaths worldwide 

- [`jhu-deaths.csv`](jhu-deaths.csv)

The cumulative number of COVID-19 deaths for each country downloaded from the
JHU CSSE COVID-19 Database (date accessed: 19 August 2020) (Dong *et al* 2020).

## Estimated number of COVID-19 infections in the UK

- [`flaxman-results.csv`](flaxman-results.csv)

For the COVID-19 epidemics in several European countries, estimates of

- the number of infections and
- the impact of non-pharmaceutical interventions.

Downloaded from [https://mrc-ide.github.io/covid19estimates/data/results.csv](https://mrc-ide.github.io/covid19estimates/data/results.csv) on 2020-09-07


## Mobility data

For complete details see the *Travel and mobility data* section of the
Supplementary Materials.

- [`extra-uk-arrivals.csv`](extra-uk-arrivals.csv)

The estimated number of daily arrivals into the UK from neighbouring countries
via means other than air travel, e.g., by ferry or the Channel Tunnel. 

- [`home-office.csv`](home-office.csv)

UK Home Office provides statistics describing the number of inbound travellers
arriving in the UK by air on each day.

## Population statistics

- [`un-population.csv`](un-population.csv)

Country population size estimates downloaded from the UN Department of Economic
and Social Affairs website:
https://population.un.org/wpp/Download/Standard/Population/

## Share of the population living in extreme poverty

We retrieved data of percentage of country populations living in poverty from
the World Bank, as curated by the team at Our World in data and extracted
estimates of percentages of populations living in poverty from Elvidge /et al/
(2009) to supplement the World Bank measurements.

### World Bank data

- [`share-of-the-population-living-in-extreme-poverty.csv`](share-of-the-population-living-in-extreme-poverty.csv)

This data was obtained from Our World in Data which at the following link:
[https://ourworldindata.org/extreme-poverty#the-evolution-of-extreme-poverty-country-by-country]()
it was sourced from the World Bank which gives the following as a recommended
citation.

### Elvidge *et al* (2009)

- [`elvidge2009global.csv`](elvidge2009global.csv)

We extracted Table 1 from Elvidge *et al* (2009) using `tabula-1.2.1` and
adjusted the header for clarity.

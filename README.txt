IRS-Migration-Data

 DOI: 10.5281/zenodo.15292816 

-Description-

This repository contains the raw migration data from the Internal Revenue Service (IRS) for the years 1990 to 2021.
It also provides cleaned and processed datasets for:

    1995 to 2021 (county-level)

    2011 to 2021 (county-level)

Additionally, all the R code used to clean, process, and generate the datasets is included.

This project is intended to facilitate research on internal migration patterns within the United States, making IRS migration data more accessible and usable.

Note that there is a discontinuous jump in the way the IRS collected it's data in the year 2011;  results may vary when using the complete dataset.  More details are available
on the IRS website, detailed below.

-Contents-

    Pre-2011/ — Raw IRS migration data files (1990–2010)

    Post-2011/ - Raw IRS migration data files (2011-2021)

    Corrupted Data/ - Data files which are unusable

    Cleaned Data/ — Cleaned datasets ready for analysis

    Code/ — R scripts used to process and clean the datasets

    metadata.xml — Dublin Core-compliant metadata file for citation and archiving

    LICENSE.txt - A copy of the MIT License

-Variables-

The cleaned datasets include the following variables:

Variable		Description

FIPS			The FIPS code for the county for which the migration data details the flow into or out of.

year			The year that the migration statistics apply to. Note that the year is defined by the start year of the observations and runs into the next year.

description		A description of the classification of migrants, or the county which the migrants are coming from or going to.

other.county.FIPS	The FIPS code for the county which the observation records migrants coming from or going to.

num.returns.inflow	The number of recorded tax returns which flow into the county from the other county. This number is analogous to the number of households.

num.exemptions.inflow	The number of recorded tax exemptions which flow into the county from the other county. This number is analogous to the number of individuals.

income.inflow		The total adjusted gross income which is flowing into the county from the other county. This number is in thousands of dollars.

num.returns.outflow	The number of recorded tax returns which flow out of the county to the other county. This number is analogous to the number of households.

num.exemptions.outflow	The number of recorded tax exemptions which flow out of the county to the other county. This number is analogous to the number of individuals.

income.outflow		The total adjusted gross income which is flowing out of the county to the other county. This number is in thousands of dollars.

net.returns		The net number of returns flowing in or out of the county, calculated by subtracting the outflow 
			returns from the inflow returns. Positive values indicate a net flow into the county; negative values indicate a net flow out.

net.exemptions		The net number of exemptions flowing in or out of the county, calculated by subtracting the outflow exemptions from the inflow exemptions. 
			Positive values indicate a net flow into the county; negative values indicate a net flow out.

net.income		The net income flowing in or out of the county, calculated by subtracting the outflow income from the inflow income. 
			Positive values indicate a net flow into the county; negative values indicate a net flow out.

nonmigrants		The number of individuals which did not migrate in the county in that year.  Calculated with the number of non-migrating exemptions.
			Note that there is a separate observation for each county which also records the number of non-migrating returns, exemptions and income.

population.2010		The baseline population for each county in the year 2010.  Calculated by adding the total number of in-migrants into a county and subtracting
(2011-2021 data		the number of out-migrants.
only)

-Usage-

All data files are provided in standard formats (.csv).
R scripts are provided to reproduce cleaning steps or adjust for custom use cases.

To run the cleaning scripts:

    Ensure you have R installed (the scripts were programmed using version 4.4.2).

    Install required R packages (see below).

    Run the scripts in the code/ folder.

Required R Packages

    dplyr

    tidyr

    readr

    stringr

Installation is automatically done when running the full script, using the "pacman" package.

-Data Source-

The original IRS migration datasets were obtained from IRS Statistics of Income - Migration Data

Please refer to the IRS website for more detailed documentation about data collection methods and limitations.
License

https://www.irs.gov/statistics/soi-tax-stats-migration-data

This project is licensed under the MIT License — see the LICENSE file for details.

-Citation-

If you use this repository, please cite it as:

Daniel Malone. (2025). IRS-Migration-Data. GitHub.  DOI: 10.5281/zenodo.15292816 

-Contact-

For any questions or feedback, please open an Issue or contact me through GitHub.
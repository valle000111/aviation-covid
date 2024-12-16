# Bachelor Thesis Code Repository

This repository contains the final code and datasets used for the analysis presented in my bachelor thesis. Below is an overview of the repository structure and its contents.

## Thesis Title

The Impact of COVID-19 on the Number of Flight Passengers in the European Union: A Pre-, During-, and Post-Pandemic Analysis.  
Written at the Chair of Applied Econometrics at TUM School of Management in 2024.

## Repository Structure

### Folders
- **`Data Sources`**:  
  Contains the raw datasets used in the analysis:
  - `covid.csv`: 
    COVID-19 data, including new cases per 100k for the EU countries. (Original source had worldwide data, however due to GitHub file size limitations, this was cut down to the EU-27 countries. This does not affect the `Data Setup.R` script)
  - `flight_arrivals.csv`: 
    Data on flight arrivals for the EU countries.
  - `flight_departures.csv`: 
    Data on flight departures for the EU countries.
  - `international_travel_covid.csv`: 
    Data on international travel restrictions during the pandemic.
  - `tourism.csv`: 
    Data on tourism arrivals for the selected countries.

`covid.csv` and `international_travel_covid.csv` are given as a Backup in Case the Web Download is not available anymore.


- **`LaTeX Sources`**:  
  Contains the source files needed to compile the PDF version of the thesis.  
  - `TODO.tex`: 
    The main LaTeX file for the Bachelor Thesis.  
  - `references.bib`: 
    Bibliography file listing all referenced literature and data sources.

- **`R Files`**:  
  Contains all scripts and subfolders related to the analysis:
  - `Data Setup.R`:  
    Script for loading, cleaning, and merging the datasets. This file also lists the required R packages for running the analysis.
  - `Regression.R`:  
    Script for performing regression analysis, including configurations and outputs.
  - `Plots`:  
    Folder containing R scripts used to generate the visualizations included in the thesis.
  - `Tables`:  
    Folder containing R scripts used to create tables presented in the thesis.

### Files
- `README.md`:  
  The current file, providing an overview of the repository structure and instructions for use.
- `Bachelor Thesis.pdf`:  
  Final PDF Version of the Thesis.
- `Permission to View.pdf`:  
  Requirement of the University to view the Thesis.

## Usage
To replicate the analysis, follow these steps:
1. Clone this repository.
2. Open `Data Setup.R` from the `R Files` folder to load, clean, and merge the datasets. Unless you are working in RStudio, set your file path for the `flight_arrivals.csv`, `flight_departures.csv`, and `tourism.csv` files.
3. Ensure all required R packages are installed. Install commands are in the `Data Setup.R` file.
4. Run the scripts in the `Plots` and `Tables` subfolders to generate the figures and tables.
5. Use `Regression.R` to perform regression analyses and generate outputs.

If you have any questions or encounter any issues, please feel free to reach out.

---

This repository ensures reproducibility of all analysis and visualizations from the thesis. Thank you for visiting!
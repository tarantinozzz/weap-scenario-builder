# WEAP Scenario Configuration and Automation

This project streamlines the preparation of water-resource modeling scenarios in WEAP. It replaces a manual spreadsheet-based configuration workflow with an R Shiny application that brings scenario setup, standardized naming, parameter export, and VBScript generation into one interface.

## Why this project was developed

The previous workflow relied on manually editing scenario parameters in spreadsheets and preparing inputs for separate automation scripts. Repeated data entry and manual naming created opportunities for inconsistent settings and configuration errors. The application was designed to reduce those manual steps and make scenario preparation more consistent. No measured reduction in processing time or error rates is claimed in this repository.

## What the application does

- Builds a data-driven interface from a CSV configuration, including switches, dropdowns, radio grids and checkbox groups.
- Lets users save a base scenario and create variations by changing selected parameters.
- Applies configured dependencies, mutually exclusive options and selected input checks. Validation is partial; limitations are listed below.
- Generates standardized scenario names using lookups, numeric transformations, conditional tokens and separators.
- Exports a scenario parameter table and generates VBScript files for the downstream WEAP simulation and results-export workflow.

## How the workflow fits together

Configuration CSV + naming lookup → Shiny interface → base scenario and variations → parameter CSV → generated VBS → local WEAP execution and results export.

The application combines scenario configuration and script generation in one R workflow. The Run WEAP button generates the automation scripts; it does not launch WEAP or execute simulations. Executing those scripts is a separate step in a compatible local WEAP environment. This project improves the modeling workflow; it does not change WEAP's solver or demonstrate faster simulation calculations.

## What this public version demonstrates

All supplied configuration records are synthetic. Branch paths, display labels, variable names, notes, defaults, choices, lookup values and naming tokens have been replaced with generic examples. They do not represent a real model, asset, location, capacity, project or operational scenario. No original-to-example mapping or operational WEAP model is included.

The public version demonstrates the interface, scenario-building process and automation design. It cannot connect unchanged to a real model. Its widget/rule pattern and dependency relationships remain visible, as described below.

## Files

- 12.R: Shiny interface and VBS generator.
- data/inputs.csv: synthetic input configuration.
- data/naming_lookup.csv: synthetic naming lookup using Trigger, Abbr and CalSim columns. Extra descriptive columns from the private source lookup are not distributed.

## Setup

Install R and optionally RStudio. Run once:

```r
install.packages(c("shiny", "shinyWidgets", "shinythemes", "openxlsx", "Rcpp",
                   "rio", "rJava", "dplyr", "tidyr", "stringr"))
```

rJava needs a compatible Java installation. Package versions are not pinned. Set the working directory to the folder containing 12.R and README.md, then run `source("12.R")`.

Save a base scenario, add at least one variation, then click Done. A parameter CSV is saved locally in data/. Run WEAP closes the app and generates VBS files under results/; it does not execute WEAP. Generated scripts need Windows, a suitable local WEAP installation and a model configured to match the example branches, variables, Example Future scenario and Example Summary/Example Details overviews. Current Accounts is a standard WEAP scenario name retained in the code. No matching WEAP model is included, so these example VBS files cannot be used unchanged with a real model.

## Synthetic configuration

All branch paths use a flat ExampleModel path with neutral parameter IDs. Branch_2 labels, Variable values, Note text, headers, groups, choices, defaults, trigger condition values, naming parameters and lookup records were replaced with invented examples. Trigger_Branch references point to the replacement Branch values. The mutually exclusive checkbox label in R matches the replacement demand row. Real configuration values, dates, abbreviations, expressions, amounts, descriptive lookup fields and the original naming convention are omitted. Internal names in R comments and hard-coded model scenario/overview names were removed or replaced.

The schema and application rule identifiers are retained for compatibility: Branch, Variable, Note, UI, Type, Choices, default, Header, Branch_2, Labels, Trigger_Branch, Trigger_When, Group, Name_Rule, Name_Param and Name_Sep. Widget types and naming rule identifiers such as lookup, math and demand are program controls, not actual facility names. The supplied rows preserve the source widget/rule pattern and dependency relationships to demonstrate the application. This is a synthetic configuration for demonstration, not an assurance that every structural detail of the original application is concealed. No original-to-example mapping is included.

## Public demonstration version

The original application is used in an internal WEAP modeling workflow. This public version presents its interface, scenario configuration logic, naming engine, and automation script generation.

To protect operational information, actual model branches, variable names, configuration values, and naming conventions have been replaced with synthetic examples. The operational WEAP model and its datasets are not included.

Executing the generated VBS files requires a local WEAP installation and a model with matching branches, variables, scenarios, and output overviews. The example configuration is not intended to run unchanged against an operational model.

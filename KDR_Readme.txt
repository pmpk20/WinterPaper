This WinterREADME.TXT file was generated on 28/09/2024 by Peter King


-------------------
GENERAL INFORMATION
-------------------


1. Title of Dataset:
Repo for the data and code for the paper "Stated preferences for the colours, smells and sounds of biodiversity" in Ecological Economics from the RELATE ERC-funded project.


2. Author Information


  Principal Investigator Contact Information
        Name: Prof. Zoe Davies.
           Institution: University of Kent
           Email: z.g.davies@kent.ac.uk

  Author Contact Information
           Name: Dr. Peter King
           Institution: University of Leeds
           Email: p.king1@leeds.ac.uk - p.m.king@kent.ac.uk


3. Date of data collection: 25/01/2021 - 19/02/2021


4. Geographic location of data collection: Great Britain


5. Information about funding sources that supported the collection of the data:
This work was funded by the European Research Council (ERC) through the Horizon 2020 Research and Innovation Programme (Consolidator Grant no. 726104).
Analysis used specialist and High-Performance Computing systems provided by Information Services at the University of Kent using RStudio 4.4.1 with Apollo version 0.3.3 (R Core Team, 2021; Hess and Palma, 2019).


---------------------
FILE OVERVIEW
---------------------

Folders:
CEModelData: Inputs for estimated models.
CEModelScripts: Scripts to estimate choice models.
CEoutput: Outputs from estimated models, split into models one, two, and three
OtherData: Inputs to plotting, tables, cleaning and other scripts. Also Winter_dataframe_Step4.csv which is the final output
OutherOutput: All table and figure outputs.
OtherScripts: R scripts to setup the data, produce the tables, figures, and other information in the manuscript.

Scripts:
The workflow to replicate the paper is detailed in 00_Winter_StepByStep.R and I have numbered the scripts in the correct order.

Methods for processing the data:
Survey data collected using Qualtrics and a survey research company. Sample collected using quota sampling.


3. Instrument- or software-specific information needed to interpret the data:
R version 4.4.1 (2024-06-14 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit) [work laptop not HPC]
Running under: Windows 11 x64 (build 22631)
RStudio 2023.06.2+561 "Mountain Hydrangea"



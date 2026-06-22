------------------------------
### Stated preferences for the colours, smells and sounds of biodiversity: supporting dataset ####
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.594113641.svg)](https://doi.org/10.5281/zenodo.20645895)


- ERC-Funded RELATE Project Work Package Five repo for the data and code
- Accepted at Ecological Economics: https://doi.org/10.1016/j.ecolecon.2024.108410 
- Author: Peter King (p.king1@leeds.ac.uk)
- Last Edited: 08/10/2024.
- Guide to replicating the paper from survey data


#####  Comments: 
- This works locally on my machine and HPC so hopefully does for you too.
- Current issues: 
  - The *model.rds* and *unconditionalWTP* are too large for github so do not appear here. 
- I have anonymised all location data where possible. 
  - For the full survey data please see: https://data.kent.ac.uk/480/
- If all the CEoutput isn't here, it is at the above official repo.
- We did loads of additional analysis which isn't here but ask me for it
- Using specialist and High-Performance Computing systems provided by Information Services at the University of Kent. Using RStudio and Apollo (Hess and Palma, 2019).
- This work was funded by the European Research Council (ERC) through the Horizon 2020 Research and Innovation Programme (Consolidator Grant no. 726104).
- Ethical approval for the data collection was granted by the School of Anthropology and Conservation Research Ethics Committee, University of Kent (Ref: 009-ST-19).

------------------------------
##### Folders:
- CEModelData: Inputs for estimated models. 
- CEModelScripts: Scripts to estimate choice models. 
- CEoutput: Outputs from estimated models, split into models one and two. 
- OtherData: Inputs to plotting, tables, cleaning and other scripts. Also Winter_dataframe_Step4.csv which is the final output 
- OutherOutput: Misc tables and figures.  
- OtherScripts: R scripts to manipulate data, present outputs, consumer surplus.  

##### Scripts:
The workflow to replicate the paper is detailed in 00_Winter_StepByStep.r and I have numbered the scripts in the correct order.

##### Methods for processing the data: 
Survey data collected using Qualtrics and a survey research company. Sample collected using quota sampling. Additional data from ONS.

##### Instrument- or software-specific information needed to interpret the data:
R version 4.4.1
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 11 x64 (build 19045)
RStudio  "2023.06.2+561" "Mountain Hydrangea"

------------------------------------------------------------------------

#### Any Questions: [p.king1\@leeds.ac.uk](mailto:p.king1@leeds.ac.uk)

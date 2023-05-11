------------------------------
### ERC-Funded RELATE Project Work Package Five ####
- Repo for the data and code for the "Stated preferences for the colours, smells and sounds of biodiversity outweigh those for ecological function. " from the RELATE ERC-funded project.
- Author: Peter King (p.m.king@kent.ac.uk)
- Last Edited: 11/05/2023.
- Guide to replicating the paper from survey data


#####  Comments: 
- This works locally on my machine and HPC so hopefully does for you too.
- Current issues: 
  - The *model.rds* and *unconditionalWTP* are too large for github so do not appear here. 

------------------------------
##### File Structure: 

Folders:
- CEModelData: Inputs for estimated models. 
- CEModelScripts: Scripts to estimate choice models. 
- CEoutput: Outputs from estimated models, split into models one and two. 
- OtherData: Inputs to plotting, tables, cleaning and other scripts. Also Winter_dataframe_Step4.csv which is the final output 
- OutherOutput: Misc tables and figures.  
- OtherScripts: R scripts to manipulate data, spatial analysis, present outputs, consumer surplus.  

Scripts:
The workflow to replicate the paper is detailed in 00_Winter_StepByStep.rmd and I have numbered the scripts in the correct order.

Methods for processing the data: 
Survey data collected using Qualtrics and a survey research company. Sample collected using quota sampling. Additional data from ONS.


Instrument- or software-specific information needed to interpret the data:
R version 4.2.0 (2022-04-22 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19045)
RStudio 2022.02.2 Build 485 "Prairie Trillium" Release (8acbd38b, 2022-04-19) for Windows


------------------------------------------------------------------------

#### Any Questions: [p.m.king\@kent.ac.uk](mailto:p.m.king@kent.ac.uk)

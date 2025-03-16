The resulting data file is in result.csv

Death column and Rate columns come from https://www.cdc.gov/nchs/pressroom/sosmap/drug_poisoning_mortality/drug_poisoning.htm in the Data folder in this github respository with its corresponding years and state
Population Data from the annual.csv and the wymoning.pop.data data object come from FRED 
For example 
![image](https://github.com/user-attachments/assets/b941291e-cde5-4428-9901-0b021682ed40)
Other columns such as PERCENTBACHELORSDEGREEORHIGHER and STATEGOVTAXCOLLECTIONSTOTALTAXES are data from FRED. 
Instructions.

File structure: Make sure in a folder you have the following files
fredGCT1502STATE.R
fredGDPSTATE.R
fredHOWNSTATE.R
fredPCPISTATE.R
fredTOTLTAXSTATE.R
datacleaning.R
annual.csv
DOMBS_2020.csv

And then run datacleaning.R 
Important: errors in file paths - look at datacleaning.R 

![image](https://github.com/user-attachments/assets/883a9571-ff14-409b-b4b1-9327d16d71f6)


All 50 States, FRED Data https://fred.stlouisfed.org/ 
Yearly - 2014- 2022

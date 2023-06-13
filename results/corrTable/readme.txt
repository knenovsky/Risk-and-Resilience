Correlation of first 5 Isos with the 1478 WD Indicators.
Every File one Iso
Every Iso was scaled and splitted into for parts. 

To compare it you have to look into the ranges and maybe scale your compered things as well. scales()
For example wdi_data_cons_df$iso3 %>% scale.

Coloumns:

iso[1-10]_all_all inlcudes the correlation of the WDI that are included in the dimred without adjustment.
Only available for the ones who were included in the dimred

iso[0-5]_*range* reflects the corrlation of that range of dimred with the WDI of the row

N_*range* Number of samples in that range

Pearson[0-5]_*range* Pearson correlation in that range with the WDI of the row. CAVE: Pearson shows the direction even though that is not correlating well

range_*range* shows exaclty the numerical in a scaled way

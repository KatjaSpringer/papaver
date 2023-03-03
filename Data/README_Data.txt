Explanation of the variables of the datasets: COX.xlsx, Measures.xlsx, Soil_humidity.xlsx

--------------------COX.xlsx----------------------------------
Dataset with the survival
-------------------------
Group = drought treatment
3_long_drought = long drought
2_short_drought = short drought
1_control = control group

prec_pred = Precipitation predictability 
1L = Less predictable precipitation
1M = More predictable precipitation

competition = referce to the competition treatment
0 = without competition 
1 = with competition

family = family of the plant (F1 = Family 1 till F12 = Family 12)

replicate = replicate of the family (each R1 (=Replicate 1) till R4)

nb_leavesi = Initial number of leaves of the plants (used as covariate in the model)

1 = first day of wilting 
0 = not wilted
1 = wilted

column 2,3,4,5,6,7,8,9,10,11 
0 = not wilted
1 = wilted

time = days they survived

status = status if there were not wilted till the end of the experiment
0 = not wilted
1 = wilted


--------------------Measures.xlsx-----------------------------
Dataset with the main measurments
-------------------------
Group = drought treatment
3_long_drought = long drought
2_short_drought = short drought
1_control = control group

prec_pred = Precipitation predictability 
1L = Less predictable precipitation
1M = More predictable precipitation

competition = referce to the competition treatment
0 = without competition 
1 = with competition

family = family of the plant (F1 = Family 1 till F12 = Family 12)

replicate = replicate of the family (each R1 (=Replicate 1) till R4)

nb_leavesi = Initial number of leaves of the plants (used as covariate in the model)

chosen_papaver (at the beginning of the experiment two papavers where planted in one pot to ensure that at least one plant survives (Plant 1 was cut if plant 2 was still alive, if not plant 1 was used in the further experiment))

edge_effect = column that contain if the plant was in the edge of the tray or not (only for trays with competition, since they could not be randomised till the end)
NA = not availible (because tray was randomised till the end)
0 = pot in the middle of the tray
1 = pot in the edge

biomass1 = biomass of the plant which was cut (refering to chosen_papaver) in g

nb_leaves1 = number of leaves (intermediate measurment)

longest_leaf1 = length of longest leaf in cm (intermediate measurment)

SPAD_16th_01 = SPAD measurment (16th January) = NOT INCLUDED in analysis

wilt_16th_01 = column that contains the information if the plant wilted on the 16th January 2023 
0 = not wilted
1 = wilted

SPAD_18th_01 = SPAD measurment (18th January) = NOT INCLUDED in analysis

wilt_18th_01 = column that contains the information if the plant wilted on the 18th January 2023 
0 = not wilted
1 = wilted

wilt_19th_01 = column that contains the information if the plant wilted on the 19th January 2023 
0 = not wilted
1 = wilted

SPAD_20th_01 = SPAD measurment (20th January) = NOT INCLUDED in analysis

wilt_20th_01 = column that contains the information if the plant wilted on the 20th January 2023 
0 = not wilted
1 = wilted

wilt_21th_01 = column that contains the information if the plant wilted on the 21st January 2023 
0 = not wilted
1 = wilted

wilt_22th_01 = column that contains the information if the plant wilted on the 22nd January 2023 
0 = not wilted
1 = wilted

wilt_23th_01 = column that contains the information if the plant wilted on the 23th January 2023 
0 = not wilted
1 = wilted

wilt_24th_01 = column that contains the information if the plant wilted on the 24th January 2023 
0 = not wilted
1 = wilted

wilt_25th_01 = column that contains the information if the plant wilted on the 25th January 2023 
0 = not wilted
1 = wilted

SPAD_26th_01 = SPAD measurment (26th January) = NOT INCLUDED in analysis

wilt_26th_01 = column that contains the information if the plant wilted on the 26th January 2023 
0 = not wilted
1 = wilted

longest_leaff = Length of the longest leaf final in cm


yellow leaves = contains the information if and how many yellowish leaves the plant had = NOT INCLUDED in analysis

root_length = Length of the main root in cm 

bground_biomass = belowground biomass in g

abground_biomass = aboveground biomass in g


--------------------Soil_humidity.xlsx-------------------------
Dataset with the soil humidity for the trays (random measurment)
-------------------------
date = date of measuring the soil humidity (with Soil Moisture Kit (Moisture Meter HH2) from delta-t.co.uk):
1 = 9th January 2023
2 = 19th January 2023
3 = 26th January 2023

drought_treatment = referce to the drought treatment (control, short drought and long drought)

prec_pred = Precipitation predictability 
L = Less predictable precipitation
M = More predictable precipitation

competition = referce to the competition treatment
0 = without competition 
1 = with competition


humidity = relative soil humidity (in %)

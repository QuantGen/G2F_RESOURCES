## Phenotypes

The script uses data from the GxE initiative of the G2F project available through the following site:
 - [G2F site](https://www.genomes2fields.org/resources/)

Trials follow an incompleted block design where a set of replicated hybrids are in complete blocks with two repetitions and a set of un-replicated hybrids assigned with stratification to one of the two replicates. Stratification ensured nearly equal proportion of families in each replicate. Hybrids were grouped by families that were randomized on each replicate.

The resulting file is [phenotypes.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/phenotypes.csv) and has the following data dictionary:

|Column|Description|
|------|-----------|
|alley_length| (feet) Length of alley|
|date_anthesis| (yyyy-mm-dd) Date when 50% of plants exhibit another exertion on more than half of the main tassel spike |
|date_harvest| (yyyy-mm-dd) Date the plot was harvested |
|date_plant| (yyyy-mm-dd) Sowing date |
|date_silking| (yyyy-mm-dd) Silking date |
|grain_moisture| (%) Grain water content at harvest |
|local_check| Name of the check used at each location |
|location| G2F field location name |
|pass| (integer) Designation of field pass. Combination of range and pass form coordinate grid system describing plot location within the field |
|pedigree| Pedigree or hybrid name. Commercial hybrid name in the case of local checks |
|plants_root_lodging| (integer) Number of plants that show root lodging per plot |
|plants_stalk_lodging| (integer) Number of plants bloken between ground level and top ear node at harvest |
|plants_stand| (integer) Number of plants per plot at harvest |
|plot| (integer) Designation of individual experimental unit |
|plot_area| (square feet) Plot area |
|plot_length| (feet) Plot length at harvest |
|plot_weight| (lbs) Shelled grain weight per plot |
|range| (integer) Field range of the plot. Perpendicular to corn rows |
|rep| (integer) Replicate block |
|row_spacing| (inches) Space between rows |
|rows| (integer) Number of rows per plot |
|source| Code assigned by project identifying hybrid or commercial hybrid name in case of local check |
|test_weight| (lbs/bu) Shelled grain weight per bushel |
|year| (integer) year of harvest |
|yield| (bushels per acre at 15.5% moisture assuming 56 lbs per bushel) Grain yield using plot area without alley |


[Go back to main](https://github.com/QuantGen/G2F_RESOURCES)

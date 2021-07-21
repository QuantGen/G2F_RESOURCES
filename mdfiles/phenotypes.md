## Phenotypes

The script uses data from the GxE initiative of the G2F project available through the following site:
 - [G2F site](https://www.genomes2fields.org/resources/)

Trials follow an incompleted block design where a set of replicated hybrids are in complete blocks with two repetitions and a set of un-replicated hybrids assigned with stratification to one of the two replicates. Stratification ensured nearly equal proportion of families in each replicate. Hybrids were grouped by families that were randomized on each replicate.

The resulting file is [phenotypes.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/phenotypes.csv) and has the following data dictionary:

|Column|Description|
|------|-----------|
|year| (integer) year of harvest |
|location| G2F field location name |
|pedigree| Pedigree or hybrid name. Commercial hybrid name in the case of local checks |
|source| Code assigned by project identifying hybrid or commercial hybrid name in case of local check |
|rep| (integer) Replicate block |
|plot_area| (square feet) Plot area |
|seed_number| (integer) Number of seeds sown per plot |
|date_plant| (yyyy-mm-dd) Sowing date |
|date_harvest| (yyyy-mm-dd) Date the plot was harvested |
|date_anthesis| (yyyy-mm-dd) Date when 50% of plants exhibit another exertion on more than half of the main tassel spike |
|date_silking| (yyyy-mm-dd) Silking date |
|plants_stand| (integer) Number of plants per plot at harvest |
|grain_moisture| (%) Grain water content at harvest |
|yield| (bushels per acre at 15.5% moisture assuming 56 lbs per bushel) Grain yield using plot area without alley |

[Go back to main](https://github.com/QuantGen/G2F_RESOURCES)

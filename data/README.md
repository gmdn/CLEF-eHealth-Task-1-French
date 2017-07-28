Data folder
================
Giorgio Maria Di Nunzio
2017/07/28

In this folder, we expect you to extract the traning and test files provided by the organizers of the Task. We expect to have the following subdirectories and files in this folder:

``` yaml
---
data:
    CLEFeHealth2017Task1_test_full_FR:
        aligned:
            corpus:
                AlignedCauses_2014_full.csv
            README.txt
        eval:
            clefehealth2017Task1_eval.pl
        raw:
            corpus:
                CausesBrutes_FR_test2014.csv
                CausesCalculees_FR_test2014_full.csv
                Ident_FR_test2014.csv
            README.txt
    CLEFeHealth2017Task1_training_FR:
        corpus:
            dev:
                AlignedCauses_2013dev.csv
                AlignedCauses_2013full.csv
            train:
                AlignedCauses_2006-2012full.csv
        dictionaries:
            Dictionnaire2006-2010.csv
            Dictionnaire2011.csv
            Dictionnaire2012.csv
            Dictionnaire2013.csv
            Dictionnaire2014.csv
            Dictionnaire2015.csv
        eval:
            clefehealth2017Task1_eval.pl
        PavillonLaurent.pdf
        README.txt
---
```

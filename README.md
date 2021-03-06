CLEF eHealth Task 1 French, Replication Track - (IMS) Unipd
================
Giorgio Maria Di Nunzio
2017/07/27

In this document, we describe the procedure to reproduce the experiments of the group IMS Unipd who participated to the CLEF eHealth 2017 Task 1 Multilingual Information Extraction - ICD10 coding and, in particular, to the "French raw dataset" subtask and Replication track.

Software Installation
---------------------

The system is written in R, therefore, it is platform independent (apart from tiny adjustments that may be needed if you access the filesystem, see later in the documentation).

1.  First, you need to download R from the [Comprehensive R Archive Network](https://cran.r-project.org). We are currently using version 3.4.0 (released on 2017/04/21).

2.  Then, download the [RStudio Desktop](https://www.rstudio.com/products/rstudio/download2/). We suggest to download the installer of the Open Source License, unless you prefer to download the Zip/tar archives. We are currently using version 1.10.143 (released on 2017/04/19)

3.  **Clone or download** the source code provided with this repository.

Prepare Data Files
------------------

Before running the code, you need to place the data files of the CLEF eHealth Task 1 French corpus in the directory where the program expects to find them. We assume that you already have the zip file provided by the organizers of Task 1; therefore, you just need to extract the content of the file in the directory **data** of the R project. (show directory structure for clarity) At the end, we expect to have the following subdirectories and files in the data folder:

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

Running Source Code
-------------------

After you decompress the zip file, in the "source" directory you will find the **ims\_unipd\_documentation\_French.Rproj** file that contains the whole RStudio project that we are going to use. You can either double click on the file or open RStudio and choose "Open Project..." from the File menu.

1.  The first we need to do is to install the R packages that are required to run the source code. Write in the RStudio Console the following line (and press Enter)

    ``` yaml
    source("./install_packages/install.R")
    ```

2.  In order to produce the index of the dictionary, write in the RStudio Console the following line (and press Enter)

    ``` yaml
    source("./index/index_dictionary.R")
    ```

    At the end of this second part, you will have two new files in the **index** folder named **index\_FR\_binary.RData**, and **index\_FR\_binary\_translit.RData**

3.  In order to load the French raw data, we have to run the script

    ``` yaml
    source("./utils/check_raw_causes_2014_test.R")
    ```

    which will clean some of the errors in the raw dataset file and prepare the dataframe that will be saved in the **data** folder as **causes\_brutes\_2014\_FR.RData**. During the execution of the script, you will see on the Console some messages like:

    ``` yaml
    [1] "line 69 = 7"
    [1] "line 306 = 6"
    [1] "line 336 = 8"
    [1] "line 389 = 8"
    ```

    We use this output to check what are the lines that have more than 6 fields (which should be the number of fields of each record).

4.  Now we are ready to set up the test runs. Before running the **run\_build\_test.R** script, open it in the Source panel of RStudio and comment either line 7 or line 8 according to the type of weight you want to use for the run ("binary" or "binary\_translit").

    ``` yaml
    # set the type of indexing for this run, choose either "binary" or "binary_translit"
    weight <- "binary_translit"
    #weight <- "binary"
    ```

    By default, we are going to use the bonary\_translit option also for the classification phase. Once you have chosen the type of weight, run the script

    ``` yaml
    source("./run_build_test.R")
    ```

    At the end of the script, there will be a new file in the **runs** folder: a **run\_binary\_translit.csv** (or **run\_binary.csv**). Moreover, at the end of the script, you will see on the Console a warning message like this one

    ``` yaml
    [1] "reading line 90000 of 91953"
    [1] "reading line 91000 of 91953"
    There were 50 or more warnings (use warnings() to see the first 50)
    ```

    This is because the expansion of the acronym is not optimized.

5.  Finally, we can classify each line of the run we have prepared. For these experiments, we will use the translitterated version of the run by running the **run\_classify\_translit.R** script.

    ``` yaml
    source("./run_classify_translit.R")
    ```

    When the scripts ends, there will be a new file in the **runs** folder: **run\_binary\_translit.csv\_unofficial.csv**. This is file that should be evaluated.

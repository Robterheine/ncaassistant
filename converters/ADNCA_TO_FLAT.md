# From an ADNCA dataset to NCA Assistant

NCA Assistant reads **flat files**: one row per sample, with subject, time,
concentration and, for crossover studies, treatment, period, sequence and dose.
It refuses files that look like CDISC ADNCA or SDTM PC data, because reading
them as flat files would ignore their analysis flags, derived records and time
variables.

**In the app**, set *What kind of file?* to **CDISC ADNCA dataset** on the
Upload page: the app shows a summary, asks for the same choices as below,
converts the dataset with the same code, and stores the choices and a
conversion log in the Analysis Record.

**Outside the app**, `adnca_to_flat.R` converts an analysis-ready, ADNCA-shaped
dataset into a flat file. Keep `adnca_import.R` (in the repository's `R/`
folder) next to it: it contains the conversion, shared with the app. Both make
every choice explicit, refuse what they cannot convert safely, and write a log
you can keep with your study documentation.

## If your dataset is a .xpt file

Neither the app nor the converter reads SAS transport (`.xpt`) files. Convert
the file to CSV first, for example in R:

```r
d <- haven::read_xpt("adnca.xpt")
write.csv(haven::zap_labels(d), "adnca.csv", row.names = FALSE, na = "")
```

`zap_labels()` removes the SAS variable labels, and `na = ""` writes missing
values as empty cells. This keeps values at full precision and missing values
missing; a conversion of the test fixtures this way gave results identical to
the original CSV. Keep the `.xpt` file with your documentation: the Analysis
Record hashes the CSV you upload.

## 1. Convert

In R, with the dataset as `.csv` or `.xlsx`:

```r
source("adnca_to_flat.R")
adnca_to_flat("adnca.csv", "flat.csv", time = "NRRLT")
```

or from a terminal:

```
Rscript adnca_to_flat.R adnca.csv flat.csv NRRLT
```

This writes `flat.csv` and `flat_conversion_log.txt`.

## 2. Choose the time variable

There is no default: the choice changes AUC.

| `time =` | Meaning | Use when |
|---|---|---|
| `"NRRLT"` | Nominal time since the dose of the profile | The protocol times are the basis of the analysis |
| `"ARRLT"` | Actual time since the dose of the profile | Actual sampling times are the basis. Pre-dose samples have negative times: add `zero_predose = TRUE` to set them to 0, or use `MRRLT` |
| `"MRRLT"` | Actual time, pre-dose samples at 0 | As ARRLT, with pre-dose already at 0 |

`AFRLT` (time since the **first** dose) is refused: in a crossover it puts
period 2 at about a week and every period-2 parameter would be wrong.

## 3. What the converter does

**Applies record selection, and counts it in the log**
- keeps records with `ANL01FL = "Y"` (when the variable exists);
- drops records with `PCSTAT = "NOT DONE"`;
- keeps one analyte (`paramcd = "..."`) or matrix (`pcspec = "..."`) when you name it.

**Refuses** (nothing is written) when the dataset has
- derived records (`DTYPE` populated, e.g. BLQ values already imputed) — the app's BLQ rule would impute a second time;
- more than one analyte or matrix and you did not choose one — analytes are never averaged or combined;
- more than one unit (even `ng/mL` and `ug/L`) or more than one LLOQ;
- no numeric time variable (date-times only), negative actual times without `zero_predose`, or profiles that do not start near time zero;
- duplicate times within a profile, more than one dose within a subject and period, or infusions;
- `AVAL` missing without a BLQ result (a missing value can mean "not taken" or "below LLOQ").

**Passes on** a missing `AVAL` whose result (`PCORRES`, `PCSTRESC` or `AVALC`)
is BLQ text such as `<0.5`, so the app's BLQ rule handles it.

## 4. Load the flat file in NCA Assistant

1. Upload `flat.csv` (comma separator, point decimal mark).
2. Map Subject, Time and Conc; for crossover data also Treatment, Period,
   Sequence and, if present, Dose. The log lists the mapping.
3. Set the LLOQ to the value in the log (`PCLLOQ`) and choose the BLQ rule.
4. Choose the concentration, time and dose units listed in the log.

## 5. Keep the log

`flat_conversion_log.txt` records the input and output files with their
hashes, the time variable, every selection and count, the units and the LLOQ.
Keep it with the Analysis Record exported from the app: together they document
the path from the ADNCA dataset to the reported parameters.

## Scope

The converter reads variables with common ADNCA names (`USUBJID`, `PARAMCD`,
`AVAL`, `AVALU`, `ARRLT`, `NRRLT`, `MRRLT`, `AFRLT`, `TRTP`, `APERIOD`,
`TRTSEQP`, `DOSEA`, `ANL01FL`, `DTYPE`, `PCSPEC`, `PCLLOQ`, `PCSTAT`). It has
not been checked against a specific version of the ADNCA Implementation Guide
and does not validate datasets for conformance. It does not build the merged
dataset from SDTM domains; that is done upstream, where it can be quality
controlled. NCA Assistant is not affiliated with, endorsed by, or certified by
CDISC. CDISC, SDTM and ADaM are trademarks of the Clinical Data Interchange
Standards Consortium.

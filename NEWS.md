# RFossilpol 0.0.2

- A general description of all changes can be found in [#13](https://github.com/HOPE-UIB-BIO/R-Fossilpol-package/pull/13)

## Individual Workflow sections

### Neotoma download

- download pollen records one-by-one ([#18](https://github.com/HOPE-UIB-BIO/R-Fossilpol-package/pull/18), [#50](https://github.com/HOPE-UIB-BIO/R-Fossilpol-package/pull/50))
- download Neotoma authors one-by-one ([#41](https://github.com/HOPE-UIB-BIO/R-Fossilpol-package/pull/41))
- {neotoma2} package can be used for site selection but is not set as defualt due to performance issues (very slow) ([#14](https://github.com/HOPE-UIB-BIO/R-Fossilpol-package/pull/14), [#27](https://github.com/HOPE-UIB-BIO/R-Fossilpol-package/pull/27), [#49](https://github.com/HOPE-UIB-BIO/R-Fossilpol-package/pull/49))


### Chronology

- save age-depth models as individual files ([#21](https://github.com/HOPE-UIB-BIO/R-Fossilpol-package/pull/21))
  - each AD model file carries information about chron.control table used for its creation ([#32](https://github.com/HOPE-UIB-BIO/R-Fossilpol-package/pull/32), [#29](https://github.com/HOPE-UIB-BIO/R-Fossilpol-package/pull/29), [#31](https://github.com/HOPE-UIB-BIO/R-Fossilpol-package/pull/31), [#33](https://github.com/HOPE-UIB-BIO/R-Fossilpol-package/pull/33))

### Harmonisation

- During cleaning of taxa names,  save `taxa_refrence_table` for back trackability to Neotoma taxonomy ([#36](https://github.com/HOPE-UIB-BIO/R-Fossilpol-package/pull/36))

### Outputs - References

- general overhaul ([#34](https://github.com/HOPE-UIB-BIO/R-Fossilpol-package/pull/34), [#37](https://github.com/HOPE-UIB-BIO/R-Fossilpol-package/pull/37))
  - user can select which output is saved using selected_outputs (new output "graphical_summary" which will produce an overview of data assembly)
  - change the barplot into a lollipop figure for the number of records ([#42](https://github.com/HOPE-UIB-BIO/R-Fossilpol-package/pull/42), [#39](https://github.com/HOPE-UIB-BIO/R-Fossilpol-package/pull/39), [#40](https://github.com/HOPE-UIB-BIO/R-Fossilpol-package/pull/40))
- taxa reference table has been added to the reproducibility bundle ([#38](https://github.com/HOPE-UIB-BIO/R-Fossilpol-package/pull/38))

## Terminology change

- "Private" to "Other" ([#24](https://github.com/HOPE-UIB-BIO/R-Fossilpol-package/pull/24))
- "sequence" to "record" ([#25](https://github.com/HOPE-UIB-BIO/R-Fossilpol-package/pull/25))

## Progress bars

- add progress bar to most `purrr::map*()` functions ([#48](https://github.com/HOPE-UIB-BIO/R-Fossilpol-package/pull/48))

## Utility functions

- move utility to {[RUtilpol](https://github.com/HOPE-UIB-BIO/R-Utilpol-package)} package ([#16](https://github.com/HOPE-UIB-BIO/R-Fossilpol-package/pull/16), [#17](https://github.com/HOPE-UIB-BIO/R-Fossilpol-package/pull/17), [#28](https://github.com/HOPE-UIB-BIO/R-Fossilpol-package/pull/28))
- replace `R.utils::withTimeout()` with `RUtilpol::do_in_time()` ([#35](https://github.com/HOPE-UIB-BIO/R-Fossilpol-package/pull/35))
- Spatial data is handled by {RUtilpol} ([#47](https://github.com/HOPE-UIB-BIO/R-Fossilpol-package/pull/47))

## Package documentation and UI

- update `DESCRIPTION` ([#19](https://github.com/HOPE-UIB-BIO/R-Fossilpol-package/pull/19))
- add package `CITATION` ([#22](https://github.com/HOPE-UIB-BIO/R-Fossilpol-package/pull/22))
- add project logo ([#43](https://github.com/HOPE-UIB-BIO/R-Fossilpol-package/pull/43))
- add onAttach message with version ([#20](https://github.com/HOPE-UIB-BIO/R-Fossilpol-package/pull/20))
- stop dependency on {devtools} ([#26](https://github.com/HOPE-UIB-BIO/R-Fossilpol-package/pull/26))
- check and clean namespace dependency ([#45](https://github.com/HOPE-UIB-BIO/R-Fossilpol-package/pull/))

# RFossilpol 0.0.1

- release of the package

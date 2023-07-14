# ReSources development version

## Version 23.07.0

## New Features
- checkbox for optionally only displaying the input for nimbleModel() instead of running the model when 
pressing _Run_

## Version 23.06.0

## New Features
- _Import Model_ button: additional option to import a model from pandora or url

## Version 23.05.1

### New Features
- button to reset a single table (#3)

## Version 23.03.4

### Bug Fixes
- Fix for baseline models when only one proxy present (#83)

## Version 23.03.3

### Updates
- remote models are loaded from the github folder `inst/app/predefinedModels` of the respective 
repository
- if there is no internet connection remote models are taken from the models that were saved with
  the last deployed app version

## ReSources 23.03.2

### Bug Fixes
- use most recent version of the shinyMatrix package (#73)
- fix update of tables after using enter to submit values (#82)

## ReSources 23.03.1

### Bug fixes
- forward more specific error messages to the user if conditions to run a model are not met (#81, #83) 

## ReSources 23.02.4

### Bug fixes
- switch to kaleido package instead of orca for plotly export (#76)

## ReSources 23.02.3

### Bug fixes
- fix update of covariate variables (#74)

## ReSources 23.02.2

### New Features
- the _Import Data_ module is now imported from the package DataTools (#70, PR #72)
- additionally to file import, now import from URL or from Pandora Platform is possible
- specific checks for matrix imports are integrated into the new _Import Data_ module
- all redundant code was removed
- using "file" as default source in Import Data

## ReSources 23.02.1

### Bug fixes
- fix bug in export of xlsx files
- fix height of sidebar with auto scroll

## ReSources 22.12.1

### Bug fixes
- fix loop when deleting a column or a row (#58)

## ReSources 22.11.3

### Bug fixes
- fix loop when entering new columns (#64)

## ReSources 22.11.2

### Bug fixes
- endless loop when uploading data with no names of list elements, that is no baseline model (#62)

## ReSources 22.11.1

### Bug fixes
- fix update of list names for sources and concentration tables after names where changed in target
tables, values are not reset anymore (#34)

## ReSources 22.09.1

### Enhancements
- check if uploaded data contains empty tables
- use shinyalerts in data uploads with different types of alarms: "error", "warning", "success"

## ReSources 22.08.2

### Bug Fixes
- fix conflicts with column names (#44)

## ReSources _older versions_

### Enhancements
- optimal prior (#42)
- interface updates (#23)

### Bug Fixes
- fix copy-paste bug (#33)
- fix crash after deleting cov column (#12)

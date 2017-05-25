Personal R package of Halle R. Dimsdale-Zucker. 

To install the current release in RStudio:
`devtools::install_github("hallez/halle", subdir="halle")`

When adding a new function, be sure to include `@export` within the description section and to run `devtools::document()` to add the function to the NAMESPACE file.

Versioning:
X.0.0: major change that is not backwards compatible
0.X.0: new function added
0.0.X: bug fix, updated documentation, etc.

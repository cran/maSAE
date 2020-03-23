# maSAE 2.0.1

* I have fixed the layout for tables in vignette "A Taxonomy of Estimators".

# maSAE 2.0.0

## Major Modifications
* I have added slot `auxiliaryWeights` to class `saeObj`, allowing for weighting 
the auxiliary information for incomplete spatial support. This is inspired by
package `forestinventory`.
* I have added the (pseudo) small area estimator and the (pseudo) synthetic
estimator to the output while removing the attributes hinting to Mandallaz'
publications.

This lead to refactoring the functions used in method `predict`.
I have nearly halved the lines of code in these functions, they are now _much_ 
easier to read, understand and maintain.

__The return value predict() has changed, it is giving 
the (pseudo) small area estimator and the (pseudo) synthetic estimator as well 
but not the attributes hinting to Mandallaz'
publications__

## Sticking with version 1.0.0
If you don't care about weights and (pseudo) small area estimator and the (pseudo) synthetic
estimators, you might want to stick with the old predict method from version 1.0.0. 
You can use  
`predict(..., version = "1.0.0)`  
or set 
`options(maSAE_version = "1.0.0")`
to do so.
The predictions for the extended (pseudo) synthetic estimator and its variance
are identical for version 1.0.0 and 2.0.0. This is ensured by tests in
runit\_tests/runit-v1.R.

## Minor Modifications
* Fixed tests for R 4.0.0 setting stringsAsFactors to FALSE by default.
* Added function `bind_data` to coerce different sampling phase data into a
  suitable `data.frame`.
* Added a vignette comparing packages `maASE` and `forestinventory`.
* Added a vignette that clarifies the estimators.

# maSAE 1.0.0

* Formalized tests using testthat.
* Updated the help pages and vignette.
* Adapted using packager.
* Added a `NEWS.md` file to track changes to the package.




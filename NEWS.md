# tdarec 0.2.0

## column modification - BREAKING CHANGE

Per issue #19, the persistent homology computation steps now modify data set list-columns in place, and these and the disaggregation steps assign roles as conventional {recipes} steps.
Previously, users could ignore the presence of persistence diagram list-columns because they were assigned the new role `"persistent homology"`. They must now be manually removed or their roles updated. Alternatively, and preferably, the data set list-columns can have their roles updated to something other than `"predictor"`, as illustrated in the README.
Users now also lose access to the data set list-column unless they explicitly duplicate it before computing persistent homology.

## unit tests

The minor version incorporates a patch to debug the vectorization tests, which had failed to run due to a prefix in the names of the generated test files.
Once corrected, the tent template functions step failed one test due to missing parameters; this has been corrected.

## default scale sequences

Default scale sequences derived from persistence diagrams now use extrema calculated after infinite values are dropped.
This accommodates an upgrade to {ripserr} that returns features with deaths at infinity.

# tdarec 0.1.0

* Initial CRAN submission.

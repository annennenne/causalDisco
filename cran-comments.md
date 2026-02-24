## R CMD check results

0 errors | 0 warnings | 1 note

Note: Possibly misspelled words in DESCRIPTION (we rewrote the description and these are false positives).

## Java library Tetrad

Our package now optionally provides an interface to the Java library Tetrad (https://github.com/cmu-phil/tetrad).
Since the .jar file is quite large (47.5 MB), we do not include it in the package. Instead, we provide an
`install_tetrad()` function that downloads and caches the .jar file (version 7.6.10) from Maven Central
(https://repo1.maven.org/maven2/io/github/cmu-phil/tetrad-gui/) using `tools::R_user_dir()`.

This approach was inspired by the R package r5r, which also provides an interface to a large Java library.

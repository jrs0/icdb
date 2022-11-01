# icdb 0.0.0.9001

* Added a `NEWS.md` file to track changes to the package.
* Added mapped database and table functions
* Fixed some missing dependency information for dbplyr in the DESCRIPTION (fixing install problem in README)
* Updated cache to use bth memory and disk
* Disabled cache by default. Call use_cache(TRUE) to use the cache
* Tables in a Databases object now require brackets to use: srv$dbname$tabname()
* Started adding tests

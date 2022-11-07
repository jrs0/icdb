# icdb 0.0.0.9001

* Added a `NEWS.md` file to track changes to the package.
* Added mapped database and table functions. Added code definition file parser.
* Fixed some missing dependency information for dbplyr in the DESCRIPTION (fixing install problem in README)
* Updated cache to use both memory and disk. Disabled cache by default. Call use_cache(TRUE) to use the cache. Added cache lifetime and configurable level 1 size.
* Renamed Databases to Server.
* Started adding tests
* Started a proper package documentation site

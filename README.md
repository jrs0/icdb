
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ICDB: a simplified database connection for NHS databases

<!-- badges: start -->
<!-- badges: end -->

The goal of ICDB is to wrap the dplyr DBI library and make it easier to
use NHS databases hosted on Microsoft SQL Servers. The library features
a connection that autocompletes database and table names, automatically
generates SQL queries from familiar dplyr functions, and caches results
to make speed up prototyping and development.

**This library is not fully developed yet – expect the interface and
other features to change**

## Installation

First clone or download this repository. If you use RStudio, open the
ICDB project, and install the library as follows:

``` r
# You may need to install devtools first 
devtools::install()
```

Once you have installed the library, you should be able to load the ICDB
package in any other script or project to try out the features. See the
sections below for how to use the library.

## Connecting to a database

The first step in using the library is connecting to a database server.
The ICDB library connects to a server, not a particular database. This
makes it possible to use the same connection across different databases
on the same server.

The easiest and most secure way to connect to the database server is to
use the Windows *ODBC Data Sources* application. This application stores
a pre-configured connection under a particular name (the *data source
name*), which can then be used in R code to connect to the database. By
using this type of connection, you do not have to store config files
with hard-coded database credentials.

To set up the connection:

1.  Open the *ODBC Data Sources* program
2.  Click *Add* to start adding a new data source
3.  Select the *ODBC Driver 17 for SQL Server*. **Important: do not
    select the SQL Server driver.** Click *Finish*.
4.  Choose an arbitrary name (e.g. *XSW*) for the connection – this will
    be the *data source name*. Add an optional description. In the
    server field, specify the hostname of the server (see the *Server
    Name* field when you open *SQL Server Management Studio*)
5.  Step through the other pages of the wizard, leaving all other values
    as default. At the end, make sure you click *Test Data Source…* to
    ensure the connection is working. If this step causes errors, make
    sure they are fixed before moving on. After completing the wizard,
    the new data source should be listed in the *ODBC Data Sources*
    program.

After setting up a data source name, try executing the code below
(replace *XSW* with whatever you used for the data source name):

``` r
library(icdb)
## The next line may take a little while to run, due to the way the library works.
srv <- Databases("XSW")
#> Connecting using data source name (DSN): XSW
```

The `srv` object behaves like a list of lists. The first level contains
databases on the server, and the next level contains tables in that
database. Database and table names tab complete (in RStudio and Emacs
ESS), to make it easier to find what you are looking for. After running
the code above, try typing `srv$` to see the list of database names.
Then type `srv$dbname$` to get the list of tables in the database called
*dbname*.

### Seeing a table summary

The simplest use of ICDB is inspecting the structure of a table. Do this
by running

``` r
# Print the structure of the table `tabname' in the database `dbname'
srv$dbname$tabname
```

This expression returns the same tibble that you would get if you had
run `dplyr::tbl` on a [DBI database
connection](https://solutions.rstudio.com/db/r-packages/dplyr/). This
object is not the full table – it is just a shell object showing what
columns and data types the table contains.

### Generating an SQL query

The main advantage of using dplyr (the library that ICDB uses) is that
SQL is often unnecessary, and can be replaced by familiar dplyr
functions. You can pipe the `srv` object to dplyr functions like
`filter` and `select` to get a tibble of data from the database. Try
running a snippet like the following (replace with valid database, table
and column names), which shows the SQL generated for a simple
dplyr-based query:

``` r
library(tidyverse)
## Assumes a database `dbname' contains a table `tabname', which has a numeric column `value'
srv$dbname$tabname %>% filter(value != 0) %>% select(value) %>% show_query()
```

To actually run the query, replace `show_query()` with `run()`:

``` r
# This returns a tibble containing the results of the query
results <- srv$dbname$tabname %>% filter(value != 0) %>% select(value) %>% run()
```

Once you have `results` (a tibble), you can do anything that you would
normally do with a tibble (`filter`, `select`, `mutate`, etc.)

The `run()` function is a wrapper around the `dplyr::collect()`, which
adds caching functionality to the query. This is described in the next
section.

### Caching for speeding up code development

Large queries (those returning a lot of data, or requiring a lot of
server-side processing) can take a long time to execute. This can make
running scripts difficult, if each query in the script takes minutes to
run. The ICDB library contains a simple query cache that helps speed up
these queries for development purposes.

When a query like

``` r
results <- srv$dbname$tabname %>% filter(value != 0) %>% select(value) %>% run()
```

is run, it may take several minutes to execute. However, the second time
it is run, the results are returned instantly. This is because the
`run()` function stored the result behind the scenes, in case you need
it again soon.

If you change any aspect of the query that changes the SQL, then the
query will be rerun. For example,

``` r
results <- srv$dbname$tabname %>% filter(value != 1) %>% select(value) %>% run()
```

will run a new query, because the `filter` condition changed. However,
if you go back to the old query, it will still be stored in the cache.
The cache is automatically deleted when the R session is closed.

TODO finish this section.

## Library development

This section contains notes about development, such as todo lists, etc.

### Code completedness

The functions in the package are labelled according to a 5 point scale
(0-4), according to the following criteria:

- 0: An initial version of the function has been written, but lacks
  either documentation or testing
- 1: The function has an initial version that also has initial
  documentation and is covered by some preliminary tests
- 2: To be confirmed…
- 3: To be confirmed…
- 4: To be confirmed…

These scores are provided at the end of the documnentation for each
function as a line of the form “doneness: 2/4”. You can take this as an
indication of what kind of state the function is in, and what needs most
work.

If there is no doneness label, assume that the function is incomplete.

### Todo list

Here is a list of improvements that need to be made:

- Connect to the server, not a database. Arrange things so that the user
  can autocomplete databases before tables. Try to make it work so that
  the same database connection can be used in multiple dplyr pipe
  operations.
- Flush the level1 cache to the disk at some point (i.e. when the
  session ends) to shorten the load time next session. Make sure there
  is a clear way to disable this (it might not be a desirable default).
- Need to try to get autocomplete working in every context it makes
  sense (col names etc.).
- Look into replacing dbSendQuery with dbGetQuery for simplicity
- Really need to find a way to lazily evaluate the contents of the
  Databases object. Currently, all the databases and tables are stored
  because the current autocomplete method rests of built-in
  autocompletion of lists – however, this requires the list to be
  populated. It would be better to find a different autocomplete method
  that allowed lazy evaluation of completion options.
- Need to reconcile the mysql and sql server (microsoft) way of getting
  lists of databases and tables. mysql returns objects properly using
  dbListObjects, but sql server lists the databases in a table and then
  there is no clear way to get the tables without raw sql.

## ICDB contains a graphical editing program for defining database mappings
## and editing groups of ICD-10 codes. This file provides functionality
## equivalent to editing the YAML configuration files manually, but it is
## designed to make the process easier and les error-prone. It is especially
## useful for the ICD-10 mappings, which uses a YAML file with a complicated
## structure to define groupings of codes.
##
## This file contains the interface that enables the graphical program to be
## opened within R. The binary for the program is stored in the inst/extdata/bin
## folder, with a version for Windows, Mac and Linux. The application uses the
## Tauri (Rust) framework, with Next.js as the frontend.

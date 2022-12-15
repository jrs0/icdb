devtools::load_all()
codes <- read.csv(system.file("testdata",
                              "icd10/icd10_code_list.txt",
                              package="icdb"))
raw <- codes %>% unlist()
parsed <- rep(raw,5) %>% icd10()

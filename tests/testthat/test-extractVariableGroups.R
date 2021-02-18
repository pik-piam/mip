context("extractVariableGroups Test")

test_that("check that extractVariableGroups correctly extracts variable groups", {
  x <- c("a|+|1|+|aa","a|+|2|abc","a|+|1|+|bb","a|+|1|+|cc","a|+|3|+|aa","a|+|3|+|bb","a","a|+|3")
  res <- list("a"=c("a|+|1|+|aa","a|+|2|abc","a|+|1|+|bb","a|+|1|+|cc","a|+|3|+|aa","a|+|3|+|bb","a|+|3"),
              "a|+|1"=c("a|+|1|+|aa","a|+|1|+|bb","a|+|1|+|cc"),
              "a|+|3"=c("a|+|3|+|aa","a|+|3|+|bb"))

  
  expect_mapequal(extractVariableGroups(x),res)
  expect_mapequal(extractVariableGroups(x,keepOrigNames = T),res)
  
  x <- as.character(read.csv(system.file("extdata","vars.csv",package = "mip"))[,1])
  expect_error(extractVariableGroups(x), NA)
  expect_error(extractVariableGroups(x,keepOrigNames = T), NA)
  
})
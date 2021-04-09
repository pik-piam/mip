context("extractVariableGroups Test")

test_that("check that extractVariableGroups correctly extracts variable groups", {
  x1 <- c("a|+|1|+|aa",
          "a|+|2|abc",
          "a|+|1|+|bb",
          "a|+|1|+|cc",
          "a|+|3|+|aa",
          "a|+|3|+|bb",
          "a",
          "a|+|1",
          "a|+|3",
          "a|++|bla",
          "a|++|blu",
          "a|+++|ble",
          "a|+++|blet",
          "a2 (unit)",
          "a2|++++|wunit (unit)",
          "a2|++++|wunit2 (unit 2)")

  res1 <- list("a"=c("a|+|1|+|aa","a|+|2|abc","a|+|1|+|bb","a|+|1|+|cc","a|+|3|+|aa","a|+|3|+|bb","a|+|1","a|+|3"),
               "a|+|1"=c("a|+|1|+|aa","a|+|1|+|bb","a|+|1|+|cc"),
               "a|+|3"=c("a|+|3|+|aa","a|+|3|+|bb"),
               "a 2"=c("a|++|bla","a|++|blu"),
               "a 3"=c("a|+++|ble" ,"a|+++|blet"),
               "a2 4"=c("a2|++++|wunit (unit)","a2|++++|wunit2 (unit 2)"))
  res2 <- res1
  names(res2) <- sub("a 2|a 3","a",names(res1))
  names(res2) <- sub("a2 4","a2 (unit)",names(res2))
  
  expect_identical(extractVariableGroups(x1),res1)
  expect_identical(extractVariableGroups(x1,keepOrigNames = TRUE),res2)

})
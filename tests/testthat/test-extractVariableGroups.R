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
  res3 <- list(`a|1` = c("a|1|aa", "a|1|bb", "a|1|cc"),
               `a|3` = c("a|3|aa","a|3|bb"),
               a = c("a|1", "a|3", "a|bla", "a|blu", "a|ble", "a|blet"))
  
  expect_identical(extractVariableGroups(x1),res1)
  expect_identical(extractVariableGroups(x1,keepOrigNames = TRUE),res2)
  expect_warning(extractVariableGroups(gsub("\\|[\\+]{1,}","",x1)))
  expect_identical(extractVariableGroups(gsub("\\|[\\+]{1,}","",x1[!grepl(")$",x1)])),res3)

  x4 <- c("B", "B|+|G", "A|++|C", "A|++|B", "A", "C|++++++++++++++++|A", "C")
  res4 <- list("B" = "B|+|G", "A 2" = c("A|++|C", "A|++|B"), "C 16" = "C|++++++++++++++++|A")
  res4sorted <- list("A 2" = c("A|++|B", "A|++|C"), "B" = "B|+|G", "C 16" = "C|++++++++++++++++|A")
  expect_identical(extractVariableGroups(x4, sorted = FALSE), res4)
  expect_identical(extractVariableGroups(x4, sorted = TRUE), res4sorted)

  x5 <- c("A|+|FE", "A|FE|+|B", "A|FE|+|A", "A|FE|++|D", "A|FE|++|C")
  res5 <- list("A" = "A|+|FE", "A|FE" = c("A|FE|+|B", "A|FE|+|A"), "A|FE 2" = c("A|FE|++|D", "A|FE|++|C"))
  expect_identical(extractVariableGroups(x5, keepOrigNames = FALSE, sorted = FALSE), res5)
  res5keep <- list("A" = "A|+|FE", "A|+|FE" = c("A|FE|+|B", "A|FE|+|A"), "A|+|FE" = c("A|FE|++|D", "A|FE|++|C"))
  expect_identical(extractVariableGroups(x5, keepOrigNames = TRUE), res5keep)
  res5sorted <- list("A" = "A|+|FE", "A|FE" = c("A|FE|+|A", "A|FE|+|B"), "A|FE 2" = c("A|FE|++|C", "A|FE|++|D"))
  expect_identical(extractVariableGroups(x5, keepOrigNames = FALSE, sorted = FALSE), res5)
  res5keepSorted <- list("A" = "A|+|FE", "A|+|FE" = c("A|FE|+|A", "A|FE|+|B"), "A|+|FE" = c("A|FE|++|C", "A|FE|++|D"))
  expect_identical(extractVariableGroups(x5, keepOrigNames = TRUE, sorted = TRUE), res5keepSorted)
})

test_that(
    'plotstyle.add() adds plot styles to the current session',
    {
        x <- plotstyle()[1]
        y <- setNames(
            paste(c('#',
                    head(rev(unlist(strsplit(x, '', fixed = TRUE))), n = -1)),
                  collapse = ''),
            names(x))

        invisible(plotstyle.add(names(x), names(x), y, replace = TRUE))

        expect_identical(object = plotstyle(names(x)), expected = y)
    })

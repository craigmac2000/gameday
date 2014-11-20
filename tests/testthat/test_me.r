test_that("case is ignored", {
	expect_equal(gday("canucks"), gday("CANUCKS"))
})

test_that("always returns logical", {
	expect_is(gday("canucks"), "logical")
})

test_that("asking for the city works just as well", {
	expect_equal(gday("canucks"), gday("Vancouver"))
})

test_that("Seattle does not have a NHL team", {
	expect_false(gday(team="Seattle"))
})


context("Check dates")

test_that("Vancouver Canucks had a game against Nashville Predators on 2014-11-02", {
	expect_true(gday(team = "canucks", date = "2014-11-02"))
	expect_true(gday(team = "predators", date = "2014-11-02"))
})

test_that("Washington Capitals did not play on 2014-11-10", {
	expect_false(gday(team = "capitals", date = "2014-11-10"))
	expect_false(gday(team = "washington", date = "2014-11-10"))
})

test_that("Wrong date type throws error", {
	expect_error(gday("Bruins", date = "201-411-12"), "Error")
})


context("Internet connection")

test_that("when internet connection is online", {
	expect_true(internet_connection())
})

test_that("when internet connection is offline", {
	expect_false(internet_connection())
})


context("check date")

test_that("returns true if date is formatted correctly", {
	expect_true(check_date("2014-02-14"))
})

test_that("returns an error if date is formatted incorrectly", {
	expect_error(check_date("201-402-14"))
})


context("scores")

test_that("returns scores if date is formatted correctly and occurs during the season", {
	expect_true(check_date("2014-02-14"))
})

test_that("returns an error if date is formatted incorrectly", {
	expect_error(scores("201-402-14"))
})

test_that("returns an error if date is during the off-season", {
	expect_error(scores("2014-08-08"))
})

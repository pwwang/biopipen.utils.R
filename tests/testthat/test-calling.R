test_that("do_call: works", {
    expect_equal(do_call("sum", list(1, 2, 3)), 6)
    expect_equal(do_call("sum", list(1, 2, 3, NA)), NA_real_)
    expect_equal(do_call("sum", list(1, 2, 3, NA, na.rm = TRUE)), 6)
})

test_that("do_call: works with ::", {
    expect_equal(do_call("base::sum", list(1, 2, 3)), 6)
})

test_that("do_call: works with function", {
    expect_equal(do_call(sum, list(1, 2, 3)), 6)
})

test_that("do_call: works with name", {
    expect_equal(do_call(rlang::as_name("sum"), list(1, 2, 3)), 6)
})

test_that("run_command: works", {
    expect_equal(run_command(c("echo", "hello"), print_command = NULL), 0)
    expect_equal(run_command(c("echo", "hello"), print_command = NULL, stdout = TRUE), "hello")
    # implicitly set stdout = TRUE
    expect_equal(run_command(c("echo", "hello"), print_command = NULL, stderr = TRUE), "hello")
})

test_that("run_command: works with fg", {
    # outputs: hello
    # How to capture/expect it?
    expect_equal(run_command(c("echo", "hello"), fg = TRUE, print_command = NULL), NULL)
})

test_that("run_command: works with input", {
    expect_equal(run_command(c("cat"), input = "hello", print_command = NULL, stdout = TRUE), "hello")
})

test_that("run_command: works with stdin", {
    tmpfile <- tempfile()
    writeLines("hello", tmpfile)
    expect_equal(run_command(c("cat"), stdin = tmpfile, print_command = NULL, stdout = TRUE), "hello")
})

test_that("run_command: works with env", {
    expect_equal(run_command(c("echo", "$var"), env = list(var = 1), print_command = NULL, stdout = TRUE), "1")
    expect_equal(run_command(c("echo", "$var"), env = "var=1;", print_command = NULL, stdout = TRUE), "1")
})

test_that("run_command: prints command", {
    expect_output(run_command(c("echo", "hello"), print_command = cat), "RUNNING COMMAND:\n  echo hello\n")
})

test_that("run_command: errors when command not found", {
    expect_error(expect_warning(run_command(c("not_a_command"), print_command = NULL)))
    expect_error(expect_warning(run_command(c("exit", "1"), stdout = TRUE, print_command = NULL)))
})

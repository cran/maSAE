test_exception <- function() {
    RUnit::checkException(maSAE:::throw("Hello, error!"))
}

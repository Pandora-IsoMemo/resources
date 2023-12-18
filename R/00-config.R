#' Config
#' 
#' @return (list) configuration parameters for import of data and models
config <- function() {
  config_path <- system.file("config.yaml", package = "ReSources")
  yaml.load_file(config_path)
}

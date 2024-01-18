#' @export
init <- function(wd = getwd(), git = TRUE, type = "plumber") {

  # If directory doesn't exist, create it
  if (!dir.exists(wd)) {
    dir.create(wd)
    setwd(wd)
  }

  if (git) {
    use_git()
  }

  switch(
    type,
    plumber = init_plumber()
  )

}

read_config <- function(wd) {
  path <- file.path(wd, "prodr.yaml")
  config <- tryCatch(
    yaml::read_yaml(path),
    error = function(e) {
      stop("Could not find prodr.yaml in working directory.")
    }
  )
  return(config)
}

#' @export
use_git <- function() {
  git2r::init(branch = "main")
  copy_gitignore()
}

#' @export
use_docker <- function(wd = getwd()) {
  config <- read_config(wd)
  if (config$type == "plumber") {
    system.file("docker", "Dockerfile.plumber", package = "prodr") |>
      file.copy(file.path(wd, "Dockerfile"))
  }
}

find_safe <- function(pattern, x) {
  res <- grepl(pattern, x)
  if (length(res) == 0) {
    return(FALSE)
  } else {
    return(res)
  }
}

#' @export
get_deps_file <- function(wd = getwd()) {
  file.path(wd, "deps.R")
}

write_to_deps <- function(pkg, deps_file = get_deps_file()) {
  command <- glue::glue("library({pkg})\n")
  write(command, deps_file, append = TRUE)
}

install <- function(pkgs) {
  deps_file <- get_deps_file()
  if (!file.exists(deps_file)) {
    file.create(deps_file)
  }
  current_pkgs <- readLines(deps_file)
  for (pkg in pkgs) {
    if (!find_safe(pkg, current_pkgs)) {
      renv::install(pkg)
      write_to_deps(pkg, deps_file)
    }
  }
}

init_plumber <- function() {
  copy_inst_dir("plumber")
  initialize_renv_plumber()
}

initialize_renv_plumber <- function() {
  cat("Initializing Renv...\n")
  renv::init(force = TRUE, restart = FALSE)
  cat("Adding Plumber to Renv...\n")
  write_to_deps("prodr")
  install("plumber")
  renv::snapshot(force = TRUE)
}

copy_gitignore <- function() {
  cat("Copying .gitignore to working directory...\n")
  path <- system.file("gitignore", package = "prodr")
  file.copy(
    path,
    file.path(getwd(), ".gitignore")
  )
  return(invisible())
}

# Copy files from inst to the working directory
copy_inst_dir <- function(dir) {
  cat("Copying files to working directory...\n")
  path <- system.file(dir, package = "prodr")
  files <- file.path(path, list.files(path, all.files = TRUE, no.. = TRUE))
  file.copy(
    files,
    getwd(),
    recursive = TRUE
  )
  return(invisible())
}

#' @export
entrypoint <- function() {
  options("box.path" = getwd())
  box::purge_cache()
  config <- read_config(getwd())
  switch(
    config$type,
    plumber = start_plumber(),
    shiny = start_shiny()
  )
}

start_plumber <- function() {
  box::use(
    . / src / main,
  )
  main$api
}

start_shiny <- function() {
  box::use(
    . / src / main,
  )
  main$app
}

get_mod_file <- function(module) {
  env <- box::topenv(module)
  name <- eval(rlang::expr(box::name()), env)
  path <- box::file(name, module = module)
  # If path is a directory, then append __init__.R
  # Otherwise, check if it ends in .R or .r
  if (dir.exists(path)) {
    path <- file.path(path, "__init__.R")
    return(path)
  }

  # Check if the path with suffix .R exists
  if (file.exists(paste0(path, ".R"))) {
    path <- paste0(path, ".R")
    return(path)
  }

  if (file.exists(paste0(path, ".r"))) {
    path <- paste0(path, ".r")
    return(path)
  }

  stop("Could not find module file.")
}

#' @export
route <- function(module) {
  path <- get_mod_file(module)
  plumber::pr(path)
}

#' Detecting Stics executables files according to the host OS
#'
#' @param dir_path JavaStics bin directory path
#' (or another one containing Stics executable files)
#'
#' @return A data.frame with columns: path (executable path),
#' exe (executable file name), version (from the executable)
#'
#' @keywords internal
#'
detect_stics_exe <- function(dir_path) {

  exe_win <- ".*stics.*\\.exe$"
  exe_mac <- ".*stics.*_mac$"
  exe_lin <- ".*stics.*"

  if ( SticsOnR:::is_windows() ) {
    bin_list <- list.files(dir_path,pattern = exe_win)
  }

  if ( SticsOnR:::is_unix()) {
    bin_list <- system(paste("find", dir_path, "-maxdepth 1 -type f -exec grep -IL . \"{}\" \\;"),
                       intern = TRUE) %>%
      grep(x=., pattern = paste0(exe_win, "|",exe_mac), invert = TRUE, value = TRUE)
  }

  if (SticsOnR:::is_mac()) {
    bin_list <- list.files(dir_path,pattern = exe_mac)
  }

  if ( !length(bin_list) ){
    warning("Not any Stics executable file detected in directory:", dir_path)
    return(NULL)
  }

  # Calling check_stics_exe for each one
  bin_info <- lapply(X = bin_list, function(x) check_stics_exe(x, version = TRUE, stop = FALSE))

  # Selecting successfull exec
  sel <- !unlist(bin_info)
  if (!any(sel)) return(NULL)

  bin_info <- bin_info[sel]
  bin_list <- bin_list[sel]

  # Selecting infos with version string in "version" attr
  sel <- unlist(lapply(bin_info, function(x) length(attr(x,"version")) == 1 ))

  if (!any(sel)) return(NULL)

  # Getting version string
  bin_info <- unlist(lapply(bin_info[sel], function(x) attr(x, "version")))
  bin_list <- bin_list[sel]

  bin_table <- data.frame( path = bin_list,
                           exe = basename(bin_list),
                           version = bin_info,
                           stringsAsFactors = FALSE)

  return(bin_table)
}

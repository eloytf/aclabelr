#' split_labels
#'
#' @param textfile
#'
#' @return nothing
#' @export
#'
#' @examples
split_labels<-function (textfile) {

  all<-readr::read_delim(file=textfile, delim = "\t",col_names = F)

  utags<-unique(stringr::str_split(all$X3,"_",n = 2,simplify = T)[,1])
  write_lable_tracks<-function(tag) {

  readr::write_delim(all[grep(tag,all$X3,perl = T),c(1,2,3)],delim = "\t",paste0(tag,"_",strptime(Sys.time(),"%Y-%m-%d"),".txt"),col_names = F)

  }
  lapply(utags,write_lable_tracks)

}

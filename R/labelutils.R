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


#' Title
#'
#' @param namehito
#' @param namecut
#'
#' @return nothing
#' @export
#'
#' @examples
cut_label_track<-function(namehito,namecut) {

  hito<-readr::read_delim(file = paste0(namehito,".txt"),delim = "\t",col_names = F)
  cut<-readr::read_delim(file = paste0(namecut,".txt"),delim = "\t",col_names = F)

  #hito<-intervals2instants(hito)

  hito$X1<-hito$X1-Vectorize(getintervalsize,vectorize.args = "instant")(cut,hito$X1)
  #no tengo claro que este bien, pero a las malas, hito$x2=hito$x1
  hito$X2<-hito$X2-Vectorize(getintervalsize,vectorize.args = "instant")(cut,hito$X2)


  readr::write_delim(hito,delim = "\t",paste0(namehito,"_recortado",".txt"),col_names = F)

}


#' getintervalsize
#'
#' @param labeldf
#' @param instant
#'
#' @return value, intervalsize
#'
#' @examples
getintervalsize<-function(labeldf,instant) {

  labeldf<-labeldf[labeldf[,1]<instant,]
  labeldf$X3<-pmin(instant,labeldf$X2)
  labeldf$X3<-labeldf$X3-labeldf$X1
  return(sum(labeldf$X3))
}

#' intervals2instants
#'
#' @param labeldf
#'
#' @return a data frame
#'
#' @examples
intervals2instants<-function(labeldf) {

  labeldf[,2]<-labeldf[,1]
  return(labeldf)

}

#' @exportClass Construct
#' @name Construct 
#' @rdname Construct
#' @aliases Construct-class 
#' @title The Construct class
#'
#' @description 
#' This class is part of the \pkg{SemDiff} which is part of the WMVG Project. 
#' 
#' @details Find here the defined slots for this class.
#'      @section Slots: 
#'          \describe{ 
#'              \item{\code{label}:}{
#'                  Object of class \code{"character"}, containing the name of 
#'                  the Construct.
#'                  The default value will be caluclated randomly.
#'              } 
#'              \item{\code{rpole}:}{
#'                  Object of class \code{"character"}, containing the name of 
#'                  the right pole of the construct
#'              } 
#'              \item{\code{lpole}:}{
#'                  Object of class \code{"character"}, containing the name of 
#'                  the left pole of the construct 
#'              } 
#'          }
#'  
#' @author Dipl. Kfm. Felix Lindemann \email{felix.lindemann@@hs-neu-ulm.de} 
#' 
#' Wissenschaftlicher Mitarbeiter
#' Kompetenzzentrum Logistik
#' Buro ZWEI, 17
#'
#' Hochschule fur angewandte Wissenschaften 
#' Fachhochschule Neu-Ulm | Neu-Ulm University 
#' Wileystr. 1 
#' 
#' D-89231 Neu-Ulm 
#' 
#' 
#' Phone   +49(0)731-9762-1437 
#' Web      \url{www.hs-neu-ulm.de/felix-lindemann/} 
#'          \url{http://felixlindemann.blogspot.de}
setClass(
  Class = "Construct",
  representation=representation( 
    label      = "character",
    assattr      = "character",
    rpole      = "character",
    lpole      = "character",
    ratings    = "matrix"
  ), 
  validity = function(object){
    
    N <- length(object@label) 
    if(length(object@rpole) != N) 
      return("Invalid Object of Type 'Construct': the length of the attributes 'rpole' and 'label' differ!")
    if(length(object@lpole) != N)     
      return("Invalid Object of Type 'Construct': the length of the attributes 'lpole' and 'label' differ!")
    if(length(object@assattr) != N)     
      return("Invalid Object of Type 'Construct': the length of the attributes 'assattr' and 'label' differ!")
    if(length(object@ratings == 1)){
      
    }else{
      if(length(ncol(ratings))!=N){
        return("Invalid Object of Type 'Construct': the Column-length of the attribute 'ratings' is not equal to the the length of the attribute 'label'!")
      }
    }
    return(TRUE) 
  } 
)
################################### initialize - Method ###################################################
#' @title initialize Method
#' @name initialize
#' @aliases initialize,Construct-method 
#' @rdname initialize-methods 
#' @param data can of type \code{\link{data.frame}} or \code{\link{list}}
setMethod("initialize", signature="Construct", function(.Object, data=NULL, ...) {
  
  li <- list(...)
  N<-1
  if(is.null(li$showwarnings))  li$showwarnings <- FALSE
  if(!is.null(data)){  
    if(class(data) =="list") {
      data <- as.data.frame(data)
    }
    if(class(data) =="data.frame") {
      N<-nrow(data)
      li$assattr    <- data$assatrr
      li$label <- data$label 
      li$rpole     <- data$rpole
      li$lpole     <- data$lpole
      li$ratings <- as.matrix(data$ratings)
    } else{ 
      stop("Error: argument data should be of type 'list' or data.frame'!")
    }
  } 
  if(is.null(li$label)) { 
    stop("Error: No label has been assigned")
  }
  if(is.null(li$rpole)) {
    li$rpole <- rep("NA", length(li$label))
  }
  if(is.null(li$lpole)) {
    li$lpole <- rep("NA", length(li$label))
  }
  if(is.null(li$assattr)) {
    li$assattr <- rep("NA", length(li$label))
  }
  if(is.null(li$ratings)) {
    li$ratings <-matrix()
  }
  .Object@label   <- as.character(li$label)
  .Object@rpole   <- as.character(  li$rpole)
  .Object@lpole   <- as.character(  li$lpole)
  
  .Object@assattr <- as.character(li$assattr)
  .Object@ratings <- li$ratings
  
  validObject(.Object)
  
  return(.Object ) 
})
################################### extract - Method ###################################################

#' @title Extract Methods
#' @name $
#' @aliases $,Construct-method 
#' @rdname Extract-methods-1
#' @param name Name of Attribute.
setMethod("$","Construct",function(x,name) {return(slot(x,name))})
#' @title Extract Methods
#' @name [
#' @aliases [,Construct-method 
#' @rdname Extract-methods-2
#' @param i Row Index
#' @param j Name or Column index
#' @param drop Optional value for Drop-Levels.
setMethod("[", "Construct",
          function(x, i, j, drop){
            N <- length(x)
            if(min(i) <=0) stop("Index i out of bound. it must be positive non zero.")
            if(max(i) >N) stop("Index i out of bound. it must not be larger than the total length.")
            
            
            if(!missing(j)){
              if(class(j) == "character"){
                return(slot(x,j)[i])
              }
              if(class(j) == "integer"){
                return(slot(x,j)[i])   
              }
            }
            df <- as.data.frame(x)[i,]
            return (new("Construct", df))         
          } 
) 
################################### Set - Method ###################################################

#' @title Set Methods
#' @name $<- 
#' @aliases $<-,Construct-method 
#' @rdname Set-methods-1
#' @param name Name of parameter to change
#' @param value new Value.
setMethod("$<-","Construct",function(x,name,value) {
  slot(x,name,check=TRUE) <- value
  valid<-validObject(x)
  return(x)
})

#' @title Set Methods
#' @name [<-
#' @aliases [<-,Construct-method 
#' @rdname Set-methods-2
#' @param x The \code{\ref{Construct}}
#' @param i row-Index
#' @param j Name or Column index
setReplaceMethod("[",signature(x="Construct"),
                 function(x,i,j,value){
                   N <- length(x)
                   if(min(i) <=0) stop("Index i out of bound. It must be positive non zero.")
                   if(max(i) >N)  stop("Index i out of bound. It must not be larger than the total length.")
                   if(length(value)!=length(i)) stop("The replacement length is not equal to the number of elements to be replaced.")    
                   
                   if(is(value, "Construct")){
                     
                     # copy original values to tmp vectors
                     tmp <- as.data.frame(x) 
                     #replace the item(s) regarding to i
                     tmp$assattr[i]    <- value@assattr
                     tmp$label[i] <- value@label 
                     tmp$rpole[i]     <- value@rpole
                     tmp$lpole[i]     <- value@lpole
                     
                     # return new item
                     return (new("assattr", tmp)) 
                   } else {
                     if(missing(j)) {
                       # call the function recursivly
                       # might throw an error if the conversion is not supported.
                       x[i] <- as.Construct(value)
                     }else{ 
                       if(class(j) == "character"){
                         slot(x,j)[i]     <- value
                       }else if(class(j) == "integer" | class(j) == "numeric"){ 
                         slot(x,slotNames(x)[j])[i]  <- value   
                       }else{
                         warning(paste("no changes have been made. the provided value of j (class:",class(j), "value:",j,") was not recocnized."))
                       }
                     }
                     valid<-validObject(x)
                     return(x) 
                   }
                 } 
)

################################### as... - Method ###################################################
#local Methods
as.Construct.list       = function(x, ...){return(new("Construct", data=x))}
as.Construct.data.frame = function(x, ...){return(new("Construct", data=x))}
#local convert message
as.data.frame.Construct = function(x, ...){
  li<-list(...)
  if(is.null(li$withrownames)) li$withrownames <- FALSE
  df<-data.frame(assattr=x@assatrr, label = x@label, rpole= x@rpole, lpole= x@lpole)
  if(li$withrownames) rownames(df)<-x@label
  return (df)
}
as.list.Construct = function(x, ...){list(assattr=x@assatrr, label = x@label, rpole= x@rpole, lpole= x@lpole)}


#' @export
setGeneric("as.Construct",       function(x, ...) standardGeneric( "as.Construct")) 

#' @title Convert Objects to Constructs
#' @name as.Construct  
#' @param x an  Object to convert.
#' @param ... optional parameters
#' @aliases as.Construct,list-method 
#' @rdname as.Construct-methods 
setMethod("as.Construct",     signature(x = "list"),       as.Construct.list) 

#' @title Convert Objects to Constructs
#' @name as.Construct
#' @aliases as.Construct,data.frame-method 
#' @rdname as.Construct-methods 
setMethod("as.Construct",    signature(x = "data.frame"),  as.Construct.data.frame)  

#' @title Convert Objects to lists
#' @name as.list
#' @param x the Construct  
#' @aliases as.list,Construct-method 
#' @rdname as.list-methods 
setMethod("as.list",        signature(x = "Construct"),        as.list.Construct) 

#' @title Convert Objects to data.frames
#' @name as.data.frame
#' @param x the Construct  
#' @aliases as.data.frame,Construct-method 
#' @rdname as.data.frame-methods 
setMethod("as.data.frame",  signature(x = "Construct"),        as.data.frame.Construct) 

setAs("data.frame", "Construct", def=function(from){return(as.Construct.data.frame(from))})
setAs("list", "Construct", def=function(from){return(as.Construct.list(from))})
################################### is... - Method ###################################################

#' @title Checks if an Object is as Construct
#' @export
#' @name is.Construct
#' @param x the Construct   
#' @param ... optional Parameters (not supported)
setGeneric("is.Construct",       function(x, ...) standardGeneric( "is.Construct")) 

#' @aliases is.Construct,Construct-method 
#' @rdname is.Construct
setMethod( "is.Construct", "Construct", function(x, ...){ 
  return(is(x ,"Construct")) 
})


#' @title How many Constructs are included?
#' @name length
#' @param x the Construct
#' @aliases length,Construct-method 
#' @rdname length 
setMethod("length", "Construct",
          function(x){
            N <- length(x@label)
            return(N)
          } 
)
setMethod(f="range", signature="Construct", definition=function( x, na.rm=FALSE) { 
  m<-matrix(rep(NA, 2*length(x)), ncol=2)
  for(i in 1:length(x)){
    m[i,] <- range(x$ratings[i,])
  }
  return (m)
})


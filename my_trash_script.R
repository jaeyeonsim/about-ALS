f <- function(x=AlsHistory$Site_of_Onset___Bulbar){
 if(x!=1)
   return(0)
  else{
    return("Bulbar")
  }
}
AlsHistory$a1 <- sapply(AlsHistory$Site_of_Onset___Bulbar, f)
head(AlsHistory$a1)

f2 <- function(x=AlsHistory$Site_of_Onset___Limb){
  if(x!=1)
return(0)
  else{
    return("Limb")
  }
}
AlsHistory$a2 <- sapply(AlsHistory$Site_of_Onset___Limb, f2)

f3 <- function(x=AlsHistory$Site_of_Onset___Limb_and_Bulbar){
  if(x!=1)
    return(0)
  else{
    return("Limb and Bulbar")
  }
}
AlsHistory$a3 <- sapply(AlsHistory$Site_of_Onset___Limb_and_Bulbar, f3)

f4 <- function(x=AlsHistory$Site_of_Onset___Other){
  if(x!=1)
return(0)
  else{
    return("Other")
  }
}
AlsHistory$a4 <- sapply(AlsHistory$Site_of_Onset___Other, f4)

AlsHistory$a5 <- sapply(AlsHistory$Site_of_Onset___Spine, f2)

AlsHistory$a6 <- gsub("Onset: ", "", AlsHistory$Site_of_Onset)
# combine a1~a6
AlsHistory$SiteofOnset <-ifelse(!is.na(AlsHistory$a1), AlsHistory$a1, ifelse(
  !is.na(AlsHistory$a2), AlsHistory$a2, ifelse(
    !is.na(AlsHistory$a3), AlsHistory$a3, ifelse(
      !is.na(AlsHistory$a4), AlsHistory$a4, ifelse(
        !is.na(AlsHistory$a5), AlsHistory$a5, ifelse(
          !is.na(AlsHistory$a6), AlsHistory$a6
        )
      )
    )
  )
) )
head(AlsHistory$SiteofOnset)
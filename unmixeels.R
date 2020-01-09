dyn.load("libdmformat/libdmformat.so")
dyn.load("my_gaussian_1.so")
library(R6)
#library(abind)

#_begin_onload
  options('unmixeels.verbose'	= T )
#_end_onload


#_begin_test

#_end_test

ThreeDimData<-R6Class( "ThreeDimData",
  public = list(
    set_data = function(array_in) {
      private$.data <- array_in
      invisible(self)
    },
    get_dims = function(){
      if (is.vector(private$.data)) {
        out <- length(private$.data)
      } else {
        out <- dim(private$.data)
      }
      out
    },
    get_num_dims = function() {
      length(self$get_dims())
    },
    get_data = function() {
      private$.data
    },
    get_data_with_dims = function(dims) {
      array(private$.data,dim=dims)
    },
    # r is using column major! be cautious! 
    get_1d_data = function() {
      c(private$.data)
    },
    get_2d_data = function(dims=NA) {
      out<-NULL
      if (is.na(dims)) {
        dims<-self$get_dims()
      } 
      self$get_data_with_dims(c(prod(dims[-length(dims)]),dims[length(dims)]))
    },
    convert_index = function(index_in) {
      self$convert_index_to_n(self$convert_index_to_one(index_in))
    },
    convert_index_to_one = function(index_in) {
      index_out <- 1
      index_in  <- index_in-rep(1,length(index_in))
      nrows<-1 ; dim_arr<-self$get_dims()
      for (i in 1:(length(dim_arr))) {
        index_out<-index_out+index_in[i]*nrows
        nrows<-nrows*dim_arr[i]
      }
      index_out
    },
    convert_index_to_n = function(index_in) {
      dim_arr<-self$get_dims()
      index_out <- rep(NA, length(dim_arr))
      nrows<-prod(dim_arr)
      res<-index_in-1
      for (i in length(dim_arr):1) {
        nrows<-nrows/dim_arr[i]
        index_out[i]<-res%/%nrows
        res<-res%%nrows
      }
      index_out <- index_out + rep(1,length(index_out))
      index_out
    },
    data_binning_2d = function( nbin, along ) {
      if (self$get_num_dims() != 2) {
        message("Error: data_binning_2d() dim is not 2")
      }
      data<-private$.data
      n<-nbin
      if (along == 2) {
        data<-t(data)
      } else if (along > 2) {
        message("Error: data_binning_2d(): along should be 1 or 2")
      }
      new_x_len <-floor(dim(data)[1]/nbin) 
      out <- array(NA, dim=c(new_x_len, dim(data)[2]))
      for (i in 1:new_x_len) {
        out[i,]<-n*colMeans(data[((i-1)*n+1):(n*i),,drop=F])
      }
      if (along == 2) {
        out <-t(out)
      }
      out
###################
    },
    data_binning_3d = function(nbin,along) {
      if (self$get_num_dims() != 3) {
        message("Error: data_binning_3d() dim is not 3")
      }
      data<-private$.data
      n<-nbin
      if (along == 2) {
        data<-aperm(data, c(2,1,3))
      } else if (along == 3) {
        data<-aperm(data, c(3,2,1))
      } else if (along > 3 ) {
        message("Error: data_binning_3d(): along should be 1, 2 or 3")
      }
      new_x_len <-floor(dim(data)[1]/nbin) 
      out <- array(NA, dim=c(new_x_len, dim(data)[2:3]))
      for (i in 1:new_x_len) {
        out[i,,]<-n*colMeans(data[((i-1)*n+1):(n*i),,,drop=F])
      }
      if (along == 2) {
        out <- aperm(out, c(2,1,3))
      } else if (along == 3 ) {
        out <- aperm(out, c(3,2,1))
      }
      out
    },
    data_binning_1d = function(nbin) {
      if (self$get_num_dims() != 1 ) {
        message("Error: data_binning_1d() dim is not 1")
      }
      data<-private$.data
      n<-nbin
      new_x_len <-floor(length(data)/nbin) 
      out <- rep(NA, new_x_len)
      for (i in 1:new_x_len) {
        out[i]<-n*mean(data[((i-1)*n+1):(n*i)])
      }
      out
    },
    initialize = function(array_in) {
      self$set_data(array_in)
      invisible(self)
    }
  ), #EOF public
  private = list(
    .data = NULL
  )
)


#_begin_Rlibdmformat

SIorImg<-R6Class( "SIorImg",
  public = list(
# TODO: NEED SOME FORMAT CHECK HERE
    set_numberOfDims = function(value) {
      private$.numberOfDims <- value
      invisible(self)
    }, 
    set_calib = function(value) {
      private$.calib <- value
      invisible(self)
    },
    set_Cnt = function(value) {
      private$.Cnt <- value
      invisible(self)
    },
    set_SIorImg = function(numberOfDims,calib,Cnt) {
      self$set_numberOfDims(numberOfDims)
      self$set_calib(calib)
      self$set_Cnt(Cnt)
      if (length(dim(Cnt))!=numberOfDims) {
        message(sprintf("Warn: dimension of Cnt is %d, but dimension of numberOfDims is %d. Set anyway...", length(dim(Cnt)), numberOfDims))
      }
      invisible(self)
    }, 
    # type is one of ("pct","px","unit"), 
    # with pct between 0-1, px starts from 1
    # and unit starts from 0 (nm)
    convert_to_index = function(x_n, n, type) {
      if (private$.numberOfDims < n) {
        message(sprintf("Error: Dim specified is %d but dim of data is %d", n, private$.numberOfDims))
      } 
      if (type == "pct") {
        x_n_out <- round(dim(private$.Cnt)[n]*x_n)
      } else if (type == "unit-absolute") {
        x_n_out <- round(x_n/(private$.calib$Scale[n])+private$.calib$Origin[n])
      } else if (type == "unit-relative") {
        x_n_out <- round(x_n/(private$.calib$Scale[n]))
      } else if (type == "px") {
        x_n_out <- x_n
      } else {
        message("Error: crop type is wrong! Must be one of (\"pct\",\"px\",\"unit\")")
      }
      if (x_n_out > dim(private$.Cnt)[n] ) {
        x_n_out <- dim(private$.Cnt)[n]
      } else if (x_n_out < 1 ) {
        x_n_out <- 1
      }
      x_n_out
    }
  ),
  private = list(
    .numberOfDims	= 0,
    .calib		= NULL,
    .Cnt		= NA
  ),
  active = list(
    numberOfDims = function(value){
      if (missing(value)) {
        private$.numberOfDims
      } else {
        message("Warn: You are changing numberOfDims")
        private$.numberOfDims<-value
      }
    },
    calib = function(value) {
      if (missing(value)) {
        private$.calib
      } else {
        message("Warn: You are changing calib")
        private$.calib<-value
      }
    },
    Cnt = function(value) {
      if (missing(value)) {
        private$.Cnt
      } else {
        message("Warn: You are changing Cnt")
        private$.Cnt<-value
      } # EOF if missing value
    } # EOF Cnt
  ) # EOF active
) # EOF SIorImg



OrigSI<-R6Class ("OrigSI", 
  inherit = SIorImg,
  public = list(
    set_E = function(value){
      private$.E<-value
      invisible(self)
    }, 
    set_SI = function(numberOfDims,calib,E,Cnt) {
      self$set_SIorImg(numberOfDims,calib,Cnt)
      self$set_E(E)
      invisible(self)
    }, 
    convert_2d_to_3d = function() {
      if (private$.numberOfDims!=2) {
        message(sprintf("Error: Original numberOfDims is %d, cannot convert to 3D SI", private$.numberOfDims))
      } else {
        private$.numberOfDims <- 3
        private$.Cnt<-array(private$.Cnt,dim=c(dim(private$.Cnt),1))
        private$.Cnt<-aperm(private$.Cnt,c(2,3,1))
        private$.calib$Origin[1:3]<-private$.calib$Origin[c(2,2,1)]
        private$.calib$Scale[1:3] <-private$.calib$Scale[c(2,2,1)]
        private$.calib$Units[1:3] <-private$.calib$Units[c(2,2,1)]
      }
      invisible(self)
    },
    convert_to_3d = function() {
      if (private$.numberOfDims==2) {
        self$convert_2d_to_3d()
      } else if (private$.numberOfDims!=3) {
        message(sprintf("Error: numberOfDims is %d, cannot convert to 3d.", private$.numberOfDims))
      }
      invisible(self)
    },
    convert_to_3d_with_warn = function() {
      if (private$.numberOfDims!=3) {
        message(sprintf("Warn: Cnt is not in 3D. Converting..."))
        self$convert_to_3d()
      }
      invisible(self)
    },
    gen_EFImg_from_3d = function(e0, e1, type){
      self$convert_to_3d_with_warn()
      EFImg<-OrigADF$new()
      ind_l<-self$convert_to_index(e0, 3, type)
      ind_h<-self$convert_to_index(e1, 3, type)
      # ind_l<-which.min(abs(private$.E - EFELow))
      # ind_h<-which.min(abs(private$.E - EFEHigh))
      EFImg_Cnt<-(ind_h-ind_l+1)*rowMeans(private$.Cnt[,,ind_l:ind_h,drop=F],na.rm=T,dims=2)
      EFImg$set_ADF(2,private$.calib[-(private$.numberOfDims),],EFImg_Cnt)
      EFImg
    },
    transpose_xy = function() {
      self$convert_to_3d_with_warn()
      private$.Cnt<-aperm(private$.Cnt,c(2,1,3))
      private$.calib<-private$.calib[c(2,1,3),]
      invisible(self)
    },
    flip_xy = function(flip_image = '') {
      self$convert_to_3d_with_warn()
      if (flip_image == 'x') {
        private$.Cnt<-private$.Cnt[dim(private$.Cnt)[1]:1,,,drop=F]
      } else if (flip_image == 'y') {
        private$.Cnt<-private$.Cnt[,dim(private$.Cnt)[2]:1,,drop=F]
      } else if (flip_image == 'xy') {
        private$.Cnt<-private$.Cnt[dim(private$.Cnt)[1]:1,,,drop=F]
        private$.Cnt<-private$.Cnt[,dim(private$.Cnt)[2]:1,,drop=F]
      } else {
        message(sprintf('Warn: flip_xy should be "x", "y" or "xy", however %s is provided', flip_image))
      }
      invisible(self)
    },
    crop_xy = function(x0,x1,y0,y1,type) {
      self$convert_to_3d_with_warn()
      x0<-self$convert_to_index(x0, 1, type)
      x1<-self$convert_to_index(x1, 1, type)
      y0<-self$convert_to_index(y0, 2, type)
      y1<-self$convert_to_index(y1, 2, type)
      private$.Cnt<-private$.Cnt[x0:x1,y0:y1,,drop=F]
      private$.calib$Origin[1]<- private$.calib$Origin[1] - (x0-1)
      private$.calib$Origin[2]<- private$.calib$Origin[2] - (x0-1)
      invisible(self)
    },
    crop_e = function(e0, e1, type) {
      self$convert_to_3d_with_warn()
      e0<-self$convert_to_index(e0, 3, type)
      e1<-self$convert_to_index(e1, 3, type)
      private$.E<-private$.E[e0:e1]
      private$.Cnt<-private$.Cnt[,,e0:e1,drop=F]
      private$.calib$Origin[3]<-private$.calib$Origin[3] - (e0-1)
      invisible(self)
    },
    shift_e = function(eshift_approx = 0) {
      self$convert_to_3d_with_warn()
      energyInterval<-private$.calib$Scale[private$.numberOfDims]
      eshift<-round((eshift_approx)/energyInterval)*energyInterval
      private$.E<-private$.E + eshift
      private$.calib$Origin[3]<-private$.calib$Origin[3]-eshift/private$.calib$Scale[3]
      invisible(self)
    },
    # along is one of 'x' 'y' or 'e'
    binning_3d = function (nbin, along) {
     if (nbin!=1) {
      if (along == 'x') {
        along <- 1
      } else if (along == 'y') {
        along <- 2
      } else if (along == 'e') {
        along <- 3
      } else {
        message("Error: binning_3d(): along must be one of 'x' 'y' or 'e'")
      }
      self$convert_to_3d_with_warn()
      self$set_Cnt(ThreeDimData$new(private$.Cnt)$data_binning_3d(nbin, along))
      if (along == 3) {
        self$set_E(ThreeDimData$new(private$.E)$data_binning_1d(nbin)/nbin)
      }
      calib<-private$.calib
      calib$Scale[along] <- nbin * calib$Scale[along]
      calib$Origin[along] <- calib$Origin[along]/nbin
      self$set_calib(calib)
     }
     invisible(self)
    },
    # to_be_reduced is one of 'x', 'y', 'xy'
    SI_3d_to_2d_or_pt = function(to_be_reduced) {
      if (to_be_reduced == 'x') {
        private$.Cnt<-array(dim(private$.Cnt)[1]*
          colMeans(private$.Cnt,dims=1,na.rm=T), dim=c(1, dim(private$.Cnt)[2:3]))
      } else if (to_be_reduced == 'xy' ) {
        private$.Cnt<-array(dim(private$.Cnt)[1]*dim(private$.Cnt)[2]*
          colMeans(private$.Cnt,dims=2,na.rm=T), dim=c(1, 1, dim(private$.Cnt)[3]))
      } else if (to_be_reduced == 'y') {
        private$.Cnt<-array(dim(private$.Cnt)[2]*
          colMeans(aperm(private$.Cnt, c(2,1,3)), dims=1, na.rm=T), dim=c(dim(private$.Cnt)[1],1,dim(private$.Cnt)[3]))
      } else {
        message("Error: to_be_reduced is not one of 'x', 'y' or 'xy'.")
      }
      invisible(self)
    }, 
    set_SI_from_EELS_file = function(filename){
	DM_filemap <-  .C("DM_file_mmap", addr = raw(8), filename=filename, fd = integer(1))
	if (sum(DM_filemap$addr != raw(8)) == 0) {
		return(NULL)
	}
	dm_version<-.C("DM_test_version_R", DM_filemap$addr, version=integer(1))$version
	message(sprintf("DMversion is %d.", dm_version))
	if (dm_version==4) {
		R_DM4_create_root<-.C("R_DM4_create_root", root = raw(8), DM_file_header = DM_filemap$addr)
		DM_get_image1<-.C("DM4_get_image1", root = R_DM4_create_root$root, with_name="EELS", Origin = double(4), Scale = double(4), dimensions = integer(8), dimension_len = integer(1), data_length = integer(1), data_type = integer(1))

		if (DM_get_image1$data_type == 7) {
			DM_get_image2<-.C("DM4_get_image2", root = R_DM4_create_root$root, with_name="EELS", Units = rep(paste(rep(" ",8),collapse=''), DM_get_image1$dimension_len), dimension_len = integer(1), data = double(DM_get_image1$data_length)) 
		} else if (DM_get_image1$data_type == 6){
			DM_get_image2<-.C("DM4_get_image2", root = R_DM4_create_root$root, with_name="EELS", Units = rep(paste(rep(" ",8),collapse=''), DM_get_image1$dimension_len), dimension_len = integer(1), data = single(DM_get_image1$data_length)) } 
	} else if (dm_version==3) {
		R_DM3_create_root<-.C("R_DM3_create_root", root = raw(8), DM_file_header = DM_filemap$addr)
		DM_get_image1<-.C("DM3_get_image1", root = R_DM3_create_root$root, with_name="EELS", Origin = double(4), Scale = double(4), dimensions = integer(8), dimension_len = integer(1), data_length = integer(1), data_type = integer(1))

		if (DM_get_image1$data_type == 7) {
			DM_get_image2<-.C("DM3_get_image2", root = R_DM3_create_root$root, with_name="EELS", Units = rep(paste(rep(" ",8),collapse=''), DM_get_image1$dimension_len), dimension_len = integer(1), data = double(DM_get_image1$data_length)) 
		} else if (DM_get_image1$data_type == 6){
			DM_get_image2<-.C("DM3_get_image2", root = R_DM3_create_root$root, with_name="EELS", Units = rep(paste(rep(" ",8),collapse=''), DM_get_image1$dimension_len), dimension_len = integer(1), data = single(DM_get_image1$data_length)) } 
	} else {
		message(sprintf("DMversion %d is not supported", dm_version))
		return(NULL)
	}
	DM_filemunmap<-.C("DM_file_munmap", DM_filemap$addr,  DM_filemap$fd)

#	origSpec<-NULL
	origSpec_numberOfDims<-DM_get_image1$dimension_len
#	origSpec_calib<-as.list(rep(NA,3))
#	names(origSpec_calib)<-c("Origin","Scale","Units")
	origSpec_calib<-data.frame(Origin=NA,Scale=NA,Units=rep(NA,origSpec_numberOfDims))
	origSpec_calib$Origin<-DM_get_image1$Origin[1:(origSpec_numberOfDims)]
	origSpec_calib$Scale<-DM_get_image1$Scale[1:(origSpec_numberOfDims)]
	origSpec_calib$Units<-DM_get_image2$Units[1:(origSpec_numberOfDims)]
	dim_arr<-DM_get_image1$dimensions[2*(1:(origSpec_numberOfDims))-1]
	for (i in 1:length(origSpec_calib$Units)) {
#		if (origSpec$calib$Units[[i]]=="eV")
		origSpec_E<-seq(from=-origSpec_calib$Scale[[i]]*origSpec_calib$Origin[[i]],by=origSpec_calib$Scale[[i]],length.out=dim_arr[i])
	}
	if (origSpec_calib$Units[[i]]!="eV") {
#	if (is.null(origSpec_E)) {
		message(sprintf("Warn: Unit of %dnd dimension is not eV, but %s", origSpec_numberOfDims, origSpec_calib$Units[[origSpec$numberOfDims]]))
	}
	origSpec_Cnt<-array(data=DM_get_image2$data,dim=dim_arr)
	self$set_SI(origSpec_numberOfDims,origSpec_calib,origSpec_E,origSpec_Cnt) 
        invisible(self)
    } 
  ),
  private = list(
    .E = NA
  ),
  active = list(
    E = function(value) {
      if (missing(value)) {
        private$.E
      } else {
        message("Warn: You are changing E")
        private$.E<-value
      }
    }
  ) # EOF active
) # EOF OrigSI



OrigADF<-R6Class ("OrigADF", 
  inherit = SIorImg,
  public = list(
    set_ADF = function(numberOfDims,calib,Cnt) {
      self$set_SIorImg(numberOfDims,calib,Cnt)
      invisible(self)
    }, 
    convert_1d_to_2d = function() {
      if (private$.numberOfDims!=1) {
        message(sprintf("Error: Original numberOfDims is %d, cannot convert to 2D image", private$.numberOfDims))
      } else {
        private$.numberOfDims <- 2
        private$.Cnt<-array(private$.Cnt,dim=c(dim(private$.Cnt),1))
        #private$.Cnt<-aperm(private$.Cnt,c(1,2))
        private$.calib$Origin[1:2]<-private$.calib$Origin[c(1,1)]
        private$.calib$Scale[1:2] <-private$.calib$Scale[c(1,1)]
        private$.calib$Units[1:2] <-private$.calib$Units[c(1,1)]
      }
      invisible(self)
    },
    convert_to_2d = function() {
      if (private$.numberOfDims==1) {
        self$convert_1d_to_2d()
      } else if (private$.numberOfDims!=2) {
        message(sprintf("Error: numberOfDims is %d, cannot convert to 2d.", private$.numberOfDims))
      }
      invisible(self)
    },
    convert_to_2d_with_warn = function() {
      if (private$.numberOfDims!=2) {
        message(sprintf("Warn: Cnt is not in 2D. Converting..."))
        self$convert_to_2d()
      }
      invisible(self)
    },
    transpose_xy = function() {
      self$convert_to_2d_with_warn()
      private$.Cnt<-aperm(private$.Cnt,c(2,1))
      private$.calib<-private$.calib[c(2,1),]
      invisible(self)
    },
    flip_xy = function(flip_image = '') {
      self$convert_to_2d_with_warn()
      if (flip_image == 'x') {
        private$.Cnt<-private$.Cnt[dim(private$.Cnt)[1]:1,,drop=F]
      } else if (flip_image == 'y') {
        private$.Cnt<-private$.Cnt[,dim(private$.Cnt)[2]:1,drop=F]
      } else if (flip_image == 'xy') {
        private$.Cnt<-private$.Cnt[dim(private$.Cnt)[1]:1,,drop=F]
        private$.Cnt<-private$.Cnt[,dim(private$.Cnt)[2]:1,drop=F]
      }
      invisible(self)
    },
    # type is one of ("pct","px","unit"), 
    # with pct between 0-1, px starts from 1
    # and unit starts from 0 (nm)
    crop_xy = function(x0,x1,y0,y1,type) {
      self$convert_to_2d_with_warn()
      x0<-self$convert_to_index(x0, 1, type)
      x1<-self$convert_to_index(x1, 1, type)
      y0<-self$convert_to_index(y0, 2, type)
      y1<-self$convert_to_index(y1, 2, type)
      private$.Cnt<-private$.Cnt[x0:x1,y0:y1,drop=F]
      private$.calib$Origin[1]<- private$.calib$Origin[1] - (x0-1)
      private$.calib$Origin[2]<- private$.calib$Origin[2] - (x0-1)
      invisible(self)
    },
    binning_2d = function (nbin, along) {
     if (nbin != 1) {
      if (along == 'x') {
        along <- 1
      } else if (along == 'y') {
        along <- 2
      } else {
        message("Error: binning_2d(): along must be one of 'x' 'y'")
      }
      self$convert_to_2d_with_warn()
      self$set_Cnt(ThreeDimData$new(private$.Cnt)$data_binning_2d(nbin, along))
      calib<-private$.calib
      calib$Scale[along] <- nbin * calib$Scale[along]
      calib$Origin[along] <- calib$Origin[along]/nbin
      self$set_calib(calib)
     }
      invisible(self)
    },
    plot_ADF = function() {

      calib<-private$.calib
      Cnt<-private$.Cnt
      rulxlab<-seq(from=calib$Scale[1]*(-calib$Origin[1]+.5),by=calib$Scale[1],length.out=dim(Cnt)[1])
      rulylab<-seq(from=calib$Scale[2]*(-calib$Origin[2]+.5),by=calib$Scale[2],length.out=dim(Cnt)[2])

      map_xlim<-c( calib$Scale[1]*(-calib$Origin[1]), calib$Scale[1]*(-calib$Origin[1]+dim(Cnt)[1]) )
      map_ylim<-c( calib$Scale[2]*(-calib$Origin[2]), calib$Scale[2]*(-calib$Origin[2]+dim(Cnt)[2]) )

      str_xlab<-sprintf("x/%s",calib$Units[1])
      str_ylab<-sprintf("y/%s",calib$Units[2])

      image(rulxlab, rulylab, Cnt , col=gray((0:2^16)/2^16), 
        xlab=str_xlab,ylab=str_ylab,xlim=map_xlim, ylim=map_ylim, asp=1)
      invisible(self)
    },
    set_ADF_from_ADF_file = function(adf_filename,with_si_filename){
	withname<-with_si_filename ; filename<-adf_filename
	DM_filemap <-  .C("DM_file_mmap", addr = raw(8), filename=filename, fd = integer(1))
	if (sum(DM_filemap$addr != raw(8)) == 0) {
		return(NULL)
	}
	dm_version<-.C("DM_test_version_R", DM_filemap$addr, version=integer(1))$version
	message(sprintf("DMversion is %d.", dm_version))
	if (dm_version==4) {
		R_DM4_create_root<-.C("R_DM4_create_root", root = raw(8), DM_file_header = DM_filemap$addr)
		DM_get_image1<-.C("DM4_get_image1", root = R_DM4_create_root$root, with_name=withname, Origin = double(4), Scale = double(4), dimensions = integer(8), dimension_len = integer(1), data_length = integer(1), data_type = integer(1))

		if (DM_get_image1$data_type == 7) { #double
			DM_get_image2<-.C("DM4_get_image2", root = R_DM4_create_root$root, with_name=withname, Units = rep(paste(rep(" ",8),collapse=''), DM_get_image1$dimension_len), dimension_len = integer(1), data = double(DM_get_image1$data_length)) 
		} else if (DM_get_image1$data_type == 6) { # single
			DM_get_image2<-.C("DM4_get_image2", root = R_DM4_create_root$root, with_name=withname, Units = rep(paste(rep(" ",8),collapse=''), DM_get_image1$dimension_len), dimension_len = integer(1), data = single(DM_get_image1$data_length)) 
		} else if (DM_get_image1$data_type == 5) { # ULONG
			DM_get_image2<-.C("DM4_get_image2", root = R_DM4_create_root$root, with_name=withname, Units = rep(paste(rep(" ",8),collapse=''), DM_get_image1$dimension_len), dimension_len = integer(1), data = integer(DM_get_image1$data_length)) 
			message(sprintf("data_type ULONG, but using signed int32"))
		} else if (DM_get_image1$data_type == 3) { # LONG
			DM_get_image2<-.C("DM4_get_image2", root = R_DM4_create_root$root, with_name=withname, Units = rep(paste(rep(" ",8),collapse=''), DM_get_image1$dimension_len), dimension_len = integer(1), data = integer(DM_get_image1$data_length)) 
		} else {
			message(sprintf("data_type %d is not implemented in getADFData!", DM_get_image1$data_type))
		}	
	}  else if (dm_version==3) {
		R_DM3_create_root<-.C("R_DM3_create_root", root = raw(8), DM_file_header = DM_filemap$addr)
		# 4 is enough. 8 is for int64
		DM_get_image1<-.C("DM3_get_image1", root = R_DM3_create_root$root, with_name=withname, Origin = double(4), Scale = double(4), dimensions = integer(8), dimension_len = integer(1), data_length = integer(1), data_type = integer(1))

		if (DM_get_image1$data_type == 7) {
			DM_get_image2<-.C("DM3_get_image2", root = R_DM3_create_root$root, with_name=withname, Units = rep(paste(rep(" ",8),collapse=''), DM_get_image1$dimension_len), dimension_len = integer(1), data = double(DM_get_image1$data_length)) 
		} else if (DM_get_image1$data_type == 6){
			DM_get_image2<-.C("DM3_get_image2", root = R_DM3_create_root$root, with_name=withname, Units = rep(paste(rep(" ",8),collapse=''), DM_get_image1$dimension_len), dimension_len = integer(1), data = single(DM_get_image1$data_length)) 
		} else if (DM_get_image1$data_type == 5) { # ULONG
			DM_get_image2<-.C("DM3_get_image2", root = R_DM3_create_root$root, with_name=withname, Units = rep(paste(rep(" ",8),collapse=''), DM_get_image1$dimension_len), dimension_len = integer(1), data = integer(DM_get_image1$data_length)) 
			message(sprintf("data_type ULONG, but using signed int32"))
		} else if (DM_get_image1$data_type == 3) { # LONG
			DM_get_image2<-.C("DM3_get_image2", root = R_DM3_create_root$root, with_name=withname, Units = rep(paste(rep(" ",8),collapse=''), DM_get_image1$dimension_len), dimension_len = integer(1), data = integer(DM_get_image1$data_length)) 
		} else {
			message(sprintf("Error: data_type %d is not implemented in getADFData!", M_get_image1$data_type))
		}
	} else {
		message(sprintf("Error: DMversion %d is not supported", dm_version))
		return(NULL)
	}
	DM_filemunmap<-.C("DM_file_munmap", DM_filemap$addr,  DM_filemap$fd)

#	ADFData<-NULL
	ADFData_numberOfDims<-DM_get_image1$dimension_len
#	ADFData_calib<-as.list(rep(NA,3))
#	names(ADFData_calib)<-c("Origin","Scale","Units")
	ADFData_calib<-data.frame(Origin=NA,Scale=NA,Units=rep(NA,ADFData_numberOfDims))
	ADFData_calib$Origin<-DM_get_image1$Origin[1:(ADFData_numberOfDims)]
	ADFData_calib$Scale<-DM_get_image1$Scale[1:(ADFData_numberOfDims)]
	ADFData_calib$Units<-DM_get_image2$Units[1:(ADFData_numberOfDims)]
        # little endian?
	dim_arr<-DM_get_image1$dimensions[2*(1:(ADFData_numberOfDims))-1]
	ADFData_Cnt<-array(data=DM_get_image2$data,dim=dim_arr)

	self$set_SIorImg(ADFData_numberOfDims,ADFData_calib,ADFData_Cnt) 
        invisible(self)
    } # EOF set_ADF_from_ADF_file
  ) # EOF public
) # EOF OrigADF

#_end_Rlibdmformat


#_begin_libeelsproc

# In your EELS experiment, right before or after each EELS acquisition, 
# immediately lower the screen and take another EELS SI using the same 
# setup as a dark correction. The number of pixels can be much smaller, 
# Square root of original is sufficient. 
# This would be a better "dark reference" if the 
# vendor's EELS software is not doing well in getting a dark reference.  
# This function returns a vector with n-channel elements representing 
# the "better" dark reference to be subtracted from your data. 

SpikeNoise<-R6Class ("SpikeNoise",
  public = list(
    set_params = function(n_sigma_1, n_sigma_2, half_max_channels, channelRes, smthWindow) {
      private$.n_sigma_1 <- n_sigma_1
      private$.n_sigma_2 <- n_sigma_2
      private$.half_max_channels <- half_max_channels
      private$.channelRes <- channelRes
      private$.smthWindow <- smthWindow
      invisible(self)
    },
    initialize = function(n_sigma_1 = 6, n_sigma_2 = 3, half_max_channels = 5, channelRes = 1, smthWindow = 40) {
      self$set_params(n_sigma_1, n_sigma_2, half_max_channels, channelRes, smthWindow)
      invisible(self)
    },
    set_spec_with_xray = function(spec_with_xray) {
      private$.spec_with_xray = spec_with_xray
      invisible(self)
    },
    get_spec_no_xray = function() {
      private$.spec_no_xray
    },
    get_spec_is_modified = function() {
      private$.spec_is_modified
    },

    # This function do a one-time search 
    # for spike noise, by comparing one pixel 
    # with others, and looping over all channels. 
    # Useful for detecting spike noise in dark reference. 
    # input is 2D data, 1st dim is pixels, 2nd is channels
    find_spike_list_1 = function(two_dim_data) {
      n_sigma<-private$.n_sigma_1
      meanDkIntensity<-colMeans(two_dim_data,na.rm=T)
      sdDkIntensity<-sapply(1:(dim(two_dim_data)[2]),function(X){sd(two_dim_data[,X],na.rm=T)})
      #plot(sdDkIntensity,type="l")
      #plot(meanDkIntensity,type="l")
      # find out all 
      vca_N<-nrow(two_dim_data)
      vca_L<-length(meanDkIntensity)
#      XRayListDkIndices<-which(abs(two_dim_data-t(matrix(meanDkIntensity,nrow=vca_L,ncol=vca_N)))
#          >n_sigma*t(matrix(sdDkIntensity,nrow=vca_L,ncol=vca_N)),arr.ind=TRUE)
      XRayListDkIndices<-which(two_dim_data-t(matrix(meanDkIntensity,nrow=vca_L,ncol=vca_N))
          >n_sigma*t(matrix(sdDkIntensity,nrow=vca_L,ncol=vca_N)),arr.ind=TRUE)
      XRayListDkIndices
    },

    # remove [index_e-half_max_channels:index_e+half_max_channels] 
    # channels as spike nose and replace by NA. 
    # loop all indexed spike noise
    remove_spike_1 = function(two_dim_data, XRayListDkIndices) {
    	half_max_channels<-private$.half_max_channels
      	if (length(XRayListDkIndices)!=0) {
		# mark [-10:10] points near the spike as NA and calc sd() again. 
#		XRayListDkIndices<-rbind(XRayListDkIndices_old,XRayListDkIndices)
#		XRayListDkIndices_old<-XRayListDkIndices
		for (i in 1:(dim(XRayListDkIndices)[1])) {
			index_pixel<-XRayListDkIndices[i,1]
			index_e<-XRayListDkIndices[i,2]
#print(two_dim_data[index_pixel, (max(1,(index_e-half_max_channels))-3):(3+min(dim(two_dim_data)[2],(index_e+half_max_channels)))])
			two_dim_data[index_pixel, max(1,(index_e-half_max_channels)):min(dim(two_dim_data)[2],(index_e+half_max_channels))]<- NA
		}
	}
	two_dim_data
    },
    do_convert_spec_1 = function() {
      three_dim_arr<-private$.spec_with_xray$Cnt
      two_dim_arr<-ThreeDimData$new(three_dim_arr)$get_2d_data()
      XRayListDkIndices<-self$find_spike_list_1(two_dim_arr)
      while (length(XRayListDkIndices)!=0) {
        two_dim_arr<-self$remove_spike_1(two_dim_arr, XRayListDkIndices)
        XRayListDkIndices<-self$find_spike_list_1(two_dim_arr)
      }
      private$.spec_no_xray<-private$.spec_with_xray$clone()
      private$.spec_no_xray$set_Cnt(two_dim_arr)
      invisible(self)
    },
    my_gaussian = function(x,window_in,alpha_in,tails_in) {
      #return(smth.gaussian(x,window=window_in,alpha=alpha_in,tails=tails_in))
      return(.C("my_gaussian_1", out=double(length(x)), input=as.double(x),
      n=length(x),as.integer(window_in),as.double(alpha_in),as.integer(tails_in))$out)
    },
    # there's a bug so remove first / last 2 e channels
    find_spike_list_init_2d_2 = function(cnt_in_2d) {
      cnt_in <- cnt_in_2d
      smthWindow<-private$.smthWindow
      AlphaInit<-private$.smthWindow/private$.channelRes
      n_sigma_threshold_sq_1pass<-(private$.n_sigma_1)^2
      smth_cnt<-t(sapply(1:dim(cnt_in)[1],function(i){
        self$my_gaussian(cnt_in[i,],window=smthWindow,alpha=AlphaInit, tails=TRUE)}))
      residu_cnt<-cnt_in - smth_cnt
      sample_index<-sample(1:prod(dim(residu_cnt)),min(40000,prod(dim(residu_cnt))))
      c_residu_sq_cnt<-(c(residu_cnt)[sample_index])^2
      c_smth_cnt<-c(smth_cnt)[sample_index]
      # remove 1% biggest because they might be spike noise
      too_large_to_be_sample<-which(c_residu_sq_cnt>quantile(c_residu_sq_cnt,0.99))
      c_residu_sq_cnt<-c_residu_sq_cnt[-too_large_to_be_sample]
      c_smth_cnt<-c_smth_cnt[-too_large_to_be_sample]
      # dark noise is a constant, Poisson noise is prop to count. do a linear fitting 
      noise_fm0<-lm(c_residu_sq_cnt~c_smth_cnt)
      if (getOption('unmixeels.verbose') == T) {
        print("Info: spike noise removal fitting coefficients:")
        print(noise_fm0$coefficients)
      }
      #residu_cnt_pred<-min(0,noise_fm0$coefficients[1])+smth_cnt*min(1,max(0,noise_fm0$coefficients[2]))
      residu_cnt_pred<-array(mean(c_residu_sq_cnt),dim=dim(smth_cnt))
      #expvalidcntlogpredany<-predict(expvalidfm0,expvaliddataany,interval="confidence")
      print("Info: number of spike noise pts: 1 pass")
      print(sum( ( sign(residu_cnt) * (residu_cnt)^2 ) >n_sigma_threshold_sq_1pass*residu_cnt_pred))
      lst_of_potential_bad_pt<-which( ( sign(residu_cnt) * (residu_cnt)^2 ) >n_sigma_threshold_sq_1pass*residu_cnt_pred,arr.ind=T)
      lst_of_potential_bad_pt<-lst_of_potential_bad_pt[
        order(residu_cnt[lst_of_potential_bad_pt]^2,decreasing=T),]
        #print(lst_of_potential_bad_pt)
      return(lst_of_potential_bad_pt)
    },
    remove_spike_in_row_by_l_and_r = function(row, l, r) {
      if (r-l<2) {
        rowout<-row
      } else {
        s <- row[l:r]
        x <- 1:(r-l+1)
        s[2:(r-l)]<-NA
        row[l:r]<-predict(lm(s~x),data.frame(x=x))
        rowout<-row
      }
      rowout
    },
    find_spike_l_and_r_in_row = function(row, lrinit) {
      n_sigma_2<-(private$.n_sigma_2)^2
      l<-lrinit ; r<-lrinit
      smthWindow<-private$.smthWindow
      AlphaInit<-private$.smthWindow/private$.channelRes
      while(1) {
        working_row<-self$remove_spike_in_row_by_l_and_r(row, l , r)
        wrs <- self$my_gaussian(working_row,window=smthWindow,alpha=AlphaInit, tails=TRUE)
        res_row<-(working_row-wrs)^2
        res_row_pred<-rep(mean(res_row), length(res_row))
        is_changed <- F
        if (  (res_row[l]>n_sigma_2*res_row_pred) && (l>1)  ) { l<-l-1 ; is_changed <-T }
        if (  (res_row[r]>n_sigma_2*res_row_pred) && (r<length(res_row))  ) { r<-r+1 ; is_changed <-T }
        if (!is_changed) {break}
      }
      c(l,r)
    },
    calc_spec_is_modified_and_spec_no_xray_2d_2 = function(spec_with_xray, spike_list_init) {
      spec_is_modified<-0*spec_with_xray
      cnt_in <- spec_with_xray
      lst_init<-spike_list_init
      for ( i in seq_len(dim(lst_init)[1]) ) {
      # loop over all rows
        lr<-self$find_spike_l_and_r_in_row(cnt_in[lst_init[i,1],], lst_init[i,2])
        if ((lr[2]-lr[1]-2)>private$.max_channels_warn) {
          message(sprintf("Warn: Spike noise width (r-l-2)>%d (=%d) at px %d ch %d)",private$.max_channels_warn, lr[2]-lr[1], lst_init[i,1],lst_init[i,2]))
        }
        if ( (lr[1]!=lr[2]) && (spec_is_modified[lst_init[i,1],lst_init[i,2]]) ) {
          message(sprintf("Warn: pixel modified more than once at px %d ch %d)", lst_init[i,1],lst_init[i,2]))
        }
        spec_is_modified[lst_init[i,1],lr[1]:lr[2]]<-1
        cnt_in[lst_init[i,1],]<-self$remove_spike_in_row_by_l_and_r(cnt_in[lst_init[i,1],], lr[1], lr[2])
      }
      out<-NULL
      out$spec_is_modified<-spec_is_modified
      out$spec_no_xray<-cnt_in
      out
    },
    neighbor_px_interpolation_3d = function(ave_func,data_3d,spec_is_modified) {
      lst<-private$.protected_chs
      orig_dims<-dim(data_3d)
      if (length(lst)!=orig_dims[3]) {
        message("Error: neighbor_px_interpolation length of lst must be the same as number of channels")
      } 
      lst_map<-colSums(aperm(spec_is_modified,c(3,1,2)) & lst)
      if (getOption('unmixeels.verbose') == T) {
        image(lst_map,zlim=c(0,1))
      }
      for (i in 1:(orig_dims[1])) {
        for (j in 1:(orig_dims[2])) {
          if (lst_map[i,j]) {
      if (getOption('unmixeels.verbose') == T) {
        message(sprintf("Info: using neighbour px to fix spike noise at px %d, %d", i,j))
      }
            l<-max(i-1,1) ; r<-min(i+1,orig_dims[1])
            u<-min(j+1,orig_dims[2]) ; d<-max(j-1,1)
            data_3d[i,j,]<-ave_func(
              (if (lst_map[i,u]) NA else data_3d[i,u,]), 
              (if (lst_map[i,d]) NA else data_3d[i,d,]),
              (if (lst_map[l,j]) NA else data_3d[l,j,]),
              (if (lst_map[r,j]) NA else data_3d[r,j,]))
          }
        }
      }
#      private$.spec_no_xray<-array(data_3d,dim=dim_in_2d)
      spec_no_xray<-data_3d
      spec_no_xray
    },
    # usage: data<- private$.spikeNoise$set_spec_with_xray(SI)$do_convert_spec_2()$neighbor_px_interpolation(function(u,d,l,r){mean(c(u,d,l,r))}, orig_dims, lst)$get_spec_no_xray()
# data.frame(l=c(452,522),h=c(470,560))
    set_protected_chs = function(e_low_and_high) {
      E<-private$.spec_with_xray$E
      private$.protected_chs <- rowSums(sapply(seq_along(e_low_and_high),function(i){(E >= e_low_and_high$l[i]) & E < e_low_and_high$h[i]}))
      invisible(self)
    },
    set_spike_list_init_2d = function() {
      three_dim_arr<-private$.spec_with_xray$Cnt
      two_dim_arr<-ThreeDimData$new(three_dim_arr)$get_2d_data()
      private$.spike_list_init_2d<-self$find_spike_list_init_2d_2(two_dim_arr)
      invisible(self)
    },
    set_spec_is_modified_and_spec_no_xray_3d_2 = function() {
      three_dim_arr<-private$.spec_with_xray$Cnt
      orig_dims<-dim(three_dim_arr)
      two_dim_arr<-ThreeDimData$new(three_dim_arr)$get_2d_data()
      spike_list_init<-private$.spike_list_init_2d
      out<-self$calc_spec_is_modified_and_spec_no_xray_2d_2(two_dim_arr, spike_list_init)

      private$.spec_is_modified<-private$.spec_with_xray$clone()
      private$.spec_no_xray<-private$.spec_with_xray$clone()

      private$.spec_is_modified$set_Cnt(ThreeDimData$new(out$spec_is_modified)$get_data_with_dims(orig_dims))

      private$.spec_no_xray$set_Cnt(ThreeDimData$new(out$spec_no_xray)$get_data_with_dims(orig_dims))
      invisible(self)
    },
    conv_neighbor_px_interpolation = function(ave_func) {
      data_3d<-private$.spec_no_xray$Cnt
      spec_is_modified<-private$.spec_is_modified$Cnt
      private$.spec_no_xray$set_Cnt(self$neighbor_px_interpolation_3d(ave_func,data_3d,spec_is_modified))
      invisible(self)
    },
    do_remove_spike_3d_2 = function() {
      self$set_spike_list_init_2d()
      self$set_spec_is_modified_and_spec_no_xray_3d_2()
      invisible(self)
    }
  ), #EOF public

  private = list(
    .spec_with_xray = NULL,
    .spec_no_xray   = NULL,
    .spec_is_modified = NULL,
    .spike_list = NULL,
    .spike_list_init_2d = NULL,
    .protected_chs = NULL,
    .n_sigma_1 = 6,
    .n_sigma_2 = 3,
    .half_max_channels = 5,
    .max_channels_warn = 5,
    .channelRes = 1,
    .smthWindow = 40
  )
) # EOF SpikeNoise


DarkRef<-R6Class ("DarkRef", 
  public = list(
    set_dkRfSI = function(dkRfSI) {
      private$.dkRfSI<-dkRfSI
      invisible(self)
    },
    set_orig_SI = function(origSI) {
      private$.origSI<-origSI
      invisible(self)
    },
    set_dkRf_scale = function(dkRf_scale) {
      private$.dkRf_scale <- dkRf_scale
      invisible(self)
    }, 
    get_corrected_SI = function() {
      private$.correctedSI
    },
    initialize = function(dkRfSI, dkRf_scale, origSI) {
      self$set_dkRfSI(dkRfSI)
      self$set_orig_SI(origSI)
      self$set_dkRf_scale(dkRf_scale)
      private$.dkRfSI$convert_to_3d_with_warn()
      private$.origSI$convert_to_3d_with_warn()
      invisible(self)
    },
    set_dkRf_intensity = function() {
      private$.dkRfIntensity <- colMeans(ThreeDimData$new(private$.dkRfSI$Cnt)$get_2d_data(),na.rm=T)
      invisible(self)
    },
    get_dark_ref_intensity = function() {
      private$.dkRfIntensity
    },
    remove_dkRf_from_3d_array = function(Cnt_3d_in) {
      Cnt_3d_out<-aperm(aperm(Cnt_3d_in,c(3,1,2))-private$.dkRf_scale*private$.dkRfIntensity,c(2,3,1))
      Cnt_3d_out
    },
    do_remove_dkRf = function() {
      self$set_dkRf_intensity()
      private$.correctedSI<-private$.origSI$clone()
      #private$.SIorigSpecDarkCorrected$convert_to_3d()
      private$.correctedSI$set_Cnt(
        self$remove_dkRf_from_3d_array(private$.origSI$Cnt))
      invisible(self)
    }

  ), # EOF public
  private = list(
#    .dkRfOrigSpec = NULL,
#    .spikeNoise = NULL,
#    .dkRfCleanSpec = NULL,
    .dkRfSI = NULL,
    .dkRfIntensity = NULL,
    .origSI = NULL,
    .correctedSI = NULL,
    .dkRf_scale = 1
  ), # EOF private
#  active = list(   
#  )
) #EOF DarkRef

SIandADF<-R6Class ( "SIandADF", 
  public = list(
    get_SI = function() {
      private$.SI
    },
    get_ADF = function() {
      private$.ADF
    },
    set_SI = function(SI) {
      private$.SI <- SI$convert_to_3d_with_warn()
      invisible(self)
    },
    set_ADF = function(ADF) {
      private$.ADF<-ADF$convert_to_2d_with_warn()
      invisible(self)
    },
    set_SI_and_ADF = function(SI, ADF){
      self$set_SI(SI)
      self$set_ADF(ADF)
      self$check_calib_consistency()
      invisible(self)
    },
    initialize = function(SI,ADF) {
#      private$.SI <- SI$convert_to_3d_with_warn()
#      private$.ADF<- ADF$convert_to_2d_with_warn()
      self$set_SI_and_ADF(SI, ADF)
      invisible(self)
    },
    # this is never used?
    set_calib_SI_and_ADF = function(SI_calib = NA) {
      if (is.na(SI_calib)) {
        SI_calib <- data.frame(Origin=rep(0,3),Scale=rep(1,3),Units=c("px","px","ch"))
      }
      private$.SI$calib<-SI_calib
      private$.ADF$calib<-SI_calib[-nrow(SI_calib),]
      invisible(self)
    },
    check_calib_consistency = function(){ 
      if ( 
        # check if difference in dim is 1
        (private$.SI$numberOfDims - private$.ADF$numberOfDims != 1) || 
        # check if actual data in calib differs by 1D
        (nrow(private$.SI$calib) - nrow(private$.ADF$calib) != 1) ||
        # check if all data are the same except the energy dim
        sum( private$.SI$calib[-(private$.SI$numberOfDims),] != private$.ADF$calib)
      ) {
        message("Error: calib in SI and ADF differs! ")
        message(".SI$calib:")
        print(private$.SI$calib)
        message(".ADF$calib:")
        print(private$.ADF$calib)
        ret<-F
      }  else {
        ret<-T
      }
      ret
    } 
  ),
  private = list(
    .SI = NULL,
    .ADF = NULL
  ) # EOF private
) #EOF SIandADF

ResizeAndRotate<-R6Class ("ResizeAndRotate", 
  inherit = SIandADF,
  public = list(
    transpose_xy = function() {
      private$.SI$transpose_xy()
      private$.ADF$transpose_xy()
      self$check_calib_consistency()
      invisible(self)
    },
    flip_xy = function(flip_image = '') {
      private$.SI$flip_xy(flip_image)
      private$.ADF$flip_xy(flip_image)
      self$check_calib_consistency()
      invisible(self)
    },
    # type is one of ("pct","px","unit"), 
    # with pct between 0-1, px starts from 1
    # and unit starts from 0 (nm)
    crop_xy = function(x0,x1,y0,y1,type) {
      private$.SI$crop_xy(x0,x1,y0,y1,type)
      private$.ADF$crop_xy(x0,x1,y0,y1,type)
      self$check_calib_consistency(x0,x1,y0,y1,type)
      invisible(self)
    }
  ), # EOF public
  private = list(
#    .IMG1 = NULL,
#    .IMG2 = NULL
  ) # EOF private
) #EOF ResizeAndRotate



ImgAndSpecAlign<-R6Class ("ImgAndSpecAlign", 
  inherit = SIandADF,
  public = list(
    # Drift direction is the slower repeating direction. 
    # (the 1st dimension of the image will be changed.)
    # Take a line of faster repeating direction as a whole. 
    # Positive drift value will lead to a selection of range towards the negative direction. 
    # (the line will be shifted to the positive direction)
    img_x_drift_by_arr_cut = function(x_drift_arr_all,img){
      timg<-t(img)
      #x_drift_arr_all<-c(0,x_drift_arr)
      left_cut<-max(x_drift_arr_all)+1
      right_cut<-ncol(timg)+min(x_drift_arr_all)
      sapply(seq_along(x_drift_arr_all),function(i){
        timg[i,(left_cut-x_drift_arr_all[i]):(right_cut-x_drift_arr_all[i])]
      })
    },
    #Do not cut blank edge for img
    img_x_drift_by_arr_nocut = function(x_drift_arr_all,img){
      old_dim<-dim(img)
      new_dim<-old_dim+c(max(x_drift_arr_all)-min(x_drift_arr_all),0)
      outimg<-array(NA,dim=new_dim)
      origShift<- -min(x_drift_arr_all)
      origWidth<-old_dim[1]
      sapply(seq_along(x_drift_arr_all),function(i){
        outimg[(origShift+x_drift_arr_all[i]+1):(origShift+x_drift_arr_all[i]+origWidth),i]<-img[,i]
        outimg[,i]
      })
#      return(outimg)
    },
    img_x_drift_by_arr = function(x_drift_arr_all,img){
	if (private$.keep_scrap_x) {
		return(self$img_x_drift_by_arr_nocut(x_drift_arr_all,img))
	} else {
		return(self$img_x_drift_by_arr_cut(x_drift_arr_all,img))
	}
    },
    spec_x_drift_by_arr_cut = function(x_drift_arr_all_int,InSpecCnt){
      old_dim<-dim(InSpecCnt)
      new_dim<-old_dim+c(min(x_drift_arr_all_int)-max(x_drift_arr_all_int),0,0)
      left_cut<-max(x_drift_arr_all_int)+1
      #right_cut<-new_dim[1]+left_cut-1
      right_cut<-old_dim[1]+min(x_drift_arr_all_int)
      outSpecCnt<-array(NA,dim=new_dim)
      for (i in seq_along(x_drift_arr_all_int)) {
        outSpecCnt[,i,]<-InSpecCnt[(left_cut-x_drift_arr_all_int[i]):(right_cut-x_drift_arr_all_int[i]),i,]
      }
#      sapply(seq_along(x_drift_arr_all_int),function(i){outSpecCnt[,i,]<<-InSpecCnt[(left_cut-x_drift_arr_all_int[i]):(right_cut-x_drift_arr_all_int[i]),i,];})
      return(outSpecCnt)
    }, 
    #Do not cut blank edge for spec
    spec_x_drift_by_arr_nocut = function(x_drift_arr_all_int,InSpecCnt){
      old_dim<-dim(InSpecCnt)
      new_dim<-old_dim+c(max(x_drift_arr_all_int)-min(x_drift_arr_all_int),0,0)
      outSpecCnt<-array(NA,dim=new_dim)
      origShift<- -min(x_drift_arr_all_int)
      origWidth<-old_dim[1]
      for (i in seq_along(x_drift_arr_all_int)) {
        outSpecCnt[(origShift+x_drift_arr_all_int[i]+1):(origShift+x_drift_arr_all_int[i]+origWidth),i,]<-InSpecCnt[,i,]
      }
#      sapply(seq_along(x_drift_arr_all_int),function(i){outSpecCnt[(origShift+x_drift_arr_all_int[i]+1):(origShift+x_drift_arr_all_int[i]+origWidth),i,]<<-InSpecCnt[,i,];})
      return(outSpecCnt)
    },
    spec_x_drift_by_arr = function(x_drift_arr_all_int,InSpecCnt){
	if (private$.keep_scrap_x) {
		return(self$spec_x_drift_by_arr_nocut(x_drift_arr_all_int,InSpecCnt))
	} else {
		return(self$spec_x_drift_by_arr_cut(x_drift_arr_all_int,InSpecCnt))
	}
    }, 
    spec_e_drift_along_y_by_arr_cut = function(e_drift_along_y_arr_all_int, InSpecCnt) {
       InSpecCnt<-aperm(InSpecCnt,c(3,2,1))
       outSpecCnt<-self$spec_x_drift_by_arr_cut(e_drift_along_y_arr_all_int, InSpecCnt)
       aperm(outSpecCnt,c(3,2,1))
#      old_dim<-dim(InSpecCnt)
#      new_dim<-old_dim+c(0,0,min(e_drift_along_y_arr_all_int)-max(e_drift_along_y_arr_all_int))
#      left_cut<-max(e_drift_along_y_arr_all_int)+1
#      right_cut<-old_dim[3]+min(e_drift_along_y_arr_all_int)
#      outSpecCnt<-array(NA,dim=new_dim)
#      for (i in 1:length(e_drift_along_y_arr_all_int)) {
#        outSpecCnt[,,i]<-InSpecCont[,i,(left_cut-e_drift_along_y_arr_all_int[i]):(right_cut-e_drift_along_y_arr_all_int[i]]
#      }
#      outSpecCnt
    },
    spec_e_drift_along_y_by_arr_nocut = function(e_drift_along_y_arr_all_int, InSpecCnt) {
      InSpecCnt<-aperm(InSpecCnt,c(3,2,1))
      outSpecCnt<-self$spec_x_drift_by_arr_nocut(e_drift_along_y_arr_all_int, InSpecCnt)
      aperm(outSpecCnt,c(3,2,1))
    },
    spec_e_drift_along_y_by_arr = function(e_drift_along_y_arr_all_int, InSpecCnt) {
	if (private$.keep_scrap_e) {
		return(self$spec_e_drift_along_y_by_arr_nocut(e_drift_along_y_arr_all_int,InSpecCnt))
	} else {
		return(self$spec_e_drift_along_y_by_arr_cut(e_drift_along_y_arr_all_int,InSpecCnt))
	}
    },
## a few more drift functions here 

    img_calc_cor_to_one = function(img,x_drift_rng,fix_line){
        sapply(seq_along(x_drift_rng),function(i_x_drift){ # loop over x_drift_rng
	  x_drift_arr_int<-rep(x_drift_rng[i_x_drift],ncol(img)) # drift every col
	  x_drift_arr_int[fix_line] <- 0                   # do not drift fix_line
          # cor() does not support na.rm=T. Must use _cut
	  img_drifted<-self$img_x_drift_by_arr_cut(x_drift_arr_int,img)
	  sapply(seq_len(ncol(img_drifted)),function(i_x){cor(img_drifted[,fix_line],img_drifted[,i_x])})
        })  # return a matrix of correction with a fix_line
	    #(each col loops over x_drift_rng and each row loop over col of img)
    },
    # 
    # usage: vertical_align(transformed_x_drift_img[,],-5:5)
    img_vertical_align_calc_arr_1 = function(img,x_drift_rng){
      # loop over all scan lines (fast scan is in a line)
      img_calc_cor_to_one<-self$img_calc_cor_to_one
      cor_m<-sapply(seq_len(ncol(img)),function(i_x_line){
        # each scan line in the loop is set as a fix_line
	cor_img<-img_calc_cor_to_one(img, x_drift_rng, i_x_line)
        # find the drift corresponds to the 
        # best correlation among dirft_x_rng
	cor_row<-x_drift_rng[max.col(cor_img)]
        # self correlation is set to 0
	cor_row[i_x_line]<-0
	cor_row
       })
      rowMeans(cor_m)
    },
    # This is not going to work! (Why?) # 
    img_vertical_align_calc_arr_2 = function(img, x_drift_rng) {
      cor_m<-array(0, dim=rep(ncol(img),2))
      for (i in 2:ncol(img)) {
        for (j in 1:(i-1)) {#print(i);print(j)
          c<-ccf(img[,i],img[,j], lag.max=max(abs(x_drift_rng)), plot=F, type="correlation")
          cor_m[i,j]<-c$lag[which.max(c$acf)]
        }
      }
      # a matrix stores the lag between i and j-th col of img
      cor_m<--cor_m+t(cor_m)
      rowMeans(cor_m)      
    },
    # This is not going to work! # no it's working
    img_vertical_align_calc_arr_3 = function(img, x_drift_rng) {
      ave_img <- rowMeans(img) #; ave_img <- img[,1]
      sapply(1:ncol(img), function(i_x_line) {
        c<-ccf(ave_img, img[,i_x_line], lag.max=max(abs(x_drift_rng)), plot=F, type="correlation")
        c$lag[which.max(c$acf)]
      })
    },
    img_vertical_align_calc_arr = function(img, x_drift_rng) {
      self$img_vertical_align_calc_arr_1(img, x_drift_rng)
    },
    drift_arr_smth_poly = function(arr_in, x_in = NULL, x_out = NULL, arg = list(poly_deg = 3)) {
      if (is.null(x_in)) {
        x_in<-seq_along(arr_in)
      }
      if (is.null(x_out)) {
        x_out<-x_in
      }
      y<-arr_in
      poly_deg<-arg$poly_deg
      fm0<-lm(y ~ poly(x_in, poly_deg))
      y_out<-as.integer(round(predict(fm0, data.frame(x_in = x_out))))
      if (getOption('unmixeels.verbose') == T) {
        print("y_out:")
        print(y_out)
      }
    },

    drift_arr_smth = function(arr_in, smth_method, x_in = NULL, x_out = NULL, arg) {
      if (smth_method == 'poly') {
        if (is.null(arg$poly_deg)) {
          arr_out <- self$drift_arr_smth_poly(arr_in, x_in, x_out)
        } else {
          # usage: drift_arr_smth_poly(arr_in, 'poly', list(poly_deg = 4))
          arr_out <- self$drift_arr_smth_poly(arr_in, x_in, x_out, arg)
        }
      } else {
        message("Warn: drift_arr_smth: smth_method not implemented!")
        arr_out <-arr_in
      }

      if (getOption('unmixeels.verbose') == T) {
        plot(x_in,arr_in,type="b")
        lines(x_out,arr_out,col="red")
      }

      arr_out
    },
    img_x_drift_align_with_refine_calc_arr = function( image, x_drift_max_search_rep, 
      x_drift_search_rng_init, x_drift_search_rng_2nd ) {
      transformed_x_drift_img<-image
      #x_drift_max_search_rep<-private$.x_drift_max_search_rep
      x_drift_arr_all_int<-0
      for (i in seq_len(x_drift_max_search_rep)){
        if (i==1) {
          x_drift_search_rng<-x_drift_search_rng_init
        } else {
          x_drift_search_rng<-x_drift_search_rng_2nd
        }
        x_drift_arr_all<-self$img_vertical_align_calc_arr(transformed_x_drift_img,x_drift_search_rng)
#        x_drift_arr_all_int_old<-x_drift_arr_all_int
#        x_drift_arr_all_int<-self$img_vertical_align_calc_arr(transformed_x_drift_img,x_drift_search_rng)
        inc<-as.integer(round(x_drift_arr_all))
        x_drift_arr_all_int<-x_drift_arr_all_int+inc
        if (sum(abs(inc))==0) {
#        if (sum(abs(x_drift_arr_all_int_old-x_drift_arr_all_int))==0) {
          break
        }
        if (getOption('unmixeels.verbose') == T) {
          print("x_drift_arr_all_int")
          print(x_drift_arr_all_int)
        }
        transformed_x_drift_img<-self$img_x_drift_by_arr_cut(x_drift_arr_all_int,image)
        if (getOption('unmixeels.verbose') == T) {
          image(1:nrow(transformed_x_drift_img),1:ncol(transformed_x_drift_img),transformed_x_drift_img,col=terrain.colors(2^16))
          lines(x_drift_arr_all_int-min(x_drift_arr_all_int),1:ncol(transformed_x_drift_img),type="b")
        }
      }
      x_lines<-1:ncol(image)
#      if (getOption('unmixeels.verbose') == T) {
#        plot(x_drift_arr_all_int,type="b")
#      }
      x_drift_arr_all_int
    },
    img_set_x_drift_img = function( x_drift_img )  {
      private$.x_drift_img <- x_drift_img
      invisible(self)
    },
    img_set_x_drift_arr = function(val) {
      private$.x_drift_arr_all_int<-as.integer(round(val))
      invisible(self)
    }, 
    spec_set_e_drift_along_y_arr = function(val) {
      private$.e_drift_along_y_arr_all_int<-as.integer(round(val))
      invisible(self)
    },
    img_set_x_drift_arr_from_x_drift_img = function() {
      self$img_set_x_drift_arr(
        self$img_x_drift_align_with_refine_calc_arr(
          private$.x_drift_img, private$.x_drift_max_search_rep, 
          private$.x_drift_search_rng_init, private$.x_drift_search_rng_2nd))
      invisible(self)
    },
    spec_set_e_drift_along_y_arr_from_e_drift_along_y_img = function() {
      self$spec_set_e_drift_along_y_arr(
        self$img_x_drift_align_with_refine_calc_arr(
          private$.e_drift_along_y_img, private$.e_drift_along_y_max_search_rep, 
          private$.e_drift_along_y_search_rng_init, private$.e_drift_along_y_search_rng_2nd))
    invisible(self)
    }, 
    img_smth_x_drift_arr = function(smth_method,y0,y1,type_y,arg = NULL) {
      x_in <- (private$.SI$convert_to_index(y0,2,type_y)):(private$.SI$convert_to_index(y1,2,type_y))
      x_out<- seq_len(dim(private$.SI$Cnt)[2])
      self$img_set_x_drift_arr(self$drift_arr_smth(
        private$.x_drift_arr_all_int,smth_method,x_in,x_out,arg))
      invisible(self)
    },
    spec_smth_e_drift_along_y_arr = function(
      smth_method,y0,y1,type_y,arg = NULL) {
      x_in <- (private$.SI$convert_to_index(y0,2,type_y)):(private$.SI$convert_to_index(y1,2,type_y))
      x_out<- seq_len(dim(private$.SI$Cnt)[2])
      self$spec_set_e_drift_along_y_arr(self$drift_arr_smth(
        private$.e_drift_along_y_arr_all_int,smth_method,x_in,x_out,arg))
      invisible(self)
    },
    img_set_x_drift_params = function( keep_scrap_x = F, 
      x_drift_max_search_rep = 5, x_drift_search_rng_init = -15:15, 
      x_drift_search_rng_2nd = -5:5, x_drift_img = NA) {
      private$.keep_scrap_x <- keep_scrap_x
      private$.x_drift_max_search_rep <-x_drift_max_search_rep
      private$.x_drift_search_rng_init<-x_drift_search_rng_init
      private$.x_drift_search_rng_2nd <-x_drift_search_rng_2nd
      # self$img_set_x_drift_img(x_drift_img)
      invisible(self)
    },
    spec_set_e_drift_along_y_params = function( keep_scrap_e = F, 
      e_drift_along_y_max_search_rep = 5, e_drift_along_y_search_rng_init = -15:15, 
      e_drift_along_y_search_rng_2nd = -5:5, e_drift_along_y_img = NA) {
      private$.keep_scrap_e <- keep_scrap_e
      private$.e_drift_along_y_max_search_rep <- e_drift_along_y_max_search_rep
      private$.e_drift_along_y_search_rng_init <- e_drift_along_y_search_rng_init
      private$.e_drift_along_y_search_rng_2nd <- e_drift_along_y_search_rng_2nd
      # private$.e_drift_along_y_img = NA,
      invisible(self)
    },
    spec_set_e_drift_along_y_img = function(e_drift_along_y_img) {
      private$.e_drift_along_y_img <- e_drift_along_y_img
      invisible(self)
    },
    ADF_SI_calib_scrap = function(calib, keep_scrap, dim, drift) {
      if (keep_scrap) { # nocut
        calib$Origin[dim]<-calib$Origin[dim] + min(drift)#/calib$Scale[dim]
      } else {
        calib$Origin[dim]<-calib$Origin[dim] - max(drift)#/calib$Scale[dim]
      }
      calib
    }, 
    ADF_do_x_drift_corr_from_arr = function() {
      private$.ADF$set_Cnt(self$img_x_drift_by_arr(private$.x_drift_arr_all_int, private$.ADF$Cnt))
      private$.ADF$set_calib(
        self$ADF_SI_calib_scrap(
          private$.ADF$calib, private$.keep_scrap_x, 1, private$.x_drift_arr_all_int))
      invisible(self)
    },
    SI_do_x_drift_corr_from_arr = function() {
      private$.SI$set_Cnt(self$spec_x_drift_by_arr(private$.x_drift_arr_all_int, private$.SI$Cnt))
      private$.SI$set_calib(
        self$ADF_SI_calib_scrap(
          private$.SI$calib, private$.keep_scrap_x, 1, private$.x_drift_arr_all_int))
      invisible(self)
    },
    SI_and_ADF_x_drift_corr = function(from_ADF_or_EFImg, 
      x0,x1,y0,y1,type_xy,e0,e1,type_e,smth_method,arg = list(poly_deg = 3),
      x_drift_max_search_rep = 10, x_drift_search_rng_init = -15:15, 
      x_drift_search_rng_2nd = -5:5) {
      self$img_set_x_drift_params(keep_scrap_x = F, 
        x_drift_max_search_rep = x_drift_max_search_rep,
        x_drift_search_rng_init = x_drift_search_rng_init, 
        x_drift_search_rng_2nd = x_drift_search_rng_2nd)
      if (from_ADF_or_EFImg == 'EFImg') {
        self$img_set_x_drift_img(
            private$.SI$gen_EFImg_from_3d(e0,e1,type_e)$crop_xy(x0,x1,y0,y1,type_xy)$Cnt)
      } else if (from_ADF_or_EFImg == 'ADF') {
        self$img_set_x_drift_img(private$.ADF$clone()$crop_xy(x0,x1,y0,y1,type_xy)$Cnt)
      } else {
        message("Error: from_SI_or_ADF must be 'EFImg' or 'ADF'")
      }
      self$img_set_x_drift_arr_from_x_drift_img()
      self$img_smth_x_drift_arr(smth_method,y0,y1,type_xy,arg)
      self$ADF_do_x_drift_corr_from_arr()
      self$SI_do_x_drift_corr_from_arr()
      invisible(self)
    },
    SI_do_e_drift_along_y_corr_from_arr = function() {
      private$.SI$set_Cnt(self$spec_e_drift_along_y_by_arr(
        private$.e_drift_along_y_arr_all_int, private$.SI$Cnt)) 
      private$.SI$set_calib(
        self$ADF_SI_calib_scrap(
          private$.SI$calib, private$.keep_scrap_e, 3, private$.e_drift_along_y_arr_all_int))
      invisible(self)
    }, 
    SI_e_drift_along_y_corr = function(x0,x1,y0,y1,type_xy,e0,e1,type_e,
      smth_method, arg = list(poly_deg = 3), e_drift_along_y_max_search_rep = 5, 
      e_drift_along_y_search_rng_init = -15:15, e_drift_along_y_search_rng_2nd = -5:5) {
      self$spec_set_e_drift_along_y_params( keep_scrap_e = F, 
        e_drift_along_y_max_search_rep = e_drift_along_y_max_search_rep,
        e_drift_along_y_search_rng_init = e_drift_along_y_search_rng_init, 
        e_drift_along_y_search_rng_2nd = e_drift_along_y_search_rng_2nd) 
      # transpose the energy dim to the first
      self$spec_set_e_drift_along_y_img(t( 
        private$.SI$clone()$crop_xy(
        x0,x1,y0,y1,type_xy)$crop_e(e0,e1,type_e)$SI_3d_to_2d_or_pt('x')$Cnt[1,,]))
      self$spec_set_e_drift_along_y_arr_from_e_drift_along_y_img()
      self$spec_smth_e_drift_along_y_arr(smth_method,y0,y1,type_xy,arg)
      self$SI_do_e_drift_along_y_corr_from_arr()
      private$.SI$set_E(private$.SI$E[(max(private$.e_drift_along_y_arr_all_int)+1):(length(private$.SI$E)+min(private$.e_drift_along_y_arr_all_int))])
      invisible(self)
    }
  ), #EOF public
  private = list(
#    .SI = NA,
#    .ADF = NA,
    .keep_scrap_x = F,
    .x_drift_max_search_rep = 10,
    .x_drift_search_rng_init = -15:15,
    .x_drift_search_rng_2nd = -5:5,
    .x_drift_img = NA,
    .x_drift_arr_all_int = NA, 
    .keep_scrap_e = F,
    .e_drift_along_y_max_search_rep = 5,
    .e_drift_along_y_search_rng_init = -15:15, 
    .e_drift_along_y_search_rng_2nd = -5:5,
    .e_drift_along_y_img = NA,
    .e_drift_along_y_arr_all_int = NA
  ), #EOF private
  active = list(
    x_drift_img = function(value) {
      if (missing(value)) {
        private$.x_drift_img
      } else {
        message("Warn: You are manually setting x_drift_img")
        private$.x_drift_img<-value
      }
    },
    x_drift_arr_all_int = function(value) {
      if (missing(value)) {
        private$.x_drift_arr_all_int
      } else {
        message("Warn: You are manually setting x_drift_arr_all_int")
        private$.x_drift_arr_all_int<-value
      }
    }
  ) # EOF active
) # EOF ImgAndSpecAlign


BkgRemoval<-R6Class( "BkgRemoval",
  inherit = OrigSI, 
  public = list(
    ExtractBkgPolyOrtho = function(Tot, E, arg = list(poly_deg = 1)){
      orig_dims<-dim(Tot)
      Tot_2d<-ThreeDimData$new(Tot)$get_2d_data()
      poly_deg<-arg$poly_deg
      x_in <- E
      y<-t(Tot_2d)
      fm0<-lm(y~poly(x_in, degree = poly_deg))
      Bkg_2d<-t(fm0$fitted.values)
      Sig_2d<-t(fm0$residuals)
      out<-NULL
      out$fm0<-fm0
      out$Bkg<-ThreeDimData$new(Bkg_2d)$get_data_with_dims(orig_dims)
      out$Sig<-ThreeDimData$new(Sig_2d)$get_data_with_dims(orig_dims)
      out
    },
    set_Bkg_Sig_algo_PolyOrtho = function(arg = list(poly_deg = 1)){
      X<-self$calc_X(private$.SigELow, private$.SigEHigh)
      Tot<-private$.Cnt[,,X,drop=F]
      private$.SigE<-private$.E[X]
      out<-self$ExtractBkgPolyOrtho(Tot, private$.SigE, arg)

      private$.fm0<-out$fm0
      private$.Tot<-Tot
      private$.Bkg<-out$Bkg
      private$.Sig<-out$Sig
      invisible(self)
    },
    ExtractBkgPoly = function(Tot_PreEdge,Tot_Sig,BkgE,SigE, arg = list(poly_deg = 1)) {
      orig_dims_PreEdge<-dim(Tot_PreEdge)
      Tot_PreEdge_2d<-ThreeDimData$new(Tot_PreEdge)$get_2d_data()

      orig_dims_Sig<-dim(Tot_Sig)
      Tot_Sig_2d<-ThreeDimData$new(Tot_Sig)$get_2d_data()

      poly_deg<-arg$poly_deg

      x_in  <- BkgE
      y<-t(Tot_PreEdge_2d)#[,1:10]
      x_out <- SigE
      fm0<-lm(y~poly(x_in, degree = poly_deg))
#      fm0<-lm(y~x_in)
      y_out<-predict(fm0, newdata = data.frame(x_in = x_out))

      Bkg_2d<-t(y_out)
      Sig_2d<-Tot_Sig_2d - Bkg_2d

      out<-NULL
      out$fm0<-fm0
      out$Bkg<-ThreeDimData$new(Bkg_2d)$get_data_with_dims(orig_dims_Sig)

      out$Sig<-ThreeDimData$new(Sig_2d)$get_data_with_dims(orig_dims_Sig)
      out
    },
    set_Bkg_Sig_algo_Poly = function(arg = list(poly_deg = 1)) {
      X_PreEdge<-self$calc_X(private$.BkgELow, private$.BkgEHigh)
      Tot_PreEdge <- private$.Cnt[,,X_PreEdge,drop=F]
      private$.BkgE<-private$.E[X_PreEdge]

      X_Sig<-self$calc_X(private$.SigELow, private$.SigEHigh)
      Tot_Sig<-private$.Cnt[,,X_Sig,drop=F]
      private$.SigE<-private$.E[X_Sig]

      out<-self$ExtractBkgPoly(Tot_PreEdge, Tot_Sig, private$.BkgE, private$.SigE, arg = list(poly_deg = 1))

      private$.fm0<-out$fm0
      private$.Tot<-Tot_Sig
      private$.Bkg<-out$Bkg
      private$.Sig<-out$Sig

      invisible(self)
    },
    ExtractBkgPower = function(Tot_PreEdge,Tot_Sig,BkgE,SigE, arg = list(use_nls=F)) {
      orig_dims_PreEdge<-dim(Tot_PreEdge)
      Tot_PreEdge_2d<-ThreeDimData$new(Tot_PreEdge)$get_2d_data()

      orig_dims_Sig<-dim(Tot_Sig)
      Tot_Sig_2d<-ThreeDimData$new(Tot_Sig)$get_2d_data()

      use_nls<-arg$use_nls

# cnt ~ A * E^{-r}
# log(cnt) ~ log(A) -r * log(E)

      x_in  <- log(BkgE)
      y<-log(t(Tot_PreEdge_2d))
      x_out <- log(SigE)

#      fm0<-lm(y~poly(x_in, degree = poly_deg))
      fm0<-lm(y~x_in)
      y_out<-exp(predict(fm0, data.frame(x_in = x_out)))

      Bkg_2d<-t(y_out)
      Sig_2d<-Tot_Sig_2d - Bkg_2d

      fm1<-NULL
      if (use_nls) {
        x_in <- BkgE
        y <-t(Tot_PreEdge_2d)
        x_out<- SigE

        a_start<-exp(fm0$coefficients[1,])#+rnorm(1)
        r_start<-fm0$coefficients[2,]#+rnorm(1)

        fm1<-nls(y~t(a*exp(-outer(r,log(x_in)))), start = list(a=a_start, r=r_start))
        y_out<-predict(fm1, data.frame(x_in = x_out))
      }

      out<-NULL
      out$fm0<-fm0
      out$Bkg<-ThreeDimData$new(Bkg_2d)$get_data_with_dims(orig_dims_Sig)
      out$Sig<-ThreeDimData$new(Sig_2d)$get_data_with_dims(orig_dims_Sig)
      out$fm1<-fm1
      out
    },
    set_Bkg_Sig_algo_Power = function(arg = list(use_nls=F)) {
      X_PreEdge<-self$calc_X(private$.BkgELow, private$.BkgEHigh)
      Tot_PreEdge <- private$.Cnt[,,X_PreEdge,drop=F]
      private$.BkgE<-private$.E[X_PreEdge]

      X_Sig<-self$calc_X(private$.SigELow, private$.SigEHigh)
      Tot_Sig<-private$.Cnt[,,X_Sig,drop=F]
      private$.SigE<-private$.E[X_Sig]

      out<-self$ExtractBkgPower(Tot_PreEdge, Tot_Sig, private$.BkgE, private$.SigE, arg)
      private$.Tot<-Tot_Sig
      private$.Bkg<-out$Bkg
      private$.Sig<-out$Sig
      invisible(self)
    },
    conv_Bkg_Sig_linear_correction = function() {
      X_PreEdge_Low<-which.min(abs(private$.BkgELow-private$.SigE))
      X_PreEdge_High<-which.min(abs(private$.BkgEHigh-private$.SigE))
      X_PreEdge<-X_PreEdge_Low:X_PreEdge_High
      private$.BkgE<-private$.E[X_PreEdge]

      X_Sig_Low<-which.min(abs(private$.SigELow-private$.SigE))
      X_Sig_High<-which.min(abs(private$.SigEHigh-private$.SigE))
      X_Sig<-X_Sig_Low:X_Sig_High

      a<-rowMeans(private$.Sig[,,X_PreEdge,drop=F],dim=2,na.rm=T)
      a<-array(a,dim=c(dim(a),length(X_Sig)))

# crop to new
      private$.Sig<-private$.Sig[,,X_Sig,drop=F]
      private$.Bkg<-private$.Bkg[,,X_Sig,drop=F]

      private$.Sig<-private$.Sig-a
      private$.Bkg<-private$.Bkg+a

      private$.Tot<-private$.Tot[,,X_Sig,drop=F]
      private$.SigE<-private$.SigE[X_Sig]

      invisible(self)
    },
#    calc_Sig_sum = function() {
#      sum(private$.Sig)
#    },
#    crop_e_Sig = function(e0, e1, type)
#    ExtractBkgExp = function(use_nls=F) {
#      out<-NULL
#      out
#    },
    set_params = function(SigELow = NULL, SigEHigh = NULL, BkgELow = NULL, BkgEHigh = NULL) {
      private$.SigELow<-SigELow
      private$.SigEHigh<-SigEHigh
      private$.BkgELow<-BkgELow
      private$.BkgEHigh<-BkgEHigh
#      private$.SigCalib<-private$.calib
      invisible(self)
    },
    calc_X = function(ELow, EHigh) {
      X<-(self$convert_to_index(ELow, 3, 'unit-absolute')):(self$convert_to_index(EHigh, 3, 'unit-absolute'))
      X
    },
#    set_BkgE = function() {
#      X<-self$calc_X(private$.BkgELow, BkgEHigh)
#      private$.BkgE<-private$.E[X]
#      invisible(self)
#    },
#    set_SigE = function() {
#      X<-self$calc_X(private$.SigELow, private$.SigEHigh)
#      private$.SigE<-private$.E[X]
#      invisible(self)
#    },
    calc_SigCalib = function() {
      SigCalib<-private$.calib
      SigCalib$Origin[private$.numberOfDims]<-min(private$.SigE)/SigCalib$Scale[private$.numberOfDims]
      SigCalib
    },
    get_Bkg = function() {
      private$.Bkg
    },
    get_Sig = function() {
      private$.Sig
    }, 
    get_Tot = function() {
      private$.Tot
    },
    get_BkgE = function() {
      private$.BkgE
    },
    get_SigE = function() {
      private$.SigE
    },
    get_Sig_as_SI = function() {
      SigCalib<-self$calc_SigCalib()
      outSI<-OrigSI$new()$set_SI(private$.numberOfDims, SigCalib, private$.SigE, private$.Sig)
      outSI
    },
    initialize = function(origSI) {
      private$.numberOfDims<-origSI$numberOfDims
      private$.calib<-origSI$calib
      private$.Cnt<-origSI$Cnt
      private$.E<-origSI$E 
      self$convert_to_3d_with_warn()
      invisible(self)    
    }
  ), # EOF public
  private = list(
    .BkgE = NA,
    .SigE = NA,
    .BkgELow = NA,
    .BkgEHigh = NA,
    .SigELow = NA, 
    .SigEHigh = NA,
    .fm0 = NA,
    .fm1 = NA,
    .Tot = NA,
    .Bkg = NA,
    .Sig = NA
  ), #EOF private
  active = list(
    SigE = function(value) {
      if (missing(value)) {
        private$.SigE
      } else {
        message("Warn: You are changing SigE")
        private$.SigE<-value
      }
    },
    Sig = function(value) {
      if (missing(value)) {
        private$.Sig
      } else {
        message("Warn: You are changing Sig")
        private$.Sig<-value
      }
    }
  ) # EOF active
) # EOF BkgRemoval



#_end_libeelsproc

#_begin_TiO_single_template


#_begin_vca_general_nobkg_prepare
# only works for 3d
EELSUnmix<-R6Class( "EELS_unmix",
#  inherit = SIandADF,
#     .numberOfDims	= 0,
#     .calib		= NULL,
#     .Cnt		= NA

  public = list(
    set_for_unmix = function(EForUnmix, calibForUnmix, SigForUnmix) {
      private$.EForUnmix<-EForUnmix
      private$.calibForUnmix<-calibForUnmix
      private$.SigForUnmix<-SigForUnmix
#      self$check_e()
      invisible(self)
    },
    set_for_fit = function (EForFit, calibForFit, SigForFit) {
      private$.EForFit<-EForFit
      private$.calibForFit<-calibForFit
      private$.SigForFit<-SigForFit
      invisible(self)
    },
    check_dims_for_unmix_and_fit = function() {
      EForUnmix<-private$.EForUnmix
      EForFit<-private$.EForFit
      ret <- T
      # check E
      if ( (length(EForUnmix)!=length(EForFit) ) 
           || (sum(abs(EForUnmix - EForFit)) > 1e-4 )  ) {
        message("Error: EForUnmix and EForFit differs!")
        ret <- F
      }
      # check dim
      SigForUnmix <- private$.SigForUnmix
      SigForFit   <- private$.SigForFit
      if ( ( length(dim(SigForUnmix))!= 3 
          ||length(dim(SigForFit))  != 3
          ||dim(SigForUnmix)[3]!=dim(SigForFit)[3] ) ) {
        message("Error: dim of SigForUnmix and SigForFit differs!")
        ret <- F
      }
      ret
    },
    check_dims_for_unmix_and_intensity_scale = function() {
      # check intensity scale
      ret <- T
      intensity_scale_for_Sig<-private$.intensity_scale_for_Sig
      SigForUnmix <- private$.SigForUnmix
      if ( (length(dim(SigForUnmix)) - length(dim(intensity_scale_for_Sig)) != 1 )
         ||(sum(abs(dim(SigForUnmix)[-3] - dim(intensity_scale_for_Sig))) !=0 ) ) {
        message("Error: dim of intensity_scale_for_Sig and SigForUnmix differs!")
        ret <- F
      }
      ret
    },
    set_sig_for_unmix_from_SI = function(SI) {
      self$set_for_unmix(SI$E, SI$calib, SI$Cnt)
      invisible(self)
    },
    set_sig_for_fit_from_SI = function(SI) {
      self$set_for_fit(SI$E, SI$calib, SI$Cnt)
      invisible(self)
    },
#    set_sig_for_unmix_from_bkg_removed = function(
#        bkg_removed_SI_for_unmix) {
#      self$set_for_unmix(
#        bkg_removed_SI_for_unmix$SigE, bkg_removed_SI_for_unmix$Sig)
#      invisible(self)
#    },
#    set_sig_for_fit_from_bkg_removed = function(
#        bkg_removed_SI_for_fit) {
#      self$set_for_fit(
#        bkg_removed_SI_for_fit$SigE,   bkg_removed_SI_for_fit$Sig)
#      invisible(self)
#    },
    set_intensity_scale_for_Sig_from_ADF = function(ADF) {
      private$.intensity_scale_for_Sig<-ADF$Cnt
      invisible(self)
    },
    set_unmix_R_and_intensity_scale_for_unmix = function (){
      self$check_dims_for_unmix_and_intensity_scale()
      private$.intensity_scale_for_R<-c(private$.intensity_scale_for_Sig)
      private$.unmix_R_for_unmix<-t(ThreeDimData$new(private$.SigForUnmix)$get_2d_data())
      invisible(self)
    },
    set_unmix_R_for_fit = function() {
      self$check_dims_for_unmix_and_fit()
      d<-dim(private$.SigForFit)
      private$.dims<-d
      private$.unmix_R_for_fit<-t(ThreeDimData$new(private$.SigForFit)$get_2d_data())
      invisible(self)
    },
    UnmixCore_set_R_for_unmix = function(UnmixCoreObj) {
      UnmixCoreObj<-UnmixCoreObj$set_R_and_scale(private$.unmix_R_for_unmix, private$.intensity_scale_for_Sig)
      UnmixCoreObj
    },
    UnmixCore_set_R_for_fit = function(UnmixCoreObj) {
      UnmixCoreObj<-UnmixCoreObj$set_R_and_scale(private$.unmix_R_for_fit, private$.intensity_scale_for_Sig)
      UnmixCoreObj
    },
    set_p_fitted_coef_residuals_from_UnmixCore = function(UnmixCoreObj) {
      out<-UnmixCoreObj$get_p_Rpem_Ae_fitted_coef_residuals()
      # convert dims back
      orig_dims<-private$.dims 
      private$.p<-out$p
      vca_p<-private$.p
      private$.Rp_em<-out$Rp_em
      private$.Ae<-out$Ae
      private$.indice<-out$indice 
      private$.fitted<-ThreeDimData$new(t(out$fitted))$get_data_with_dims(orig_dims) 
      private$.coef<-ThreeDimData$new(t(out$coef))$get_data_with_dims(c(orig_dims[-length(orig_dims)],vca_p))
      private$.residuals<-ThreeDimData$new(t(out$residuals))$get_data_with_dims(orig_dims)
      private$.residuals_chisq<-out$residuals_chisq 
      private$.residuals_chisq$chisq_3d<-ThreeDimData$new(t(private$.residuals_chisq$chisq_2d))$get_data_with_dims(orig_dims) 
      private$.residuals_chisq$line_ave_3d<-ThreeDimData$new(private$.residuals_chisq$line_ave)$get_data_with_dims(orig_dims[-3]) 
      invisible(self)
    },

    do_plot_em_sig = function() {
      vca_p<-private$.p
      Ae<-private$.Ae
      EForUnmix<-private$.EForUnmix
      print(sprintf("Info: Number of EMs (p) : %d", vca_p))
      EForUnmix<-private$.EForUnmix
      calibForUnmix<-private$.calibForUnmix
      plot(EForUnmix,5*Ae[,1]/max(abs(Ae)),type="l",col=2,ylim=range(5*Ae/max(abs(Ae))),xlab=sprintf("E/%s", calibForUnmix$Units[3]),ylab="Count/arb. unit",las=1)
      if (vca_p>1) {
        for (i in 2:vca_p) {
          lines(EForUnmix, 5*Ae[,i]/max(abs(Ae)),col=i+1)
        }
      }
      invisible(self)
    }, 
    do_plot_coef = function(plot_em_position = T, em_index_for_color_map = NA) {
      vca_p<-private$.p
      coef<-private$.coef
      EForFit<-private$.EForFit
      indice<-private$.indice

      calibForFit<-private$.calibForFit
      rulxlab<-seq(from=calibForFit$Scale[1]*(-calibForFit$Origin[1]+.5),by=calibForFit$Scale[1],length.out=dim(coef)[1])
      rulylab<-seq(from=calibForFit$Scale[2]*(-calibForFit$Origin[2]+.5),by=calibForFit$Scale[2],length.out=dim(coef)[2])
      indice_position_x<-(rulxlab[indice[]])
      map_xlim<-c( calibForFit$Scale[1]*(-calibForFit$Origin[1]), calibForFit$Scale[1]*(-calibForFit$Origin[1]+dim(coef)[1]) )
      map_ylim<-c( calibForFit$Scale[2]*(-calibForFit$Origin[2]), calibForFit$Scale[2]*(-calibForFit$Origin[2]+dim(coef)[2]) )

      str_xlab<-sprintf("x/%s",calibForFit$Units[1])
      str_ylab<-sprintf("y/%s",calibForFit$Units[2])
      if (dim(coef)[2] == 1) {
        coef<-coef/max(coef)
        sum_line<-rowSums(coef[,1,,drop=F])
        line_ylim<-range(c(coef, sum_line))
        if (plot_em_position) {
          line_ylim_new<-c(min(line_ylim)-.1*(max(line_ylim)-min(line_ylim)),max(line_ylim))
          plot(indice_position_x,rep(min(line_ylim_new),vca_p),pch="+",col=2:(vca_p+1),cex=5,xlim=map_xlim,ylim=line_ylim_new,las=1,cex.axis=1,cex.lab=1, xaxs="i", yaxs="i",ylab="Coefficients/arb. unit",xlab=str_xlab)
          lines(rulxlab,sum_line,type="l",ylim=line_ylim_new,col=1,xaxs="i")
        } else {
          plot(rulxlab,sum_line,type="l",ylim=line_ylim,col=1,ylab="Coefficients/arb. unit",xlab=str_xlab,xaxs="i")
        }
        for (i in seq_len(vca_p)) {
          lines(rulxlab,coef[,1,i],col=i+1)
        }
      } else { # make map
        indice_2d<-sapply(indice,function(x){ThreeDimData$new(coef[,,1])$convert_index_to_n(x)})
        indice_position_x<-(rulxlab[indice_2d[1,]])
        indice_position_y<-(rulylab[indice_2d[2,]])
        plot(NA,col=2:(vca_p+1),cex=5,xlim=map_xlim,ylim=map_ylim,las=1,cex.axis=1,cex.lab=1, xaxs="i", yaxs="i",xlab=str_xlab,ylab=str_ylab,asp=1)

	if (is.na(em_index_for_color_map)) {
	  em_index_for_color_map<-1:3
	}
	#_begin_vca_fit_map
	if (vca_p==1){
	  rgb_map<-array(c(coef, rep(0, 2*prod(dim(coef)[1:2])) ),dim=dim(coef)+c(0,0,2))
	} else if (vca_p==2){
	  rgb_map<-array(c(coef, rep(0, prod(dim(coef)[1:2])) ),dim=dim(coef)+c(0,0,1))
	} else {
	  rgb_map<-coef[,,em_index_for_color_map]
	}
	scale_to_saturate<-array(t(matrix(c(
			{function(x) {if (x==0) {return(0)} else {return(1/x)}}  }(quantile(rgb_map[,,1],.99)),
			{function(x) {if (x==0) {return(0)} else {return(1/x)}}  }(quantile(rgb_map[,,2],.99)),
			{function(x) {if (x==0) {return(0)} else {return(1/x)}}  }(quantile(rgb_map[,,3],.99)) ),
		ncol=length(rgb_map[,,1]),nrow=3)),dim=c(dim(rgb_map)[c(1,2)],3))
	to_display_scaled<-rgb_map*scale_to_saturate
	to_display<-to_display_scaled*as.double(to_display_scaled>0)*as.double(to_display_scaled<1)+as.double(to_display_scaled>1)
	to_display<-aperm(array(to_display,dim=c(dim(rgb_map)[c(1,2)],3)),c(2,1,3))

	rasterArray<-array(to_display*1.,dim=c(dim(rgb_map)[c(2,1)],3))
print(dim(to_display));print(dim(rasterArray))
	rasterMap<-as.raster(rasterArray[rev(seq_len(dim(rasterArray)[1])),,,drop=F])
	rasterImage(rasterMap,min(rulxlab)+(rulxlab[1]-rulxlab[2])/2.,min(rulylab)+(rulylab[1]-rulylab[2])/2.,max(rulxlab)+(rulxlab[2]-rulxlab[1])/2., max(rulylab)+(rulylab[2]-rulylab[1])/2., interpolate = FALSE)
		if (plot_em_position) {
		   points(indice_position_x,indice_position_y,pch=0,col="white",cex=2,xlim=map_xlim,ylim=map_ylim,las=1,cex.axis=1,cex.lab=1, xaxs="i", yaxs="i",xlab=str_xlab,ylab=str_ylab)
		  points(indice_position_x,indice_position_y,pch="+",col=2:(vca_p+1),cex=3,xlim=map_xlim,ylim=map_ylim,las=1,cex.axis=1,cex.lab=1, xaxs="i", yaxs="i",xlab=str_xlab,ylab=str_ylab)
        } 
      }
    }, # EOF do_plot_coef
    do_plot_residuals = function() {
      vca_p<-private$.p
      residuals<-private$.residuals
      residuals_chisq<-private$.residuals_chisq

      chisq_2d<-residuals_chisq$chisq_2d
      chisq_3d<-residuals_chisq$chisq_3d
      chisq_line_ave<-residuals_chisq$line_ave
      chisq_line_ave_3d<-residuals_chisq$line_ave_3d
      chisq_lb<-residuals_chisq$lb
      chisq_ub<-residuals_chisq$ub
      vca_noise<-residuals_chisq$noise

      calibForFit<-private$.calibForFit
      rulxlab<-seq(from=calibForFit$Scale[1]*(-calibForFit$Origin[1]+.5),by=calibForFit$Scale[1],length.out=dim(residuals)[1])
      rulylab<-seq(from=calibForFit$Scale[2]*(-calibForFit$Origin[2]+.5),by=calibForFit$Scale[2],length.out=dim(residuals)[2])

      map_xlim<-c( calibForFit$Scale[1]*(-calibForFit$Origin[1]), calibForFit$Scale[1]*(-calibForFit$Origin[1]+dim(residuals)[1]) )
      map_ylim<-c( calibForFit$Scale[2]*(-calibForFit$Origin[2]), calibForFit$Scale[2]*(-calibForFit$Origin[2]+dim(residuals)[2]) )

      str_xlab<-sprintf("x/%s",calibForFit$Units[1])
      str_ylab<-sprintf("y/%s",calibForFit$Units[2])

      if (dim(residuals)[2] == 1) {
        image(rulxlab, seq(0,5,length.out=dim(residuals)[3]), residuals[,1,]/(vca_noise) ,zlim=c(qnorm(1/prod(dim(chisq_2d))), qnorm(1-1/prod(dim(chisq_2d)))),col=terrain.colors(2^16),xlab=str_xlab,ylab=expression(paste("reduced ",chi^2)),xlim=map_xlim,las=1)
        lines(rulxlab,chisq_line_ave)
        lines(range(rulxlab)+.5*rulxlab[2]*c(-1,1), rep(chisq_lb,2),col="red")
        lines(range(rulxlab)+.5*rulxlab[2]*c(-1,1), rep(chisq_ub,2),col="red")
      } else {
        image(rulxlab, rulylab, log(chisq_line_ave_3d) , 
          zlim=log(c(chisq_lb,chisq_ub)),col=terrain.colors(2^16),
          xlab=str_xlab,ylab=expression(paste("reduced ",chi^2)),xlim=map_xlim,asp=1)
      }

    } #EOF do_plot_residuals
  ), # EOF public
  private = list(

### in original dims
    .EForUnmix = NA,
    .calibForUnmix = NA,
    .SigForUnmix = NA,
    .EForFit = NA,
    .calibForFit = NA,
    .SigForFit = NA,
    .intensity_scale_for_Sig = NA,
    .dims = NA, # for fit only
#    .em = NA,

### in dims required by UnmixCore
    .unmix_R_for_unmix = NA,
    .intensity_scale_for_R = NA,
    .unmix_R_for_fit = NA,
    .Rp_em = NA,
    .Ae = NA,
    .p = NA,
    .indice = NA,
    .fitted = NA,
    .coef = NA,
    .residuals = NA,
    .residuals_chisq = NA
  ) # EOF private
) #EOF EELS_unmix

#_end_vca_general_nobkg_prepare

#_end_TiO_single_template



UnmixCore = R6Class( "UnmixCore", 
  public = list(
    set_L_and_N = function() {
      private$.L<-dim(private$.R)[1]
      private$.N<-dim(private$.R)[2]
      invisible(self)
    },
    set_Ro = function() {
      private$.rm<-rowMeans(private$.R)
      private$.Rm <-matrix(private$.rm, nrow=length(private$.rm), ncol=private$.N)
      private$.Ro<-private$.R-private$.Rm
      invisible(self)
    },
    dim_reduction = function() {
      self$set_L_and_N()
      self$set_Ro()
      vca_R<-private$.R
      vca_p<-private$.p
      vca_pca<-NULL
      vca_L<-private$.L
      vca_N<-private$.N
      vca_rm<-private$.rm
      vca_Rm<-private$.Rm
      vca_Ro<-private$.Ro
      dr_method <- private$.dr_method
      pca_method<- private$.pca_method
      vca_noise_mag <- private$.noise_mag
      if (dr_method == "svd") {
        if (getOption('unmixeels.verbose') == T) {
          print("Info: using SVD for dimensionality reduction.")
        }
        if ( !is.na(pca_method) ) { # Use package pcaMethods
          if (getOption('unmixeels.verbose') == T) {
            print(sprintf("Info: using pcaMethods::pca with %s.", pca_method))
          }
          pcamethodres<-pcaMethods::pca(vca_R, center=F, method=pca_method, nPcs=vca_p)
          # vca_pca$u<-pcamethodres@scores
          vca_pca$u<-t(t(pcamethodres@scores)/sqrt(colSums(pcamethodres@scores^2)))
        } else {
          if (getOption('unmixeels.verbose') == T) {
            print("Info: using svd()")
          }
          vca_pca<-svd(vca_R, nu=vca_p)
        } # EOF !is.na()
        # proj into p dims
        vca_xp<-t(vca_pca$u)%*%vca_R
        # again in L dims
        vca_Rp<-vca_pca$u%*%vca_xp
        #noise level per point per dim
        vca_noise<-vca_noise_mag*as.double(sqrt((sum(c(vca_R)^2)/vca_N - sum(c(vca_xp)^2)/vca_N )/vca_L))
        # (fake) proj shift orig
        vca_xp_shifted<-vca_xp
        vca_xp_norm<-sqrt( colSums(vca_xp_shifted*vca_xp_shifted) )
        vca_xp_shifted_normalized<-vca_xp_shifted/t(matrix(vca_xp_norm, nrow=vca_N, ncol=vca_p))
      } else if (dr_method == 'pca') { # pca
        if (getOption('unmixeels.verbose') == T) {
          print("Info: using PCA for dimensionality reduction.")
        }
        if ( !is.na(pca_method) ) { # Use package pcaMethods
          if (getOption('unmixeels.verbose') == T) {
            print(sprintf("Info: using pcaMethods::pca with %s.", pca_method))
          }
          pcamethodres<-pcaMethods::pca(vca_R, center=T, method=pca_method, nPcs=vca_p)
          # vca_pca$u<-pcamethodres@scores
          vca_pca$u<-t(t(pcamethodres@scores)/sqrt(colSums(pcamethodres@scores^2)))
        } else {
          # Both u and v are u. They are the same
          if (getOption('unmixeels.verbose') == T) {
            print("Info: using svd()")
          }
          vca_pca<-svd(vca_Ro%*%t(vca_Ro)/vca_N, nu=vca_p, nv=vca_p)
        }
        #proj into p dims
        vca_xp<-t(vca_pca$u)%*%vca_Ro
        #again in L dims
        vca_Rp<-vca_pca$u%*%vca_xp + vca_Rm
        #noise level per point per dim
        vca_noise<-vca_noise_mag*as.double(sqrt((sum(c(vca_R)^2)/vca_N - sum(c(vca_xp)^2)/vca_N - t(vca_rm)%*%vca_rm)/vca_L))
        #proj shift orig
        vca_u<-t(vca_pca$u)%*%(-vca_rm)
        vca_xp_shifted<-vca_xp-matrix(c(vca_u), nrow=vca_p, ncol=vca_N)
        vca_xp_norm<-sqrt( colSums(vca_xp_shifted*vca_xp_shifted) )
        vca_xp_shifted_normalized<-vca_xp_shifted/t(matrix(vca_xp_norm, nrow=vca_N, ncol=vca_p))
      } else {
        message(sprintf("Error: dr_method `%d` not implemented!", dr_method))
      }#endif dr_method
      private$.u<-vca_pca$u
      private$.Rp<-vca_Rp
      private$.Xp<-vca_xp
      private$.Xp_shifted<-vca_xp_shifted
      private$.Xp_norm<-vca_xp_norm
      private$.Xp_shifted_normalized<-vca_xp_shifted_normalized
      private$.noise<-vca_noise
      invisible(self)
    },
    #_begin_nfindr
    calc_simplex_vol_001 = function(V) { # volume with another vertex at zero
      #  https://en.wikipedia.org/wiki/Solid_angle#Solid_angles_in_arbitrary_dimensions
      #  print(abs(det(V)))
      m_V<-as.matrix(V)
      abs(det(m_V))/norm(as.matrix(rowMeans(m_V)),type="F")
      #  calc_omega_compiled(m_V)
    },
    calc_simplex_vol = function(V) { # volume with another vertex at zero
      #  https://en.wikipedia.org/wiki/Solid_angle#Solid_angles_in_arbitrary_dimensions
      m_V<-as.matrix(V)
      abs(det(m_V))/factorial(dim(m_V)[1])
      #  calc_omega_compiled(m_V)
    },
    calc_simplex_face_area = function(V) {
      #   V = QR
      #        |   |         |    R11 R12 R13 ...
      #     = (q1, q2, ... , qn) (    R22 R23 ...)
      #     =  |   |         |            R33 ...
      #
      #     =(q1 R11, q1 R12 + q2 R22, ....)
      #
      #   m_V<-as.matrix(V)#;print(m_V)
      m_V<-V
      d<-dim(m_V)[1]
      if (!is.null(d) && d>1) {
#      if (d>1)  {
        m_V<-m_V-m_V[,1]
        ##  use qr
        qrres<-qr(m_V)
        V_in_low_d<-qr.R(qrres)[1:(d-1),1:(d-1),drop=F]#;print(V_in_low_d)
        #fa<-abs(prod(diag(V_in_low_d)))#abs(det(V_in_low_d))
        fa<-self$calc_simplex_vol(V_in_low_d)
        ## use svd
        #   svdres<-svd(m_V)
        #   fa<-abs(prod(svdres$d[-length(svdres$d)]))
      } else {
#        fa<-V
         fa<-0
      }
      fa
    },
    #  https://en.wikipedia.org/wiki/Solid_angle#Solid_angles_in_arbitrary_dimensions
    calc_omega = function(V, max_order=4) {
      V<-as.matrix(V)
      d<-dim(V)[1];n<-d
      omega_d<-2*pi^(d/2)/gamma(d/2)
      absdetV<-(abs(det(V)))#;print(absdetV)
      alpha<-t(V)%*%(V)
      cn2<-choose(n,2)
      # given a max_order, calculate nterms possible a matrix
      nterms<-factorial(cn2+max_order)/factorial(max_order)/factorial(cn2)
      a_all<-array(0,dim=c(n,n,nterms))
      # fill a_all
      # T is barrier (cn2-1), F is coin (max_order)
      bes_tmp_01<-simplify2array(permn(cn2+max_order))
      bes_tmp_02<-(bes_tmp_01<=cn2)*1#bes_tmp_01
      bes<-unique( bes_tmp_02 ,MARGIN=2)
      bes_tmp_1<-matrix(which(bes==1,arr.ind=T)[,1],ncol=ncol(bes))
      bes_tmp_2<-rbind(bes_tmp_1,(cn2+1+max_order))-rbind(0,bes_tmp_1)-1
      for (term in seq_len(nterms)) {
        a<-a_all[,,term]
        a[lower.tri(a)]<-c(bes_tmp_2[seq_len(cn2),term])
        a<-a+t(a)
        a_all[,,term]<-a
      }
      termval<-sum(sapply(seq_len(nterms),function(term){
        # calc alpha^a
        a<-a_all[,,term]
        alpha_a<-prod(unlist(sapply(seq_len(n-1),function(i){sapply((i+1):n,function(j){alpha[i,j]^a[i,j]})})))
        # calc -2sumaij, piaijfactorial
        m2sumaij<-(-2)^sum(unlist(sapply(seq_len(n-1),function(i){sapply((i+1):n,function(j){a[i,j]})})))
        piaijfactorial<-prod(unlist(sapply(seq_len(n-1),function(i){sapply((i+1):n,function(j){factorial(a[i,j])})})))
        pigamma<-prod(sapply(seq_len(n),function(i){gamma((1+sum(a[i,]))/2)}))
        # calc sumaim
        # calc pigamma
        m2sumaij/piaijfactorial*pigamma*alpha_a
        # addup per a term
      })) #end of sum a in N^(d,2)
      omega<-omega_d*absdetV/(4*pi)^(d/2)*termval
      omega
    },
    set_em_indice = function(ind_input) {
      if ( (min(ind_input)<1) || (max(ind_input)>private$.N) ) {
        message(sprintf("Error: vca_N is %d, indice input is out of range!", ind_input))
      }
      private$.indice <- ind_input
      invisible(self)
    },
    conv_knn_weighted_ave = function() {

      vca_xp_shifted_normalized <- private$.Xp_shifted_normalized
      vca_xp_norm <- private$.Xp_norm
      vca_N <- private$.N
      vca_p <- private$.p
      vca_noise<-private$.noise
      manifold_dim<-private$.manifold_dim
      cut_off_prob<-1-1/vca_N#/1e9
      cut_off_radius<-2*vca_noise/vca_xp_norm*sqrt(qchisq(cut_off_prob, vca_p))
      private$.cut_off_radius<-cut_off_radius
      vca_xp_new<-vca_xp_shifted_normalized
      pps_lens<-rep(0,vca_N)
      center_of_mass_all<-rowMeans(vca_xp_shifted_normalized)
      for (i in seq_len(vca_N)) {
        dist_i<- sqrt(colSums((vca_xp_shifted_normalized-vca_xp_shifted_normalized[ ,i])^2))
        pps<-which(dist_i<cut_off_radius[i])
        pps_lens[i]<-length(pps)
        # row: vca_p dims (repeated) ; col: pps pts
        weight_from_sigma<-t(matrix(vca_xp_norm[pps,drop=F]^2,ncol=vca_p,nrow=length(pps)))
	center_of_mass<-rowMeans(vca_xp_shifted_normalized[,pps,drop=F])
	m_direct<-center_of_mass-vca_xp_shifted_normalized[,i]
	if (sum(m_direct^2)>0) { # only one pt in that sphere
          m_direct<-m_direct/sqrt(sum(m_direct^2))
          proj_v<-m_direct%*%(vca_xp_shifted_normalized[,pps,drop=F]-vca_xp_shifted_normalized[,i])
          # proj_v*vca_xp_norm, norm here is for compatibility, can be deleted. 
          # row: vca_p dims (repeated) ; col: pps pts 
          weight_from_distance<-t(
            matrix(exp(-abs(proj_v)*vca_xp_norm[i]/vca_noise/(1/sqrt(manifold_dim))),
            ncol=vca_p,nrow=length(pps)))
	} else { # the point itself
          weight_from_distance<-1
	}
        total_weight<-weight_from_sigma*weight_from_distance
        # sum over all points in each dimension (result in vca_p dims)
        total_weight<-total_weight/rowSums(total_weight)
        # apply weighting
        vca_xp_new[,i]<-rowSums(vca_xp_shifted_normalized[,pps,drop=F]*total_weight)
        # proj back as vca_xp, on the sphere
        vca_xp_new[,i]<-vca_xp_new[,i]/norm(as.matrix(vca_xp_new[,i]),type="F")*vca_xp_norm[i]
      } # endof for i in 1:vca_p

      vca_xp_new_norm<-sqrt( colSums(vca_xp_new*vca_xp_new) )
      vca_xp_new_normalized<-vca_xp_new/t(matrix(vca_xp_new_norm, nrow=vca_N, ncol=vca_p))

      private$.Xp_shifted_normalized_old<-private$.Xp_shifted_normalized
      private$.Xp_shifted_normalized<-vca_xp_new_normalized

      invisible(self)
    },
    vca = function(vca_xp, vca_p) {
      vca_u <- rowMeans(vca_xp)
      vca_y <- vca_xp #/ t(replicate(vca_p, colSums( vca_xp*replicate(vca_N, vca_u) )))
      vca_indice <- array(0, dim=c(1,vca_p))
      vca_A <- array(0, dim=c(vca_p,vca_p))
      vca_A[vca_p,1] <- 1

      for (i in seq_len(vca_p)) {
        w <- array(runif(vca_p*1), dim=c(vca_p,1))
        f <- w - vca_A%*%ginv(vca_A)%*%w

        f <- f / sqrt(sum(f^2))
        if (vca_p==1) {f<-1}

        v <- t(f)%*%vca_y
        v_max <- max(abs(v))
        vca_indice[i] <- which.max(abs(v))
        vca_A[,i] <- vca_y[,vca_indice[i]]	# same as x(:,indice(i))
      }
      vca_indice<-sort(vca_indice)
      out<-NULL
      out$indice<-vca_indice
      out$A<-vca_A
      out
    },
    nfindr_all = function(vca_p, data, algo) {

my_perm<-function(m,n){
  pool<-rep(0,m)

  out_all<-NULL
  out_i <- 1
  pool_i<- 1
  #dbg<-1
  fi<-F
  while ( !fi ) {
    if (pool[pool_i]==0) {
      pool[pool_i] <- out_i
      out_i<-out_i+1
      if (out_i==n+1) {
        out<-order(pool+1e9*(0==pool))
        out_all<-cbind(out_all,out)
        while (1) {
          out_i<-out_i-1
          if (out_i == 0) {
            fi<-T
            break
          }
          out_i_ind<-which(pool==out_i)
          if (prod(pool[out_i_ind:m])!=0) {
            pool[out_i_ind]<-0
            pool_i<-which(pool==out_i-1)
          } else {
            pool[out_i_ind]<-0
            pool_i<-pool_i+1
            break
          }
        }
      } else {
        pool_i<-1
      }
    } else {
      pool_i<-pool_i+1
    }
  }
  out_all
}

       if (!(algo %in% c("1-in","p-sc-in"))) {
         message(sprintf("Error: algo %s not supported!", algo))
       }
#       if (getOption('unmixeels.verbose') == T) {
#         print(sprintf("Info: using %s-nfindr"))
#       }
#      if (vca_p>1) { 

        vca_N <- dim(data)[2]
#        nfindr_indice_old<-sample(1:dim(data)[2],vca_p)
        nfindr_indice_old<-seq_len(vca_p)

        nfindr_pass_all<-my_perm(vca_p, vca_p)#;print(nfindr_pass_all)
        for (nfindr_pass_i in 1:ncol(nfindr_pass_all)) {
          nfindr_pass<-nfindr_pass_all[,nfindr_pass_i]
          if (getOption('unmixeels.verbose') == 2) {
            print("Info: nfindr pass:");print(nfindr_pass)
          }
          nfindr_indice_old<-nfindr_indice_old[nfindr_pass]
         while (1) {

          nfindr_indice_best<-nfindr_indice_old

#          if (algo == "p-sc-in")  {
            nfindr_indice_now <- nfindr_indice_old
            max_vol<-self$calc_simplex_face_area(data[, nfindr_indice_now,drop=F])
#          }
          for (i in seq_len(vca_p)) {
#            if (algo == "1-in" ) {
#              max_vol<-0
#              nfindr_indice_now <- nfindr_indice_old
#              max_vol<-self$calc_simplex_face_area(data[, nfindr_indice_now,drop=F]) ; print("mv") ; print(max_vol) ; print("@") ; print(nfindr_indice_old)
#            }
            for (j in seq_len(vca_N)) {
#              if ( (algo == "1-in") && (j %in% nfindr_indice_best) ) {
#                next
#              }
              nfindr_indice_now[i]<-j 
              if (getOption('unmixeels.verbose') == 3) {
                print(nfindr_indice_now)
              }

              now_vol<-self$calc_simplex_face_area(data[, nfindr_indice_now,drop=F])

              if ( now_vol > max_vol) {
                if (getOption('unmixeels.verbose') == 2) {
                  print(nfindr_indice_now);print(sprintf("Info: n_findr max_vol improved! Old vol %f, New vol %f", max_vol, now_vol))
                }
                max_vol<-now_vol
                if (algo == "p-sc-in")  {
                  nfindr_indice_best<-nfindr_indice_now
                } else if (algo == "1-in" ) {
                  nfindr_indice_best[i]<-j
                }
              }
            } # EOF j in 1:vca_N
            if (algo == "p-sc-in") {
              nfindr_indice_now<-nfindr_indice_best
            }
          } # EOF i in 1:vca_p
          # test if improved
          if (prod(nfindr_indice_best == nfindr_indice_old)) {
            break 
          } else {  # break while
            nfindr_indice_old<-nfindr_indice_best
          }
         } # EOF while (1)


        } # EOF nfindr_pass
#      }
       if (getOption('unmixeels.verbose') == T) {
         print("Info: best_indice by nfindr:");print(sort(nfindr_indice_best))
       }
      out<-NULL
      out$indice<-sort(nfindr_indice_best)
      out
    },
    em_extraction = function() {
      vca_xp <- private$.Xp
      vca_xp_shifted_normalized<-private$.Xp_shifted_normalized
      vca_xp_norm <- private$.Xp_norm
      vca_N <- private$.N
      vca_p <- private$.p
      vca_noise<-private$.noise
      em_algo<- private$.em_algo
      if (em_algo == 'vca') {
        if (getOption('unmixeels.verbose') == T) {
          print("Info: using VCA for endmember extraction. ")
        }
        out<-self$vca(vca_xp, vca_p)
        self$set_em_indice(out$indice)
      } else if (em_algo == 'p-sc-in-nfindr') {
        if (getOption('unmixeels.verbose') == T) {
          print("Info: using p-SC-IN-N-FINDR for endmember extraction. ")
        }
        # algo %in% c("1-in","p-sc-in")
        out<-self$nfindr_all(vca_p,vca_xp_shifted_normalized,"p-sc-in")
        self$set_em_indice(out$indice)
      } else if (em_algo == "1-in-nfindr") {
        if (getOption('unmixeels.verbose') == T) {
          print("Info: using 1-IN-N-FINDR for endmember extraction. ")
        }
        # algo %in% c("1-in","p-sc-in")
        out<-self$nfindr_all(vca_p,vca_xp_shifted_normalized,"1-in")
        self$set_em_indice(out$indice)
      } else if (is.na(private$.indice)){
        message("Error: either `em_algo` is not supported, or indice is not set manually")
      }
      invisible(self)
    },
    set_param = function(dr_method = 'svd', pca_method = NA, noise_mag = 1, em_algo = 'p-sc-in-nfindr', optimize_em = T, manifold_dim = 1) {
      private$.dr_method <- dr_method
      private$.pca_method<- pca_method
      private$.noise_mag <- noise_mag
      private$.em_algo   <- em_algo
      private$.optimize_em<-optimize_em
      private$.manifold_dim<-manifold_dim
      invisible(self)
    },
    set_R_and_scale = function(vca_R, intensity_scale) {
      private$.intensity_scale <- intensity_scale
      private$.R<-vca_R
      invisible(self)
    },
    set_Rp_em_from_Xp = function() {
      vca_N <- private$.N
      vca_p <- private$.p
      vca_noise<-private$.noise
      vca_indice<-private$.indice
      manifold_dim<-private$.manifold_dim
      vca_xp<-private$.Xp
      vca_xp_shifted_normalized <- private$.Xp_shifted_normalized
      vca_xp_norm <- private$.Xp_norm
      dr_method<-private$.dr_method
      vca_Rm<-private$.Rm
      vca_u<-private$.u
      vca_xp_em<-vca_xp[,vca_indice,drop=F]
      if (dr_method == "svd") {
        vca_Rp_em<-vca_u%*%vca_xp_em
      } else if (dr_method == "pca") {
        vca_Rp_em<-vca_u%*%vca_xp_em+vca_Rm
      }
      private$.Rp_em<-vca_Rp_em
      invisible(self)
    },
    set_Ae = function(Ae) {
      private$.Ae<-Ae
      invisible(self)
    },
    set_Ae_from_Rp_em_and_scale = function() {
      vca_L<-private$.L
      Rp_em<-private$.Rp_em
      vca_p<-private$.p

      intensity_scale<-private$.intensity_scale
      indice<-private$.indice
      intensity_scale_em<-t(matrix(intensity_scale[indice],nrow=vca_p , ncol=vca_L))
      Ae<-Rp_em / intensity_scale_em
      self$set_Ae(Ae)
      invisible(self)
    },
    # use R and Ae
    mlls = function() {
      # lm: y = x %*% a + b
      vca_R<-private$.R
      Ae<-private$.Ae
      fm0<-lm(vca_R ~ Ae + 0)
      fitted   <- fm0$fitted.values
      # coef has p rows and N cols. 
      coef     <- fm0$coefficients
      residuals<- fm0$residuals
      private$.fitted<-fitted
      private$.coef  <-coef
      private$.residuals<-residuals
      private$.fm0<-fm0
      invisible(self)
    },
    set_residuals_chisq = function() {
      residuals <- private$.residuals
      vca_noise <- private$.noise
      vca_L <- private$.L
      vca_p <- private$.p
      vca_N <- private$.N

      chisq_2d <- (residuals)^2/vca_noise
      chisq_line_ave<-colSums((residuals)^2)/(vca_noise^2*(vca_L-vca_p))
      chisq_lb<-qchisq(1-(.975)^(1/vca_N),vca_L-vca_p)/(vca_L-vca_p)
      chisq_ub<-qchisq((.975)^(1/vca_N),vca_L-vca_p)/(vca_L-vca_p)
      residuals_chisq<-NULL
      residuals_chisq$noise<-vca_noise
      residuals_chisq$chisq_2d<-chisq_2d
      residuals_chisq$line_ave<-chisq_line_ave
      residuals_chisq$lb<-chisq_lb
      residuals_chisq$ub<-chisq_ub
      private$.residuals_chisq<-residuals_chisq
      invisible(self)
    },
    residual_chisq_test = function() {
      residuals_chisq<-private$.residuals_chisq
      chisq_line_ave<-residuals_chisq$line_ave
      chisq_lb<-residuals_chisq$lb
      chisq_ub<-residuals_chisq$ub
      overfit<-sum(chisq_line_ave<chisq_lb)
      underfit<-sum(chisq_line_ave>chisq_ub)
      if ( (underfit == 0) && (overfit == 0) ) {
        if (getOption('unmixeels.verbose') == T) {
          message("Info: chisq test passed. ")
        }
        ret <- 0
      } else if ( underfit > 0 ) {
        if (getOption('unmixeels.verbose') == T) {
          message(sprintf("Warn: chisq test failed, underfit pts %d, overfit pts %d. ", underfit, overfit))
        }
        ret <- underfit
      } else if ( overfit >0 ) {
        if (getOption('unmixeels.verbose') == T) {
          message(sprintf("Warn: chisq test failed, underfit pts %d, overfit pts %d. ", underfit, overfit))
        }
        ret <- -overfit
      }
      ret
    },
    set_p = function(p) {
      private$.p <- p
      invisible(self)
    },
    do_dim_reduction_and_em_extraction_from_p = function() {
      self$dim_reduction()
      if (private$.optimize_em) {
        self$conv_knn_weighted_ave()
      }
      self$em_extraction()
      self$set_Rp_em_from_Xp()
      self$set_Ae_from_Rp_em_and_scale()
      self$mlls()
      self$set_residuals_chisq()
      invisible(self)
    },
    find_p = function(max_p) {
      private$.p <- 1
      max_p<-max_p+1
      while (private$.p < max_p) {
        if (getOption('unmixeels.verbose') == T) {
          message(sprintf("Info: Trying p = %d", private$.p))
        }
        self$do_dim_reduction_and_em_extraction_from_p()
        chisq_test_res <- self$residual_chisq_test()
        if ( chisq_test_res <= 0 ) {
          if (getOption('unmixeels.verbose') == T) {
            message("Info: Stop searching for p.")
          }
          break
        }
        private$.p = private$.p + 1
      }
      if (private$.p == max_p) {
        message(sprintf("Warn: could not find p < %d! Stop at p=%d.", max_p,max_p-1))
        private$.p<-private$.p-1
      }
      invisible(self)
    },
    get_p_Rpem_Ae_fitted_coef_residuals = function() {
      out<-NULL
      out$p<-private$.p
      out$Rp_em<-private$.Rp_em
      out$Ae<-private$.Ae
      out$indice<-private$.indice
      out$fitted<-private$.fitted
      out$coef<-private$.coef
      out$residuals<-private$.residuals
      out$residuals_chisq<-private$.residuals_chisq
      out
    },
    do_plot = function(map_type="b", fig_out = c(T,T,T,T,T)) {
      plot_signature <- fig_out[1]
      plot_score     <- fig_out[2]
      plot_Xp_shifted_normalized <- fig_out[3]
      plot_xp_shifted_12 <-fig_out[4]
      plot_xp_shifted_13 <-fig_out[5]
      vca_u<-private$.u
      vca_indice<-private$.indice
        vca_p<-dim(private$.Xp_shifted_normalized)[1]
        vca_N<-dim(private$.Xp_shifted_normalized)[2]
      if (private$.optimize_em) {
        vca_xp_shifted_new<-private$.Xp_shifted_normalized*t(matrix(private$.Xp_norm, nrow=vca_N, ncol=vca_p))
        vca_xp_shifted<-private$.Xp_shifted
      } else {
        vca_xp_shifted<-private$.Xp_shifted
      }
      if (plot_signature) {
        plot(vca_u[,1], ylim=range(vca_u), xlab="channel", ylab="Signature",type="l",col=2)
        if (dim(vca_u)[2]>1) {
          for (i in 2:dim(vca_u)[2]) {
            lines(vca_u[,i],col=i+1)
          }
        }
      } #EOF plot_signature
      if (plot_score) {
        plot(vca_xp_shifted[1,], ylim=range(vca_xp_shifted), xlab="px", ylab="Scores",type="l", col=2)
      }
      if (vca_p>1) {
       if (plot_score) {
        for (i in 2:vca_p) {
          lines(vca_xp_shifted[i,], col=i+1)
        }
       }

      if (plot_Xp_shifted_normalized) {
      plot(private$.Xp_shifted_normalized_old[1,],
        private$.Xp_shifted_normalized_old[2,],pch=4,col="red",
        xlab="Xp_shifted_normalized[1,]",
        ylab="Xp_shifted_normalized[2,]",
        xlim=c(-1.5,1.5),ylim=c(-1.5,1.5),asp=1)
      has_draw_circle<-require("plotrix") 
      for (k in seq_len(vca_N)) { 
        if (has_draw_circle) {
          draw.circle(  private$.Xp_shifted_normalized_old[1,k],
            private$.Xp_shifted_normalized_old[2,k],private$.cut_off_radius[k],
            border=rgb(127, 8, 0, max = 255, alpha = 32)  )
        }
        lines(c(private$.Xp_shifted_normalized[1,k],private$.Xp_shifted_normalized_old[1,k]),
          c(private$.Xp_shifted_normalized[2,k],private$.Xp_shifted_normalized_old[2,k]),
          col=rgb(191, 191, 0, max = 255, alpha = 127) )
      }
      points(private$.Xp_shifted_normalized[1,],private$.Xp_shifted_normalized[2,],pch=3)
      } #EOF plot_Xp_shifted_normalized

      if (plot_xp_shifted_12) {
        plot(vca_xp_shifted[1,], vca_xp_shifted[2,],pch=3,cex=.5,col=rainbow(vca_N),type=map_type,asp=1,xlab="Xp_shifted[1,]/arb. unit",ylab="Xp_shifted[2,]/arb. unit")
        if (private$.optimize_em) {
          points(vca_xp_shifted_new[1,], vca_xp_shifted_new[2,],pch=3,cex=.7,asp=1)
          points(0,0,pch=10,cex=5,col="gray")
          for (j in seq_along(vca_indice)) {
            lines(c(0,vca_xp_shifted_new[1,vca_indice[j]]), c(0,vca_xp_shifted_new[2,vca_indice[j]]), col="gray")
          }
        }
        points(vca_xp_shifted_new[1,vca_indice],vca_xp_shifted_new[2,vca_indice],pch="+",cex=5,col=1+(1:vca_p))
        grid()
      } #EOF plot_xp_shifted_12
      } #EOF vca_p>1
      if (vca_p>2) {
       if (plot_xp_shifted_13) {
        for (k in 3:vca_p) {
          plot(vca_xp_shifted[1,], vca_xp_shifted[k,],pch=3,cex=.5,col=rainbow(vca_N),type=map_type,asp=1,xlab="Xp_shifted[1,]/arb. unit",ylab="Xp_shifted[3,]/arb. unit")
          points(vca_xp_shifted_new[1,], vca_xp_shifted_new[k,],pch=3,cex=.5,asp=1)
          points(0,0,pch=10,cex=5,col="gray")
          for (j in seq_along(vca_indice)) {
            lines(c(0,vca_xp_shifted_new[1,vca_indice[j]]), c(0,vca_xp_shifted_new[k,vca_indice[j]]), col="gray")
          }
          points(vca_xp_shifted_new[1,vca_indice],vca_xp_shifted_new[k,vca_indice],pch="+",cex=5,col=1+(1:vca_p))
          grid()
        }
       }
      }
#      image(vca_xp_shifted_normalized)
    }
# add built-in data set
  ),
  private = list(
    .R = NA,
    .N = NA,
    .L = NA,
    .p = NA,
    .rm = NA,
    .Rm = NA,
    .Ro = NA,
    .u = NA,
    .Rp = NA,
    .Ae = NA,
    .intensity_scale = NA,
    .Rp_em = NA,
    .Xp = NA,
    .Xp_shifted = NA,
    .Xp_shifted_normalized = NA,
    .Xp_shifted_normalized_old = NA,
    .Xp_norm = NA,
    .cut_off_radius = NA,
    .noise = NA,
    .indice = NA, 
    .dr_method = 'svd', 
    .pca_method = NA,
    .noise_mag = 1,
    .em_algo = 'p-sc-in-nfindr',
    .optimize_em = T,
    .manifold_dim = 1,
    .fitted = NA,
    .coef = NA,
    .residuals = NA,
    # residuals_chisq is a list
    .residuals_chisq = NA,
    .fm0 = NA
  ) # EOF private
) #EOF UnmixCore


SimulatedData<-R6Class( "SimulatedData",
  public = list(
    initialize = function(type = "particle") {
      nchannel<-private$.nchannel
      s_dark<-private$.s_dark
      E_seq<-seq(from=390.0,by=0.1,length.out=nchannel)
      dark_ref<-s_dark/sqrt(2)*rnorm(nchannel)
      x_flat<-private$.x_flat
      if (type=="interface") {
        x_drift_arr_all_int <- round(20*(1-cos(seq_len(dim(volcano)[2])/100)))
        e_drift_along_x_arr_all_int<- round(0*sin(seq_len(dim(volcano)[1]+x_flat)/100))
        e_drift_along_y_arr_all_int<- round(40*(1-cos(seq_len(dim(volcano)[2])/100)))
        private$.x_drift_arr_all_int<-x_drift_arr_all_int
        private$.e_drift_along_x_arr_all_int<-e_drift_along_x_arr_all_int
        private$.e_drift_along_y_arr_all_int<-e_drift_along_y_arr_all_int
      }
      private$.type<-type
      private$.E_seq<-E_seq
      private$.dark_ref<-dark_ref
      self$set_imgAndSI()
      invisible(self)
    },
    get_origDkRf = function() {
      private$.origDkRf
    },
    set_imgAndSI = function() {
      x_flat<-private$.x_flat
      s_dark<-private$.s_dark
      nchannel<-private$.nchannel
      dark_ref<-private$.dark_ref
      E_seq<-private$.E_seq
      type<-private$.type
      origADFObj<-OrigADF$new()$set_ADF(
        2,
        (function(){
          calib<-data.frame(Origin=c(0,0),Scale=c(1,1),Units=rep(NA,2))
          calib[,3]<-c("nm","nm")
          calib
        })(),
        (function(){
          if (type=="particle") {
            out<-volcano
          } else if (type=="interface") {
            x_drift_arr_all_int<-private$.x_drift_arr_all_int
            slice<-volcano[,which.max(colSums(volcano))]
            out<-rbind(replicate(dim(volcano)[2],slice),array(min(slice),dim=c(x_flat,dim(volcano)[2])))
            # ImgAndSpecAlign$public_methods$img_x_drift_by_arr_cut(x_drift_arr_all_int,out)
            out
          }
          out
        })()
      )
      origSIObj<-OrigSI$new()$set_SI(
        3,
        data.frame(Origin=c(0,0,-3900),Scale=c(1,1,0.1),Units=c("nm","nm","eV")),
        E_seq,
        (function(){
          bkg_seq<-exp(27.56-2.20*log(E_seq))/(210*43)
          edge_pos_E<-457
          edge_pos_X<-which.min(abs(E_seq-edge_pos_E))
          E_seq_sig<-seq(from=0,by=0.1,length.out=length(E_seq)-edge_pos_X+1)
          sig_0<-E_seq*0+c(rep(0, edge_pos_X-1), 100*(besselJ(E_seq_sig,1))^2)
          sig_1<-E_seq*0+c(rep(0, edge_pos_X-1), 100*(besselJ(E_seq_sig,2))^2)
          sig_2<-E_seq*0+c(rep(0, edge_pos_X-1), 100*(besselJ(E_seq_sig,3))^2)
          cnt2d<-origADFObj$Cnt
          cnt_2d_n<-(cnt2d-min(cnt2d))/(max(cnt2d)-min(cnt2d))
          sc<-2
          ab0<-(cos( (cnt_2d_n*(cnt_2d_n<.5)+.5*(cnt_2d_n>=.5))*pi-0*pi/2)^2)*cnt2d/max(cnt2d)*sc
          ab1<-(cos(  cnt_2d_n*pi                                 -1*pi/2)^2)*cnt2d/max(cnt2d)*sc
          ab2<-(cos( (cnt_2d_n*(cnt_2d_n>.5)+.5*(cnt_2d_n<=.5))*pi-0*pi/2)^2)*cnt2d/max(cnt2d)*sc
          SI_dim<-c(dim(cnt2d),nchannel)
          Cnt<-array(s_dark*rnorm(prod(SI_dim)), dim=SI_dim)        + # dark noise
              (function(){
                spike_list<-array(rnorm(prod(SI_dim)), dim=SI_dim)>5
                print(sprintf("Info: number of simulated noise spikes in SI: %d", sum(spike_list)))
                spike_list*3000
              })()                                                  + # spike noise
              aperm(array(bkg_seq, dim=SI_dim[c(3,1,2)]),c(2,3,1))  + # background
              outer(ab0,sig_0)+outer(ab1,sig_1)+outer(ab2,sig_2)      # signal
          Cnt<-(Cnt+abs(Cnt))/2+1e-5
          Cnt<-Cnt+sqrt(Cnt)/sqrt(2)*rnorm(length(Cnt))               # Poisson shot noise (\sqrt(2) for CTF of CCD)
          if (type == "particle") {
            Cnt<-Cnt + aperm(array(dark_ref, dim=SI_dim[c(3,1,2)]),c(2,3,1)) # dark reference
          }
          Cnt
        })()
      )
      imgAndSI<-ImgAndSpecAlign$new(origSIObj, origADFObj)
      if (type=="interface") {
        x_drift_arr_all_int<-private$.x_drift_arr_all_int
        e_drift_along_x_arr_all_int<-private$.e_drift_along_x_arr_all_int
        e_drift_along_y_arr_all_int<-private$.e_drift_along_y_arr_all_int
        imgAndSI$img_set_x_drift_arr(x_drift_arr_all_int)
        imgAndSI$ADF_do_x_drift_corr_from_arr()
        imgAndSI$SI_do_x_drift_corr_from_arr()
        imgAndSI$spec_set_e_drift_along_y_arr(e_drift_along_y_arr_all_int)
        imgAndSI$SI_do_e_drift_along_y_corr_from_arr()
        imgAndSI$get_SI()$transpose_xy()
        e_drift_along_x_arr_all_int<-e_drift_along_x_arr_all_int[seq_len(dim(origADFObj$Cnt)[1])]
        imgAndSI$spec_set_e_drift_along_y_arr(e_drift_along_x_arr_all_int)
        imgAndSI$SI_do_e_drift_along_y_corr_from_arr()
        imgAndSI$get_SI()$transpose_xy()
        # fix dkrf channels
        nchannel<-dim(imgAndSI$get_SI()$Cnt)[3]
        dark_ref<-dark_ref[seq_len(nchannel)]
        cnt_with_dkrf<-origSIObj$Cnt + aperm(array(dark_ref, dim=dim(origSIObj$Cnt)[c(3,1,2)]),c(2,3,1)) # dark reference
        origSIObj$set_Cnt(cnt_with_dkrf)
      }
      private$.imgAndSI<-imgAndSI
      origDkRf<-OrigSI$new()$set_SI(
        3,
        (function(){
          calib<-data.frame(Origin=c(0,0,-3900),Scale=c(1,1,0.1),Units=rep(NA,3))
          calib[,3]<-c("nm","nm","eV")
          calib
        })(),
        imgAndSI$get_SI()$E,
        (function(){
          DkRf_dim<-c(ceiling(sqrt(dim(volcano))),nchannel)
          aperm(array(dark_ref, dim=DkRf_dim[c(3,1,2)]),c(2,3,1)) +    # dark reference
            array(s_dark*rnorm(prod(DkRf_dim)), dim=DkRf_dim)     +    # dark noise
            (array(rnorm(prod(DkRf_dim)), dim=DkRf_dim)>4)*1000        # spike noise
          })()
      )
      private$.origDkRf<-origDkRf
      invisible(self)
    },
    get_origADF = function() {
      private$.imgAndSI$get_ADF()
    },
    get_origSIObj = function() {
      private$.imgAndSI$get_SI()
    }
  ),
  private = list(
    .type	= NULL, # interface/particle
    .s_dark	= 35,
    .nchannel	= 2048,
    .E_seq	= NULL, 
    .dark_ref	= NULL, 
    .x_drift_arr_all_int	= NULL, 
    .e_drift_along_x_arr_all_int= NULL, 
    .e_drift_along_y_arr_all_int= NULL, 
    .x_flat	= 26, 
    .imgAndSI	= NULL, 
    .origDkRf   = NULL
  ),
) # EOF SimulatedData





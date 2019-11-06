dyn.load("libdmformat.so")


getEELSSpectra<-function(filename){
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

	origSpec<-NULL
	origSpec$numberOfDims<-DM_get_image1$dimension_len
	origSpec$calib<-as.list(rep(NA,3))
	names(origSpec$calib)<-c("Origin","Scale","Units")
	origSpec$calib$Origin<-DM_get_image1$Origin
	origSpec$calib$Scale<-DM_get_image1$Scale
	origSpec$calib$Units<-DM_get_image2$Units
	dim_arr<-DM_get_image1$dimensions[2*(1:(origSpec$numberOfDims))-1]
	for (i in 1:length(origSpec$calib$Units)) {
		if (origSpec$calib$Units[[i]]=="eV")
		origSpec$E<-seq(from=-origSpec$calib$Scale[[i]]*origSpec$calib$Origin[[i]],by=origSpec$calib$Scale[[i]],length.out=dim_arr[i])
	}
	if (is.null(origSpec$E)) {
		print(sprintf("Error: Unit of %dnd dimension is not eV, but %s", origSpec$numberOfDims, origSpec$calib$Units[[origSpec$numberOfDims]]))
	}
	origSpec$Cnt<-array(data=DM_get_image2$data,dim=dim_arr)

	return(origSpec)
}


getADFData<-function(filename,withname){
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
			message(sprintf("data_type %d is not implemented in getADFData!", M_get_image1$data_type))
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
			message(sprintf("data_type %d is not implemented in getADFData!", M_get_image1$data_type))
		}
	} else {
		message(sprintf("DMversion %d is not supported", dm_version))
		return(NULL)
	}
	DM_filemunmap<-.C("DM_file_munmap", DM_filemap$addr,  DM_filemap$fd)

	ADFData<-NULL
	ADFData$numberOfDims<-DM_get_image1$dimension_len
	ADFData$calib<-as.list(rep(NA,3))
	names(ADFData$calib)<-c("Origin","Scale","Units")
	ADFData$calib$Origin<-DM_get_image1$Origin
	ADFData$calib$Scale<-DM_get_image1$Scale
	ADFData$calib$Units<-DM_get_image2$Units
	#print(DM_get_image1$dimensions)
	dim_arr<-DM_get_image1$dimensions[2*(1:(ADFData$numberOfDims))-1]
	#print(dim_arr)
	ADFData$Cnt<-array(data=DM_get_image2$data,dim=dim_arr)

	return(ADFData)
}


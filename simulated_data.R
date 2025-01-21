################# EXAMPLE OF AN INTERFACE ###############
source("unmixeels.R")
# subplot in 3x3
par(mfcol=c(3,3))
# set random seed to make results reproducible 
set.seed(3)

# To use simulated dataset or not
use_simulated_data<-T

if (!use_simulated_data) {

# If not using the simulated dataset, specify the file names here. 

# This is the file of lines scan/spectral image in dm3/dm4 format.
  FILENAME_SI<-"my_si.dm4"
# This is the simultaneously collected annular dark-field image.
  FILENAME_ADF = "my_adf.dm4"
# This is the "dark reference". 
# In your EELS experiment, before or after collection the SI, collect 
# another SI with less pixels without the electron beam as the dark reference. 
  FILENAME_DKRFDK<-"my_dkrf.dm4"

  origDkRf<-OrigSI$new()$set_SI_from_EELS_file(FILENAME_DKRFDK)
  origSIObj<-OrigSI$new()$set_SI_from_EELS_file(FILENAME_SI)
  origADFObj<-OrigADF$new()$set_ADF_from_ADF_file(FILENAME_ADF,"SI")
} else {
# Here we use the simulated data as a demo. 
  SimulatedData<-SimulatedData$new(type = "interface") # "interface" or "particle"
  origADFObj <- SimulatedData$get_origADF()
  origDkRf   <- SimulatedData$get_origDkRf()
  origSIObj  <- SimulatedData$get_origSIObj()
}


# == skip below if dark reference correction is not needed. ===
# remove spike noise in the dark reference, then remove the dark reference from SI
origDkRf_xray_clean<- SpikeNoise$new()$set_spec_with_xray(origDkRf)$do_convert_spec_1()$get_spec_no_xray()

# remove dark reference
darkCorrectedSI<-DarkRef$new(
  origDkRf_xray_clean, 1, origSIObj)$do_remove_dkRf()$get_corrected_SI()

# === skip the two steps above and un-comment the following if dark reference correction is not needed. ===
# darkCorrectedSI<-origSIObj

# correct the spike noise in the SI
spikeCorrectedSI<-SpikeNoise$new(channelRes=2)$set_spec_with_xray(darkCorrectedSI$crop_e(3,2046,'px')$shift_e(-1.7))$do_remove_spike_3d_2()$set_protected_chs(data.frame(l=c(452,522),h=c(470,560)))$conv_neighbor_px_interpolation(function(u,d,l,r){rowMeans(cbind(u,d,l,r),na.rm=T)})$get_spec_no_xray()

# link the ADF and the SI
imgAndSI<-ImgAndSpecAlign$new(spikeCorrectedSI, origADFObj)

# === skip below if alignment is not needed. ===
# === energy drift alignment ===
# The drift alignment subroutine assumes that the specimen 
# in a box defined by x0,x1,y0,y1 is homogeneous. 
# The spectrum is aligned within the box and extrapolated 
# to the outside. 
# Energy channels in e0,e1 is used for the alignment. 
# type_xy, type_e: type can be one of the following:
# "pct"		  : 0 to 1 (e.g. x0=0.3, x1=0.5 (in percentage))
# "unit-absolute" : min_value to max_value (e.g. e0=300, e1=800 (in eV) )
# "unit-relative" : 0 to max_value-min_value (e.g. e0=10, e1=20 (in eV, relative to minimum value) )
# "px"		  : min_pixel_or_channel to max_pixel_or_channel (e.g. x0=10, x1=20 (in pixel) )
# smth_method	  : need to be set to "poly", the drift correction values are smoothed using a polynomial function.
# Other optional parameters not listed here:
# arg = list(poly_deg = 3) : degree of polynomial smooth function
# e_drift_along_y_max_search_rep = 5 : max number of iterations 
# e_drift_along_y_search_rng_init = -15:15 : search range in the first iteration
# e_drift_along_y_search_rng_2nd = -5:5 : search range in the following iterations

# e drift alignment for y
imgAndSI$SI_e_drift_along_y_corr(x0=88, x1=109, y0=1, y1=61, type_xy='px', e0=453, e1=470, type_e='unit-absolute', smth_method='poly')

# e drift alignment for x (doing a transpose for xy, align, and transpose back)
imgAndSI$get_SI()$transpose_xy()
imgAndSI$SI_e_drift_along_y_corr(1,61,88,109,'px',453,470,'unit-absolute','poly',arg = list(poly_deg = 1))
imgAndSI$get_SI()$transpose_xy()

# === image drift alignment ===
# The image drift correction subroutine assumes that the specimen 
# is homogeneous along the y direction, such as a multi-layer 
# cross-section sample with the growth direction along the x direction 
# and x is the fast-scanning direction in the EELS experiment. 
# The subroutine aligns the interface along the y direction. 
# Arguments are : 
# from_ADF_or_EFImg	: "EFImg" or "ADF" use energy-filtered image or ADF for image drift correction
# x0,x1,y0,y1,type_xy,e0,e1,type_e,smth_method : same as energy drift correction. 

# imgAndSI$SI_and_ADF_x_drift_corr(from_ADF_or_EFImg = 'EFImg',1,210,1,43,'px',453,470,'unit-absolute','poly',arg = list(poly_deg = 3),x_drift_search_rng_init=-30:30)
imgAndSI$SI_and_ADF_x_drift_corr(from_ADF_or_EFImg = 'ADF',1,109,1,61,'px',NA,NA,'unit-absolute','poly',arg = list(poly_deg = 3),x_drift_search_rng_init=-30:30)
# === end of drift alignment === 
# === skip the above if alignment is not needed. ===

# subplot in 4x3
par(mfcol=c(3,4))

# Bin the pixels and channels for higher SNR
# For a multi-layer structure with the interface aligned in the y direction, 
# sum along the "y" direction first. 
# The energy channel binning is 4 and the x direction binning is 2. 
# You can use other values for x and e binning. 
SI_2d<-imgAndSI$get_SI()$clone()$SI_3d_to_2d_or_pt('y')$binning_3d(nbin = 4, along = 'e')$binning_3d(nbin = 2, along = 'x')

# Separate the signal from the background using 
# the algorithm in 10.1016/j.ultramic.2018.08.013
# SigELow and SigEHigh defines the energy range to 
# be unmixed. 
SI_2d_bkg_removed<-BkgRemoval$new(SI_2d)$set_params(SigELow = 452, SigEHigh = 470, BkgELow = NA, BkgEHigh = NA)$set_Bkg_Sig_algo_PolyOrtho(arg = list(poly_deg = 1))$get_Sig_as_SI()

# Signal extracted by using the conventional power-law 
# background removal method is required by the algorithm 
# to normalize the cross-section. 
# SigELow and SigEHigh defines the energy range and they should be the same as above. 
# BkgELow and BkgEHigh defines the energy range for the power-law background fitting. 
SI_2d_intensity<-BkgRemoval$new(SI_2d)$set_params(SigELow = 452, SigEHigh = 470, BkgELow = 370, BkgEHigh = 451)$set_Bkg_Sig_algo_Power()$get_Sig_as_SI()$gen_EFImg_from_3d(0,1,'pct')

# pass data to the unmix subroutine
EELSUnmixObj<-EELSUnmix$new()$set_sig_for_unmix_from_SI(SI_2d_bkg_removed)$set_intensity_scale_for_Sig_from_ADF(SI_2d_intensity)$set_unmix_R_and_intensity_scale_for_unmix()$set_sig_for_fit_from_SI(SI_2d_bkg_removed)$set_unmix_R_for_fit()

use_svd<-F
if (use_svd) {
# optimize_em  : Optimize the extracted endmembers
# manifold_dim : Set the dimension of the local manifold, which will be used in the optimization. 
# For 1D structure (layers, nanorods) manifold_dim=1
# For 2D structure (bulk alloys) manifold_dim=2
# For 0D structure (different particles) manifold_dim=0
UnmixCoreObj<-UnmixCore$new()$set_param(dr_method = 'svd', pca_method = NA, noise_mag = 1, em_algo = 'p-sc-in-nfindr', optimize_em = T, manifold_dim = 1)

UnmixCoreObj<-EELSUnmixObj$UnmixCore_set_R_for_unmix(UnmixCoreObj)

# max_p defines the maximum number of endmembers in the search
# The subroutine tries from 1 until find a suitable p or p reaches max_p
UnmixCoreObj$find_p(max_p = 3)

} else {
# or, you can define your own dimensionality reduction function. 
# Here we use sammon plot for example. 
UnmixCoreObj<-UnmixCore$new()$set_param(dr_method = 'manifold_general', pca_method = (function(R,k){
  # use_sammon
    require("Rdimtools")
    res<-do.sammon(t(R), ndim=k)
    Xp<-t(res$Y)
    Rp<-t(R)
    u<-array(0,dim=c(dim(R)[1],k))
    Xp_shifted<-Xp
  list(Rp = Rp, Xp=Xp, u=u, Xp_shifted=Xp_shifted)
}), noise_mag = 1, em_algo = 'p-sc-in-nfindr', optimize_em = T, manifold_dim = 1)

UnmixCoreObj<-EELSUnmixObj$UnmixCore_set_R_for_unmix(UnmixCoreObj)
UnmixCoreObj$set_p(3)$do_dim_reduction_and_em_extraction_from_p()

}



# map_type defines type of plot, possible values are "p" for points, "l" for lines & etc. 
# See ?plot for details.
# fig_out is an array of T or F which defines what to plot. 
# elements of the array defines if 
# 1. plot_signature
# 2. plot_score 
# 3. plot_Xp_shifted_normalized
# 4. plot_xp_shifted_12
# 5. plot_xp_shifted_13
# Where xp_shifted is the low-dimensional map of the data
UnmixCoreObj$do_plot(map_type="p",fig_out=c(T,T,T,T,T))

UnmixCoreObj<-EELSUnmixObj$UnmixCore_set_R_for_fit(UnmixCoreObj)
UnmixCoreObj$mlls()

EELSUnmixObj$set_p_fitted_coef_residuals_from_UnmixCore(UnmixCoreObj)

# plot the endmember signatures. 
# em_index_for_color_map is an array defines the RGB color maps to which endmember. 
# e.g., em_index_for_color_map=c(1,3,2) makes the 1st endmember to be red, 
#       3rd to be blue, and 2nd to be green. 
EELSUnmixObj$do_plot_em_sig(em_index_for_color_map=NA)
# If a label is on each pixel where the endmembers are found or not. 
EELSUnmixObj$do_plot_coef(plot_em_position=T)
EELSUnmixObj$do_plot_residuals()
EELSUnmixObj$do_plot_qq()

imgAndSI$get_ADF()$plot_ADF()

################# END OF EXAMPLE OF AN INTERFACE ###############


################# EXAMPLE OF A PARTICLE ###############



par(mfcol=c(3,4))
set.seed(3)

use_simulated_data<-T

if (!use_simulated_data) {
  FILENAME_DKRFDK<-"my_dkrf.dm4"
  FILENAME_SI<-"my_si.dm4"
  FILENAME_ADF = "my_adf.dm4"

  origDkRf<-OrigSI$new()$set_SI_from_EELS_file(FILENAME_DKRFDK)
  origSIObj<-OrigSI$new()$set_SI_from_EELS_file(FILENAME_SI)
  origADFObj<-OrigADF$new()$set_ADF_from_ADF_file(FILENAME_ADF,"SI")
} else {
  SimulatedData<-SimulatedData$new(type = "particle") # "interface" or "particle"
  origADFObj <- SimulatedData$get_origADF()
  origDkRf   <- SimulatedData$get_origDkRf()
  origSIObj  <- SimulatedData$get_origSIObj()
}


# remove spike noise in the dark reference, then remove the dark reference from SI
origDkRf_xray_clean<- SpikeNoise$new()$set_spec_with_xray(origDkRf)$do_convert_spec_1()$get_spec_no_xray()

# remove dark reference
darkCorrectedSI<-DarkRef$new(
  origDkRf_xray_clean, 1, origSIObj)$do_remove_dkRf()$get_corrected_SI()

spikeCorrectedSI<-SpikeNoise$new(channelRes=2)$set_spec_with_xray(darkCorrectedSI$crop_e(3,2046,'px')$shift_e(-1.7))$do_remove_spike_3d_2()$set_protected_chs(data.frame(l=c(452,522),h=c(470,560)))$conv_neighbor_px_interpolation(function(u,d,l,r){rowMeans(cbind(u,d,l,r),na.rm=T)})$get_spec_no_xray()

imgAndSI<-ImgAndSpecAlign$new(spikeCorrectedSI, origADFObj)

par(mfcol=c(3,4))
SI_3d<-imgAndSI$get_SI()$clone()$binning_3d(nbin = 4, along = 'e')$binning_3d(nbin = 4, along = 'x')$binning_3d(nbin = 4, along = 'y')
SI_3d_bkg_removed<-BkgRemoval$new(SI_3d)$set_params(SigELow = 452, SigEHigh = 470, BkgELow = 450, BkgEHigh = 470)$set_Bkg_Sig_algo_PolyOrtho(arg = list(poly_deg = 1))$get_Sig_as_SI()
SI_3d_intensity<-BkgRemoval$new(SI_3d)$set_params(SigELow = 452, SigEHigh = 470, BkgELow = 370, BkgEHigh = 451)$set_Bkg_Sig_algo_Power()$get_Sig_as_SI()$gen_EFImg_from_3d(0,1,'pct')
EELSUnmixObj<-EELSUnmix$new()$set_sig_for_unmix_from_SI(SI_3d_bkg_removed)$set_intensity_scale_for_Sig_from_ADF(SI_3d_intensity)$set_unmix_R_and_intensity_scale_for_unmix()$set_sig_for_fit_from_SI(SI_3d_bkg_removed)$set_unmix_R_for_fit()
# Here
UnmixCoreObj<-UnmixCore$new()$set_param(dr_method = 'svd', pca_method = NA, noise_mag = 1, em_algo = 'p-sc-in-nfindr', optimize_em = T, manifold_dim = 2)
UnmixCoreObj<-EELSUnmixObj$UnmixCore_set_R_for_unmix(UnmixCoreObj)
UnmixCoreObj$find_p(max_p = 3)
UnmixCoreObj<-EELSUnmixObj$UnmixCore_set_R_for_fit(UnmixCoreObj)
UnmixCoreObj$mlls()

UnmixCoreObj$do_plot(map_type="p")




EELSUnmixObj$set_p_fitted_coef_residuals_from_UnmixCore(UnmixCoreObj)

EELSUnmixObj$do_plot_em_sig()
EELSUnmixObj$do_plot_coef(plot_em_position=T)
EELSUnmixObj$do_plot_residuals()
EELSUnmixObj$do_plot_qq()

imgAndSI$get_ADF()$plot_ADF()



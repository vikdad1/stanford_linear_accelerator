#vector of feature extraction functions
featureExtractionFunctions <- c(
  'pjtn'= pjtnFeatureExtraction,
  'thy_resv'=thyresvFeatureExtraction,
  'sigma'=sigmaFeatureExtraction,
  'swrd'=swrdFeatureExtraction,
  'temp'=tempFeatureExtraction,
  'dqi'=dqiFeatureExtraction,
  'hvv'=hvvFeatureExtraction,
  'wndw'=wndwFeatureExtraction)
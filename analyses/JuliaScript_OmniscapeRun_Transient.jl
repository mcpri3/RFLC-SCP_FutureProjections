#The code is written for one combination of parameters
filepath = string("/bettik/primam/RFLC-SCP_FutureProjections/data/derived-data/OmniscapeParamFiles/IniFile_Transient_", ARGS[1], "_GroupID_", ARGS[2], "_TransfoCoef_", ARGS[3], "_SuitThreshold_", ARGS[4], "_DispDist_", ARGS[5], "km_", ARGS[6], "_", ARGS[7], "_", ARGS[8], ".ini")

using Omniscape
run_omniscape(filepath)
Package["Economica`"]


(*

blkdiag in matlab that makes a block diagonal matrix in mathematica is 

a = {{1, 2, 3}, {4, 5, 6}}
b = {{7, 8}, {9, 10}}
SparseArray[Band[{1, 1}] -> {a, b}].

*)


DFMSignalExtract::usage="Internal DFM function.";

Options[DFMSignalExtract] = {FullSampleInformation -> False};

DFMSignalExtract[data_, Parameters_, KFS_, OptionsPattern[]] := Module[
  {kfs = KFS, signal, signP, nobs, n, S,
  dates=Parameters["Dates"]},
  (*% Initalize*)
  S = DFMTimeSeriesCreateStateSpace[Parameters, Association[], 
    First@dates];
  
  (*TODO: *)
  (*if size(S.Z,1)~=size(Ym,2)
  error('Inappropriate length of data vector');
  end*)
  
  (*% Dimensions*)
  {nobs, n} = Dimensions@(data);
  signal = zeros[nobs, n];
  signP = zeros[nobs, n];
  
  (*% Go*)
  Do[ 
   S = DFMTimeSeriesCreateStateSpace[Parameters, S, dates[[t]]];
   If[
   	OptionValue@ FullSampleInformation,
	   	signal[[t]] = (S["Z"].(kfs["AmT"][[t]]) + S["c1"]);
	    signP[[t]] = Sqrt@(Diagonal@(S["Z"].squeeze[kfs["PmT"][[t]]]).S["Z"]);,
    (*else*)
    	signal[[t]] = (S["Z"].(kfs["AmU"][[t]]) + S["c1"]);
    	signP[[t]] = 
     Sqrt@(Diagonal@(S["Z"].squeeze@(kfs["PmU"][[t]]).S["Z"]\[Transpose]));
    ],
   {t, nobs}];
  kfs["signal"] = signal; kfs["signP"] = signP;
  kfs];

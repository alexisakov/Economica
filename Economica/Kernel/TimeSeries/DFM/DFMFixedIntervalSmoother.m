(* Mathematica package *)

BeginPackage["Economica`"]
(* Exported symbols added here with SymbolName::usage *)  

DFMFixedIntervalSmoother::usage="Internal DFM fucntion";

Begin["`Private`"] (* Begin Private Context *) 

DFMFixedIntervalSmoother[data_, Parameters_, KFS_] := Module[
  {kfs = KFS, dates = Parameters["Dates"], nobs, m,
   AmT, PmT, Pstar, S, PmU, Pm1, PT, c},
  
  {nobs, m} = Dimensions[KFS["Am"]];
  AmT = zeros[nobs, m];
  PmT = zeros[nobs, m, m];
  Pstar = zeros[nobs - 1];
  AmT[[nobs]] = squeeze@(kfs["AmU"][[nobs]]);
  PmT[[nobs]] = squeeze@(kfs["PmU"][[nobs]]);
  
  (*% Initalise& update to get S(nobs)*)
  S = DFMTimeSeriesCreateStateSpace[Parameters, Association[], 
    First@dates];
  S = DFMTimeSeriesCreateStateSpace[Parameters, S, Last@dates];
  
  Do[
   PmU = squeeze@(KFS["PmU"][[t]]);
   Pm1 = squeeze@(KFS["Pm"][[t + 1]]);
   PT = squeeze@(PmT[[t + 1]]);
   
   
   S = DFMTimeSeriesCreateStateSpace[Parameters, S, dates[[t]]];
   
   c = PmU.S["T"]\[Transpose]*PseudoInverse@(Pm1);
   
   AmT[[t]] = 
    kfs["AmU"][[t]] + (c*(AmT[[t + 1]] - kfs["Am"][[t + 1]]));
   PmT[[t]] = PmU + c.(PT - Pm1).c\[Transpose];
   Pstar[[t]] = c;,
   {t, nobs - 1, 1, -1}];
  kfs["AmT"] = AmT; kfs["PmT"] = PmT; kfs["Pstar"] = Pstar;
  kfs
  ]

End[] (* End Private Context *)

EndPackage[]
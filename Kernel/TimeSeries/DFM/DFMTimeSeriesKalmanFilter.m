(* Mathematica package *)


BeginPackage["Economica`"]
(* Exported symbols added here with SymbolName::usage *)  

DFMTimeSeriesKalmanFilter::usage="Internal DFM function";

Begin["`Private`"] (* Begin Private Context *) 

DFMTimeSeriesKalmanFilter::datavnotfitz = 
  "TimeSeriesKalmanFilter:Data vector does not fit Z";
DFMTimeSeriesKalmanFilter::invdnw = 
  "Inversion of matrix F in did not work";

DFMTimeSeriesKalmanFilter[x_, P_] := Module[
  {s = Association[], R = Association[],
   dates = P["Dates"],
   sF, nobs, m, n, Q, rAm, rAmU, rPm, rPmU, A, p, Au,
   y, Z, G, c1, L, Pu, iF, k, PZ, GG, d, PZF, V, rk, riF},
  s = DFMTimeSeriesCreateStateSpace[P, s, First@dates];
  If[ Length@s["Z"] != Last@Dimensions@x,
   Message[DFMTimeSeriesKalmanFilter::datavnotfitz]; Abort[]
   ];
  
  (*% Output structure& dimensions*)
  {n , m} = Dimensions@(s["Z"]);
  nobs = Length@x;
  sF = 0;
  Q = 0;
  rAm = ConstantArray[Null, {nobs, m}]; 
  rPm = ConstantArray[Null, {nobs, m, m}];
  rAmU = ConstantArray[Null, {nobs, m}]; 
  rPmU = ConstantArray[Null, {nobs, m, m}];
  riF = ConstantArray[Null, nobs];
  rk = ConstantArray[Null, nobs];
  (*%____________*)
  A = s["A1"];
  p = s["P1"];
  Au = zeros@(Sequence @@ Dimensions@A);
  
  Do[
   rAm[[t]] = A; rPm[[t]] = p;
   (*% Obtain SSF and D=inv(GG')*)
   s = DFMTimeSeriesCreateStateSpace[P, s, dates[[t]]];

   {y, Z, G, c1, L} = DFMReshapeObservationEquation[x[[t]], s];
   If[
    	 y == {},
    		Au = A;
    	Pu = p;
    	A = s["T"].A + s["c2"];
    	p = s["T"].p.(s["T"]\[Transpose]) + s["H"].(s["H"]\[Transpose]);
    	iF = zeros[n, n];
    	k = zeros[m, n];,
    (*else*)
    (*% Compute inv(F_t)*)
    PZ = p.Z\[Transpose];
    GG = G.G\[Transpose];
    If[
      Total[Total[GG - DiagonalMatrix@Diagonal@GG]] > 0 || 
      Total[Diagonal[GG] /. {xx_ /; xx == 0 -> 1, 
          xx_ /; xx != 0 -> 0}] > 0,
     iF = Inverse[Z.PZ + GG];,
     (*else*)
     d = DiagonalMatrix[1./Diagonal@(GG)];
     iF = d - d.Z.Inverse[eye[m] + PZ.d.Z].PZ.d;
     ];
    If[ Total[
       Total[Abs[
         iF.(Z.p.Z\[Transpose] + G.G\[Transpose]) - 
          eye[Sequence @@ Dimensions[iF]]]]] > n^(2*10^-6),
     Message[DFMTimeSeriesKalmanFilter::invdnw]; Abort[]];
    (*% Kalman gain K_t*)
    PZF = PZ.iF;
    k = s["T"].PZF;
    
    (*% Au=A_t|t& Pu=P_t|t*)
    V = y - Z.A - c1;
    Au = A + (PZF.V);
    Pu = p - (PZF.PZ\[Transpose]);
    
    (*% A=A_t+1|t& P=P_t+1|t*)
    A = s["T"].A + k.V + s["c2"];
    p = (s["T"] - k.Z).p.s["T"]\[Transpose] + 
      s["H"].s["H"]\[Transpose];
    p = 0.5*(p + p\[Transpose]);
    
    (*% Likelihood*)
    Q = Q + V\[Transpose].iF.V;
    sF = sF - Log@(Det@(iF));
    
    (*% Restore structure of iF and K*)
    iF = L.iF.L\[Transpose]; k = k.L\[Transpose];
    ];
   rAmU[[t]] = Au;
   rPmU[[t]] = Pu;
   	riF[[t]] = iF;
   rk[[t]] = k;(**),
   {t, nobs}]; 
  
  (*% Likelihood*)
  R["Q"] = Q; R["sF"] = sF;
  R["loglik"] = 0.5*(R["sF"] + R["Q"]);
  R["Am"] = squeeze@rAm;
  R["Pm"] = rPm;
  R["AmU"] = squeeze@rAmU;
  R["PmU"] = rPmU;
  R["iF"] = riF;
  R["K"] = rk;
  (**)
  (*Return association*)
  R
  ]




End[] (* End Private Context *)

EndPackage[]
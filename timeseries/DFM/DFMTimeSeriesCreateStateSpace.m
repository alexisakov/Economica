Package["Economica`"]

(* Exported symbols added here with SymbolName::usage *)  
DFMTimeSeriesCreateStateSpace::usage="Internal DFM function.";
DFMReshapeObservationEquation::usage="Internal DFM function."


DFMTimeSeriesCreateStateSpace[P_, S_, t_] := Module[
  {
  	(*tau = First@Flatten@Position[P["Dates"], t],*)
   s = S, n, m, nQ, TT, T0,
   Z, c1, c2, G, H, A1, P1, T
   },
  {n, m} = Dimensions[P["C"]];
  nQ = Last@Dimensions@P["beta"];
  
  If[S != Association[],(*If this NOT a first call, then*)
   	TT = ConstantArray[0, {m + 2*nQ, m + 2*nQ}];
   	TT[[1 ;; m, 1 ;; m]] = P["A"];
   	If[Mod[DateValue[t, "Month"], 3, 1] != 3,
    		TT[[-1 - nQ + 1 ;;, -1 - nQ + 1 ;;]] = IdentityMatrix[nQ];
    	];
   	T0 = IdentityMatrix[m + 2*nQ];
   	T0[[m + nQ + 1 ;; m + 2*nQ, m + 1 ;; m + nQ]] = -1/3*
     IdentityMatrix[nQ];
    T0[[m + 1 ;; m + nQ,1 ;; P["r"]]] = -(P["beta"][[2 ;;]])\[Transpose];
   	s["T"] = Inverse[T0].TT;
   	Return[s]
   ];
  (*If this is the FIRST call to function*)
  (*1. Observation equation*)
  Z = zeros[n + nQ, m + 2*nQ];
  Z[[1 ;; n, 1 ;; m]] = P["C"];
  Z[[n + 1 ;; n + nQ, m + nQ + 1 ;; m + 2*nQ]] = IdentityMatrix@(nQ);
  c1 = zeros[n + nQ, 1];
  c1[[n + 1 ;; n + nQ]] = P["beta"][[1]];
  G = zeros[n + nQ, n + m + nQ];
  G[[1 ;; n, 1 ;; n]] = CholeskyDecomposition[P["R"]];
  (*G[[n+1,n+m+1]]=P["s"];*)
  (*Transition equation*)
  TT = zeros[m + 2*nQ, m + 2*nQ];
  TT[[1 ;; m, 1 ;; m]] = P["A"];
  If[
    (First@Flatten@Position[P["Dates"], t] > 1) && 
    Mod[DateValue[t, "Month"], 3, 1] != 3,
   TT[[-1 - nQ + 1 ;; -1, -1 - nQ + 1 ;; -1]] = eye[nQ];
   ];
  T0 = eye[m + 2*nQ];
  T0[[m + nQ + 1 ;; m + 2*nQ, m + 1 ;; m + nQ]] = -1/3*eye[nQ];
  T0[[m + 1 ;; m + nQ, 
     1 ;; P["r"]]] = (-P["beta"][[2 ;; -1]])\[Transpose];
  c2 = zeros[m + 2*nQ, 1];
  H = zeros[m + 2*nQ, m + nQ];
  H[[1 ;; m, 1 ;; m]] = P["H"];
  H[[m + 1 ;; m + nQ, m + 1 ;; m + nQ]] = P["s"]*Sqrt[3.];
  H = Join[zeros[m + 2*nQ, n], H, 2];
  T = Inverse[T0].TT;
  H = Inverse[T0].H;
  c2 = Inverse[T0].c2;
  (*Initial condition*)
  A1 = zeros[Length[T], 1];
  P1 = DFMInitializeCovariance[T, H.(H\[Transpose])];
  s["Z"] = Z; s["c1"] = c1; s["G"] = G; s["c2"] = c2; s["H"] = H; 
  s["A1"] = A1; s["P1"] = P1;
  s["T"] = T;
  s
  ];
  
  (*%_________________________________________________________________\
_____
  % PROC missdata
  % PURPOSE:eliminates the rows in y& matrices Z,G that correspond to
  % missing data (NaN) in y
  % INPUT y vector of observations at time t (n x 1)
  % S KF system matrices (structure)
  % must contain Z& G
  % OUTPUT y vector of observations (reduced) (# x 1)
  % Z G KF system matrices (reduced) (# x?)
  % L To restore standard dimensions (n x #)
  % where # is the nr of available data in y
  %___________________________________________________________________\
___*)
  
  DFMReshapeObservationEquation[Y_, KFS_] := Module[
  {ix, e, L, y = Y, Z, G, c1},
  
  ix = Flatten@Rest@Position[y, Except@Null, {1}];
  e = eye@(Length@y);
  L = e[[All, ix]]; y = y[[ix]]; Z = KFS["Z"][[ix]];
  G = KFS["G"][[ix]]; c1 = KFS["c1"][[ix]];
  {y, Z, G, c1, L}
  ]
  
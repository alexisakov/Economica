Package["Economica`"]

(* Exported symbols added here with SymbolName::usage *)  
DFMInitializeCovariance::usage="PROCEDURE InitCov
 PURPOSE : Calculates the unconditional VCV of a stationary VAR (1)
 y = T y {1} + e, e~(0, P)
 P must be p.s.d.T must have all evals < 1 in absval
 This is used for initializing the stationary part of the Kalman Filter
 See Luetkepohl, p .22
 OUTPUT C with y~N (0, C)*)"


DFMInitializeCovariance::incvone = 
  "InitCov:Matrix T has eigenvalues\[GreaterEqual]1";
DFMInitializeCovariance::subzero = "InitCov:Matrix P has eigenvalues<0";
DFMInitializeCovariance::lma = "InitCov:Large matrix adjustment";

DFMInitializeCovariance[T_, P_] := Module[
  {v, d, m, vecC, c, c2},
  
  If[ Max[Abs[Eigenvalues[T]]] >= 1, 
   Message[DFMInitializeCovariance::incvone]];
  (*% Check whether P is psd*)
  {v, d} = {Eigenvectors[P], DiagonalMatrix@Eigenvalues[P]};
  If[(Min@(Diagonal@d)/
       Max@(Diagonal@(d)) < -10^-8) || (Max@(Diagonal@(d)) < 0),
   Message[DFMInitializeCovariance::subzero]; Abort[]];
  
  (*% Calculate unconditional VCV*)
  m = Length@(T);
  vecC = Inverse[(eye@(m^2) - KroneckerProduct[T, T])].ArrayReshape[
     P, {m^2, 1}];
  c = ArrayReshape[vecC, {m, m}];
  c2 = c;
  c = Re[0.5*(c + c\[Transpose])];
  
  If[ Abs@(Max@(Max@(c2 - c))) > 10^-8, 
   Message[DFMInitializeCovariance::incvone];];
  c
  ]
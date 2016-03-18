(* Mathematica package *)


BeginPackage["Economica`"]
(* Exported symbols added here with SymbolName::usage *)  
DFMEstimateStaticPC::usage="Internal function."

Begin["`Private`"] (* Begin Private Context *) 



DFMEstimateStaticPC[xe_,rMAX_]:=Module[{
	V,d,F},
	
  
  d=Eigenvalues[Covariance[xe]];
  d=d[[;;rMAX]];
  d=DiagonalMatrix[d];
  V=Eigenvectors[Covariance[xe]];
  V=V[[All,;;rMAX]];
  F=xe.V;
  {F,V}
]


End[] (* End Private Context *)

EndPackage[]
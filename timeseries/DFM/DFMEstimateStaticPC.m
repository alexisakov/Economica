Package["Economica`"]
(* Exported symbols added here with SymbolName::usage *)  
DFMEstimateStaticPC::usage="Internal function."




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



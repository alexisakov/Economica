(* Mathematica package *)

Package["Economica`"]

(* Exported symbols added here with SymbolName::usage *)  
VARMakeLags::usage="VARMakeLags[S,nlags] builds a matrix with lagged values of S, i.e. 
if S={x1,x2} VARMakeLags[S,1] yields {x1 x2 x1_t-1 x2_t-1}.
It's an internal VAR function."


(*TODO: Test the case when there's only one exo variable (vector, not the matrix)
*)

VARMakeLags[S_,nlags_]:=Module[
	{X,nobs,nvar},

	{nobs,nvar}=Dimensions[S];
	X=Join[Sequence@@(S[[#+1;;nobs-nlags+#,All]]&/@Range[0, nlags - 1]), 2];
	X=Join[S[[nlags+1;;,All]],X,2]
]

PackageExport["VARMakeLags"]
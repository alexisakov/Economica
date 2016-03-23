(* Mathematica package *)

BeginPackage["Economica`"]
(* Exported symbols added here with SymbolName::usage *)  
VARMakeXY::usage="VARMakeXY[endo,nlags,ccase] is an internal VAR function that creates independent vector and lagged dependent matrix."

Begin["`Private`"] (* Begin Private Context *) 
VARMakeXY[endo_, nlags_, ccase_:1]:=Module[
	{X,Y,nobs,nvar},
	
	{nobs,nvar}=Dimensions[endo];
	Y=endo[[nlags+1;;,All]];
	
	(*Now construct the X matrix:*)
	X = Reverse/@Join[Sequence@@((Reverse/@endo[[#+1;;nobs-nlags+#,All]])&/@Range[0, nlags - 1]), 2];
	
	Which[
	ccase==0,
		Sequence[];,
	ccase==1,
		X = Join[{ConstantArray[1, nobs - nlags]}\[Transpose], X, 2];,
	ccase==2,
		X = Join[
			{ConstantArray[1, nobs - nlags]}\[Transpose],
			{Range[1, nobs - nlags]}\[Transpose],
			 X, 2];,
	ccase==3,
		X = Join[
			{ConstantArray[1, nobs - nlags]}\[Transpose],
			{Range[1, nobs - nlags]}\[Transpose],
			{Range[1, nobs - nlags]^2}\[Transpose],
			 X, 2];];
	{Y,X}
	]


End[] (* End Private Context *)

EndPackage[]
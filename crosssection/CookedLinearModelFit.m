Package["Economica`"]

CookedLinearModelFit::usage="Simple robust linear regression";

Options[CookedLinearModelFit]={
	NominalVariables -> None,
	IncludeConstantBasis -> False};


CookedLinearModelFit[l_List,OptionsPattern[]] := Module[{x, vars, cooks, clean,nomVar},
	vars = Table[Subscript[x, i], {i, Length[First[l]]-1}];
 	
	nomVar = If[Not[(OptionValue@NominalVariables)===None],
		vars[[OptionValue@NominalVariables]],
		OptionValue@NominalVariables];

	cooks = LinearModelFit[l, vars, vars,
  		IncludeConstantBasis->OptionValue@IncludeConstantBasis,
  		NominalVariables -> nomVar]["CookDistances"];
  
	clean = Extract[l, Position[cooks, x_ /; x < 4/Length[l]]];
  
  	LinearModelFit[clean, vars, vars,
  		IncludeConstantBasis->OptionValue@IncludeConstantBasis,
  		NominalVariables -> nomVar]

 ] 


PackageExport["CookedLinearModelFit"]
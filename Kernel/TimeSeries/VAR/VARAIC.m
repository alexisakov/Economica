(* Mathematica package *)

BeginPackage["Economica`"]
(* Exported symbols added here with SymbolName::usage *)  
VARAIC::usage=""

Begin["`Private`"] (* Begin Private Context *) 

(*
TODO: Check the validity of AIC calculation on some example. 
*)

VARAIC[endo_, maxlags_, ccase_:1, exog_:{}, nlagsex_:0]:=Module[
	{X,nobs,logL,AIC,varmodel,neqs,residuals,ntotcoeff,sigma},
	logL=ConstantArray[0,maxlags];
	AIC=ConstantArray[0,maxlags];
	
	Do[
		X=endo[[maxlags+1-i;;,All]];
		varmodel=VARModelFit[X,i,ccase,exog,nlagsex];
		nobs=varmodel["nobs"];
		neqs=varmodel["nvar"];
		ntotcoeff=varmodel["ntotcoeff"];
		residuals=varmodel["FitResiduals"];
		sigma=(1/(nobs))*(Transpose[(residuals)].(residuals));
		(*logL[[i]]=-(nobs/2)*(neqs*(1+Log[2Pi])+Log[Det[sigma]]);
		AIC[[i]]=-2 logL[[i]]/nobs+2(neqs*neqs/nobs);*)
		AIC[[i]]=-Log[Det[sigma]]-2(neqs*(ntotcoeff))/nobs;,
		{i,maxlags}];
	AIC
]

End[] (* End Private Context *)

EndPackage[]
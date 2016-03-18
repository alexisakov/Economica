(* Mathematica package *)
BeginPackage["Economica`"]
(* Exported symbols added here with SymbolName::usage *)  

VARModelFit::usage="VARModelFit[endo,nlags,ccase,exog,nlagsex]  performs an OLS estimation of the vector autoregressive process."

(*
DONE: Add lag choice procedures: Added VARAIC.
TODO: Add tests for residiual stationarity, normality, lack of autocorrelation.
TODO: Check VAR stability
TODO: Add the standard form for output with `Format[bin[x_,y_],StandardForm]:=MatrixForm[{{x},{y}}]
*)

Begin["`Private`"] (* Begin Private Context *) 
VARModelFit::nobsError="There are `1` endogenious observations and `2` exogenous observations.";

VARModelFit[endo_, nlags_, ccase_:1, exog_:{}, nlagsex_:0]:=Module[
	{Y,X,nobs,nobs2,nvar,nobse,nvarex,
		ncoeff,nlagex=nlagsex,ntotcoeff,ncoeffex,
		Ft,sigma,residuals,XEX,diff},

	{nobs,nvar}=Dimensions[endo];

	If[exog!={},
		{nobs2,nvarex}=Dimensions[exog];
		If[nobs!=nobs2,Message[VARModelFit::nobsError,nobs,nobs2]];,
		nvarex=0		
		];
	
	(*Save some parameters and create data for VAR esetimation*)
	nobse=nobs-Max[nlags,nlagsex];
	nobs=nobse;
	ncoeff=nvar*nlags;
	ncoeffex=nvarex*(nlagex+1);
	ntotcoeff=ncoeff+ncoeffex+ccase;
	
	(*Create independent vector and laaged dependent matrix*)
	{Y,X}=VARMakeXY[endo,nlags,ccase];
	
	(*Create (lagged exogenous matrix:*)
	If[ncoeffex>0,
		XEX=VARMakeLags[exog,nlagex];
		Which[
			nlags==nlagex,
				X=Join[X,XEX,2],
			nlags>nlagex,
				diff=nlags-nlagex;
				XEX=XEX[[diff+1;;,All]];
				X=Join[X,XEX,2],
			nlags<nlagex,
				diff=nlagex-nlags;
				Y=Y[[diff+1;;,All]];
				X=Join[X[[diff+1;;,All]],XEX,2]
			];
		];
	
	
	
	(*OLS estimation by equation*)
	(*
	TODO: Implement OLSModel.m 
	*)
	
	(*Now we compute the variance-covariance matrix and OLS estimates of the 
	coefficients.*)
	
	Ft=Inverse[Transpose[X].X].Transpose[X].Y;
	sigma=(1/(nobse-ntotcoeff))*Transpose[(Y-X.Ft)].(Y-X.Ft);
	residuals=Y-X.Ft;
	
	Association["nobs"->nobs,"nvar"->nvar,"ncoeffex"->ncoeffex,
	"nlags"->nlags,"nlagex"->nlagex,
	"ncoeff"->ncoeff,"ntotcoeff"->ntotcoeff,
	"ccase"->ccase,"Ft"->Ft,"sigma"->sigma,"X"->X,
	"Y"->Y,"FitResiduals"->residuals,
	"nvarex"->nvarex]
	]


End[] (* End Private Context *)

EndPackage[]
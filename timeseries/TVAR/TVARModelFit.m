Package["Economica`"]

(* Exported symbols added here with SymbolName::usage *)  

TVARModelFit::usage="TVARModelFit[endo,nlags,ccase,exog,nlagsex]  performs an OLS estimation of the threshold vector autoregressive process.
Now it only supports exogenous variable for determining thresholds, but in principle it can be determined 
by an endogenous variable."

(*DONE: Add lag choice procedures: Added VARAIC.
TODO: Add tests for residiual stationarity, normality, lack of autocorrelation.
TODO: Check VAR stability
TODO: Add the standard form for output with `Format[bin[x_,y_],StandardForm]:=MatrixForm[{{x},{y}}]
*)

Begin["`Private`"] (* Begin Private Context *) 
TVARModelFit::nobsError="There are `1` endogenious observations and `2` exogenous observations.";

Options[TVARModelFit]={NumberOfTrialThresholds->10};

TVARModelFit[endo_, nlags_, thresholdVariable_,OptionsPattern[]]:=Module[
	{Y,X,nobs,nobs1,nobs2,nvar,nobse,nvarex,
		ncoeff,nlagex,ntotcoeff,ncoeffex,
		Ft,residuals,output=Association[],
		sumoferrors,marker1,marker2,threshold,
		input,residuals1,residuals2,thresholdrangee,
		X1,X2,Y1,Y2,Ft1,Ft2,sigma1,sigma2,
		 ccase=1, exog={}, nlagsex=0,
		 likelihood		
		},
	nlagex=nlagsex;
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
	
	
	(*1. First choose a rate a range for the threshold*)
	
	thresholdrangee=N@linspace[
		Quantile[thresholdVariable, 0.25],
		Quantile[thresholdVariable, 0.75],
		OptionValue[NumberOfTrialThresholds]];
	
	(*2. Find a threshold for which the sum of squared errors is minimal.*)
	sumoferrors={};
	{Y,X} = VARMakeXY[endo, nlags];
	Quiet@Table[
	(*2.1 Regime 1.*)
	marker1 = Flatten@Position[thresholdVariable[[nlags+1;;]], n_ /; n > threshold];
	marker2 = Complement[Range@(Length@thresholdVariable-nlags),marker1];
	
	(*marker1 = marker1;
	marker1 = marker1 //. {k___List, h___Integer, x_Integer, y_Integer, 
    	  t___} /; y - x > 1 :> {k, {h, x}, y, t};
	marker1 = marker1 /. {j__List, k__Integer} :> {j, {k}};
	marker1 = marker1 /. {x_Integer, ___} /; x < (nlags + 2) :> Sequence[];
	marker1 =Join[Range[First@# - nlags, First@# - 1], #] & /@ marker1;*)
		
	Y1 = Y[[marker1]];
	X1 = X[[marker1]];
	
	Ft=Inverse[Transpose[X1].X1].Transpose[X1].Y1;
	residuals1=Y1-X1.Ft;
	
		
	(*2.2 Regime 2*)
	Y2 = Y[[marker2]];
	X2 = X[[marker2]];
	
	Ft=Inverse[Transpose[X2].X2].Transpose[X2].Y2;
	residuals2=Y2-X2.Ft;
			
	(*residuals=Total[Power[Flatten[Join[residuals1,residuals2]],2]];*)
	likelihood=If[
		NumericQ[Total@Flatten@residuals1] && NumericQ[Total@Flatten@residuals2],
		Likelihood[MultinormalDistribution[zeros[nvar], Covariance[residuals1]],residuals1]
			+ Likelihood[MultinormalDistribution[zeros[nvar], Covariance[residuals2]],residuals2]
				];
		If[
			NumericQ[likelihood],
			AppendTo[sumoferrors,{likelihood,threshold}]],
	
		{threshold,thresholdrangee}];
	
	threshold=Last@First@Sort@sumoferrors;
	
	(*2.1 Regime 1.*)
	
	marker1 = Flatten@Position[thresholdVariable[[nlags+1;;]], n_ /; n > threshold];
	marker2 = Complement[Range@(Length@thresholdVariable-nlags),marker1];
	
	Y1 = Y[[marker1]];
	X1 = X[[marker1]];
	
	Y2 = Y[[marker2]];
	X2 = X[[marker2]];
	
	Ft1=Inverse[Transpose[X1].X1].Transpose[X1].Y1;
	residuals1=Y1-X1.Ft1;
	nobs1=Length[Y1];
	(*2.2 Regime 2*)
	Ft2=Inverse[Transpose[X2].X2].Transpose[X2].Y2;
	residuals2=Y2-X2.Ft2;
	nobs2=Length[Y2];
	
	sigma1=(1/(nobse-ntotcoeff))*Transpose[(Y1-X1.Ft1)].(Y1-X1.Ft1);
	sigma2=(1/(nobse-ntotcoeff))*Transpose[(Y2-X2.Ft2)].(Y2-X2.Ft2);
	
	
	output["Regime 1"]=Association["nobs"->nobs1,"nvar"->nvar,"ncoeffex"->ncoeffex,
	"nlags"->nlags,"nlagex"->nlagex,
	"ncoeff"->ncoeff,"ntotcoeff"->ntotcoeff,
	"ccase"->ccase,"Ft"->Ft1,"sigma"->sigma1,"X"->X1,
	"Y"->Y1,"FitResiduals"->residuals1];
	
	output["Regime 2"]=Association["nobs"->nobs2,"nvar"->nvar,"ncoeffex"->ncoeffex,
	"nlags"->nlags,"nlagex"->nlagex,
	"ncoeff"->ncoeff,"ntotcoeff"->ntotcoeff,
	"ccase"->ccase,"Ft"->Ft2,"sigma"->sigma2,"X"->X2,
	"Y"->Y2,"FitResiduals"->residuals2];(**)
	
	output["ChoiceOfThreshold"]=sumoferrors;
	output["ThresholdRange"]=thresholdrangee;
	output
	]
	

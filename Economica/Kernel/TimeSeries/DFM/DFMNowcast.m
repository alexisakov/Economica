(* Mathematica package *)

(*DFMNowcast is the main function of the package.
It produces the nowcast for one quarterly series using provided monthly
data.*)

BeginPackage["Economica`"]
(* Exported symbols added here with SymbolName::usage *)  
DFMNowcast::usage="DFMNowcast[M,q,n] produces a forecast for quarterly varialble 
q for n quarters ahead using monthly data M."

(*TODO: n does nothing. Should correct the preparedata function.*)

Begin["`Private`"] (* Begin Private Context *) 

Options[DFMNowcast]={
	CorrectionType -> "ReplaceMissingWithCenteredAverage",
	CenteredAverageWidth -> 3,
	NumberOfFactors->5,
	NumberOfDynamicFactors->2,
	NumberOfDynamicFactorsLags->2,
	OutlierCorrection->False};

DFMNowcast[M_?MatrixQ,q_?MatrixQ,n_?IntegerQ,OptionsPattern[]]:=Module[
	{DFMparameters=Association[],
	xe,rMAX,F,V,
	Vj,Fj,FQ,inputToSS,klmnMat},
	(*1. Prepare the data*)
	DFMparameters=DFMPrepareData[M,q,n];
	(*2. Set some parameters*)
	(*Parametrization*)
	(*TODO: Make Options instead of setting these*)
	DFMparameters["r"] = OptionValue@NumberOfFactors;
	DFMparameters["q"] = OptionValue@NumberOfDynamicFactors;
	DFMparameters["p"] = OptionValue@NumberOfDynamicFactorsLags;
	DFMparameters["OutlierCorrection"] = OptionValue@OutlierCorrection;
	(*3. Do some outlier correction and estimate model matrices*)
	xe = DFMCorrectOutliers@(DFMStandardizeMissing[DFMparameters["x"][[;;-1-DFMparameters["DiscardMonthsToObtainBalancedDataset"]]]]);
	(*DONE: just rewrite the static components as they were in the original code*)
	rMAX = DFMparameters["r"];
	(* Estimate static principal components:*)
	(*{F , V} = {PrincipalComponents[Standardize@xe][[All, ;; rMAX]],
		Eigenvectors[Covariance@xe][[All, ;; rMAX]]};*)
	{F , V}=DFMEstimateStaticPC[xe,rMAX];
	
	FQ = (DFMFilter[ConstantArray[1/3.,3],{1},#]&/@(F\[Transpose]))\[Transpose];
    FQ[[1 ;; 2, All]] = Null;
	(*Xf = DFMStandardizeMissing@(DFMparameters["x"]);*)
	DFMparameters["x"] = DFMStandardizeMissing@(DFMparameters["x"]);
    DFMparameters["NumberOfMonthlyVariables"] = Last@Dimensions@DFMparameters["x"];
    Fj = F[[All , ;; DFMparameters["r"]]];
    Vj = V[[All, ;; DFMparameters["r"]]];
    
    (*3.1 Estimation of the factor model*)
    DFMparameters = DFMTransitionEquationFit[
    	Fj, 
    	DFMparameters,
    	NumberOfDynamicFactors -> DFMparameters["q"],
   		NumberOfDynamicFactorsLags -> DFMparameters["p"]];
    DFMparameters["R"] = DiagonalMatrix@(Diagonal@(Covariance@(xe - Fj.Vj\[Transpose])));
    DFMparameters["C"] =  Join[Vj,
    	ConstantArray[0, 
    		{Length@Vj, DFMparameters["r"]*(DFMparameters["p"] - 1)}], 2];
   	
   	
   	DFMparameters["OLSY"]=DFMparameters["y"][[3 ;; -1 - DFMparameters["DiscardMonthsToObtainBalancedDataset"]]];
    DFMparameters["OLSX"]=FQ[[3 ;;, 1 ;; DFMparameters["r"]]];
    DFMparameters= DFMOLSModelFit[DFMparameters, IncludeConstantBasis -> True];
           
    DFMparameters["R"] = (ReplacePart[DFMparameters["R"], {i_, i_} :> Max[DFMparameters["R"][[i, i]], 0.1]]);
	
	(*4*)
	
	inputToSS=Join[DFMparameters["x"], List /@DFMparameters["y"], 2];
	
	klmnMat = DFMTimeSeriesKalmanFilter[inputToSS, DFMparameters];
	
	klmnMat = DFMFixedIntervalSmoother[inputToSS, DFMparameters, klmnMat];
	
	klmnMat = DFMSignalExtract[inputToSS, DFMparameters, klmnMat];

	Transpose[{DFMparameters["Dates"],klmnMat["signal"][[All,-1]]}]
]


End[] (* End Private Context *)

EndPackage[]
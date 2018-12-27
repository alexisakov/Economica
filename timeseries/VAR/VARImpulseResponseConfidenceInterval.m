Package["Economica`"]

(* Exported symbols added here with SymbolName::usage *)  

VARImpulseResponseConfidenceInterval::usage="VARImpulseResponseConfidenceInterval.";

Options[VARImpulseResponseConfidenceInterval] = {NumberOfDraws -> 100, ConfidenceIntervalWidth -> 0.9,
	Method -> "Bootstrap"};
	
VARImpulseResponseConfidenceInterval::methodError = "`1` method is not yet implemented.";

VARImpulseResponseConfidenceInterval[SVARObject_, OptionsPattern[]]:=Module[
	{svarM=SVARObject,
	nsteps,impact,ident,beta,nvar,nvarex,nlags,ccase,nobs,Y,resid,exog,
	Yartificial,u,LAG,LAGplus,T,
	varM,IRF,irfDraw,pctginf,pctgsup,SUP,INF,MED},
	
	nsteps = svarM["nsteps"];
	impact = svarM["impact"];
	ident = svarM["ident"];
	beta = svarM["Ft"];
	nvar = svarM["nvar"];
	nvarex = svarM["nvarex"];
	nlags = svarM["nlags"];
	ccase = svarM["ccase"];
	nobs = svarM["nobs"];
	Y = svarM["Y"];
	resid = svarM["FitResiduals"];
	If[nvarex != 0, exog = svarM["XEX"]];
	Yartificial = zeros[nobs, nvar];
	IRF=zeros[OptionValue[NumberOfDraws]];
	
	Do[
		If[OptionValue[Method] == "Bootstrap",
			u = resid[[Flatten@Ceiling[First@Dimensions[resid, 1]*RandomReal[{0, 1}, {nobs, 1}]], All]];,
			Message[VARImpulseResponseConfidenceInterval::methodError, OptionValue[Method]]; 
   			Abort[];];
 	(*2.0 generate the artificial data*)
 	(*2.1 Intialize the first nlags*)
 		 LAG = {};
		 
		 Do[
		  Yartificial[[jj, All]] = Y[[jj, All]] + u[[jj, All]];
		  LAG = Append[LAG, Yartificial[[jj, All]]];
		  (*% STEP 2.1: 
		  Initialize the artificial series and the LAGplus vector*)
		  Which[
		   ccase == 0,
		   LAGplus = LAG;,
		    ccase == 1,
		   LAGplus = Join[{1}, LAG];,
		    ccase == 2,
		   T = Range[nobs];
		   LAGplus = Join[{1}, T[[jj]], LAG];,
		    ccase == 3,
		   T = Range[nobs];
		   LAGplus = Join[{1}, T[[jj]], T[[jj]]^2 , LAG];];
		  , {jj, nlags}];
		 LAGplus = Flatten@LAGplus;
		 LAG = Flatten@LAG;
		 
		 (*%% STEP 2.2:generate artificial series*)
		 
		 Do[
		  Do[
		   Yartificial[[jj, mm]] = LAGplus.beta[[All, mm]] + u[[jj, mm]];
		   , {mm, nvar}];
		  LAG = Append[Yartificial[[jj]] , LAG[[1 ;; (nlags - 1)*nvar]]];
		  LAG = Flatten@LAG;
		  Which[
		   ccase == 0,
		   LAGplus = LAG;,
		    ccase == 1,
		   LAGplus = Join[{1}, LAG];,
		    ccase == 2,
		   T = Range[nobs];
		   LAGplus = Join[{1}, T[[jj]], LAG];,
		    ccase == 3,
		   T = Range[nobs];
		   LAGplus = Join[{1}, T[[jj]], T[[jj]]^2 , LAG];];
		  , {jj, nlags + 1, nobs}];
			
			
		
		(*STEP 3:estimate VAR on artificial data.*)
		varM = VARModelFit[Yartificial, nlags, ccase];
		svarM = VARImpulseResponse[varM, nsteps, "Cholesky"];
		irfDraw = svarM["IRF"];
		IRF[[tt]] = irfDraw;
	
	, {tt,OptionValue[NumberOfDraws]}];
	
		pctginf = (1 - OptionValue[ConfidenceIntervalWidth])/2;
		pctgsup = 1 - (1 - OptionValue[ConfidenceIntervalWidth])/2;
		INF = Quantile[IRF, pctginf];
		SUP = Quantile[IRF, pctgsup];
		MED = Quantile[IRF, 1/2];

	
	svarM["LowerConfidenceBoundOfIRF"]=INF;
	svarM["UpperConfidenceBoundOfIRF"]=SUP;
	svarM["MedianConfidenceBoundOfIRF"]=MED;
		
	svarM
];

End[] (* End Private Context *)

EndPackage[]
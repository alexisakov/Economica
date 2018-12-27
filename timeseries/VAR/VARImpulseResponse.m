Package["Economica`"]

(*This file loads all other VAR files


TODO: Implemet sign restrictions identification scheme.
TODO: Implement long run restrictions identification scheme BlanchardQuah
*)

(* Exported symbols added here with SymbolName::usage *)  


VARImpulseResponse::usage="VARImpulseResponse[VARObject,nsteps,ident,impact,S,shut] computes IRFs for a AR model estimated with VARModelFit.
Three identification schemes are supported: zero short-run restrictions, zero long-run restrictions, and sign restrictions.";
VARImpulseResponse::identError="`1` identification scheme is not yet implemented.";
VARImpulseResponse::impactError="impact should be euther 0 or 1.s"

Options[VARImpulseResponse]={IdentificationScheme};

VARImpulseResponse[VARObject_, nsteps_, ident_:"Cholesky", impact_:0, S_:Null, shut_:0]:=Module[
	{ccase,nvar,nlags,Ft,F,sigma,IRF,
	Fcomp,FcompEye,response,impulse,
	out,invA,responseBig,impulseBig,
	FinfBig,Finf,d,output,
	matrixToBeSimetricized,simmetricMatrix},
	
	(*BlanchardQuah*)
	
	(*Retrieve and preallocate variables*)
	ccase=VARObject["ccase"];
	nvar=VARObject["nvar"];
	nlags=VARObject["nlags"];
	Ft=VARObject["Ft"];
	sigma=VARObject["sigma"];
	
	IRF=ConstantArray[0,{nsteps,nvar,nvar}];
	
	(*Comupte the IR*)
	
	Do[	
		F=Transpose[Ft];
		Fcomp=Join[
			F[[All,1+ccase;;nvar*nlags+ccase]],
			Join[IdentityMatrix[nvar*(nlags-1)],ConstantArray[0,{nvar*(nlags-1),nvar}],2]
			];
		FcompEye=IdentityMatrix[Length[Fcomp]];
		If[shut!=0,
			Fcomp[[shut,All]]=0];
		
		response=ConstantArray[0,{nvar,nsteps}];
		
		impulse=ConstantArray[0,nvar];
		
		Which[
			ident=="Cholesky",
				out=CholeskyDecomposition[sigma];
				invA=Transpose[out];,
			ident=="BlanchardQuah",
				FinfBig=Inverse[IdentityMatrix[Length@Fcomp]-Fcomp];
				Finf=FinfBig[[;;nvar,;;nvar]];
				(*simmetricize the matrix as in http://mathematica.stackexchange.com/questions/13808/can-the-choleskydecomposition-function-in-mathematica-be-made-to-work-on-non-sym*)
				matrixToBeSimetricized=Finf.sigma.Transpose[Finf];
				simmetricMatrix=matrixToBeSimetricized+Transpose[matrixToBeSimetricized];
				d=Transpose[CholeskyDecomposition[simmetricMatrix]];
				invA=LinearSolve[Finf,d];,
			True,
				Message[VARImpulseResponse::identError,ident];
				Abort[];
			];
		
		
			(*Set the size of the shock*)
			Which[
				impact==0,
					impulse[[mm]]=1,
				impact==1,
					impulse[[mm]]=1/invA[[mm,mm]],
				True,
					Message[VARImpulseResponse::impactError,ident];
					Abort[]];
			(*First period impulse response = impulse vector*)
			response[[All,1]]=invA.impulse;
			
			If[shut!=0,response[[shut,1]]=0];
			
			(*Make it comparable with companion*)
			impulseBig=Flatten[Join[invA.impulse, ConstantArray[0, {1, nvar*(nlags-1)}]]];
			
			
			Do[
				FcompEye=FcompEye.Fcomp;
				responseBig=FcompEye.impulseBig;
				response[[All,kk]]=responseBig[[1;;nvar]];,
				{kk, 2, nsteps}];
			IRF[[All,All,mm]]=Transpose[response];,
			{mm, 1, nvar}];
			
	output=Association[
		"nsteps"->nsteps,
		"ident"->ident,
		"impact"->impact,
		"invA"->invA,
		"Fcomp"->Fcomp,
		"maxEig"->Max[Abs[Eigenvalues[Fcomp]]],
		"shut"->"shut",
		"IRF"->IRF,
		"FComp"->Fcomp
		];
		
	output["Ft"]=Ft;
	output["nvar"]=nvar;
	output["nvarex"]=VARObject["nvarex"];
	output["nlags"]=VARObject["nlags"];
	output["ccase"]=VARObject["ccase"];
	output["nobs"]=VARObject["nobs"];
	output["Y"]=VARObject["Y"];
	output["FitResiduals"]=VARObject["FitResiduals"];
	
	output
];

PackageExport["VARImpulseResponse"]

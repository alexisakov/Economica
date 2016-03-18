(* Mathematica package *)
(*DFMPrepareData takes the inputed monthly and quarterly data
and transforms them into the format suitable for further operations.
It produces an association.
*)

BeginPackage["Economica`"]
(* Exported symbols added here with SymbolName::usage *)  
DFMPrepareData::usage="DFMPrepareData[M,q,n]"

Begin["`Private`"] (* Begin Private Context *) 
DFMPrepareData::incrtdts="First observation of the monthly data should be three months prior to the first observation of the 
quarterly data";
DFMPrepareData::yxlen="Lenght of monthly data does not match length of quarterly data. Do something."


DFMPrepareData[M_?MatrixQ,q_?MatrixQ,n_?IntegerQ]:=Module[
	{A = Association[],
		dataQ,datesQ,additionalDatesQ,nullQ,
		dataM,datesM,additionalDatesM,nullM},

	dataQ = q[[All, 2]];
	dataQ = squeeze@
	    KroneckerProduct[dataQ, {{Null, Null, 1}}\[Transpose]] /. {0 -> 
	     Null, x_ /; 
	      MemberQ[{Power, Plus, Times}, Head@x] && Not@NumericQ[x] :> 
	     Null};
	datesQ = q[[All, 1]];
	datesQ = DateRange[DatePlus[First@datesQ, {-2, "Month"}], Last@datesQ,
	    "Month"];
	(*add additional dates and nulls*)
	additionalDatesQ = 
	  Rest@DateRange[Last@datesQ, DatePlus[Last@datesQ, {4, "Quarter"}], 
	    "Month"];
	nullQ = ConstantArray[Null, Length@additionalDatesQ];
	dataQ = Join[dataQ, nullQ];
	datesQ = Join[datesQ, additionalDatesQ];
	
	dataM = M[[All, 2 ;;]];
	datesM = M[[All, 1]];
	(*add additional dates and nulls*)
	If[Last@datesM < Last@datesQ,
	  additionalDatesM = Rest@DateRange[Last@datesM, Last@datesQ, "Month"];
	  nullM = 
	   ConstantArray[
	    Null, {Length[additionalDatesM], Last@Dimensions@dataM}];
	  dataM = Join[dataM, nullM];
	  datesM = Join[datesM, additionalDatesM];];
	
	(*prepend as much null months as needed to the start*)
	(*TODO: NOT YET IMPLEMENTED JUST PROVIDE THE DATA WITH THE SAME \
	STARTING MONHTH*)
	Which[Length@dataM != Length@dataQ, Message[DFMPrepareData::incrtdts];
	   Abort[];];
	
	A["Dates"] = datesQ;
	
	A["DiscardMonthsToObtainBalancedDataset"] = 
	  Length@TakeWhile[Reverse@dataM, MemberQ[#, Null] &];
	
	A["x"] = dataM; A["y"] = dataQ;
	(*A["mqFirtsObservationDifference"]=mqFirtsObservationDifference;
	A["monthOfquarter"]=monthOfquarter;*)
	A
	]

End[] (* End Private Context *)

EndPackage[]




(*	{
	X=M,Y=q,mqFirtsObservationDifference,monthOfquarter,
	A=Association[]		
	},
	X[[All,1]]=DatePlus[#, {1-DateValue[DayRound[#, "EndOfMonth"], "Day"], "Day"}]&/@X[[All,1]];
	Y[[All,1]]=DatePlus[#, {1-DateValue[DayRound[#, "EndOfMonth"], "Day"], "Day"}]&/@Y[[All,1]];
	mqFirtsObservationDifference=QuantityMagnitude@DateDifference[X[[1,1]],Y[[1,1]], "Month"];
	monthOfquarter=Mod[DateValue[X[[1,1]], "Month"],3,1];
	(*1. Prepare the data*)
	(*The convention is that the quarterly data is assigned to the last month of the quarter.
	The data starts 2 months prior to the first quarterly observation.*)
	
	(*TODO: Need to produce correct data preparation. Now we go with the most simple:*)
	
	A["Dates"]=DateRange[X[[1,1]],DatePlus[Y[[-1,1]],{n,"Quarter"}],{1,"Month"}];
	
	Which[
		mqFirtsObservationDifference==2,
			X=X[[All,2;;]];Y=Y[[All,2]];,
		True,
			Message[DFMPrepareData::incrtdts];Abort[];
	];
	(*Left-over from the attepmt:*)
(*	Which[
		mqFirtsObservationDifference==2,
			X=X[[All,2;;]];Y=Y[[All,2]];,
		mqFirtsObservationDifference<2,
			X=X[[3-Mod[DateValue[M[[1,1]], "Month"],3,1];;,2;;]];
			Y=Y[[Ceiling@(Abs@mqFirtsObservationDifference/3.);;,2]];,
		mqFirtsObservationDifference>2,
			X=X[[mqFirtsObservationDifference-1;;,2;;]];Y=Y[[All,2]];
	];*)
(*	Quarterly at monthly frequency*)
	Y=KroneckerProduct[Y, {{Null, Null, 1}}\[Transpose]] /. {0 -> Null,
		x_ /; MemberQ[{Power, Plus, Times}, Head@x] && Not@NumericQ[x] :>  Null};
	
	Y=Join[Y,ConstantArray[Null, n*3]];
	X = Join[X, ConstantArray[Null, {Length@Y-Length@X, Last@Dimensions@X}]];
	
	
	A["DiscardMonthsToObtainBalancedDataset"] = Length@TakeWhile[Reverse@X,MemberQ[#,Null]&]; 
	
	If[Length@Y!=Length@X,Message[DFMPrepareData::yxlen];Abort[]];
			
	A["x"]=X; A["y"]=Y;
	(*	A["mqFirtsObservationDifference"]=mqFirtsObservationDifference;
	A["monthOfquarter"]=monthOfquarter;*)
	A
	(*2. *)*)

Package["Economica`"]
(*DFMPrepareData takes the inputed monthly and quarterly data
and transforms them into the format suitable for further operations.
It produces an association.
*)

(* Exported symbols added here with SymbolName::usage *)  
DFMPrepareData::usage="DFMPrepareData[M,q,n]"

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

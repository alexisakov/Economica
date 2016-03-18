(* Mathematica Package *)

BeginPackage["Economica`"]
(* Exported symbols added here with SymbolName::usage *)  

ImportCEIC::usage="ImportCEIC[] imports an Excel sheet converting it to an Association[] of time series";

ImportTimeSeries::usage="ImportTimeSeries[] imports an Excel sheet converting it to an Association[] of TimeSeries[]";
ImportIRIS::usage="ImportIRIS[] imports an Excel sheet converting it to an Association[] of TimeSeries[]";

Begin["`Private`"] (* Begin Private Context *) 
Options[ImportCEIC]={SeriesInformation->True};


(*ImportCEIC[XLFile_, SheetName_,OptionsPattern[]] := Module[
  {impt = Import[XLFile, {"Sheets", SheetName}],
   timesseries, dims, offset},
  offset=If[OptionValue[SeriesInformation]==True,12,2];
  timesseries = impt[[offset;;]];
  dims = Dimensions@timesseries;
  timesseries[[All, 1]] = (DatePlus[{1900, 1, 1}, # - 2])& /@ timesseries[[All, 1]];
  timesseries = timesseries[[All, {1, #}]] & /@ Range[2, Last@dims];
  timesseries = timesseries /. {_, "#N/A"} :> Sequence[];
  timesseries = timesseries /. {_, $Failed} :> Sequence[];
  Association[Rule @@@ ({impt[[1, 2 ;;]], timesseries}\[Transpose])]
  ];*)
  
  
  ImportCEIC[XLFile_, SheetName_String, OptionsPattern[]] := Module[
	  {impt = Import[XLFile, {"Sheets", SheetName}]},
	  ParseXlSheet@impt	 
  ];
  
 ImportCEIC[XLFile_, SheetName_List, OptionsPattern[]] := Module[
	  {impt = Import[XLFile, {"Sheets", SheetName}]},
	  ParseXlSheet/@impt	 
 ];
  

Options[ParseXlSheet]=Options[ImportCEIC]
ParseXlSheet[sheet_,OptionsPattern[]]:=Module[
	{timesseries, dims, offset},
	offset=If[OptionValue[SeriesInformation]==True,12,2];
  	timesseries = sheet[[offset;;]];
  	dims = Dimensions@timesseries;
  	timesseries[[All, 1]] = (DatePlus[{1900, 1, 1}, # - 2])& /@ timesseries[[All, 1]];
  	timesseries = timesseries[[All, {1, #}]] & /@ Range[2, Last@dims];
  	timesseries = timesseries /. {_, "#N/A"} :> Sequence[];
  	timesseries = timesseries /. {_, $Failed} :> Sequence[];
  	Association[Rule @@@ ({sheet[[1, 2 ;;]], timesseries}\[Transpose])]
];
  
  
  
  (*TODO: Write a version of the function where there is no need to 
  specify the name of the sheet as it imports all of them.*)
Options[ImportTimeSeries]=Options[ImportCEIC];

With[{opt = First /@ Options[ImportCEIC]},
 	ImportTimeSeries[XLFile_String, SheetName_String,OptionsPattern[]]:=(
		Map[TimeSeries,ImportCEIC[XLFile, SheetName,Sequence @@ ((# -> OptionValue[#]) & /@ opt)]]);
	ImportTimeSeries[XLFile_, SheetName_List,OptionsPattern[]]:=Module[
		{op,impCEIC},
		op=((#->OptionValue[#])&/@opt);
		impCEIC=ImportCEIC[XLFile, SheetName, Sequence@@op];
		Map[TimeSeries/@#&,impCEIC]
	];
];
 

Options[ImportIRIS]=Options[ImportCEIC]
ImportIRIS[XLFile_, OptionsPattern[]] := Module[
  {impt = Import[XLFile],
   timesseries, dims, offset, ts},
  offset=If[OptionValue[SeriesInformation]==True,3,3];
  timesseries = impt[[offset;;]];
  dims = Dimensions@timesseries;
  timesseries[[All, 1]] = DatePlus[DateList[{#, {"Year", "Q", "Quarter"}}], {2, "Month"}] & /@ timesseries[[All, 1]];
  timesseries = timesseries[[All, {1, #}]] & /@ Range[2, Last@dims];
  timesseries = timesseries /. {_, "NaN"} :> Sequence[];
  timesseries = timesseries /. {_, $Failed} :> Sequence[];
  ts=Association[Rule @@@ ({impt[[1, 2 ;;]], timesseries}\[Transpose])];
  Map[TimeSeries,ts]
  ];
  


End[] (* End Private Context *)

EndPackage[]

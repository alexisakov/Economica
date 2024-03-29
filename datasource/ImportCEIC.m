Package["Economica`"]

ImportCEIC::usage="ImportCEIC[] imports an Excel sheet converting it to an Association[] of time series";

ImportTimeSeries::usage="ImportTimeSeries[] imports an Excel sheet converting it to an Association[] of TimeSeries[]";
ImportIRIS::usage="ImportIRIS[] imports an Excel sheet converting it to an Association[] of TimeSeries[]";

ExcelDateToDateList::usage="To date list"

(*https://mathematica.stackexchange.com/questions/184535/11-3-import-of-files-with-paths-containing-non-ascii-characters
The change in mathematica Importer 
*)

Options[ImportCEIC]={SeriesInformation->True};



ISOWeekDateToDate[s_String] := Module[{
   dt = DateObject[{ToExpression@StringTake[s, 4], 1, 1}] + 
     Quantity[2, "Days"] - 
     Quantity[
      DateValue[{ToExpression@StringTake[s, 4], 1, 3}, "ISOWeekDay"], 
      "Days"] + Quantity[7*ToExpression@StringTake[s, -2], "Days"]
   },
  DateList[dt][[;; 3]]
  ]


ExcelDateToDateList[x_]:= (DatePlus[{1900, 1, 1}, x - 2]);
  
ImportCEIC[XLFile_, SheetName_String, OptionsPattern[]] := Module[
	  {impt},
	  impt=If[$VersionNumber<11.3,
	  	Import[XLFile, {"Sheets", SheetName}],
	  	Import[XLFile, {"SheetsLegacy", SheetName}]
	  	];
	  ParseXlSheet@impt	 
  ];
  
 ImportCEIC[XLFile_, SheetName_List, OptionsPattern[]] := Module[
	 {impt},
	  impt=If[$VersionNumber<11.3,
	  	Import[XLFile, {"Sheets", SheetName}],
	  	Import[XLFile, {"SheetsLegacy", SheetName}]
	  	];
	  ParseXlSheet/@impt	 
 ];
  

Options[ParseXlSheet]=Options[ImportCEIC]
ParseXlSheet[sheet_,OptionsPattern[]]:=Module[
	{timesseries, dims, offset},
	offset=If[OptionValue[SeriesInformation]==True,12,2];
  	timesseries = sheet[[offset;;]];
  	dims = Dimensions@timesseries;
  	
  	(*This deals with dates: 
  		if the date is properly imported as DateObject, then do nothing
  		if date is imported as Excel number, then convert appropriately*)

	If[ Head[timesseries[[1,1]]] === DateObject,
		timesseries[[1,1]] = timesseries[[1,1]],
		timesseries[[All, 1]] = ExcelDateToDateList /@ timesseries[[All, 1]]
		];
  	timesseries = timesseries[[All, {1, #}]] & /@ Range[2, Last@dims];
  	timesseries = timesseries /. {_, "#N/A"} :> Sequence[];
  	timesseries = timesseries /. {_, $Failed} :> Sequence[];
  	Association[Rule @@@ ({sheet[[1, 2 ;;]], timesseries}\[Transpose])]
];
  
  
  
(*TODO: Write a version of the function where there is no need to specify the name of the sheet as it imports all of them.*)
Options[ImportTimeSeries]=Options[ImportCEIC];

With[{opt = First /@ Options[ImportCEIC]},
 	
 	ImportTimeSeries[XLFile_String, SheetName_String,OptionsPattern[]]:=(
		Map[TimeSeries,ImportCEIC[XLFile,SheetName,Sequence @@ ((# -> OptionValue[#]) & /@ opt)]]);

	ImportTimeSeries[XLFile_, SheetName_List,OptionsPattern[]]:=Module[
		{op,impCEIC},
		op=((#->OptionValue[#])&/@opt);
		impCEIC=ImportCEIC[XLFile, SheetName, Sequence@@op];
		Map[TimeSeries/@#&,impCEIC]
	];
];

Options[ImportIRIS]=Join[Options[ImportCEIC],{Frequency->"Quarter",RowOffset->3}]

ImportIRIS[XLFile_, OptionsPattern[]] := Module[
  {impt = Import[XLFile],
   timesseries, dims, offset, ts},
  offset=OptionValue[RowOffset];
  timesseries = impt[[offset;;]];
  dims = Dimensions@timesseries;

  Which[
    OptionValue[Frequency]=="Quarter",
      timesseries[[All, 1]] = DatePlus[DateList[{#, {"Year", "Q", "Quarter"}}], {2, "Month"}] & /@ timesseries[[All, 1]],
    OptionValue[Frequency]=="Month",
      timesseries[[All, 1]] = DateList[{#, {"Year", "M", "Month"}}] & /@ timesseries[[All, 1]],
    OptionValue[Frequency]=="Week",
      timesseries[[All, 1]] = ISOWeekDateToDate[#] & /@ timesseries[[All, 1]]
    ]; 

  timesseries = timesseries[[All, {1, #}]] & /@ Range[2, Last@dims];
  timesseries = timesseries /. {_, "NaN"} :> Sequence[];
  timesseries = timesseries /. {_, $Failed} :> Sequence[];
  ts=Association[Rule @@@ ({impt[[1, 2 ;;]], timesseries}\[Transpose])];
  Map[TimeSeries,ts]
  ];
  
PackageExport["ImportIRIS"]
PackageExport["ImportTimeSeries"]
PackageExport["ExcelDateToDateList"]
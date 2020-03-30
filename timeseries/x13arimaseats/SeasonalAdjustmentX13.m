Package["Economica`"]

(*
Author: Alex Isakov
Contact: ale.isakov@gmail.com

Version V.5 2019-07-01
More options, better support for working day adjustment functions.
Version: V .4 2015-07-13
Added ability to run from the program folder without the need to copy to Windows\System32.
Version: V .3 2014-03-04
Added quarterly data support. Frequency is determined automatically.
Version: V .2 2014-03-04

TODO: 2014-01-22 add test for missing values and an error message
TODO: ability to set coefficient estimates for the model
TODO: https://mathematica.stackexchange.com/questions/178863/run-does-not-work
(*DetermineTimeSeriesFrequency[]*)

*)

SeasonalAdjustmentX13::usage="SeasonalAdjustmentX13[s] seasonally adjust time-series s."

CreateTemporaryX13FreeFile::usage="Create temporary file from TimeSeries object."

x13::mode="Option Mode can only take following values: auto, mult, add, pseudoadd, logadd";
x13::freq="ARIMA-X13 supports only frequencies Month and Quarter";
x13::threeyear="ARIMA-X13 runs only on 3 or more years of observations";
x13::thirtyseconds="File has not been generated.\n `1`";
runx13::nosuchfreq="X11 supports only monthly of quarterly data";


Options[SeasonalAdjustmentX13]={
	(*ARIMA->"(1 1 1)(0 1 1)",*)
	SAX13Transform->"none",
	Frequency->"Month",
	SAX13X1Output->"d11",
	(*Outliers->{},*) 
	SAX13MaxLead->6, 
	X11Regression->False,
	SAX13RegressionBuiltIn -> False,
	X11RegressionSave->False
	(*,StockOrFlow->"Stock"*)
};


(*ARIMAX13 utilities*)
QuoteString[s_]:="\""<>ToString@s<>"\"";
StringDeleteBraces[s_]:=StringReplace[ToString@s,{"{"->"","}"->""}];
StringSwapBraces[s_]:=StringReplace[ToString@s,{"{"->"(","}"->")"}];

$X13Directory=DirectoryName[$InputFileName];

(*ReadX13Output - function for returning ARIMA-X13 output*)
Options[ReadX13Output]={DatesColumn->True,X13Period->12};

ReadX13Output[output_String, OptionsPattern[]]:=Module[{imp},
	(*We pause for 30 sec as it takes about this times to generate files*)
	TimeConstrained[
		While[Not@FileExistsQ[
			FileNameJoin[{$TemporaryDirectory,If[Length@output>0,"mmax13."<>output,
				"mmax13."<>output]}]]],
				15,
				Message[x13::thirtyseconds,Import[FileNameJoin[{$TemporaryDirectory,"mmax13.err"}],"Text"]];
				Abort[]];
	imp=Import[FileNameJoin[{$TemporaryDirectory,"mmax13."<>output}],{"Data"}];
	DeleteFile[FileNameJoin[{$TemporaryDirectory,"mmax13."<>output}]];
	If[
		OptionValue[DatesColumn],
		imp[[3;;]]/.{x_,y__/;Length[{y}]<=10}:>

		Which[
				OptionValue[X13Period]==12,
					{DateList[{ToString@x, {"Year", "", "Month"}}],y},
				OptionValue[X13Period]==4,
					{(DateList[{StringJoin[{StringTake[ToString@x,4],StringTake[ToString@x,{-1}]}], {"Year", "", "Quarter"}}])+{0, 2, 0, 0, 0, 0},y}
			],
		imp[[3;;]]]
	];

ReadX13Output[output_List,OptionsPattern[]]:=Module[
	{},
	ReadX13Output[#,
	DatesColumn->True,
	X13Period->OptionValue[X13Period]]&/@output];

	(*CreateTemporaryX13FreeFile - function for preparing files in ARIMA-X13-readable format*)
	Options[CreateTemporaryX13FreeFile]={X13Period->12};
	CreateTemporaryX13FreeFile[filename_, x_,OptionsPattern[]] := Module[
	{dates,values,exportText},
	Which[
		(OptionValue@X13Period)==12,
			dates = DateString[#, {"Year", " ", "MonthShort"}] & /@ If[ListQ@x,First@(#["Dates"]&/@ x),x["Dates"]],
		(OptionValue@X13Period)==4,
			dates = DateString[#, {"Year", " ", "Quarter"}] & /@ If[ListQ@x,First@(#["Dates"]&/@ x),x["Dates"]]
	];
	values = If[ListQ@x,#["Values"]&/@ x,x["Values"]];
	exportText=StringReplace[StringJoin@Riffle[(StringDeleteBraces@#)<>" " & /@ (If[ListQ@x,Flatten[{{dates}, values},1],{dates, values}]\[Transpose]), "\n"], "," -> ""];
	Export[FileNameJoin[{$TemporaryDirectory,filename }],exportText,"Text"];
  ];


(* ***The key SA function*** *)
SeasonalAdjustmentX13[x_,OptionsPattern[]]:=Module[
		{input=x["Path"] /. {xx_, y_} :> {DateList@xx, y}, 
		outputText,
		originalDates=DateList /@ x["Dates"],
		inputfile, period
		},

	(*TODO: Determine the period of the series in a function.*)
	period=Max[Length/@GatherBy[DateList/@originalDates,First]];
	
(*	If[OptionValue[StockOrFlow]=="Flow",
			input[[All, 2]] = Rest@FoldList[#1*(1 + #2/100.) &, 1, input[[All, 2]]]
			];*)
		
		CreateTemporaryX13FreeFile["x11reg.dat",TimeSeries@input,X13Period->period]; 
		inputfile=FileNameJoin[{$TemporaryDirectory,"x11reg.dat"}];
		
		outputText="series{
			title = \""<>"Title"<>"\"
			period = "<>ToString@period<>"
			file = "<> QuoteString@inputfile <>"
			format = \"DATEVALUE\""<>"
			
		}\n"

		<> "transform{function="<>OptionValue[SAX13Transform]<>"}\n"

		<> (If[OptionValue[X11Regression] === False, "",
			CreateTemporaryX13FreeFile["x11reg.var",OptionValue[X11Regression]]; 
			inputfile=FileNameJoin[{$TemporaryDirectory,"x11reg.var"}];

			"regression {"
				<> 
				If[OptionValue[SAX13RegressionBuiltIn]===False,"",

				"
				variables = "<> OptionValue[SAX13RegressionBuiltIn] <>"
				"
				]

				<>"
				user = "<> If[
					ListQ@OptionValue[X11Regression],
					StringSwapBraces[("\"somevariable"<>ToString[#]<>"\"")&/@(Range@Length@OptionValue[X11Regression])],
					"\"somevariable\""] <>"
				usertype = "<> If[ListQ@OptionValue[X11Regression],
					StringSwapBraces[Prepend[ConstantArray["user",Length@OptionValue[X11Regression]-1],"td"]],
					"td"] <>"
				file = "<> QuoteString@inputfile <>"

				format = \"DATEVALUE\"\n

				"<>

				If[
					 OptionValue[X11RegressionSave] === False,
 					"",
					"save = (" <> StringDeleteBraces@ToString[OptionValue[X11RegressionSave]] <> ")"]
				<>"
				}"]) <>
		"

		pickmdl{
	    		mode = both
	     		file = \""<>Export[FileNameJoin[{$TemporaryDirectory,"pickmdl.mds" }],
				"(0, 1, 1)(0, 1, 1) *
				(0, 1, 2)(0, 1, 1) X
				(2, 1, 0)(0, 1, 1) X
				(0, 2, 2)(0, 1, 1) X
				(2, 1, 2)(0, 1, 1)", "Text"] <>"\"
     		}

 		outlier{}

		forecast{
			maxlead = " <> ToString[OptionValue[SAX13MaxLead]] <> "
			maxback = 12}
			
		x11{
			# mode= " (*<> If[OptionValue[StockOrFlow]=="Flow","mult","mult"] *)<>"
			final = (user)
			appendfcst = YES
			print=none
			save=("<>Quiet@Check[StringDeleteBraces@ToString[OptionValue[SAX13X1Output]],""]<>")
		}";
		
		inputfile =FileNameJoin[{$TemporaryDirectory,"mmax13"}];

		Export[inputfile <> ".spc",outputText,"Text"];
		
		Run["cd \"" <> $X13Directory <> "\" && .\\x13as", QuoteString@inputfile];
			
		output = Flatten@Join[
					If[OptionValue[SAX13X1Output]===False,{},{OptionValue[SAX13X1Output]}],
					If[OptionValue[X11RegressionSave]===False,{},{OptionValue[X11RegressionSave]}]
					];

		inputfile=ReadX13Output[output,DatesColumn->True,X13Period->period];
		
		If[Length@output==1,
			TimeSeries@inputfile,
			TimeSeries/@inputfile
		]

		
]

PackageExport["SeasonalAdjustmentX13"]

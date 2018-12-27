(* ::Package:: *)

(*
Author: Alex Isakov
Contact: ale.isakov@gmail.com
Version: V .4 2015-07-13
Added ability to run from the program folder without the need to copy to Windows\System32.
Version: V .3 2014-03-04
Added quarterly data support. Frequency is determined automatically.
Version: V .2 2014-03-04

TODO: 2014-01-22 add test for missing values and an error message
TODO: add ability to including working day factors in separate files in regression statement
TODO: Ability to use pickModel to test for arbitratry model structure, not only an optimal one.
TODO: ability to set coefficient estimates for the model
TODO: add a check of whether there is a x13as.exe and try to copy it to the system32
TODO: add ability to run x13 without copying the file to system32: 
	pathToExe=FileNameJoin[{$UserBaseDirectory,"Applications","Economica","Kernel","TimeSeries","x13","X13AS"}]
TODO: Switch between negative and positive values
TODO: Add frequency options: month and quarter
TODO: Add Multivariate support
TODO: https://mathematica.stackexchange.com/questions/178863/run-does-not-work


*)

BeginPackage["Economica`"]

SeasonalAdjustmentX13::usage="SeasonalAdjustmentX13[s] seasonally adjust time-series s."

CreateTemporaryX13FreeFile::usage="Create temporary file from TimeSeries object."
QuoteString[s_]:="\""<>ToString@s<>"\"";
Begin["`Private`"] (* Begin Private Context *) 

(*ClearAll[x13,runx13]*)


(*FileNameJoin[{$UserBaseDirectory, 
  "Applications\\Economica\\Kernel\\TimeSeries\\x13"}]*)

(*Options[x13]={ARIMA->True,Mode->"auto",Frequency->"Month", Output->"d11",Outliers->{}};
Options[runx13]=Options[x13];
Options[adjustTextX13]=Options[SeasonalAdjustmentX13];*)

Options[outlierX13]={Outliers->All};
x13::mode="Option Mode can only take following values: auto, mult, add, pseudoadd, logadd";
x13::freq="ARIMA-X13 supports only frequencies Month and Quarter";
x13::threeyear="ARIMA-X13 runs only on 3 or more years of observations";
x13::thirtyseconds="File has not been generated.\n `1`";
runx13::nosuchfreq="X11 supports only monthly of quarterly data";


Options[SeasonalAdjustmentX13]={
	ARIMA->"(1 1 1)(0 1 1)",Transform->"auto",Frequency->"Month",
	Output->"d11",
	Outliers->{}, 
	X11Regression->False,
	StockOrFlow->"Stock"};

(*ARIMAX13 utilities*)

StringDeleteBraces[s_]:=StringReplace[ToString@s,{"{"->"","}"->""}];
StringSwapBraces[s_]:=StringReplace[ToString@s,{"{"->"(","}"->")"}];
$X13Directory=DirectoryName[$InputFileName];

(*DetermineTimeSeriesFrequency[]*)

Options[ReadX13Output]={DatesColumn->True,X13Period->12};

ReadX13Output[output_String, OptionsPattern[]]:=Module[{imp},
	(*We pause for 30 sec as it takes about this times to generate files*)
	TimeConstrained[
		While[Not@FileExistsQ[
			FileNameJoin[{$TemporaryDirectory,If[Length@output>0,"mmax13."<>First@output,
				"mmax13."<>output]}]]],
				15,
				Message[x13::thirtyseconds,Import[FileNameJoin[{$TemporaryDirectory,"mmax13.err"}],"Text"]];
				Abort[]];
	imp=Import[FileNameJoin[{$TemporaryDirectory,"mmax13."<>output}],{"Data"}];
	DeleteFile[FileNameJoin[{$TemporaryDirectory,"mmax13."<>output}]];
	If[
		OptionValue[DatesColumn],
		imp[[3;;]]/.{x_,y__/;Length[{y}]<=10}:>{{Round[x/100,1],
			Which[
				OptionValue[X13Period]==12,x-Round[x,100],
				OptionValue[X13Period]==4,(x-Round[x,100])*3],28},y},
		imp[[3;;]]]
	];

ReadX13Output[output_List,OptionsPattern[]]:=Module[
	{},
	ReadX13Output[#,DatesColumn->True,X13Period->OptionValue[X13Period]]&/@output];


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
	
	If[OptionValue[StockOrFlow]=="Flow",
			input[[All, 2]] = Rest@FoldList[#1*(1 + #2/100.) &, 1, input[[All, 2]]]
			];
		
		CreateTemporaryX13FreeFile["x11reg.dat",TimeSeries@input,X13Period->period]; 
		inputfile=FileNameJoin[{$TemporaryDirectory,"x11reg.dat"}];
		
		outputText="series{
			title = \""<>"Title"<>"\"
			period = "<>ToString@period<>"
			file = "<> QuoteString@inputfile <>"
			format = \"DATEVALUE\""<>"
			
		}\n"
		

		<> If[OptionValue[StockOrFlow]=="Flow","transform{function=log}", ""] 

		<> (If[OptionValue[X11Regression] === False, "",
			CreateTemporaryX13FreeFile["x11reg.var",OptionValue[X11Regression]]; 
			inputfile=FileNameJoin[{$TemporaryDirectory,"x11reg.var"}];
			"x11regression {
				user = "<> If[ListQ@OptionValue[X11Regression],
					StringSwapBraces[("\"somevariable"<>ToString[#]<>"\"")&/@(Range@Length@OptionValue[X11Regression])],
					"\"somevariable\""] <>"
				usertype = "<> If[ListQ@OptionValue[X11Regression],
					StringSwapBraces[Prepend[ConstantArray["user",Length@OptionValue[X11Regression]-1],"td"]],
					"td"] <>"
				file = "<> QuoteString@inputfile <>"

				format = \"DATEVALUE\"
				}"]) <>
		"
		forecast{
			maxlead = 6
			maxback = 6}
			
		x11{
			mode= " <> If[OptionValue[StockOrFlow]=="Flow","mult","mult"] <>"
			final = (user)
			print=none
			save=("<>Quiet@Check[StringDeleteBraces@ToString[OptionValue[Output]],""]<>")}";
		
		inputfile =FileNameJoin[{$TemporaryDirectory,"mmax13"}];
		Export[inputfile <> ".spc",outputText,"Text"];
		Run["cd \"" <> $X13Directory <> "\" && .\\x13as", QuoteString@inputfile];
			
		inputfile=ReadX13Output[OptionValue[Output],DatesColumn->True,X13Period->period];
		
		(*If[OptionValue[StockOrFlow]=="Flow",
			inputfile = (Ratios@inputfile-1)*100;
			originalDates=originalDates[[2;;]];
			];*)
		
		TimeSeries@Transpose[{originalDates,inputfile[[All,2]]}]
]


End[] (* End Private Context *)
EndPackage[]

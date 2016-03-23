(* ::Package:: *)

(*
Author: Alex Isakov
Contact: ale.Isakov@gmail.com
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
TODO: Add Multivariate support*)

(*BeginPackage["Economica`"]

SeasonalAdjustmentX13::usage="SeasonalAdjustmentX13[s] seasonally adjust time-series s."

Begin["`Private`"] (* Begin Private Context *) 

ClearAll[x13,runx13]
quote[s_]:="\""<>s<>"\"";
deleteBraces[s_String]:=StringReplace[s,{"{"->"","}"->""}];
changeBraces[s_String]:=StringReplace[ToString@s,{"{"->"(","}"->")"}];
$X13Directory=DirectoryName[$InputFileName];

(*FileNameJoin[{$UserBaseDirectory, 
  "Applications\\Economica\\Kernel\\TimeSeries\\x13"}]*)

Options[x13]={ARIMA->True,Mode->"auto",Frequency->"Month",
Output->"e2",Outliers->{}};
Options[runx13]=Options[x13];
Options[adjustTextX13]=Options[SeasonalAdjustmentX13];
Options[readX31output]={DatesColumn->True,X13Period->12};
Options[outlierX13]={Outliers->All};
x13::mode="Option Mode can only take following values: auto, mult, add, pseudoadd, logadd";
x13::freq="ARIMA-X13 supports only frequencies Month and Quarter";
x13::threeyear="ARIMA-X13 runs only on 3 or more years of observations";
x13::thirtyseconds="File has not been generated.\n `1`";
runx13::nosuchfreq="X11 supports only monthly of quarterly data";


x13[x_,OptionsPattern[]]:=Module[
{},

Which[
ARIMA \[Element] Booleans && ARIMA==True, SetOptions[x13,ARIMA]=8,
ARIMA \[Element] Booleans && ARIMA==False, SetOptions[x13,ARIMA]=0,
True,Null];

If[
	MemberQ[{"auto","mult", "add", "pseudoadd", "logadd"},OptionValue[x13,Mode]],
	Null,
	Message[x13::mode];Abort[]
];

If[
	MemberQ[{"Month","Quarter"},OptionValue[x13,Frequency]],
	Null,
	Message[x13::freq];Abort[]
];

If[
	Length@x<(OptionValue[x13,Frequency]/.{"Month"->12,"Quarter"->4})*3,
	Message[x13::threeyear];Abort[],
	Null
];

(*Add error and condtition for the Output option, all values should be in the table in the manual
If[
	Length@x<(OptionValue[x13,Frequency]/.{"Month"->12,"Quarter"->4})*3,
	Message[x13::threeyear];Abort[],
	Null
];
*)

(*Add error and condtition for oulier option. 
It should be a regular expression that checks whether they are propperly syntax.
If[
	Length@x<(OptionValue[x13,Frequency]/.{"Month"->12,"Quarter"->4})*3,
	Message[x13::threeyear];Abort[],
	Null
];
*)

(*After checking for issues run the adjustment procedute*)
runx13[x,Outliers->OptionValue[Outliers],Output->OptionValue[Output]]

]





runx13[x_,OptionsPattern[]]:=Module[
{outputText,
dateList=Sort@x,dataList,
startDate,endDate,
spanDate,
inputfile,period

},


(*Determine the period of the series*)
period=Max[Length/@GatherBy[dateList[[;;,1]],First]];
dataList=dateList[[;;,2]];
Which[
	period==12,
		startDate=DateString[First@First@dateList,{"Year",".","Month"}];
		endDate=DateString[First@Last@dateList,{"Year",".","Month"}];
		spanDate=DateString[DatePlus[First@First@dateList,{3,"Year"}],{"Year",".","Month"}];,
	period==4,
		startDate=DateString[First@First@dateList,{"Year",".","Quarter"}];
		endDate=DateString[First@Last@dateList,{"Year",".","Quarter"}];
		spanDate=DateString[DatePlus[First@First@dateList,{3,"Year"}],{"Year",".","Quarter"}];,
	True,
		Message[runx13::nosuchfreq];Abort[]
];

outputText="series{
  title=\""<>"Title"<>"\"
  start="<>startDate<>"
  data=("<>Riffle[ToString[#]<>" "&/@dataList,"\n\t",13]<>")
  period="<>ToString@period<>"
}

transform{
  function=auto
  savelog=autotransform  
}

regression {
	variables = ("<>Quiet@Check[deleteBraces@ToString[OptionValue[Outliers]],""]<>")
}

#outlier{
# method=addone
# types = (tc ao ls)
#}


automdl{}

#pickmdl { 
#	mode = both
#	method = best
# "<> Export[FileNameJoin[{$TemporaryDirectory,"EVX13.MDL"}],"(0, 1, 1)(0, 0, 1) X
(0, 1, 2)(0, 0, 1) X
(2, 1, 0)(0, 0, 1) X
(0, 2, 2)(0, 0, 1) X
(0, 1, 1)(0, 1, 1) X
(0, 1, 2)(0, 1, 1) X
(2, 1, 0)(0, 1, 1) X
(0, 2, 2)(0, 1, 1) X
(2, 1, 2)(0, 1, 1)","Text"] <>"	
#	file = \""<>FileNameJoin[{$TemporaryDirectory,"EVX13.MDL"}]<>"\"
#	fcstlim = 25.0
#	bcstlim = 25.0
#	qlim = 15.0
#	outofsample = yes
#	}

x11{
  print=none
  save=("<>Quiet@Check[deleteBraces@ToString[OptionValue[Output]],""]<>")}

";

Export[FileNameJoin[{$TemporaryDirectory,"mmax13.spc"}],outputText,"Text"];

inputfile =FileNameJoin[{$TemporaryDirectory,"mmax13"}];

Run["cd \"" <> $X13Directory <> "\" && .\\x13as", quote@inputfile];

(*We pause for 30 sec as it takes about this times to generate files*)
TimeConstrained[
	While[Not@FileExistsQ[FileNameJoin[
	{$TemporaryDirectory,If[Length@OptionValue[Output]>0,"mmax13."<>First@OptionValue[Output],"mmax13."<>OptionValue[Output]]
	}]]],
15,
Message[x13::thirtyseconds,Import[FileNameJoin[{$TemporaryDirectory,"mmax13.err"}],"Text"]];Abort[]
];

(*Pause[30];*)

readX31output[OptionValue[Output],X13Period->period]

]



spectrumX13[x_,OptionsPattern[]]:=Module[
{outputText,
dateList=Sort@x,dataList,
startDate,endDate,
spanDate,
inputfile

},



dataList=dateList[[;;,2]];
startDate=DateString[First@First@dateList,{"Year",".","Month"}];
endDate=DateString[First@Last@dateList,{"Year",".","Month"}];
spanDate=DateString[DatePlus[First@First@dateList,{3,"Year"}],{"Year",".","Month"}];

outputText="series{
  title=\""<>"Title"<>"\"
  start="<>startDate<>"
  data=("<>Riffle[ToString[#]<>" "&/@dataList,"\n\t",13]<>")
}

spectrum{
	savelog = all
	save = (sp0)
}";

Export[FileNameJoin[{$TemporaryDirectory,"mmax13.spc"}],outputText,"Text"];

inputfile =FileNameJoin[{$TemporaryDirectory,"mmax13"}];

Run["cd \"" <> $X13Directory <> "\" && .\\x13as", quote@inputfile];

(*We pause for 30 sec as it takes about this times to generate files*)
TimeConstrained[
	While[Not@FileExistsQ[FileNameJoin[{$TemporaryDirectory,"mmax13.sp0"}]]],
15,
Message[x13::thirtyseconds,Import[FileNameJoin[{$TemporaryDirectory,"mmax13.err"}],"Text"]];Abort[]
];

(*Pause[30];*)

readX31output["sp0",DatesColumn->False,X13Period->period]

]



transformX13[x_]:=Module[
(**){},
"transform{function = "<>x<>"}"]


outlierX13[x_,OptionsPattern[]]:=Module[
{outputText,
dateList=Sort@x,dataList,
startDate,endDate,
spanDate,inputfile

},

dataList=dateList[[;;,2]];
startDate=DateString[First@First@dateList,{"Year",".","Month"}];
endDate=DateString[First@Last@dateList,{"Year",".","Month"}];
spanDate=DateString[DatePlus[First@First@dateList,{3,"Year"}],{"Year",".","Month"}];

outputText="series{
  title=\""<>"Title"<>"\"
  start="<>startDate<>"
  data=("<>Riffle[ToString[#]<>" "&/@dataList,"\n\t",13]<>")
}
Estimate { }
outlier{
	types = "<>changeBraces@ToString@OptionValue[Outliers]<>"
	save = (fts)
	method=addone
}";

Export[FileNameJoin[{$TemporaryDirectory,"mmax13.spc"}],outputText,"Text"];

inputfile =FileNameJoin[{$TemporaryDirectory,"mmax13"}];

Run["cd \"" <> $X13Directory <> "\" && .\\x13as", quote@inputfile];


(*We pause for 30 sec as it takes about this times to generate files*)
TimeConstrained[
	While[Not@FileExistsQ[FileNameJoin[{$TemporaryDirectory,"mmax13.fts"}]]],
15,
Message[x13::thirtyseconds,Import[FileNameJoin[{$TemporaryDirectory,"mmax13.err"}],"Text"]];Abort[]
];

(*Pause[30];*)

readX31output["fts"]

]



Options[pickmodelX13]={ARIMA->True,Transform->"auto",Frequency->"Month",
Output->"e2",Outliers->{}};


pickmodelX13[x_,OptionsPattern[]]:=Module[
{outputText,
dateList=Sort@x,dataList,
startDate,endDate,
spanDate, inputfile,
chosenModel

},
(*
TODO: Switch between negative and positive values
TODO: Add frequency options: month and quarter
TODO: Add Multivariate support*)

dataList=dateList[[;;,2]];
startDate=DateString[First@First@dateList,{"Year",".","Month"}];
endDate=DateString[First@Last@dateList,{"Year",".","Month"}];
spanDate=DateString[DatePlus[First@First@dateList,{3,"Year"}],{"Year",".","Month"}];

outputText="series{
  title=\""<>"Title"<>"\"
  start="<>startDate<>"
  data=("<>Riffle[ToString[#]<>" "&/@dataList,"\n\t",13]<>")
}
"<>""
(*transformX13[OptionValue[Transform]]*)
<>"


regression {
	variables = ("<>Quiet@Check[deleteBraces@ToString[OptionValue[Outliers]],""]<>")
}

pickmdl { 
	mode = both
	method = best
# "<> Export[FileNameJoin[{$TemporaryDirectory,"EVX13.MDL"}],
"(0, 1, 1)(0, 0, 1) X
(0, 1, 2)(0, 0, 1) X
(2, 1, 0)(0, 0, 1) X
(0, 0, 1)(0, 0, 1) X
(0, 0, 2)(0, 0, 1) X
(1, 0, 0)(0, 0, 1) X
(2, 0, 0)(0, 0, 1) X
(0, 0, 2)(0, 0, 1) X
(0, 0, 1)(0, 0, 1) X
(1, 0, 1)(0, 0, 1) X
(0, 2, 2)(0, 0, 1) X
(0, 1, 1)(0, 1, 1) X
(0, 1, 2)(0, 1, 1) X
(2, 1, 0)(0, 1, 1) X
(0, 2, 2)(0, 1, 1) X
(2, 1, 2)(0, 1, 1)","Text"] <>"	
	file = \""<>FileNameJoin[{$TemporaryDirectory,"EVX13.MDL"}]<>"\"
	fcstlim = 12.0
	bcstlim = 12.0
	qlim = 0.1
	outofsample = yes
	print = pickmdlchoice
	}
";

Export[FileNameJoin[{$TemporaryDirectory,"mmax13.spc"}],outputText,"Text"];

inputfile =FileNameJoin[{$TemporaryDirectory,"mmax13"}];

Run["cd \"" <> $X13Directory <> "\" && .\\x13as", quote@inputfile];

inputfile=Import[FileNameJoin[{$TemporaryDirectory,"mmax13.out"}],{"Text"}];

DeleteFile[FileNameJoin[{$TemporaryDirectory,"mmax13.out"}]];

chosenModel=First@StringCases[inputfile,
Shortest["The model chosen is " ~~d___~~"Likelihood Statistics"]:> "The model chosen is " <> d

];


(*chosenParametersWe pause for 30 sec as it takes about this times to generate files*)
TimeConstrained[
	While[Not@FileExistsQ[FileNameJoin[{$TemporaryDirectory,"mmax13.fts"}]]],
15,
Message[x13::thirtyseconds,Import[FileNameJoin[{$TemporaryDirectory,"mmax13.err"}],"Text"]];Abort[]
];

(*Pause[30];*)

readX31output["fts"]

]



Options[SeasonalAdjustmentX13]={ARIMA->"(1 1 1)(0 1 1)",Transform->"auto",Frequency->"Month",
Output->"e2",Outliers->{},X11Regression->False};


SeasonalAdjustmentX13[x_,OptionsPattern[]]:=Module[
	{outputText,
	originalDates=x[[All,1]],
	dateList=Sort@x,dataList,
	startDate,endDate,
	spanDate, inputfile,	period
	},

(*Determine the period of the series*)

period=Max[Length/@GatherBy[DateList/@dateList[[;;,1]],First]];
dataList=dateList[[;;,2]];
Which[
	period==12,
		startDate=DateString[First@First@dateList,{"Year",".","Month"}];
		endDate=DateString[First@Last@dateList,{"Year",".","Month"}];
		spanDate=DateString[DatePlus[First@First@dateList,{3,"Year"}],{"Year",".","Month"}];,
	period==4,
		startDate=DateString[First@First@dateList,{"Year",".","Quarter"}];
		endDate=DateString[First@Last@dateList,{"Year",".","Quarter"}];
		spanDate=DateString[DatePlus[First@First@dateList,{3,"Year"}],{"Year",".","Quarter"}];,
	True,
		Message[runx13::nosuchfreq];Abort[];
];

outputText="series{
  title=\""<>"Title"<>"\"
  start="<>startDate<>"
  data=("<>Riffle[ToString[#]<>" "&/@dataList,"\n\t",13]<>")
  period="<>ToString@period<>"
}
"<>""
(*transformX13[OptionValue[Transform]]*)<>"

regression {
	variables = ("<>Quiet@Check[deleteBraces@ToString[OptionValue[Outliers]],""]<>")
}

arima {
	 model = "<>OptionValue[ARIMA]<>"
}


x11{
  print=none
  mode = add
  save=("<>Quiet@Check[deleteBraces@ToString[OptionValue[Output]],""]<>")}

";

Export[FileNameJoin[{$TemporaryDirectory,"mmax13.spc"}],outputText,"Text"];

inputfile =FileNameJoin[{$TemporaryDirectory,"mmax13"}];

Run["cd \"" <> $X13Directory <> "\" && .\\x13as", quote@inputfile];

(*We pause for 30 sec as it takes about this times to generate files*)
TimeConstrained[
	While[Not@FileExistsQ[FileNameJoin[
	{$TemporaryDirectory,If[Length@OptionValue[Output]>0,"mmax13."<>First@OptionValue[Output],"mmax13."<>OptionValue[Output]]
	}]]],
15,
Message[x13::thirtyseconds,Import[FileNameJoin[{$TemporaryDirectory,"mmax13.err"}],"Text"]];Abort[]
];

(*Pause[30];*)

inputfile=readX31output[OptionValue[Output],DatesColumn->True,X13Period->period];

Transpose[{originalDates,inputfile[[All,2]]}]


]



readX31output[output_String,OptionsPattern[]]:=Module[{imp},
imp=Import[FileNameJoin[{$TemporaryDirectory,"mmax13."<>output}],{"Data"}];
DeleteFile[FileNameJoin[{$TemporaryDirectory,"mmax13."<>output}]];

If[
	OptionValue[DatesColumn],
	imp[[3;;]]/.{x_,y__/;Length[{y}]<=10}:>{{Round[x/100,1],
		Which[
			OptionValue[X13Period]==12,x-Round[x,100],
			OptionValue[X13Period]==4,(x-Round[x,100])*3],28},y},
	imp[[3;;]]
]
];
readX31output[output_List,OptionsPattern[]]:=Module[
{},
readX31output[#,DatesColumn->True,X13Period->OptionValue[X13Period]]&/@output
];


adjustTextX13[x_,text_String,OptionsPattern[]]:=Module[
{outputText,
dateList=Sort@x,dataList,
startDate,endDate,
spanDate, inputfile,
period

},


(*Determine the period of the series*)
period=Max[Length/@GatherBy[dateList[[;;,1]],First]];
dataList=dateList[[;;,2]];
Which[
	period==12,
		startDate=DateString[First@First@dateList,{"Year",".","Month"}];
		endDate=DateString[First@Last@dateList,{"Year",".","Month"}];
		spanDate=DateString[DatePlus[First@First@dateList,{3,"Year"}],{"Year",".","Month"}];,
	period==4,
		startDate=DateString[First@First@dateList,{"Year",".","Quarter"}];
		endDate=DateString[First@Last@dateList,{"Year",".","Quarter"}];
		spanDate=DateString[DatePlus[First@First@dateList,{3,"Year"}],{"Year",".","Quarter"}];,
	True,
		Message[runx13::nosuchfreq];Abort[]
];

outputText="series{
  title=\""<>"Title"<>"\"
  start="<>startDate<>"
  data=("<>Riffle[ToString[#]<>" "&/@dataList,"\n\t",13]<>")
  period="<>ToString@period<>"
}
" <> text;

Export[FileNameJoin[{$TemporaryDirectory,"mmax13.spc"}],outputText,"Text"];

inputfile =FileNameJoin[{$TemporaryDirectory,"mmax13"}];

Run["cd \"" <> $X13Directory <> "\" && .\\x13as", quote@inputfile];

(*We pause for 30 sec as it takes about this times to generate files*)
TimeConstrained[
	While[Not@FileExistsQ[FileNameJoin[
	{$TemporaryDirectory,If[Length@OptionValue[Output]>0,"mmax13."<>First@OptionValue[Output],"mmax13."<>OptionValue[Output]]
	}]]],
15,
Message[x13::thirtyseconds,Import[FileNameJoin[{$TemporaryDirectory,"mmax13.err"}],"Text"]];Abort[]
];

(*Pause[30];*)

readX31output[OptionValue[Output],DatesColumn->True,X13Period->period]

]

End[] (* End Private Context *)

EndPackage[]
*)

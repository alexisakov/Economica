
BeginPackage["FRED`"];

(* Exported symbols added here with SymbolName::usage *) 

FRED::usage="FRED[\!\(\*
StyleBox[\"seriesID\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"startDate\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"endDate\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"options\",\nFontSlant->\"Italic\"]\)] takes a series ID, or lists of several series ID, and returns the data time series. The currently supported FRED options are those listed here: http://api.stlouisfed.org/docs/fred/series_observations.html. Options should be entered as rules: FRED option entered as a string \[Rule] option value entered as a string. Start and end dates are the date range for the data entered as Mathematica date lists.";


Unprotect[FRED];

Begin["`Private`"];
(* Implementation of the package *)


Clear[FRED,makeQueryString,stringToNumber,dateRules];

(* the next two functions I've left in a private section -- that is why they are colored differently. If you want to use them for other purposes then add usage statement above where the FRED usage statement is and also Unprotect and Protect them. *)

makeQueryString[rules_List]:=Module[{tmp},
tmp=rules/.Rule[x_String,y_String]:>StringJoin@@List[x,"=",y];
StringJoin@@(ToLowerCase/@Riffle[tmp,"&"])
];


Options[FRED]={APIKey->None};

FRED[series:(_String|_List),opts:OptionsPattern[]]:=Module[
	{today,tmp},
	today=DateString[DateList[],{"Year","-","Month","-","Day"}];

	(* collect options *)
tmp=Import["http://api.stlouisfed.org/fred/series/observations?series_id="<>ToString[series]<>"&api_key="<>OptionValue[APIKey],"XML"];

Cases[tmp,
 XMLElement[
   "observation", {___, "date" -> x_, 
    "value" -> y_}, ___] :> {DateList@x, ToExpression@y}, {1, 4}]
]

End[];
EndPackage[]




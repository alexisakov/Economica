(* Mathematica Package *)

(* 
TODO: QuandlToken - add option Overwrite->True
*)


BeginPackage["Economica`"]
(* Exported symbols added here with SymbolName::usage *)  

QuandlToken::usage="QuandlToken[s] saves Quandl token. Is called only once" 

QuandlData::usage="QuandlData[l,{d1,d2}] get's Quandl data. It uses token if QuandlToken has been saved."

Begin["`Private`"] (* Begin Private Context *) 

QuandlToken[s_String] := Module[{},
  Export[
   FileNameJoin[{$TemporaryDirectory, "QuandToken.txt"}],
   s]
  ]

Options[QuandlData]={APIKey->None};

QuandlData[code_String, {startDate_, endDate_},OptionsPattern[]] := Module[
  {text,
   token =Check["&auth_token=" <> (OptionValue[APIKey]), ""],
	data},
  text = "http://www.quandl.com/api/v1/datasets/" 
    <> code 
    <> ".xls?"
    <> "trim_start=" 
    <> DateString[startDate, {"Year", "-", "Month", "-", "Day"}]
    <> "&trim_end=" 
    <> DateString[endDate, {"Year", "-", "Month", "-", "Day"}]
    <>"&sort_order=asc"
    <> token;
  data=Rest@First@Import@text;
  data[[All,1]]=(DateList[#][[;;3]]&/@data[[All,1]]);
  data]
  
  
  QuandlData[code_String, OptionsPattern[]] := Module[
  {text,
   token =Check["&auth_token=" <> (OptionValue[APIKey]), ""],
	data},
  text = "http://www.quandl.com/api/v1/datasets/" 
    <> code 
    <> ".xls?"
    <>"&sort_order=asc"
    <> token;
  data=Rest@First@Import@text;
  data[[All,1]]=(DateList[#][[;;3]]&/@data[[All,1]]);
  data]


(*
TODO: Integrate this function properly.
TODO: Add support for token.*)

(*ClearAll@GetQuandlData;
Options[GetQuandlData]={"Collapse"->"daily","Tansformation"->"none",
	"Sort"->"asc",
	"ExcludeHeaders"->True,
	"Column"->4,
	"DataFormat"->"csv",
	"Token"->None};*)

(*GetQuandlData[ticker_,nData:({startDate_,endDate_}|rows_),opts:OptionsPattern[]]:= GetQuandlData[ticker,nData,opts]=
Module[{dataFormat,collapse,transformation,sort,url,excludeHeaders,column,token},
    {dataFormat,collapse,transformation,sort,excludeHeaders,column,token}=
        OptionValue[GetQuandlData,#]&/@{"DataFormat","Collapse","Tansformation","Sort","ExcludeHeaders","Column","Token"};

    url="http://www.quandl.com/api/v1/"~~
        If[ListQ@ticker,
            "multisets."~~
            dataFormat~~
            "?columns="~~
            StringJoin@@
                Riffle[
                    MapThread[
                        #1~~"."~~#2&
                        ,
                        {StringReplace[#,"/"->"."]&/@ticker,ToString/@If[ListQ@column,column,ConstantArray[column,Length@ticker]]}
                    ]
                    ,
                    ","
                ]
            ,
            "datasets/"~~
            ticker~~
            "."~~
            dataFormat~~
            "?"~~
            If[column===All,
                ""
                ,
                "column="~~
                ToString@column

  ]
        ]~~
        "&collapse="~~
        collapse~~
        If[ListQ@nData,
            "&trim_start="~~
            DateString[startDate,{"Year","-","Month","-","Day"}]~~
            "&trim_end="~~
            DateString[endDate,{"Year","-","Month","-","Day"}]
            ,
            "&rows="~~
            ToString@rows
        ]~~
        "&sort_order="~~
        sort~~
        "&transformation="~~
        transformation~~
        "&exclude_headers="~~
        ToString@excludeHeaders~~
        If[token===None,
            ""
            ,
            "&auth_token="~~
            token
        ];

    Import@url
];*)


(*
Usage:
GetQuandlData["YAHOO/INDEX_GSPC", {{2014, 08, 16}, {2014, 10, 16}}, "Tansformation"->"diff"]  
GetQuandlData["YAHOO/INDEX_VIX",10,"Sort"->"desc"]  
GetQuandlData[{"YAHOO/INDEX_GSPC", "YAHOO/INDEX_VIX"},10,"Sort"->"desc","Column"->{4,2}]  
GetQuandlData[{"YAHOO/INDEX_GSPC", "YAHOO/INDEX_VIX"},10,"Sort"->"desc","Column"->4]

*)


End[] (* End Private Context *)

EndPackage[]
(* Mathematica Package *)

(*
Source: http://company.yandex.ru/technologies/mystem/help.xml.
TODO: Make import time constrained
TimeConstrained[
	While[Not@FileExistsQ[FileNameJoin[{$TemporaryDirectory,"mmax13.sp0"}]]],
15,
Message[x13::thirtyseconds,Import[FileNameJoin[{$TemporaryDirectory,"mmax13.err"}],"Text"]];Abort[]
];
*)



BeginPackage["Economica`"]
(* Exported symbols added here with SymbolName::usage *)  

YandexMyStem::usage="YandexMyStem[s] produces a list of stemmed words." 
InitializeYandexMyStem::usage="YandexMyStem[s] produces a list of stemmed words." 


Begin["`Private`"] (* Begin Private Context *) 

YandexMyStem[t_] := Module[
  {result},
  (*First check whether mystem.exe exists.*)
  InitializeYandexMyStem[];
  Export[FileNameJoin[{$TemporaryDirectory, "mystem.txt"}], t, "Text",
    CharacterEncoding -> "WindowsCyrillic"];
  Run["mystem -l \"" <> 
    FileNameJoin[{$TemporaryDirectory, "mystem.txt"}] <> "\" \"" <> 
    FileNameJoin[{$TemporaryDirectory, "mystemOUT.txt"}] <> "\""];
  Pause[3];
  result = 
   Import[FileNameJoin[{$TemporaryDirectory, "mystemOUT.txt"}], 
    "Text", CharacterEncoding -> "WindowsCyrillic"];
  DeleteFile[FileNameJoin[{$TemporaryDirectory, "mystem.txt"}]];
  DeleteFile[FileNameJoin[{$TemporaryDirectory, "mystemOUT.txt"}]];
  toWordList[result]
  ];
  
  
  toWordList[l_String] := StringSplit[
   StringReplace[
    StringReplace[l,
     Shortest["{" ~~ x__ ~~ "|" ~~ __ ~~ "}"] :> x <> " "],
    {
     "{" -> " ",
     "}" -> " ",
     {"?"} -> ""
     }
    ],
   WhitespaceCharacter] /. "" :> Sequence[];
  
InitializeYandexMyStem[]:=Module[{},
	If[Not@FileExistsQ["C:\\Windows\\System32\\mystem.exe"],
		CopyFile[
			FileNameJoin[{DirectoryName[$InputFileName],"mystem.exe"}],
			"C:\\Windows\\System32\\mystem.exe"];
		]
	];

End[] (* End Private Context *)

EndPackage[]
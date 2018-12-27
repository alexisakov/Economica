Package["Economica`"]

(*
Source: https://tech.yandex.ru/mystem/
TODO: 
201712 Check if export is succesful

v2.0 Now no need to copy the mystem.exe to win directory. Problem solve using this note http://forums.wolfram.com/mathgroup/archive/2006/May/msg00457.html 
    + update the mystem distro. It is a x64 version from here https://tech.yandex.ru/mystem/
*)

YandexMyStem::usage="YandexMyStem[s] produces a list of stemmed words." 
InitializeYandexMyStem::usage="YandexMyStem[s] produces a list of stemmed words." 

QuoteString[s_]:="\""<>ToString@s<>"\"";
$YMSDirectory=DirectoryName[$InputFileName];


YandexMyStem[t_] := Module[{
  result,
  inFile = (FileNameJoin[{$TemporaryDirectory, "mystem.txt"}]),
  ouFile = (FileNameJoin[{$TemporaryDirectory, "mystemOUT.txt"}])
  },
  Export[inFile, t, "Text", CharacterEncoding -> "UTF-8"];
  Run["cd " <> (QuoteString@$YMSDirectory) <> " && .\\mystem -l " <> (QuoteString@inFile) <> " " <> (QuoteString@ouFile)];
  Pause[3];
  result = Import[ouFile, "Text", CharacterEncoding ->  "UTF-8"];
  DeleteFile[{inFile,ouFile}];
  toWordList[result]
  ];
  
  
toWordList[l_String] := StringSplit[
   StringReplace[
    StringReplace[l,
     Shortest["{" ~~ x__ ~~ "|" ~~ __ ~~ "}"] :> x <> " "],
    {     "{" -> " ", "}" -> " ", {"?"} -> ""}], WhitespaceCharacter] /. "" :> Sequence[];
  
PackageExport["YandexMyStem"]

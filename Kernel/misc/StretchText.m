(* Mathematica Package *)

BeginPackage["Economica`"]
(* Exported symbols added here with SymbolName::usage *)  

(*
Source: By Jens, http://mathematica.stackexchange.com/a/4387/128
*)

StretchText::usage = "";

(* Usage example *)
(* 
t = With[{xMax = 3, wX = .2},
   Table[
    Show[Plot[1/x, {x, 0, 5}],
     Graphics[
      stretchText["{", {xMin - wX, 1/xMax}, {wX, 1/xMin - 1/xMax}]],
     Graphics[{Red, Dashed, 
       Map[Line[{{0, 1/#}, {#, 1/#}}] &, {xMin, xMax}]}],
     PlotRange -> {0, 1.5}, Frame -> True, Axes -> False],
    {xMin, .7, 2.5, .2}]
   ];
*)


Begin["`Private`"] (* Begin Private Context *) 

StretchText[char_, pos_, scale_, angle_: 0] := 
 Module[{g, coords, xMin, xMax, yMin, yMax}, 
  g = First@First@ImportString[ExportString[char, "PDF"],
      "TextOutlines" -> True];
  coords = 
   Apply[Join, 
    Cases[g, FilledCurve[___, p_] :> Flatten[p, 1], Infinity]];
  {{xMin, xMax}, {yMin, yMax}} = 
   Map[{Min[#], Max[#]} &[#] &, Transpose[coords]];
  Rotate[
   Inset[
    Graphics[g,
     PlotRange -> {{xMin, xMax}, {yMin, yMax}},
     If[ListQ[scale],
      AspectRatio -> Full,
      AspectRatio -> Automatic
      ]
     ],
    pos,
    {xMin, yMin},
    scale],
   angle]
  ]

End[] (* End Private Context *)
EndPackage[]
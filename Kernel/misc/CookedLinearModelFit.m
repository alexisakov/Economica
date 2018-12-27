(* ::Package:: *)

(* Mathematica Package *)

BeginPackage["Economica`"]
(* Exported symbols added here with SymbolName::usage *)  

CookedLinearModelFit::usage="Simple robust linear regression";

Begin["`Private`"] (* Begin Private Context *) 
(**)

CookedLinearModelFit[l_List] := Module[{x, vars, cooks, clean},
  vars = Table[Subscript[x, i], {i, Length[First[l]]-1}];
  cooks = LinearModelFit[l, vars, vars]["CookDistances"];
  clean = Extract[l, Position[cooks, x_ /; x < 4/Length[l]]];
  LinearModelFit[clean, vars, vars]
  ]


End[] (* End Private Context *)

EndPackage[]

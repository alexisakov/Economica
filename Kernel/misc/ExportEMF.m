(* ::Package:: *)

(* Wolfram Language package *)
BeginPackage["Economica`"]
(* Exported symbols added here with SymbolName::usage *)  

ExportEMF::usage="ExportEMF";
ExportEPS::usage="ExportEPS";


Begin["`Private`"] (* Begin Private Context *) 


Options[ExportEMF] = Join[Options[Export],{BarChartQ->False}];
Options[ExportEPS] = Options[Export];



(*The Verbeia solution: http://mathematica.stackexchange.com/a/65551/128
	The resulting charts are ugly though*)
(*ExportEMF[pathorfile_String, obj_, opts : OptionsPattern[{Export}]] := With[
{stringtouse = If[ToLowerCase[StringTake[pathorfile,-4]]===".emf", pathorfile, pathorfile<>".emf"]},
	Export[stringtouse, 
	DeleteCases[ obj /. {_Opacity, p_Point} :> {PointSize[0], p}, _Opacity, Infinity], opts] ]
*)


With[{opt = First /@ Options[Export]},
	ExportEMF[filename_String, expr_, OptionsPattern[]] := Module[{
	pic = First@ImportString[ExportString[expr, "PDF"]]},
	If[OptionValue[BarChartQ],
    Export[filename <> ".emf",expr/.{Opacity[0],__}->{}],
    Export[filename <> ".emf", pic, ImageResolution -> 4100, Sequence @@ ((# -> OptionValue[#]) & /@ opt)
    	]
    ]
  ]
  ];
  
With[{opt = First /@ Options[Export]},
	ExportEPS[filename_String, expr_, OptionsPattern[]] := Module[{
    pic = First@ImportString[ExportString[expr, "PDF"]]},
    Export[filename <> ".eps", pic, 
    	Sequence @@ ((# -> OptionValue[#]) & /@ opt),
    	ImageResolution -> 4100
    	]
    ]
  ];
  


End[] (* End Private Context *)

EndPackage[]

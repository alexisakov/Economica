(* Wolfram Language package *)
BeginPackage["Economica`"]
(* Exported symbols added here with SymbolName::usage *)  

ExportEMF::usage="ExportEMF";

Begin["`Private`"] (* Begin Private Context *) 


Options[ExportEMF] = Options[Export];


With[{opt = First /@ Options[Export]},
	ExportEMF[filename_String, expr_, OptionsPattern[]] := Module[{
    pic = First@ImportString[ExportString[expr, "PDF"]]},
    Export[filename <> ".emf", pic, 
    	Sequence @@ ((# -> OptionValue[#]) & /@ opt),
    	ImageResolution -> 4100
    	]
    ]
  ];


End[] (* End Private Context *)

EndPackage[]

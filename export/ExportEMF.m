Package["Economica`"]
(* Exported symbols added here with SymbolName::usage *)  

ExportEMF::usage="ExportEMF";
ExportEPS::usage="ExportEPS";

Options[ExportEMF] = Join[Options[Export],{BarChartQ->False, ImageResolution -> 4100}];
Options[ExportEPS] = Options[Export];

(*The Verbeia solution: http://mathematica.stackexchange.com/a/65551/128 The resulting charts are ugly though*)
(*ExportEMF[pathorfile_String, obj_, opts : OptionsPattern[{Export}]] := With[
 {stringtouse = If[ToLowerCase[StringTake[pathorfile,-4]]===".emf", pathorfile, pathorfile<>".emf"]},
	Export[stringtouse, DeleteCases[ obj /. {_Opacity, p_Point} :> {PointSize[0], p}, _Opacity, Infinity], opts] ]*)


InkscapeConvert[path_]:=Module[{
	inkpath="\"C:\\Program Files\\Inkscape\\inkscape.exe\"",
	command
	},
		Run["\"" <> inkpath <> " -f=\""<>path<>".pdf\"" <> " --export-emf=\""<>path<>".emf\"\""]
			(*DeleteFile[path <> ".pdf"]*)

]


With[{opt = First /@ Options[Export]},
	ExportEMF[filename_String, expr_, OptionsPattern[]] := Module[{
		pic = First@ImportString[ExportString[expr, "PDF"]],
		picO = expr /. _Opacity -> (## &[])
		},
	
		Switch[OptionValue[BarChartQ],
    		True,
	    		(*if this is a bar chart then we attempt to use a W10 installation to export it in the background*)  
	        	pic,
        	False,
      			Export[filename <> ".emf", pic, ImageResolution -> OptionValue[ImageResolution], Sequence @@ ((# -> OptionValue[#]) & /@ opt)],
	        "OpacityTrick",
      			Export[filename <> ".emf", picO, ImageResolution -> OptionValue[ImageResolution], Sequence @@ ((# -> OptionValue[#]) & /@ opt)],
		    "Inkscape",
      			Export[filename <> ".pdf", expr];
      			InkscapeConvert[filename]   			

    ]
  ]
  ];
  
With[{opt = First /@ Options[Export]},
	ExportEPS[filename_String, expr_, OptionsPattern[]] := Module[{
    pic = First@ImportString[ExportString[expr, "PDF"]]},
    Export[filename <> ".eps", pic, 
    	Sequence @@ ((# -> OptionValue[#]) & /@ opt),
    	ImageResolution -> 8000
    	]
    ]
  ];
  
PackageExport["ExportEMF"]
PackageExport["ExportEPS"]
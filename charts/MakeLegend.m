Package["Economica`"]
 
ClearAll@MakeLegend;
Options[MakeLegend]={BaseStyle -> Directive[FontFamily -> "Arial", FontSize -> 22/1.5],FontSize->22/1.5};

MakeLegend[list_,OptionsPattern[]] := Module[
  {icons, text, legend},
  icons = MapIndexed[
	Graphics[{PointSize[Large], Thickness[0.5],
		If[Length@#1 == 1, ColorDataVTBCapital2018@First@#2, Last@#1],
		Which[
			First@#1 == "Box", Rectangle[],
			First@#1 == "Line", Line[{{0, 0.5}, {1, 0.5}}],
			First@#1 == "Dot", Point[{0, 0}]]},
		ImageSize -> 16] &,
	list[[All, 2 ;;]]];

  text = VTBTextStyle[#,FontSize->OptionValue[FontSize]]& /@ list[[All, 1]];
  
  legend = {icons, text}\[Transpose];
  Grid[legend, Alignment -> {{Right, Left}, Center}, 
   BaseStyle -> OptionValue[BaseStyle], 
   Spacings -> {{0, 0.5, 2}, 0}]
]

PackageExport["MakeLegend"]
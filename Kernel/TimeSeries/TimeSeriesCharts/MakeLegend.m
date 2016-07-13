(* ::Package:: *)

(*  box1 = Graphics[{VTBBlue, Rectangle[]}, ImageSize -> 16];
 box2 = Graphics[{VTBViolet, Rectangle[]}, ImageSize -> 16];
text1 = VTBTextStyle@voc["investment, pp"];
text2 = VTBTextStyle@voc["consumption, pp"];
line1 = Graphics[{VTBBlue, Line[{{0, 0.5}, {1, 0.5}}]},
   ImageSize -> {16, 16}];
text3 = VTBTextStyle@voc["domestic demand, % YoY"];
legend = Grid[
  {{box1, text1}, {box2, text2}, {line1, text3}}, 
  Alignment -> {{Right, Left}, Center}, 
  BaseStyle -> Directive[FontFamily -> "Arial"], 
  Spacings -> {{0, 0.5, 2}, 0}] 
  
  
  
  
  
  Export[FileNameJoin[{exportDirectory, "pic.pdf"}], %]*)
 
ClearAll@MakeLegend;
Options[MakeLegend]={BaseStyle -> Directive[FontFamily -> "Arial", FontSize -> 22/1.5],FontSize->22/1.5};

MakeLegend[list_,OptionsPattern[]] := Module[
  {icons, text, legend},
  icons = MapIndexed[
	Graphics[{PointSize[Large], Thickness[0.1],
		If[Length@#1 == 1, ColorData["Rainbow", First@#2], Last@#1],
		Which[
			First@#1 == "Box", Rectangle[],
			First@#1 == "Line", Line[{{0, 0.5}, {1, 0.5}}],
			First@#1 == "Dot", Point[{0, 0}]]},
		ImageSize -> 24] &,
	list[[All, 2 ;;]]];

  text = VTBTextStyle[#,FontSize->OptionValue[FontSize]]& /@ list[[All, 1]];
  
  legend = {icons, text}\[Transpose];
  Grid[legend, Alignment -> {{Right, Left}, Center}, 
   BaseStyle -> OptionValue[BaseStyle], 
   Spacings -> {{0, 0.5, 2}, 0}]
]
  

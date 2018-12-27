(* ::Package:: *)

(* Wolfram Language package *)

(* Mathematica Package *)
BeginPackage["Economica`"]
(* Exported symbols added here with SymbolName::usage *)  

(**Source: http://mathgis.blogspot.ru/2008/12/how-to-make-tenary-plot.html
For density plot the SE answer is here: http://mathematica.stackexchange.com/questions/39733/how-to-plot-ternary-density-plots
**)


(*TODO: add the plot range option*)


TernaryPlot::usage="TernaryPlot[dat_, regnum_, nLab_, nms_List, OptionsPattern[]] "

Begin["`Private`"] (* Begin Private Context *) 

ClearAll[TernaryPlot];

Options[TernaryPlot] = {
	ImageSize -> {24, 18}/1.5 cm, 
  	AspectRatio -> 3/4,
  	FrameTicks->True,
	PlotRange->All};

unitize[x_] := x/Total[x];
coords[{x1_, x2_, x3_}] := {x1/2 + x2, x1 Tan[Pi/3]/2};
makeDisk[{x_, y_}] := {RGBColor[Sequence @@ x], Disk[coords[x], y]};


TernaryPlot[dat_, regnum_, nLab_, nms_List, OptionsPattern[]] := Module[
	{rltx,bmtx,gltx,r,
		
		(**The 'axis' of the chart**)
		rl =  
			Table[
				{Dotted, RGBColor[i/255, 0, 0], Line[{{0 + i/255 Sqrt[3]/2 Tan[30 Degree],  i/255 Sqrt[3]/2}, {1 - i/255 Sqrt[3]/2 Tan[30 Degree], i/255 Sqrt[3]/2}}]},
				{i, 256*0.1, 256*0.9, 256*0.1}],
		bl = (Table[
				{Dotted, RGBColor[0, 0, i/255],Rotate[Line[{{0 + i/255 Sqrt[3]/2 Tan[30 Degree],i/255 Sqrt[3]/2}, {1 - i/255 Sqrt[3]/2 Tan[30 Degree],i/255 Sqrt[3]/2}}], 120 Degree, {0.5, Sqrt[3]/6}]},
				{i, 256*0.1, 256*0.9, 256*0.1}]),
		gl = Table[
				{Dotted, RGBColor[0, i/255, 0],Rotate[Line[{{0 + i/255 Sqrt[3]/2 Tan[30 Degree],i/255 Sqrt[3]/2}, {1 - i/255 Sqrt[3]/2 Tan[30 Degree],i/255 Sqrt[3]/2}}], -120 Degree, {0.5, Sqrt[3]/6}]},
				{i, 256*0.1, 256*0.9, 256*0.1}],
		tg = Graphics[{
			Text[Style[nms[[3]], Blue, FontFamily -> "Arial", FontSize -> 22/1.5, LineSpacing -> {0.1, 10}], {-0.02, -0.02}],     
			Text[Style[nms[[2]], Green, FontFamily -> "Arial",FontSize -> 22/1.5,LineSpacing -> {0.1, 10}], {1 + 0.02, -0.02}],
			Text[Style[nms[[1]], Red, FontFamily -> "Arial", FontSize -> 22/1.5,LineSpacing -> {0.1, 10}], {0.5, Sqrt[3]/2 + 0.02}]}, 
			ImageSize -> 600],
		cl = Graphics[{Thin, Line[{{0, 0}, {1, 0}, {0.5, Sqrt[3]/2}, {0, 0}}]},  ImageSize -> 600],
		
		(**The data points**)
		urd = unitize /@ (dat),
		sizes = unitize[(Total /@ (dat))/Total[Flatten[dat]]]*0.3,
		data, regNumbers, strikeOuts,
		plotRange},
	
	plotRange=If[OptionValue[PlotRange]==All,ConstantArray[{0,1},3],OptionValue[PlotRange]];
	
	
	If[OptionValue[PlotRange]!= All,
		urd=urd/.{{x_,y_,z_}/;Nand[
			IntervalMemberQ[Interval[OptionValue[PlotRange][[1]]], x],
			IntervalMemberQ[Interval[OptionValue[PlotRange][[2]]], y],
			IntervalMemberQ[Interval[OptionValue[PlotRange][[3]]], z]
			]:>Sequence[]};
		urd[[All,1]]=Rescale[urd[[All,1]], OptionValue[PlotRange][[1]]];
		urd[[All,2]]=Rescale[urd[[All,2]], OptionValue[PlotRange][[2]]];
		urd[[All,3]]=Rescale[urd[[All,3]], OptionValue[PlotRange][[3]]];
		urd = unitize /@ (urd);
		];
	
	data = Graphics[Flatten[makeDisk /@ (Transpose@{urd, sizes})], ImageSize -> 600];
	
	(**Callouts**)
    regNumbers = Graphics[
    Thread[
     Text[VTBTextStyle/@regnum[[nLab]], # + {.25 + (Random[] - .5)/10, .1} & /@ (coords /@ 
          urd)[[nLab]]]]];
		  strikeOuts = Graphics[{Dashed, Gray, Line[{#, # + {.1, .1}, # + {.2, .1}}] & /@ (coords /@ urd)[[nLab]]}];
  
	(**The frame ticks**)    
    r = RotationTransform[-120 Degree];
	rltx = Graphics[Table[{
		RGBColor[(255 - i)/255, 0, 0],	
		Text[VTBTextStyleNoColor@ToString[Rescale[Ceiling[100 - 100*i/256],{0,100},100*plotRange[[1]]]],
			r@{0 + i/255 Sqrt[3]/2 Tan[30 Degree], i/255 Sqrt[3]/2} + {0.525, Sqrt[3.]/2}]},
		{i, 256*0.2, 256*0.9, 256*0.2}],
		ImageSize -> 600];
	bmtx = Graphics[Table[{
      RGBColor[0, 0, (255 - i)/255],
      Text[VTBTextStyleNoColor@ToString[Ceiling[100 - 100*i/256]],{-0.025+i/255 Sqrt[3]/2 Tan[30 Degree], i/255 Sqrt[3]/2}]},
      	{i, 256*0.2, 256*0.9, 256*0.2}],
      	ImageSize -> 600];
	r = RotationTransform[120 Degree];
	gltx = Graphics[Table[{RGBColor[0, (255 - i)/255, 0],
		Text[VTBTextStyleNoColor@ToString[Ceiling[100 - 100*i/256]], 
			r@{0 + i/255 Sqrt[3]/2 Tan[30 Degree], 
				i/255 Sqrt[3]/2} + {1, -0.025}]},
				{i, 256*0.2, 256*0.9, 256*0.2}],
				ImageSize -> 600];
     
     
  
	Show[
		Graphics[rl, ImageSize -> 600],
		Graphics[bl, ImageSize -> 600],
		Graphics[gl, ImageSize -> 600],
		tg, cl, strikeOuts, data, regNumbers, 
		If[OptionValue[FrameTicks],{rltx,bmtx,gltx},{}],
		ImageSize -> OptionValue[ImageSize], 
		AspectRatio -> OptionValue[AspectRatio]]];
    	
     



End[] (* End Private Context *)
EndPackage[]




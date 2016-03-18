(* Wolfram Language package *)

(* Mathematica Package *)
BeginPackage["Economica`"]
(* Exported symbols added here with SymbolName::usage *)  

TernaryPlot::usage="TernaryPlot[dat_, regnum_, nLab_, nms_List, OptionsPattern[]] is used to BoE fan charts."

Begin["`Private`"] (* Begin Private Context *) 

ClearAll[TernaryPlot];

Options[TernaryPlot] = {
	ImageSize -> {24, 18}/1.5 cm, 
  	AspectRatio -> 3/4,
  	FrameTicks->True};

unitize[x_] := x/Total[x];
coords[{x1_, x2_, x3_}] := {x1/2 + x2, x1 Tan[Pi/3]/2};
makeDisk[{x_, y_}] := {RGBColor[Sequence @@ x], Disk[coords[x], y]};


TernaryPlot[dat_, regnum_, nLab_, nms_List, OptionsPattern[]] := Module[
  {rltx,bmtx,gltx,r,
   rl = Graphics[
     Table[
      {Dotted, RGBColor[i/255, 0, 0], Line[
        {{0 + i/255 Sqrt[3]/2 Tan[30 Degree], 
          i/255 Sqrt[3]/2}, {1 - i/255 Sqrt[3]/2 Tan[30 Degree], 
          i/255 Sqrt[3]/2}}]},
      {i, 256*0.1, 256*0.9, 256*0.1}], ImageSize -> 600],
   bl = Graphics[
     Table[
      {Dotted, RGBColor[0, 0, i/255],
       Rotate[Line[
         {{0 + i/255 Sqrt[3]/2 Tan[30 Degree], 
           i/255 Sqrt[3]/2}, {1 - i/255 Sqrt[3]/2 Tan[30 Degree], 
           i/255 Sqrt[3]/2}}], 120 Degree, {0.5, Sqrt[3]/6}]},
      {i, 256*0.1, 256*0.9, 256*0.1}], ImageSize -> 600],
   gl = Graphics[
     Table[
      {Dotted, RGBColor[0, i/255, 0],
       Rotate[Line[
         {{0 + i/255 Sqrt[3]/2 Tan[30 Degree], 
           i/255 Sqrt[3]/2}, {1 - i/255 Sqrt[3]/2 Tan[30 Degree], 
           i/255 Sqrt[3]/2}}], -120 Degree, {0.5, Sqrt[3]/6}]},
      {i, 256*0.1, 256*0.9, 256*0.1}], ImageSize -> 600],
   tg = Graphics[
     {Text[
       Style[nms[[3]], Blue, FontFamily -> "Arial", 
        FontSize -> 22/1.5, LineSpacing -> {0.1, 10}], {-0.02, -0.02}],
      Text[
       Style[nms[[2]], Green, FontFamily -> "Arial", 
        FontSize -> 22/1.5, 
        LineSpacing -> {0.1, 10}], {1 + 0.02, -0.02}],
      Text[
       Style[nms[[1]], Red, FontFamily -> "Arial", FontSize -> 22/1.5,
         LineSpacing -> {0.1, 10}], {0.5, Sqrt[3]/2 + 0.02}]}, 
     ImageSize -> 600],
   cl = Graphics[{Thick, 
      Line[{{0, 0}, {1, 0}, {0.5, Sqrt[3]/2}, {0, 0}}]}, 
     ImageSize -> 600],
   urd = unitize /@ (dat),
   sizes = unitize[(Total /@ (dat))/Total[Flatten[dat]]]*0.3,
   data, regNumbers, strikeOuts},
  data = Graphics[Flatten[makeDisk /@ ({urd, sizes}\[Transpose])], 
    ImageSize -> 600];
  (*We would like to add comments that would show what is the \
registration number of the credit orginasation*)
  
  regNumbers = Graphics[
    Thread[
     Text[VTBTextStyle/@regnum[[nLab]], # + {.25 + (Random[] - .5)/10, .1} & /@ (coords /@ 
          urd)[[nLab]]]]];
  strikeOuts = 
   Graphics[{Dashed, Gray, 
     Line[{#, # + {.1, .1}, # + {.2, .1}}] & /@ (coords /@ urd)[[nLab]]}];
 (*The frame ticks*)    
     
	r = RotationTransform[-120 Degree];
	rltx = Graphics[Table[{RGBColor[(255 - i)/255, 0, 0],
		Text[VTBTextStyleNoColor@ToString[Ceiling[100 - 100*i/256]], 
			r@{0 + i/255 Sqrt[3]/2 Tan[30 Degree], i/255 Sqrt[3]/2} + {0.525, Sqrt[3.]/2}]},
			{i, 256*0.2, 256*0.9, 256*0.2}],
			ImageSize -> 600];
	r = RotationTransform[120 Degree];
	bmtx = Graphics[Table[{
      RGBColor[0, 0, (255 - i)/255],
      Text[VTBTextStyleNoColor@ToString[Ceiling[100 - 100*i/256]],
      	{-0.025+i/255 Sqrt[3]/2 Tan[30 Degree], i/255 Sqrt[3]/2}]},
      	{i, 256*0.2, 256*0.9, 256*0.2}],
      	ImageSize -> 600];
	r = RotationTransform[120 Degree];
	gltx = Graphics[Table[{RGBColor[0, (255 - i)/255, 0],
		Text[VTBTextStyleNoColor@ToString[Ceiling[100 - 100*i/256]], 
			r@{0 + i/255 Sqrt[3]/2 Tan[30 Degree], 
				i/255 Sqrt[3]/2} + {1, -0.025}]},
				{i, 256*0.2, 256*0.9, 256*0.2}],
				ImageSize -> 600];
     
     
  
  Show[rl, bl, gl, tg, cl, strikeOuts, data, regNumbers, 
  	If[OptionValue[FrameTicks],{rltx,bmtx,gltx},{}],
   ImageSize -> OptionValue[ImageSize], 
   AspectRatio -> OptionValue[AspectRatio]]];
    	
     



End[] (* End Private Context *)
EndPackage[]
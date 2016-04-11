(* Mathematica Package *)

BeginPackage["Economica`"]
(* Exported symbols added here with SymbolName::usage *)  



(*VTBBlue::usage="zeros[x1,x2,...] ";
VTPurple::usage="";*)

VTBViolet = RGBColor[0.86328125`, 0.6484375`, 0.796875`];
VTBBlue = RGBColor[0.15625`, 0.43359375`, 0.69140625`];
VTBLightBlue = RGBColor[0.39453125`, 0.66015625`, 0.99609375`];
VTBTextStyle[x_] := Style[x, FontFamily -> "Arial", FontSize -> 22/1.5, Black, LineSpacing -> {0.1, 10}];
VTBTextStyleNoColor[x_] := Style[x, FontFamily -> "Arial", FontSize -> 22/1.5,  LineSpacing -> {0.1, 10}];


{{0.0392156862745098, 0.1607843137254902, 0.45098039215686275`}, {1., 1., 1.}, {0.4549019607843137, 0.1450980392156863, 0.34901960784313724`}, {0.4549019607843137, 0.1450980392156863, 0.34901960784313724`}}

Unprotect[ColorData];
ColorData["Rainbow"] = 
  Function[x, 
   Blend[
   	Transpose[
   		{Range[12], 
   			{VTBViolet, 
   			 VTBBlue, 
   			 RGBColor[{0.6588235294117647, 0.6588235294117647,0.6588235294117647}], 
   			 RGBColor[{0.4549019607843137,  0.1450980392156863, 0.34901960784313724`}],
   			 RGBColor[{0.0392156862745098, 0.1607843137254902, 0.45098039215686275`}], 
   			 RGBColor[{0.396078431372549, 0.6627450980392157, 1.}], 
   			 RGBColor[{0.6509803921568628, 0.24705882352941178`, 0.5254901960784314}],   			
   			 VTBLightBlue, Gray, Darker@Gray,
   			 VTBBlue, VTBViolet}}],
     x]];
Protect[ColorData]



cm = 72/2.54;


SetOptions[BarChart, ImageSize -> {24, 18}/1.5 cm, AspectRatio -> 3/4,
  ChartStyle ->(ColorData["Rainbow"]/@Range[10]), ChartLayout->"Stacked",
  TicksStyle -> Directive[22/1.5, FontName -> "Arial" , Black, Opacity[0.3], FontOpacity -> 1]];
  
SetOptions[DateListPlot, ImageSize -> {24, 18}/1.5 cm, AspectRatio -> 3/4,
  Axes -> True, Frame -> False, PlotStyle ->(ColorData["Rainbow"]/@Range[12]), 
  TicksStyle -> Directive[22/1.5, FontName -> "Arial" , Black],
  FrameTicksStyle -> Directive[22/1.5, FontName -> "Arial" , Black]];
SetOptions[ListPlot, ImageSize -> {24, 18}/1.5 cm, AspectRatio -> 3/4, 
  TicksStyle -> Directive[22/1.5, FontName -> "Arial" , Black], PlotStyle ->(ColorData["Rainbow"]/@Range[10])];
SetOptions[Plot, ImageSize -> {24, 18}/1.5 cm, AspectRatio -> 3/4, 
  TicksStyle -> Directive[22/1.5, FontName -> "Arial" , Black], PlotStyle -> (ColorData["Rainbow"]/@Range[10])];
SetOptions[TimelinePlot, ImageSize -> {24, 18}/1.5 cm, AspectRatio -> 3/4, 
  TicksStyle -> Directive[22/1.5, FontName -> "Arial" , Black], PlotStyle -> (ColorData["Rainbow"]/@Range[10]),
  FrameTicksStyle -> Directive[22/1.5, FontName -> "Arial" , Black]];
SetOptions[PieChart, ImageSize -> {24, 18}/1.5 cm, AspectRatio -> 3/4,
  ChartStyle ->(ColorData["Rainbow"]/@Range[10]), ChartLayout->"Stacked",
  TicksStyle -> Directive[22/1.5, FontName -> "Arial" , Black]];
SetOptions[BoxWhiskerChart, ImageSize -> {24, 18}/1.5 cm, AspectRatio -> 3/4,
  ChartStyle ->(ColorData["Rainbow"]/@Range[10]), 
  TicksStyle -> Directive[22/1.5, FontName -> "Arial" , Black, Opacity[0.3], FontOpacity -> 1],
   FrameTicksStyle -> Directive[22/1.5, FontName -> "Arial" , Black]];

SetOptions[BubbleChart, ImageSize -> {24, 18}/1.5 cm, AspectRatio -> 3/4,
  ChartStyle ->(ColorData["Rainbow"]/@Range[10]), 
  TicksStyle -> Directive[22/1.5, FontName -> "Arial" , Black, Opacity[0.3], FontOpacity -> 1],
   FrameTicksStyle -> Directive[22/1.5, FontName -> "Arial" , Black]];

   
   
   


Begin["`Private`"] (* Begin Private Context *) 





End[] (* End Private Context *)

EndPackage[]
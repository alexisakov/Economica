(* Mathematica Package *)

(*
Source: http://mathematica.stackexchange.com/questions/2334/how-to-create-word-clouds
*)



BeginPackage["Economica`"]
(* Exported symbols added here with SymbolName::usage *)  

WordCloudPlot::usage="WordCloudPlot[s] plots a word cloud" 

Begin["`Private`"] (* Begin Private Context *) 

Options[WordCloudPlot] = {FontFamily -> "Garamond", 
  FontWeight -> Bold, FontColor -> Automatic}
WordCloudPlot[l_List, OptionsPattern[]] := 
  Module[{tally, range, words, wordsimg, topten},
   tally = Tally@l;
   topten = Quantile[tally[[All, -1]], .9];
   tally = Reverse@SortBy[tally, Last];
   range = {Min@(Last /@ tally), Max@(Last /@ tally)};
   words = 
    Style[First@#, FontFamily -> OptionValue@FontFamily, 
       FontWeight -> OptionValue@FontWeight, 
       FontColor -> 
        If[OptionValue@FontColor == Automatic, 
         Hue[RandomReal[], RandomReal[{.5, 1}], RandomReal[{.5, 1}]],
         If[Last@# > topten, OptionValue@FontColor, LightGray]],
       FontSize -> Last@Rescale[#, range, {12, 70}]] & /@ tally;
   wordsimg = ImageCrop[Image[Graphics[Text[#]]]] & /@ words;
   Fold[iteration, wordsimg[[1]], Rest[wordsimg]]];


iteration[img1_, w_, fun_: (Norm[#1 - #2] &)] := 
  Module[{imdil, centre, diff, dimw, padding, padded1, minpos}, 
   dimw = ImageDimensions[w];
   padded1 = ImagePad[img1, {dimw[[1]] {1, 1}, dimw[[2]] {1, 1}}, 1];
   imdil = 
    MaxFilter[Binarize[ColorNegate[padded1], 0.01], 
     Reverse@Floor[dimw/2 + 2]];
   centre = ImageDimensions[padded1]/2;
   minpos = 
    Reverse@Nearest[Position[Reverse[ImageData[imdil]], 0], 
       Reverse[centre], DistanceFunction -> fun][[1]];
   diff = ImageDimensions[imdil] - dimw;
   padding[pos_] := Transpose[{#, diff - #} &@Round[pos - dimw/2]];
   ImagePad[#, (-Min[#] {1, 1}) & /@ BorderDimensions[#]] &@
    ImageMultiply[padded1, ImagePad[w, padding[minpos], 1]]];


End[] (* End Private Context *)

EndPackage[]
Package["Economica`"]

(*Source: By Szabolcs, http://mathematica.stackexchange.com/a/42662/128*)

BeeswarmPlot::usage = "";

(* Usage example *)
(* 
data1 = RandomVariate[NormalDistribution[1.5, 1], 1000];
data2 = RandomVariate[NormalDistribution[2, 1], 200]; 

BeeswarmPlot[data2]
BeeswarmPlot[{data1, data2}, 0.07] // Rotate[#, 90 Degree] &

Graphics[{First@BeeswarmPlot[data1, 0.05],  Translate[First@BeeswarmPlot[data2, 0.05], {3, 0}]}, Frame -> True, FrameTicks -> {{Automatic, Automatic}, 
	{{{0, "exponential"}, {3, "normal"}}, None}}, GridLines -> {{0, 3}, None}]
*)

intervalInverse[Interval[]] := Interval[{-Infinity, Infinity}]
intervalInverse[Interval[int__]] :=
 Interval @@ Partition[
   Replace[Flatten[{int}],
    {{-Infinity, mid___, Infinity} :> {mid},
     {-Infinity, mid__} :> {mid, Infinity},
     {mid__, Infinity} :> {-Infinity, mid},
     {mid___} :> {-Infinity, mid, Infinity}
     }
    ], 2]

intervalComplement[a_Interval, b__Interval] := 
 IntervalIntersection[a, intervalInverse@IntervalUnion[b]]



(* data is assumed to be a sorted vector of numbers *)
beeswarm[data_, radius_] :=
 Module[{points, left, right, int},
  points = {};
  Do[
   int = Interval @@ Cases[points, {x_, y_} /; y > pt - radius :> x + {-1, 1} Sqrt[radius^2 - (pt - y)^2]];
   right = Min[intervalComplement[Interval[{0,  Infinity}], int]];
   left =  Max[intervalComplement[Interval[{-Infinity, 0}], int]];
   AppendTo[points, {If[right < -left, right, left], pt}],
   {pt, data}
  ];
  points
 ]



Options[BeeswarmPlot] =  Join[ Options[Graphics],   {PlotStyle -> Automatic} ];

SetOptions[BeeswarmPlot, Frame -> True];
SetOptions[BeeswarmPlot, FrameTicks -> {None, Automatic}];

BeeswarmPlot[data_?(VectorQ[#, NumericQ] &), radius : (_?NumericQ | Automatic) : Automatic, opt : OptionsPattern[]] := BeeswarmPlot[{data}, radius, opt]
BeeswarmPlot[data : {__?(VectorQ[#, NumericQ] &)}, radius : (_?NumericQ | Automatic) : Automatic, opt : OptionsPattern[]] :=  Module[
	{r, order, flatData, colours, colfun},

  (* generate colour indices and sort them together with the data *)
  flatData = Flatten[data];
  order = Ordering[flatData];
  colours = Flatten@Table[ConstantArray[i, Length[data[[i]]]], {i, Length[data]}];
  flatData = flatData[[order]];
  colours = colours[[order]];

  (* automatic radius selection *)
  r = If[radius === Automatic, 4 Mean@Differences[flatData], 2 radius];

  (* handle the PlotStyle option *)
  colfun = With[
    {ps = OptionValue[PlotStyle]},
    Switch[ps,
      Automatic, ColorData[1],
      _List, Function[i, ps[[ Mod[i, Length[ps], 1] ]] ],
      _, ps &
    ]
  ];

  (* call the packing function and build the graphics using the result *)
  Graphics[
   MapThread[{colfun[#2], Disk[#1, 0.95 r/2]} &, {beeswarm[flatData, r], colours}],
   Sequence @@ FilterRules[{opt}, Options[Graphics]],
   Frame -> OptionValue[Frame],
   FrameTicks -> OptionValue[FrameTicks]
  ]
 ]

PackageExport["BeeswarmPlot"]
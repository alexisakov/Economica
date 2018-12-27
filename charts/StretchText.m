Package["Economica`"]

StretchText::usage = "";

StretchText[char_, pos_, scale_, angle_: 0] := 
 Module[{g, coords, xMin, xMax, yMin, yMax}, 
  g = First@First@ImportString[ExportString[char, "PDF"],
      "TextOutlines" -> True];
  coords = 
   Apply[Join, 
    Cases[g, FilledCurve[___, p_] :> Flatten[p, 1], Infinity]];
  {{xMin, xMax}, {yMin, yMax}} = 
   Map[{Min[#], Max[#]} &[#] &, Transpose[coords]];
  Rotate[
   Inset[
    Graphics[g,
     PlotRange -> {{xMin, xMax}, {yMin, yMax}},
     If[ListQ[scale],
      AspectRatio -> Full,
      AspectRatio -> Automatic
      ]
     ],
    pos,
    {xMin, yMin},
    scale],
   angle]
  ]

PackageExport["StretchText"]
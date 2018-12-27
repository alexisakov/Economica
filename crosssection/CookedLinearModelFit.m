Package["Economica`"]

CookedLinearModelFit::usage="Simple robust linear regression";

CookedLinearModelFit[l_List] := Module[{x, vars, cooks, clean},
  vars = Table[Subscript[x, i], {i, Length[First[l]]-1}];
  cooks = LinearModelFit[l, vars, vars]["CookDistances"];
  clean = Extract[l, Position[cooks, x_ /; x < 4/Length[l]]];
  LinearModelFit[clean, vars, vars]
  ]

PackageExport["CookedLinearModelFit"]
Package["Economica`"]

ExportDynare::usage="ExportDynare";

ExportTimeSeries::usage="ExportTimeSeries";

ExportTimeSeriesIRIS::usage="Prepare data for IRIS";

(*TODO: Write a function that exports in IRIS format.*)

Options[ExportTimeSeriesIRIS] = {DateFormat->{"Year","Q","Quarter"}};

ExportDynare[filename_String, x_Association] := Module[
  {keys = Keys@x, values = Values@x,
   result},
  result = MapThread[
    #1 <> "=" <> 
      StringReplace[ToString@#2, {"{" -> "[", "}" -> "]"}] <> 
      ";\n" <> #1 <> "=" <> #1 <> "';\n" &,
    {keys, values}];
  result = StringJoin[result];
  Export[filename, result, "Text"]
  ];


ExportTimeSeries[filename_String, x_Association] := Module[{
    keys = Keys@x,
    values = Transpose[(#["Values"] & /@ (Values@x))],
    dates, result},
   dates = DateList[#][[;; 3]] & /@ (x[First@keys]["Dates"]);
   
   result = 
    MapThread[Insert, {values, dates, Table[1, {Length[dates]}]}];
   keys = Join[{""}, keys];
   result = Join[{keys}, result];
   Export[filename, result]];

(*Only quarterly data for starters.*)
ExportTimeSeriesIRIS[filename_String, x_Association, OptionsPattern[]] := Module[{
    keys = Keys@x,
    values = Transpose[(#["Values"] & /@ (Values@x))],
    dates, result},
	dates = DateString[DateList[#][[;; 3]], OptionValue[DateFormat]] & /@ (x[First@keys]["Dates"]);
   
   result = MapThread[Insert, {values, dates, Table[1, {Length[dates]}]}];
   keys = Join[{""}, keys];
   result = Join[{keys}, result];
   Export[filename, result]];
  

PackageExport["ExportDynare"]
PackageExport["ExportTimeSeries"]
PackageExport["ExportTimeSeriesIRIS"]
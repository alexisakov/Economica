(* Mathematica Init File *)

<< JLink`;
InstallJava[];
ReinstallJava[JVMArguments -> "-Xmx4024m"];

Get["MiscInit`",Path -> {FileNameJoin[{DirectoryName[$InputFileName],"misc"}]}];

Get["RussianData`",Path -> {FileNameJoin[{DirectoryName[$InputFileName],"RussianData"}]}];


Get["x13init`",Path -> {FileNameJoin[{DirectoryName[$InputFileName],"TimeSeries","x13"}]}];

Get["NatLanInit`",Path -> {FileNameJoin[{DirectoryName[$InputFileName],"NaturalLanguage"}]}];

Get["TransformTimeSeries`",Path -> {FileNameJoin[{DirectoryName[$InputFileName],"TimeSeries"}]}];
Get["TimeSeriesFillMissing`",Path -> {FileNameJoin[{DirectoryName[$InputFileName],"TimeSeries"}]}];
Get["HPFilter`",Path -> {FileNameJoin[{DirectoryName[$InputFileName],"TimeSeries"}]}];
Get["TimeSeriesPureF`",Path -> {FileNameJoin[{DirectoryName[$InputFileName],"TimeSeries"}]}];
Get["TimeSeriesStartOfMonthShift`",Path -> {FileNameJoin[{DirectoryName[$InputFileName],"TimeSeries"}]}];
<<<<<<< HEAD
Get["TimeSeriesMakeCommonRange`",Path -> {FileNameJoin[{DirectoryName[$InputFileName],"TimeSeries"}]}];


=======
>>>>>>> origin/master


Get["TernaryPlot`",Path -> {FileNameJoin[{DirectoryName[$InputFileName],"TimeSeries","TimeSeriesCharts"}]}];
Get["ShadedDateListPlot`",Path -> {FileNameJoin[{DirectoryName[$InputFileName],"TimeSeries","TimeSeriesCharts"}]}];
Get["SnippedDateListPlot`",Path -> {FileNameJoin[{DirectoryName[$InputFileName],"TimeSeries","TimeSeriesCharts"}]}];
Get["FanChart`",Path -> {FileNameJoin[{DirectoryName[$InputFileName],"TimeSeries","TimeSeriesCharts"}]}];
Get["TimeSeriesBarChart`",Path -> {FileNameJoin[{DirectoryName[$InputFileName],"TimeSeries","TimeSeriesCharts"}]}];
Get["TwoAxisDateListPlot`",Path -> {FileNameJoin[{DirectoryName[$InputFileName],"TimeSeries","TimeSeriesCharts"}]}];
Get["MakeLegend`",Path -> {FileNameJoin[{DirectoryName[$InputFileName],"TimeSeries","TimeSeriesCharts"}]}];


Get["GrangerTest`",Path -> {FileNameJoin[{DirectoryName[$InputFileName],"TimeSeries"}]}];
Get["BerGalSegment`",Path -> {FileNameJoin[{DirectoryName[$InputFileName],"TimeSeries"}]}];
Get["TimeSeriesJoin`",Path -> {FileNameJoin[{DirectoryName[$InputFileName],"TimeSeries"}]}];
Get["TimeSeriesMinMax`",Path -> {FileNameJoin[{DirectoryName[$InputFileName],"TimeSeries"}]}];
Get["VARinit`",Path -> {FileNameJoin[{DirectoryName[$InputFileName],"TimeSeries","VAR"}]}];
Get["TVARinit`",Path -> {FileNameJoin[{DirectoryName[$InputFileName],"TimeSeries","TVAR"}]}];

Get["Quandl`",Path -> {FileNameJoin[{DirectoryName[$InputFileName],"Quandl"}]}];
Get["FRED`",Path -> {FileNameJoin[{DirectoryName[$InputFileName],"Quandl"}]}];

Get["DFMinit`",Path -> {FileNameJoin[{DirectoryName[$InputFileName],"TimeSeries","DFM"}]}];


SetOptions[$FrontEnd, PrivateFontOptions -> {"OperatorSubstitution" -> False}];
SetOptions[$FrontEnd, CharacterEncoding -> "UTF8"];

DateListPlot;(*preload; not remove!*)
With[{dv := 
   DownValues[Graphics`DateListPlotDump`iDateListPlot]}, 
 dv = dv /. (fr : FilterRules)[a : Graphics`DateListPlotDump`opts, 
     b_Options] :> 
    fr[Join[a, Options@Graphics`DateListPlotDump`caller], b]];

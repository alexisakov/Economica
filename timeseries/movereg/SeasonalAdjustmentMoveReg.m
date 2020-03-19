Package["Economica`"]


(*Version: V .4 2015-07-13
Initial version.
*)


$MoveRegDirectory=DirectoryName[$InputFileName];
$MoveRegStupidMultiplier=10^4;


(*This function creates DATA.TXT*)

ExportMoveRegDataFile[ts_] := Module[{
   tsLength = ts["PathLength"],
   tsStartDate = ts["FirstDate"],
   tsLastDate = ts["LastDate"],
   variableName = "varName",
   periodNum = "52",
   datesString =  ToUpperCase@DateString[#, {"Day", "MonthNameShort", "Year"}] & /@ ts["Dates"],
   dateNumeric =    DateString[#, {"Month", " ", "Day", " ", "Year"}] & /@ ts["Dates"],
   weekISONumber =    Flatten@(DateValue[#, {"ISOWeek"}] & /@ ts["Dates"]),
   values = ToString[Evaluate@IntegerPart[#*$MoveRegStupidMultiplier]] & /@ ts["Values"],
   varNameList,
   weekCumNumber,
   dataFrame,
   maxLength,
   fortranFormat,
   datafile
   },
		varNameList = ConstantArray[variableName, tsLength];
		weekCumNumber = Range@tsLength;
		(*create the data block*)
		dataFrame =   Map[ToString, {datesString, varNameList, dateNumeric, weekISONumber, weekCumNumber, values}, {-1}];
  		maxLength = Max /@ StringLength[dataFrame];
  		dataFrame =  MapThread[StringPadLeft[#1, #2] &, {dataFrame, maxLength}, 1];
  		dataFrame = StringJoin@( StringJoin[Append[#, "\n"] &@Riffle[#, " "]] & /@ (Transpose[    dataFrame])     );
  		
  		fortranFormat =   "(" <> ToString[Evaluate[Total@Most@maxLength + 5]] <> "x,f" <>  ToString[Last@maxLength] <> ".0)";
  		datafile = variableName <> "\n";
  		
  		datafile =    datafile <> (StringJoin[
  			Append[#, "\n"] &@  Riffle[ToString /@ {tsLength, periodNum,
  			DateString[tsStartDate, {"Year", " ", "Month", " ", "Day"}],
  			DateString[tsLastDate, {"Year", " ", "Month", " ", "Day"}], 
  			"6", "0"
        }, " "]]);
		
		datafile = datafile <> fortranFormat <> "\n";
		datafile = datafile <> dataFrame;
		
		Export[FileNameJoin[{$MoveRegDirectory,"DATA.TXT"}], datafile]
  ]

(*This function creates CONTROL.TXT*)

ExportMoveControlFile[ts_] := Module[{
	frequency = 52,
	sourceType = 0,
	numAdditiveOutliers = 0,
	numLevelShifts = 0,
	numberHolidays = 0,
	widthDetrendingFilter = 2,
	weightType = 2,
	phi = 0.4,
	varianceRatio = 16,
	numberFrequencies = 60,
	tsStartDate = ts["FirstDate"],
	tsLastDate = ts["LastDate"],
	controlFile
	},
  controlFile = StringJoin@(Riffle[ToString /@ {frequency, sourceType, "\n"}, " "]);
  controlFile =  controlFile <> 
    StringJoin@(Riffle[
       ToString /@ {numAdditiveOutliers, numLevelShifts, 
         numberHolidays, widthDetrendingFilter, weightType, "\n"}, 
       " "]);
  controlFile =  controlFile <> 
    StringJoin@(Riffle[ToString /@ {phi, varianceRatio, "\n"}, " "]);
  controlFile =   controlFile <> 
    StringJoin@(Riffle[ToString /@ {numberFrequencies, "\n"}, " "]);
  controlFile = controlFile <> StringJoin@(Riffle[ToString /@ {
         DateString[tsStartDate, {"Year", " ", "Month", " ", "Day"}],
         DateString[tsLastDate, {"Year", " ", "Month", " ", "Day"}], 
         "6", "\n"}, " "]);
	Export[FileNameJoin[{$MoveRegDirectory,"CONTROL.TXT"}], controlFile]
	]

(*This function imports the values of the seasonally adjusted time series. It is not aware of dates, however*)

ImportMoveRegResults2[flPath_] := Module[{results2TXT},
  results2TXT = Import[flPath, "Data"];
  results2TXT = Flatten[results2TXT[[9 ;;]]];
  results2TXT = Map[ToExpression, StringSplit /@ results2TXT, {-1}];
  results2TXT[[All, 3]]
  ]


movereg::thirtyseconds="File has not been generated.\n `1`";


SeasonalAdjustmentMoveReg[ts_]:=Module[{
	tsValuesSA,
	integer
	},
	(*create DATA.TXT*)
	ExportMoveRegDataFile[ts];
	(*create CONTROL.TXT*)
	ExportMoveControlFile[ts];
	(*run the MoveReg.exe*)
	Run["cd \"" <> $MoveRegDirectory <> "\" && .\\movereg.exe"];
	(*Import the resulting time-series

	TimeConstrained[
		While[Not@FileExistsQ[FileNameJoin[{$MoveRegDirectory, "results2.out"}]]],
		15,
		Message[movereg::thirtyseconds, Import[FileNameJoin[{$MoveRegDirectory, "summary.out"}], "Text"]];
		Abort[]];
		*)
	tsValuesSA = ImportMoveRegResults2[FileNameJoin[{$MoveRegDirectory,"results2.out"}]];
	tsValuesSA =TimeSeries[Transpose[{ts["Dates"][[-Length@tsValuesSA ;;]], tsValuesSA}]];
	(*clean up
	DeleteFile[FileNames[{"*.out", "*.txt"},$MoveRegDirectory]];*)
	(*return the resulting time series*)
	tsValuesSA/$MoveRegStupidMultiplier
	]







PackageExport["SeasonalAdjustmentMoveReg"]

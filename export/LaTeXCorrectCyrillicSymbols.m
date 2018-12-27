Package["Economica`"]

LaTeXCorrectCyrillicSymbols::usage="";

LaTeXCorrectCyrillicSymbols[inputFileLink_, outputFileLink_] := 
 Module[{
 	inp=Import[inputFileLink, "Text"],
 	oup,
 	link = FileNameJoin[{$UserBaseDirectory, "Applications","Economica","Kernel","misc","cyrillic_encoding.xlsx"}],
 	cyr,ucode,HexToCyrillicSymbols},
 	 	
	cyr = Import[link, {"Data", 1}][[3 ;;]];
	ucode = StringJoin[{"\\unicode{", ToLowerCase@StringTake[#, {-4, -1}],"}"}] & /@ cyr[[All, 1]];
	HexToCyrillicSymbols = Rule @@@ ({ucode, cyr[[All, 2]]}\[Transpose]);

 	oup="\\usepackage[utf8]{inputenc} \n \\usepackage[T2A]{fontenc} \n" <> StringReplace[inp, HexToCyrillicSymbols];

 	Export[outputFileLink,oup,"Text"]
 	];
 	
LaTeXCorrectCyrillicSymbols[inputFileLink_]:=LaTeXCorrectCyrillicSymbols[inputFileLink, inputFileLink];

PackageExport["LaTeXCorrectCyrillicSymbols"]

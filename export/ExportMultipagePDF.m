Package["Economica`"]

(*Exports mutipage pdf.
Source: http://stackoverflow.com/questions/7974804/can-mathematica-create-multi-page-pdf-files.
*)

ExportMultipagePDF::usage="ExportMultipagePDF[name_String, g_List] exports a multipage pdf file
using pdftk.
Source: http://stackoverflow.com/questions/7974804/can-mathematica-create-multi-page-pdf-files" 

InitializePDFTK::usage="InitializePDFTK tries to copy files."

ExportMultipagePDF[name_String, g_List, options___] :=
  Module[
    {fileNames, quote},
    InitializePDFTK[];
    quote[s_] := "\"" <> s <> "\"";
    fileNames = 
      Table[
        FileNameJoin[{$TemporaryDirectory, "mmapage" <> IntegerString[i] <> ".pdf"}],
        {i, Length[g]}
      ];
    Check[
      Export[#1, #2, "PDF", options] & @@@ Thread[{fileNames, g}], 
      Return[$Failed]
    ];
    If[
      Run["pdftk", Sequence @@ (quote /@ fileNames), "cat output", name] =!= 0,
      Return[$Failed]
    ];
    DeleteFile /@ fileNames;
  ];
  
  
InitializePDFTK[]:=Module[{},
	If[Not@FileExistsQ["C:\\Windows\\System32\\pdftk.exe"],
		Print[
		"pdftk is not available on your system.
		Distribution is available at https://www.pdflabs.com/tools/pdftk-server/"
		]];
		Abort[]
];


PackageExport["ExportMultipagePDF"]

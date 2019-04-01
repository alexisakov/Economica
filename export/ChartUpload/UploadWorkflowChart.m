Package["Economica`"]

(*G:\Research\Database\Tools\ChartUpload.rar*)

(*ChartUpload.exe "C:\Files\VS\CustomImages\Samples\cpi.yoy3decomp.emf" "222123" "src:234"*)

$UWCDirectory=DirectoryName[$InputFileName];

QuoteString[s_]:="\""<>ToString@s<>"\"";

UploadWorkflowChart[emfFilePath_,title_,source_]:=Module[{
	},
	Run["cd \"" <> $UWCDirectory <> "\" && .\\ChartUpload", QuoteString@emfFilePath,QuoteString@title,QuoteString@source];
	]

PackageExport["UploadWorkflowChart"]

Package["Economica`"]

Options[ExportFigure] = {
  UploadToWorkflow -> False,
  ExportFiles -> False,
  ShowChart -> False,
  ChartTitle -> Null,
  ChartSource -> Null};



  ExportFigure[fileName_String, chart_, vocabulary_, 
  OptionsPattern[]] := Module[{
   chartRU = chart /. (Normal@vocabulary)
   },
  If[
   OptionValue[ExportFiles],
   Export[fileName <> ".pdf", chart];
   Export[fileName <> ".svg", chart];
   Export[fileName <> "RUS.pdf", chartRU];
   Export[fileName <> "RUS.svg", chartRU];
   If[OptionValue[UploadToWorkflow],
    UploadWorkflowChart[fileName <> ".svg", OptionValue[ChartTitle], 
     OptionValue[ChartSource]];
    UploadWorkflowChart[fileName <> "RUS.svg", 
     OptionValue[ChartTitle] /. (Normal@vocabulary), 
     OptionValue[ChartSource] /. (Normal@vocabulary)];
    ]
   ];
  
  If[OptionValue[ShowChart],
   TableForm@{
     {OptionValue[ChartTitle], 
      OptionValue[ChartTitle] /. (Normal@vocabulary)},
     {chart, chartRU},
     {OptionValue[ChartSource], 
      OptionValue[ChartSource] /. (Normal@vocabulary)}},
   ""
   ]
  ]

PackageExport["ExportFigure"]
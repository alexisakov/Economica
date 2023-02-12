Package["Economica`"]


PackageImport["CorporateTheme`"]


Options[TileMap] = {
  ColorFunction -> 
   Function[u, 
    Blend[{$ThemeColorDataIndexed@3, $ThemeColorDataIndexed@1}, u]]
  , MissingDataColorFunction -> Function[u, White]
  , ChartElementFunction -> Function[u, Rectangle[u]]
  , LabelingStyleFunction -> Function[u, White]
  , LabelingFunction -> 
   Function[{label, position}, 
    Text[ThemeTextStyle[label, FontSize -> $ThemeFontSizeSmall], position + {0.5, 0.7}]]
  , EpilogFunction -> 
   Function[{value, position}, 
    Text[ThemeTextStyle[ToString[value], FontSize -> 8], 
     position + {0.5, 0.3}]],
  Epilog -> {},
  Background -> ColorData["CorpNamed"]["Black"]
  };

TileMap[coord_Association, data_Association, OptionsPattern[]] := 
 Module[{
   keys = Keys@coord
   , tile, label, epi
   },
  tile = Flatten[
    {
       If[
        MissingQ@(data@#)
        , OptionValue[MissingDataColorFunction][data[#]]
        , OptionValue[ColorFunction][data[#]]
        ],
       OptionValue[ChartElementFunction][coord[#]]
       } & /@ keys
    ];
  
  label = {
      OptionValue[LabelingStyleFunction][coord[#]],
      OptionValue[LabelingFunction][#, coord[#]]} & /@ keys;
  
  epi =
   If[
      MissingQ@(data@#)
      , Nothing
      , OptionValue[EpilogFunction][data[#], coord[#]]
      ] & /@ keys;
  
  (*put everything together*)
  Graphics[{
    tile,
    label,
    epi
    }
   , ImageSize -> {1152, 576}
   , Epilog -> OptionValue[Epilog]
   , Background -> OptionValue[Background]]
  
  ];   


  
  PackageExport["TileMap"]
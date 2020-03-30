Package["Economica`"]

cm = 72/2.54;

(*Colors of the 2018 theme*)
ColorDataVTBCapital2018 = Function[x,   
    Blend[Transpose[{Range@8,  
 		(RGBColor/@({{0, 170, 255}, {120, 180, 151}, {234, 107, 80}, {168, 46, 20}, {0, 102, 153}, {64, 116, 91}, 
 			{224, 62, 27},{0, 170, 255}}/255.))}], Mod[x,8,1]]];  	  

LoadVTBCapitalTheme[]:= (
	(*The 2018 theme definition*)
	Themes`AddThemeRules["VTBCapital2018",
		DefaultPlotStyle -> (ColorDataVTBCapital2018/@Range[8]),
		ChartBaseStyle->EdgeForm[Directive[GrayLevel[0.356],Opacity[1]]],
		ChartStyle ->(ColorDataVTBCapital2018/@Range[8]),
		ImageSize -> {24, 18}/1.5 cm,
		AspectRatio -> 3/4,
		TicksStyle -> Directive[22/1.5, FontName -> "Arial" , Black, Opacity[0.3], FontOpacity -> 1],
		FrameTicksStyle -> Directive[22/1.5, FontName -> "Arial" , Black]
		];
	Themes`AddThemeRules["VTBCapital2018", BarChart,
		ChartStyle ->(Directive[EdgeForm[],ColorDataVTBCapital2018@#]&/@Range[8]), 
		ImageSize -> {24, 18}/1.5 cm,
		AspectRatio -> 3/4,
		ChartLayout->"Stacked",
		TicksStyle -> Directive[22/1.5, FontName -> "Arial" , Black, Opacity[0.3], FontOpacity -> 1]
		];
	Themes`AddThemeRules["VTBCapital2018", TimeSeriesBarChart,
		ChartStyle ->(Directive[EdgeForm[],ColorDataVTBCapital2018@#]&/@Range[8]), 
		ChartLayout->"Stacked",
		TicksStyle -> Directive[22/1.5, FontName -> "Arial" , Black, Opacity[0.3], FontOpacity -> 1]
	
		];
	Themes`AddThemeRules["VTBCapital2018", ShadedDateListPlot,
		PlotStyle -> (ColorDataVTBCapital2018/@Range[8]),
		ImageSize -> {24, 18}/1.5 cm,
		AspectRatio -> 3/4,
		TicksStyle -> Directive[22/1.5, FontName -> "Arial" , Black, Opacity[0.3], FontOpacity -> 1],
		FrameTicksStyle -> Directive[22/1.5, FontName -> "Arial" , Black]
	
		];
	Themes`AddThemeRules["VTBCapital2018",Histogram,
		ChartLayout->"Stacked",
		ChartStyle ->(Directive[EdgeForm[],ColorDataVTBCapital2018@#]&/@Range[8]), 
		ImageSize -> {24, 18}/1.5 cm,
		AspectRatio -> 3/4,
		TicksStyle -> Directive[22/1.5, FontName -> "Arial" , Black, Opacity[0.3], FontOpacity -> 1]
		];
	Themes`AddThemeRules["VTBCapital2018",DateListPlot,   
		Axes -> True, 
		Frame -> False,
		PlotStyle -> ({ColorDataVTBCapital2018[#],Thickness[0.008]}&/@Range[8]),
		ImageSize -> {24, 18}/1.5 cm,
		AspectRatio -> 3/4,
		TicksStyle -> Directive[22/1.5, FontName -> "Arial" , Black, Opacity[0.3], FontOpacity -> 1],
		FrameTicksStyle -> Directive[22/1.5, FontName -> "Arial" , Black]
		];
	Themes`AddThemeRules["VTBCapital2018",ListPlot,   
		Axes -> True, 
		Frame -> False,
		PlotStyle -> ({ColorDataVTBCapital2018[#],Thickness[0.008]}&/@Range[8]),
		ImageSize -> {24, 18}/1.5 cm,
		AspectRatio -> 3/4,
		TicksStyle -> Directive[22/1.5, FontName -> "Arial" , Black, Opacity[0.3], FontOpacity -> 1],
		FrameTicksStyle -> Directive[22/1.5, FontName -> "Arial" , Black]
		];
	Themes`AddThemeRules["VTBCapital2018",Plot,   
	    Axes -> True, 
	    Frame -> False,
	    Axes -> True, 
		Frame -> False,
		PlotStyle -> ({ColorDataVTBCapital2018[#],Thickness[0.008]}&/@Range[8]),
		ImageSize -> {24, 18}/1.5 cm,
		AspectRatio -> 3/4,
		TicksStyle -> Directive[22/1.5, FontName -> "Arial" , Black, Opacity[0.3], FontOpacity -> 1],
		FrameTicksStyle -> Directive[22/1.5, FontName -> "Arial" , Black]
		]; 
 	Themes`AddThemeRules["VTBCapital2018",PieChart,
		ChartLayout->"Stacked" 
	   	]; 
	Themes`AddThemeRules["VTBCapital2018",PairedHistogram,  
	    AxesStyle -> Directive[FontFamily -> "Arial", FontSize -> 22/1.5] 
	    ];
	Themes`AddThemeRules["VTBCapital2018",RadarChart`RadarChart,   
		AxesType -> "Star"
	    ]; 
	SetOptions[Callout, 
		Background -> Directive[Opacity[0.0]],
		Appearance -> "SlantedLabel",
		LabelStyle->{14.6667,Black, FontFamily -> "Arial", Background -> Opacity[0.0]}	
		];
	)

PackageExport["LoadVTBCapitalTheme"]
PackageExport["ColorDataVTBCapital2018"]

Options[VTBTextStyle]={FontSize -> 22/1.5}
Options[VTBTextStyleNoColor]={FontSize -> 22/1.5}

VTBTextStyle[x_,OptionsPattern[]] := Style[x, FontFamily -> "Arial", FontSize -> OptionValue[FontSize], Black, LineSpacing -> {0.1, 10}]; 
VTBTextStyleNoColor[x_,OptionsPattern[]] := Style[x, FontFamily -> "Arial", FontSize -> OptionValue[FontSize],  LineSpacing -> {0.1, 10}];
PackageExport["VTBTextStyle"]
PackageExport["VTBTextStyleNoColor"]
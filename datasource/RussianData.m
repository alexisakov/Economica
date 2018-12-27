Package["Economica`"]

(* Exported symbols added here with SymbolName::usage *) 
RussianData::usage="RussianData is used to get macroeconomic data from several selected sources.";

(* Implementation of the package *)
RussianData::unknwnindctr = "Indicator `1` is unknown and yet to be included.";

russianDataIndicators={"ElectricityConsumption","Temperature"};

RussianData[indicator_String]:=Module[{},
	CheckAbort[If[Not@MemberQ[russianDataIndicators,indicator],Abort[]],Message[RussianData::unknwnindctr,indicator]];
	GetRussianData[indicator]	
]

(*Altenativa syntax with time range*)
RussianData[indicator_String,{tMin_,tMax_}]:=Module[{},
	CheckAbort[If[Not@MemberQ[russianDataIndicators,indicator],Abort[]],Message[RussianData::unknwnindctr,indicator]];
	GetRussianData[indicator,{tMin,tMax}]	
]
(*Now for the data*)
(*1. Economic activity*)
(*1.1. Electricity consumption
Source: http://www.so-ups.ru/index.php?id=ees_gen_consump_day&no_cache=1*)

GetRussianData["ElectricityConsumption", {t1_, t2_}] /; 
  DateDifference[t1, t2] > Quantity[30, "Days"] && 
   AbsoluteTime@t1 < AbsoluteTime@t2 := Module[
  {ranges},
  ranges = {First@#, Last@#} & /@ Partition[DateRange[t1, DatePlus[t2, {1, "Day"}]], 30, 30, 1, {}];
  Union@Flatten[ GetRussianData["ElectricityConsumption", #] & /@ ranges, 1]
  
  (*;  Union@(Join@@((GetRussianData["ElectricityConsumption",  Sequence@@#]&)/@ranges))*)
  ]
  
  GetRussianData["ElectricityConsumption", {t1_, t2_}] /; 
  DateDifference[t1, t2] <= Quantity[30, "Days"] && 
   AbsoluteTime@t1 < AbsoluteTime@t2 := Module[
  {rawEnergy, cleanEnergy},
  rawEnergy = 
   Import[
   "http://www.so-ups.ru/index.php?id=ees_gen_consump_day&no_cache=1&tx_ms1cdu_pi1[kpo]=1019&tx_ms1cdu_pi1[dt]=" <> 
     DateString[DatePlus[t2, {1, "Day"}], {"Day", ".", "Month", ".", "Year"}] <> 
     "&tx_ms1cdu_pi1[format]=xml", "XML"];
  cleanEnergy = Cases[rawEnergy,
    XMLElement[
      "item", {}, {XMLElement["date", {}, {date_}], 
       XMLElement["generation", {}, {generation_}], 
       XMLElement["consumption", {}, {consumption_}]}] :>
     {DateList[date], 
      ToExpression@StringReplace[generation, "," -> "."],
      ToExpression@StringReplace[consumption, "," -> "."]
      }, \[Infinity]];
  
  Select[cleanEnergy, AbsoluteTime@First@# >= AbsoluteTime@t1 &][[All, {1, 2}]]
  ];
  
  GetRussianData["Temperature", {t1_, t2_}] /; 
  DateDifference[t1, t2] > Quantity[30, "Days"] && 
   AbsoluteTime@t1 < AbsoluteTime@t2 := Module[
  {ranges},
  ranges = {First@#, Last@#} & /@ Partition[DateRange[t1, DatePlus[t2, {1, "Day"}]], 30, 30, 1, {}];
  Union@Flatten[
    GetRussianData["Temperature", #] & /@ ranges, 1]
  ]; 
  GetRussianData["Temperature", {t1_, t2_}] /; 
  DateDifference[t1, t2] <= Quantity[30, "Days"] && 
   AbsoluteTime@t1 < AbsoluteTime@t2 := Module[
  {rawEnergy, cleanEnergy},
  rawEnergy = 
   Import["http://so-ups.ru/index.php?id=ees_temperature&no_cache=1&tx_ms1cdu_pi1[kpo]=1019&tx_ms1cdu_pi1[dt]=" <> 
     DateString[t2, {"Day", ".", "Month", ".", "Year"}] <> 
     "&tx_ms1cdu_pi1[format]=xml", "XML"];
  cleanEnergy = Cases[rawEnergy,
    XMLElement[
      "item", {}, {XMLElement["datetime", {}, {datetime_}], 
       XMLElement["temperature", {}, {temperature_}]}] :>
     {DateList[StringTake[datetime,10]], 
      ToExpression@StringReplace[temperature, "," -> "."]},
 \[Infinity]];
    Select[cleanEnergy, AbsoluteTime@First@# >= AbsoluteTime@t1 &][[All, {1, 2}]]
  ];
  
PackageExport["GetRussianData"]


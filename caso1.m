(* ::Package:: *)

(*Ejercicio 1*)
Needs[ "RandomData`"]
(*Ejercicio 2*)
RandomTable = Table [RandomData[],1000];
Histogram[RandomTable]



(*Ejercicio 3*)
lambda=50;
RandomExpTable=Table[RandomExp[lambda],1000];
Histogram[RandomExpTable]


(*Ejercicio 4*)
nmax = 5000;
lambda = 90;
mu = 100;

InterArrivalsTime = Table[RandomExp[lambda],nmax];

ServiceTime = Table[RandomExp[mu],nmax];

AcumSeries[listInterArrivalsTime_]:= Module[{AcumVal=0},AcumVal+=#&/@listInterArrivalsTime];
Arrivals = AcumSeries[InterArrivalsTime];
 (*Arrivals = Accumulate[InterArrivalsTime];*)
 (*& para que considere que es una funci\[OAcute]n*)

 FifoSchedulling [arrivals_,service_]:=Module[{n,checkTime},n=1;checkTime=arrivals[[1]]; (If [checkTime >= #,checkTime+=service[[n++]],checkTime=#+service[[n++]]])&/@arrivals];
 Departures = FifoSchedulling [Arrivals,ServiceTime];
 
 Manipulate[Show[ListPlot[Arrivals[[origin;;origin+width]],PlotStyle->Red],ListPlot[Departures[[origin;;origin+width]],PlotStyle->Blue],PlotRange->All],{origin,1,nmax-width,1},{width,1,50,1}]


 (*UserStepStair*)
 calculateNumberUsers [arrivals_,service_]:= Module [ {nUsers}, nUsers=0; 
 ArrivalsList = Transpose[{Arrivals,Table[1,Length[Arrivals]]}];
 ServiceList = Transpose[{Departures,Table[-1,Length[Departures]]}];
 MergedList = Join[{{0,0}},ArrivalsList,ServiceList];
 MergedList = SortBy[MergedList,#[[1]]];
 ({#[[1]],nUsers+=#[[2]]})&/@MergedList
 ];
 UserStepStair=calculateNumberUsers[Arrivals,Departures];
 maxLength = Length [UserStepStair];
 Manipulate[ListPlot[UserStepStair[[origin;;origin+width]],InterpolationOrder -> 0,Joined -> True],{origin,1,nmax-width,1},{width,1,50,1}]




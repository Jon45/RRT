(* ::Package:: *)

(*Ejercicio 1*)
Needs["RandomData`"]
(*Ejercicio 2*)
RandomTable = Table [RandomData[],1000];
Histogram[RandomTable]


(*Ejercicio 3*)
lambda=10;
RandomExpTable=Table[RandomExp[lambda],1000];
Histogram[RandomExpTable]


(*Ejercicio 4*)
nmax = 1000;
lambda = 10;
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
 Manipulate[ListPlot[UserStepStair[[origin;;origin+width]],InterpolationOrder -> 0,Joined -> True],{origin,1,nmax-width,1},{width,1,50,1}] (*ListStepPlot tambi\[EAcute]n vale*)


 calculateStepStair [lista_]:= Module [ {nUsers}, nUsers=0;
 Join [{{0,0}},({#,nUsers+=1})&/@lista]
 ];
 StepsArrivals = calculateStepStair[Arrivals];
 StepsDepartures = calculateStepStair[Departures];
 Manipulate[Show[ListPlot[StepsArrivals[[origin;;origin+width]],InterpolationOrder -> 0,Joined -> True],ListPlot[StepsDepartures[[origin;;origin+width]],InterpolationOrder -> 0,Joined -> True,PlotStyle->Red]],{origin,1,nmax-width,1},{width,1,50,1}]


(*Ejercicio 5*)
NPoints=100;
Module[{InterArrivalsTime,ServiceTime,Arrivals,Departures,lambda,mu},
lambda=Range[1/NPoints,1,1/NPoints];
mu=1;
MeanWaitingTime=(
InterArrivalsTime = Table[RandomExp[#],nmax];
ServiceTime = Table[RandomExp[mu],nmax];
Arrivals = AcumSeries[InterArrivalsTime];
Departures = FifoSchedulling [Arrivals,ServiceTime];
{#,Mean[Departures-Arrivals]}
)&/@lambda;
]
ListPlot[MeanWaitingTime]


(*Ejercicio 6*)
nmax=100;
ProbabilidadesTeoricas[nmax_,p_]:= Module[{n},n=Range[0,nmax,1];((1-p)*p^n)&/@n];
pTeoricas=ProbabilidadesTeoricas[nmax,lambda/mu];
ListPlot[pTeoricas,PlotRange -> Full]

ProbabilidadesSimuladas[UserStepStair_,nmax_] := Module[{ocurrencias,n,p,i,acumval},
ocurrencias=MapThread[({#2[[1]]-#1[[1]],#1[[2]]})&,{UserStepStair[[1;;Length[UserStepStair]-1]],UserStepStair[[2;;Length[UserStepStair]]]}];
n=Range[0,nmax,1];
p=(acumval=0;For[i=1,i<=Length[ocurrencias],i++,If[#==ocurrencias[[i,2]],acumval+=ocurrencias[[i,1]]]];acumval)&/@n;
p/Total[p]
];
pSimuladas=ProbabilidadesSimuladas[UserStepStair,nmax];
ListPlot[pSimuladas,PlotRange -> Full]

Pasta[Arrivals_,UserStepStair_,nmax_]:= Module[{position,n,cuentaPasta,probabilidadesPasta},
cuentaPasta=(position=FirstPosition[Transpose[UserStepStair][[1]],#]; Flatten[UserStepStair[[position]]] [[2]]-1)&/@Arrivals;
n=Range[0,nmax,1];
probabilidadesPasta = (Count[cuentaPasta,#])&/@n;
probabilidadesPasta/Total[probabilidadesPasta]
];
probabilidadesPasta=Pasta[Arrivals,UserStepStair,nmax];
ListPlot[probabilidadesPasta,PlotRange -> Full]




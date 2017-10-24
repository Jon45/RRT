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

AcumSeries[listInterArrivalsTime_]:= Module[{AcumVal=0},AcumVal+=#&/@listInterArrivalsTime]; (*Obtiene una lista con los valores acumulados: Por ejemplo,en la posici\[OAcute]n 3, lo que hab\[IAcute]a en la posici\[OAcute]n 1 + posici\[OAcute]n 2 + posici\[OAcute]n 3 de la lista de entrada,...*)
Arrivals = AcumSeries[InterArrivalsTime];
 (*Arrivals = Accumulate[InterArrivalsTime];*)
 (*& para que considere que es una funci\[OAcute]n*)

 FifoSchedulling [arrivals_,service_]:=Module[{n,checkTime},n=1;checkTime=arrivals[[1]]; (If [checkTime >= #,checkTime+=service[[n++]],checkTime=#+service[[n++]]])&/@arrivals];
 Departures = FifoSchedulling [Arrivals,ServiceTime];
 
 Manipulate[Show[ListPlot[Arrivals[[origin;;origin+width]],PlotStyle->Red],ListPlot[Departures[[origin;;origin+width]],PlotStyle->Blue],PlotRange->All],{origin,1,nmax-width,1},{width,1,50,1}]


 (*UserStepStair*)
 calculateNumberUsers [arrivals_,service_]:= Module [ {nUsers}, nUsers=0; 
 ArrivalsList = Transpose[{Arrivals,Table[1,Length[Arrivals]]}]; (*Crea una lista de tuplas, un campo con el tiempo de llegada y otro con el valor 1*)
 ServiceList = Transpose[{Departures,Table[-1,Length[Departures]]}]; (*Crea una lista de tuplas, un campo con el tiempo de salida y otro con el valor -1*)
 MergedList = Join[{{0,0}},ArrivalsList,ServiceList]; (*A\[NTilde]ade el punto 0,0 de origen para que se visualize posteriormente*)
 MergedList = SortBy[MergedList,#[[1]]]; (*Ordena la lista en funci\[OAcute]n del primero de los campos: Tiempo de llegada o tiempo de salida dependiendo del elemento*)
 ({#[[1]],nUsers+=#[[2]]})&/@MergedList (*Devuelve una lista con tiempos y el n\[UAcute]mero de usuarios que le corresponde a cada tiempo. Va sumando los valores del segundo campo, -1 y 1, para determinar el n\[UAcute]mero de usuarios*)
 ];
 UserStepStair=calculateNumberUsers[Arrivals,Departures];
 maxLength = Length [UserStepStair];
 Manipulate[ListPlot[UserStepStair[[origin;;origin+width]],InterpolationOrder -> 0,Joined -> True],{origin,1,nmax-width,1},{width,1,50,1}] (*ListStepPlot tambi\[EAcute]n vale. Interpolation order y joined para que te una los puntos en forma de escalera*)


 calculateStepStair [lista_]:= Module [ {nUsers}, nUsers=0;
 Join [{{0,0}},({#,nUsers+=1})&/@lista] (*A\[NTilde]ade el punto 0,0. El resto de puntos consisten en un valor de tiempos y un valor acumulado al que se va sumando 1 cada vez que se genera un nuevo punto*)
 ];
 StepsArrivals = calculateStepStair[Arrivals];
 StepsDepartures = calculateStepStair[Departures];
 Manipulate[Show[ListPlot[StepsArrivals[[origin;;origin+width]],InterpolationOrder -> 0,Joined -> True],ListPlot[StepsDepartures[[origin;;origin+width]],InterpolationOrder -> 0,Joined -> True,PlotStyle->Red]],{origin,1,nmax-width,1},{width,1,50,1}]


(*Ejercicio 5*)
NPoints=100;
Module[{InterArrivalsTime,ServiceTime,Arrivals,Departures,lambda,mu}, (*Module para que no nos toque las variables que ya tenemos definidas*)
lambda=Range[1/NPoints,1,1/NPoints]; (*Los valores de lambda para el experimento dependeran del n\[UAcute]mero de puntos*)
mu=1; (*Normalizamos con esta mu para que lambda=rho*)
MeanWaitingTime=(
InterArrivalsTime = Table[RandomExp[#],nmax];
ServiceTime = Table[RandomExp[mu],nmax];
Arrivals = AcumSeries[InterArrivalsTime];
Departures = FifoSchedulling [Arrivals,ServiceTime];
{#,Mean[Departures-Arrivals]} (*Se acabara devolviendo una lista de puntos con lambda (o rho en este caso) y el tiempo medio de espera*)
)&/@lambda; (*Calcula para los diferentes valores de lambda*)
]
ListPlot[MeanWaitingTime]


(*Ejercicio 6*)
nmax=100;
ProbabilidadesTeoricas[nmax_,p_]:= Module[{n},n=Range[0,nmax,1];((1-p)*p^n)&/@n]; (*F\[OAcute]rmula te\[OAcute]rica*)
pTeoricas=ProbabilidadesTeoricas[nmax,lambda/mu];
ListPlot[pTeoricas,PlotRange -> Full]

ProbabilidadesSimuladas[UserStepStair_,nmax_] := Module[{ocurrencias,n,p,i,acumval},
ocurrencias=MapThread[({#2[[1]]-#1[[1]],#1[[2]]})&,{UserStepStair[[1;;Length[UserStepStair]-1]],UserStepStair[[2;;Length[UserStepStair]]]}]; (*Obtiene una lista de parejas de valores. 
Primer valor \[Rule] Diferencia de tiempos entre 2 puntos contiguos de userStepStair. Segundo valor \[Rule] n\[UAcute]mero de usuarios del primero de los puntos contiguos*)
n=Range[0,nmax,1];
p=(acumval=0;For[i=1,i<=Length[ocurrencias],i++,If[#==ocurrencias[[i,2]],acumval+=ocurrencias[[i,1]]]];acumval)&/@n; (*Cuenta el tiempo que se est\[AAcute] en cada estado. p[[i+1]] \[Rule] tiempo en el estado i*)
p/Total[p] (*Normaliza de forma que la suma total de p sea 1*)
];
pSimuladas=ProbabilidadesSimuladas[UserStepStair,nmax];
ListPlot[pSimuladas,PlotRange -> Full]

Pasta[Arrivals_,UserStepStair_,nmax_]:= Module[{position,n,cuentaPasta,probabilidadesPasta},
cuentaPasta=(position=FirstPosition[Transpose[UserStepStair][[1]],#]; (*Muestrea userStepStair en puntos de tiempo definidos por los tiempos de llegada. Busca en UserStepStair puntos cuyo tiempo coincida con el tiempo
de llegada y obtiene su posici\[OAcute]n. Para cada iteraci\[OAcute]n deber\[IAcute]a s\[OAcute]lo haber uno, por eso se utiliza FirstPosition*)
Flatten[UserStepStair[[position]]] [[2]]-1)&/@Arrivals; (*Teniendo en cuenta la posici\[OAcute]n, se extrae el n\[UAcute]mero de usuarios correspondiente. Se resta 1 para considerar el n\[UAcute]mero de usuarios justo antes de la llegada,
de forma que sea posible que haya 0 usuarios. Estas dos sentencias se repiten para cada valor del array Arrivals*)
n=Range[0,nmax,1];
probabilidadesPasta = (Count[cuentaPasta,#])&/@n; (*Para cada valor de n, cuenta el n\[UAcute]mero de ocurrencias de ese valor en el n\[UAcute]mero de usuarios*)
probabilidadesPasta/Total[probabilidadesPasta] (*Normalizaci\[OAcute]n para que la suma sea 1*)
];
probabilidadesPasta=Pasta[Arrivals,UserStepStair,nmax];
ListPlot[probabilidadesPasta,PlotRange -> Full]


  (*Ejercicio 7*)
  Needs["RandomData`"]
  
  nmax=1000;
  lambda=10;
  mu=100;
  
  InterArrivalsTime=Table[RandomExp[lambda],nmax];
  
  ServiceTime=Table[RandomExp[mu],nmax];
  
  AcumSeries[listInterArrivalsTime_]:=Module[{AcumVal=0},AcumVal+=#&/@listInterArrivalsTime]; 
  Arrivals=AcumSeries[InterArrivalsTime];
  
  FifoSchedulling[arrivals_,service_]:=Module[{n,checkTime,checkTime2,departures,MergedList},n=1;checkTime=arrivals[[1]];checkTime2=arrivals[[1]];departures=(If[checkTime<=checkTime2,
  If[checkTime>=#,checkTime+=service[[n++]],checkTime=#+service[[n++]]],If[checkTime2>=#,checkTime2+=service[[n++]],checkTime=#+service[[n++]]]
  ])&/@arrivals; (*Similar a la de MM1, s\[OAcute]lo que ahora hay 2 checktimes. Se comprueba en cada iteraci\[OAcute]n cual es m\[AAcute]s peque\[NTilde]o y se utiliza ese checktime para hacer las mismas operaciones que en caso MM1. 
  Se podr\[IAcute]an pasar 2 tiempos de servicio para poder simular colas con diferentes u. Intentar\[EAcute] hacerlo para m\[AAcute]s tarde. Tambi\[EAcute]n separar por colas para poder analizar el comportamiento de cada cola*)
  MergedList=Transpose[{arrivals,departures}];
  Transpose[SortBy[MergedList,Last]] (*Se han juntado las listas para ordenarlas conjuntamente, de manera que se pueda seguir asociando cada tiempo de salida a su tiempo de llegada correspondiente*)
  ];

  {Arrivals,Departures}=FifoSchedulling[Arrivals,ServiceTime];
  Length[Departures]
  
  Manipulate[Show[ListPlot[Arrivals[[origin;;origin+width]],PlotStyle->Red],ListPlot[Departures[[origin;;origin+width]],PlotStyle->Blue],PlotRange->All],{origin,1,nmax-width,1},{width,1,50,1}]


(*UserStepStair*)
calculateNumberUsers[arrivals_,service_]:=Module[{nUsers},nUsers=0;
ArrivalsList=Transpose[{Arrivals,Table[1,Length[Arrivals]]}];
ServiceList=Transpose[{Departures,Table[-1,Length[Departures]]}];
MergedList=Join[{{0,0}},ArrivalsList,ServiceList];
MergedList=SortBy[MergedList,#[[1]]];
({#[[1]],nUsers+=#[[2]]})&/@MergedList];
UserStepStair=calculateNumberUsers[Arrivals,Departures];
maxLength=Length[UserStepStair];
Manipulate[ListPlot[UserStepStair[[origin;;origin+width]],InterpolationOrder->0,Joined->True],{origin,1,nmax-width,1},{width,1,50,1}]


calculateStepStair[lista_]:=Module[{nUsers},nUsers=0;
Join[{{0,0}},({#,nUsers+=1})&/@lista]];
StepsArrivals=calculateStepStair[Arrivals];
StepsDepartures=calculateStepStair[Departures];
Manipulate[Show[ListPlot[StepsArrivals[[origin;;origin+width]],InterpolationOrder->0,Joined->True],ListPlot[StepsDepartures[[origin;;origin+width]],InterpolationOrder->0,Joined->True,PlotStyle->Red]],{origin,1,nmax-width,1},{width,1,50,1}]


NPoints=100;
Module[{InterArrivalsTime,ServiceTime,Arrivals,Departures,lambda,mu},
mu=10;
lambda=Range[1/NPoints,2*mu,1/NPoints];
MeanWaitingTime=(InterArrivalsTime=Table[RandomExp[#],nmax];
ServiceTime=Table[RandomExp[mu],nmax];
Arrivals=AcumSeries[InterArrivalsTime];
{Arrivals,Departures}=FifoSchedulling[Arrivals,ServiceTime];
{#/(2*mu),Mean[Departures-Arrivals]})&/@lambda];
ListPlot[MeanWaitingTime] (*Igual mejor separar por colas. Queda pendiente*)


nmax=100;
ProbabilidadesTeoricas[nmax_,p_]:=Module[{n,estado},n=Range[0,nmax,1];(If[#==0,(1-p)/(1+p),2*(1-p)/(1+p)*p^#])&/@n];
pTeoricas=ProbabilidadesTeoricas[nmax,lambda/(2*mu)];
ListPlot[pTeoricas,PlotRange->Full]

ProbabilidadesSimuladas[UserStepStair_,nmax_]:=Module[{ocurrencias,n,p,i,acumval},ocurrencias=MapThread[({#2[[1]]-#1[[1]],#1[[2]]})&,{UserStepStair[[1;;Length[UserStepStair]-1]],UserStepStair[[2;;Length[UserStepStair]]]}];
n=Range[0,nmax,1];
p=(acumval=0;For[i=1,i<=Length[ocurrencias],i++,If[#==ocurrencias[[i,2]],acumval+=ocurrencias[[i,1]]]];acumval)&/@n;
p/Total[p]];
pSimuladas=ProbabilidadesSimuladas[UserStepStair,nmax];
ListPlot[pSimuladas,PlotRange->Full]

Pasta[Arrivals_,UserStepStair_,nmax_]:=Module[{position,n,cuentaPasta,probabilidadesPasta},cuentaPasta=(position=FirstPosition[Transpose[UserStepStair][[1]],#];Flatten[UserStepStair[[position]]][[2]]-1)&/@Arrivals;
n=Range[0,nmax,1];
probabilidadesPasta=(Count[cuentaPasta,#])&/@n;
probabilidadesPasta/Total[probabilidadesPasta]];
probabilidadesPasta=Pasta[Arrivals,UserStepStair,nmax];
ListPlot[probabilidadesPasta,PlotRange->Full]
(*Estar\[IAcute]a bien comparar con MM1 para rho m\[AAcute]s altas*)

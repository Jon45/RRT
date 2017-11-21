(* ::Package:: *)

<<  /home/jon/RRT/RandomData.m
<<  /home/jon/RRT/drawTx.m


a={0,9600,1,0,0};
SetIniParDraw[.5,0];
Show[DrawPacketTx[a],DrawWin[0,10,8]]  (*Tiempo de ack: bits, tiempo del paquete es longitud*)

lstPck = Table[{2+n*10,100,n+1,0,0},{n,0,9}];
Show[DrawPacketTx[#]&/@lstPck,DrawWin[0,100,8]]


(*Ejercicio 2*)

nmax = 1000;
lambda = 1;
mu = 10;

tp=0;
tack=0;

InterArrivalsTime = Table[RandomExp[lambda],nmax];

ServiceTime = Table[RandomExp[mu],nmax];

AcumSeries[listInterArrivalsTime_]:= Module[{AcumVal=0},AcumVal+=#&/@listInterArrivalsTime];
Arrivals = AcumSeries[InterArrivalsTime];

FifoSchedulling [arrivals_,service_]:=Module[{n,checkTime},n=1;checkTime=arrivals[[1]]; (If [checkTime >= #,checkTime+=service[[n++]],checkTime=#+service[[n++]]])&/@arrivals];
Departures = FifoSchedulling [Arrivals,ServiceTime];

injTimes = Departures - ServiceTime; (*Cuando se empieza a transmitir el paquete*)
(*sequenceNumbers = Table[n,{n,1,9,1}];
lstPck = Table[{2+n*10,10,n+1,0,0},{n,0,9}]*)

sequenceNumbers = Table[n,{n,1,Length[injTimes],1}];
lstPck = MapThread[({#1,9600*#2,#3,0,0})&,{injTimes,ServiceTime,sequenceNumbers}]; (*Duraci\[OAcute]n de paquete se multiplica por 9600 para obtener longitud de paquete en bits*)


SetIniParDraw[tp,tack];
Manipulate [ Show [ DrawWin[origin,ww,8],Map[DrawPacketTx[#]&, SelectPacketInWin[lstPck]]],{origin,0,100-ww},{ww,1,50}]


PacketArrivalsGenTimePeriod[lambda_,mu_,period_,ini_] := Module[{llegada,llegadas,len,n},
len=ini + period;
llegada = ini + RandomExp[lambda];
n=1;
llegadas={};
While [llegada < len,
AppendTo[llegadas,llegada];
n++;
llegada+=RandomExp[lambda];
];
llegadas
]; (*Esta funci\[OAcute]n no se utiliza. En el ejemplo subido a egela est\[AAcute] como hacerlo*)

FifoPacketTxSW[lstArr_,p_]:= Module[{n,checkTime,paquetes,lista,numRepeticiones,paquetesIter},n=1;checkTime=lstArr[[1,1]]; paquetes={}; 
(lista=GetPacketRTxSW[#,p];paquetesIter = lista[[2]]; numRepeticiones=lista[[1]]; (*Para cada paquete, obtiene una lista de paquetes derivados de su transmisi\[OAcute]n correcta y las transmisiones err\[OAcute]neas*)
If [checkTime >= #[[1]],
checkTime+=numRepeticiones*(#[[2]]/9600+tack+2*tp),
checkTime=#[[1]]+numRepeticiones*(#[[2]]/9600+tack+2*tp)
];
n=numRepeticiones+1;
paquetesIter = (n--;{checkTime-n*(#[[2]]/9600+tack+2*tp),#[[2]],#[[3]],#[[4]],#[[5]]})&/@paquetesIter; (*Se ponen los tiempos de inyecci\[OAcute]n de los paquetes para cada una de las transmisiones*)
AppendTo[paquetes,paquetesIter];
)&/@lstArr;
Partition[Flatten[paquetes],5] (*Para que tengan el formato adecuado. Se hace un flatten de toda la lista, y se subdivide cada 5 elementos, los que conforman un paquete*)
];

GetPacketRTxSW[pck_,p_]:= Module[{numRepet,paquetes},
numRepet= Length[NestWhileList[#&,p,#>RandomReal[]&]]; (*Establece el n\[UAcute]mero de repeticiones*)
paquetes = Table[{pck[[1]],pck[[2]],pck[[3]],1,n},{n,1,numRepet,1}]; (*Genera los paquetes repetidos y el correcto*)
paquetes[[numRepet,4]]=0; (*El \[UAcute]ltimo paquete es el correcto, por lo que su cuarto campo tiene que valer 0*)
{numRepet,paquetes}
];


tp=0.5;
tack=0.1;
nmax=10000;
lambda=1.99;
mu=2;
p=0;
ti=1/mu;
a=(ti + 2*tp + tack)/ti; (*a no se puede meter en las funciones. Preguntar*)

InterArrivalsTime = Table[RandomExp[lambda],nmax];
ServiceTime = Table[RandomExp[mu],nmax];

Arrivals = AcumSeries[InterArrivalsTime];

sequenceNumbers = Table[n,{n,1,Length[Arrivals],1}];

paquetesLlegados = MapThread[({#1,9600*#2,#3,0,0})&,{Arrivals,ServiceTime,sequenceNumbers}];

lstPck = FifoPacketTxSW[paquetesLlegados,p];
SetIniParDraw[tp,tack];
Manipulate [ Show [ DrawWin[origin,ww,8],Map[DrawPacketTx[#]&, SelectPacketInWin[lstPck]]],{origin,0,100-ww},{ww,0.1,50}]


ThroughputTeorico = (1-p)/(a*ti)

AcumVal=0;
AcumVal2=0;

ThroughputReal = Module[{n,lastThroughput},n=0;lastThroughput=0;(If [#[[4]]==0,n++;lastThroughput=n/(#[[1]]+#[[2]]/9600+2*tp+tack),lastThroughput])&/@lstPck];
ListPlot[ThroughputReal] (*Para que tienda a algo, utilizar lambdas altas. Tampoco parece que tienda mucho, de todas formas*)
(AcumVal+=#[[2]]/9600+2*tp+tack; If [#[[4]]==0,AcumVal2+=1])&/@lstPck;
ThroughputReal2=AcumVal2/(AcumVal) (*Esto funciona incluso con lambdas bajas. Throughput al que tiende*)


Plot[(1-p)/(a*ti),{p,0,1},AxesLabel -> {p,"Throughput(paquetes/s)"}]
Plot[(1-p)/(a*ti),{a,1,10},AxesLabel -> {a,"Throughput(paquetes/s)"}]
Plot[(1-p)/(a*ti),{ti,0,100},AxesLabel -> {ti,"Throughput(paquetes/s)"}]
(*\[DownQuestion]Rho por qu\[EAcute]? Throughput m\[AAcute]ximo es suponiendo que rho \[Rule] 1 *)


Manipulate[Plot[(1-p)/(a*ti),{ti,0,100}],{p,0,1},{a,1,10}]
(*S\[OAcute]lo de esos 2, no de rho. Gr\[AAcute]ficas te\[OAcute]ricas o pr\[AAcute]cticas? Aqu\[IAcute] son te\[OAcute]ricas*)


FifoPacketTxSW2[lstArr_,p_]:= Module[{n,checkTime,paquetes,lista,numRepeticiones,paquetesIter},n=1;checkTime=lstArr[[1,1]]; paquetes={}; (lista=GetPacketRTxSW2[#,p];paquetesIter = lista[[2]]; numRepeticiones=lista[[1]]; If [checkTime >= #[[1]],
checkTime+=numRepeticiones*#[[2]]/9600+(numRepeticiones-1)*(tack+2*tp), (*Los paquetes correctos ahora s\[OAcute]lo necesitan ti tiempo para transmitirse, tp y tack no influyen.*)
checkTime=#[[1]]+numRepeticiones*#[[2]]/9600+(numRepeticiones-1)*(tack+2*tp)
];
n=numRepeticiones+1;
paquetesIter = (n--;{checkTime-n*#[[2]]/9600-(n-1)*(tack+2*tp),#[[2]],#[[3]],#[[4]],#[[5]]})&/@paquetesIter;
AppendTo[paquetes,paquetesIter];
)&/@lstArr;
Partition[Flatten[paquetes],5]
];

GetPacketRTxSW2[pck_,p_]:= Module[{numRepet,paquetes},
numRepet= Length[NestWhileList[#&,p,#>RandomReal[]&]];
paquetes = Table[{pck[[1]],pck[[2]],pck[[3]],1,n},{n,1,numRepet,1}];
paquetes[[numRepet,4]]=0;
{numRepet,paquetes}
];


InterArrivalsTime = Table[RandomExp[lambda],nmax];
ServiceTime = Table[RandomExp[mu],nmax];

Arrivals = AcumSeries[InterArrivalsTime];

sequenceNumbers = Table[n,{n,1,Length[Arrivals],1}];

paquetesLlegados = MapThread[({#1,9600*#2,#3,0,0})&,{Arrivals,ServiceTime,sequenceNumbers}];

lstPck = FifoPacketTxSW2[paquetesLlegados,p];
SetIniParDraw[tp,tack];
Manipulate [ Show [ DrawWin[origin,ww,8],Map[DrawPacketTx[#]&, SelectPacketInWin[lstPck]]],{origin,0,100-ww},{ww,0.1,50}]


ThroughputTeorico = (1-p)/(ti*(1+(a-1)*p))
ThroughputReal = Module[{n,lastThroughput},n=0;lastThroughput=0;(If [#[[4]]==0,n++;lastThroughput=n/(#[[1]]+#[[2]]/9600),lastThroughput])&/@lstPck]
ListPlot[ThroughputReal]
AcumVal=0;
AcumVal2=0;
(If [#[[4]]==0,AcumVal2+=1;AcumVal+=#[[2]]/9600,AcumVal+=#[[2]]/9600+2*tp+tack])&/@lstPck;
ThroughputReal2=AcumVal2/(AcumVal) (*Esto funciona incluso con lambdas bajas*)


Plot[(1-p)/(ti*(1+(a-1)*p)),{p,0,1},AxesLabel -> {p,"Throughput(paquetes/s)"}]
Plot[(1-p)/(ti*(1+(a-1)*p)),{a,1,10},AxesLabel -> {a,"Throughput(paquetes/s)"}]
Plot[(1-p)/(ti*(1+(a-1)*p)),{ti,0,100},AxesLabel -> {ti,"Throughput(paquetes/s)"}]


Manipulate[Plot[(1-p)/(ti*(1+(a-1)*p)),{ti,0,100}],{p,0,1},{a,1,10}]

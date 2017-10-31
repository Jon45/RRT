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

tp=0.5;
tack=0.1;

InterArrivalsTime = Table[RandomExp[lambda],nmax];

ServiceTime = Table[RandomExp[mu],nmax];

AcumSeries[listInterArrivalsTime_]:= Module[{AcumVal=0},AcumVal+=#&/@listInterArrivalsTime];
Arrivals = AcumSeries[InterArrivalsTime];

FifoSchedulling [arrivals_,service_]:=Module[{n,checkTime},n=1;checkTime=arrivals[[1]]; (If [checkTime >= #,checkTime+=service[[n++]],checkTime=#+service[[n++]]])&/@arrivals];
Departures = FifoSchedulling [Arrivals,ServiceTime];

injTimes = Departures - ServiceTime;
(*sequenceNumbers = Table[n,{n,1,9,1}];
lstPck = Table[{2+n*10,10,n+1,0,0},{n,0,9}]*)

sequenceNumbers = Table[n,{n,1,Length[injTimes],1}];
lstPck = MapThread[({#1,9600*#2,#3,0,0})&,{injTimes,ServiceTime,sequenceNumbers}];


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
];

FifoPacketTxSW[lstArr_,p_]:= Module[{n,checkTime,paquetes,lista,numRepeticiones,paquetesIter},n=1;checkTime=lstArr[[1,1]]; paquetes={}; (lista=GetPacketRTxSW[#,p];paquetesIter = lista[[2]]; numRepeticiones=lista[[1]]; If [checkTime >= #[[1]],
checkTime+=numRepeticiones*(#[[2]]/9600+tack+2*tp),
checkTime=#[[1]]+numRepeticiones*(#[[2]]/9600+tack+2*tp)
];
n=numRepeticiones+1;
paquetesIter = (n--;{checkTime-n*(#[[2]]/9600+tack+2*tp),#[[2]],#[[3]],#[[4]],#[[5]]})&/@paquetesIter;
AppendTo[paquetes,paquetesIter];
)&/@lstArr;
Partition[Flatten[paquetes],5]
];

GetPacketRTxSW[pck_,p_]:= Module[{numRepet,paquetes},
numRepet= Length[NestWhileList[#&,p,#>RandomReal[]&]];
paquetes = Table[{pck[[1]],pck[[2]],pck[[3]],1,n},{n,1,numRepet,1}];
paquetes[[numRepet,4]]=0;
{numRepet,paquetes}
];


nmax=100;
lambda=1;
u=2;
p=0.5;
ti=1/u;
a=(ti + 2*tp + tack)*u;

InterArrivalsTime = Table[RandomExp[lambda],nmax];
ServiceTime = Table[RandomExp[mu],nmax];

Arrivals = AcumSeries[InterArrivalsTime];

sequenceNumbers = Table[n,{n,1,Length[Arrivals],1}];

paquetesLlegados = MapThread[({#1,9600*#2,#3,0,0})&,{Arrivals,ServiceTime,sequenceNumbers}];

lstPck = FifoPacketTxSW[paquetesLlegados,p];
SetIniParDraw[tp,tack];
Manipulate [ Show [ DrawWin[origin,ww,8],Map[DrawPacketTx[#]&, SelectPacketInWin[lstPck]]],{origin,0,100-ww},{ww,0.1,50}]

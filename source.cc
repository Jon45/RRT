#include <stdio.h>
#include <string.h>
#include <omnetpp.h>
#include "paquete_m.h"
using namespace omnetpp;

class Source: public cSimpleModule
{
    private:
        paquete * nuevoPqt;
        simtime_t startTime;
        int seq;
        int numPaquetesEnviados;
        int totalPaquetes;
    public:
        virtual ~Source();
    protected:

        virtual void handleMessage(cMessage *msg) override;
        virtual paquete * generaPaquete();
        virtual void initialize() override;

};

Define_Module(Source);

Source::~Source(){
    cancelAndDelete(nuevoPqt);
}
void Source::initialize(){
    startTime=(simtime_t) par("interArrivalsTime");
    nuevoPqt = new paquete();
    scheduleAt(startTime, nuevoPqt);
    seq=0;
    numPaquetesEnviados=0;
    totalPaquetes=(int)par("n_paquetes");
}
void Source::handleMessage(cMessage * msg){
    paquete *pqt = generaPaquete();
    send(pqt,"out");
    numPaquetesEnviados++;
    if (numPaquetesEnviados < totalPaquetes)
    {
        scheduleAt(simTime()+(simtime_t) par("interArrivalsTime"),nuevoPqt); //Se podría hacer tiempo entre llegadas aleatorio con exponential()
    }
}

paquete * Source::generaPaquete(){
    char nombrePaquete[15];
    sprintf(nombrePaquete,"msg-%d",seq++);
    paquete *msg = new paquete(nombrePaquete,0);
    msg -> setSequenceNumber(seq);
    msg -> setBitLength((int)par("packet_length")); //Se podría hacer tamaño de paquete aleatorio
    return msg;
}

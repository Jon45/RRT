#include <string.h>
#include <omnetpp.h>
#include <paquete_m.h>
#include <timeoutMessage_m.h>
#include <algorithm>

using namespace omnetpp;

class gobackn_tx : public cSimpleModule
{
  private :
    int numPaquete;
    int transmitted_packets;
    cLongHistogram throughputStats;
    cOutVector throughputVector;
    struct arrivals
    {
        double llegadas;
        paquete *packet;
    };
    arrivals *arr;
    cMessage **timeoutEvents;
    cMessage *sendEvent;
    simtime_t timeout;
  protected:
    virtual void initialize() override;
    virtual void handleMessage(cMessage *msg) override;
    virtual void finish() override;
};

Define_Module(gobackn_tx);

void gobackn_tx::initialize()
{
    timeout=par("timeout");
    sendEvent = new cMessage("sendEvent");
    arr = new arrivals[(int)par("n_paquetes")];
    for(int i=0;i<(int) par("n_paquetes");i++)
    {
        arr[i].llegadas = (double) par("interArrivalsTime");
        if (i!=0)
        {
            arr[i].llegadas+=arr[i-1].llegadas;
        }
        paquete *msg = new paquete("mensaje ");
        msg->setByteLength((int)par("packet_length"));
        msg -> setSequenceNumber(i);
        arr[i].packet = msg;
    }

    send(arr[0].packet -> dup(), "gate$o");
    numPaquete=0;
    myTimeoutMessage *timeoutEvent = new myTimeoutMessage();
    scheduleAt(simTime()+timeout, timeoutEvent);
    transmitted_packets=0;

    throughputStats.setName("throughputStats");
    throughputStats.setRangeAutoUpper(0, 10, 1.5);
    throughputVector.setName("throughput");
}

void gobackn_tx::handleMessage(cMessage *msg)
{
    if (strcmp(msg->getName(),"timeoutEvent") == 0)
    {
        EV << "Timeout expired, reseting packet number";
        //cancelEvent(timeoutEvent); // Cancelar múltiples timeouts
        paquete *pack = check_and_cast<paquete *>(msg);
        numPaquete=pack->getSequenceNumber()-1;
        //Igual marcar para que siguiente recepción de ack se considere inválida
    }
    else if (msg == sendEvent)
    {
        EV << "Timers cancelled.\n";

        numPaquete++;
        if (numPaquete <(int)par("n_paquetes"))
        {
            send(arr[numPaquete].packet -> dup(), "gate$o");
            //setTimeoutEvent
            //scheduleAt(simTime()+timeout, timeoutEvent);
            scheduleAt(simTime()+timeout, sendEvent); //cambiar tiempo
            transmitted_packets++;
        }
        delete msg;
    }
    else
    {
        if (uniform(0,1)<par("p_ack").doubleValue())
        {
            EV << "\"Losing\" ack.\n";
            bubble("ack lost");
        }

        else
        {
            //Cancelar timeout de ese paquete
        }
    }
    double throughput = numPaquete/simTime();
    throughputVector.record(throughput);
    throughputStats.collect(throughput);
}

void gobackn_tx::finish()
{
    double package_error_rate = (1-(double)par("n_paquetes")/(double)transmitted_packets);
    EV << "package_error_rate: " << package_error_rate << endl;
    EV << "throughput: " << par("n_paquetes")/simTime() << endl;

    throughputStats.recordAs("Throughput");
}

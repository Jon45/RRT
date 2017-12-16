#include <string.h>
#include <omnetpp.h>
#include <paquete_m.h>
#include <timeoutMessage_m.h>
#include <algorithm>
#include <functional>

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
    cMessage *sendEvent;
    simtime_t timeout;
    std::map <int,myTimeoutMessage *>timeoutEvents;
    cChannel * txChannel;
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

    numPaquete=0;
    myTimeoutMessage *timeoutEvent = new myTimeoutMessage();
    timeoutEvent->setSequenceNumber(numPaquete);
    timeoutEvent->setName("timeoutEvent");
    timeoutEvents.insert({numPaquete, timeoutEvent});
    transmitted_packets=0;
    scheduleAt(arr[numPaquete].llegadas, sendEvent); //cambiar tiempo

    throughputStats.setName("throughputStats");
    throughputStats.setRangeAutoUpper(0, 10, 1.5);
    throughputVector.setName("throughput");

    txChannel = gate("gate$o")->getTransmissionChannel();
}

void gobackn_tx::handleMessage(cMessage *msg)
{
    if (strcmp(msg->getName(),"timeoutEvent") == 0)
    {
        EV << "Timeout expired" << endl;
        EV << "Timers cancelled.\n" << endl;
        std::map<int,myTimeoutMessage *>::iterator it;
        for (it=timeoutEvents.begin();it!=timeoutEvents.end();it++)
        {
            cancelEvent(it->second);
        }
        timeoutEvents.erase(timeoutEvents.begin(),timeoutEvents.end());
        myTimeoutMessage *timeout_rec = check_and_cast<myTimeoutMessage *>(msg);
        numPaquete=timeout_rec->getSequenceNumber()-1;
        EV << "Reset packet number to: " << numPaquete << endl;
        //Igual marcar para que siguiente recepción de ack se considere inválida
    }
    else if (msg == sendEvent)
    {
        numPaquete++;
        if (numPaquete <(int)par("n_paquetes"))
        {
            EV << "Sending packet with sequence number: " << numPaquete << endl;
            send(arr[numPaquete].packet -> dup(), "gate$o");
            myTimeoutMessage *timeoutEvent = new myTimeoutMessage();
            timeoutEvent->setSequenceNumber(numPaquete);
            timeoutEvent->setName("timeoutEvent");
            scheduleAt(simTime()+timeout, timeoutEvent);
            timeoutEvents.insert({numPaquete, timeoutEvent});
            double time = std::max(txChannel->getTransmissionFinishTime().dbl(),arr[numPaquete].llegadas);
            scheduleAt(time, sendEvent);
            transmitted_packets++;
        }
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
            EV << "Ack received";
            paquete *pack = check_and_cast<paquete *>(msg);
            std::map<int,myTimeoutMessage *>::iterator it = timeoutEvents.find(pack->getSequenceNumber());
            if (it != timeoutEvents.end())
            {
                cancelEvent(it -> second);
                timeoutEvents.erase (it);
            }
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

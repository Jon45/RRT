#include <string.h>
#include <omnetpp.h>
#include <paquete_m.h>

using namespace omnetpp;

class stop_wait_tx : public cSimpleModule
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
    cMessage *timeoutEvent;
    simtime_t timeout;
  protected:
    virtual void initialize() override;
    virtual void handleMessage(cMessage *msg) override;
    virtual void finish() override;
};

Define_Module(stop_wait_tx);

void stop_wait_tx::initialize()
{
    timeout=par("timeout");
    timeoutEvent = new cMessage("timeoutEvent");
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
    scheduleAt(simTime()+timeout, timeoutEvent);
    transmitted_packets=0;

    throughputStats.setName("throughputStats");
    throughputStats.setRangeAutoUpper(0, 10, 1.5);
    throughputVector.setName("throughput");
}

void stop_wait_tx::handleMessage(cMessage *msg)
{
    if (msg == timeoutEvent)
    {
        EV << "Timeout expired, resending message and restarting timer\n";
        send(arr[numPaquete].packet -> dup(), "gate$o");
        scheduleAt(simTime()+timeout, timeoutEvent);
        transmitted_packets++;
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
            EV << "Timer cancelled.\n";
            cancelEvent(timeoutEvent);

            numPaquete++;
            if (numPaquete <(int)par("n_paquetes"))
            {
                send(arr[numPaquete].packet -> dup(), "gate$o");
                scheduleAt(simTime()+timeout, timeoutEvent);
                transmitted_packets++;
            }
        }
        delete msg;
    }
    double throughput = numPaquete/simTime();
    throughputVector.record(throughput);
    throughputStats.collect(throughput);
}

void stop_wait_tx::finish()
{
    double package_error_rate = (1-(double)par("n_paquetes")/(double)transmitted_packets);
    EV << "package_error_rate: " << package_error_rate << endl;
    EV << "throughput: " << par("n_paquetes")/simTime() << endl;

    throughputStats.recordAs("Throughput");
}

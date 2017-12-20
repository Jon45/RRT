#include <string.h>
#include <omnetpp.h>
#include <paquete_m.h>
#include <algorithm>

using namespace omnetpp;

class stop_wait_tx : public cSimpleModule
{
  private :
    int numPaquete;
    int transmitted_packets;
    cLongHistogram throughputStats;
    cOutVector throughputVector;
    paquete *currentMessage;
    cMessage *timeoutEvent;
    simtime_t timeout;
    cQueue *txQueue;
    cChannel * txChannel;
    enum state {idle=0,active=1};
    state estado;
  protected:
    virtual void initialize() override;
    virtual void handleMessage(cMessage *msg) override;
    virtual void finish() override;
    virtual void sendNewPacket();
    virtual void sendPacket();
};

Define_Module(stop_wait_tx);

void stop_wait_tx::initialize()
{
    timeout=par("timeout");
    timeoutEvent = new cMessage("timeoutEvent");
    txQueue = new cQueue();
    numPaquete=0;
    transmitted_packets=0;
    estado=idle;
    txChannel = gate("gate$o")->getTransmissionChannel();

    throughputStats.setName("throughputStats");
    throughputStats.setRangeAutoUpper(0, 10, 1.5);
    throughputVector.setName("throughput");
}

void stop_wait_tx::handleMessage(cMessage *msg)
{
    if (msg->isSelfMessage())
    {
        if (msg == timeoutEvent)
        {
            EV << "Timeout expired, resending message and restarting timer\n";
            send(currentMessage->dup(), "gate$o");
            scheduleAt(txChannel->getTransmissionFinishTime()+timeout, timeoutEvent);
            transmitted_packets++;
        }
    }

    else
    {
        if (strcmp(msg->getArrivalGate()->getFullName(),"inPaquete") == 0)
            {
                if (estado == idle)
                {
                    currentMessage = check_and_cast <paquete *>(msg);
                    estado=active;
                    sendPacket();
                }

                else
                {
                    txQueue->insert(msg);
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
                    EV << "Timer cancelled.\n";
                    cancelEvent(timeoutEvent);

                    numPaquete++;
                    sendNewPacket();
                }
                delete msg;
            }
    }
        double throughput = numPaquete/simTime();
        throughputVector.record(throughput);
        throughputStats.collect(throughput);
}


void stop_wait_tx::finish()
{
    double package_error_rate = (1-(double)numPaquete/(double)transmitted_packets);
    EV << "package_error_rate: " << package_error_rate << endl;
    EV << "throughput: " << numPaquete/simTime() << endl;

    throughputStats.recordAs("Throughput");
}

void stop_wait_tx::sendNewPacket()
{
    if (txQueue->isEmpty() == false)
    {
        currentMessage = check_and_cast<paquete *>(txQueue->pop());
        sendPacket();
    }

    else
    {
        estado = idle;
    }
}

void stop_wait_tx::sendPacket()
{
    send(currentMessage -> dup(), "gate$o");
    scheduleAt(txChannel->getTransmissionFinishTime()+timeout, timeoutEvent);
    transmitted_packets++;
}

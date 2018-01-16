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
        simtime_t llegadas;
        paquete *packet;
    };
    cMessage *sendEvent;
    simtime_t timeout;
    std::map <int,myTimeoutMessage *>timeoutEvents;
    cChannel *txChannel;
    enum state {idle=0,active=1};
    state estado;
    cQueue *txQueue;
    paquete *currentMessage;
    std::vector<paquete*> pendingPackets;
    std::vector<int> receivedAcks;
    int id = 0;

  protected:
    virtual void initialize() override;
    virtual void handleMessage(cMessage *msg) override;
    virtual void finish() override;
    virtual void sendNewPacket();
    virtual void sendPacket();
    virtual void onTimeoutEvent(cMessage *msg);
    virtual void cancelTimeoutEvents();
    virtual void onSendEvent(cMessage *msg);
    virtual void insertInPendingPackets();
    virtual void insertReceivedAck(int ack);
    virtual void checkAndRemoveFromVectors();
    virtual void resetQueue();
};

Define_Module(gobackn_tx);

void gobackn_tx::initialize()
{
    timeout=par("timeout");
    sendEvent = new cMessage("sendEvent");

    numPaquete=0;
    transmitted_packets=0;

    throughputStats.setName("throughputStats");
    throughputStats.setRangeAutoUpper(0, 10, 1.5);
    throughputVector.setName("throughput");

    txChannel = gate("gate$o")->getTransmissionChannel();
    estado = idle;
    txQueue = new cQueue();
}

void gobackn_tx::handleMessage(cMessage *msg)
{
    if (msg -> isSelfMessage())
    {
        if (strcmp(msg->getName(),"timeoutEvent") == 0)
        {
            onTimeoutEvent(msg);
        }
        else if (msg == sendEvent)
        {
            onSendEvent(msg);
        }
    }

    else if (strcmp(msg->getArrivalGate()->getFullName(),"inPaquete") == 0)
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
            paquete *pack = check_and_cast<paquete *>(msg);
            if (pack->getId() == id)
            {
                EV << "Ack received: " << pack->getSequenceNumber();
                std::map<int,myTimeoutMessage *>::iterator it = timeoutEvents.find(pack->getSequenceNumber());
                if (it != timeoutEvents.end())
                {
                    cancelEvent(it -> second);
                    timeoutEvents.erase (it);
                }
                insertReceivedAck(pack->getSequenceNumber());
                checkAndRemoveFromVectors();
            }
        }
    }
    double throughput = numPaquete/simTime();
    throughputVector.record(throughput);
    throughputStats.collect(throughput);
}

void gobackn_tx::finish()
{
    double package_error_rate = (1-(double)(numPaquete)/(double)transmitted_packets);
    EV << "package_error_rate: " << package_error_rate << endl;
    EV << "throughput: " << numPaquete/simTime() << endl;

    throughputStats.recordAs("Throughput");
}

void gobackn_tx::sendNewPacket()
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

void gobackn_tx::sendPacket()
{
    EV << "Sending packet with sequence number: " << currentMessage -> getSequenceNumber() << endl;
    numPaquete++;
    currentMessage->setId(id);
    send(currentMessage -> dup(), "gate$o");
    transmitted_packets++;
    myTimeoutMessage *timeoutEvent = new myTimeoutMessage();
    timeoutEvent->setSequenceNumber(currentMessage -> getSequenceNumber());
    timeoutEvent->setName("timeoutEvent");
    timeoutEvents.insert({currentMessage -> getSequenceNumber(), timeoutEvent});
    scheduleAt(txChannel->getTransmissionFinishTime()+timeout, timeoutEvent);
    scheduleAt(txChannel->getTransmissionFinishTime(), sendEvent);
    insertInPendingPackets();
}

void gobackn_tx::onTimeoutEvent(cMessage *msg)
{
    EV << "Timeout expired" << endl;
    EV << "Timers cancelled.\n" << endl;
    cancelTimeoutEvents();
    myTimeoutMessage *timeout_rec = check_and_cast<myTimeoutMessage *>(msg);
    numPaquete=timeout_rec->getSequenceNumber();
    EV << "Reset packet number to: " << numPaquete << endl;
    receivedAcks.erase(receivedAcks.begin(),receivedAcks.end());
    resetQueue();
    cancelEvent(sendEvent);
    txChannel->forceTransmissionFinishTime(simTime());
    scheduleAt(simTime(), sendEvent);
    id++;
}

void gobackn_tx::cancelTimeoutEvents()
{
    std::map<int,myTimeoutMessage *>::iterator it;
    for (it=timeoutEvents.begin();it!=timeoutEvents.end();it++)
    {
        cancelEvent(it->second);
    }
    timeoutEvents.erase(timeoutEvents.begin(),timeoutEvents.end());
}

void gobackn_tx::onSendEvent(cMessage *msg)
{
    sendNewPacket();
}

void gobackn_tx::insertInPendingPackets()
{
    if (pendingPackets.size() == 0)
    {
        std::vector<paquete*>::iterator it = pendingPackets.begin();
        pendingPackets.insert(it,currentMessage);
    }
    else
    {
        std::vector<paquete*>::iterator it;
        for (it = pendingPackets.begin(); it != pendingPackets.end(); ++it)
        {
            paquete *pack = *it;
            if (pack ->getSequenceNumber() > currentMessage->getSequenceNumber())
            {
                pendingPackets.insert(it, currentMessage);
                return;
            }
        }
        if (it == pendingPackets.end())
        {
            pendingPackets.insert(it,currentMessage);
        }
    }
}

void gobackn_tx::insertReceivedAck(int ack)
{
    if (receivedAcks.size() == 0)
    {
        std::vector<int>::iterator it = receivedAcks.begin();
        receivedAcks.insert(it,ack);
    }
    else
    {
        std::vector<int>::iterator it;
        for ( it = receivedAcks.begin(); it != receivedAcks.end(); ++it)
        {
            int ackIterated = *it;
            if (ackIterated == ack)
            {
                return;
            }
            else if (ackIterated > ack)
            {
                receivedAcks.insert(it, ack);
                return;
            }
        }
        if (it == receivedAcks.end())
        {
            receivedAcks.insert(it,ack);
        }
    }
}

void gobackn_tx::checkAndRemoveFromVectors()
{
    while (receivedAcks.size()!=0 && pendingPackets.size()!=0 && pendingPackets.at(0)->getSequenceNumber() == receivedAcks.at(0))
    {
        EV << "Eliminado paquete con numero de secuencia " << pendingPackets.at(0)->getSequenceNumber();
        pendingPackets.erase(pendingPackets.begin());
        receivedAcks.erase(receivedAcks.begin());
    }
}

void gobackn_tx::resetQueue()
{
    for (std::vector<paquete*>::reverse_iterator it = pendingPackets.rbegin(); it != pendingPackets.rend(); ++it)
    {
        paquete *pack = *it;
        if (txQueue->isEmpty() == false)
        {
            txQueue->insertBefore(txQueue->front(), pack);
        }
        else
        {
            txQueue->insert(pack);
        }
    }
    estado = active;
    pendingPackets.erase(pendingPackets.begin(),pendingPackets.end());
    receivedAcks.erase(receivedAcks.begin(),receivedAcks.end());
}


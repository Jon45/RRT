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
    if (msg->isSelfMessage()) // Si te llega un automensaje
    {
        if (msg == timeoutEvent) // Se ha producido timeout
        {
            EV << "Timeout expired, resending message and restarting timer\n";
            send(currentMessage->dup(), "gate$o"); // Se reenvía el mensaje que ha fallado
            scheduleAt(txChannel->getTransmissionFinishTime()+timeout, timeoutEvent); // Armar el timeout correspondiente. El timeout se cuenta desde que se termina el paquete
            transmitted_packets++;
        }
    }

    else
    {
        if (strcmp(msg->getArrivalGate()->getFullName(),"inPaquete") == 0) //Si el paquete me ha llegado por la interfaz inPaquete
            {
                if (estado == idle) // Si no se está enviando ningún paquete
                {
                    //Se envia directamente
                    currentMessage = check_and_cast <paquete *>(msg);
                    estado=active;
                    sendPacket();
                }

                else
                {
                    //Se encola para enviar más adelante
                    txQueue->insert(msg);
                }
            }

            else
            {
                if (uniform(0,1)<par("p_ack").doubleValue()) //Pierde ack con probabilidad p_ack
                {
                    EV << "\"Losing\" ack.\n";
                    bubble("ack lost");
                }

                else
                {
                    //Ha recibido ack correctamente
                    EV << "Timer cancelled.\n";
                    cancelEvent(timeoutEvent); //Como se ha recibido confirmación, se cancela el timeout del paquete

                    numPaquete++; //Se incrementa para enviar el siguiente paquete
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
    if (txQueue->isEmpty() == false) // Si la cola no está vacia
    {
        //Se saca paquete de la cola y se envía
        currentMessage = check_and_cast<paquete *>(txQueue->pop());
        sendPacket();
    }

    else // Si la cola está vacia
    {
        //No hay nada que enviar. Se pasa a estado idle.
        estado = idle;
    }
}

void stop_wait_tx::sendPacket()
{
    //Se envia el mensaje actual.
    send(currentMessage -> dup(), "gate$o");
    scheduleAt(txChannel->getTransmissionFinishTime()+timeout, timeoutEvent);
    transmitted_packets++;
}

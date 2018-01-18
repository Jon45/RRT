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
        if (strcmp(msg->getName(),"timeoutEvent") == 0) // si el mensaje es un evento de timeout
        {
            onTimeoutEvent(msg);
        }
        else if (msg == sendEvent) // si el mensaje es un evento de enviar nuevo paquete
        {
            onSendEvent(msg);
        }
    }

    else if (strcmp(msg->getArrivalGate()->getFullName(),"inPaquete") == 0) // Si me llega algo por inpaquete
    {
        if (estado == idle) //Si no está enviando nada
        {
            //Lo envio directamente
            currentMessage = check_and_cast <paquete *>(msg);
            estado=active;
            sendPacket();
        }

        else
        {
            //Lo encolo
            txQueue->insert(msg);
        }
    }
    else
    {
        if (uniform(0,1)<par("p_ack").doubleValue()) //Pierdo ack con probabilidad p_ack
        {
            EV << "\"Losing\" ack.\n";
            bubble("ack lost");
        }

        else
        {
            paquete *pack = check_and_cast<paquete *>(msg);
            if (pack->getId() == id) //Solo hace caso al paquete si su id es la id actual. Esto me permite no hacer caso a acks provenientes de transmisiones producidas antes del timeout
                //Basicamente, porque modela mejor el comportamiento que se definía, va a ser más parecido el resultado al teórico de esta manera
            {
                EV << "Ack received: " << pack->getSequenceNumber();
                std::map<int,myTimeoutMessage *>::iterator it = timeoutEvents.find(pack->getSequenceNumber()); // Como se ha recibido un ack, cancelo el evento de timeout correspondiente
                //a ese ack
                if (it != timeoutEvents.end())
                {
                    cancelEvent(it -> second);
                    timeoutEvents.erase (it);
                }
                insertReceivedAck(pack->getSequenceNumber()); //Inserto el ack en la lista de acks recibidos.
                checkAndRemoveFromVectors(); //Compruebo si hay que eliminar paquetes pendientes y los elimino si es necesario
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
        //Si la cola está vacia, mando el mensaje actual
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
    timeoutEvent->setSequenceNumber(currentMessage -> getSequenceNumber()); // Para distinguir que timeout corresponde a que paquete
    timeoutEvent->setName("timeoutEvent");
    timeoutEvents.insert({currentMessage -> getSequenceNumber(), timeoutEvent}); //Inserto el paquete en el mapa
    scheduleAt(txChannel->getTransmissionFinishTime()+timeout, timeoutEvent); //El timeout se empieza a contar desde el final de la transmisión del paquete
    scheduleAt(txChannel->getTransmissionFinishTime(), sendEvent); //Cuando se termina de enviar un paquete, se empieza a mandar el paquete
    insertInPendingPackets(); //Se inserta en los paquetes pendientes de confirmar
}

void gobackn_tx::onTimeoutEvent(cMessage *msg)
{
    EV << "Timeout expired" << endl;
    EV << "Timers cancelled.\n" << endl;
    cancelTimeoutEvents(); //Ha ocurrido un timeout, el resto ya no importan. Los cancelamos.
    myTimeoutMessage *timeout_rec = check_and_cast<myTimeoutMessage *>(msg);
    numPaquete=timeout_rec->getSequenceNumber(); // Reseteamos el número de paquete al del paquete que ha fallado
    EV << "Reset packet number to: " << numPaquete << endl;
    receivedAcks.erase(receivedAcks.begin(),receivedAcks.end()); //La lista de acks recibidos se resetea. Se eliminan todos los elementos.
    resetQueue(); //Se insertan los paquetes pendientes de nuevo en la cola de transmisión
    cancelEvent(sendEvent); // Para que se empieze a enviar directamente después del timeout (para ajustarse al comportamiento teórico), se cancela el sendEvent ...
    txChannel->forceTransmissionFinishTime(simTime()); //Y se fuerza al canal a que termine de transmitir el mensaje
    scheduleAt(simTime(), sendEvent); //Se vuelve a armar el sendEvent
    id++; // Se aumenta el id para distinguir acks anteriores al timeout que falten por llegar
}

void gobackn_tx::cancelTimeoutEvents()
{
    //Recorre el mapa, cancela todos los eventos, y resetea el mapa, eliminando todos los elementos
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
        //Si tiene tamaño 0, se inserta sin más
        std::vector<paquete*>::iterator it = pendingPackets.begin();
        pendingPackets.insert(it,currentMessage);
    }
    else
    {
        //Se inserta de forma ordenada en el vector, en función del número de secuencia. De menor a mayor
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
        //Se inserta el ack recibido sin más
        std::vector<int>::iterator it = receivedAcks.begin();
        receivedAcks.insert(it,ack);
    }
    else
    {
        //Se inserta de forma ordenada en el vector, en función del número de secuencia. De menor a mayor.
        std::vector<int>::iterator it;
        for ( it = receivedAcks.begin(); it != receivedAcks.end(); ++it)
        {
            int ackIterated = *it;
            if (ackIterated == ack) // No puede haber duplicados
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
        // Se eliminan los paquetes en orden. Es decir, si ha llegado ack con número de secuencia 1 y otro con 2, se elimina el paquete y ack recibido con número de secuencia 1 y después el de 2
        //Se para si no se ha recibido ack para el paquete que está al inicio del vector
        EV << "Eliminado paquete con numero de secuencia " << pendingPackets.at(0)->getSequenceNumber();
        pendingPackets.erase(pendingPackets.begin());
        receivedAcks.erase(receivedAcks.begin());
    }
}

void gobackn_tx::resetQueue()
{
    //Se insertan paquetes de nuevo en la cola de transmisión. Se insertan de forma que los paquetes queden en orden en la cola (Que salgan primero los de menor número de secuencia)
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
    estado = active; //Esta función siempre inserta algún paquete en la cola. El estado debe ser active
    pendingPackets.erase(pendingPackets.begin(),pendingPackets.end()); //Reseteamos el vector de paquetes pendientes y el de acks recibidos
    receivedAcks.erase(receivedAcks.begin(),receivedAcks.end());
}


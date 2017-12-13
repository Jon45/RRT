#include <string.h>
#include <omnetpp.h>
#include <paquete_m.h>

using namespace omnetpp;

class stop_wait_rx : public cSimpleModule

{
  protected:
    virtual void initialize() override;
    virtual void handleMessage(cMessage *msg) override;
};

Define_Module(stop_wait_rx);

void stop_wait_rx::initialize()
{

}

void stop_wait_rx::handleMessage(cMessage *msg)
{
    if (uniform(0,1) < par("p_packet").doubleValue())
    {
        EV << "\"Losing\" packet.\n";
        bubble("packet lost");
    }
    else
    {
        paquete *rec_msg = check_and_cast<paquete *>(msg);
        paquete *confirmacion = new paquete("ack");
        confirmacion->setByteLength(0);
        confirmacion->setSequenceNumber(rec_msg -> getSequenceNumber());
        send(confirmacion, "gate$o");
    }
    delete msg;
}

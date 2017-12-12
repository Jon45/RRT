simple stop_wait_rx
{
    parameters:
    	double p_packet = default(0.1);
	gates:
		inout gate;
}

simple stop_wait_tx
{
    parameters:
    	int n_paquetes = default(100);
   		double interArrivalsTime = default(1.0);
    	int packet_length = default(100);
    	double p_ack = default(0.1);
    	double timeout = default(1);
	gates:
		inout gate;
}

network s_w
{
	types:
		channel Channel extends ned.DatarateChannel {
			delay = 100ms;
			datarate = 9600 bps;
		}
	submodules:
	tx: stop_wait_tx;
	rx: stop_wait_rx;
	connections:
	tx.gate <--> Channel <--> rx.gate;
}
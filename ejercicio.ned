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
    	double p_ack = default(0.1);
    	double timeout = default(1);
	gates:
	    input inPaquete;
		inout gate;
}

simple gobackn_rx
{
    parameters:
    	double p_packet = default(0.1);
	gates:
		inout gate;
}

simple gobackn_tx
{
    parameters:
    	double p_ack = default(0.1);
    	double timeout = default(1);
	gates:
	    input inPaquete;
		inout gate;
}

simple Source
{
    parameters:
        double interArrivalsTime = default(1.0);
    	int packet_length = default(100);
    	int n_paquetes = default(100);
    gates:
        output out;
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
	source: Source;
	connections:
	source.out --> tx.inPaquete;
	tx.gate <--> Channel <--> rx.gate;
}

network gobackn
{
	types:
		channel Channel extends ned.DatarateChannel {
			delay = 100ms;
			datarate = 9600 bps;
		}
	submodules:
	tx: gobackn_tx;
	rx: gobackn_rx;
	source: Source;
	connections:
	source.out --> tx.inPaquete;
	tx.gate <--> Channel <--> rx.gate;
}



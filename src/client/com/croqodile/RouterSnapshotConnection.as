package com.croqodile {
    import flash.events.*;
    import flash.utils.ByteArray;
    import flash.utils.Timer;
    import flash.net.Socket;
    import com.senocular.utils.Output;
    import com.croqodile.events.*;
    
    public class RouterSnapshotConnection extends RouterConnection {
	
	protected static const SNAPSHOT_SERVER_HOST:String = "localhost";
	protected static const SNAPSHOT_SERVER_PORT:int = 5002;

	public static const LOGIN_YOU_ARE_FIRST_ACK:String = "LOGIN_YOU_ARE_FIRST_ACK";
	public static const LOGIN_NO_SNAPSHOTS_ACK:String = "LOGIN_NO_SNAPSHOTS_ACK";
	public static const SNAPSHOT_REQUEST:String = "SNAPSHOT_REQUEST";

	//Event types
	public static const NO_SNAPSHOT_AVAILABLE:String = "noSnapshotAvailable";
	public static const NO_SNAPSHOT_NECESSARY:String = "noSnapshotNecessary";
	public static const SNAPSHOT_REQUESTED:String = "snapshotRequested";

	public function RouterSnapshotConnection(config:Object) {
	    config.host = (config.host ? config.host : SNAPSHOT_SERVER_HOST);
	    config.port = (config.port ? config.port : SNAPSHOT_SERVER_PORT);
	    config.socket = (config.socket ? config.socket : new Socket());
	    super(config);
	}	
	
	///////////////////////////
        //    Event Handlers	 //
        ///////////////////////////
	
	override protected function onSentenceReceived(data:String):void {
	    if(this._loggedIn){
		if(data == SNAPSHOT_REQUEST){
		    this.dispatchEvent(new Event(SNAPSHOT_REQUESTED));
		}
		else{
		    this.dispatchEvent(new SnapshotReceivedEvent(data));
		}
	    }
	    else if(data == LOGIN_ACK){
		Output.trace("Received " + LOGIN_ACK + "from snapshot server.");
		this._loggedIn = true;
		this.dispatchEvent(new Event(CONNECTION_READY));
	    }
	    else if(data == LOGIN_NO_SNAPSHOTS_ACK){
		Output.trace("Received " + LOGIN_NO_SNAPSHOTS_ACK);
		this._loggedIn = true;
		this.dispatchEvent(new Event(CONNECTION_READY));
		this.dispatchEvent(new Event(NO_SNAPSHOT_AVAILABLE));
	    }
	    else if(data == LOGIN_YOU_ARE_FIRST_ACK){
		Output.trace("Received " + LOGIN_YOU_ARE_FIRST_ACK);
		this._loggedIn = true;
		this.dispatchEvent(new Event(CONNECTION_READY));
		this.dispatchEvent(new Event(NO_SNAPSHOT_NECESSARY));
	    }
	}

	override protected function onSocketConnectError(event:IOErrorEvent):void {
	    throw new Error("An error occurred while connecting to the Snapshot Server.");
	}
	
    }
}
package com.croqodile {
    import flash.events.*;
    import flash.utils.ByteArray;
    import flash.utils.Timer;
    import flash.net.Socket;
    import com.senocular.utils.Output;
    import com.croqodile.events.*;
    
    public class RouterConnection extends EventDispatcher {
	
	public static const TERMINATOR:Array = [12, 12];
	public static const LOGIN_ACK:String = "LOGIN_ACK";

	public static const CONNECTION_READY:String = "connectionReady";
	public static const CONNECTION_CLOSED:String = "connectionClosed";
	
	protected var _host:String;
	protected var _port:int;
	protected var _socket:Socket;
	protected var _userId:String;
	protected var _input: String = "";
	protected var _loggedIn:Boolean = false;
	
	public function RouterConnection(config:Object) {
	    this._host = config.host;
	    this._port = config.port;
	    this._socket = config.socket;
	    this._socket.addEventListener(Event.CONNECT, onSocketConnect);
	    this._socket.addEventListener(Event.CLOSE, onSocketClose);
	    this._socket.addEventListener(IOErrorEvent.IO_ERROR, onSocketConnectError);
	    this._socket.addEventListener(ProgressEvent.SOCKET_DATA, onSocketData);
	}	
	
	public function ready():Boolean{
	    return (this._socket.connected && this._loggedIn);
	}

	public function connect(userId:String):void {
	    this._userId = userId;
	    if(this._socket.connected) {
		throw new Error("Already connected");
	    }
	    this._socket.connect(this._host, this._port);
	}
	
	public function disconnect():void {
	    this._loggedIn = false;
	    this._socket.close();
	}
	
	public function sendSentence(message:String):void {
	    if(!this._socket.connected) {
		throw new Error("No connection to router; cannot send message.");
	    }
	    var newMsg:ByteArray = new ByteArray();
	    newMsg.writeUTFBytes(message);
	    this._socket.writeBytes(newMsg);
	    this._socket.writeByte(TERMINATOR[0]);
	    this._socket.writeByte(TERMINATOR[1]);
	    this._socket.flush();
	}	
	

	///////////////////////////
        //    Event Handlers	 //
        ///////////////////////////
	
	
	private function onSocketConnect(event:Event):void {
	    this.sendSentence(this._userId); //Send the login
	}
	
	private function onSocketClose(event:Event):void {
	    this._loggedIn = false;
	    this.dispatchEvent(new Event(CONNECTION_CLOSED));
	}
	
	protected function onSocketConnectError(event:IOErrorEvent):void {
	    throw new Error("An error occurred wile connecting.");
	}
	
	protected function onSentenceReceived(data:String):void {
	    if(this._loggedIn){
		this.dispatchEvent(new ExternalMessageEvent(data));
	    }
	    else if(data == LOGIN_ACK){
		Output.trace("Received LOGINOK.");
		this._loggedIn = true;
		this.dispatchEvent(new Event(CONNECTION_READY));
	    }
	}
	
	protected function onSocketData(event:ProgressEvent):void {
	    while ( this._socket.bytesAvailable > 0 ) { 
		var byte: uint = this._socket.readUnsignedByte();
		var next: uint;
		
		if ( byte == TERMINATOR[0] ) {
		    if ( this._socket.bytesAvailable >= 1 ) {
			next = this._socket.readUnsignedByte();
			
			if ( next == TERMINATOR[1] ) {
			    this.onSentenceReceived( this._input );
			    this._input = '';
			}
			else {
			    this._input += String.fromCharCode( byte );
			    this._input += String.fromCharCode( next );
			}
		    }
		    else {
			this._input += String.fromCharCode( byte );
		    }
		}
		else {
		    this._input += String.fromCharCode( byte );
		}
	    }
	}
	
    }
}
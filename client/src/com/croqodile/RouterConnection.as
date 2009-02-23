package com.croqodile {
    import flash.events.*;
    import flash.utils.ByteArray;
    import flash.utils.Timer;
    import flash.net.Socket;
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
			_host = config.host;
			_port = config.port;
			_socket = config.socket;
			_socket.addEventListener(Event.CONNECT, onSocketConnect);
			_socket.addEventListener(Event.CLOSE, onSocketClose);
			_socket.addEventListener(IOErrorEvent.IO_ERROR, onSocketConnectError);
			_socket.addEventListener(ProgressEvent.SOCKET_DATA, onSocketData);
		}	
		
		public function ready():Boolean{
			return (_socket.connected && _loggedIn);
		}

		public function connect(userId:String):void {
			_userId = userId;
			if(_socket.connected) {
				throw new Error("Already connected");
			}
			_socket.connect(_host, _port);
		}
		
		public function disconnect():void {
			_loggedIn = false;
			_socket.close();
		}
		
		public function sendSentence(message:String):void {
			if(!_socket.connected) {
				throw new Error("No connection to router; cannot send message.");
			}
			var newMsg:ByteArray = new ByteArray();
			newMsg.writeUTFBytes(message);
			_socket.writeBytes(newMsg);
			_socket.writeByte(TERMINATOR[0]);
			_socket.writeByte(TERMINATOR[1]);
			_socket.flush();
		}	
		

		///////////////////////////
        //    Event Handlers	 //
        ///////////////////////////
		
		
		private function onSocketConnect(event:Event):void {
			sendSentence(_userId); //Send the login
		}
		
		private function onSocketClose(event:Event):void {
			_loggedIn = false;
			dispatchEvent(new Event(CONNECTION_CLOSED));
		}
		
		protected function onSocketConnectError(event:IOErrorEvent):void {
			throw new Error("An error occurred wile connecting.");
		}
		
		protected function onSentenceReceived(data:String):void {
			if(_loggedIn){
				dispatchEvent(new ExternalMessageEvent(data));
			}
			else if(data == LOGIN_ACK){
				_loggedIn = true;
				dispatchEvent(new Event(CONNECTION_READY));
			}
		}
		
		protected function onSocketData(event:ProgressEvent):void {
			while ( _socket.bytesAvailable > 0 ) { 
				var byte: uint = _socket.readUnsignedByte();
				var next: uint;
				
				if ( byte == TERMINATOR[0] ) {
					if ( _socket.bytesAvailable >= 1 ) {
						next = _socket.readUnsignedByte();
						
						if ( next == TERMINATOR[1] ) {
							onSentenceReceived( _input );
							_input = '';
						}
						else {
							_input += String.fromCharCode( byte );
							_input += String.fromCharCode( next );
						}
					}
					else {
						_input += String.fromCharCode( byte );
					}
				}
				else {
					_input += String.fromCharCode( byte );
				}
			}
		}
		
    }
}
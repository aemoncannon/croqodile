package com.croqodile {
    import flash.events.*;
    import flash.utils.*;
    import flash.net.Socket;
    import com.croqodile.events.*;
    import com.croqodile.util.Log;
    
    public class RouterConnection extends EventDispatcher {
		
		public static const CONNECTION_READY:String = "connectionReady";
		public static const CONNECTION_CLOSED:String = "connectionClosed";
		
		protected var _host:String;
		protected var _port:int;
		protected var _buf:ByteArray;
		protected var _socket:Socket;
		protected var _userId:String;
		protected var _islandId:String;
		protected var _input: String = "";
		protected var _loggedIn:Boolean = false;
		protected var _processor:Function;
		
		public function RouterConnection(config:Object) {
			_buf = new ByteArray();
			_host = config.host;
			_port = config.port;
			_userId = config.userId;
			_islandId = config.islandId;
			_socket = config.socket || new Socket();
			_socket.addEventListener(Event.CONNECT, onSocketConnect);
			_socket.addEventListener(Event.CLOSE, onSocketClose);
			_socket.addEventListener(IOErrorEvent.IO_ERROR, onSocketConnectError);
			_socket.addEventListener(ProgressEvent.SOCKET_DATA, onSocketData);
			_processor = processHTTPHeader;
		}	
		
		public function ready():Boolean{
			return (_socket.connected && _loggedIn);
		}

		public function connect():void {
			if(_socket.connected) {
				throw new Error("Already connected");
			}
			_processor = processHTTPHeader;
			_socket.connect(_host, _port);
		}
		
		public function disconnect():void {
			_loggedIn = false;
			_socket.close();
		}
		
		public function sendMessage(msg:ExternalMessage):void {
			if(!_socket.connected) {
				throw new Error("No connection to router; cannot send message.");
			}
			msg.writeTo(_socket);
			_socket.flush();
		}	
		

		///////////////////////////
        //    Event Handlers	 //
        ///////////////////////////
		
		
		private function onSocketConnect(event:Event):void {
			_socket.writeUTFBytes("GET /join_island?id=" + _islandId + "&clientId=" + _userId + " HTTP/1.0\r\n\r\n");
		}
		
		private function onSocketClose(event:Event):void {
			_loggedIn = false;
			dispatchEvent(new Event(CONNECTION_CLOSED));
		}
		
		protected function onSocketConnectError(event:IOErrorEvent):void {
			throw new Error("An error occurred while connecting to " + _host + ":" + _port + "  " + event + ".");
		}
		
		protected function onSocketData(event:ProgressEvent):void {
			// 			var oldBuf:ByteArray = new ByteArray();
			// 			_buf = new ByteArray();
			// 			oldBuf.readBytes(_buf, 0);

			// TODO: This needs to be more efficient.

			var pos:int = _buf.position;
			_buf.length = _buf.length + _socket.bytesAvailable;
			_socket.readBytes(_buf, pos, _socket.bytesAvailable);
			_buf.position = pos;
			_processor();
		}

		/**
		* Burn through characters of the HTTP request, until we're
		* past the terminator, '\r\n\r\n'.
		* 
		* @return 
		*/		
		protected function processHTTPHeader():void{
			var lf:int = 10;
			var cr:int = 13;
			var mem:Array = new Array(4);
			var i:uint = 0;
			_buf.position = 0;
			while(_buf.position < _buf.length){
				var byte:int = _buf.readByte();
				mem[i] = byte;
				if(mem[0] === cr && mem[1] === lf && mem[2] === cr && mem[3] === lf){
					_loggedIn = true;
					dispatchEvent(new Event(CONNECTION_READY));
					_processor = processMessages;
					trace("Finished HTTP header, processing messages..")
					break;
				}
				i = (i + 1) % mem.length;
			}
		}


		/**
		* Process messages as they arrive.
		* 
		* @return 
		*/		
		protected function processMessages():void{
			var msgs:Array = ExternalMessage.parseAll(_buf);
			for each(var msg:ExternalMessage in msgs){
				trace(msg.toString());
				dispatchEvent(new ExternalMessageEvent(msg));
			}
		}
		
	}
}


package com.croqodile {
    import flash.events.*;
    import flash.utils.ByteArray;
    import flash.utils.Timer;
    import flash.net.Socket;
    import com.croqodile.events.*;
    
    public class RouterMessageConnection extends RouterConnection {
		
		protected static const DEFAULT_ROUTER_HOST:String = "localhost";
		protected static const DEFAULT_ROUTER_PORT:int = 5000;
		
		protected static const POLICY_SERVER_HOST:String = "localhost";

		public function RouterMessageConnection(config:Object) {
			config.host = (config.host ? config.host : DEFAULT_ROUTER_HOST);
			config.port = (config.port ? config.port : DEFAULT_ROUTER_PORT);
			config.socket = (config.socket ? config.socket : new Socket());
			super(config);
		}	
		
		///////////////////////////
        //    Event Handlers	 //
        ///////////////////////////
		
		override protected function onSentenceReceived(data:String):void {
			if(this._loggedIn){
				this.dispatchEvent(new ExternalMessageEvent(data));
			}
			else if(data == LOGIN_ACK){
				this._loggedIn = true;
				this.dispatchEvent(new Event(CONNECTION_READY));
			}
		}

		override protected function onSocketConnectError(event:IOErrorEvent):void {
			throw new Error("An error occurred wile connecting to the router message server.");
		}
		
    }
}
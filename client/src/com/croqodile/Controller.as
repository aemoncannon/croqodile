package com.croqodile {
    import flash.display.MovieClip;
    import flash.system.Security;
    import flash.utils.Timer;
    import flash.events.*;
    import com.croqodile.*;
    import com.croqodile.events.*;
    

    public /*abstract*/ class Controller extends EventDispatcher {
		
		protected static const POLICY_SERVER_HOST:String = "localhost";
		protected static const POLICY_SERVER_PORT:int = 5001;
		
		protected var _messageCon:RouterMessageConnection;
		protected var _island:IslandReplica;
		protected var _userId:String;
		protected var _msgBuffer:Array = [];
		
		protected function genUserId():String {
			return "user" + (new Date()).getTime() + "avatar" + Math.random();
		}
		
		public function userId():String{
			return _userId;
		}
		
		public function island():IslandReplica {
			return _island;
		}
		
		public function signalIslandEvent(event:Event):void{
			dispatchEvent(event);
		}
		
		public function propagateFarSend(msgString:String):void{
			_messageCon.sendSentence(msgString);
		}
		
		public function disconnectFromRouter():void{
			_messageCon.disconnect();
		}
		
		///////////////////////////
        //    Event Handlers	 //
        ///////////////////////////
		
		protected function onRouterConnectionReady(event:Event):void {
			dispatchEvent(new RouterConnectionReadyEvent());
		}
		
		protected function onRouterConnectionClosed(event:Event):void {
			dispatchEvent(new DisconnectedFromRouterEvent());
		}
		
		protected function onMessageFromRouter(e:ExternalMessageEvent):void {
			_island.advanceToExternalMessage(ExternalMessage.fromRouterString(e.msg, _island));
		}
		
    }
}



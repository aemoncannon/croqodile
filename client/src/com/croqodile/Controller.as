package com.croqodile {
    import flash.display.MovieClip;
    import flash.system.Security;
    import flash.utils.Timer;
    import flash.events.*;
    import com.croqodile.*;
    import com.croqodile.events.*;
    

    public class Controller extends EventDispatcher {
		
		protected var _routerCon:RouterConnection;
		protected var _island:IslandReplica;
		protected var _userId:String;
		protected var _msgBuffer:Array = [];

		public function Controller(config:Object):void{
			super();

			_userId = config.userId;
			_island = config.island;
			_island.setController(this);
			
			_messageCon = (config.messageCon ? config.messageCon : new RouterConnection({}));

			_messageCon.addEventListener(RouterConnection.CONNECTION_READY, onRouterConnectionReady);
			_messageCon.addEventListener(RouterConnection.CONNECTION_CLOSED, onRouterConnectionClosed);
			_message.addEventListener(RouterSnapshotConnection.SNAPSHOT_REQUESTED, onSnapshotRequested);
			_messageCon.addEventListener(ExternalMessageEvent.type, onMessageFromRouterBuffered);
			_messageCon.connect(_userId, config.islandId);
		}
		
		public static function genUserId():String {
			return "user" + (new Date()).getTime() + "avatar" + Math.random();
		}
		
		public function get userId():String{
			return _userId;
		}
		
		public function get island():IslandReplica {
			return _island;
		}
		
		public function signalIslandEvent(event:Event):void{
			dispatchEvent(event);
		}
		
		public function propagateFarSend(msg:ExternalMessage):void{
			_messageCon.sendMessage(msg);
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

		protected function onSnapshotRequested(event:Event):void {
			//_snapshotCon.sendSentence(_island.snapshot());
		}
		
		protected function onSnapshotReceived(event:Event, data:ByteArray):void {
			_island.initFromSnapshot(data);
			
			var time:Number = _island.time();
			for each(var m:ExternalMessage in _msgBuffer){
				//Timestamps are guaranteed by router to be 
				//always increasing, never repeating
				if(m.executionTime > time){
					_island.advanceToExternalMessage(m);
				}
			}
			_msgBuffer = [];
			_messageCon.removeEventListener(ExternalMessageEvent.type, onMessageFromRouterBuffered);
			_messageCon.addEventListener(ExternalMessageEvent.type, onMessageFromRouter);
		}
		
		protected function onNoSnapshotAvailable(event:Event):void {
			throw new Error("Could not get a snapshot :(");
		}
		
		protected function onNoSnapshotNecessary(event:Event):void {
			for each(var m:ExternalMessage in _msgBuffer){
				_island.advanceToExternalMessage(m);
			}
			
			_msgBuffer = [];
			
			_messageCon.removeEventListener(ExternalMessageEvent.type, 
				onMessageFromRouterBuffered);
			_messageCon.addEventListener(ExternalMessageEvent.type, 
				onMessageFromRouter);
			
			//We are the first controller to connect, so send the island a sunrise :)
			var self:SnapshottingController = this;
			var readyHandler:Function = function():void{
				self._island.farRef().send("sunrise", []);
				self.onRouterConnectionReady(new Event(RouterConnection.CONNECTION_READY));
			}
			
			//Start waiting on the main message connection..
			_messageCon.addEventListener(RouterConnection.CONNECTION_READY, readyHandler);
			
			//..might have already connected while we were asking for the snapshot, so..
			if(_messageCon.ready()){ readyHandler(); }
			
		}
		
		//Buffer the messages while the snapshot until we have a valid snapshot.
		protected function onMessageFromRouterBuffered(e:ExternalMessageEvent):void {
			_msgBuffer.push(e.msg);
		}
		
    }
}



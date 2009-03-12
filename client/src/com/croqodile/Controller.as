package com.croqodile {
    import flash.display.MovieClip;
    import flash.system.Security;
    import flash.utils.*;
    import flash.events.*;
    import flash.net.URLLoader;
    import flash.net.URLRequest;
    import com.croqodile.*;
    import com.croqodile.events.*;
    import com.croqodile.util.Base64;
    

    public class Controller extends EventDispatcher {
		
		protected var _routerCon:RouterConnection;
		protected var _island:IslandReplica;
		protected var _userId:String;
		protected var _msgBuffer:Array = [];
		protected var _buffering:Boolean = true;
		protected var _host:String;
		protected var _port:int;

		public function Controller(config:Object):void{
			super();
			_userId = config.userId;
			_island = config.island;
			_host = config.host;
			_port = config.port;
			_buffering = true;
			_island.controller = this;
			_routerCon = (config.routerCon ? config.routerCon : new RouterConnection({
						userId: _userId,
						islandId: _island.id,
						host: _host,
						port: _port
					}));
			_routerCon.addEventListener(RouterConnection.CONNECTION_CLOSED, onRouterConnectionClosed);
			_routerCon.addEventListener(ExternalMessageEvent.type, onFirstExternalMessage);
		}

		public function connect():void{
			_routerCon.connect();
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
			_routerCon.sendMessage(msg);
		}
		
		public function disconnectFromRouter():void{
			_routerCon.disconnect();
		}
		
		///////////////////////////
        //    Event Handlers	 //
        ///////////////////////////
		
		protected function onRouterConnectionClosed(event:Event):void {
			dispatchEvent(new DisconnectedFromRouterEvent());
		}

		protected function onFirstExternalMessage(e:ExternalMessageEvent):void {
			_routerCon.removeEventListener(ExternalMessageEvent.type, onFirstExternalMessage);
			_routerCon.addEventListener(ExternalMessageEvent.type, onExternalMessage);
			var msg:ExternalMessage = e.msg;
			if(msg.sequenceNumber == 1){
				// Don't need snapshot
				_buffering = false;
				onExternalMessage(e);
				_island.farRef().send("sunrise", []);
				dispatchEvent(new RouterConnectionReadyEvent());
			}
			else{
				getSnapshot();
				_buffering = true;
				onExternalMessage(e);
			}
		}
		
		protected function onExternalMessage(e:ExternalMessageEvent):void {
			var msg:ExternalMessage = e.msg;
			if(_buffering){
				_msgBuffer.push(msg);
			}
			else if(msg is HeartbeatMessage || msg is ExternalIslandMessage){
				_island.advanceToExternalMessage(msg);
			}
			else if(msg is SnapshotRequestMessage){
				sendSnapshot();
			}
		}

		protected function sendSnapshot():void {
			var loader:URLLoader = new URLLoader();
			var errHandler:Function = function(e:Event):void{
				trace("Error during snapshot upload.");
			};
			var completeHandler:Function = function(e:Event):void{
				trace("Completed snapshot upload.");
			};
			loader.addEventListener(Event.COMPLETE, completeHandler);
            loader.addEventListener(SecurityErrorEvent.SECURITY_ERROR, errHandler);
            loader.addEventListener(IOErrorEvent.IO_ERROR, errHandler);
			var request:URLRequest = new URLRequest(
				"http://" +  _host + ":" + _port + "/send_snapshot?" + 
				"id=" + _island.id + 
				"&clientId=" + _userId
			);
			var data:String = Base64.encodeByteArray(_island.snapshot());
			trace("Sending snapshot of length " + data.length);
			request.data = data;
			request.method = "POST";
			loader.load(request);
		}
		
		protected function getSnapshot():void {
			var loader:URLLoader = new URLLoader();
			var errHandler:Function = function(e:Event):void{
				throw new Error("Could not get a snapshot :(");
			};
			var completeHandler:Function = function(e:Event):void{
				var data:String = String(loader.data);
				trace("Got snapshot of length " + data.length);
				var b:ByteArray = Base64.decodeToByteArray(data);
				installSnapshot(b);
			};
			loader.addEventListener(Event.COMPLETE, completeHandler);
            loader.addEventListener(SecurityErrorEvent.SECURITY_ERROR, errHandler);
            loader.addEventListener(IOErrorEvent.IO_ERROR, errHandler);
			var request:URLRequest = new URLRequest(
				"http://" +  _host + ":" + _port + "/get_snapshot?" + 
				"id=" + _island.id + 
				"&clientId=" + _userId
			);
			request.method = "GET";
			loader.load(request);
		}

		
		protected function installSnapshot(data:ByteArray):void {
			if(_island.time > 0) throw new Error("Can't apply snapshot to non-fresh island replica.");
			_island.initFromSnapshot(data);

			/*
			* island.time is now set from snapshot, and we assume that all messages that
			* 'sortsBefore' island.time have already been applied (via the snapshot).
			*
			* Now we take the messages that arrived before the snapshot and apply those 
			* that do not 'sortsBefore' the new island.time. 
			* That is, messages whose effects are not already part of the snapshot.
			*
			*/
			var time:Number = _island.time;
			for each(var m:ExternalMessage in _msgBuffer){
				if(!m.sortsBeforeTime(time)){
					_island.advanceToExternalMessage(m);
				}
			}
			_msgBuffer = [];
			_buffering = false;
			dispatchEvent(new RouterConnectionReadyEvent());
		}
		
    }
}



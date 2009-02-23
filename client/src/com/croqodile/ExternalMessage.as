package com.croqodile{
    import flash.display.MovieClip;
    import flash.system.Security;
    import flash.utils.Timer;
    import com.croqodile.IslandReplica;
    import com.croqodile.IslandObject;
    import com.croqodile.Controller;
    import com.croqodile.Message;
    import com.croqodile.serialization.json.JSON;
    import flash.events.*;
    
    public class ExternalMessage implements Message{
		
		public static const MESSAGE_SEP:String = "@";
		protected static const HEARTBEAT_MSG_NAME:String = "heartbeat";
		
		protected var _island:IslandReplica;
		protected var _targetGuid:int;
		protected var _args:Array;
		protected var _msg:String;
		protected var _timestamp:Number = 0;
		
		public static function create(timestamp:Number,
			targetGuid:int, 
			msg:String, 
			args:Array, 
			island:IslandReplica):ExternalMessage {
			var newMsg:ExternalMessage = new ExternalMessage();
			newMsg._timestamp = timestamp;
			newMsg._targetGuid = targetGuid;
			newMsg._msg = msg;
			newMsg._args = args;
			newMsg._island = island;
			return newMsg;
		}
		
		public static function fromRouterString(str:String, island:IslandReplica):ExternalMessage {
			var sepIndex:int = str.indexOf(MESSAGE_SEP);
			var timestamp:Number = parseFloat(str.substr(0, sepIndex + 1));
			var msgData:Array = (JSON.decode(str.substr(sepIndex + 1)) as Array);
			var msgName:String = msgData[0];
			
			if(msgName == HEARTBEAT_MSG_NAME){
				return HeartbeatMessage.create(timestamp, island);
			}
			
			var targetGuid:int = msgData[1];
			var args:Array = msgData[2];
			
			return ExternalMessage.create(timestamp, targetGuid, msgName, args, island);
		}
		
		
		//Utility method to create a string suitable to send to the router for propagation
		public static function createRouterString(targetGuid:int, 
			msgName:String,
			args:Array, 
			island:IslandReplica):String{
			return JSON.encode([msgName, targetGuid,  args]);
		}
		
		public function executionTime():Number{
			return _timestamp;
		}
		
		public function sequenceNumber():int{
			return 0;
		}

		public function execute():void{
			var target:IslandObject = IslandObject.byGuid(_targetGuid);
			target[_msg].apply(target, _args);
		}
		
		public function toString():String{
			return "ExternalMessage(" + [_timestamp,
				_targetGuid,
				_msg,
				_args].join(",") + ")";
		}
    }
    
}



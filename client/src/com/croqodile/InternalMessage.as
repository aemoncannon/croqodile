package com.croqodile{
    import flash.system.Security;
    import flash.utils.Timer;
    import com.croqodile.IslandReplica;
    import com.croqodile.IslandObject;
    import com.croqodile.Controller;
    import com.croqodile.Message;
    import flash.events.*;
    
    public class InternalMessage extends AMessage{
		
		private var _targetGuid:int;
		private var _args:Array;
		private var _msg:String;
		private var _timestamp:Number = 0;
		
		public function InternalMessage(
			timestamp:Number,
			target:IslandObject, 
			msg:String, 
			args:Array
		):InternalMessage
		{
			_timestamp = timestamp;
			_targetGuid = target.guid();
			_msg = msg;
			_args = args;
		}

// 		public static function unfreeze(data:Object, island:IslandReplica):InternalMessage{
// 			var newMsg:InternalMessage = new InternalMessage();
// 			newMsg._timestamp = data.timestamp;
// 			newMsg._targetGuid = data.targetGuid;
// 			newMsg._msg = data.msg;
// 			newMsg._args = data.args;
// 			return newMsg;
// 		}

// 		public function freeze():Object{
// 			var data:Object = new Object();
// 			data.timestamp = _timestamp;
// 			data.targetGuid = _targetGuid;
// 			data.msg = _msg;
// 			data.args = _args;
// 			data.sequenceNumber = _sequenceNumber;
// 			return data;
// 		}
		
		public function executionTime():Number{
			return _timestamp;
		}

		public function execute(island:IslandReplica):void{
			var target:IslandObject = island.islandObjectbyGuid(_targetGuid);
			target[_msg].apply(target, _args);
		}
		
		public function toString():String{
			return "InternalMessage(" + [
				_timestamp,
				_targetGuid,
				_msg,
				_args].join(",") + ")";
		}
    }
    
}



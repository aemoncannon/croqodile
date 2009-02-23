package com.croqodile{
    import flash.system.Security;
    import flash.utils.Timer;
    import com.croqodile.IslandReplica;
    import com.croqodile.IslandObject;
    import com.croqodile.Controller;
    import com.croqodile.Message;
    import flash.events.*;
    
    public class InternalMessage implements Message{
		
		private var _island:IslandReplica;
		private var _targetGuid:int;
		private var _args:Array;
		private var _msg:String;
		private var _timestamp:Number = 0;
		private var _sequenceNumber:int = 0;
		
		public static function create(timestamp:Number,
			target:IslandObject, 
			msg:String, 
			args:Array, 
			island:IslandReplica):InternalMessage{
			var newMsg:InternalMessage = new InternalMessage();
			newMsg._timestamp = timestamp;
			newMsg._targetGuid = target.guid();
			newMsg._msg = msg;
			newMsg._args = args;
			newMsg._island = island;
			return newMsg;
		}

		public static function unfreeze(data:Object, island:IslandReplica):InternalMessage{
			var newMsg:InternalMessage = new InternalMessage();
			newMsg._timestamp = data.timestamp;
			newMsg._targetGuid = data.targetGuid;
			newMsg._msg = data.msg;
			newMsg._args = data.args;
			newMsg._sequenceNumber = data.sequenceNumber;
			newMsg._island = island;
			return newMsg;
		}

		public function freeze():Object{
			var data:Object = new Object();
			data.timestamp = _timestamp;
			data.targetGuid = _targetGuid;
			data.msg = _msg;
			data.args = _args;
			data.sequenceNumber = _sequenceNumber;
			return data;
		}
		
		public function executionTime():Number{
			return _timestamp;
		}

		public function sequenceNumber():int{
			return _sequenceNumber;
		}

		public function setSequenceNumber(num:int):void{
			_sequenceNumber = num;
		}

		public function execute():void{
			var target:IslandObject = IslandObject.byGuid(_targetGuid);
			target[_msg].apply(target, _args);
		}
		
		public function toString():String{
			return "InternalMessage(" + [_timestamp,
				_sequenceNumber,
				_targetGuid,
				_msg,
				_args].join(",") + ")";
		}
    }
    
}



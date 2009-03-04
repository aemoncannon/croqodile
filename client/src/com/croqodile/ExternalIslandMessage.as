package com.croqodile{
    import com.croqodile.IslandReplica;
    import com.croqodile.IslandObject;
    import com.croqodile.Message;
    import com.croqodile.serialization.json.JSON;
    import flash.events.*;
    import flash.utils.ByteArray;
    
    public class ExternalIslandMessage extends ExternalMessage{

		protected var _targetGuid:int;
		protected var _args:Array;
		protected var _msg:String;

		override protected function get type():uint{ return MSG_TYPE_NORMAL; }

		public function ExternalIslandMessage(num:Number, timestamp:Number, targetGuid:int, msg:String, args:Array):void{
			super(num, timestamp);
			_targetGuid = targetGuid;
			_msg = msg;
			_args = args;
		}

		public static function fromBytes(num:Number, timestamp:Number, payload:ByteArray):ExternalIslandMessage{
			var src:String = payload.readUTFBytes(payload.length);
			var parts:Array = Array(JSON.decode(src));
			return new ExternalIslandMessage(num, timestamp, parts[0], parts[1], parts[2]);
		}

		public static function forCall(targetGuid:int, msg:String, args:Array):void{
			return new ExternalIslandMessage(0, 0, targetGuid, msg, args);
		}

		override protected function payloadBytes():ByteArray{ 
			var payload:ByteArray = new ByteArray();
			var src:String = JSON.encode([targetGuid, msg, args]);
			payload.writeUTFBytes(src);
			return payload;
		}

		override public function execute(island:IslandReplica):void{
			var target:IslandObject = IslandObject(island.islandObjectByGuid(_targetGuid));
			target[_msg].apply(target, _args);
		}
		
		override public function toString():String{
			return "ExternalIslandMessage(" + [
				_timestamp,
				_targetGuid,
				_msg,
				_args].join(",") + 
			")";
		}
    }
    
}



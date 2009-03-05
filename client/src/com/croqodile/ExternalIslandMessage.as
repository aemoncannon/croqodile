package com.croqodile{
    import com.croqodile.IslandReplica;
    import com.croqodile.IslandObject;
    import com.croqodile.Message;
    import com.croqodile.serialization.json.JSON;
    import flash.events.*;
    import flash.utils.ByteArray;
    
    public class ExternalIslandMessage extends ExternalMessage{
		protected var _targetGuid:String;
		protected var _msg:String;
		protected var _args:Array;

		override protected function get type():uint{ return MSG_TYPE_NORMAL; }

		public function ExternalIslandMessage(num:Number, timestamp:Number, targetGuid:String, msg:String, args:Array):void{
			super(num, timestamp);
			_targetGuid = targetGuid;
			_msg = msg;
			_args = args;
		}

		public static function createFromPayload(num:Number, timestamp:Number, payload:ByteArray):ExternalIslandMessage{
			var src:String = payload.readUTFBytes(payload.length);
			var parts:Array = JSON.decode(src) as Array;
			return new ExternalIslandMessage(num, timestamp, parts[0], parts[1], parts[2]);
		}

		public static function createForCall(targetGuid:String, msg:String, args:Array):ExternalIslandMessage{
			return new ExternalIslandMessage(0, 0, targetGuid, msg, args);
		}

		override protected function payloadBytes():ByteArray{ 
			var payload:ByteArray = new ByteArray();
			var src:String = JSON.encode([_targetGuid, _msg, _args]);
			payload.writeUTFBytes(src);
			return payload;
		}

		override public function execute(island:IslandReplica):void{
			var target:IslandObject = IslandObject(island.islandObjectByGuid(_targetGuid));
			target[_msg].apply(target, _args);
		}

		override public function equals(o:Object):Boolean{
			return (
				super.equals(o) && 
				o is ExternalIslandMessage && 
				_targetGuid == o._targetGuid && 
				_msg == o._msg &&
				JSON.encode(_args) === JSON.encode(o._args)
			);
		}
		
		override public function toString():String{
			return "ExternalIslandMessage(" + [
				_timestamp,
				_targetGuid,
				_msg,
				_args
			].join(",") + ")";
		}
    }
    
}



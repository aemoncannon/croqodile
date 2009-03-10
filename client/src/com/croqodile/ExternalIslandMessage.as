package com.croqodile{
    import com.croqodile.IslandReplica;
    import com.croqodile.IslandObject;
    import com.croqodile.Message;
    import com.croqodile.util.Util;
    import flash.events.*;
    import flash.utils.*;
    
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
			return new ExternalIslandMessage(
				num, 
				timestamp, 
				payload.readUTF(),
				payload.readUTF(),
				payload.readObject() as Array
			);
		}

		public static function createForCall(targetGuid:String, msg:String, args:Array):ExternalIslandMessage{
			return new ExternalIslandMessage(0, 0, targetGuid, msg, args);
		}

		override protected function writePayloadTo(b:IDataOutput):void{ 
			var payload:ByteArray = new ByteArray();
			payload.writeUTF(_targetGuid);
			payload.writeUTF(_msg);
			payload.writeObject(_args);
			b.writeUnsignedInt(payload.length);
			payload.position = 0;
			b.writeBytes(payload);
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
				Util.equal(_args, o._args)
			);
		}
		
		override public function toString():String{
			return "ExternalIslandMessage(" + [
				_sequenceNumber,
				_timestamp,
				_targetGuid,
				_msg,
				"[" + _args + "]",
			].join(",") + ")";
		}
    }
    
}



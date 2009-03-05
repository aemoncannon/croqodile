package com.croqodile{
    import flash.system.Security;
    import flash.utils.Timer;
    import com.croqodile.IslandReplica;
    import com.croqodile.IslandObject;
    import com.croqodile.Controller;
    import com.croqodile.Message;
    import flash.events.*;
    import flash.utils.*;
    
    public class InternalMessage extends AMessage{
		
		private var _targetGuid:String;
		private var _args:Array;
		private var _msg:String;
		
		public function InternalMessage(
			timestamp:Number,
			guid:String, 
			msg:String, 
			args:Array
		):void
		{
			_timestamp = timestamp;
			_targetGuid = guid;
			_msg = msg;
			_args = args;
		}

		public static function readFrom(b:IDataInput):InternalMessage{
			return new InternalMessage(
				b.readDouble(),
				b.readUTF(),
				b.readUTF(),
				b.readObject() as Array
			);
		}

		public function writeTo(b:IDataOutput):void{
			b.writeDouble(_timestamp);
			b.writeUTF(_targetGuid);
			b.writeUTF(_msg);
			b.writeObject(_args);
		}
		
		override public function execute(island:IslandReplica):void{
			var target:IslandObject = island.islandObjectByGuid(_targetGuid);
			target[_msg].apply(target, _args);
		}
		
		override public function toString():String{
			return "InternalMessage(" + [
				_timestamp,
				_targetGuid,
				_msg,
				_args].join(",") + 
			")";
		}
    }
    
}



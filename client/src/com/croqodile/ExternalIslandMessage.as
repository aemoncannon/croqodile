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

		public function ExternalIslandMessage(num:Number, timestamp:Number, content:ByteArray):void{
			super(num, timestamp);
		}

		public static function forInvocation(targetGuid:int, msg:String, args:Array):void{
			var b:ByteArray = new ByteArray();
			b.write

			return new ExternalIslandMessage(0, 0, b);
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



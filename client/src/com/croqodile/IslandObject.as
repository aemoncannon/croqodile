package com.croqodile {
    import flash.events.Event;
    import flash.events.MouseEvent;
    import flash.utils.Dictionary;
    import com.croqodile.*;
    import flash.utils.*;
    
    public class IslandObject extends Obj{
		protected var _island:IslandReplica;
		protected var _guid:String;
		
		public function IslandObject(island:IslandReplica){
			_island = island;
		}

		public function get guid():String{
			return _guid;
		}
		
		public function farRef():FarRef {
			return new FarRef(_guid, _island);
		}
		
		public function futureSend(offset:Number, msg:String, args:Array):void {
			_island.scheduleInternalMessage(new InternalMessage(
					_island.time + offset,
					_guid,
					msg,
					args
				)
			);
		}

		public function readFrom(b:IDataInput):void {
			_guid = _island.internIslandObject(this, b.readUTF());
		}
		
		public function writeTo(b:IDataOutput):void {
			b.writeUTF(_guid);
		}

		override public function equals(o:Object):Boolean { 
			return o is IslandObject && _guid === o.guid;
		}


		protected static function writeBytesForCreateRandom(b:ByteArray, island:IslandReplica, x0:Number, y0:Number, w:Number, h:Number):void{
			b.writeUTF(island.nextGuid());
		}
		
    }
}


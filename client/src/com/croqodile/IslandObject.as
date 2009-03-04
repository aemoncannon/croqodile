package com.croqodile {
    import flash.events.Event;
    import flash.events.MouseEvent;
    import flash.utils.Dictionary;
    import com.croqodile.*;
    
    public class IslandObject {
		protected var _island:IslandReplica;
		protected var _guid:int;
		
		public function IslandObject(island:IslandReplica){
			_guid = island.internIslandObject(this);
			_island = island;
		}
		
		public function get guid():int{
			return _guid;
		}
		
		public function farRef():FarRef {
			return new FarRef(_guid, _island);
		}
		
		public function futureSend(offset:Number, msg:String, args:Array):void {
			_island.scheduleInternalMessage(new InternalMessage(
					_island.time + offset,
					this,
					msg,
					args)
			);
		}
		
		public static function byRef(ref:FarRef):IslandObject{
			return _island.islandObjectByGuid(ref.guid());
		}

		public static function byGuid(guid:int):IslandObject{
			return _island.islandObjectByGuid(guid);
		}
		
    }
}


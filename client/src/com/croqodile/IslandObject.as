package com.croqodile {
    import flash.events.Event;
    import flash.events.MouseEvent;
    import flash.utils.Dictionary;
    import com.croqodile.*;
    
    public class IslandObject {
		protected var _island:IslandReplica;
		protected var _guid:int;
		
		private static var _dict:Dictionary;
		private static var _highestGuid:int = 0;
		
		public function IslandObject(island:IslandReplica){
			if(!IslandObject._dict){
				IslandObject._dict = new Dictionary();
			}
			IslandObject._highestGuid += 1;
			IslandObject._dict[IslandObject._highestGuid] = this;
			_guid = IslandObject._highestGuid;
			_island = island;
		}
		
		public function guid():int{
			return _guid;
		}
		
		public function farRef():FarRef {
			return new FarRef(_guid, _island);
		}
		
		public function futureSend(offset:Number, msg:String, args:Array):void {
			_island.scheduleInternalMessage(InternalMessage.create(_island.time() + offset, 
					this, 
					msg, 
					args, 
					_island));
		}
		
		public static function byRef(ref:FarRef):IslandObject{
			return IslandObject.byGuid(ref.guid());
		}
		
		public static function byGuid(guid:int):IslandObject{
			try {
				return IslandObject._dict[guid];
			}
			catch(e:Error){
				throw new Error("IslandObject for guid #" + 
					guid + 
					" not found. Highest guid is " + 
					IslandObject._highestGuid);
			}
			return null;
		}
		
		public static function setHighestGuid(guid:int):void{
			IslandObject._highestGuid = guid;
		}	
		
		public static function reset():void{
			IslandObject.setHighestGuid(0);
		}
		
		
    }
}


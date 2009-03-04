package com.croqodile{
    import flash.system.Security;
    import flash.utils.Timer;
    import com.croqodile.IslandReplica;
    import com.croqodile.IslandObject;
    import com.croqodile.ExternalMessage;
    import com.croqodile.Controller;
    import flash.events.*;
    
    public class FarRef{
		
		private var _guid:int;
		private var _island:IslandReplica;

		public function FarRef(guid:int, island:IslandReplica){
			_guid = guid;
			_island = island;
		}

		public function guid():int {
			return _guid;
		}

		public function target():IslandObject {
			return _island.islandObjectByGuid(_guid);
		}

		public function send(msg:String, args:Array):void{
			_island.controller().propagateFarSend(new ExternalIslandMessage());
				ExternalMessage.createRouterString(
					_guid, 
					msg, 
					args,
					_island
				));
		}

    }
}



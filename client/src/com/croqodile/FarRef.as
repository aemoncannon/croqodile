package com.croqodile{
    import flash.system.Security;
    import flash.utils.Timer;
    import com.croqodile.IslandReplica;
    import com.croqodile.IslandObject;
    import com.croqodile.ExternalMessage;
    import com.croqodile.Controller;
    import flash.events.*;
    
    public class FarRef{
		
		private var _guid:String;
		private var _island:IslandReplica;

		public function FarRef(guid:String, island:IslandReplica){
			_guid = guid;
			_island = island;
		}

		public function get guid():String {
			return _guid;
		}

		public function send(msg:String, args:Array):void{
			_island.controller.propagateFarSend(ExternalIslandMessage.createForCall(_guid, msg, args));
		}

    }
}



package com.croqodile{
    import flash.display.MovieClip;
    import flash.system.Security;
    import flash.utils.Timer;
    import com.croqodile.IslandReplica;
    import com.croqodile.Controller;
    import flash.events.*;
    
    public interface Message {

		function get time():Number;
		
		function execute(island:IslandReplica):void;

		function sortsBefore(msg:Message):Boolean;
		
		function toString():String;

    }
    
}



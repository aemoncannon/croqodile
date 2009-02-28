package com.croqodile{
    import flash.display.MovieClip;
    import flash.system.Security;
    import flash.utils.Timer;
    import com.croqodile.IslandReplica;
    import com.croqodile.Controller;
    import flash.events.*;
    
    public interface Message {

		function get executionTime():Number;

		function get sequenceNumber():int;
		
//		function execute():void;
		
		function toString():String;

    }
    
}



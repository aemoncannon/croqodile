package com.croqodile{
    import flash.display.MovieClip;
    import flash.system.Security;
    import com.senocular.utils.Output;
    import flash.utils.Timer;
    import com.croqodile.IslandReplica;
    import com.croqodile.Controller;
    import flash.events.*;
    
    public interface Message {

	function executionTime():Number;

	function sequenceNumber():int;
	
	function execute():void;
	
	function toString():String;

    }
    
}



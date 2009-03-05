package com.croqodile.test.support {
    import flash.display.MovieClip;
    import flash.system.Security;
    import flash.utils.Timer;
    import com.croqodile.*;
    import flash.events.*;
    import com.croqodile.events.*;
    
    public class DumbController extends Controller {
	
	function DumbController(config:Object){
	    this._island = config.island;
	    this._island.setController(this);
	}


}



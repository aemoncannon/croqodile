package com.croqodile.test.support {
    import flash.display.MovieClip;
    import flash.system.Security;
    import com.senocular.utils.Output;
    import flash.utils.Timer;
    import com.croqodile.*;
    import flash.events.*;
    import com.croqodile.events.*;
    import com.croqodile.serialization.json.JSON;
    
    public class DumbController extends Controller {
	
	function DumbController(config:Object){
	    this._island = config.island;
	    this._island.setController(this);
	}

	public function triggerExternalMessage(timestamp:Number, msg:Array):void {
	    this.onMessageFromRouter(new ExternalMessageEvent(String(timestamp) + 
							      ExternalMessage.MESSAGE_SEP +
							      JSON.encode(msg)));
	}

    }

}



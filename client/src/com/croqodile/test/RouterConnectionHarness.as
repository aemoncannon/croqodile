package com.croqodile.test{
    import com.croqodile.events.*;
    import com.croqodile.*;
    import flash.events.*;
    import flash.display.*;
    import flash.net.Socket;
    import flash.system.Security;
    import flash.net.URLLoader;
    import flash.net.URLRequest;
    
    public class RouterConnectionHarness extends Sprite{

		public function RouterConnectionHarness():void{
			Security.loadPolicyFile("xmlsocket://" + "localhost" + ":" + "6667");
			var url:URLLoader = new URLLoader();
			url.load(new URLRequest("http://localhost:6666/new_island?type=1&desc=2"))
			url.addEventListener(Event.COMPLETE, function(e:Event):void{ onIslandCreated(url.data); } );
		}

		protected function onIslandCreated(data:*):void{
			trace("Island created: joining...")
			var con:RouterConnection = new RouterConnection({
					host: "localhost", 
					port: 6666, 
					socket: new Socket()
				});
			con.connect("aemon", String(data));
		}

	}
}

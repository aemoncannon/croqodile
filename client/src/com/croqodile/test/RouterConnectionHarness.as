package com.croqodile.test{
    import com.croqodile.events.*;
    import com.croqodile.*;
    import flash.events.*;
    import flash.display.*;
    import flash.net.Socket;
    
    public class RouterConnectionHarness extends Sprite{

		public function RouterConnectionHarness():void{
			var con:RouterConnection = new RouterConnection({
					host: "localhost", 
					port: 6666, 
					socket: new Socket()
				});

			con.connect("kljdsfdf");
		}

	}
}

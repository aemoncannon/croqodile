package com.croqodile.util{
    import flash.utils.*;
    import flash.external.*;
    public class Log {

		public static function ffTrace(val:String):void{
			if(ExternalInterface.available){
				ExternalInterface.call("console.log", val);
			}
		}

		public static function logStackTrace():void{
			var e:Error = new Error();
			trace(e.getStackTrace());
		}

	}
}
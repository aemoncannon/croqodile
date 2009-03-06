package com.croqodile.test {
    import flash.display.*;
    import com.croqodile.*;
    import flash.events.*;
    import flash.utils.*;
    
    public class AccumulatorIsland extends IslandReplica {

		protected var _content:String = "";
		
		public function AccumulatorIsland(config:Object){
			super(config);
		}
		
		override protected function readFrom(b:IDataInput):void { 
			_content = b.readUTF();
		}

		override protected function writeTo(b:IDataOutput):void { 
			b.writeUTF(_content);
		}

		public function unsafeGetContent():String{
			return _content;
		}

		////////////////////////
        // External Interface //
        ////////////////////////
		
		public function addString(str:String):void{
			_content += str;
		}

		override public function sunrise():void{}
		
    }
}



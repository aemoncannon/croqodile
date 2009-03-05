package com.croqodile.test{
    import asunit.framework.*;
    import flash.utils.ByteArray;
    
    public class NumberSerializationTest extends TestCase {
		
		override protected function setUp():void {
		}
		
		override protected function tearDown():void {
		}
		

		//Test one possible number serialization scheme...

		public function testWritingAndReadingNumber():void {
			var num:Number = Math.PI / 27.4532 / Math.PI * -1;
			assertTrue("String representations should be the same", String(num) == 
				String(this.deserializeNum(this.serializeNum(num))));
			
			num = 9238293829839238.023023;
			assertTrue("String representations should be the same", String(num) == 
				String(this.deserializeNum(this.serializeNum(num))));
			
			num = 25;
			assertTrue("String representations should be the same", String(num) == 
				String(this.deserializeNum(this.serializeNum(num))));
			
			num = 0;
			assertTrue("String representations should be the same", String(num) == 
				String(this.deserializeNum(this.serializeNum(num))));
			
		}
		
		
		private function serializeNum(num:Number):String{
			if(isFinite(num as Number)){
				var bytes:ByteArray = new ByteArray();
				bytes.writeDouble(num);
				bytes.position = 0;
				var str:String = [bytes.readByte(),
					bytes.readByte(),
					bytes.readByte(),
					bytes.readByte(),
					bytes.readByte(),
					bytes.readByte(),
					bytes.readByte(),
					bytes.readByte()].join("|");
				return str;
			}
			else{
				return "null";
			}
		}
		
		private function deserializeNum(str:String):Number{
			var byteValues:Array = str.split("|");
			var bytes:ByteArray = new ByteArray();
			for(var i:int = 0; i < 8; i++){
				bytes.writeByte(0);
			}
			bytes.position = 0;
			for(i = 0; i < byteValues.length; i++){
				bytes.writeByte(int(byteValues[i]));
			}
			bytes.position = 0;
			return bytes.readDouble();
		}
		
		
    }
}
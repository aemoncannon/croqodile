package com.croqodile.test{
	import flexunit.framework.TestCase;
 	import flexunit.framework.TestSuite;
	import flash.utils.*;
	import com.croqodile.*;

	public class MessageSerializationTest extends RichTestCase {
		
		public function testSerialize():void{
			var msg:ExternalMessage = ExternalIslandMessage.createForCall("dude", "hello", [1, 2, 3]);
			var bytes:ByteArray = msg.toBytes();
			var msgs:Array = ExternalMessage.parseAll(bytes);
			assertTrue("Should be one message.", msgs.length == 1);
			assertTrue("msg should be in msgs.", msgs[0].equals(msg));
		}

	}
}
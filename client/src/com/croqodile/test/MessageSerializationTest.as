package com.croqodile.test{
	import flexunit.framework.TestCase;
 	import flexunit.framework.TestSuite;
	import flash.utils.*;
	import com.croqodile.*;

	public class MessageSerializationTest extends RichTestCase {
		
		public function testSerialize():void{
			var bytes:ByteArray = new ByteArray();
			var msg1:ExternalMessage = ExternalIslandMessage.createForCall("dude", "hello", [1, 2, 3]);
			msg1.writeTo(bytes);
			var msg2:ExternalMessage = new HeartbeatMessage(6, 25);
			msg2.writeTo(bytes);
			bytes.position = 0;
			var msgs:Array = ExternalMessage.parseAll(bytes);
			assertTrue("Should be two messages.", msgs.length == 2);
			assertTrue("msg should be in msgs.", msgs[0].equals(msg1));
			assertTrue("msg should be in msgs.", msgs[1].equals(msg2));
		}

	}
}
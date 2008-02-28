package com.croqodile.test{
    import asunit.framework.*;
    import com.croqodile.di.*;
    import com.croqodile.test.*;
    import com.croqodile.test.support.*;
    import com.senocular.utils.Output;
    import com.croqodile.*;
    
    public class SerializationTest extends TestCase {
	
	override protected function setUp():void {
	    IslandObject.reset();
	}
	
	override protected function tearDown():void {
	}
	
 	public function testSerializesToString():void {
	    var config:Object = DIRunner.run([
	    {name: "island", klass: DataIsland, args: {}, injectArgs: {}},
	    {name: "controller", klass: DumbController, args: {}, injectArgs: {island: "island"}} ]);

 	    var str:String = config.island.snapshot();
 	    assertTrue("Length of serialized string should be > 0.", str.length > 0);
 	}

	public function testSimpleSerializeAndDeserialize():void {
	    var config:Object = DIRunner.run([
	    {name: "island", klass: DataIsland, args: {}, injectArgs: {}},
	    {name: "controller", klass: DumbController, args: {}, injectArgs: {island: "island"}} ]);
	    config.controller.triggerExternalMessage(1002.23,["createAvatar",1,["aemon"]]);
	    config.controller.triggerExternalMessage(10001.734,["createAvatar",1,["max"]]);
	    config.island.rand().random();
	    config.island.rand().random();


	    var config2:Object = DIRunner.run([
	    {name: "island", klass: DataIsland, args: {}, injectArgs: {}},
	    {name: "controller", klass: DumbController, args: {}, injectArgs: {island: "island"}} ]);
	    config2.island.initFromSnapshot(config.island.snapshot());
	    assertTrue("The time should be the same.", config2.island.time() == config.island.time());
	    assertTrue("The random generator should be in the same state.", config2.island.rand().random() == config.island.rand().random());
	    assertTrue("There should still be 2 avatars", config2.island.avatars().length == 2);
	}


 	public function testSerializeMessageQ():void {
	    var config:Object = DIRunner.run([
	    {name: "island", klass: DataIsland, args: {}, injectArgs: {}},
	    {name: "controller", klass: DumbController, args: {}, injectArgs: {island: "island"}} ]);

 	    config.controller.triggerExternalMessage(10000.23,["setOff",1,[20,"doSomething"]]);
 	    config.controller.triggerExternalMessage(10001.2313,["setOff",1,[40,"doSomething"]]);

 	    assertTrue("There should be one internal message on the q.", config.island.msgQ().length() == 2);

	    var config2:Object = DIRunner.run([
	    {name: "island", klass: DataIsland, args: {}, injectArgs: {}},
	    {name: "controller", klass: DumbController, args: {}, injectArgs: {island: "island"}} ]);

 	    assertTrue("There should be no messages on this fresh q.", config2.island.msgQ().length() == 0);
 	    config2.island.initFromSnapshot(config.island.snapshot());
	    assertTrue("The time should be the same.", config2.island.time() == config.island.time());
 	    assertTrue("There should be one internal message on the new q.", config2.island.msgQ().length() == 2);
 	}

    }

}
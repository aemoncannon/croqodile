package com.croqodile.test{
    import asunit.framework.*;
    import com.croqodile.di.*;
    import com.croqodile.test.*;
    import com.croqodile.test.support.*;
    import com.croqodile.*;
    import com.croqodile.serialization.json.JSON;
    
    public class StartupTest extends TestCase {
	
	override protected function setUp():void {
	    IslandObject.reset();
	}

	override protected function tearDown():void {
	}
	
 	public function testStartup():void {
	    var config:Object = DIRunner.run([
	    {name: "messageSocket", klass: MockSocket, args: {}, injectArgs: {}},
	    {name: "snapshotSocket", klass: MockSocket, args: {}, injectArgs: {}},
	    {name: "messageCon", klass: RouterMessageConnection, args: {}, injectArgs: {socket: "messageSocket"}},
	    {name: "snapshotCon", klass: RouterSnapshotConnection, args: {}, injectArgs: {socket: "snapshotSocket"}},
	    {name: "island", klass: DataIsland, args: {}, injectArgs: {}},
	    {name: "controller", klass: SnapshottingController, args: {}, injectArgs: {island: "island", 
									       messageCon: "messageCon", 
									       snapshotCon: "snapshotCon" }} ]);
	    
	    assertTrue("message socket should be connected", config.messageSocket.connected);
	    assertTrue("snapshot socket should be connected", config.snapshotSocket.connected);

	    config.messageSocket.triggerData(RouterConnection.LOGIN_ACK + this.term());
	    config.snapshotSocket.triggerData(RouterSnapshotConnection.LOGIN_YOU_ARE_FIRST_ACK + this.term());

	    assertTrue("message connection should be ready", config.messageCon.ready());
	    assertTrue("snapshot connection should be ready", config.snapshotCon.ready());
	}

 	public function testCreateAvatars():void {
	    var config:Object = DIRunner.run([
	    {name: "messageSocket", klass: MockSocket, args: {}, injectArgs: {}},
	    {name: "snapshotSocket", klass: MockSocket, args: {}, injectArgs: {}},
	    {name: "messageCon", klass: RouterMessageConnection, args: {}, injectArgs: {socket: "messageSocket"}},
	    {name: "snapshotCon", klass: RouterSnapshotConnection, args: {}, injectArgs: {socket: "snapshotSocket"}},
	    {name: "island", klass: DataIsland, args: {}, injectArgs: {}},
	    {name: "controller", klass: SnapshottingController, args: {}, injectArgs: {island: "island", 
									       messageCon: "messageCon", 
									       snapshotCon: "snapshotCon" }} ]);

	    config.messageSocket.triggerData(RouterConnection.LOGIN_ACK + this.term());
	    config.snapshotSocket.triggerData(RouterSnapshotConnection.LOGIN_YOU_ARE_FIRST_ACK + this.term());
	    
	    config.messageSocket.triggerData(this.toMsgData(1002.23, ["createAvatar",1,["aemon"]]));
	    assertTrue("there should be 1 avatar", config.island.avatars().length == 1);

	    config.messageSocket.triggerData(this.toMsgData(1004, ["createAvatar",1,["max"]]));
	    assertTrue("there should be 1 avatar", config.island.avatars().length == 2);
	}


 	public function testReceiveSnapshot():void {
	    var config:Object = DIRunner.run([
	    {name: "messageSocket", klass: MockSocket, args: {}, injectArgs: {}},
	    {name: "snapshotSocket", klass: MockSocket, args: {}, injectArgs: {}},
	    {name: "messageCon", klass: RouterMessageConnection, args: {}, injectArgs: {socket: "messageSocket"}},
	    {name: "snapshotCon", klass: RouterSnapshotConnection, args: {}, injectArgs: {socket: "snapshotSocket"}},
	    {name: "island", klass: DataIsland, args: {}, injectArgs: {}},
	    {name: "controller", klass: SnapshottingController, args: {}, injectArgs: {island: "island", 
									       messageCon: "messageCon", 
									       snapshotCon: "snapshotCon" }} ]);

	    config.messageSocket.triggerData(RouterConnection.LOGIN_ACK + this.term());
	    config.snapshotSocket.triggerData(RouterSnapshotConnection.LOGIN_YOU_ARE_FIRST_ACK + this.term());

	    config.messageSocket.triggerData(this.toMsgData(1002.23, ["createAvatar",1,["aemon"]]));
	    assertTrue("there should be 1 avatar", config.island.avatars().length == 1);

	    config.messageSocket.triggerData(this.toMsgData(1004, ["createAvatar",1,["max"]]));
	    assertTrue("there should be 2 avatar", config.island.avatars().length == 2);


	    var config2:Object = DIRunner.run([
	    {name: "messageSocket", klass: MockSocket, args: {}, injectArgs: {}},
	    {name: "snapshotSocket", klass: MockSocket, args: {}, injectArgs: {}},
	    {name: "messageCon", klass: RouterMessageConnection, args: {}, injectArgs: {socket: "messageSocket"}},
	    {name: "snapshotCon", klass: RouterSnapshotConnection, args: {}, injectArgs: {socket: "snapshotSocket"}},
	    {name: "island", klass: DataIsland, args: {}, injectArgs: {}},
	    {name: "controller", klass: SnapshottingController, args: {}, injectArgs: {island: "island", 
									       messageCon: "messageCon", 
									       snapshotCon: "snapshotCon" }} ]);

	    assertTrue("there should be 2 avatar", config2.island.avatars().length == 0);

	    config2.messageSocket.triggerData(RouterConnection.LOGIN_ACK + this.term());
	    config2.snapshotSocket.triggerData(RouterConnection.LOGIN_ACK + this.term());
	    config2.snapshotSocket.triggerData(config.island.snapshot() + this.term());

	    assertTrue("there should be 2 avatar", config2.island.avatars().length == 2);
	}
	    
	private function toMsgData(timestamp:Number, msg:Array):String{
	    return String(timestamp) + ExternalMessage.MESSAGE_SEP + JSON.encode(msg) + this.term();
	}

	private function term():String {
	    return String.fromCharCode(RouterConnection.TERMINATOR[0]) + String.fromCharCode(RouterConnection.TERMINATOR[1]);
	}
	
    }
    
}
package com.croqodile.test{
	import flexunit.framework.TestCase;
 	import flexunit.framework.TestSuite;
	import flash.utils.*;
	import com.croqodile.*;
	import com.croqodile.di.DIRunner;

	public class IslandSynchTest extends RichTestCase {
		
		public function testTrivialSnapshot():void{
			var config1:Object = DIRunner.run([
					{name: "island", klass: AccumulatorIsland, args: { id: "id" }, injectArgs: {}},
					{name: "con", klass: MockRouterConnection, args: {}, injectArgs: {}},
					{name: "controller", klass: Controller, args: {}, injectArgs: {island: "island", routerCon: "con"}} 
				]);
			var config2:Object = DIRunner.run([
					{name: "island", klass: AccumulatorIsland, args: { id: "id" }, injectArgs: {}},
					{name: "con", klass: MockRouterConnection, args: {}, injectArgs: {}},
					{name: "controller", klass: Controller, args: {}, injectArgs: {island: "island", routerCon: "con"}} 
				]);
			var isl1:IslandReplica = config1.island;
			var ref1:FarRef = isl1.farRef();
			var isl2:IslandReplica = config2.island;
			var ref2:FarRef = isl2.farRef();
			assertTrue("islands should be equal.", isl1.equals(isl2));
		}

		public function testAFewMessagesMessage():void{
			var config1:Object = DIRunner.run([
					{name: "island", klass: AccumulatorIsland, args: { id: "id" }, injectArgs: {}},
					{name: "con", klass: MockRouterConnection, args: {}, injectArgs: {}},
					{name: "controller", klass: Controller, args: {}, injectArgs: {island: "island", routerCon: "con"}} 
				]);
			var config2:Object = DIRunner.run([
					{name: "island", klass: AccumulatorIsland, args: { id: "id" }, injectArgs: {}},
					{name: "controller", klass: Controller, args: { routerCon: config1.con }, injectArgs: {island: "island"}} 
				]);
			var isl1:AccumulatorIsland = config1.island;
			var ref1:FarRef = isl1.farRef();
			var isl2:AccumulatorIsland = config2.island;
			var ref2:FarRef = isl2.farRef();
			var con:MockRouterConnection = config1.con;
			ref1.send("addString", ["apple"]);
			ref1.send("addString", ["dog"]);
			ref1.send("addString", ["hello"]);
			con.pump();
			con.pump();
			con.pump();
			assertFalse("islands should be equal.", isl1.equals(isl2));
			assertTrue("island1 content should be...", isl1.unsafeGetContent() === "appledoghello");
			assertTrue("island2 content should be...", isl2.unsafeGetContent() === "appledoghello");
		}


	}
}
package com.croqodile.test{
    import asunit.framework.*;
    import com.croqodile.*;
    import com.croqodile.di.*;
    
    public class DIRunnerTest extends TestCase {
	
	override protected function setUp():void {}
	
	override protected function tearDown():void {}
	
 	public function testSimple():void {
	    var created:Object = DIRunner.run([
	    { name: "person", klass: Person, args: {name: "Karl"}, injectArgs: {}}]);
	    
	    assertTrue("his name should be Karl", created.person.config.name == "Karl");
	}

 	public function testInjectOne():void {
	    var created:Object = DIRunner.run([
	    { name: "karl", klass: Person, args: {name: "Karl"}, injectArgs: {}},
	    { name: "mike", klass: Person, args: {name: "Mike"}, injectArgs: {karl: "karl"}}
					       ]);
	    assertTrue("his name should be Karl", created.karl.config.name == "Karl");

	    assertTrue("his name should be Mike", created.mike.config.name == "Mike");
	    assertTrue("Mike should have a Karl", created.mike.config.karl.config.name == "Karl");
	    
	}


 	public function testInjectMore():void {
	    var created:Object = DIRunner.run([
	    { name: "eyeball", klass: EyeBall, args: {}, injectArgs: {}},
	    { name: "karl", klass: Person, args: {name: "Karl"}, injectArgs: {eyeball: "eyeball"}},
	    { name: "mike", klass: Person, args: {name: "Mike"}, injectArgs: {karl: "karl", eyeball: "eyeball"}}
					       ]);
	    assertTrue("karl and mike should have the same eyeball", 
		       created.karl.config.eyeball == created.mike.config.eyeball);
	    
	}

	
    }
    
}

class Person {
    public var config:Object;
    function Person(config:Object){
	this.config = config;
    }
}

class EyeBall {
    public var config:Object;
    function EyeBall(config:Object){
	this.config = config;
    }
}

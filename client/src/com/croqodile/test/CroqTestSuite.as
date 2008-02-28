package com.croqodile.test {
    import asunit.framework.*;
    import com.croqodile.test.*;
    public class CroqTestSuite extends TestSuite {
	public function CroqTestSuite(){
	    super();
	    addTest(new SerializationTest());
	    addTest(new NumberSerializationTest());
	    addTest(new DIRunnerTest());
	    addTest(new StartupTest());
	}
	
    }
}
package com.croqodile.test {
    
    import asunit.textui.TestRunner;
    import com.croqodile.test.*;
    import com.senocular.utils.Output;
    
    public class CroqTestRunner extends TestRunner {

        public function CroqTestRunner() {
	    super();
	    stage.addChild(new Output());
	    Output.trace("Starting tests...");
            start(CroqTestSuite);
        }

	public static function main():void {
	    var runner:TestRunner = new CroqTestRunner();
	}

    }
    
}
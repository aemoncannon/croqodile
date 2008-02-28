package com.croqodile.di{

    public class DIRunner {
		
		/* Expected format for a DI(Dependency Injection) Description looks like this:
		[
		{name: "namedInstance", klass: SomeKlass, args: {count: 1}, injectArgs: {}},
		{name: "anotherInstance", klass: AnotherKlass, args: {count: 2}, injectArgs: {instance: "namedInstance"}}
		]

		Each row in the description represents an object in the system configuration.
		The class of the object will be 'klass'. A config dictionary comprised of 'args' and
		'injectArgs' will be passed to the object's constructor. 

		For instance:
		
		var config:Object = DIRunner.run([
		{name: "eyeball", klass: PrettyEyeball , args: {color: "brown"}, injectArgs: {}},
		{name: "person", klass: Person, args: {name: "Julio"}, injectArgs: {eye: "eyeball"}}
		]);

		Where "eyeball" in the second row refers to the instance of PrettyEyeball created in the first.

		We can now use config.eyeball and config.person to refer to the instantiated objects. */

		public static function run(desc:Array):Object {
			var created:Object = new Object();
			for each(var target:Object in desc){
				var args:Object = new Object();
				for(var argName:String in target.args){
					args[argName] = target.args[argName];
				}
				for(var injectArgName:String in target.injectArgs){
					args[injectArgName] = created[target.injectArgs[injectArgName]];
				}
				created[target.name] = new (target.klass)(args);
			}
			return created;
		}
		
    }
    
}



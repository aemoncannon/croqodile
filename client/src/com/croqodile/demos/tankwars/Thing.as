package com.croqodile.demos.tankwars {
    import flash.events.*;
    import flash.utils.*;
    import com.croqodile.*;
    import com.croqodile.demos.tankwars.*;
    import org.cove.ape.*;
    
    public class Thing extends PhysObj {
		
		public function Thing(island:IslandReplica){
			var part:CircleParticle = new CircleParticle(
				300, //x
				200, //y
				10, //radius
				false, //fixed?
				0.2, //mass
				0.3, //elasticity
				0.1); //friction
			super(island, part);
		}


		public static function readFrom(b:IDataInput, island:IslandReplica):Thing{
			var thing:Thing = new Thing(island);
			thing.readFrom(b);
			return thing;
		}

		
    }
}


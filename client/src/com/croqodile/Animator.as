package com.croqodile{
    import com.croqodile.*;
    import flash.events.*;
    import flash.utils.*;
    
    public class Animator extends IslandObject {
		
		protected var _step:int = 0;
		protected var _stepFunc:Function;
		protected var _timeIncrement:Number;
		
		public function Animator(island:IslandReplica, timeIncrement:Number, stepFunc:Function){
			super(island);
			_timeIncrement = timeIncrement;
			_stepFunc = stepFunc;
			_step = 0;
		}

		public function start():void {
			step();
		}

		public function restart():void {
			_step = 0;
			start();
		}

		override public function readFrom(b:IDataInput):void { 
			_step = b.readUnsignedInt();
		}
		
		override public function writeTo(b:IDataOutput):void {
			b.writeUnsignedInt(_step);
		}

		public function step():void {
			_step += 1;
			if(_stepFunc(_step)){
				futureSend(_timeIncrement, "step", []);
			}
		}

    }
}



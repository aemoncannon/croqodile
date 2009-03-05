package com.croqodile{
    import com.croqodile.*;
    import flash.events.*;
    
    public class Animator extends IslandObject {
		
		private var _step:int = 0;
		private var _stepFunc:Function;
		private var _timeIncrement:Number;
		
		public function Animator(island:IslandReplica, timeIncrement:Number, stepFunc:Function){
			super(island);
			_timeIncrement = timeIncrement;
			_stepFunc = stepFunc;
			_step = 0;
		}

		public function start():void {
			this.step();
		}

		public function restart():void {
			this._step = 0;
			this.start();
		}

		public function freeze():Object { 
			return { step: this._step }
		}
		
		public function unfreeze(data:Object):void {
			this._step = data.step;
		}

		public function step():void {
			this._step += 1;
			if(this._stepFunc(this._step)){
				this.futureSend(this._timeIncrement, "step", []);
			}
		}

    }
}



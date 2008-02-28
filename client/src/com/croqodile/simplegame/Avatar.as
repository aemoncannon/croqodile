package com.croqodile.simplegame {
    import flash.display.MovieClip;
    import flash.events.Event;
    import flash.events.MouseEvent;
    import com.senocular.utils.Output;
    import com.croqodile.*;
    import com.croqodile.simplegame.*;
    
	public class Avatar extends IslandObject {
	    
	    private var _userId:String;
	    private var _clip:MovieClip;
	    private var _xVel:Number = 0;
	    private var _yVel:Number = 0;
	    
	    public function Avatar(island:IslandReplica, userId:String){
		super(island);
		this._userId = userId;
		this._clip = new AvatarClip();
		this.moveTo(50, 50);
	    }

	    public function startAnimating():void{
		this.futureSend(50, "updatePosition", []);
	    }

	    public function clip():MovieClip {
		return this._clip;
	    }

	    public function setXVelocity(xVel:Number):void{
		this._xVel = xVel;
	    }

	    public function setYVelocity(yVel:Number):void{
		this._yVel = yVel;
	    }

	    public function updatePosition():void{
		this._clip.x += this._xVel;
		this._clip.y += this._yVel;
		this.futureSend(50, "updatePosition", []);
	    }

	    public function moveTo(x:Number, y:Number):void{
		this._clip.x = x;
		this._clip.y = y;
	    }

	    public function setColor(color:String):void{
	    }

	    public function freeze():Object{
		var data:Object = new Object();
		data.userId = this._userId;
		data.x = this._clip.x;
		data.y = this._clip.y;
		data.xVel = this._xVel;
		data.yVel = this._yVel;
		return data;
	    }

	    public function unfreeze(data:Object):void{
		this._userId = data.userId;
		this._clip.x = data.x;
		this._clip.y = data.y;
		this._xVel = data.xVel;
		this._yVel = data.yVel;
	    }
	    
	}
}


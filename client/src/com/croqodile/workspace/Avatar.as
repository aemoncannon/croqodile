package com.croqodile.workspace {
    import flash.display.MovieClip;
    import flash.display.Sprite;
    import flash.events.Event;
    import flash.events.MouseEvent;
    import com.senocular.utils.Output;
    import com.croqodile.*;
    import com.croqodile.workspace.*;
    import org.cove.ape.*;
    
    public class Avatar extends IslandObject {
	private var _userId:String;
	private var _clip:MovieClip;
	private var _wordBubbleFadeAnimator:Animator;
	private var _wordBubble:WordBubbleClip;
	
	public function Avatar(island:IslandReplica, userId:String){
	    super(island);
	    this._userId = userId;
	    this._clip = new AvatarClip();
	    var canvas:Sprite = WorkspaceIsland(island).canvas();
	    canvas.addChild(this._clip);

	    this._wordBubble = new WordBubbleClip();
	    canvas.addChild(this._wordBubble);
	    
	    var self:Avatar = this;

	    this._wordBubbleFadeAnimator = new Animator(island, 600, function(step:int):Boolean {
		    self._wordBubble.alpha = (1 - (step/10.0));
		    return step < 10;
		});
	}
	
	public function render():void{
	    this._wordBubble.x = this._clip.x + this._clip.width;
	    this._wordBubble.y = this._clip.y - 20;
	}
	
	public function freeze():Object{
	    var data:Object = {
		userId: this._userId,
		x: this._clip.x,
		y: this._clip.y,
		wordBubbleText: this._wordBubble.text(),
		wordBubbleAlpha: this._wordBubble.alpha,
		wordBubbleFadeAnimator: this._wordBubbleFadeAnimator.freeze()
	    }
	    return data;
	}
	
	public function unfreeze(data:Object):void{
	    this._userId = data.userId;
	    this._clip.x = data.x;
	    this._clip.y = data.y;

	    this._wordBubble.setText(data.wordBubbleText);
	    this._wordBubble.alpha = data.wordBubbleAlpha;
	    
	    this._wordBubbleFadeAnimator.unfreeze(data.wordBubbleFadeAnimator);
	}
	
	///////////////////////////////
        // External Interface	     //
        ///////////////////////////////
	
	public function moveTo(x:Number, y:Number){
	    this._clip.x = x;
	    this._clip.y = y;
	    WorkspaceIsland(this._island).avatarMoveTo(this, x, y);
	}

	public function grab(x:Number, y:Number){
	    WorkspaceIsland(this._island).avatarGrab(this, x, y);
	    this._clip.showGrabbing();
	}

	public function release(x:Number, y:Number){
	    WorkspaceIsland(this._island).avatarRelease(this, x, y);
	    this._clip.showReleased();
	}

	public function sayWords(words:String){
	    this._wordBubbleFadeAnimator.restart();
	    this._wordBubble.setText("\"" + words + "\"");
	}
	
	
    }
}

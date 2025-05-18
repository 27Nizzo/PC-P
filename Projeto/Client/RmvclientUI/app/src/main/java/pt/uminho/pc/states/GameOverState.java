package pt.uminho.pc.states;

import processing.core.PApplet;
import pt.uminho.pc.GUI;
import pt.uminho.pc.State;

public class GameOverState implements GameState {
    private final PlayState playState;
    
    public GameOverState(PlayState playState) {
        this.playState = playState;
    }
    
    @Override
    public void draw(GUI context) {
        context.background(200);
        
        context.fill(50, 50, 150);
        context.rect(context.width/2 - 150, context.height/2 - 150, 300, 300);
        
        context.fill(255);
        context.textAlign(PApplet.CENTER, PApplet.CENTER);
        context.textSize(24);
        context.text("GAME OVER", context.width/2, context.height/2 - 100);
        
        context.textSize(20);
        context.text(playState.getWinnerMessage(), context.width/2, context.height/2 - 50);
        
        context.textSize(16);
        context.text("Final Scores", context.width/2, context.height/2);
        context.text("Player 1 (Blue): " + playState.getPlayer1().getScore(), 
                context.width/2, context.height/2 + 30);
        context.text("Player 2 (Red): " + playState.getPlayer2().getScore(), 
                context.width/2, context.height/2 + 60);
        
        context.textSize(16);
        context.fill(100);
        context.rect(context.width/2 - 60, context.height/2 + 100, 120, 40);
        context.fill(255);
        context.text("BACK TO MENU", context.width/2, context.height/2 + 120);
        
        context.textSize(12);
        
        context.textAlign(PApplet.LEFT, PApplet.BASELINE);
    }
    
    @Override
    public void mousePressed(GUI context) {
        if (context.mouseX > context.width/2 - 60 && context.mouseX < context.width/2 + 60 && 
            context.mouseY > context.height/2 + 100 && context.mouseY < context.height/2 + 140) {
            context.switchState(State.MENU);
        }
    }
    
    @Override
    public void keyPressed(GUI context, int keyCode, char key) {
        // unused
    }
} 
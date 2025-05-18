package pt.uminho.pc.states;

import processing.core.PApplet;
import pt.uminho.pc.GUI;
import pt.uminho.pc.State;

public class SearchingState implements GameState {
    private long searchStartTime = 0;
    
    @Override
    public void draw(GUI context) {
        if (searchStartTime == 0) {
            searchStartTime = context.millis();
        }
        
        float elapsedTime = (context.millis() - searchStartTime) / 1000.0f;
        
        context.background(200);
        
        float loadingProgress = (elapsedTime % 3) / 3.0f;
        context.fill(50);
        context.rect(context.width/2 - 100, context.height/2 + 20, 200, 20);
        context.fill(0, 150, 200);
        context.rect(context.width/2 - 100, context.height/2 + 20, 200 * loadingProgress, 20);
        
        context.fill(0);
        context.textAlign(PApplet.CENTER, PApplet.CENTER);
        
        if (elapsedTime < 5) {
            context.text("Searching for opponents...", context.width/2, context.height/2 - 30);
        } else if (elapsedTime < 10) {
            context.text("Opponents found!", context.width/2, context.height/2 - 50);
            context.text("Game will start in " + (10 - (int)elapsedTime) + " seconds", context.width/2, context.height/2 - 10);
        } else {
            context.switchState(State.PLAY);
            searchStartTime = 0;
        }
        
        context.textAlign(PApplet.LEFT, PApplet.BASELINE);
    }
    
    @Override
    public void mousePressed(GUI context) {
        // unused
    }
    
    @Override
    public void keyPressed(GUI context, int keyCode, char key) {
        // unused
    }
}

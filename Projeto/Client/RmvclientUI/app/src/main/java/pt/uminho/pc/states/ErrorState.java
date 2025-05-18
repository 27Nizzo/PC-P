package pt.uminho.pc.states;

import processing.core.PApplet;
import pt.uminho.pc.GUI;
import pt.uminho.pc.State;

public class ErrorState implements GameState {
    
    @Override
    public void draw(GUI context) {
        context.fill(200);
        context.rect(0, 0, context.width, context.height);
        
        context.fill(240, 20, 20);
        context.rect(context.width/2 - 150, context.height/2 - 100, 300, 200);
        
        context.fill(255);
        context.textAlign(PApplet.CENTER, PApplet.CENTER);
        context.text("ERROR", context.width/2, context.height/2 - 70);
        context.text(context.getErrorMessage(), context.width/2, context.height/2);
        
        context.fill(100);
        context.rect(context.width/2 - 40, context.height/2 + 50, 80, 30);
        context.fill(255);
        context.text("OK", context.width/2, context.height/2 + 70);
        
        context.textAlign(PApplet.LEFT, PApplet.BASELINE);
    }
    
    @Override
    public void mousePressed(GUI context) {
        if (context.mouseX > context.width/2 - 40 && context.mouseX < context.width/2 + 40 && 
            context.mouseY > context.height/2 + 50 && context.mouseY < context.height/2 + 80) {
            context.switchState(State.valueOf(context.getStateBeforeError()));
        }
    }
    
    @Override
    public void keyPressed(GUI context, int keyCode, char key) {
        // unused
    }
} 
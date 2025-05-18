package pt.uminho.pc.states;

import processing.core.PApplet;
import pt.uminho.pc.GUI;
import pt.uminho.pc.State;

public class MenuState implements GameState {
    @Override
    public void draw(GUI context) {
        context.background(200);
        
        if (!context.isLoggedIn()) {
            drawButton(context, "LOGIN", context.width/2-60, 100, 120, 30, State.ENTER_CREDENTIALS);
            drawButton(context, "REGISTER", context.width/2-60, 150, 120, 30, State.ENTER_CREDENTIALS);
        } else {
            drawButton(context, "PLAY", context.width/2-60, 100, 120, 30, State.SEARCHING);
            drawButton(context, "UNREGISTER", context.width/2-60, 150, 120, 30, State.ENTER_CREDENTIALS);
        }
    }
    
    @Override
    public void mousePressed(GUI context) {
        if (!context.isLoggedIn()) {
            if (isMouseOver(context, context.width/2-60, 100, 120, 30)) {
                context.switchState(State.ENTER_CREDENTIALS);
            } else if (isMouseOver(context, context.width/2-60, 150, 120, 30)) {
                context.switchState(State.ENTER_CREDENTIALS);
            }
        } else {
            if (isMouseOver(context, context.width/2-60, 100, 120, 30)) {
                context.switchState(State.SEARCHING);
            } else if (isMouseOver(context, context.width/2-60, 150, 120, 30)) {
                context.switchState(State.ENTER_CREDENTIALS);
            }
        }
    }
    
    @Override
    public void keyPressed(GUI context, int keyCode, char key) {
        // unused
    }
    
    private void drawButton(GUI context, String label, int x, int y, int w, int h, State nextState) {
        boolean over = isMouseOver(context, x, y, w, h);
        context.fill(over ? 150 : 100);
        context.rect(x, y, w, h);
        context.fill(255);
        context.text(label, x + 10, y + 20);
    }
    
    private boolean isMouseOver(GUI context, int x, int y, int w, int h) {
        return context.mouseX >= x && context.mouseX <= x + w && 
               context.mouseY >= y && context.mouseY <= y + h;
    }
}

package pt.uminho.pc.states;

import processing.core.PApplet;
import pt.uminho.pc.GUI;

public interface GameState {
    void draw(GUI context);
    void mousePressed(GUI context);
    void keyPressed(GUI context, int keyCode, char key);
} 
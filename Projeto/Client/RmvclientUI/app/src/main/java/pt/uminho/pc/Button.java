package pt.uminho.pc;

import processing.core.PApplet;
import processing.net.Client;
import processing.core.PFont;
import java.util.ArrayList;
import java.util.List;

class Button {
    String label;
    int x, y, width, height;
    boolean isHighlighted;

    Button(String label, int x, int y, int width, int height) {
        this.label = label;
        this.x = x;
        this.y = y;
        this.width = width;
        this.height = height;
        this.isHighlighted = false;
    }
}

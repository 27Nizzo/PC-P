package pt.uminho.pc;

import processing.core.PApplet;

public class Mouse {

    private PApplet app;

    public Mouse(PApplet app) {
        this.app = app;
    }

    public boolean isMouseOver(int x, int y, int w, int h) {
        return app.mouseX > x && app.mouseX < x + w && app.mouseY > y && app.mouseY < y + h;
    }

    public void showCursor() {
        app.cursor();
    }

    public void hideCursor() {
        app.noCursor();
    }
}
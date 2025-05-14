package pt.uminho.pc.components;

import processing.core.PApplet;

public class Mouse {
    private Tuple<Float, Float> position;
    private boolean pressed;
    private PApplet app;

    public Mouse(PApplet app) {
        this.position = new Tuple<Float, Float>(0.0f, 0.0f);
        this.pressed = false;
        this.app = app;
    }

    public synchronized Tuple<Float, Float> getPosition() {
        return position;
    }

    public synchronized void updatePosition(Tuple<Float, Float> newPosition) {
        this.position = newPosition;
    }

    public synchronized void updatePosition(float x, float y) {
        this.position.first = x;
        this.position.second = y;
    }

    public synchronized void updateState(float x, float y, boolean pressed) {
        this.position.first = x;
        this.position.second = y;
        this.pressed = pressed;
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

    @Override
    public synchronized String toString() {
        String state = pressed ? "2" : "1";
        pressed = false;
        return position.toString() + " " + state;
    }
}

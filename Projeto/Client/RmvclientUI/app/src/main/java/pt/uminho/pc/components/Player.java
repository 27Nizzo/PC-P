package pt.uminho.pc.components;

import processing.core.PApplet;

public class Player {
    private int x;
    private int y;
    private int score;
    private int lastDirX;
    private int lastDirY;
    private int color;
    private boolean isPlayer1;

    public Player(int x, int y, int lastDirX, int lastDirY, int color, boolean isPlayer1) {
        this.x = x;
        this.y = y;
        this.score = 0;
        this.lastDirX = lastDirX;
        this.lastDirY = lastDirY;
        this.color = color;
        this.isPlayer1 = isPlayer1;
    }

    public void draw(PApplet p, int boardX, int boardY, int cellSize) {
        p.fill(color);
        p.ellipse(boardX + cellSize * x, boardY + cellSize * y, cellSize * 0.8f, cellSize * 0.8f);
    }

    public void moveUp() {
        if (y > 1) {
            y--;
            lastDirX = 0;
            lastDirY = -1;
        }
    }

    public void moveDown() {
        if (y < 23) {
            y++;
            lastDirX = 0;
            lastDirY = 1;
        }
    }

    public void moveLeft() {
        if (x > 1) {
            x--;
            lastDirX = -1;
            lastDirY = 0;
        }
    }

    public void moveRight() {
        if (x < 23) {
            x++;
            lastDirX = 1;
            lastDirY = 0;
        }
    }

    public void incrementScore() {
        score++;
    }

    public void reset(int startX, int startY) {
        this.x = startX;
        this.y = startY;
        this.score = 0;
        this.lastDirX = isPlayer1 ? 1 : -1;
        this.lastDirY = 0;
    }

    public int getX() {
        return x;
    }

    public int getY() {
        return y;
    }

    public int getScore() {
        return score;
    }

    public int getLastDirX() {
        return lastDirX;
    }

    public int getLastDirY() {
        return lastDirY;
    }
} 
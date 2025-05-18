package pt.uminho.pc.components;

import processing.core.PApplet;

public class Projectile {
    private int x;
    private int y;
    private int dirX;
    private int dirY;
    private boolean active;
    private int color;

    public Projectile(int color) {
        this.active = false;
        this.color = color;
    }

    public void fire(int startX, int startY, int directionX, int directionY) {
        this.x = startX;
        this.y = startY;
        this.dirX = directionX;
        this.dirY = directionY;
        
        if (dirX == 0 && dirY == 0) {
            dirX = 1;
            dirY = 0;
        }
        
        this.active = true;
    }

    public void update(int boardSize, Player owner, Player target) {
        if (!active) return;
        
        x += dirX;
        y += dirY;
        
        // check for wall collisions
        if (x <= 0 || x >= boardSize - 1 || y <= 0 || y >= boardSize - 1) {
            active = false;
            return;
        }
        
        // check hit on target player
        if (x == target.getX() && y == target.getY()) {
            owner.incrementScore();
            active = false;
        }
    }

    public void draw(PApplet p, int boardX, int boardY, int cellSize) {
        if (!active) return;
        
        p.fill(color);
        p.ellipse(boardX + cellSize * x, boardY + cellSize * y, cellSize * 0.4f, cellSize * 0.4f);
    }

    public void reset() {
        active = false;
    }

    public boolean isActive() {
        return active;
    }
    
    public void setActive(boolean active) {
        this.active = active;
    }
} 
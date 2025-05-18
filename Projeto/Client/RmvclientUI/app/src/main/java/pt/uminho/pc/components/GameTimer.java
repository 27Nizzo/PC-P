package pt.uminho.pc.components;

import processing.core.PApplet;

public class GameTimer {
    private long startTime;
    private final long duration;
    private boolean isActive;
    
    public GameTimer(long durationMillis) {
        this.duration = durationMillis;
        this.isActive = false;
    }
    
    public void start() {
        startTime = System.currentTimeMillis();
        isActive = true;
    }
    
    public void reset() {
        isActive = false;
    }
    
    public long getRemainingTime() {
        if (!isActive) return duration;
        
        long elapsedTime = System.currentTimeMillis() - startTime;
        return Math.max(0, duration - elapsedTime);
    }
    
    public boolean isTimeUp() {
        return getRemainingTime() <= 0;
    }
    
    public void drawTimer(PApplet p, int x, int y) {
        long remainingSeconds = getRemainingTime() / 1000;
        int minutes = (int)(remainingSeconds / 60);
        int seconds = (int)(remainingSeconds % 60);
        
        p.fill(0);
        p.textAlign(PApplet.CENTER);
        p.text("Time remaining: " + String.format("%02d:%02d", minutes, seconds), x, y);
        p.textAlign(PApplet.LEFT);
    }
} 
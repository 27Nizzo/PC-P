package pt.uminho.pc.components;

import processing.core.PApplet;
import processing.core.PApplet;


public class Board {
    private final int BOARD_WIDTH = 800;
    private final int BOARD_HEIGHT = 600;
    private final int WALL_THICKNESS = 10;

    private PApplet app; // Store the PApplet instance
    private String username;
    private String response;

    public Board(PApplet app) {
        this.app = app;
    }

    // Placeholder implementation for Board class
    public void setBoard(String username, String response) {
        this.username = username;
        this.response = response;
    }

    public void drawBoard() {
        app.background(0);
        app.fill(255);
        // Top
        app.rect(0, 0, BOARD_WIDTH, WALL_THICKNESS);
        // Bottom
        app.rect(0, BOARD_HEIGHT - WALL_THICKNESS, BOARD_WIDTH, WALL_THICKNESS);
        // Left
        app.rect(0, 0, WALL_THICKNESS, BOARD_HEIGHT);
        // Right
        app.rect(BOARD_WIDTH - WALL_THICKNESS, 0, WALL_THICKNESS, BOARD_HEIGHT);
    }
}

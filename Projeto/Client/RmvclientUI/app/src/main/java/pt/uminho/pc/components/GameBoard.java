package pt.uminho.pc.components;

import processing.core.PApplet;

public class GameBoard {
    private int cellSize;
    private int boardSize;
    private int boardWidth;
    private int boardHeight;
    private int boardX;
    private int boardY;
    
    public GameBoard(int cellSize, int boardSize, int appWidth, int appHeight) {
        this.cellSize = cellSize;
        this.boardSize = boardSize;
        this.boardWidth = boardSize * cellSize;
        this.boardHeight = boardSize * cellSize;
        this.boardX = (appWidth - boardWidth) / 2;
        this.boardY = (appHeight - boardHeight) / 2;
    }
    
    public void draw(PApplet p) {
        p.fill(255);
        p.rect(boardX, boardY, boardWidth, boardHeight);
        
        p.stroke(200);
        for (int i = 0; i <= boardSize; i++) {
            p.line(boardX + i * cellSize, boardY, boardX + i * cellSize, boardY + boardHeight);
            p.line(boardX, boardY + i * cellSize, boardX + boardWidth, boardY + i * cellSize);
        }
        
        p.stroke(0);
        p.strokeWeight(4);
        p.noFill();
        p.rect(boardX, boardY, boardWidth, boardHeight);
        p.strokeWeight(1);
    }
    
    public void drawInstructionPanels(PApplet p, Player player1, Player player2) {
        p.textSize(12);

        // Player 1 instruction panel (left side)
        p.fill(200, 220, 255);
        p.rect(10, boardY, boardX - 20, 170);

        p.fill(0, 0, 150);
        p.textAlign(PApplet.CENTER);
        p.text("PLAYER 1 (BLUE)", 10 + (boardX - 20) / 2, boardY + 20);
        p.textAlign(PApplet.LEFT);
        p.fill(0);
        p.text("Move: Arrow Keys", 20, boardY + 45);
        p.text("Shoot: SPACE", 20, boardY + 65);
        p.text("Goal: Hit the red", 20, boardY + 85);
        p.text("player with", 20, boardY + 105);
        p.text("your projectile", 20, boardY + 125);
        p.text("Score: " + player1.getScore(), 20, boardY + 150);

        // Player 2 instruction panel (right side)
        p.fill(255, 220, 220);
        p.rect(boardX + boardWidth + 10, boardY, boardX - 20, 170);

        p.fill(150, 0, 0);
        p.textAlign(PApplet.CENTER);
        p.text("PLAYER 2 (RED)", boardX + boardWidth + 10 + (boardX - 20) / 2, boardY + 20);
        p.textAlign(PApplet.LEFT);
        p.fill(0);
        p.text("Move: W,A,S,D Keys", boardX + boardWidth + 20, boardY + 45);
        p.text("Shoot: F key", boardX + boardWidth + 20, boardY + 65);
        p.text("Goal: Hit the blue", boardX + boardWidth + 20, boardY + 85);
        p.text("player with", boardX + boardWidth + 20, boardY + 105);
        p.text("your projectile", boardX + boardWidth + 20, boardY + 125);
        p.text("Score: " + player2.getScore(), boardX + boardWidth + 20, boardY + 150);
    }

    public int getBoardX() {
        return boardX;
    }

    public int getBoardY() {
        return boardY;
    }

    public int getCellSize() {
        return cellSize;
    }

    public int getBoardSize() {
        return boardSize;
    }

    public int getBoardWidth() {
        return boardWidth;
    }

    public int getBoardHeight() {
        return boardHeight;
    }

    public void update(String username, String response) {
        // Update logic if needed
    }
}

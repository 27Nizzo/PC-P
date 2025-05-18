package pt.uminho.pc.states;

import processing.core.PApplet;
import pt.uminho.pc.GUI;
import pt.uminho.pc.State;
import pt.uminho.pc.components.GameBoard;
import pt.uminho.pc.components.GameTimer;
import pt.uminho.pc.components.Player;
import pt.uminho.pc.components.Projectile;

public class PlayState implements GameState {
    private Player player1;
    private Player player2;
    private Projectile player1Projectile;
    private Projectile player2Projectile;
    private GameBoard gameBoard;
    private GameTimer gameTimer;
    private String winnerMessage = "";

    public PlayState(GUI context) {
        player1 = new Player(5, 5, 1, 0, context.color(0, 0, 255), true);
        player2 = new Player(18, 18, -1, 0, context.color(255, 0, 0), false);
        
        player1Projectile = new Projectile(context.color(0, 0, 200));
        player2Projectile = new Projectile(context.color(200, 0, 0));
        
        gameTimer = new GameTimer(120000);
    }
    
    public PlayState() {
        gameTimer = new GameTimer(120000);
    }
    
    public void initializeIfNeeded(GUI context) {
        if (player1 == null) {
            player1 = new Player(5, 5, 1, 0, context.color(0, 0, 255), true);
            player2 = new Player(18, 18, -1, 0, context.color(255, 0, 0), false);
            
            player1Projectile = new Projectile(context.color(0, 0, 200));
            player2Projectile = new Projectile(context.color(200, 0, 0));
        }
    }
    
    @Override
    public void draw(GUI context) {
        initializeIfNeeded(context);
        
        if (gameBoard == null) {
            gameBoard = new GameBoard(15, 24, context.width, context.height);
            gameTimer.start();
        }
        
        if (gameTimer.isTimeUp()) {
            determineWinner();
            context.switchState(State.GAME_OVER);
            return;
        }
        
        context.background(200);
        
        gameBoard.draw(context);
        
        player1.draw(context, gameBoard.getBoardX(), gameBoard.getBoardY(), gameBoard.getCellSize());
        player2.draw(context, gameBoard.getBoardX(), gameBoard.getBoardY(), gameBoard.getCellSize());
        
        player1Projectile.draw(context, gameBoard.getBoardX(), gameBoard.getBoardY(), gameBoard.getCellSize());
        player2Projectile.draw(context, gameBoard.getBoardX(), gameBoard.getBoardY(), gameBoard.getCellSize());
        
        player1Projectile.update(gameBoard.getBoardSize(), player1, player2);
        player2Projectile.update(gameBoard.getBoardSize(), player2, player1);
        
        context.fill(0);
        context.textAlign(PApplet.CENTER);
        context.text("Player 1: " + player1.getScore() + " | Player 2: " + player2.getScore(), 
               context.width/2, gameBoard.getBoardY() + gameBoard.getBoardHeight() + 30);
        
        gameTimer.drawTimer(context, context.width/2, gameBoard.getBoardY() - 10);
        
        gameBoard.drawInstructionPanels(context, player1, player2);
        
        context.textAlign(PApplet.LEFT);
    }
    
    @Override
    public void mousePressed(GUI context) {
        // unused
    }
    
    @Override
    public void keyPressed(GUI context, int keyCode, char key) {
        if (keyCode == PApplet.UP) {
            player1.moveUp();
        } else if (keyCode == PApplet.DOWN) {
            player1.moveDown();
        } else if (keyCode == PApplet.LEFT) {
            player1.moveLeft();
        } else if (keyCode == PApplet.RIGHT) {
            player1.moveRight();
        } else if (key == ' ' && !player1Projectile.isActive()) {
            // Player 1 shoots with space bar
            player1Projectile.fire(player1.getX(), player1.getY(), 
                                  player1.getLastDirX(), player1.getLastDirY());
        }
        
        if (key == 'w') {
            player2.moveUp();
        } else if (key == 's') {
            player2.moveDown();
        } else if (key == 'a') {
            player2.moveLeft();
        } else if (key == 'd') {
            player2.moveRight();
        } else if (key == 'f' && !player2Projectile.isActive()) {
            player2Projectile.fire(player2.getX(), player2.getY(), 
                                  player2.getLastDirX(), player2.getLastDirY());
        }
    }
    
    private void determineWinner() {
        if (player1.getScore() > player2.getScore()) {
            winnerMessage = "PLAYER 1 (BLUE) WINS!";
        } else if (player2.getScore() > player1.getScore()) {
            winnerMessage = "PLAYER 2 (RED) WINS!";
        } else {
            winnerMessage = "IT'S A TIE!";
        }
    }
    
    public void reset() {
        if (player1 == null || player2 == null) {
            return;
        }
        
        player1.reset(5, 5);
        player2.reset(18, 18);
        player1Projectile.reset();
        player2Projectile.reset();
        gameTimer.reset();
        winnerMessage = "";
    }
    
    public String getWinnerMessage() {
        return winnerMessage;
    }
    
    public Player getPlayer1() {
        return player1;
    }
    
    public Player getPlayer2() {
        return player2;
    }
} 
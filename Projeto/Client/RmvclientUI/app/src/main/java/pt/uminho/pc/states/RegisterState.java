package pt.uminho.pc.states;

import processing.core.PApplet;
import pt.uminho.pc.GUI;
import pt.uminho.pc.State;
import pt.uminho.pc.courier.Exceptions;

import java.io.IOException;

public class RegisterState implements GameState {
    private boolean usernameActive = false;
    private boolean passwordActive = false;
    
    @Override
    public void draw(GUI context) {
        context.background(200);
        
        // Title
        context.textSize(24);
        context.fill(0);
        context.text("Register New Account", context.width/2 - 120, 40);
        context.textSize(12);
        
        context.fill(0);
        context.text("Username:", 80, 80);
        context.text("Password:", 80, 130);

        // username
        context.stroke(0);
        context.noFill();
        context.rect(200, 60, 200, 30);
        context.fill(0);
        context.text(context.getUsername(), 205, 80);
        if (usernameActive && ((context.frameCount / 30) % 2 == 0)) {
            float cx = 205 + context.textWidth(context.getUsername());
            context.stroke(0);
            context.line(cx, 65, cx, 85);
        }

        // password
        context.stroke(0);
        context.noFill();
        context.rect(200, 110, 200, 30);
        context.fill(0);
        context.text(context.getPassword(), 205, 130);
        if (passwordActive && ((context.frameCount / 30) % 2 == 0)) {
            float cx2 = 205 + context.textWidth(context.getPassword());
            context.stroke(0);
            context.line(cx2, 115, cx2, 135);
        }

        // register button
        boolean registerOver = isMouseOver(context, 75, 200, 100, 30);
        context.fill(registerOver ? 150 : 100);
        context.rect(75, 200, 100, 30);
        context.fill(255);
        context.text("Register", 85, 220);
        
        // back button
        boolean backOver = isMouseOver(context, 200, 200, 100, 30);
        context.fill(backOver ? 150 : 100);
        context.rect(200, 200, 100, 30);
        context.fill(255);
        context.text("Back", 220, 220);
    }
    
    @Override
    public void mousePressed(GUI context) {
        if (isMouseOver(context, 200, 60, 200, 30)) {
            usernameActive = true;
            passwordActive = false;
        } else if (isMouseOver(context, 200, 110, 200, 30)) {
            usernameActive = false;
            passwordActive = true;
        } else if (isMouseOver(context, 75, 200, 100, 30)) {
            if (registerUser(context)) {
                context.switchState(State.MENU);
            }
        } else if (isMouseOver(context, 200, 200, 100, 30)) {
            // Back to menu
            context.setUsername("");
            context.setPassword("");
            context.switchState(State.MENU);
        }
    }
    
    @Override
    public void keyPressed(GUI context, int keyCode, char key) {
        if (keyCode == PApplet.BACKSPACE) {
            if (passwordActive && !context.getPassword().isEmpty()) {
                context.setPassword(context.getPassword().substring(0, context.getPassword().length() - 1));
            } else if (usernameActive && !context.getUsername().isEmpty()) {
                context.setUsername(context.getUsername().substring(0, context.getUsername().length() - 1));
            }
        } else if (keyCode != PApplet.ENTER && keyCode != PApplet.RETURN) {
            if (usernameActive && context.getUsername().length() < 20) {
                context.setUsername(context.getUsername() + key);
            } else if (passwordActive && context.getPassword().length() < 20) {
                context.setPassword(context.getPassword() + key);
            }
        }
    }
    
    private boolean registerUser(GUI context) {
        String username = context.getUsername();
        String password = context.getPassword();
        
        if (username.isEmpty() || password.isEmpty()) {
            context.showError("Username and password cannot be empty");
            return false;
        }
        
        try {
            context.registerAccount(username, password);
            return true;
        } catch (IOException e) {
            context.showError(e.getMessage());
        } catch (Exceptions.InvalidPassword e) {
            context.showError("Invalid password format");
        } catch (Exceptions.UserExists e) {
            context.showError("Username already exists");
        }
        
        return false;
    }
    
    private boolean isMouseOver(GUI context, int x, int y, int w, int h) {
        return context.mouseX >= x && context.mouseX <= x + w && 
               context.mouseY >= y && context.mouseY <= y + h;
    }
} 
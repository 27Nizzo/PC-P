package Client;



public class GameObjectsColors {
    public float x;
    public float y;
    public String username;
    public int r, g, b;
    public int mass;
    public boolean isPlayer;
    
    public GameObjectsColors(float x, float y, String username, int mass, boolean isPlayer) {
        this.username = username;
        this.x = x;
        this.y = y;
        this.mass = mass;
        this.isPlayer = isPlayer;

        if (isPlayer) {
            setPlayerColor(username);            
        }
    }

    public GameObjectsColors(ModifierColor modifierType, float x, float y, int mass) {
        this.username = "<>";
        this.x = x;
        this.y = y;
        this.mass = mass;
        this.isPlayer = false;
        setModifierColor(modifierType);
    }


    private void setPlayerColor(String username) {
        if(username.equals("Player1")) {
            // Roxo
            this.r = 128;
            this.g = 0;
            this.b = 128;
        } else if(username.equals("Player2")) {
            // Indigo
            this.r = 75;
            this.g = 0;
            this.b = 130;
        } else {
            // Generico
            this.r = 255;
            this.g = 255;
            this.b = 255;
        }

    }

    private void setModifierColor(ModifierColor modifierType) {
        switch (modifierType) {
            case RED:
                this.r = 255;
                this.g = 0;
                this.b = 0;
                break;
            case GREEN:
                this.r = 0;
                this.g = 255;
                this.b = 0;
                break;
            case BLUE:
                this.r = 0;
                this.g = 0;
                this.b = 255;
                break;
            case ORANGE:
                this.r = 255;
                this.g = 165;
                this.b = 0;
                break;
        }
    }

    public enum ModifierColor {
        RED,
        BLUE,
        GREEN,
        ORANGE
    }
    
}

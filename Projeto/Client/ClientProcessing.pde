Client client;
String serverResponse = "";

void setup() {
  size(600, 400);
  try {
    client = new Client("localhost", 1234);
    client.send("INFO");
    serverResponse = client.receive();
  } catch (Exception e) {
    serverResponse = "Erro: " + e.getMessage();
  }
}

void draw() {
  background(30);
  fill(255);
  textAlign(LEFT, TOP);
  text("Resposta do servidor:", 20, 20);
  text(serverResponse, 20, 40);
}

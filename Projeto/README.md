
# Projeto de Programação Concorrente — Mini-jogo "Duelo"

**Unidade Curricular:** Programação Concorrente  
**Instituição:** Universidade do Minho  
**Data de Entrega:** até 18 de maio de 2025, 23h59  
**Apresentação:** a confirmar (entre 30 de maio e 5 de junho de 2025)  
**Grupo:** até 4 elementos

## 📋 Descrição

Este projeto consiste na implementação de um mini-jogo 2D chamado **Duelo**, onde múltiplos jogadores interagem através de uma **interface gráfica em Java**, comunicando com um **servidor em Erlang**. O jogo inclui movimentação, combate com projéteis, modificadores, e progressão por níveis.

## 🧩 Arquitetura Geral

- **Cliente (Java)**:
  - Interface gráfica (sugestão: [Processing](http://processing.org))
  - Comunicação via sockets TCP

- **Servidor (Erlang)**:
  - Simula o mundo do jogo 
  - Garante autenticidade e lógica das interações 
  - Gera respostas que sincronizam o estado do jogo com os clientes 

## 🎮 Funcionalidades do Jogo

### 1. Autenticação e Gestão de Utilizadores
- Registo com `username` e `password` : CHECK
- Login e verificação de credenciais : CHECK
- Remoção de conta : CHECK
- Ficheiro persistente para guardar dados (cliente auxiliar) : MAIS OU MENOS
- Jogadores começam no nível 1 : CHECK

### 2. Progressão de Nível
- Sobe de nível após ganhar `n` partidas consecutivas (nível n)
- Desce se perder `⌈n/2⌉` partidas consecutivas (nível mínimo = 1)

### 3. Matchmaking e Partidas
- Cada partida tem 2 jogadores com no máximo 1 nível de diferença
- Jogadores entram em espera ao pedir para jogar
- Várias partidas podem decorrer em simultâneo

### 4. Mundo 2D
- Espaço retangular, limitado por paredes
- Avatares em forma de círculo, posicionados inicialmente em lados opostos

### 5. Mecânica de Movimento
- Controlado por teclas (cima, baixo, esquerda, direita)
- Aceleração progressiva com inércia
- Velocidade máxima predefinida

### 6. Projéteis
- Cada jogador pode disparar periodicamente
- Direção definida pelo cursor do rato
- Projéteis com velocidade fixa

### 7. Modificadores (Power-ups) A FAZER
- Aparecem aleatoriamente no mapa
- Tipos:
  - **Verde**: aumenta velocidade do projétil (decresce com o tempo)
  - **Laranja**: diminui velocidade do projétil
  - **Azul**: diminui tempo entre disparos (volta ao normal)
  - **Vermelho**: aumenta tempo entre disparos
- Ao serem apanhados, aplicam efeito ao jogador

### 8. Colisões
- Jogador atingido por projétil → +1 ponto para o adversário
- Jogador encosta numa parede → +2 pontos para o adversário e reset de posições
- Projétil atinge borda → destruído
- Projétil atinge modificador ou jogador → aplica efeito
- Colisões entre jogadores e entre projéteis/modificadores → ignoradas

### 9. Pontuação e Fim de Partida
- Duração de cada partida: 2 minutos
- Jogador com maior pontuação vence
- Empates não afetam níveis nem top 10

### 10. Top 10 Jogadores
- Ordenado por nível e sequência atual de vitórias/derrotas

## 🗂️ Entrega

- Código-fonte (cliente e servidor)
- Relatório (máximo 6 páginas, PDF)
- Apresentação oral (datas a confirmar)

---

## 📞 Contacto

Grupo de Sistemas Distribuídos  
Departamento de Informática  
Universidade do Minho


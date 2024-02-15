package main.java.game.server;

import com.badlogic.gdx.ApplicationAdapter;
import main.java.game.server.utils.SerializationUtils;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Server extends ApplicationAdapter {
    public static final int PORT = 3000;
    public Labyrinth labyrinth;
    public List<ClientHandler> clients = new ArrayList<>();
    public Map<String, Player> playersMap = new HashMap<>();

    public Server() {
        this.start();
    }

    public void start() {
        try (ServerSocket serverSocket = new ServerSocket(PORT)) {
            System.out.println("Server started on port " + PORT);
            while (true) {
                Socket clientSocket = serverSocket.accept(); // fica voltando para cá a cada novo accept de socket
                System.out.println("passou do accept");
                System.out.println("New client connected: " + clientSocket);
                ClientHandler clientHandler = new ClientHandler(clientSocket);
                clients.add(clientHandler);
                // enviando labirinto
                clientHandler.start();
            }
        } catch (IOException e) {
            System.out.println("catch do server socket");
            e.printStackTrace();
        }
    }

    public class ClientHandler extends Thread {
        public Socket socket;
        public BufferedReader in;
        public PrintWriter out;
        public Player clientPlayer;

        public ClientHandler(Socket socket) {
            this.socket = socket;
        }

        @Override
        public void run() {
            try {
                System.out.println("ENTRO no TRY DO RUN - inicio da buffer de input e do stream de saida");
                this.in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
                this.out = new PrintWriter(socket.getOutputStream(), true);

                // enviando o labirinto ao client
                labyrinth = new Labyrinth(20, 20); // Supondo que você tenha um construtor padrão para Labyrinth
                String jsonLabyrinth = SerializationUtils.serializeLabyrinth(labyrinth);
                this.out.println(jsonLabyrinth);

                // enviando player ao client
                Player player = new Player(labyrinth);
                this.clientPlayer = player;
                String jsonPlayer = SerializationUtils.serializePlayer(player);
                this.out.println(jsonPlayer); // enviando player para o cliente fazendo login

                //enviando a lista de players para p novo client
                playersMap.put(player.id, player);// Atualiza o jogador no HashMap
                String playersMapJsonInicial = SerializationUtils.serializePlayersMap(new HashMap<>(playersMap));
                this.out.println(playersMapJsonInicial);
                while (true) {
                    updatePlayers();
                }
                // update player aguarda inputs nos clients e redistribui as atualizações
            } catch (IOException e) {
                System.out.println("Caiu no catch do CLient hanler");
                e.printStackTrace();
            } finally {
                try {
                    clients.remove(this);
                    playersMap.remove(clientPlayer.id);
                    socket.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }

        private void updatePlayers() {
            try {
                if (in.ready()) {
                    String input = in.readLine();
                    // Processar os dados recebidos
                    System.out.println("Rodando atualização de jogadores");
                    Player updatedPlayer = SerializationUtils.deserializePlayer(input, Player.class);
                    playersMap.put(updatedPlayer.id, updatedPlayer);
                    String playersMapJson = SerializationUtils.serializePlayersMap(new HashMap<>(playersMap));
                    for (ClientHandler client : clients) {
                        client.out.println(playersMapJson);
                    }
                }
            } catch (IOException e) {
                // A conexão foi fechada ou ocorreu um erro de comunicação
                System.out.println("Entrou no CATCH de UpdatePlayer");
                e.printStackTrace();
            }
        }
    }
}
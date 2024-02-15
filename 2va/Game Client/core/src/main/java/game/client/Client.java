package main.java.game.client;

import com.badlogic.gdx.ApplicationAdapter;
import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.graphics.OrthographicCamera;
import com.badlogic.gdx.graphics.Texture;
import com.badlogic.gdx.graphics.g2d.SpriteBatch;
import com.badlogic.gdx.math.Rectangle;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;

import com.badlogic.gdx.math.Vector3;
import com.google.gson.reflect.TypeToken;
import com.badlogic.gdx.utils.Array;
import com.badlogic.gdx.utils.ScreenUtils;
import main.java.game.client.utils.SerializationUtils;

import java.lang.reflect.Type;
import java.util.*;

public class Client extends ApplicationAdapter {
    public static final int PORT = 3000;
    public static final String SERVERADRESS = "localhost";
    public Socket socket;
    public BufferedReader in;
    public PrintWriter out;
    public OrthographicCamera camera;
    public SpriteBatch batch;
    public Labyrinth labyrinth = null;
    public Map<String, Player> playersMap = null;
    public Player player = null;
    public Texture wallImage;
    public Array<Rectangle> walls = null;
    public float cameraWidth = 640;
    public float cameraHeight = 640;

    @Override
    public void create() {
        try {
            System.out.println("ENTRO no TRY DO CREATE");
            wallImage = new Texture(Gdx.files.internal("wall.png"));
            camera = new OrthographicCamera();
            camera.setToOrtho(false, cameraWidth, cameraHeight);
            batch = new SpriteBatch();

            socket = new Socket(SERVERADRESS, PORT); // Conecta ao localhost na porta 8080
            socket.setSoTimeout(15000);
            in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
            out = new PrintWriter(socket.getOutputStream(), true);
        } catch (IOException e) {
            System.out.println("ENTROU no catch DO CREATE");
            e.printStackTrace();
        }

    }

    public void createLabyrinth() {
        try {
            if (in.ready()) {
                String Labyrinthjson = in.readLine();
                if (Labyrinthjson == null) return;
                Labyrinth obj = SerializationUtils.deserializeLabyrinth(Labyrinthjson, Labyrinth.class);
                if (obj == null) return;
                System.out.println("Labirinto chegou: " + obj);
                labyrinth = obj;
                walls = new Array<>();
                for (int x = 0; x < labyrinth.width; x++) {
                    for (int y = 0; y < labyrinth.height; y++) {
                        if (labyrinth.walls[x][y]) {
                            Rectangle wall = new Rectangle();
                            wall.x = x * 32;
                            wall.y = y * 32;
                            wall.width = 32;
                            wall.height = 32;
                            walls.add(wall);
                        }
                    }
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void createPlayer() {
        try {
            if (in.ready()) {
                String playerJson = in.readLine();
                if (playerJson == null) return;
                Player obj = SerializationUtils.deserializePlayer(playerJson, Player.class);
                if (obj == null) return;
                System.out.println("Player chegou: " + obj);
                player = obj;
                // a lista de players sera atualizada pelo server, e não pelo Client que receberá em UpdatePlayers
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void updatePlayers() {
        // carregando os players do server
        try {
            if (in.ready()) {
                String input = in.readLine();
                System.out.println("travou aqui: " + input);
                if (input == null) return;
                System.out.println("saiu daaqui 1: " + input);
                System.out.println("nova informação dos players chegou");
                String allPlayersJson = input;
                System.out.println("o que chegou no updatePLayers: " + allPlayersJson);
                Class<? extends HashMap<String, Player>> classMap = (Class<? extends HashMap<String, Player>>) (new HashMap<String, Player>()).getClass();
                Map<String, Player> allUpdatedPlayers = SerializationUtils.deserializePlayersMap(allPlayersJson, classMap);
                // Depurar o conteúdo do LinkedTreeMap
                playersMap = new HashMap<>();
                // Continuar com a lógica de conversão para objetos Player
                for (Map.Entry<String, Player> entry : allUpdatedPlayers.entrySet()) {
                    Player p = entry.getValue();
                    String playerId = entry.getKey();
                    System.out.println("player id: " + playerId);
                    playersMap.put(playerId, p);
                }
            }
        } catch (IOException e) {
            System.out.println("Entrouno catch update players");
            e.printStackTrace();
        }
    }

    public void update() {
        // roda apenas no inicio
        if (labyrinth == null || player == null || playersMap == null) {
            if (labyrinth == null) {
                // PRIMEIRO dado enviado no create do server
                createLabyrinth();
            }
            if (player == null) {
                // SEGUNDO dado enviado no create do server - Player com id da THREAD do server
                createPlayer();
            }
            if (playersMap == null) {
                // TERCEIRO dado enviado no create do server - HashMap com Players atualizando players no client
                updatePlayers();
            }

        }
        if (player != null) {
            System.out.println("Testando input de movimento");
            boolean moved = player.move();
            if (moved) {
                // envia dados se se moveu
                if (player.body.x < 0)
                    player.body.x = 0;
                else if (player.body.x > cameraWidth)
                    player.body.x = cameraWidth;
                if (player.body.y < 0)
                    player.body.y = 0;
                else if (player.body.y > cameraHeight)
                    player.body.y = cameraHeight;
                String playerUpdated = SerializationUtils.serializePlayer(player);
                out.println(playerUpdated);
                System.out.println("Player enviou posição atual");
            }
        }
        if (playersMap != null)
            updatePlayers();
    }

    @Override
    public void render() {
        update();
        ScreenUtils.clear(0, 0, 0.1f, 1);
        if (labyrinth != null && player != null && playersMap != null) {
            camera.update();
            batch.setProjectionMatrix(camera.combined);
            batch.begin();
            for (Rectangle wall : walls) {
                batch.draw(wallImage, wall.x, wall.y);
            }
            for (Player p : playersMap.values()) {
                p.draw(batch);
            }
            batch.end();
            System.out.println("CARREGOU O RENDER");
        }
    }

    public void dispose() {
        wallImage.dispose();
        batch.dispose();
        if (socket != null) {
            try {
                socket.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }
}
package main.java.game.server.utils;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.reflect.TypeToken;
import main.java.game.server.Labyrinth;
import main.java.game.server.Player;

import java.lang.reflect.Type;
import java.util.HashMap;
import java.util.Map;

public class SerializationUtils {
    public static String serializePlayer(Player player) {
        Gson gson = new GsonBuilder().serializeNulls().create();
        System.out.println("Serialize player: " + player.toString());
        String json = gson.toJson(player);
        System.out.println("Serialize versao json " + json);
        return json;
    }

    public static String serializeLabyrinth(Labyrinth labyrinth) {
        Gson gson = new GsonBuilder().serializeNulls().create();
        System.out.println("Serialize player: " + labyrinth.toString());
        String json = gson.toJson(labyrinth);
        System.out.println("Serialize versao json " + json);
        return json;
    }

    public static String serializePlayersMap(HashMap<String, Player> playersMap) {
        Gson gson = new GsonBuilder()
                .registerTypeAdapter(new TypeToken<Map<String, Player>>() {
                }
                        .getType(), new MapPlayerAdapter())
                .create();
        System.out.println("Serialize playersMap: " + playersMap.toString());
        String json = gson.toJson(playersMap);
        System.out.println("Serialize versao json " + json);
        return json;
    }

    public static <Labyrinth> Labyrinth deserializeLabyrinth(String json, Class<Labyrinth> labyrinthClass) {
        Gson gson = new GsonBuilder().create();
        System.out.println("deserialize " + json);
        Labyrinth labyrinth = gson.fromJson(json, labyrinthClass);
        System.out.println("Serialize versao objeto " + labyrinth);
        return labyrinth;
    }

    public static <Player> Player deserializePlayer(String json, Class<Player> playerClass) {
        Gson gson = new GsonBuilder().create();
        System.out.println("deserialize " + json);
        Player player = gson.fromJson(json, playerClass);
        System.out.println("Serialize versao objeto " + player);
        return player;
    }

    public static Map<String, Player> deserializePlayersMap(String json, Class<? extends Map<String, Player>> playersMapClass) {
        Gson gson = new GsonBuilder()
                .registerTypeAdapter(new TypeToken<Map<String, Player>>() {
                }
                        .getType(), new MapPlayerAdapter())
                .create();
        System.out.println("deserialize " + json);
        Map<String, Player> playersMap = gson.fromJson(json, playersMapClass);
        System.out.println("Serialize versao objeto " + playersMap);
        return playersMap;
    }
//    public static String serialize(Object object) {
//        Gson gson = new GsonBuilder().serializeNulls().create();
//        System.out.println("Serialize " + object.toString());
//        String json = gson.toJson(object);
//        System.out.println("Serialize versao json " + json);
//        return json;
//    }
    // Desserialize um JSON para objeto
//    public static <T> T deserialize(String json, Class<T> clazz) {
//        Gson gson = new GsonBuilder().create();
//        System.out.println("deserialize " + json);
//        T obj = gson.fromJson(json, clazz);
//        System.out.println("Serialize versao objeto " + obj);
//        return obj;
//    }
//public static <T> T deserializeMap(String json, Type type) {
//    Gson gson = new GsonBuilder().create();
//    return gson.fromJson(json, type);
//}
}

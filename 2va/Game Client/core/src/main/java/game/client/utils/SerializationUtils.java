package main.java.game.client.utils;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonElement;
import com.google.gson.internal.LinkedTreeMap;
import com.google.gson.reflect.TypeToken;
import main.java.game.client.Labyrinth;
import main.java.game.client.Player;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

public class SerializationUtils {
    public static String serializePlayer(Player player) {
        Gson gson = new GsonBuilder().serializeNulls().create();
        String json = gson.toJson(player);
        System.out.println("Serialize versao json do player" + json);
        return json;
    }

    public static String serializeLabyrinth(Labyrinth labyrinth) {
        Gson gson = new GsonBuilder().serializeNulls().create();
        String json = gson.toJson(labyrinth);
        System.out.println("Serialize versao json labyrinth " + json);
        return json;
    }

    public static String serializePlayersMap(Map<String, Player> playersMap) {
        Gson gson = new GsonBuilder().serializeNulls()
                .registerTypeAdapter(new TypeToken<Map<String, Player>>() {
                }
                        .getType(), new MapPlayerAdapter())
                .create();
        String json = gson.toJson(playersMap);
        System.out.println("Serialize versao json do playertsMap" + json);
        return json;
    }

    public static <Labyrinth> Labyrinth deserializeLabyrinth(String json, Class<Labyrinth> labyrinthClass) {
        Gson gson = new GsonBuilder().create();
        System.out.println("deserialize Labyrinth " + json);
        Labyrinth labyrinth = gson.fromJson(json, labyrinthClass);
        return labyrinth;
    }

    public static <Player> Player deserializePlayer(String json, Class<Player> playerClass) {
        Gson gson = new GsonBuilder().create();
        System.out.println("deserialize player" + json);
        Player player = gson.fromJson(json, playerClass);
        return player;
    }

    public static Map<String, Player> deserializePlayersMap(String json, Class<? extends HashMap<String, Player>> playersMapClass) {
        Gson gson = new GsonBuilder()
                .registerTypeAdapter(new TypeToken<Map<String, Player>>() {
                }.getType(), new MapPlayerAdapter())
                .create();
        System.out.println("deserialize PlayersMap " + json);
        Map<String, Player> playersWithLinkedTreeMap = gson.fromJson(json, playersMapClass);
        Map<String, Player> result = new HashMap<>();
        for (Map.Entry<String, Player> entry : playersWithLinkedTreeMap.entrySet()) {
            String linkedtreeMapSerializada = serialize(entry.getValue());
            Player entryPlayer = deserializePlayer(linkedtreeMapSerializada, Player.class);
            String entryKey = entry.getKey();
            result.put(entryKey, entryPlayer);
        }
        return result;
    }

    public static String serialize(Object object) {
        Gson gson = new GsonBuilder().serializeNulls().create();
        System.out.println("Serialize " + object.toString());
        String json = gson.toJson(object);
        System.out.println("Serialize versao json " + json);
        return json;
    }
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

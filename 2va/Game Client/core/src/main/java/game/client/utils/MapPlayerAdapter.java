package main.java.game.client.utils;

import com.google.gson.*;
import main.java.game.client.Player;

import java.lang.reflect.Type;
import java.util.HashMap;
import java.util.Map;

public class MapPlayerAdapter implements JsonSerializer<Map<String, Player>>, JsonDeserializer<Map<String, Player>> {

    @Override
    public JsonElement serialize(Map<String, Player> src, Type typeOfSrc, JsonSerializationContext context) {
        JsonObject result = new JsonObject();
        for (Map.Entry<String, Player> entry : src.entrySet()) {
            result.add(entry.getKey(), context.serialize(entry.getValue()));
        }
        return result;
    }

    @Override
    public Map<String, Player> deserialize(JsonElement json, Type typeOfT, JsonDeserializationContext context) throws JsonParseException {
        System.out.println("Using MapPlayerAdapter for deserialization");

        JsonObject jsonObject = json.getAsJsonObject();
        Map<String, Player> result = new HashMap<>();
        for (Map.Entry<String, JsonElement> entry : jsonObject.entrySet()) {
            String key = entry.getKey();
            JsonElement value = entry.getValue();
            System.out.println("Deserializing entry with key: " + key + " and value: " + value);
            result.put(key, context.deserialize(value, Player.class));
        }
        return result;
    }
}

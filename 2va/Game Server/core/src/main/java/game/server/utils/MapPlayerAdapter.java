package main.java.game.server.utils;

import com.google.gson.*;
import main.java.game.server.Player;

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
        JsonObject jsonObject = json.getAsJsonObject();
        Map<String, Player> result = new HashMap<>();
        for (Map.Entry<String, JsonElement> entry : jsonObject.entrySet()) {
            result.put(entry.getKey(), context.deserialize(entry.getValue(), Player.class));
        }
        return result;
    }
}

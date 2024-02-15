package main.java.game.client;

import com.badlogic.gdx.math.Vector3;
import com.google.gson.annotations.Expose;

import java.util.Random;

public class Labyrinth {
    @Expose(deserialize = true)
    public int width, height;
    @Expose(deserialize = true)
    public boolean[][] walls;
    @Expose(deserialize = true)
    public Vector3 exit;

    public Vector3 randomPosition() {
        int x, y;
        x= 20;
        y = 40;
//        do {
//            x = random.nextInt(width);
//            y = random.nextInt(height);
//        } while (walls[x][y]); // Garante que a posição não está dentro de uma parede
        return new Vector3(x, y, 0);
    }
}

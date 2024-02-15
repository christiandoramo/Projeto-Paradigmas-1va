package main.java.game.server;

import com.badlogic.gdx.math.Vector3;
import com.google.gson.annotations.Expose;

public class Labyrinth {
    @Expose(deserialize = true)
    public int width, height;
    @Expose(deserialize = true)
    public boolean[][] walls;
    @Expose(deserialize = true)
    public Vector3 exit;

    public Labyrinth(int width, int height) {
        this.width = width;
        this.height = height;
        walls = new boolean[width][height];
        // Crie um padrão em zigue-zague
        for (int x = 0; x < width; x++) {
            for (int y = 0; y < height; y++) {
                if (x % 3 == 0 && y % 3 == 0) {
                    walls[x][y] = true;
                }
            }
        }

        exit = new Vector3(width - 2, height - 2, 0); // por exemplo
    }

    public Vector3 randomPosition() {
        int x, y;
        x = 40;
        y = 50;
//        do {
//            x = random.nextInt(width);
//            y = random.nextInt(height);
//        } while (walls[x][y]); // Garante que a posição não está dentro de uma parede
        return new Vector3(x, y, 0);
    }
}

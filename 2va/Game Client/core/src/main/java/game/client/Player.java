package main.java.game.client;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.Input;
import com.badlogic.gdx.graphics.Texture;
import com.badlogic.gdx.graphics.g2d.SpriteBatch;
import com.badlogic.gdx.math.Rectangle;
import com.badlogic.gdx.math.Vector3;
import com.google.gson.annotations.Expose;
import com.google.gson.internal.LinkedTreeMap;

public class Player {
    @Expose(deserialize = true)
    public int life;
    @Expose(deserialize = true)
    public Vector3 position = new Vector3(0, 0, 0);
    @Expose(deserialize = true)
    public Rectangle body;
    @Expose(deserialize = true)
    public String id;

    public Player(Labyrinth labyrinth) {
        this.body = new Rectangle();
        this.body.x = position.x;
        this.body.y = position.y;
        this.body.width = 16;
        this.body.height = 16;
        this.life = 100;
    }

    public Player(LinkedTreeMap labyrinth) {
        this.body = new Rectangle();
        this.body.x = position.x;
        this.body.y = position.y;
        this.body.width = 16;
        this.body.height = 16;
        this.life = 100;
    }

    public void draw(SpriteBatch batch) {
        batch.draw(new Texture(Gdx.files.internal("player.png")), this.body.x, this.body.y);
    }

    public Boolean move() {
        boolean moved = false;
        if (Gdx.input.isKeyPressed(Input.Keys.LEFT)) {
            this.body.x -= 200 * Gdx.graphics.getDeltaTime();
            moved = true;
        }
        if (Gdx.input.isKeyPressed(Input.Keys.RIGHT)) {
            this.body.x += 200 * Gdx.graphics.getDeltaTime();
            moved = true;
        }
        if (Gdx.input.isKeyPressed(Input.Keys.DOWN)) {
            this.body.y -= 200 * Gdx.graphics.getDeltaTime();
            moved = true;
        }
        if (Gdx.input.isKeyPressed(Input.Keys.UP)) {
            this.body.y += 200 * Gdx.graphics.getDeltaTime();
            moved = true;
        }
        return moved;
    }
}

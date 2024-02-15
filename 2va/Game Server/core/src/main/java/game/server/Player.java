package main.java.game.server;
//import java.util.UUID;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.Input;
import com.badlogic.gdx.graphics.Texture;
import com.badlogic.gdx.graphics.g2d.SpriteBatch;
import com.badlogic.gdx.math.Rectangle;
import com.badlogic.gdx.math.Vector3;
import com.google.gson.annotations.Expose;

import java.util.UUID;
//import com.badlogic.gdx.Gdx.*;

public class Player {
    @Expose(deserialize = true)
    public int life;
    @Expose(deserialize = true)
    public Rectangle body;
    @Expose(deserialize = true)
    public String id;

    public Player(Labyrinth labyrinth) {
        this.life = 100;
        this.id = UUID.randomUUID().toString();
        this.body = new Rectangle();
        Vector3 initialPosition = labyrinth.randomPosition();
        this.body.x = initialPosition.x;
        this.body.y = initialPosition.y;
        this.body.width = 16;
        this.body.height = 16;
    }

    public void move() {
        if (Gdx.input.isKeyPressed(Input.Keys.LEFT)) this.body.x -= 200 * Gdx.graphics.getDeltaTime();
        if (Gdx.input.isKeyPressed(Input.Keys.RIGHT)) this.body.x += 200 * Gdx.graphics.getDeltaTime();
        if (Gdx.input.isKeyPressed(Input.Keys.DOWN)) this.body.y -= 200 * Gdx.graphics.getDeltaTime();
        if (Gdx.input.isKeyPressed(Input.Keys.UP)) this.body.y += 200 * Gdx.graphics.getDeltaTime();
    }
}

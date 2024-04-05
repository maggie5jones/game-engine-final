use assets_manager::{asset::Png, AssetCache};
use frenderer::{
    input::{Input, Key},
    sprites::{Camera2D, SheetRegion, Transform},
    wgpu, Renderer,
};
use rand::Rng;
mod geom;
mod grid;
use geom::*;
mod level;
use level::Level;

#[derive(Debug, PartialEq, Eq)]
enum EntityType {
    Player,
    Enemy,
    // which level, grid x in dest level, grid y in dest level
    #[allow(dead_code)]
    Door(String, u16, u16),
}

#[derive(Clone, Copy, Debug)]
pub struct TileData {
    solid: bool,
    sheet_region: SheetRegion,
}

#[derive(Clone, Debug)]
struct Contact {
    displacement: Vec2,
    a_index: usize,
    _a_rect: Rect,
    b_index: usize,
    b_rect: Rect,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
enum Dir {
    N,
    E,
    S,
    W,
}
const PLAYER: [SheetRegion; 4] = [
    //n, e, s, w
    SheetRegion::rect(461 + 16 * 2, 39, 16, 16),
    SheetRegion::rect(461, 39, 16, 16),
    SheetRegion::rect(461 + 16 * 3, 39, 16, 16),
    SheetRegion::rect(461 + 16, 39, 16, 16),
];
const PLAYER_ATK: [SheetRegion; 4] = [
    //n, e, s, w
    SheetRegion::rect(428, 0, 16, 8), // offset by 8px in direction
    SheetRegion::rect(349, 22, 8, 16),
    SheetRegion::rect(162, 13, 16, 8),
    SheetRegion::rect(549, 17, 8, 16),
];
const ENEMY: [SheetRegion; 4] = [
    SheetRegion::rect(533 + 16 * 2, 39, 16, 16),
    SheetRegion::rect(533 + 16 * 1, 39, 16, 16),
    SheetRegion::rect(533, 39, 16, 16),
    SheetRegion::rect(533 + 16 * 3, 39, 16, 16),
];

const HEART: SheetRegion = SheetRegion::rect(525, 35, 8, 8);

impl Dir {
    fn to_vec2(self) -> Vec2 {
        match self {
            Dir::N => Vec2 { x: 0.0, y: 1.0 },
            Dir::E => Vec2 { x: 1.0, y: 0.0 },
            Dir::S => Vec2 { x: 0.0, y: -1.0 },
            Dir::W => Vec2 { x: -1.0, y: 0.0 },
        }
    }
}
#[derive(Clone, Copy, Debug, PartialEq)]
struct Pos {
    pos: Vec2,
    dir: Dir,
}
struct Game {
    camera: Camera2D,
    current_level: usize,
    levels: Vec<Level>,
    enemies: Vec<(Pos, usize)>,
    player: Pos,
    attack_area: Rect,
    attack_timer: f32,
    knockback_timer: f32,
    health: u8,
    paused: bool,
}

// Feel free to change this if you use a different tilesheet
const TILE_SZ: usize = 16;
const W: usize = 220; // 320
const H: usize = 140; // 240
const SCREEN_FAST_MARGIN: f32 = 64.0;

// pixels per second
const PLAYER_SPEED: f32 = 64.0;
const ENEMY_SPEED: f32 = 32.0;
const KNOCKBACK_SPEED: f32 = 128.0;

const ATTACK_MAX_TIME: f32 = 0.3;
const ATTACK_COOLDOWN_TIME: f32 = 0.1;
const KNOCKBACK_TIME: f32 = 0.25;

const DT: f32 = 1.0 / 60.0;

fn main() {
    #[cfg(not(target_arch = "wasm32"))]
    let source =
        assets_manager::source::FileSystem::new("content").expect("Couldn't load resources");
    #[cfg(target_arch = "wasm32")]
    let source = assets_manager::source::Embedded::from(assets_manager::source::embed!("content"));
    let cache = assets_manager::AssetCache::with_source(source);

    let drv = frenderer::Driver::new(
        winit::window::WindowBuilder::new()
            .with_title("test")
            .with_inner_size(winit::dpi::LogicalSize::new(1024.0, 768.0)),
        Some((W as u32, H as u32)),
    );

    let mut input = Input::default();

    let mut now = frenderer::clock::Instant::now();
    let mut acc = 0.0;
    drv.run_event_loop::<(), _>(
        move |window, mut frend| {
            let game = Game::new(&mut frend, &cache);
            (window, game, frend)
        },
        move |event, target, (window, ref mut game, ref mut frend)| {
            use winit::event::{Event, WindowEvent};
            match event {
                Event::WindowEvent {
                    event: WindowEvent::CloseRequested,
                    ..
                } => {
                    target.exit();
                }
                Event::WindowEvent {
                    event: WindowEvent::Resized(size),
                    ..
                } => {
                    if !frend.gpu.is_web() {
                        frend.resize_surface(size.width, size.height);
                    }
                    window.request_redraw();
                }
                Event::WindowEvent {
                    event: WindowEvent::RedrawRequested,
                    ..
                } => {
                    let elapsed = now.elapsed().as_secs_f32();
                    // You can add the time snapping/death spiral prevention stuff here if you want.
                    // I'm not using it here to keep the starter code small.
                    acc += elapsed;
                    now = std::time::Instant::now();
                    // While we have time to spend
                    while acc >= DT {
                        // simulate a frame
                        acc -= DT;
                        game.simulate(&input, DT);
                        input.next_frame();
                    }
                    game.render(frend);
                    frend.render();
                    window.request_redraw();
                }
                event => {
                    input.process_input_event(&event);
                }
            }
        },
    )
    .expect("event loop error");
}

impl Game {
    fn new(renderer: &mut Renderer, cache: &AssetCache) -> Self {
        let tile_handle = cache
            .load::<Png>("texture")
            .expect("Couldn't load tilesheet img");
        let tile_img = tile_handle.read().0.to_rgba8();
        let tile_tex = renderer.create_array_texture(
            &[&tile_img],
            wgpu::TextureFormat::Rgba8UnormSrgb,
            tile_img.dimensions(),
            Some("tiles-sprites"),
        );
        let levels = vec![
            Level::from_str(
                &cache
                    .load::<String>("level3")
                    .expect("Couldn't access level3.txt")
                    .read(),
            ),
            Level::from_str(
                &cache
                    .load::<String>("level1")
                    .expect("Couldn't access level1.txt")
                    .read(),
            ),
            Level::from_str(
                &cache
                    .load::<String>("level2")
                    .expect("Couldn't access level2.txt")
                    .read(),
            ),
        ];
        let current_level = 0;
        let camera = Camera2D {
            screen_pos: [0.0, 0.0],
            screen_size: [W as f32, H as f32],
        };
        let sprite_estimate =
            levels[current_level].sprite_count() + levels[current_level].starts().len();
        renderer.sprite_group_add(
            &tile_tex,
            vec![Transform::ZERO; sprite_estimate],
            vec![SheetRegion::ZERO; sprite_estimate],
            camera,
        );
        let player_start = *levels[current_level]
            .starts()
            .iter()
            .find(|(t, _)| *t == EntityType::Player)
            .map(|(_, ploc)| ploc)
            .expect("Start level doesn't put the player anywhere");
        let mut game = Game {
            camera,
            current_level,
            attack_area: Rect {
                x: 0.0,
                y: 0.0,
                w: 0,
                h: 0,
            },
            knockback_timer: 0.0,
            attack_timer: 0.0,
            levels,
            health: 3,
            enemies: vec![],
            player: Pos {
                pos: player_start,
                dir: Dir::S,
            },
            paused: false,
        };
        game.enter_level(player_start);
        game
    }
    fn level(&self) -> &Level {
        &self.levels[self.current_level]
    }
    fn enter_level(&mut self, player_pos: Vec2) {
        self.enemies.clear();
        self.player.pos = player_pos;
        for (etype, pos) in self.levels[self.current_level].starts().iter() {
            match etype {
                EntityType::Player => {}
                EntityType::Door(_rm, _x, _y) => {}
                EntityType::Enemy => self.enemies.push((Pos {
                    pos: *pos,
                    dir: Dir::S,
                }, 1)), // 1 is the health of the enemy
            }
        }
    }
    fn sprite_count(&self) -> usize {
        //todo!("count how many entities and other sprites we have");
        self.level().sprite_count() + self.enemies.len() + 1 + 1 + self.health as usize //+ player + sword + hearts
    }
    fn render(&mut self, frend: &mut Renderer) {
        let mut rng = rand::thread_rng();
        let rand = rng.gen_range(0..1000);
        if rand > 960 {
            let mut randx = rng.gen_range(2..self.levels[self.current_level].width()*TILE_SZ);
            let mut randy = rng.gen_range(2..self.levels[self.current_level].height()*TILE_SZ);
            while ((randx as f32 - self.player.pos.x).abs() < 10.0) && ((randy as f32 - self.player.pos.y).abs() < 10.0)
            && self.level().get_tile_at(Vec2{x:randx as f32, y:randy as f32}).unwrap().solid == false  {
                randx = rng.gen_range(2..self.levels[self.current_level].width()*TILE_SZ);
                randy = rng.gen_range(2..self.levels[self.current_level].height()*TILE_SZ);
            } 
            let monster = Pos {
                pos: Vec2{x: randx as f32, y: randy as f32},
                dir: Dir::S,
            };
            self.enemies.push((monster, 1));
        }
        // make this exactly as big as we need
        frend.sprite_group_resize(0, self.sprite_count());
        frend.sprite_group_set_camera(0, self.camera);

        let sprites_used = self.level().render_into(frend, 0);
        let (sprite_posns, sprite_gfx) = frend.sprites_mut(0, sprites_used..);

        for (enemy, (trf, uv)) in self
            .enemies
            .iter()
            .zip(sprite_posns.iter_mut().zip(sprite_gfx.iter_mut()))
        {
            if enemy.1 == 1 {
                *trf = Transform {
                    w: TILE_SZ as u16,
                    h: TILE_SZ as u16,
                    x: enemy.0.pos.x,
                    y: enemy.0.pos.y,
                    rot: 0.0,
                };
                *uv = ENEMY[enemy.0.dir as usize];
            }
            else {
                *trf = Transform::ZERO;
                *uv = SheetRegion::ZERO;
            }
        }
        let sprite_posns = &mut sprite_posns[self.enemies.len()..];
        let sprite_gfx = &mut sprite_gfx[self.enemies.len()..];
        sprite_posns[0] = Transform {
            w: TILE_SZ as u16,
            h: TILE_SZ as u16,
            x: self.player.pos.x,
            y: self.player.pos.y,
            rot: 0.0,
        };
        sprite_gfx[0] = PLAYER[self.player.dir as usize].with_depth(1);
        if self.attack_area.is_empty() {
            sprite_posns[1] = Transform::ZERO;
        } else {
            let (w, h) = match self.player.dir {
                Dir::N | Dir::S => (16, 8),
                _ => (8, 16),
            };
            let delta = self.player.dir.to_vec2() * 7.0;
            sprite_posns[1] = Transform {
                w,
                h,
                x: self.player.pos.x + delta.x,
                y: self.player.pos.y + delta.y,
                rot: 0.0,
            };
        }
        sprite_gfx[1] = PLAYER_ATK[self.player.dir as usize].with_depth(0);
        // done point: draw hearts
        let heart_pos = Transform {
            w: (TILE_SZ/2) as u16, 
            h: (TILE_SZ/2) as u16,
            x: self.player.pos.x - (TILE_SZ/2 - 2) as f32,
            y: self.player.pos.y + TILE_SZ as f32, 
            rot: 0.0,
        };
        for i in 0..self.health {
            let j = i as usize + 2;
            sprite_posns[j] = Transform {
                x: heart_pos.x + i as f32 * (TILE_SZ/2) as f32,
                ..heart_pos
            };
            sprite_gfx[j] = HEART.with_depth(0);
        }
    }
    fn simulate(&mut self, input: &Input, dt: f32) {
        if input.is_key_pressed(Key::Escape) {
            self.paused = !self.paused;
        }
        if self.paused{
            self.simulate_pause(input, dt);
        }
        if self.attack_timer > 0.0 {
            self.attack_timer -= dt;
        }
        if self.knockback_timer > 0.0 {
            self.knockback_timer -= dt;
        }
        let mut dx = input.key_axis(Key::ArrowLeft, Key::ArrowRight) * PLAYER_SPEED * DT;
        // now down means -y and up means +y!  beware!
        let mut dy = input.key_axis(Key::ArrowDown, Key::ArrowUp) * PLAYER_SPEED * DT;
        let attacking = !self.attack_area.is_empty();
        let knockback = self.knockback_timer > 0.0;
        if attacking {
            // while attacking we can't move
            dx = 0.0;
            dy = 0.0;
        } else if knockback {
            // during knockback we move but don't turn around
            let delta = self.player.dir.to_vec2();
            dx = -delta.x * KNOCKBACK_SPEED * dt;
            dy = -delta.y * KNOCKBACK_SPEED * dt;
        } else {
            // not attacking, no knockback, do normal movement
            if dx > 0.0 {
                self.player.dir = Dir::E;
            }
            if dx < 0.0 {
                self.player.dir = Dir::W;
            }
            if dy > 0.0 {
                self.player.dir = Dir::N;
            }
            if dy < 0.0 {
                self.player.dir = Dir::S;
            }
        }
        if self.attack_timer <= 0.0 && input.is_key_pressed(Key::Space) {
            // compute the attack area's center based on the player's position and facing and some offset
            // For the spritesheet provided, the attack is placed 8px "forwards" from the player.
            self.attack_timer = ATTACK_MAX_TIME;
            self.attack_area = match self.player.dir {
                Dir::N => Rect {
                    x: self.player.pos.x,
                    y: self.player.pos.y + TILE_SZ as f32,
                    w: 2*TILE_SZ as u16,
                    h: 32,
                },
                Dir::E => Rect {
                    x: self.player.pos.x + TILE_SZ as f32,
                    y: self.player.pos.y,
                    w: 32,
                    h: 2*TILE_SZ as u16,
                },
                Dir::S => Rect {
                    x: self.player.pos.x,
                    y: self.player.pos.y - 16.0,
                    w: 2*TILE_SZ as u16,
                    h: 32,
                },
                Dir::W => Rect {
                    x: self.player.pos.x - 16.0,
                    y: self.player.pos.y,
                    w: 32,
                    h: 2*TILE_SZ as u16,
                },
            };
        } else if self.attack_timer <= ATTACK_COOLDOWN_TIME {
            // "turn off" the attack, but the cooldown is still going
            self.attack_area = Rect {
                x: 0.0,
                y: 0.0,
                w: 0,
                h: 0,
            };
        }
        // self.player.pos += Vec2 { x: dx, y: dy };
        let dest = self.player.pos + Vec2 { x: dx, y: dy };
        if self.level().get_tile_at(dest).unwrap().solid == false {
            self.player.pos = dest;
        }
        let mut rng = rand::thread_rng();
        for enemy in self.enemies.iter_mut() {
            if rng.gen_bool(0.05) {
                enemy.0.dir = match rng.gen_range(0..4) {
                    0 => Dir::N,
                    1 => Dir::E,
                    2 => Dir::S,
                    3 => Dir::W,
                    _ => panic!(),
                };
            }
            let enemy_dest = enemy.0.pos + (enemy.0.dir.to_vec2() * ENEMY_SPEED * dt);
            if (enemy_dest.x >= 0.0 && enemy_dest.x <= (self.levels[self.current_level].width()*TILE_SZ) as f32)
             && (enemy_dest.y > 0.0 && enemy_dest.y <= (self.levels[self.current_level].height()*TILE_SZ) as f32) 
            {
                enemy.0.pos = enemy_dest;
            }
        }

        let lw = self.level().width();
        let lh = self.level().height();

        while self.player.pos.x
            > self.camera.screen_pos[0] + self.camera.screen_size[0] - SCREEN_FAST_MARGIN
        {
            self.camera.screen_pos[0] += 1.0;
        }
        while self.player.pos.x < self.camera.screen_pos[0] + SCREEN_FAST_MARGIN {
            self.camera.screen_pos[0] -= 1.0;
        }
        while self.player.pos.y
            > self.camera.screen_pos[1] + self.camera.screen_size[1] - SCREEN_FAST_MARGIN
        {
            self.camera.screen_pos[1] += 1.0;
        }
        while self.player.pos.y < self.camera.screen_pos[1] + SCREEN_FAST_MARGIN {
            self.camera.screen_pos[1] -= 1.0;
        }
        self.camera.screen_pos[0] =
            self.camera.screen_pos[0].clamp(0.0, (lw * TILE_SZ).max(W) as f32 - W as f32);
        self.camera.screen_pos[1] =
            self.camera.screen_pos[1].clamp(0.0, (lh * TILE_SZ).max(H) as f32 - H as f32);

        // implement collision detection here. 
        // for collision with the tilemap, you can use Level::tiles_within to find the tiles touching a rectangle, 
        // and filter out the ones that are not solid.  Then you have rects you can test against your player/enemies.
        // let player_rect = Rect {
        //     x: self.player.pos.x,
        //     y: self.player.pos.y,
        //     w: TILE_SZ as u16,
        //     h: TILE_SZ as u16,
        // };

        // Check for collision with enemies
        // let enemy_rects = self
        //     .enemies
        //     .iter()
        //     .filter(|enemy| enemy.1 > 0)
        //     .map(|enemy| Rect {
        //         x: enemy.0.pos.x,
        //         y: enemy.0.pos.y,
        //         w: TILE_SZ as u16,
        //         h: TILE_SZ as u16,
        //     })
        //     .collect::<Vec<_>>();

        let mut contacts = Vec::new();
        let p_rect = Rect {
            x: self.player.pos.x - (TILE_SZ / 2) as f32,
            y: self.player.pos.y - (TILE_SZ / 2) as f32,
            w: (TILE_SZ/2) as u16,
            h: (TILE_SZ/2) as u16,
        };
        let player = [p_rect, self.attack_area];
        let enemy_rect: Vec<_> = self.enemies.iter().map(|e| make_rect(e.0.pos)).collect();
        generate_contact(&player, &enemy_rect, &mut contacts);
        // dbg!(contacts.len());

        // Tile and Player contacts
        let mut tile_contacts = Vec::new();
        generate_tile_contact(&[player[0]], self.level(), &mut tile_contacts);
        // dbg!(tile_contacts.len());

        // Tile and Enemy contacts
        let mut tile_enemy_contacts = Vec::new();
        generate_tile_contact(&enemy_rect, self.level(), &mut tile_enemy_contacts);
        // dbg!(tile_enemy_contacts.len());

        // TODO POINT: damage/destroy the enemy

        // I suggest gathering player-tile collisions and enemy-tile collisions, then doing collision response on those sets of contacts (it's OK to use two sets of contacts).
        // You could have helper functions like gather_contacts_tiles(&[rect], &Level, &mut Vec<Contact>) and gather_contacts(&[rect], &[rect], &mut Vec<Contact>) or do_collision_response(&[Contact], &mut [rect], &[rect]) or compute_displacement(rect, rect) -> Vec2.
        // A Contact struct is not strictly necessary but it's a good idea (with fields like displacement, a_index, a_rect, b_index, and b_rect fields).
        // Then, you can check for contacts between the player & their attack rectangle on one side, and the enemies on the other side (you can reuse gather_contacts for this).  These don't need to participate in collision response, but you can use them to determine whether the player or enemy should be damaged.

        // Contact Resolution for player vs. world
        tile_contacts.sort_by(|a, b| {
            b.displacement
                .mag_sq()
                .partial_cmp(&a.displacement.mag_sq())
                .unwrap()
        });
        for contact in tile_contacts {
            self.player.pos += find_displacement(p_rect, contact.b_rect);
        }

        // Contact Resolution for enemies vs. world
        tile_enemy_contacts.sort_by(|a, b| {
            b.displacement
                .mag_sq()
                .partial_cmp(&a.displacement.mag_sq())
                .unwrap()
        });
        for contact in tile_enemy_contacts {
            self.enemies[contact.a_index].0.pos +=
                find_displacement(enemy_rect[contact.a_index], contact.b_rect);
        }

        // For deleting enemies, it's best to add the enemy to a "to_remove" vec, and then remove those enemies after this loop is all done.
        contacts.sort_by(|a, b| {
            b.displacement
                .mag_sq()
                .partial_cmp(&a.displacement.mag_sq())
                .unwrap()
        });

        let mut removable = Vec::new();
        for contact in contacts {
            if contact.a_index == 1 && !removable.contains(&contact.b_index){
                self.enemies[contact.b_index].0.pos +=
                    find_displacement(p_rect, enemy_rect[contact.b_index]);
                removable.push(contact.b_index);
            }
            if contact.a_index == 0 {
                if self.knockback_timer == 0.0 {
                    self.knockback_timer = KNOCKBACK_TIME;
                    self.health -= 1;
                    if self.health == 0 {
                        panic!("Game Over!");
                    }
                } else if self.knockback_timer < KNOCKBACK_TIME {
                    // "turn off" the ability to get hit
                    if self.knockback_timer.abs() < 0.02 {
                        self.knockback_timer = 0.0;
                    }
                }
            }
        }
        // Alternatively, you could "disable" an enemy by giving it an `alive` flag or similar and setting that to false, not drawing or updating dead enemies.
        removable.sort();
        for i in removable.iter().rev() {
            self.enemies.swap_remove(*i);
        }
        
    }
    fn simulate_pause(&mut self, input: &Input, dt: f32) {
        if input.is_key_pressed(Key::Escape) {
            self.paused = !self.paused;
        }
        if !self.paused{
            self.simulate(input, dt);
        }
    }
    
}

fn generate_contact(group_a: &[Rect], group_b: &[Rect], contacts: &mut Vec<Contact>) {
    for (a_i, a_rect) in group_a.iter().enumerate() {
        for (b_i, b_rect) in group_b.iter().enumerate() {
            if let Some(overlap) = a_rect.overlap(*b_rect) {
                contacts.push(Contact {
                    displacement: overlap,
                    a_index: a_i,
                    _a_rect: *a_rect,
                    b_index: b_i,
                    b_rect: *b_rect,
                });
            }
        }
    }
}

fn generate_tile_contact(group_a: &[Rect], lvl: &Level, contacts: &mut Vec<Contact>) {
    for (a_i, a_rect) in group_a.iter().enumerate() {
        for (b_rect, _) in lvl.tiles_within(*a_rect).filter(|(_r, td)| td.solid) {
            if let Some(overlap) = a_rect.overlap(b_rect) {
                contacts.push(Contact {
                    displacement: overlap,
                    a_index: a_i,
                    _a_rect: *a_rect,
                    b_index: 0,
                    b_rect,
                });
            }
        }
    }
}

fn find_displacement(a: Rect, b: Rect) -> Vec2 {
    if let Some(mut overlap) = a.overlap(b) {
        if overlap.x < overlap.y {
            overlap.y = 0.0;
        } else {
            overlap.x = 0.0;
        }
        if a.x < b.x {
            overlap.x *= -1.0;
        }
        if a.y < b.y {
            overlap.y *= -1.0;
        }
        overlap
    } else {
        Vec2 { x: 0.0, y: 0.0 }
    }
}

fn make_rect(position: Vec2) -> Rect {
    Rect {
        x: position.x - (TILE_SZ / 2) as f32,
        y: position.y - (TILE_SZ / 2) as f32,
        w: TILE_SZ as u16,
        h: TILE_SZ as u16,
    }
}

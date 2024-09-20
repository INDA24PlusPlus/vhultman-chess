use raylib::prelude::*;
use vhultman_chess::{ChessMove, GameState, PieceType, Position};

const WINDOW_WIDTH: i32 = 1024;
const WINDOW_HEIGHT: i32 = 1024;
const RECT_WIDTH: i32 = WINDOW_WIDTH / 8;

const COLOR_EVEN: u32 = 0xebecd0ff;
const COLOR_ODD: u32 = 0x779556ff;
const COLOR_MOVABLE: u32 = 0xcdcdb4ff;
const COLOR_WHITE_SELECTED: u32 = 0xf5f580ff;
const COLOR_BLACK_SELECTED: u32 = 0xb9ca42ff;

fn main() {
    let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR";

    let mut pos = match Position::from_fen(fen) {
        Ok(p) => p,
        Err(e) => panic!("{}", e),
    };

    let (mut rl, thread) = raylib::init()
        .size(WINDOW_WIDTH, WINDOW_HEIGHT)
        .title("Chess Debug GUI")
        .msaa_4x()
        .log_level(TraceLogLevel::LOG_ERROR)
        .build();

    let audio = match RaylibAudio::init_audio_device() {
        Ok(audio) => audio,
        Err(e) => panic!("{}", e),
    };

    let move_sound = audio.new_sound("assets/move-self.mp3").unwrap();
    let capture_sound = audio.new_sound("assets/capture.mp3").unwrap();
    let check_sound = audio.new_sound("assets/capture.mp3").unwrap();
    let promote_sound = audio.new_sound("assets/move-check.mp3").unwrap();
    let castle_sound = audio.new_sound("assets/castle.mp3").unwrap();

    let textures = load_textures(&mut rl, &thread);
    let mut selected_square: Option<u32> = None;
    let mut game_state = GameState::Playing;

    while !rl.window_should_close() {
        if rl.is_mouse_button_pressed(MouseButton::MOUSE_BUTTON_LEFT) {
            let maybe_to = select_move(&mut rl, &pos, &mut selected_square);

            if maybe_to.is_some() && selected_square.is_some() {
                let from = selected_square.unwrap();
                let to = maybe_to.unwrap();

                let possible_move = pos.get_move(from, to);
                if let Some(mut m) = possible_move {
                    if m.is_promotion() {
                        // Hardcoded for now
                        m.set_promotion_piece(PieceType::Queen);
                    }

                    let flags = pos.make_move(m);
                    if flags.capture() {
                        capture_sound.play();
                    }
                    if flags.check() {
                        check_sound.play();
                    }
                    if flags.promotion() {
                        promote_sound.play();
                    }
                    if flags.castle() {
                        castle_sound.play();
                    }
                    if flags.quiet() {
                        move_sound.play();
                    }

                    game_state = pos.check_game_state();
                }

                selected_square = None;
            }
        }

        let mut d = rl.begin_drawing(&thread);

        draw_board(&mut d);
        draw_pieces(&mut d, &pos, &textures);

        if let Some(square) = selected_square {
            highlight_movable_squares(&mut d, &pos.moves_for_square(square));
        }

        match game_state {
            GameState::Checkmate => {
                let text = format!("{:?} won by checkmate", pos.opposite_side());
                let length = d.measure_text(text.as_str(), 64);

                d.draw_text(
                    text.as_str(),
                    WINDOW_WIDTH / 2 - length / 2,
                    WINDOW_HEIGHT / 2 - 24,
                    64,
                    Color::CORNFLOWERBLUE,
                )
            }
            GameState::Stalemate
            | GameState::DrawByInsufficientMaterial
            | GameState::DrawByRepetition => {
                let text = "Stalemate!";
                let length = d.measure_text(text, 64);

                d.draw_text(
                    text,
                    WINDOW_WIDTH / 2 - length / 2,
                    WINDOW_HEIGHT / 2 - 24,
                    64,
                    Color::WHITE,
                )
            }
            _ => {}
        }

        d.clear_background(Color::WHITE);
    }
}

fn select_move(
    rl: &mut RaylibHandle,
    pos: &Position,
    selected_square: &mut Option<u32>,
) -> Option<u32> {
    let x = rl.get_mouse_x() / RECT_WIDTH;
    let y = rl.get_mouse_y() / RECT_WIDTH;
    let clicked_square = (y * 8 + x) as u32;

    if selected_square.is_none() {
        *selected_square = Some(clicked_square);
        return None;
    }

    let maybe_piece_on = pos.piece_on(clicked_square);

    if maybe_piece_on.is_none() {
        return Some(clicked_square);
    }

    if maybe_piece_on.unwrap().color == pos.current_side() {
        *selected_square = Some(clicked_square);
    } else {
        return Some(clicked_square);
    }

    None
}

fn highlight_movable_squares(d: &mut RaylibDrawHandle, moves: &[ChessMove]) {
    for piece_move in moves {
        let x = piece_move.to() % 8;
        let y = piece_move.to() / 8;
        let center_x = x as i32 * RECT_WIDTH + RECT_WIDTH / 2;
        let center_y = y as i32 * RECT_WIDTH + RECT_WIDTH / 2;

        d.draw_circle(center_x, center_y, 24.0, Color::get_color(COLOR_MOVABLE));
    }
}

fn draw_pieces(d: &mut RaylibDrawHandle, pos: &Position, textures: &[Texture2D]) {
    for y in 0..8 {
        for x in 0..8 {
            if let Some(piece) = pos.piece_on(y * 8 + x) {
                let texture_index = piece.t as usize + 6 * piece.color as usize;
                let texture = &textures[texture_index];

                let x = x as i32 * RECT_WIDTH;
                let y = y as i32 * RECT_WIDTH;

                d.draw_texture_pro(
                    texture,
                    Rectangle::new(0.0, 0.0, texture.width() as f32, texture.height() as f32),
                    Rectangle::new(x as f32, y as f32, RECT_WIDTH as f32, RECT_WIDTH as f32),
                    Vector2::zero(),
                    0.0,
                    Color::WHITE,
                );
            }
        }
    }
}

fn draw_board(d: &mut RaylibDrawHandle) {
    for y in 0..8 {
        for x in 0..8 {
            let color = if (x + y) % 2 == 0 {
                Color::get_color(COLOR_EVEN)
            } else {
                Color::get_color(COLOR_ODD)
            };

            d.draw_rectangle(
                x * RECT_WIDTH,
                y * RECT_WIDTH,
                RECT_WIDTH,
                RECT_WIDTH,
                color,
            );
        }
    }
}

fn load_textures(rl: &mut RaylibHandle, thread: &RaylibThread) -> Vec<Texture2D> {
    let mut textures = Vec::new();
    const NUM_PIECES: u32 = 6;

    for idx in 0..NUM_PIECES * 2 {
        let texture = match rl.load_texture(&thread, format!("assets/{}.png", idx).as_str()) {
            Ok(texture) => texture,
            Err(msg) => panic!("{}", msg),
        };

        textures.push(texture);
    }

    textures
}

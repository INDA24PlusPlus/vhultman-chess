# Examples
Below are some examples of how the library is suppose the be used.
## Example Usage
```rust
use vhultman_chess::*;

fn main() {
    let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR";

    let mut pos = match Position::from_fen(fen) {
        Ok(p) => p,
        Err(e) => panic!("{}", e),
    };

    let a2 = 48;
    let a3 = 40;

    // Checks if the move is valid, and if it is returns a structure representing this move.
    // The struct also contains a bunch of flags for the internal logic of the library so don't mess with those :)
    let maybe_move = pos.get_move(a2, a3);

    if let Some(mut m) = maybe_move {

        // If the move is a promotion we should set the promotion piece (if we don't do this it defaults to a queen)
        if m.is_promotion() {
            m.set_promotion_piece(PieceType::Queen);
        }


        let flags = pos.make_move(m);
        if flags.capture() {
            println!("We captured something!");
        }
        if flags.check() {
            println!("We gave check!");
        }
        if flags.promotion() {
            println!("We promoted a piece!");
        }
        if flags.castle() {
            println!("We castled our king!");
        }
        if flags.quiet() {
            println!("We just moved our piece (and nothing else)!");
        }

        let game_state = pos.check_game_state();

        match game_state {
            GameState::Checkmate => println!("Checkmate"),
            GameState::Stalemate => println!("Checkmate"),
            GameState::DrawByInsufficientMaterial => println!("Insufficient material"),
            GameState::DrawByRepetition  => println!("Repetition"),
            GameState::Playing => println!("Keep on playing :)"),
        };
    }

    let piece = pos.piece_on(a3).unwrap();
    println!("A {:?} {:?} is currently on A3", piece.color, piece.t);
}

```

## Example GUI
A example of a GUI written using [Raylib](https://github.com/raysan5/raylib) can be found in the example directory.
#![feature(portable_simd)]
use std::simd::cmp::SimdPartialEq;
use std::simd::u64x8;

mod move_gen;

const BOARD_SIZE: u32 = 8;

#[derive(Clone, Copy)]
pub enum PieceType {
    Pawn = 0,
    Knight = 1,
    Bishop = 2,
    Rook = 3,
    Queen = 4,
    King = 5,
}

#[derive(Clone, Copy, PartialEq)]
pub enum Color {
    White = 0,
    Black = 1,
}

#[derive(Clone, Copy)]
pub struct Piece {
    pub pos: u32,
    pub t: PieceType,
    pub color: Color,
    _private: (),
}

#[derive(Clone, Copy)]
pub struct PieceMove {
    pub start: u32,
    pub target: u32,
}

pub struct Board {
    current_turn: Color,
    state: BoardState,

    // This is private since it is not the true state of the board but rather a nice interface for viewing it :)
    // so we don't want people to modify it and think it changes the board state.
    pieces: [Option<Piece>; 8 * 8],

    pub moves: Vec<PieceMove>,
}

impl Board {
    pub fn from_fem(fem_string: &str) -> Result<Board, String> {
        // Rank is reveresed here because the bitboard is top down.
        let mut x: u32 = 0;
        let mut y: u32 = 0;

        let mut board = BoardState::new();

        for c in fem_string.chars() {
            if x == 9 {
                return Err(format!("Attempted to place piece past 8th file!"));
            }

            match c {
                'p' => {
                    board.pieces[PieceType::Pawn as usize].set(x, y);
                    board.color[1].set(x, y);
                }
                'P' => {
                    board.pieces[PieceType::Pawn as usize].set(x, y);
                    board.color[0].set(x, y);
                }
                'n' => {
                    board.pieces[PieceType::Knight as usize].set(x, y);
                    board.color[1].set(x, y);
                }
                'N' => {
                    board.pieces[PieceType::Knight as usize].set(x, y);
                    board.color[0].set(x, y);
                }
                'b' => {
                    board.pieces[PieceType::Bishop as usize].set(x, y);
                    board.color[1].set(x, y);
                }
                'B' => {
                    board.pieces[PieceType::Bishop as usize].set(x, y);
                    board.color[0].set(x, y);
                }
                'r' => {
                    board.pieces[PieceType::Rook as usize].set(x, y);
                    board.color[1].set(x, y);
                }
                'R' => {
                    board.pieces[PieceType::Rook as usize].set(x, y);
                    board.color[0].set(x, y);
                }
                'q' => {
                    board.pieces[PieceType::Queen as usize].set(x, y);
                    board.color[1].set(x, y);
                }
                'Q' => {
                    board.pieces[PieceType::Queen as usize].set(x, y);
                    board.color[0].set(x, y);
                }
                'k' => {
                    board.pieces[PieceType::King as usize].set(x, y);
                    board.color[1].set(x, y);
                }
                'K' => {
                    board.pieces[PieceType::King as usize].set(x, y);
                    board.color[0].set(x, y);
                }
                '1'..'9' => {
                    x += c.to_digit(10).unwrap() - 1;
                }
                '/' => {
                    if x != 8 {
                        return Err(format!(
                            "Rank advanced early. Rank can only be advanced at the 8th file"
                        ));
                    }

                    if y == 7 {
                        return Err(format!("Attempt to advance past the 1th rank"));
                    }

                    x = 0;
                    y += 1;

                    continue;
                }
                _ => return Err(format!("Unknown Erorr encountred!")),
            }

            x += 1;
        }

        let mut board = Board {
            current_turn: Color::White,
            state: board,
            pieces: [None; 64],
            moves: Vec::new(),
        };
        board.recalculate_pieces_from_state();
        board.gen_moves();

        Ok(board)
    }

    pub fn pieces(&self) -> &[Option<Piece>] {
        &self.pieces
    }

    pub fn current_turn(&self) -> Color {
        self.current_turn
    }

    pub fn make_move(&mut self, m: PieceMove) -> (bool, bool) {
        // Linear search is good enough since move < 220
        let is_valid = self
            .moves
            .iter()
            .find(|&valid_move| valid_move.start == m.start && valid_move.target == m.target)
            .is_some();

        if !is_valid {
            return (false, false);
        }

        let index = m.start as usize;
        let piece = &self.pieces[index].unwrap();

        let capture_mask = (1 << m.target) & self.state.piece_mask();
        self.state.color[self.current_turn as usize + 1 & 1] ^= capture_mask;

        let mask = !(1 << index);
        self.state.pieces[piece.t as usize] &= mask;
        self.state.color[piece.color as usize] &= mask;

        let index = m.target as usize;
        let new_location = 1 << index;
        self.state.pieces[piece.t as usize] |= new_location;
        self.state.color[piece.color as usize] |= new_location;

        self.next_player();

        (true, capture_mask != 0)
    }

    /// Prepares the board state for next player's turn.
    fn next_player(&mut self) {
        self.moves.truncate(0);
        self.change_turn_color();
        self.gen_moves();
        self.recalculate_pieces_from_state();
    }

    /// Populates 'moves' field with current legal moves based upon board state.
    fn gen_moves(&mut self) {
        self.get_pawn_moves();
    }

    /// Updates 'current_color' bit board to correct color bit board.
    fn change_turn_color(&mut self) {
        self.current_turn = match self.current_turn {
            Color::White => Color::Black,
            Color::Black => Color::White,
        };
    }

    /// Updates the public api facing pieces to reflect the actual state of the board.
    fn recalculate_pieces_from_state(&mut self) {
        self.pieces = [None; 64];
        let mut piece_mask = self.state.piece_mask();

        while piece_mask != 0 {
            let pos = piece_mask.trailing_zeros();
            let curr_square = 1 << pos;

            // We know there exists a piece here cause of the trailing zero count.
            let piece_type = self.state.piece_type(curr_square).unwrap();
            let is_white = curr_square & self.state.color[0] != 0;

            self.pieces[pos as usize] = Some(Piece {
                pos,
                color: if is_white { Color::White } else { Color::Black },
                t: piece_type,
                _private: (),
            });

            piece_mask ^= 1 << pos;
        }
    }
}

struct BoardState {
    color: [BitBoard; 2],
    pieces: [BitBoard; 6],
}

impl BoardState {
    fn new() -> BoardState {
        BoardState {
            color: [0; 2],
            pieces: [0; 6],
        }
    }

    fn piece_mask(&self) -> BitBoard {
        let mut mask: u64 = 0;
        for piece in self.pieces {
            mask |= piece;
        }

        mask
    }

    fn piece_type(&self, square: BitBoard) -> Option<PieceType> {
        let square = u64x8::splat(square);
        let boards = u64x8::from_array([
            self.pieces[0],
            self.pieces[1],
            self.pieces[2],
            self.pieces[3],
            self.pieces[4],
            self.pieces[5],
            0,
            0,
        ]);

        let masks = boards & square;
        let truth_mask = masks.simd_ne(u64x8::splat(0));
        let maybe_index = truth_mask.first_set();

        if let Some(index) = maybe_index {
            Some(PieceType::from_usize(index))
        } else {
            None
        }
    }
}

impl PieceType {
    fn from_usize(value: usize) -> PieceType {
        match value {
            0 => Self::Pawn,
            1 => Self::Knight,
            2 => Self::Bishop,
            3 => Self::Rook,
            4 => Self::Queen,
            5 => Self::King,
            _ => unreachable!(),
        }
    }
}

const fn rank(index: u32) -> u64 {
    0xff << (BOARD_SIZE * index)
}

type BitBoard = u64;

trait BitBoardExtensions {
    fn print(&self);
    fn set(&mut self, x: u32, y: u32);
}

impl BitBoardExtensions for BitBoard {
    fn set(&mut self, x: u32, y: u32) {
        *self |= 1 << (y * BOARD_SIZE + x);
    }

    #[allow(unused)]
    fn print(&self) {
        for y in 0..8 {
            for x in 0..8 {
                let is_set = self & 1 << (y * 8 + x) != 0;
                if is_set {
                    print!("1");
                } else {
                    print!("0");
                }
            }

            println!("");
        }
    }
}

#![feature(portable_simd)]
use std::simd::cmp::SimdPartialEq;
use std::simd::u64x8;

mod move_gen;

const BOARD_SIZE: u32 = 8;
const SQUARE_COUNT: usize = 64;

#[derive(Clone, Copy, PartialEq)]
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

    prev_move: PieceMove,
    prev_piece: Piece,
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
                _ => {
                    return Err(format!(
                        "Unknown character encountred! Not the entirety of FEM is supported"
                    ))
                }
            }

            x += 1;
        }

        let mut board = Board {
            current_turn: Color::White,
            state: board,
            pieces: [None; 64],
            moves: Vec::new(),
            prev_move: PieceMove {
                start: 0,
                target: 0,
            },
            prev_piece: Piece {
                pos: 0,
                t: PieceType::Pawn,
                color: Color::White,
            },
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

    /// Tries to make the specified move.
    /// return:
    ///     (false, false): move was not valid and therefore not made.
    ///     (true, false): move was made, but no capture occured.
    ///     (true, true): move was made and capture occured.
    pub fn make_move(&mut self, m: PieceMove) -> (bool, bool) {
        // Linear search is good enough since moves.len < 220
        let is_valid = self
            .moves
            .iter()
            .find(|&valid_move| valid_move.start == m.start && valid_move.target == m.target)
            .is_some();

        if !is_valid {
            return (false, false);
        }

        let start_square = m.start as usize;
        let moving_piece = self.pieces[start_square].unwrap();
        let target_piece = self.pieces[m.target as usize];

        let did_en_passant_capture = self.handle_en_passant(m, &moving_piece, &target_piece);

        // BitBoard representing any captures.
        let capture = (1 << m.target) & self.state.piece_mask();

        // Unset opposite side if there was a capture.
        self.state.color[self.current_turn as usize + 1 & 1] &= !capture;

        // if there was a capture unset correct bit in piece masks
        if let Some(target) = target_piece {
            self.state.pieces[target.t as usize] &= !capture;
        }

        // Unset the previous position of the piece
        let mask = !(1 << start_square);
        self.state.pieces[moving_piece.t as usize] &= mask;
        self.state.color[moving_piece.color as usize] &= mask;

        // Set the new location on the boards.
        let target_square = m.target as usize;
        let new_location = 1 << target_square;
        self.state.pieces[moving_piece.t as usize] |= new_location;
        self.state.color[moving_piece.color as usize] |= new_location;

        // Needed for en passant move gen.
        self.prev_move = m;
        self.prev_piece = moving_piece;
        self.prev_piece.pos = m.target;

        self.next_player();

        (true, capture != 0 || did_en_passant_capture)
    }

    /// Checks if en passant occured and handles
    /// state updating.
    /// returns true if en passant happened.
    fn handle_en_passant(
        &mut self,
        m: PieceMove,
        moving_piece: &Piece,
        target_piece: &Option<Piece>,
    ) -> bool {
        let is_en_passant = Self::is_move_en_passant(m, moving_piece, &target_piece);
        if is_en_passant {
            let capture_square = if self.current_turn == Color::White {
                1 << (m.target as u64 + 8)
            } else {
                1 << (m.target as u64 - 8)
            };

            let mask = !capture_square;
            self.state.pieces[PieceType::Pawn as usize] &= mask;
            *self.enemy_color_mask() &= mask;
            return true;
        };

        false
    }

    fn is_move_en_passant(
        m: PieceMove,
        moving_piece: &Piece,
        target_piece: &Option<Piece>,
    ) -> bool {
        if target_piece.is_none() && moving_piece.t == PieceType::Pawn {
            let move_dist = (m.start as i32 - m.target as i32).abs();
            return move_dist == 7 || move_dist == 9;
        }

        false
    }

    /// Prepares the board state for next player's turn.
    fn next_player(&mut self) {
        self.moves.truncate(0);
        self.change_turn_color();
        self.gen_moves();
        self.recalculate_pieces_from_state();
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
            let pos = piece_mask.trailing_zeroes_with_reset();
            let curr_square = 1 << pos;

            // We know there exists a piece here cause of the trailing zero count.
            let piece_type = self.state.piece_type(curr_square).unwrap();
            let is_white = curr_square & self.state.color[0] != 0;

            self.pieces[pos as usize] = Some(Piece {
                pos,
                color: if is_white { Color::White } else { Color::Black },
                t: piece_type,
            });
        }
    }

    fn enemy_color_mask(&mut self) -> &mut BitBoard {
        &mut self.state.color[(self.current_turn as usize + 1) & 1]
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
        self.color[0] | self.color[1]
    }

    // TODO: Make this work on Rust stable.
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

trait BoolMaskTrait {
    fn as_mask(self) -> BitBoard;
}

impl BoolMaskTrait for bool {
    /// Returns a mask of all 1's for true and all 0's for false.
    fn as_mask(self) -> BitBoard {
        -(self as i64) as u64
    }
}

type BitBoard = u64;

trait BitBoardExtensions {
    #[allow(unused)]
    fn print(&self);

    fn set(&mut self, x: u32, y: u32);
    fn bit_scan(self, forward: bool) -> u32;
    fn trailing_zeroes_with_reset(&mut self) -> u32;
}

impl BitBoardExtensions for BitBoard {
    fn set(&mut self, x: u32, y: u32) {
        *self |= 1 << (y * BOARD_SIZE + x);
    }

    // "borrowed" from
    // https://www.chessprogramming.org/BitScan#GeneralizedBitscan
    fn bit_scan(mut self, reverse: bool) -> u32 {
        let r_mask = reverse.as_mask();
        self &= self.wrapping_neg() | r_mask;
        self.leading_zeros()
    }

    // https://www.chessprogramming.org/BitScan#Bitscan_with_Reset
    /// Returns position of the most significant bit and unsets it.
    fn trailing_zeroes_with_reset(&mut self) -> u32 {
        let pos = self.trailing_zeros();
        *self &= *self - 1;
        pos
    }

    #[allow(unused)]
    /// Only for debugging.
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

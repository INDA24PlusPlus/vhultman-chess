#![feature(portable_simd)]
use std::simd::cmp::SimdPartialEq;
use std::simd::u64x8;

mod move_gen;

use move_gen::{attack_mask_bishop, attack_mask_rook, generate_pseudo_legal, LUT};
use std::fmt;

const NUM_PIECE_TYPES: usize = 6;
const NUM_SQUARES: usize = 64;
const NUM_COLORS: usize = 2;

type BitBoard = u64;

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Color {
    White,
    Black,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum PieceType {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
}

#[derive(Clone, Copy, Debug)]
pub struct Piece {
    pub t: PieceType,
    pub color: Color,
}

#[derive(Clone, Copy)]
pub struct ChessMove(u32);

impl ChessMove {
    const FLAG_EN_PASSANT: u32 = 0b0101;

    const FLAG_QUEEN_PROMO: u32 = 0b1011;
    const FLAG_ROOK_PROMO: u32 = 0b1010;
    const FLAG_BISHOP_PROMO: u32 = 0b1001;
    const FLAG_KNIGHT_PROMO: u32 = 0b1000;

    const FLAG_CASTLE_KING: u32 = 0b0010;
    const FLAG_CASTLE_QUEEN: u32 = 0b0011;

    pub fn new(from: u32, to: u32, flags: u32) -> ChessMove {
        // bits 16-12 (4 bits): flags
        // bits 12-6 (6 bits): from
        // bits 6-0 (6 bits): to
        ChessMove((flags & 0xf) << 12 | ((from & 0x3f) << 6) | (to & 0x3f))
    }

    pub fn to(self) -> u32 {
        self.0 & 0x3f
    }

    pub fn from(self) -> u32 {
        (self.0 >> 6) & 0x3f
    }

    fn flags(self) -> u32 {
        self.0 >> 12
    }

    fn is_capture(self) -> bool {
        self.0 & 0b0100 != 0
    }

    pub fn is_en_passant(self) -> bool {
        self.flags() == ChessMove::FLAG_EN_PASSANT
    }

    fn is_promotion(self) -> bool {
        self.0 & 0b1000_0000_0000_0000 != 0
    }

    fn is_castle_move(self) -> bool {
        self.is_castle_king() || self.is_castle_queen()
    }

    fn is_castle_king(self) -> bool {
        self.flags() == ChessMove::FLAG_CASTLE_KING
    }

    fn is_castle_queen(self) -> bool {
        self.flags() == ChessMove::FLAG_CASTLE_QUEEN
    }

    fn promotion_piece(self) -> PieceType {
        unsafe { std::mem::transmute((self.flags() - 7) as u8) }
    }
}

pub struct Position {
    board: [Option<Piece>; NUM_SQUARES],
    current_side: Color,
    state: State,
    prev_states: Vec<State>,
}

#[derive(Clone, Copy)]
struct State {
    pieces: [BitBoard; NUM_PIECE_TYPES],
    colors: [BitBoard; NUM_COLORS],
    castling_rights: [bool; 2 * NUM_COLORS],
    m: ChessMove,
    p: PieceType,
    captured_p: Option<PieceType>,
}

impl Position {
    pub fn king(&self, c: Color) -> BitBoard {
        self.state.pieces[PieceType::King as usize] & self.state.colors[c as usize]
    }

    fn queen(&self, c: Color) -> BitBoard {
        self.state.pieces[PieceType::Queen as usize] & self.state.colors[c as usize]
    }

    fn rooks(&self, c: Color) -> BitBoard {
        self.state.pieces[PieceType::Rook as usize] & self.state.colors[c as usize]
    }

    fn bishops(&self, c: Color) -> BitBoard {
        self.state.pieces[PieceType::Bishop as usize] & self.state.colors[c as usize]
    }

    fn knights(&self, c: Color) -> BitBoard {
        self.state.pieces[PieceType::Knight as usize] & self.state.colors[c as usize]
    }

    fn pawns(&self, c: Color) -> BitBoard {
        self.state.pieces[PieceType::Pawn as usize] & self.state.colors[c as usize]
    }

    fn all(&self) -> BitBoard {
        self.state.colors[0] | self.state.colors[1]
    }

    fn color_mask(&self, c: Color) -> BitBoard {
        self.state.colors[c as usize]
    }

    fn switch_side(&mut self) {
        self.current_side = unsafe { std::mem::transmute((self.current_side as u8 + 1) & 1) }
    }

    fn opposite_side(&self) -> Color {
        unsafe { std::mem::transmute((self.current_side as u8) + 1 & 1) }
    }

    pub fn piece_on(&self, square: usize) -> Option<Piece> {
        self.board[square]
    }

    fn piece_color(&self, square: usize) -> Color {
        if self.state.colors[0] & (1 << square) != 0 {
            Color::White
        } else {
            Color::Black
        }
    }

    fn en_passant_possible(&self) -> bool {
        self.state.p == PieceType::Pawn
            && (self.state.m.from() as i32 - self.state.m.to() as i32).abs() == 16
    }

    fn piece_type(&self, square: usize) -> Option<PieceType> {
        let square = u64x8::splat(square as u64);
        let boards = u64x8::from_array([
            self.state.pieces[0],
            self.state.pieces[1],
            self.state.pieces[2],
            self.state.pieces[3],
            self.state.pieces[4],
            self.state.pieces[5],
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

    pub fn make_move(&mut self, m: ChessMove) {
        self.prev_states.push(self.state);
        self.state.m = m;

        let from = m.from() as usize;
        let to = m.to() as usize;

        let color_idx = self.current_side as usize;
        let mut piece_type = self.piece_type(1 << from).unwrap();
        self.state.p = piece_type;

        self.state.pieces[piece_type as usize] &= !(1 << from);
        self.state.colors[color_idx] &= !(1 << from);

        let maybe_capture = self.piece_type(1 << to);
        self.state.captured_p = maybe_capture;
        if let Some(p) = maybe_capture {
            assert_ne!(self.current_side, self.piece_color(to));

            self.state.pieces[p as usize] &= !(1 << to);
            self.state.colors[self.piece_color(to) as usize] &= !(1 << to);
        }

        if m.is_en_passant() {
            let offset = 1 - 2 * self.current_side as i32;
            let piece_color = self.opposite_side();
            assert_ne!(piece_color, self.current_side);

            self.state.colors[piece_color as usize] &= !(1 << ((to as i32 + 8 * offset) as usize));
            self.state.pieces[PieceType::Pawn as usize] &=
                !(1 << ((to as i32 + 8 * offset) as usize));
        } else if m.is_castle_move() {
            let offset = if m.is_castle_king() { 3 } else { -4 };

            // Remove rook.
            let rook_square = (from as i32 + offset) as usize;

            self.state.pieces[PieceType::Rook as usize] &= !(1 << rook_square);
            self.state.colors[color_idx] &= !(1 << rook_square);

            let rook_square = to + (2 * m.is_castle_queen() as usize) - 1;
            assert!(1 << rook_square & self.all() == 0);
            // Place rook
            self.state.pieces[PieceType::Rook as usize] |= 1 << rook_square;
            self.state.colors[color_idx] |= 1 << rook_square;
        } else if m.is_promotion() {
            piece_type = m.promotion_piece();
        }

        self.update_castling_rights(m, piece_type);

        self.state.pieces[piece_type as usize] |= 1 << to;
        self.state.colors[color_idx] |= 1 << to;

        self.switch_side();
    }

    pub fn undo_move(&mut self, m: ChessMove) {
        self.state = self.prev_states.pop().unwrap();
        self.switch_side();
    }

    fn update_castling_rights(&mut self, m: ChessMove, moving_piece: PieceType) {
        let c = self.current_side as usize;

        if moving_piece == PieceType::King {
            self.state.castling_rights[2 * c + 0] = false;
            self.state.castling_rights[2 * c + 1] = false;
        } else if moving_piece == PieceType::Rook {
            match m.from() {
                63 => self.state.castling_rights[0] = false,
                56 => self.state.castling_rights[1] = false,
                7 => self.state.castling_rights[2] = false,
                0 => self.state.castling_rights[3] = false,
                _ => {}
            };
        }

        if let Some(p) = self.state.captured_p {
            if p == PieceType::Rook {
                match m.to() {
                    63 => self.state.castling_rights[0] = false,
                    56 => self.state.castling_rights[1] = false,
                    7 => self.state.castling_rights[2] = false,
                    0 => self.state.castling_rights[3] = false,
                    _ => {}
                };
            }
        }
    }

    pub fn generate_moves(&mut self, moves: &mut Vec<ChessMove>) {
        let start_pieces = self.state.pieces;
        let start_colors = self.state.colors;

        generate_pseudo_legal(self, moves);

        let mut legal_moves = Vec::new();
        legal_moves.reserve(256);

        let mut responses = Vec::new();
        responses.reserve(256);

        for m in moves.iter() {
            self.make_move(*m);
            let king_square = self.king(self.opposite_side()).trailing_zeros();

            responses.truncate(0);
            generate_pseudo_legal(self, &mut responses);

            if responses.iter().any(|r| r.to() == king_square) {
                //println!("Rejected move");
            } else {
                legal_moves.push(*m);
            }

            self.undo_move(*m);
        }

        moves.truncate(0);
        *moves = legal_moves;

        self.state.colors = start_colors;
        self.state.pieces = start_pieces;
    }

    pub fn generate_checkers_mask(&self) -> BitBoard {
        let c = self.opposite_side();
        let king = self.king(self.current_side);
        let king_square = king.trailing_zeros();
        let occupied = self.all();

        let rooks = self.rooks(c) | self.queen(c);
        let king_ortho = attack_mask_rook(king_square, occupied);
        let rook_checkers = rooks & king_ortho;

        let bishops = self.bishops(c) | self.queen(c);
        let king_diag = attack_mask_bishop(king_square, occupied);
        let bishop_checkers = bishops & king_diag;

        let knights = self.knights(c);
        let possible_knight_squares = LUT::KNIGHT[king_square as usize];
        let knight_checkers = possible_knight_squares & knights;

        let pawns = self.pawns(c);

        const NOT_FILE_A: u64 = !0x0101010101010101;
        const NOT_FILE_H: u64 = !0x8080808080808080;
        let pawn_checkers = if c == Color::White {
            let possible_pawn_squares = ((king << 9) & NOT_FILE_A) | ((king << 7) & NOT_FILE_H);
            pawns & possible_pawn_squares
        } else {
            let possible_pawn_squares = ((king >> 9) & NOT_FILE_H) | ((king >> 7) & NOT_FILE_A);
            possible_pawn_squares & pawns
        };

        bishop_checkers | rook_checkers | knight_checkers | pawn_checkers
    }

    pub fn from_fen(fen_string: &str) -> Result<Position, String> {
        // Rank is reveresed here because the bitboard is top down.
        let mut x: usize = 0;
        let mut y: usize = 0;

        let mut colors = [0; NUM_COLORS];
        let mut pieces = [0; NUM_PIECE_TYPES];
        let mut board = [None; NUM_SQUARES];

        let mut parts = fen_string.split(' ');
        let position_string = parts.next().unwrap();
        let lowered = position_string.to_lowercase();

        for (token, c) in lowered.chars().zip(position_string.chars()) {
            if token == '/' {
                if x != 8 {
                    return Err(format!(
                        "Attempt to advance rank(/) while file is still {x}"
                    ));
                }

                y += 1;
                x = 0;
                continue;
            } else if token.is_digit(10) {
                x += token.to_digit(10).unwrap() as usize;
                continue;
            }

            let color = !(token > c);
            colors[color as usize] |= 1 << (y * 8 + x);

            let piece_type = match token {
                'p' => 0,
                'n' => 1,
                'b' => 2,
                'r' => 3,
                'q' => 4,
                'k' => 5,
                _ => {
                    return Err(format!("Invalid token encountered!"));
                }
            };

            pieces[piece_type] |= 1 << (y * 8 + x);
            let piece = Piece {
                t: unsafe { std::mem::transmute(piece_type as u8) },
                color: unsafe { std::mem::transmute(color as u8) },
            };

            board[y * 8 + x] = Some(piece);

            if x > 7 {
                return Err(format!("Attempt to move past the 8th file"));
            }

            x += 1;
        }

        let mut states = Vec::new();
        let state = State {
            pieces,
            colors,
            m: ChessMove::new(0, 0, 0),
            p: PieceType::Pawn,
            castling_rights: [true; 2 * NUM_COLORS],
            captured_p: None,
        };

        Ok(Position {
            current_side: Color::White,
            state,
            board,
            prev_states: states,
        })
    }
    /// Updates the public api facing pieces to reflect the actual state of the board.
    pub fn recalculate_pieces_from_state(&mut self) {
        self.board = [None; 64];
        let mut piece_mask = self.all();

        while piece_mask != 0 {
            let pos = piece_mask.trailing_zeroes_with_reset();
            let curr_square = 1 << pos;

            // We know there exists a piece here cause of the trailing zero count.
            let piece_type = self.piece_type(curr_square).unwrap();
            let is_white = curr_square as u64 & self.state.colors[0] != 0;

            self.board[pos as usize] = Some(Piece {
                color: if is_white { Color::White } else { Color::Black },
                t: piece_type,
            });
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

trait BoolMaskTrait {
    fn as_mask(self) -> BitBoard;
}

impl BoolMaskTrait for bool {
    /// Returns a mask of all 1's for true and all 0's for false.
    fn as_mask(self) -> BitBoard {
        -(self as i64) as u64
    }
}

trait BitBoardExtensions {
    #[allow(unused)]
    fn print(&self);

    fn set(&mut self, x: u32, y: u32);
    fn bit_scan(self, forward: bool) -> u32;
    fn trailing_zeroes_with_reset(&mut self) -> u32;
}

impl BitBoardExtensions for BitBoard {
    fn set(&mut self, x: u32, y: u32) {
        *self |= 1 << (y * 8 + x);
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

const fn between_bb(sq0: usize, sq1: usize) -> BitBoard {
    LUT::BETWEEN_BBS[sq0 * NUM_SQUARES + sq1]
}

const fn rank(index: u32) -> u64 {
    0xff << (8 * index)
}

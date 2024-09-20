#![feature(portable_simd)]
use std::simd::cmp::SimdPartialEq;
use std::simd::u64x8;

mod move_gen;

use move_gen::{attack_mask_bishop, attack_mask_rook, generate_legal, LUT};

const NUM_PIECE_TYPES: usize = 6;
const NUM_SQUARES: usize = 64;
const NUM_COLORS: usize = 2;

type BitBoard = u64;

#[derive(Clone, Copy, Debug)]
pub enum GameState {
    Playing,
    CheckMate,
    StaleMate,
    DrawByRepetion,
    DrawByInsufficientMaterial,
}

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

    pub fn is_en_passant(self) -> bool {
        self.flags() == ChessMove::FLAG_EN_PASSANT
    }

    pub fn is_promotion(self) -> bool {
        self.0 & 0b1000_0000_0000_0000 != 0
    }

    pub fn is_castle_move(self) -> bool {
        self.is_castle_king() || self.is_castle_queen()
    }

    pub fn is_castle_king(self) -> bool {
        self.flags() == ChessMove::FLAG_CASTLE_KING
    }

    pub fn is_castle_queen(self) -> bool {
        self.flags() == ChessMove::FLAG_CASTLE_QUEEN
    }

    pub fn promotion_piece(self) -> PieceType {
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
    repetion_count: u32,
    pieces: [BitBoard; NUM_PIECE_TYPES],
    colors: [BitBoard; NUM_COLORS],
    castling_rights: [bool; 2 * NUM_COLORS],
    m: ChessMove,
    p: PieceType,
    captured_p: Option<PieceType>,

    king_blockers: [BitBoard; NUM_COLORS],
    pinners: [BitBoard; NUM_COLORS],
    checkers: [BitBoard; NUM_COLORS],
}

impl Position {
    /// Square 0 is the top left square on the board (A8)
    /// Square 1 is B8, etc
    pub fn piece_on(&self, square: usize) -> Option<Piece> {
        self.board[square]
    }

    /// Makes a move on the board and switches to the other color.
    /// It is assumed that the move is a valid move and is produced by the function
    /// "generate_moves"
    pub fn make_move(&mut self, m: ChessMove) {
        self.make_move_internal(m);
        self.recalculate_pieces_from_state();
    }

    /// Reverts the previous move.
    pub fn undo_move(&mut self, _: ChessMove) {
        self.state = self.prev_states.pop().unwrap();
        self.switch_side();
        self.recalculate_pieces_from_state();
    }

    /// Generates and fills "moves" with all legal moves for the current color.
    /// returns what state the game is currently in (checkmate, stalemate etc)
    pub fn generate_moves(&mut self, moves: &mut Vec<ChessMove>) -> GameState {
        generate_legal(self, moves);
        self.check_win_conditions(moves)
    }

    /// Construct board state from FEN notation. Only supports piece positions right now.
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

        let states = Vec::new();
        let state = State {
            pieces,
            colors,
            repetion_count: 0,
            m: ChessMove::new(0, 0, 0),
            p: PieceType::Pawn,
            castling_rights: [true; 2 * NUM_COLORS],
            captured_p: None,
            pinners: [0; NUM_COLORS],
            king_blockers: [0; NUM_COLORS],
            checkers: [0; NUM_COLORS],
        };

        Ok(Position {
            current_side: Color::White,
            state,
            board,
            prev_states: states,
        })
    }

    fn make_move_internal(&mut self, m: ChessMove) {
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
            self.state.pieces[p as usize] &= !(1 << to);
            self.state.colors[self.piece_color(to) as usize] &= !(1 << to);
        }

        if m.is_en_passant() {
            let offset = 1 - 2 * self.current_side as i32;
            let piece_color = self.opposite_side();

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
        self.generate_pin_data(self.current_side);
        self.state.checkers[self.opposite_side() as usize] = self.generate_checkers_mask();
    }

    /// Updates the public api facing pieces to reflect the actual state of the board.
    fn recalculate_pieces_from_state(&mut self) {
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

    fn check_win_conditions(&mut self, moves: &Vec<ChessMove>) -> GameState {
        if moves.len() == 0 {
            // Game is over
            if self.state.checkers[self.opposite_side() as usize] == 0 {
                return GameState::StaleMate;
            } else {
                return GameState::CheckMate;
            }
        }

        if self.state.repetion_count == 3 {
            return GameState::DrawByRepetion;
        }

        // Draw by insufficient material.
        if self.state.pieces[PieceType::King as usize] == self.all() {
            return GameState::DrawByInsufficientMaterial;
        }

        let num_white_pieces = self.color_mask(Color::White).count_ones();
        let num_black_pieces = self.color_mask(Color::White).count_ones();

        if num_white_pieces == 2 && num_black_pieces == 1 {
            // Impossible to checkmate with just knight/bishop and king
            if self.knights(Color::White) != 0 || self.bishops(Color::White) != 0 {
                return GameState::DrawByInsufficientMaterial;
            }
        }

        if num_black_pieces == 2 && num_white_pieces == 1 {
            // Impossible to checkmate with just knight/bishop and king
            if self.knights(Color::Black) != 0 || self.bishops(Color::Black) != 0 {
                return GameState::DrawByInsufficientMaterial;
            }
        }

        for s in self.prev_states.iter() {
            if s.colors == self.state.colors && s.pieces == self.state.pieces {
                self.state.repetion_count += 1;
                break;
            }
        }

        GameState::Playing
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

    /// Generates what pieces block and pin relevant colors pin.
    fn generate_pin_data(&mut self, color: Color) {
        let king_bb = self.king(color);
        let king_square = king_bb.trailing_zeros();

        let c = color as usize;
        let ec = (c + 1) & 1;

        self.state.king_blockers[c] = 0;
        self.state.pinners[ec] = 0;

        let rooks = self.state.pieces[PieceType::Queen as usize]
            | self.state.pieces[PieceType::Rook as usize];

        let bishops = self.state.pieces[PieceType::Queen as usize]
            | self.state.pieces[PieceType::Bishop as usize];

        let ortho = attack_mask_rook(king_square, 0);
        let diag = attack_mask_bishop(king_square, 0);
        let mut possible_snipers = ((ortho & rooks) | (diag & bishops)) & self.state.colors[ec];

        while possible_snipers != 0 {
            let pos = possible_snipers.trailing_zeroes_with_reset();
            let occupied = self.all() ^ (1 << pos);
            let bb = between_bb(king_square as usize, pos as usize) & occupied;

            if bb.count_ones() == 1 {
                self.state.king_blockers[c] |= bb;
                if bb & self.state.colors[c] != 0 {
                    self.state.pinners[ec] |= 1 << pos;
                }
            }
        }
    }

    /// Generates a mask of what pieces give check to current side's king
    fn generate_checkers_mask(&self) -> BitBoard {
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

    fn pinned(&self, c: Color) -> BitBoard {
        self.state.king_blockers[c as usize]
    }

    fn king(&self, c: Color) -> BitBoard {
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

    fn bit_scan(self, forward: bool) -> u32;
    fn trailing_zeroes_with_reset(&mut self) -> u32;
}

impl BitBoardExtensions for BitBoard {
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

impl ChessMove {
    const FLAG_EN_PASSANT: u32 = 0b0101;

    const FLAG_QUEEN_PROMO: u32 = 0b1011;
    const FLAG_ROOK_PROMO: u32 = 0b1010;
    const FLAG_BISHOP_PROMO: u32 = 0b1001;
    const FLAG_KNIGHT_PROMO: u32 = 0b1000;

    const FLAG_CASTLE_KING: u32 = 0b0010;
    const FLAG_CASTLE_QUEEN: u32 = 0b0011;

    fn flags(self) -> u32 {
        self.0 >> 12
    }
}

const fn between_bb(sq0: usize, sq1: usize) -> BitBoard {
    LUT::BETWEEN_BBS[sq0 * NUM_SQUARES + sq1]
}

const fn rank(index: u32) -> u64 {
    0xff << (8 * index)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_perft_6() {
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR";

        let mut pos = match Position::from_fen(fen) {
            Ok(p) => p,
            Err(e) => panic!("{}", e),
        };

        let nodes = perft_test(&mut pos, 6);
        assert_eq!(nodes, 119060324);
    }

    #[test]
    fn modified_perft_5() {
        let fen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R";

        let mut pos = match Position::from_fen(fen) {
            Ok(p) => p,
            Err(e) => panic!("{}", e),
        };

        let nodes = perft_test(&mut pos, 5);
        assert_eq!(nodes, 193690690);
    }
}

fn perft_driver(pos: &mut Position, depth: u64) -> u64 {
    let mut nodes = 0;
    // reccursion escape condition
    if depth == 0 {
        return 1;
    }

    let mut moves = Vec::new();

    // generate moves
    generate_legal(pos, &mut moves);

    // loop over generated moves
    for m in moves.iter() {
        pos.make_move_internal(*m);
        nodes += perft_driver(pos, depth - 1);
        pos.undo_move_perft(*m);
    }

    nodes
}

#[allow(unused)]
fn perft_test(pos: &mut Position, depth: u64) -> u64 {
    // reset nodes count
    let mut nodes = 0;
    let mut moves = Vec::new();

    // generate moves
    generate_legal(pos, &mut moves);

    // loop over generated moves
    for m in moves {
        // make move
        pos.make_move_internal(m);

        // call perft driver recursively
        nodes += perft_driver(pos, depth - 1);

        // take back
        pos.undo_move_perft(m);
    }

    nodes
}

impl Position {
    #[allow(unused)]
    fn undo_move_perft(&mut self, _: ChessMove) {
        self.state = self.prev_states.pop().unwrap();
        self.switch_side();
    }
}

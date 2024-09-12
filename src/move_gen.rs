use crate::rank;
use crate::BitBoard;
use crate::BitBoardExtensions;
use crate::Board;
use crate::BoolMaskTrait;
use crate::Color;
use crate::PieceMove;
use crate::PieceType;
use crate::BOARD_SIZE;
use crate::SQUARE_COUNT;

const KING_LUT: [BitBoard; SQUARE_COUNT] = LUTs::gen_king_moves();
const KNIGHT_LUT: [BitBoard; SQUARE_COUNT] = LUTs::gen_knight_moves();
const RAY_LUT: [BitBoard; SQUARE_COUNT * 8] = LUTs::gen_rays();

impl Board {
    /// Populates 'moves' field with current legal moves based upon board state.
    /// returns all bitboard with all squares that
    pub(crate) fn gen_moves(&mut self) {
        let en_passant_mask = self.en_passant_possible().as_mask() & (1 << self.prev_piece.pos);

        if self.current_turn == Color::White {
            self.get_pawn_moves_white(en_passant_mask)
        } else {
            self.get_pawn_moves_black(en_passant_mask)
        };

        self.get_knight_moves();
        self.get_king_moves();

        // Queen moves are also generated in these.
        self.get_rook_moves();
        self.get_bishop_moves();
    }

    /// Determine if en passant is possbile based upon previous move.
    fn en_passant_possible(&self) -> bool {
        if self.prev_piece.t == PieceType::Pawn {
            let move_dist = (self.prev_move.start as i32 - self.prev_move.target as i32).abs();
            return move_dist == 16;
        }

        false
    }

    fn get_pawn_moves_black(&mut self, en_passant_mask: BitBoard) {
        let color_mask = self.state.color[self.current_turn as usize];
        let mut pawns = self.state.pieces[PieceType::Pawn as usize] & color_mask;
        let occupied = self.state.piece_mask() & !color_mask;

        while pawns != 0 {
            let pos = pawns.trailing_zeroes_with_reset();
            let bb = 1 << pos;

            // single push
            let mut moves = (bb << 8) & !occupied;

            // double pushes
            moves |= ((moves & rank(2)) << BOARD_SIZE) & !occupied;

            // Handle captures
            let normal_captures = (bb << 7 | bb << 9) & occupied;
            let en_passant_targets = (bb >> 1 | bb << 1) & en_passant_mask & rank(4);
            let en_passant_moves = (en_passant_targets << 8) & !occupied;

            moves |= normal_captures | en_passant_moves;
            moves &= !color_mask;

            self.add_moves_from_mask(pos, moves);
        }
    }

    fn get_pawn_moves_white(&mut self, en_passant_mask: BitBoard) {
        let color_mask = self.state.color[self.current_turn as usize];
        let mut pawns = self.state.pieces[PieceType::Pawn as usize] & color_mask;
        let occupied = self.state.piece_mask();

        while pawns != 0 {
            let pos = pawns.trailing_zeroes_with_reset();
            let bb = 1 << pos;

            // single push
            let mut moves = (bb >> 8) & !occupied;

            // double pushes
            moves |= ((moves & rank(5)) >> BOARD_SIZE) & !occupied;

            // Handle captures
            let normal_captures = (bb >> 7 | bb >> 9) & occupied;
            let en_passant_targets = (bb >> 1 | bb << 1) & en_passant_mask & rank(3);
            let en_passant_moves = (en_passant_targets >> 8) & !occupied;

            moves |= normal_captures | en_passant_moves;
            moves &= !color_mask;

            self.add_moves_from_mask(pos, moves);
        }
    }

    fn get_king_moves(&mut self) {
        let color_mask = self.state.color[self.current_turn as usize];
        let king = self.state.pieces[PieceType::King as usize] & color_mask;

        let pos = king.trailing_zeros();

        let mut moves = KING_LUT[pos as usize];
        moves &= !color_mask;

        self.add_moves_from_mask(pos, moves);
    }

    fn get_knight_moves(&mut self) {
        let color_mask = self.state.color[self.current_turn as usize];
        let mut knights = self.state.pieces[PieceType::Knight as usize] & color_mask;

        while knights != 0 {
            let pos = knights.trailing_zeroes_with_reset();
            let moves = KNIGHT_LUT[pos as usize] & !color_mask;

            self.add_moves_from_mask(pos, moves);
        }
    }

    /// Generates bishop + queen moves.
    fn get_bishop_moves(&mut self) {
        let color_mask = self.state.color[self.current_turn as usize];
        let mut bishops = self.state.pieces[PieceType::Bishop as usize]
            | self.state.pieces[PieceType::Queen as usize] & color_mask;

        let occupied = self.state.piece_mask();

        while bishops != 0 {
            let pos = bishops.trailing_zeroes_with_reset();

            let mut attacks = self.get_ray_attack(RayDir::NorthEast, pos, occupied);
            attacks |= self.get_ray_attack(RayDir::NorthWest, pos, occupied);
            attacks |= self.get_ray_attack(RayDir::SouthEast, pos, occupied);
            attacks |= self.get_ray_attack(RayDir::SouthWest, pos, occupied);

            attacks &= !color_mask;

            self.add_moves_from_mask(pos, attacks);
        }
    }

    // https://www.chessprogramming.org/Classical_Approach
    /// Generates rook + queen moves.
    fn get_rook_moves(&mut self) {
        let color_mask = self.state.color[self.current_turn as usize];
        let mut rooks = self.state.pieces[PieceType::Rook as usize]
            | self.state.pieces[PieceType::Queen as usize] & color_mask;

        let occupied = self.state.piece_mask();

        while rooks != 0 {
            let pos = rooks.trailing_zeroes_with_reset();

            let mut attacks = self.get_ray_attack(RayDir::North, pos, occupied);
            attacks |= self.get_ray_attack(RayDir::South, pos, occupied);
            attacks |= self.get_ray_attack(RayDir::East, pos, occupied);
            attacks |= self.get_ray_attack(RayDir::West, pos, occupied);

            attacks &= !color_mask;

            self.add_moves_from_mask(pos, attacks);
        }
    }

    fn get_ray_attack(&self, dir: RayDir, mut square: u32, occupied: BitBoard) -> BitBoard {
        let mut attacks = RAY_LUT[square as usize * 8 + dir as usize];
        let blocker = attacks & occupied;
        if blocker != 0 {
            square = 63 - blocker.bit_scan(!dir.is_negative());
            attacks ^= RAY_LUT[square as usize * 8 + dir as usize];
        }

        attacks
    }

    fn add_moves_from_mask(&mut self, start: u32, mut mask: BitBoard) {
        while mask != 0 {
            let pos = mask.trailing_zeroes_with_reset();
            self.moves.push(PieceMove { start, target: pos });
        }
    }
}

struct LUTs;

impl LUTs {
    const fn gen_king_moves<const N: usize>() -> [BitBoard; N] {
        let mut moves = [0; N];
        let mut i = 0;
        while i < N {
            moves[i] = LUTs::king_moves(i as u32);
            i += 1;
        }

        moves
    }

    const fn king_moves(square: u32) -> BitBoard {
        let move_offsets: [i32; 8] = [-1, -9, -8, -7, 1, 7, 8, 9];
        let mut i = 0;
        let mut moves = 0;
        let bb = 1 << square;

        while i < move_offsets.len() {
            let offset = move_offsets[i];

            let target_square = if offset < 0 {
                bb >> -offset
            } else {
                bb << offset
            };

            if target_square != 0
                && LUTs::chebyshev_dist(square as i32, square as i32 + offset) == 1
            {
                moves |= target_square;
            }

            i += 1;
        }

        moves
    }

    /// Computes chebyshev distance between two squares.
    /// For more see: https://www.chessprogramming.org/Distance
    const fn chebyshev_dist(square1: i32, square2: i32) -> u32 {
        // square % 8
        let x1 = square1 & 7;
        let x2 = square2 & 7;

        // square / 8
        let y1 = square1 >> 3;
        let y2 = square2 >> 3;

        // std::cmp::max is not const so cannot be used.
        let v1 = (y2 - y1).abs();
        let v2 = (x2 - x1).abs();

        if v1 > v2 {
            v1 as u32
        } else {
            v2 as u32
        }
    }

    const fn gen_rays() -> [BitBoard; 8 * 8 * 8] {
        let mut rays: [BitBoard; 8 * 8 * 8] = [0; 8 * 8 * 8];

        let mut square = 0;
        while square < SQUARE_COUNT {
            let i = square * 8;
            rays[i + RayDir::North as usize] = LUTs::gen_ray(square as i32, 0, -1);
            rays[i + RayDir::NorthEast as usize] = LUTs::gen_ray(square as i32, 1, -1);
            rays[i + RayDir::NorthWest as usize] = LUTs::gen_ray(square as i32, -1, -1);
            rays[i + RayDir::East as usize] = LUTs::gen_ray(square as i32, 1, 0);

            rays[i + RayDir::SouthEast as usize] = LUTs::gen_ray(square as i32, 1, 1);
            rays[i + RayDir::South as usize] = LUTs::gen_ray(square as i32, 0, 1);
            rays[i + RayDir::SouthWest as usize] = LUTs::gen_ray(square as i32, -1, 1);
            rays[i + RayDir::West as usize] = LUTs::gen_ray(square as i32, -1, 0);

            square += 1;
        }

        rays
    }

    const fn gen_ray(square_idx: i32, dx: i32, dy: i32) -> BitBoard {
        let mut ray = 0;
        let mut x = square_idx % 8;
        let mut y = square_idx / 8;

        loop {
            x += dx;
            y += dy;

            // hit edge, ray is finished.
            if x < 0 || x >= 8 || y < 0 || y >= 8 {
                break;
            }

            ray |= 1 << (y * 8 + x);
        }

        ray
    }

    const fn gen_knight_moves() -> [BitBoard; 8 * 8] {
        let mut moves = [0; 8 * 8];
        let mut square = 0;

        while square < SQUARE_COUNT {
            moves[square] = LUTs::knight_moves_from_square(square as u32);
            square += 1;
        }

        moves
    }

    const fn knight_moves_from_square(square_idx: u32) -> BitBoard {
        let bb = 1 << square_idx;
        let mut moves = 0;

        let move_offsets: [i32; 8] = [-17, -15, -10, -6, 6, 10, 15, 17];
        let mut i = 0;
        while i < move_offsets.len() {
            let offset = move_offsets[i];

            let target_square = if offset < 0 {
                bb >> -offset
            } else {
                bb << offset
            };

            if target_square != 0
                && Self::m_dist(square_idx as i32, square_idx as i32 + offset) == 3
            {
                moves |= target_square;
            }

            i += 1;
        }

        moves
    }

    const fn m_dist(square1: i32, square2: i32) -> u32 {
        // square % 8
        let x1 = square1 & 7;
        let x2 = square2 & 7;

        // square / 8
        let y1 = square1 >> 3;
        let y2 = square2 >> 3;
        ((x1 - x2).abs() + (y1 - y2).abs()) as u32
    }
}

#[derive(Clone, Copy)]
enum RayDir {
    North = 0,
    NorthWest = 1,
    West = 2,
    NorthEast = 3,

    East = 4,
    SouthEast = 5,
    South = 6,
    SouthWest = 7,
}

impl RayDir {
    fn is_negative(self) -> bool {
        self as u32 > 3
    }
}

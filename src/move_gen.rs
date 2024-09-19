use crate::*;

pub(crate) fn generate_pseudo_legal(pos: &Position, moves: &mut Vec<ChessMove>) {
    generate_rook_moves(pos, moves);
    generate_bishop_moves(pos, moves);
    generate_knight_moves(pos, moves);

    let en_passant_mask = pos.en_passant_possible().as_mask() & (1 << pos.state.m.to());

    if pos.current_side == Color::White {
        generate_pawn_moves::<true>(pos, moves, en_passant_mask);
    } else {
        generate_pawn_moves::<false>(pos, moves, en_passant_mask);
    }

    let threat_mask = compute_threat_mask(pos, pos.opposite_side());
    generate_king_moves(pos, moves, !threat_mask);
}

fn generate_king_moves(pos: &Position, moves: &mut Vec<ChessMove>, movable_squares: BitBoard) {
    let king = pos.king(pos.current_side);
    let king_square = king.trailing_zeros();

    let mut move_mask = LUT::KING[king_square as usize];
    move_mask &= !pos.color_mask(pos.current_side);
    move_mask &= movable_squares;

    while move_mask != 0 {
        let to = move_mask.trailing_zeroes_with_reset();
        moves.push(ChessMove::new(king_square, to, 0));
    }

    gen_castling_moves(pos, moves, movable_squares);
}

pub fn compute_threat_mask(pos: &Position, color: Color) -> BitBoard {
    let mut mask = 0;
    let occupied = pos.all();

    let mut rooks = pos.rooks(color) | pos.queen(color);
    while rooks != 0 {
        let square = rooks.trailing_zeroes_with_reset();
        mask |= attack_mask_rook(square, occupied);
    }

    let mut bishops = pos.bishops(color) | pos.queen(color);
    while bishops != 0 {
        let square = bishops.trailing_zeroes_with_reset();
        mask |= attack_mask_bishop(square, occupied);
    }

    let mut knights = pos.knights(color);
    while knights != 0 {
        let square = knights.trailing_zeroes_with_reset();
        mask |= LUT::KNIGHT[square as usize];
    }

    const NOT_FILE_A: u64 = !0x0101010101010101;
    const NOT_FILE_H: u64 = !0x8080808080808080;

    let pawns = pos.pawns(color);
    if color == Color::White {
        mask |= ((pawns >> 9) & NOT_FILE_H) | ((pawns >> 7) & NOT_FILE_A);
    } else {
        mask |= ((pawns << 9) & NOT_FILE_A) | ((pawns << 7) & NOT_FILE_H);
    }

    let king = pos.king(color);
    mask |= LUT::KING[king.trailing_zeros() as usize];

    mask
}

fn gen_castling_moves(pos: &Position, moves: &mut Vec<ChessMove>, movable_squares: BitBoard) {
    let c = pos.current_side as usize;

    // Cannot castle if we are in check
    if pos.generate_checkers_mask().count_ones() != 0 {
        return;
    }

    let king = pos.king(pos.current_side);
    let king_sq = king.trailing_zeros();
    let occupied = pos.all() ^ pos.state.pieces[PieceType::Rook as usize];

    let has_to_be_clear_king = if c == 0 {
        between_bb(60, 63)
    } else {
        between_bb(4, 7)
    };

    let has_to_be_clear_queen = if c == 0 {
        between_bb(60, 56)
    } else {
        between_bb(4, 0)
    };

    let king_between = if c == 0 {
        between_bb(60, 62)
    } else {
        between_bb(4, 6)
    };

    let queen_between = if c == 0 {
        between_bb(60, 58)
    } else {
        between_bb(4, 2)
    };

    let is_between_attacked = king_between & movable_squares == king_between;
    let is_valid = king_between
        & !occupied
        & is_between_attacked.as_mask()
        & (has_to_be_clear_king & !occupied == has_to_be_clear_king).as_mask()
        == king_between;

    if pos.state.castling_rights[2 * c + 0] && is_valid {
        moves.push(ChessMove::new(
            king_sq,
            king_sq + 2,
            ChessMove::FLAG_CASTLE_KING,
        ));
    }

    let is_between_attacked = queen_between & movable_squares == queen_between;
    let is_valid = queen_between
        & !occupied
        & is_between_attacked.as_mask()
        & (has_to_be_clear_queen & !occupied == has_to_be_clear_queen).as_mask()
        == queen_between;

    if pos.state.castling_rights[2 * c + 1] && is_valid {
        moves.push(ChessMove::new(
            king_sq,
            king_sq - 2,
            ChessMove::FLAG_CASTLE_QUEEN,
        ));
    }
}

fn generate_knight_moves(pos: &Position, moves: &mut Vec<ChessMove>) {
    let mut knights = pos.knights(pos.current_side);

    while knights != 0 {
        let from = knights.trailing_zeroes_with_reset();
        let mut attacks = LUT::KNIGHT[from as usize];
        attacks &= !pos.color_mask(pos.current_side);

        while attacks != 0 {
            let to = attacks.trailing_zeroes_with_reset();
            moves.push(ChessMove::new(from, to, 0));
        }
    }
}

fn generate_pawn_moves<const WHITE: bool>(
    pos: &Position,
    moves: &mut Vec<ChessMove>,
    en_passant_mask: BitBoard,
) {
    let mut pawns = pos.pawns(pos.current_side);
    let occupied = pos.all();

    let double_push_rank = if WHITE { rank(5) } else { rank(2) };
    let en_passant_rank = if WHITE { rank(3) } else { rank(4) };
    let promotion_rank = if WHITE { rank(0) } else { rank(7) };

    let right_capture_shift = if WHITE { 7 } else { 9 };
    let left_capture_shift = if WHITE { 9 } else { 7 };

    const NOT_FILE_A: u64 = !0x0101010101010101;
    const NOT_FILE_H: u64 = !0x8080808080808080;

    while pawns != 0 {
        let from = pawns.trailing_zeroes_with_reset();
        let bb = 1 << from;

        // single push
        let mut move_mask = shift::<WHITE>(bb, 8) & !occupied;

        // double pushes
        move_mask |= shift::<WHITE>(move_mask & double_push_rank, 8) & !occupied;

        // Handle captures
        let right_captures = shift::<WHITE>(bb, right_capture_shift) & NOT_FILE_A;
        let left_captures = shift::<WHITE>(bb, left_capture_shift) & NOT_FILE_H;

        let normal_captures = (right_captures | left_captures) & occupied;
        move_mask |= normal_captures;
        move_mask &= !pos.color_mask(pos.current_side);

        // Handle promotions.
        let mut promotion_moves = move_mask & promotion_rank;
        move_mask &= !promotion_moves;

        while promotion_moves != 0 {
            let to = promotion_moves.trailing_zeroes_with_reset();
            moves.push(ChessMove::new(from, to, ChessMove::FLAG_QUEEN_PROMO));
            moves.push(ChessMove::new(from, to, ChessMove::FLAG_ROOK_PROMO));
            moves.push(ChessMove::new(from, to, ChessMove::FLAG_BISHOP_PROMO));
            moves.push(ChessMove::new(from, to, ChessMove::FLAG_KNIGHT_PROMO));
        }

        while move_mask != 0 {
            let to = move_mask.trailing_zeroes_with_reset();
            moves.push(ChessMove::new(from, to, 0));
        }

        // En passant
        let en_passant_left = (bb >> 1) & NOT_FILE_H & en_passant_mask;
        let en_passant_right = (bb << 1) & NOT_FILE_A & en_passant_mask;

        let en_passant_targets = (en_passant_left | en_passant_right) & en_passant_rank;
        let mut en_passant_moves = shift::<WHITE>(en_passant_targets, 8) & !occupied;

        if en_passant_moves != 0 {
            let to = en_passant_moves.trailing_zeroes_with_reset();
            moves.push(ChessMove::new(from, to, ChessMove::FLAG_EN_PASSANT));
        }
    }
}

fn shift<const RIGHT: bool>(v: BitBoard, shift: u32) -> BitBoard {
    if RIGHT {
        v >> shift
    } else {
        v << shift
    }
}

fn generate_bishop_moves(pos: &Position, moves: &mut Vec<ChessMove>) {
    let mut bishops = pos.bishops(pos.current_side) | pos.queen(pos.current_side);
    let occupied = pos.all();

    while bishops != 0 {
        let from = bishops.trailing_zeroes_with_reset();

        let mut attacks = attack_mask_bishop(from, occupied);
        attacks &= !pos.color_mask(pos.current_side);

        while attacks != 0 {
            let to = attacks.trailing_zeroes_with_reset();
            moves.push(ChessMove::new(from, to, 0));
        }
    }
}

fn generate_rook_moves(pos: &Position, moves: &mut Vec<ChessMove>) {
    let mut rooks = pos.rooks(pos.current_side) | pos.queen(pos.current_side);
    let occupied = pos.all();

    while rooks != 0 {
        let from = rooks.trailing_zeroes_with_reset();

        let mut attacks = attack_mask_rook(from, occupied);
        attacks &= !pos.color_mask(pos.current_side);

        while attacks != 0 {
            let to = attacks.trailing_zeroes_with_reset();
            moves.push(ChessMove::new(from, to, 0));
        }
    }
}

pub(crate) fn attack_mask_rook(square: u32, occupied: BitBoard) -> BitBoard {
    let mut attacks = get_ray_attack(RayDir::North, square, occupied);
    attacks |= get_ray_attack(RayDir::South, square, occupied);
    attacks |= get_ray_attack(RayDir::East, square, occupied);
    attacks |= get_ray_attack(RayDir::West, square, occupied);

    attacks
}

pub(crate) fn attack_mask_bishop(square: u32, occupied: BitBoard) -> BitBoard {
    let mut attacks = get_ray_attack(RayDir::NorthEast, square, occupied);
    attacks |= get_ray_attack(RayDir::NorthWest, square, occupied);
    attacks |= get_ray_attack(RayDir::SouthEast, square, occupied);
    attacks |= get_ray_attack(RayDir::SouthWest, square, occupied);

    attacks
}

fn get_ray_attack(dir: RayDir, mut square: u32, occupied: BitBoard) -> BitBoard {
    let mut attacks = LUT::RAY[square as usize * 8 + dir as usize];
    let blocker = attacks & occupied;
    if blocker != 0 {
        square = 63 - blocker.bit_scan(!dir.is_negative());
        attacks ^= LUT::RAY[square as usize * 8 + dir as usize];
    }

    attacks
}

pub(crate) struct LUT;

impl LUT {
    const KING: [BitBoard; NUM_SQUARES] = LUT::gen_king_moves();
    pub(crate) const KNIGHT: [BitBoard; NUM_SQUARES] = LUT::gen_knight_moves();
    const RAY: [BitBoard; NUM_SQUARES * 8] = LUT::gen_rays();

    // we might need this?
    #[allow(long_running_const_eval)]
    pub(crate) const BETWEEN_BBS: [BitBoard; NUM_SQUARES * NUM_SQUARES] = LUT::gen_between_bbs();

    // This is runtime since it might take some time.
    const fn gen_between_bbs() -> [BitBoard; NUM_SQUARES * NUM_SQUARES] {
        let mut between_bb = [0; NUM_SQUARES * NUM_SQUARES];

        let mut sq0: usize = 0;
        while sq0 < NUM_SQUARES {
            let x0 = sq0 & 7;
            let y0 = sq0 / 8;

            let mut sq1: usize = 0;
            while sq1 < NUM_SQUARES {
                let x1 = sq1 & 7;
                let y1 = sq1 / 8;

                let mut dx = x1 as i32 - x0 as i32;
                let mut dy = y1 as i32 - y0 as i32;

                if (dx != 0 && dy != 0) && dx.abs() != dy.abs() {
                    sq1 += 1;
                    continue;
                }

                if dx != 0 {
                    let sign = dx.signum();
                    dx /= dx;
                    dx *= sign;
                }

                if dy != 0 {
                    let sign = dy.signum();
                    dy /= dy;
                    dy *= sign;
                }

                let mut x = x0 as i32;
                let mut y = y0 as i32;
                let mut ray = 0;

                loop {
                    // hit edge, ray is finished.
                    if x == x1 as i32 && y == y1 as i32 {
                        break;
                    }

                    x += dx;
                    y += dy;

                    ray |= 1 << (y * 8 + x);
                }

                between_bb[sq0 * NUM_SQUARES + sq1] = ray;

                sq1 += 1;
            }

            sq0 += 1;
        }

        between_bb
    }

    const fn gen_king_moves() -> [BitBoard; NUM_SQUARES] {
        let mut moves = [0; NUM_SQUARES];
        let mut i = 0;
        while i < NUM_SQUARES {
            moves[i] = LUT::king_moves(i as u32);
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

            if target_square != 0 && LUT::chebyshev_dist(square as i32, square as i32 + offset) == 1
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
        while square < 64 {
            let i = square * 8;
            rays[i + RayDir::North as usize] = LUT::gen_ray(square as i32, 0, -1);
            rays[i + RayDir::NorthEast as usize] = LUT::gen_ray(square as i32, 1, -1);
            rays[i + RayDir::NorthWest as usize] = LUT::gen_ray(square as i32, -1, -1);
            rays[i + RayDir::East as usize] = LUT::gen_ray(square as i32, 1, 0);

            rays[i + RayDir::SouthEast as usize] = LUT::gen_ray(square as i32, 1, 1);
            rays[i + RayDir::South as usize] = LUT::gen_ray(square as i32, 0, 1);
            rays[i + RayDir::SouthWest as usize] = LUT::gen_ray(square as i32, -1, 1);
            rays[i + RayDir::West as usize] = LUT::gen_ray(square as i32, -1, 0);

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

        while square < 64 {
            moves[square] = LUT::knight_moves_from_square(square as u32);
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

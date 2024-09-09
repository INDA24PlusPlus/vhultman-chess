use crate::rank;
use crate::Board;
use crate::Color;
use crate::PieceMove;
use crate::PieceType;
use crate::BOARD_SIZE;

impl Board {
    pub(crate) fn get_pawn_moves(&mut self) {
        let move_mask = self.get_pawns_move_mask();
        let capture_mask = self.get_pawn_capture_mask();

        self.pawn_moves_from_move_mask(move_mask);
        self.pawn_moves_from_capture_mask(capture_mask);
    }

    fn get_pawns_move_mask(&self) -> u64 {
        let all_pieces = self.state.piece_mask().0;
        let pawn_mask = self.state.pieces[PieceType::Pawn as usize].0;
        let piece_mask = pawn_mask & self.state.color[self.current_turn as usize].0;

        if self.current_turn == Color::White {
            let single_push_mask = (piece_mask >> BOARD_SIZE) & !all_pieces;
            let double_push_mask = (single_push_mask & rank(5)) >> BOARD_SIZE & !all_pieces;

            return single_push_mask | double_push_mask;
        } else {
            let single_push_mask = (piece_mask << BOARD_SIZE) & !all_pieces;
            let double_push_mask = (single_push_mask & rank(2)) << BOARD_SIZE & !all_pieces;

            return single_push_mask | double_push_mask;
        }
    }

    fn get_pawn_capture_mask(&self) -> u64 {
        let pawn_mask = self.state.pieces[PieceType::Pawn as usize].0
            & self.state.color[self.current_turn as usize].0;

        let enemy_mask = self.state.color[self.current_turn as usize + 1 & 1].0;
        const FILE_A: u64 = 0x0101010101010101;
        const FILE_H: u64 = 0x8080808080808080;

        if self.current_turn == Color::White {
            // right side.
            let mut captures = (pawn_mask >> 9) & !FILE_A & enemy_mask;

            // left side.
            captures |= (pawn_mask >> 7) & !FILE_H & enemy_mask;

            return captures;
        } else {
            // right side.
            let mut captures = (pawn_mask << 9) & !FILE_A & enemy_mask;

            // left side.
            captures |= (pawn_mask << 7) & !FILE_H & enemy_mask;

            return captures;
        }
    }

    fn pawn_moves_from_capture_mask(&mut self, mut capture_mask: u64) {
        let piece_mask = self.state.pieces[PieceType::Pawn as usize].0;

        // NOTE: I am lazy so screw code deduplication.
        if self.current_turn == Color::White {
            while capture_mask != 0 {
                let pos = capture_mask.trailing_zeros();
                let curr_square = 1 << pos;

                let right = curr_square << 9 & piece_mask != 0;
                let left = curr_square << 7 & piece_mask != 0;

                if right {
                    self.moves.push(PieceMove {
                        start: pos + 9,
                        target: pos,
                    });
                }

                if left {
                    self.moves.push(PieceMove {
                        start: pos + 7,
                        target: pos,
                    });
                }

                capture_mask ^= curr_square;
            }
        } else {
            while capture_mask != 0 {
                let pos = capture_mask.trailing_zeros();
                let curr_square = 1 << pos;

                let right = curr_square >> 9 & piece_mask != 0;
                let left = curr_square >> 7 & piece_mask != 0;

                if right {
                    self.moves.push(PieceMove {
                        start: pos - 9,
                        target: pos,
                    });
                }

                if left {
                    self.moves.push(PieceMove {
                        start: pos - 7,
                        target: pos,
                    });
                }

                capture_mask ^= curr_square;
            }
        }
    }

    fn pawn_moves_from_move_mask(&mut self, mut move_mask: u64) {
        let piece_mask = self.state.pieces[PieceType::Pawn as usize].0;
        while move_mask != 0 {
            let pos = move_mask.trailing_zeros();

            let curr_square = 1 << pos;
            let is_double_push = if self.current_turn == Color::White {
                (curr_square << 2 * BOARD_SIZE) & piece_mask != 0
            } else {
                (curr_square >> 2 * BOARD_SIZE) & piece_mask != 0
            };

            // White -> 1, Black -> -1
            let mult = 1 - 2 * self.current_turn as i32;

            if is_double_push {
                self.moves.push(PieceMove {
                    start: (pos as i32 + mult * 2 * BOARD_SIZE as i32) as u32,
                    target: pos,
                });
            }

            self.moves.push(PieceMove {
                start: (pos as i32 + mult * BOARD_SIZE as i32) as u32,
                target: pos,
            });

            move_mask ^= curr_square;
        }
    }
}

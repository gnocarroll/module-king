// utility functionality related to Expr struct

use crate::{
    parse::{AST, Expr, ExprVariant, If, Operation, Tokens, ast_contents::ExprID},
    scan::Token,
};

impl AST {
    pub fn expr(&self, expr: ExprID) -> &Expr {
        self.objs.expr(expr)
    }

    pub fn expr_mut(&mut self, expr: ExprID) -> &mut Expr {
        self.objs.expr_mut(expr)
    }

    pub fn expr_unit(&mut self, tok_idx: u32) -> ExprID {
        self.expr_push(Expr {
            tok: tok_idx,
            end_tok: tok_idx,
            variant: ExprVariant::Unit,
            ..Default::default()
        })
    }

    pub fn expr_underscore(&mut self, tok_idx: u32) -> ExprID {
        self.expr_push(Expr {
            tok: tok_idx,
            end_tok: tok_idx + 1,
            variant: ExprVariant::Underscore,
            ..Default::default()
        })
    }

    pub fn expr_dollar_number(&mut self, tokens: &Tokens, tok_idx: u32, tok: &Token) -> ExprID {
        let mut chars = tokens.tok_as_str(tok).chars();

        chars.next();

        let val = chars
            .as_str()
            .parse::<u64>()
            .expect("DollarNumber scanning broken");

        self.expr_push(Expr {
            tok: tok_idx,
            end_tok: tok_idx + 1,
            variant: ExprVariant::DollarNumber(val),
            ..Default::default()
        })
    }

    pub fn expr_push(&mut self, expr: Expr) -> ExprID {
        self.objs.expr_push(expr)
    }

    pub fn expr_infix(&mut self, op: Token, lhs: ExprID, rhs: ExprID) -> ExprID {
        self.expr_push(Expr {
            tok: self.objs.expr(lhs).tok,
            end_tok: self.objs.expr(rhs).end_tok,
            variant: ExprVariant::Operation(Operation {
                op: op.ttype,
                operand1: Some(lhs),
                operand2: Some(rhs),
            }),
            ..Default::default()
        })
    }

    pub fn expr_postfix_around(
        &mut self,
        op: Token,
        lhs: ExprID,
        rhs: ExprID,
        found_end: bool,
    ) -> ExprID {
        self.expr_push(Expr {
            tok: self.expr(lhs).tok,
            end_tok: self.expr(rhs).end_tok + if found_end { 1 } else { 0 },
            variant: ExprVariant::Operation(Operation {
                op: op.ttype,
                operand1: Some(lhs),
                operand2: Some(rhs),
            }),
            ..Default::default()
        })
    }

    pub fn expr_prefix(&mut self, op: Token, rhs: ExprID) -> ExprID {
        self.expr_push(Expr {
            tok: self.expr(rhs).tok - 1,
            end_tok: self.expr(rhs).end_tok,
            variant: ExprVariant::Operation(Operation {
                op: op.ttype,
                operand1: Some(rhs),
                operand2: None,
            }),
            ..Default::default()
        })
    }

    pub fn expr_postfix(&mut self, op: Token, rhs: ExprID) -> ExprID {
        self.expr_push(Expr {
            tok: self.expr(rhs).tok,
            end_tok: self.expr(rhs).end_tok + 1,
            variant: ExprVariant::Operation(Operation {
                op: op.ttype,
                operand1: Some(rhs),
                operand2: None,
            }),
            ..Default::default()
        })
    }

    pub fn expr_around(&mut self, op: Token, rhs: ExprID, found_end: bool) -> ExprID {
        self.expr_push(Expr {
            tok: self.expr(rhs).tok - 1,
            end_tok: self.expr(rhs).end_tok + if found_end { 1 } else { 0 },
            variant: ExprVariant::Operation(Operation {
                op: op.ttype,
                operand1: Some(rhs),
                operand2: None,
            }),
            ..Default::default()
        })
    }

    pub fn expr_prefix_around(&mut self, op: Token, lhs: ExprID, rhs: ExprID) -> ExprID {
        self.expr_push(Expr {
            tok: self.expr(lhs).tok - 1,
            end_tok: self.expr(rhs).end_tok,
            variant: ExprVariant::Operation(Operation {
                op: op.ttype,
                operand1: Some(lhs),
                operand2: Some(rhs),
            }),
            ..Default::default()
        })
    }

    pub fn expr_if(
        &mut self,
        cond: ExprID,
        body: ExprID,
        else_expr: Option<ExprID>,
        is_elif: bool,
    ) -> ExprID {
        let if_struct = If {
            cond,
            body,
            else_expr,
        };

        self.expr_push(Expr {
            tok: self.expr(cond).tok - 1,
            end_tok: self.expr(body).end_tok,
            variant: if is_elif {
                ExprVariant::Elif(if_struct)
            } else {
                ExprVariant::If(if_struct)
            },
            ..Default::default()
        })
    }
}

// placing this in separate module to ensure that order of members in scope is recorded

use std::collections::HashMap;

use crate::parse::{ast_contents::MemberID, errors::DuplicateName};

#[derive(Clone, Default)]
pub struct ScopeMembers {
    map: HashMap<String, MemberID>,
    order: Vec<String>,

    member_to_idx: HashMap<MemberID, usize>,
}

impl ScopeMembers {
    pub fn get(&self, s: &String) -> Option<MemberID> {
        self.map.get(s).map(|id| *id)
    }

    pub fn nth_member(&self, idx: usize) -> MemberID {
        let key = &self.order[idx];

        self.map[key]
    }

    pub fn get_map(&self) -> &HashMap<String, MemberID> {
        &self.map
    }

    pub fn member_count(&self) -> usize {
        self.order.len()
    }

    pub fn member_idx(&self, member_id: MemberID) -> Option<usize> {
        self.member_to_idx.get(&member_id).map(|idx| *idx)
    }

    // try to insert to scope, return error if name exists
    pub fn insert(&mut self, s: String, member: MemberID) -> Result<MemberID, DuplicateName> {
        if let Some(old_member) = self.map.get(&s) {
            return Err(DuplicateName {
                name: s,
                old_member: *old_member,
                new_member: member,
            });
        }

        self.order.push(s.clone());
        self.map.insert(s, member);

        // it is at last index of order vec so index is len - 1

        self.member_to_idx.insert(member, self.order.len() - 1);

        Ok(member)
    }
}
// placing this in separate module to ensure that order of members in scope is recorded

use std::collections::HashMap;

use crate::parse::ast_contents::MemberID;

#[derive(Clone, Default)]
pub struct ScopeMembers {
    map: HashMap<String, MemberID>,
    order: Vec<String>,
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

    pub fn insert(&mut self, s: String, member: MemberID) {
        if self.map.contains_key(&s) {
            panic!("duplicate key ScopeMembers");
        }

        self.order.push(s.clone());
        self.map.insert(s, member);
    }
}
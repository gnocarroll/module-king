// Some global types, analogous to Rust preamble
// currently this does not work since need to implement
// 1. specifying discriminant for enum members
// 2. import names from enum

type Integer is integer;

type Character is new Integer;

type Float is float;

type Boolean is enum
    false;
    true;
end enum;

from Boolean import true, false;
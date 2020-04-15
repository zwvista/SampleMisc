mod examples1;
mod examples2;
mod examples3;
mod jys;

use examples1::*;
use examples2::*;
use examples3::*;
use jys::*;

extern crate itertools;

fn main() {
    example11();
    example12();
    example13();
    example14();

    example21();
    example22();
    example23();

    example31();
    example32();
    example33();
    example34();

    jys();
}
/*
rect1 is Rectangle { width: 30, height: 50 }
rect1 is Rectangle {
    width: 30,
    height: 50
}
The area of the rectangle is 1500 square pixels.
Can rect1 hold rect2? true
Can rect1 hold rect3? false
State quarter from Alaska!
State quarter from Alaska!
24, Зд
З д р а в с т в у й т е
*/

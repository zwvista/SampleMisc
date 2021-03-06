mod examples1;
mod examples2;
mod examples3;
mod examples4;
mod examples5;
mod examples6;
mod jys;
mod regexes;
mod jsons;
mod rest;

use examples1::*;
use examples2::*;
use examples3::*;
use examples4::*;
use examples5::*;
use examples6::*;
use jys::*;
use regexes::*;
use jsons::*;
use rest::*;

#[tokio::main]
async fn main() {
    // jys();

    // example11();
    // example12();
    // example13();
    // example14();

    // example21();
    // example22();
    // example23();

    // example31();
    // example32();
    // example33();
    // example34();

    // example41();
    // example42();
    // example43();

    // example51();
    // example52();
    // example53();

    // example61();
    // example62();

    // regex1();

    // json1();

    rest1().await.unwrap();
}

use std::error::Error;
use regex::{Regex, Captures};
use itertools::Itertools;

pub fn regex1() -> Result<(), Box<dyn Error>> {
    let s = "123-4567-89,987-6543-21";
    let r = Regex::new(r"\d{3}-(\d{4})-\d{2}")?;
    if r.is_match(s) { // if let m = r.find(s) {
        println!("Found Matches:")
    }
    for (i, c) in r.captures_iter(&s).enumerate() {
        for j in 0..c.len() {
            println!("group {},{} : {}", i, j, &c[j]);
        }
    }

    let r2 = Regex::new(r"(\d+)-(\d+)-(\d+)")?;
    let s2 = r2.replace_all(&s, "$3-$1-$2");
    println!("{}", s2);

    let r3 = Regex::new(r"\d+")?;
    let s3 = r3.replace_all(&s, |c: &Captures| c[0].chars().rev().collect::<String>());
    println!("{}", s3);

    let r4 = Regex::new("%(begin|next|end)%")?;
    let s4 = "%begin%hello%next%world%end%";
    let v = r4.split(s4).collect_vec();
    println!("{:?}", v);

    Ok(())
}

/*
Found Matches:
group 0,0 : 123-4567-89
group 0,1 : 4567
group 1,0 : 987-6543-21
group 1,1 : 6543
89-123-4567,21-987-6543
321-7654-98,789-3456-12
["", "hello", "world", ""]
*/

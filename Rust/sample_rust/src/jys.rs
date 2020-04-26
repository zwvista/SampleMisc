use itertools::Itertools;
use reduce::Reduce;

pub fn jys() {
    let text = "床前明月光疑是地上霜举头望明月低头思故乡";
    let offset = 5;
    let f = |x: &(usize, char)| x.0 % offset;
    for (_, group) in text.chars().enumerate().sorted_by_key(&f).group_by(&f).into_iter() {
        // let s: String = group.map(|x| x.1).collect_vec().into_iter().rev().intersperse('|').collect();
        // let s = group.map(|x| x.1.to_string()).fold(String::new(), |acc, c| if acc.is_empty() {c} else {c + "|" + &acc});
        let s = group.map(|x| x.1.to_string()).reduce(|acc, c| c + "|" + &acc).unwrap();
        println!("{}", s);
    }
}

/*
低|举|疑|床
头|头|是|前
思|望|地|明
故|明|上|月
乡|月|霜|光
*/

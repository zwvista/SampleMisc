use itertools::Itertools;

pub fn jys() {
    let text = "床前明月光疑是地上霜举头望明月低头思故乡";
    let offset = 5;
    for (_, group) in text.chars().enumerate().sorted_by_key(|x| x.0 % offset).group_by(|x| x.0 % offset).into_iter() {
        // let s: String = group.map(|x| x.1).collect_vec().into_iter().rev().intersperse('|').collect();
        let s = group.map(|x| x.1.to_string()).fold(String::new(), |acc, c| if acc.is_empty() {c} else {c + "|" + &acc});
        println!("{}", s);
    }
}

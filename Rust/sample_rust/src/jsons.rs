use std::error::Error;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug)]
struct Persons {
    // #[serde(rename(serialize = "persons", deserialize = "persons"))]
    persons: Vec<Person>,
}

#[derive(Serialize, Deserialize, Debug)]
struct Person {
    // #[serde(rename(serialize = "name", deserialize = "name"))]
    name: String,
    // #[serde(rename(serialize = "age", deserialize = "age"))]
    age: i32,
}

pub fn json1() -> Result<(), Box<dyn Error>> {
    let json_str = r#"{
        "persons" : [
            {
                "name" : "Joe",
                "age" : 12
            }
        ]
    }"#;
    let v: Persons = serde_json::from_str(&json_str)?;
    println!("{:?}", v);
    let json_str = serde_json::to_string(&v)?;
    println!("{}", json_str);
    let json_str = serde_json::to_string_pretty(&v)?;
    println!("{}", json_str);

    Ok(())
}

/*
Persons { persons: [Person { name: "Joe", age: 12 }] }
{"persons":[{"name":"Joe","age":12}]}
{
  "persons": [
    {
      "name": "Joe",
      "age": 12
    }
  ]
}
*/

//! This is the file where you will write your code.
#![allow(unused_imports)]

use std::{
    collections::HashMap,
    fs::File,
    io::{BufRead, BufReader},
    iter,
    ops::Range,
};

use regex::Regex;

/// Sums the numbers from 1 to `n`, where `n` is an arbitrary positive
/// integer.
/// # Example
///
/// ```
/// # use project_6::gauss;
/// assert_eq!(gauss(10), Some(55));
/// ```

pub fn gauss(n: i32) -> Option<i32> {
    if n <= 0 {
        None
    } else {
        let mut sum = 0;
        let mut i = 1;
        while i <= n {
            sum = sum + i;
            i = i + 1;
        }
        Some(sum)
    }
}

/// This function takes in a slice of integers and returns a vec of integers
/// The returned vec is double the slice but with a twist
/// It doubles up and dances!

pub fn double_up_and_dance(dancer_slice: &[i32]) -> Vec<i32> {
    let mut count = 1;
    let mut answer = Vec::new();
    for element in dancer_slice {
        if count == 3 {
            answer.push(element * (dancer_slice[1]));
            answer.push(*element);
        } else {
            answer.push(*element);
            answer.push(*element);
        }
        count = count + 1;
    }
    answer
}

/// This function takes in an integer and returns the equivalent binary string

pub fn to_binstring(num: u32) -> String {
    let mut div = num;
    let mut answer = String::new();
    if div == 0 {
        String::from("0")
    } else {
        while div != 0 {
            answer.push(char::from_digit(div % 2, 10).unwrap());
            div = div / 2;
        }
        answer.chars().rev().collect()
    }
}

/// This function takes in an iterable of integers,
/// and counts how many elements in the iterator are within the given range

pub fn in_range(itemlist: impl IntoIterator<Item = i32>, range: Range<i32>) -> usize {
    itemlist.into_iter().fold(0, |acc, ele| {
        if ele >= range.start && ele < range.end {
            acc + 1
        } else {
            acc
        }
    })
}

/// Given an iterator over mutable strings, this function will capitalize the
/// first letter of each word in the iterator, in place.

pub fn capitalize_words<'a>(wordlist: impl IntoIterator<Item = &'a mut String>) {
    for ele in wordlist {
        let first: String = ele.chars().next().unwrap().to_uppercase().collect();
        let answer = first + &ele[1..];
        *ele = answer;
    }
}

/// Given a txt file, parse and return the items sold by a vending machine, along
/// with their prices.

pub fn read_prices(filename: &str) -> Option<HashMap<String, u32>> {
    let mut map = HashMap::new();
    let comment = Regex::new("^;.*$").unwrap();
    let item = Regex::new("(\\s*[A-Za-z]*\\s*)*;").unwrap();
    let price = Regex::new("\\s*[0-9]+\\s*(c|cents)\\s*").unwrap();
    let file = File::open(filename).ok()?;
    let reader = BufReader::new(file);
    for reader_line in reader.lines() {
        let line = reader_line.ok()?;
        if comment.is_match(&line) {
        } else if item.is_match(&line) {
            let item_full = item.find(&line).unwrap().as_str();
            let item_str = (item_full[..item_full.len() - 1])
                .to_string()
                .trim()
                .to_string();
            if price.is_match(&line) {
                let price_full = price.find(&line).unwrap().as_str();
                let price_str: String = price_full.chars().filter(|x| x.is_ascii_digit()).collect();
                let price_num = price_str.parse().ok()?;
                if price_num >= 1 && price_num <= 50 {
                    match map.get(&item_str) {
                        None => {
                            map.insert(item_str, price_num);
                        }
                        _ => return None,
                    }
                } else {
                    return None;
                }
            } else {
                return None;
            }
        } else {
            return None;
        }
    }
    Some(map)
}

// STUDENT TESTS:
#[test]
fn student_test1() {
    assert_eq!(
        read_prices("inputs/file1.txt"),
        Some(HashMap::from([
            ("ice cream".to_owned(), 10),
            ("sandwich".to_owned(), 49),
            ("hot dog".to_owned(), 49)
        ]))
    );
}

use std::{collections::HashMap, iter};

use project_6::{
    capitalize_words, double_up_and_dance, gauss, in_range, read_prices, to_binstring,
};

#[test]
fn test_gauss() {
    assert_eq!(gauss(1), Some(1));
    assert_eq!(gauss(5), Some(15));
    assert_eq!(gauss(10), Some(55));
    assert_eq!(gauss(19), Some(190));
}

#[test]
fn test_gauss2() {
    assert_eq!(gauss(-2), None);
    assert_eq!(gauss(-400), None);
    assert_eq!(gauss(330), Some(54615));
    assert_eq!(gauss(0), None);
}

#[test]
fn test_double_up_and_dance() {
    assert_eq!(double_up_and_dance(&[-9, 7]), [-9, -9, 7, 7]);
    assert_eq!(double_up_and_dance(&[]), []);
}

#[test]
fn test_double_up_and_dance2() {
    assert_eq!(double_up_and_dance(&[1, 2, 2]), [1, 1, 2, 2, 4, 2]);
    assert_eq!(double_up_and_dance(&[1, 2, 2, 8]), [1, 1, 2, 2, 4, 2, 8, 8]);
}

#[test]
fn test_in_range1() {
    assert_eq!(in_range([5, 2, 1, 3, 9], 2..6), 3);
    assert_eq!(in_range([5, 2, 1, 3, 9], 3..5), 1);
    assert_eq!(in_range(vec![5, 2, 1, 3, 9], 2..11), 4);
    assert_eq!(in_range([1, 3, 5], 2..5), 1);
    assert_eq!(in_range([1, 2, 3, 5, 6], 2..5), 2);
}

#[test]
fn test_in_range2() {
    assert_eq!(in_range([], 2..11), 0); // check empty handling
    assert_eq!(in_range([-4, -3, -2, -1, 0, 1, 2, 3, 4], -3..3), 6); // ensure negatives handled
    assert_eq!(in_range(0..100, 25..51), 26); // larger range check
}

#[test]
fn test_to_binstring1() {
    assert_eq!(to_binstring(1), "1",);
    assert_eq!(to_binstring(2), "10");
    assert_eq!(to_binstring(3), "11");
    assert_eq!(to_binstring(6), "110");
    assert_eq!(to_binstring(9), "1001");
    assert_eq!(to_binstring(32), "100000");
    assert_eq!(to_binstring(35), "100011");
}

#[test]
fn test_to_binstring2() {
    assert_eq!(to_binstring(0), "0"); // special case of 0
    assert_eq!(to_binstring(511), "111111111");
    assert_eq!(to_binstring(900), "1110000100");
    assert_eq!(to_binstring(1024), "10000000000");
    assert_eq!(to_binstring(1099), "10001001011");
    assert_eq!(to_binstring(41952), "1010001111100000");
    assert_eq!(to_binstring(76351), "10010101000111111");
}

#[test]
fn test_capitalize_words() {
    // Testing modifying in place

    let mut tester = vec!["cmsc330".to_owned(), "is".to_owned(), "great!".to_owned()];
    capitalize_words(&mut tester);
    let expected = ["Cmsc330".to_owned(), "Is".to_owned(), "Great!".to_owned()];
    assert_eq!(tester, expected);

    let mut tester2 = ["hello".to_owned(), "world".to_owned()];
    capitalize_words(&mut tester2);
    let expected2 = ["Hello".to_owned(), "World".to_owned()];
    assert_eq!(tester2, expected2);
}

#[test]
fn test_capitalize_words_empty() {
    let mut tester3 = [];
    capitalize_words(&mut tester3);
    let expected3: [String; 0] = [];
    assert_eq!(tester3, expected3);

    capitalize_words(iter::empty());
}

#[test]
fn test_read_prices_valid() {
    assert_eq!(
        read_prices("inputs/file1.txt"),
        Some(HashMap::from([
            ("ice cream".to_owned(), 10),
            ("sandwich".to_owned(), 49),
            ("hot dog".to_owned(), 49)
        ]))
    );

    assert_eq!(read_prices("inputs/empty.txt"), Some(HashMap::new()));
}

#[test]
fn test_read_prices_invalid() {
    assert_eq!(read_prices("inputs/file2.txt"), None);
    assert_eq!(read_prices("inputs/file3.txt"), None);
    assert_eq!(read_prices("inputs/file4.txt"), None);
    assert_eq!(read_prices("inputs/file5.txt"), None);
}

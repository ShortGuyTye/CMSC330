use regex::Regex;
use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

use crate::types::{Memory, RefCountMem};
use crate::utils::*;

pub fn deref(mut mem: RefCountMem, vec: &Vec<u32>) -> RefCountMem {
    let mut to_deref: Vec<(u32, Option<Vec<u32>>)> = vec![];
    for &ele in vec {
        let (ref_val, count) = &mem.heap[ele as usize];
        if *count > 1 {
            mem.heap[ele as usize] = (ref_val.clone(), count - 1);
        } else {
            to_deref.push((ele, ref_val.clone()));
        }
    }
    for (ele, maybe_children) in to_deref {
        if let Some(children) = maybe_children {
            mem = deref(mem, &children);
        }
        mem.heap[ele as usize] = (None, 0);
    }
    mem
}

pub fn reference_counting(string_vec: Vec<String>) -> RefCountMem {
    let stack = Regex::new("Ref Stack(\\s[0-9])+").unwrap();
    let heap = Regex::new("Ref Heap(\\s[0-9])+").unwrap();
    let pop = Regex::new("Pop").unwrap();
    let nums = Regex::new("(\\s[0-9])+").unwrap();
    let mut mem = RefCountMem {
        stack: vec![],
        heap: vec![(None, 0); 10],
    };

    for ele in string_vec {
        if stack.is_match(&ele) {
            let numbers = nums.captures(&ele).unwrap();
            let mut num_v = vec![];
            for ele in numbers[0].split_whitespace() {
                num_v.push(int_of_string(ele.to_string()))
            }
            mem.stack.push(num_v.clone());
            for ele in num_v {
                if mem.heap[ele as usize] == (None, 0) {
                    mem.heap[ele as usize] = (Some(vec![]), 1);
                } else {
                    match &mem.heap[ele as usize] {
                        (x, y) => mem.heap[ele as usize] = (x.clone(), y + 1),
                    }
                }
            }
        } else if heap.is_match(&ele) {
            let numbers = nums.captures(&ele).unwrap();
            let mut num_v = vec![];
            for ele in numbers[0].split_whitespace() {
                num_v.push(int_of_string(ele.to_string()))
            }
            match &mem.heap[num_v[0] as usize] {
                (x, y) => mem.heap[num_v[0] as usize] = (Some(num_v[1..].to_vec()), *y),
            }
            for ele in num_v[1..].to_vec() {
                if mem.heap[ele as usize] == (None, 0) {
                    mem.heap[ele as usize] = (Some(vec![]), 1);
                } else {
                    match &mem.heap[ele as usize] {
                        (x, y) => mem.heap[ele as usize] = (x.clone(), y + 1),
                    }
                }
            }
        } else if pop.is_match(&ele) {
            let last = &mem.stack.last().unwrap().clone();
            mem = deref(mem, last);
            mem.stack.pop();
        } else {
        }
    }
    mem
}
// suggested helper function. You may modify parameters as you wish.
// Takes in some form of stack and heap and returns all indicies in heap
// that can be reached.
pub fn reachable(stack: Vec<u32>, heap: &Vec<Option<(String, Vec<u32>)>>, mut reached: Vec<u32>) -> Vec<u32> {
    for ele in stack {
        if !(reached.contains(&ele)) {
            reached.push(ele);
            let (_s, v) = heap.get(ele as usize).unwrap().clone().unwrap();
            reached.append(&mut reachable(v, heap, reached.clone()));
        }
    }
    reached
}

pub fn mark_and_sweep(mem: &mut Memory) -> () {
    let mut reached = vec![];
    for ele in &mem.stack {
        reached.append(&mut reachable(ele.to_vec(), &mem.heap, vec![]),);
    }
    let mut count = 0;
    let mut new_heap = vec![];
    for ele in &mem.heap {
        if count < mem.heap.len() as u32 {
            if !(reached.contains(&count)) {
                new_heap.push(None);
            } else {
                new_heap.push(ele.clone());
            }
            count = count + 1;
        }
    }
    mem.heap = new_heap;
}

// alive says which half is CURRENTLY alive. You must copy to the other half.
// 0 for left side currently in use, 1 for right side currently in use
pub fn stop_and_copy(mem: &mut Memory, alive: u32) -> () {
    let mut reached = vec![];
    for ele in &mem.stack {
        reached.append(&mut reachable(ele.to_vec(), &mem.heap, vec![]),);
    }
    let mut new_space = vec![];
    for ele in reached {
        new_space.push(mem.heap[ele as usize].clone());
    }
    if alive == 0 {
        
        
    } else if alive == 1 {

    }
}

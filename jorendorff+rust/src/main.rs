#![feature(iter_arith)]

use std::path::{Path, PathBuf};
use std::fs::File;
use std::io;
use std::io::prelude::*;

struct Observation {
    label: u8,
    pixels: Vec<u8>
}

fn distance(training_pixels: &[u8], test_pixels: &[u8]) -> u64 {
    training_pixels
        .iter()
        .zip(test_pixels)
        .map(|(&a, &b)| (a as i64 - b as i64).abs() as u64)
        .sum()
}

fn predict(training_data: &Vec<Observation>, test_pixels: &[u8]) -> u8 {
    training_data
        .iter()
        .map(|obs| (obs, distance(&obs.pixels, test_pixels)))
        .min_by_key(|pair| pair.1)
        .unwrap()
        .0
        .label
}

fn read_csv_file(filename: &Path) -> io::Result<Vec<Observation>> {
    let f = try!(File::open(filename));
    let mut reader = io::BufReader::new(f);

    // Discard the first line.
    let mut line = String::new();
    try!(reader.read_line(&mut line));

    let mut v = vec![];
    for line in reader.lines() {
        let line = try!(line);
        let mut iter = line.split(',');
        let label: u8 = iter.next().unwrap().parse().expect("error parsing input file");
        let parsing_result: Result<Vec<u8>, _> = line.split(',').map(|s| s.parse()).collect();
        let pixels: Vec<u8> = parsing_result.expect("error parsing input file");
        v.push(Observation { label: label, pixels: pixels });
    }
    Ok(v)
}

fn fallible_main() -> io::Result<f64> {
    let training_data = try!(read_csv_file(&PathBuf::from("../training-sample.csv")));
    let test_data = try!(read_csv_file(&PathBuf::from("../test-sample.csv")));
    let mut passed = 0;
    let mut total = 0;
    for obs in test_data {
        if predict(&training_data, &obs.pixels) == obs.label {
            passed += 1;
        }
        total += 1;
    }
    Ok(100.0 * passed as f64 / total as f64)
}

fn main() {
    match fallible_main() {
        Ok(result) => println!("{}", result),
        Err(err) => println!("{}", err)
    }
}

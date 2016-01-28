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
        .map(|(&a, &b)| (a as i32 - b as i32).abs() as u64)
        .fold(0, |a, b| a + b)
}

fn display_pixel(x: u8) -> char {
    match x {
        0 ... 10 => ' ',
        11 ... 64 => '.',
        65 ... 160 => ':',
        161 ... 255 => '#',
        _ => unreachable!("i have discovered an exciting new u8 value: {}", x)
    }
}

fn display_miss(test: &Observation, best: &Observation) {
    assert_eq!(test.pixels.len(), best.pixels.len());
    println!("Test case ({}):                  Best match ({}):", test.label, best.label);
    for r in 0 .. 28 {
        let mut s = String::new();
        for c in 0 .. 28 {
            s.push(display_pixel(test.pixels[r * 28 + c]));
        }
        for _ in 0 .. 4 {
            s.push(' ');
        }
        for c in 0 .. 28 {
            s.push(display_pixel(best.pixels[r * 28 + c]));
        }
        println!("{}", s);
    }
    println!("");
}

fn predict<'a, 'b>(training_data: &'a Vec<Observation>, test_pixels: &'b [u8]) -> &'a Observation {
    training_data
        .iter()
        .map(|obs| (obs, distance(&obs.pixels, test_pixels)))
        .min_by_key(|pair| pair.1)
        .unwrap()
        .0
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
    for test_case in test_data {
        let best_match = predict(&training_data, &test_case.pixels);
        if best_match.label == test_case.label {
            passed += 1;
        } else {
            display_miss(&test_case, best_match);
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

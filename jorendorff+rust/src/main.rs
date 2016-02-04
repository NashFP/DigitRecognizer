use std::fs::File;
use std::io::{self, BufRead, BufReader};

struct Observation {
    label: u8,
    pixels: Vec<u8>
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

fn display_images(test: &Observation, best: &Observation) {
    const WIDTH: usize = 28;
    const HEIGHT: usize = 28;
    assert_eq!(test.pixels.len(), WIDTH * HEIGHT);
    assert_eq!(best.pixels.len(), WIDTH * HEIGHT);
    println!("Test case ({}):                  Best match ({}):", test.label, best.label);
    for r in 0 .. HEIGHT {
        let mut s = String::new();
        for c in 0 .. WIDTH {
            s.push(display_pixel(test.pixels[r * WIDTH + c]));
        }
        for _ in 0 .. 4 {
            s.push(' ');
        }
        for c in 0 .. WIDTH {
            s.push(display_pixel(best.pixels[r * WIDTH + c]));
        }
        println!("{}", s);
    }
    println!("{}", if test.label == best.label { "hit!" } else { "miss" });
    println!("");
}

fn distance(training_pixels: &[u8], test_pixels: &[u8]) -> u64 {
    training_pixels
        .iter()
        .zip(test_pixels)
        .map(|(a, b)| (*a as i32 - *b as i32).abs() as u64)
        .fold(0, |a, b| a + b)
}

fn predict<'a, 'b>(training_data: &'a Vec<Observation>, test_pixels: &'b [u8]) -> &'a Observation {
    training_data
        .iter()
        .map(|obs| (obs, distance(&obs.pixels, test_pixels)))
        .min_by_key(|pair| pair.1)
        .unwrap()
        .0
}

fn read_csv_file(filename: &str) -> io::Result<Vec<Observation>> {
    let f = try!(File::open(filename));
    let mut reader = BufReader::new(f);

    let mut line = String::new();
    try!(reader.read_line(&mut line));      // Discard the first line.

    let mut v = vec![];
    for line in reader.lines() {
        let line = try!(line);
        let mut iter = line.split(',');
        let label: u8 = iter.next().unwrap().parse().expect("error parsing input file");
        let parsing_result: Result<Vec<u8>, _> = iter.map(|s| s.parse()).collect();
        let pixels: Vec<u8> = parsing_result.expect("error parsing input file");
        v.push(Observation { label: label, pixels: pixels });
    }
    Ok(v)
}

fn fallible_main() -> io::Result<f64> {
    let training_data = try!(read_csv_file("../training-sample.csv"));
    let test_data = try!(read_csv_file("../test-sample.csv"));
    let mut passed = 0;
    let mut total = 0;
    for test_case in test_data {
        let best_match = predict(&training_data, &test_case.pixels);
        if best_match.label == test_case.label {
            passed += 1;
        }
        display_images(&test_case, best_match);
        total += 1;
    }
    Ok(100.0 * passed as f64 / total as f64)
}

fn main() {
    match fallible_main() {
        Ok(result) => println!("{}% correct", result),
        Err(err) => println!("{}", err)
    }
}

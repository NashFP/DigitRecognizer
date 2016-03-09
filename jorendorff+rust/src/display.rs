//! Routines for showing images as ASCII art.
//!
//! The input data is 28-pixel-by-28-pixel grayscale bitmaps.
//! Here is some code for printing them on stdout.

use super::Observation;

fn display_pixel(x: u8) -> char {
    match x {
        0 ... 10 => ' ',
        11 ... 64 => '.',
        65 ... 160 => ':',
        161 ... 255 => '#',
        _ => unreachable!("i have discovered an exciting new u8 value: {}", x)
    }
}

/// Print a pair of images as ASCII art, on stdout.
pub fn display_images(test: &Observation, best: &Observation) {
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

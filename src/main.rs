#![feature(plugin)]
#![plugin(regex_macros)]

extern crate regex;
extern crate xml;
extern crate core;

use std::os;

use regex::Regex;

use std::collections::HashMap;
use core::num::Int;

extern crate num;
use num::bigint::BigInt;
use num::rational::BigRational;
use num::traits::One;
use core::num::FromPrimitive;

use std::ops::Mul;

mod ucum_loader;
use ucum_loader::{load_defs, Prefix, Unit, Defs};

#[macro_use]
extern crate itertools;
use itertools::Itertools;

#[derive(Debug, Clone)]
struct Units(HashMap<String, i8>);

#[derive(Debug)]
struct UcumExpression {
    constant: BigRational,
    //sigfigs: u8,
    units: Units,
}

impl Units {

    fn mult(&self, &Units(ref bu): &Units) -> Units {
        let &Units(ref u) = self;
        let Units(mut ret) = Units::new(&[]);
        for (key, val) in u.iter() { ret.insert((*key).clone(), *val); }
        for (key, val) in bu.iter() {
            let nextval =
                match ret.get_mut(key) {
                    Some(x) => *x + *val,
                    None => *val,
                };
            ret.insert((*key).clone(), nextval);
        }
        return Units(ret);
    }

    fn invert(&self) -> Units {
        let Units(ref ui) = *self;

        let ret = Units::new(&[]);
        let Units(mut retu) = ret;

        for (&ref key, &val) in ui.iter() {
            retu.insert(key.clone(), -1 * val);
        }

        return Units(retu);
    }

}


impl UcumExpression {

    fn unity() -> UcumExpression {
        UcumExpression{constant: BigRational::one(), units: Units::new(&[]),}
    }

    fn inc_unit(&self, u: &str, i: i8) -> UcumExpression {

        let mut incu = HashMap::new();
        incu.insert(u.to_string(), i);
        let ret =
            UcumExpression{constant: self.constant.clone(),
                           units: self.units.mult(&Units(incu)),};

        return ret;
    }

    fn set_constant(&self, i: BigRational) -> UcumExpression {

        let ret = UcumExpression{constant: i, units: self.units.clone(),};
        return ret;
    }



    fn times(&self, other: UcumExpression) -> UcumExpression {
        UcumExpression{constant: self.constant.clone() * other.constant,
                       units: self.units.mult(&other.units)}
    }

    fn pow(&self, p: i8) -> UcumExpression {
        if (p < 0) { return self.invert().pow(-1 * p); }
        let Units(ref uextract) = self.units;
        let mut u = uextract.clone();
        for (_, v) in u.iter_mut() { *v *= p; }
        UcumExpression{constant: num::pow(self.constant.clone(), p as usize),
                       units: Units(u),}

    }

    fn divide(&self, other: UcumExpression) -> UcumExpression {
        return self.times(other.invert());
    }

    fn invert(&self) -> UcumExpression {
        UcumExpression{constant: self.constant.clone().recip(),
                           units: self.units.invert()}
    }

}

impl Units {
    fn unity() -> Units { return Units::new(&[]); }

    fn new(req: &[&str]) -> Units {
        let mut ret = HashMap::new();
        for pq in req.iter() {
            let v =
                match ret.get(&pq.to_string()) { Some(x) => *x, None => 0, };
            ret.insert(pq.to_string(), v + 1);
        }
        return Units(ret);
    }
}

use std::iter::Peekable;

fn main() {
    println!("Loading ucum defs...");
    let defs = load_defs("vendor/ucum-essence.xml");
    println!("Loaded!");

    let args: Vec<String> = os::args();
    for a in args[1..].iter() {
        println!("Parsed {} as {:?}", a, defs.parse_ucum(a));
    }
}

struct CautiousTakeWhile<'a, T: Iterator + 'a, P> where T::Item: 'a {
    inner: &'a mut Peekable<T>,
    condition: P,
}

impl <'a, T: Iterator, P> Iterator for CautiousTakeWhile<'a, T, P> where
 P: FnMut(&T::Item) -> bool {type
    Item
    =

    T::Item;

    fn next(&mut self) -> Option<T::Item> {
        let return_next =
            match self.inner.peek() {
                Some(ref v) => (self.condition)(v),
                _ => false,
            };
        if return_next { self.inner.next() } else { None }
    }
}

trait CautiousTakeWhileable<'a, T>: Iterator {
    fn cautious_take_while<P>(&'a mut self, P) -> CautiousTakeWhile<'a, T, P>
        where P: FnMut(&Self::Item) -> bool;
}

impl <'a, T: Iterator> CautiousTakeWhileable<'a, T> for Peekable<T> {
    fn cautious_take_while<P>(&'a mut self, f: P)
     -> CautiousTakeWhile<'a, T, P> where P: FnMut(&'a (T::Item)) -> bool {
        CautiousTakeWhile{inner: self, condition: f,}
    }
}


use core::str::Chars;

impl Defs {

    fn parse_ucum(&self, unit: &str) -> Option<UcumExpression> {
        let mut chars =unit.chars().peekable();
        let parsed = self.parse_term_or_annotation_or_unit(&mut chars);
        if chars.peek().is_some() {
            println!("Failed to consume all chars of input {:?}" ,
                     chars. collect::<String>());
            None
        } else {
            Some(parsed)
        }
    }

    fn parse_term_or_annotation_or_unit(&self,
                                        mut chars: &mut Peekable<Chars>)
     -> UcumExpression {
        let debug: String = chars.clone().collect();
        //println!("parse_term_or_annotation_or_unit {:?}" , debug);
        let mut parsed = UcumExpression::unity();
        loop  {
            match chars.peek() {
                Some(&'(') => { parsed = self.parse_term(chars); }
                Some(&'{') => { self.parse_annotation(chars); }
                Some(&'/') => {
                    chars.next();
                    let next = self.parse_term_or_annotation_or_unit(chars);
                    //println!("Divide by next of \n  {:?}\n / \n   {:?}\n---", parsed, next);
                    parsed = parsed.divide(next);
                }
                Some(&'.') => {
                    chars.next();
                    let next = self.parse_term_or_annotation_or_unit(chars);
                    //println!("Multiply by next of \n  {:?}\n * \n   {:?}\n---", parsed, next);
                    parsed = parsed.times(next);
                }
                Some(&')') => { break ; }
                Some(&whatever) => {
                    let mut next = self.parse_unit(chars);
                    parsed = parsed.times(next);
                }
                None => { break ; }
            }

        }
        //println!("Yields: {:?}", parsed);
        parsed
    }

    fn parse_term(&self, mut chars: &mut Peekable<Chars>) -> UcumExpression {
        //println!("consuming open-paren {:?}" , chars . peek (  ));
        chars.next(); // (
        let ret = self.parse_term_or_annotation_or_unit(chars);
        //println!("consuming close-paren {:?}" , chars . peek (  ));
        chars.next(); // )
        ret
    }

    fn parse_annotation(&self, mut chars: &mut Peekable<Chars>)
     -> UcumExpression {
        chars.take_while(|&x| x != '}').min();
        UcumExpression::unity()
    }

    fn unit_for_name(&self, name: String) -> Option<UcumExpression> {

        let mut unit_name = "";
        let mut unit_sign = "+";
        let mut unit_power = 1;

        let mut ret_unit = None;
        let mut ret_prefix = None;
        let mut ret_power = 1;

        let mut nums_only = false;
        let mut ret = UcumExpression::unity();

        let nums_only_re =
            regex!(r"^(?P<prefix>\d+)(?:(?P<sign>\+|-)(?P<exp>\d+))?$");
        match nums_only_re.captures_iter(name.as_slice()).next() {
            Some(cap) => {
                //println!("Nums only cap {:?}" , cap . name ( "prefix" ));
                nums_only = true;
                unit_sign = cap.name("sign").unwrap_or("+");
                unit_power =
                    cap.name("exp").map_or(1,
                                           |x| x.parse::<i8>().ok().unwrap());
                ret_prefix =
                    Some(Prefix{code: "constant".to_string(),
                                value:
                                    cap.name("prefix").unwrap().parse().unwrap(),});
            }
            _ => { }
        }

        if !nums_only {
            let re = regex!(r"^([^0-9)}\+-]+)(\+|-)?(\d*)$");

            match re.captures_iter(name.as_slice()).next() {
                None => { return None; }
                Some(cap) => {
                    unit_name = cap.at(1).unwrap();
                    unit_sign = cap.at(2).unwrap_or("+");
                    unit_power =
                        cap.at(3).map_or(1, |x| x.parse::<i8>().ok().unwrap_or(1));
                }
            }

            //println!("So unit parts: {:?},{:?},{:?}" , unit_name , unit_sign ,
            //         unit_power);
            let mut exact: Vec<&Unit> =
                self.units.iter().filter(|&u|
                                             unit_name ==
                                                 (u.code.as_slice())).collect();

            if exact.len() == 1 {
                //println!("Yes exact match {:?}" , exact);
                ret_unit = Some((*exact[0]).clone());
            } else {
                //println!("No exact match {:?}" , exact);
                let metric: Vec<(&Prefix, &Unit)> =
                    iproduct!(self.prefixes.iter(), self.metric_units.iter())
                             .filter(|&(p, u)|
                                    unit_name == ((p.code.clone() + u.code.as_slice()).as_slice()))
                             .collect();

                if metric.len() == 1 {
                    let (prefix, unit) = metric[0];
                    ret_unit = Some(unit.clone());
                    ret_prefix = Some(prefix.clone());
                }
            }
        }

        ret_power = unit_power * match unit_sign { "-" => -1, _ => 1, };
        if ret_unit.is_some() {
            ret = ret.inc_unit(ret_unit.unwrap().code.as_slice(), ret_power);
        }
        if let Some(p) = ret_prefix {
            if ret_power < 0 {
                let recip = p.value.recip();
                let raised = num::pow(recip, (-1 * ret_power) as usize);
                let back = raised.recip();
                ret = ret.set_constant(back);
            } else { ret = ret.set_constant(p.value); }
        }
        //println!("ret {:?}" , ret);
        Some(ret)
    }

    fn parse_unit(&self, mut chars: &mut Peekable<Chars>) -> UcumExpression {

        let debug: String = chars.clone().collect();
       // println!("parse_unit {:?}" , debug);
        let mut val: String = "".to_string();
        let ends: Vec<char> = "./({})".chars().collect();
        loop  {
            //println!("parse_unit peeked {:?}" , chars . peek (  ));
            match chars.peek() {
                None => { break  }
                Some(&c) if !ends.contains(&c) => {
                    val.push(c);
                    chars.next();
                }
                _ => { break  }
            }
        }

        //println!("so val in parse_unit {:?}" , val);
        let ret = self.unit_for_name(val.to_string());
        ret.unwrap()
    }

}

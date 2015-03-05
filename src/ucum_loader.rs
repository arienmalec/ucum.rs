use num::bigint::BigInt;
use num::rational::BigRational;
use num::traits::One;
use core::num::FromPrimitive;

use xml::reader::EventReader;
use xml::reader::events::*;
use xml::name::OwnedName;
use xml::attribute::*;

use std::old_io::{File, BufferedReader};

#[derive(Debug, Clone)]
pub struct Prefix {
    //name: String,
    pub code: String,
    pub value: BigRational,
}

#[derive(Debug, Clone)]
pub struct Unit {
    pub is_base: bool,
    pub is_metric: bool,
    pub code: String,
    pub value_quantity: f64,
    pub value_units: String,
}

fn get_attr(attrs: &Vec<OwnedAttribute>, aname: &str) -> Option<String> {
    attrs.iter().filter(|a|
                            a.name.local_name ==
                                aname).peekable().peek().map(|x|
                                                                 x.value.clone())
}

fn as_bigrational(s: String) -> Option<BigRational> {
    let re = regex!(r"^(?P<base>\d+)(?:e(?P<sign>\+|-)?(?P<exp>\d+))?$");

    match re.captures_iter(s.as_slice()).next() {
        None => { return None; }
        Some(cap) => {
            let mut num: BigInt =
                cap.name("base").unwrap().parse::<BigInt>().unwrap();
            let mut den: BigInt = BigInt::one();
            let ten = &BigInt::from_int(10).unwrap();

            let exp: u32 = cap.name("exp").unwrap_or("0").parse().unwrap();
            if cap.name("sign").unwrap_or("+") == "+" {
                for i in 0..exp { num = num * ten; }
            } else { for i in 0..exp { den = den * ten; } }
            let ret = BigRational::new(num, den);
            return Some(ret);
        }
    }
    return None
}

fn load_prefix(name: String, attrs: Vec<OwnedAttribute>,
               ref mut parser: &mut EventReader<BufferedReader<File>>)
 -> Option<Prefix> {
    let mut depth = 0;
    let prefix_name = get_attr(&attrs, "Code");
    loop  {
        match parser.next() {
            XmlEvent::StartElement { name, attributes: attrs, namespace: _ }
            => {
                depth += 1;
                if (name.local_name == "value" && prefix_name.is_some()) {
                    let val: String = get_attr(&attrs, "value").unwrap();
                    //println!("PArsing prefix {:?}" , prefix_name);
                    let p =
                        Prefix{code: prefix_name.clone().unwrap(),
                               value: as_bigrational(val.clone()).unwrap(),};
                    return Some(p);
                }
            }
            XmlEvent::EndElement { name } => {
                depth -= 1;
                if depth < 0 { return None; }
            }
            _ => (),
        }
    }
}

fn load_unit(name: String, attrs: Vec<OwnedAttribute>,
             ref mut parser: &mut EventReader<BufferedReader<File>>)
 -> Option<Unit> {
    let mut depth = 0;
    let unit_name = get_attr(&attrs, "Code");
    let mut is_metric =
        get_attr(&attrs, "isMetric").map_or(false, |x| x == "yes");
    let is_base = name == "base-unit";
    if is_base { is_metric = true; }
    let mut val: Option<String> = None;
    let mut value_units: Option<String> = None;
    loop  {
        match parser.next() {
            XmlEvent::StartElement { name, attributes: attrs, namespace: _ }
            => {
                depth += 1;
                if (name.local_name == "value") {
                    val = get_attr(&attrs, "value");
                    value_units = get_attr(&attrs, "Unit");
                }
            }
            XmlEvent::EndElement { name } => {
                depth -= 1;
                if depth < 0 {
                    return Some(Unit{is_base: is_base,
                                     is_metric: is_metric,
                                     code: unit_name.clone().unwrap(),
                                     value_units:
                                         value_units.map_or(unit_name.clone().unwrap(),
                                                            |x| x),
                                     value_quantity:
                                         val.map_or(1.0,
                                                    |x|
                                                        x.parse().unwrap()),});

                    return None;
                }
            }
            _ => (),
        }
    }
}

use std::old_io::Buffer;

struct LoadType<T, B> {
    load: fn(String, Vec<OwnedAttribute>, &mut B) -> Option<T>,
    store: Vec<T>,
}


impl <T, B> Loader<B> for LoadType<T, B> {
    fn loadit(&mut self, n: String, a: Vec<OwnedAttribute>, b: &mut B) {
        let v = (self.load)(n, a, b);
        if v.is_some() { (self.store).push(v.unwrap()); }
    }
}

impl <T, B> LoadType<T, B> {
    fn new(f: fn(String, Vec<OwnedAttribute>, &mut B) -> Option<T>)
     -> LoadType<T, B> {
        LoadType{load: f, store: Vec::new(),}
    }
}

trait Loader<B> {
    fn loadit(&mut self, n: String, a: Vec<OwnedAttribute>, b: &mut B);
}

pub struct Defs {
    pub units: Vec<Unit>,
    pub prefixes: Vec<Prefix>,
    pub metric_units: Vec<Unit>,
}

pub fn load_defs(filename: &str) -> Defs {

    let file = File::open(&Path::new(filename)).unwrap();
    let reader = BufferedReader::new(file);
    let mut parser = EventReader::new(reader);

    let mut UnitLoader = LoadType::new(load_unit);
    let mut PrefixLoader = LoadType::new(load_prefix);
    let mut depth = 0;
    loop  {
        let e = parser.next();
        match e {
            XmlEvent::StartElement { name, attributes: attrs, namespace: _ }
            => {
                let l: Option<&mut Loader<_>> =
                    match name.local_name.as_slice() {
                        "prefix" => Some(&mut PrefixLoader),
                        "unit" | "base-unit" => Some(&mut UnitLoader),
                        _ => None,
                    };
                if l.is_some() {
                    l.unwrap().loadit(name.local_name, attrs, &mut parser);
                }
            }
            XmlEvent::EndDocument => { break ; }
            XmlEvent::Characters(ref data) => { }


            XmlEvent::Error(e) => {
                println!("Error: {}" , e);
                break ;
            }
            _ => { }
        }
    }

    //println!("Store: {:?}", UnitLoader.store); 
    Defs{metric_units:
             UnitLoader.store.clone().iter().filter(|&u|
                                                        u.is_metric).map(|x|
                                                                             x.clone()).collect(),
         units: UnitLoader.store,
         prefixes: PrefixLoader.store,}
}



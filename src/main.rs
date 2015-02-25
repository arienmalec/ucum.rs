use std::collections::HashMap;
extern crate core;
use core::num::Int;

#[derive(Copy,Debug)]
struct Rational {
  numerator: i64,
  denominator: i64
}

#[derive(Debug)]
struct Units(HashMap<PhysicalQuantity, i8>);

#[derive(Debug)]
struct UcumExpression {
 constant: Rational,
 units: Units
}

#[derive(Debug,PartialOrd,PartialEq,Eq, Ord,Hash,Copy, Clone)]
enum PhysicalQuantity {
 Meter,
 Second,
 Gram,
 Radian,
 Kelvin,
 Coulomb,
 Candela
}

fn mult (a: Rational, b: Rational) -> Rational{
  return Rational{
    numerator: a.numerator * b.numerator,
    denominator: a.denominator * b.denominator
  };
}

fn mult_units(a: &Units, b: &Units) -> Units{
  let Units(ref au) = *a;
  let Units(ref bu) = *b;
  let Units(mut ret) = make_units(&[]);
  au.clone();
  for (key, val) in au.iter() {
    ret.insert(*key, *val);
  }
  for (key, val) in bu.iter() {
    let nextval = match ret.get_mut(key) {
      Some(x) => *x + *val,
      None => *val
    };
    ret.insert(*key, nextval);
  }
  return Units(ret);
}

  fn invert_units(u : &Units) -> Units{
    let Units(ref ui) = *u;

    let ret = make_units(&[]);
    let Units(mut retu) = ret;

    for (&key,&val) in ui.iter() {
      retu.insert(key, -1*val); 
    }

    return Units(retu);
  }


impl UcumExpression {

  fn times(&self, other: UcumExpression) -> UcumExpression {
    
    let ret = UcumExpression {
        constant: mult(self.constant, other.constant),
	units: mult_units(&self.units, &other.units)
    };

    return ret;
  }

  fn pow(&self, p: i8) -> UcumExpression {
    if (p < 0) {
      return self.invert().pow(-1*p);
    }
    
    let Units(ref uextract) = self.units;
    let mut u = uextract.clone();
    for (_, v) in u.iter_mut() {
      *v  *= p;
    }
    

    let ret = UcumExpression {
        constant: Rational{
          numerator: self.constant.numerator.pow(p as usize),
          denominator: self.constant.denominator.pow(p as usize)
        },
	units: Units(u)
    };

    return ret;
  }


  fn divide(&self, other: UcumExpression)->UcumExpression {
    return self.times(other.invert());
  }

 
  fn invert(&self)->UcumExpression {

    let ret = UcumExpression {
        constant: Rational{
          numerator: self.constant.denominator,
          denominator: self.constant.numerator
        },
        // TODO: flip
        units: invert_units(&self.units)
    };

    return ret;
  }

}

fn make_units(req: &[PhysicalQuantity]) -> Units {
  let mut ret = HashMap::new();
  for &pq in req.iter() {
    let v = match ret.get(&pq){
      Some(x) => *x,
      None => 0
    };
    ret.insert(pq, v+1);
  }
  return Units(ret);
}

fn main() {
    println!("Hello, world!");

    let cm = UcumExpression {
        constant: Rational{numerator:1,denominator:100},
	units: make_units(&[PhysicalQuantity::Meter])
    };

    let m = UcumExpression {
        constant: Rational{numerator:1,denominator:1},
	units: make_units(&[PhysicalQuantity::Meter])
    };

    let res = cm.divide(m);
    println!("And got {:?}",res);

    let res = cm.pow(2);
    println!("And got {:?}",res);

}

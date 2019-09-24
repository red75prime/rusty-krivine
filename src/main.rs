extern crate rusty;

use std::thread;

use rusty::*;
use Term::*;

fn church(k: u8) -> CTerm {
  let b = (0..k).fold(var(0), |a, _| app(var(1), a));
  lam(lam(b))
}

fn nmpair(n: u8, m: u8) -> CTerm {
  // t = \x.\y.((a)(x)y)(b)(y)x
  let b = app( app( free("a"), app(var(1), var(0))), app(free("b"), app(var(0), var(1))) );
  let t = lam(lam(b));
  app(app(t, church(n)), church(m))
}

fn klmn(k: u8, l: u8, m: u8, n: u8) -> CTerm {
  // t = \x1.\x2.\x3.\x4.((((a)(x1)x2) (b)(x2)x1) (c)(x3)x4) (d)(x4)x3
  let v = vec![app(var(3), var(2)),
               app(free("b"), app(var(2), var(3))),
               app(free("c"), app(var(1), var(0))),
               app(free("d"), app(var(0), var(1)))];
  let b = v.into_iter().fold(free("a"), |a, c| app(a, c));
  let t = lam(lam(lam(lam( b ))));
  app(app(app(app(t, church(k)), church(l)), church(m)), church(n))
}

fn main() {
//  let child = thread::Builder::new().stack_size(1024 * 1024 * 1024).spawn(|| {
    // let id = Lam(Box::new(Var(0)));
    // println!("{}", to_string(&id));
    // let c2 = church(2);
    // println!("{}", to_string(&c2));
    // let c3 = church(3);
    // let c8 = eval(&app(c3, c2));
    // println!("{}", to_string(&c8));

    // let _nm = run(move || {
    //       return eval(&nmpair(7, 6));
    //   });
    // println!("{}", to_string(&nm)); // no need to print it to evaluate it, it's an eager language
    let _klmn = eval(klmn(8, 7, 9, 6));
    //println!("{}", to_string(&_klmn));
    println!("Done!");

  // }).unwrap();
  // if let Err(e) = child.join() {
  //   if let Some(s) = e.downcast_ref::<String>() {
  //     println!("Thread panicked with '{}'", s);
  //   }
  // }
}

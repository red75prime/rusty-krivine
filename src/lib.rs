// The engine

use std::mem;
use std::rc::Rc;
use std::sync::{Arc, Mutex};
use typed_arena::Arena;
use lazy_static::lazy_static;
use rayon::prelude::*;

#[derive(Clone)]
pub enum Term {
  Var(u32),
  Lam(CTerm),
  App(CTerm, CTerm),
  Free(&'static str), // FIXME: `&str`?
}

// thread_local!{
//   static ALLOC: Arena<Term> = Arena::new();
// }

// fn static_alloc(v: Term) -> &'static Term {
//   ALLOC.with(move |a| -> &'static Term {
//     let r: &Term = a.alloc(v);
//     unsafe {
//       mem::transmute::<&Term, &'static Term>(r)
//     }
//   })
// }

struct DumbAlloc {
  blocks: Mutex<Vec<Vec<Term>>>,
}

lazy_static!{
  static ref ALLOC: DumbAlloc = DumbAlloc { blocks: Mutex::new(vec![Vec::with_capacity(1024)]) };
}

fn static_alloc(t: Term) -> &'static Term {
  let mut blocks = ALLOC.blocks.lock().unwrap();
  let mut last = blocks.last_mut().unwrap();
  if last.len() != last.capacity() {
  } else {
    blocks.push(Vec::with_capacity(1024));
    last = blocks.last_mut().unwrap();
  };
  last.push(t);
  unsafe { mem::transmute(last.last().unwrap()) }
}

pub struct CTerm {
  //term: Arc<Term>,
  term: &'static Term,
}

impl Clone for CTerm {
  fn clone(&self) -> CTerm {
    CTerm {
      // term: Arc::clone(&self.term),
      term: self.term,
    }
  }
}

impl CTerm {
  fn into_inner(mut self) -> Term {
    self.term.clone()
    // match Arc::try_unwrap(mem::replace(&mut self.term, Arc::new(Term::Free("")))) {
    //   Ok(t) => { t }
    //   Err(rc) => { (*rc).clone() }
    // }
  }

  fn as_ref(&self) -> &Term {
    self.term
  }

  pub fn from(t: Term) -> CTerm {
    CTerm {
//      term: Arc::new(t),
      term: static_alloc(t),
    }
  }

  fn destructure_onto<A: Extend<CTerm>>(&mut self, a: &mut A) {
    // match Arc::try_unwrap(mem::replace(&mut self.term, Arc::new(Term::Free("")))) {
    //   Ok(Lam(t)) => { a.extend(Some(t)); }
    //   Ok(App(u, v)) => { a.extend(Some(u)); a.extend(Some(v)); }
    //   _ => {}
    // }
  }
}

impl Drop for CTerm {
  fn drop(&mut self) {
    // use std::mem;
    // mem::forget(self.term.take());

    // use smallvec::SmallVec;

    // let mut drop_vec = SmallVec::<[CTerm; 16]>::new();
    // self.destructure_onto(&mut drop_vec);
    // while let Some(mut v) = drop_vec.pop() {
    //   v.destructure_onto(&mut drop_vec);
    // }
  }
}

pub fn var(n: u32) -> CTerm {
  CTerm::from(Var(n))
}

pub fn lam(t: CTerm) -> CTerm {
  CTerm::from(Lam(t))
}

pub fn app(u: CTerm, v: CTerm) -> CTerm {
  CTerm::from(App(u, v))
}

pub fn free(s: &'static str) -> CTerm {
  CTerm::from(Free(s))
}


use Term::*;

// Smart constructors

fn to_string_rec(acc: &mut String, t: &CTerm) {
  use std::fmt::Write;

  match t.as_ref() {
    Var(i) => { let _ = write!(acc, "{}", i); }
    Lam(t1) => { acc.push_str("λ.");  to_string_rec(acc, t1); }
    App(u, v) => { acc.push_str("("); to_string_rec(acc, u); acc.push_str(") "); to_string_rec(acc, v); }
    Free(name) => { acc.push_str(name) }
  }
}

pub fn to_string(t: &CTerm) -> String {
  let mut acc = String::with_capacity(1024);
  to_string_rec(&mut acc, t);
  acc
}

enum Env {
  Nil,
  Env(Closure, Arc<Env>),
  Lift(Arc<Env>)
}

use Env::*;

fn level(e: &Arc<Env>) -> u32 {
  match **e {
    Nil => 0,
    Env(_, ref p) => level(p),
    Lift(ref p) => 1 + level(p)
  }
}


enum Closure {
  NilClosure(Arc<Env>),
  Closure(CTerm, Arc<Env>)
}

use Closure::*;

impl Clone for Closure {
  fn clone(&self) -> Closure {
    match self {
      NilClosure(ref e) => NilClosure(Arc::clone(e)),// FIXME: for this machine we can just transfer ownership
      Closure(term, env) => Closure(
        term.clone(),
        Arc::clone(env) // FIXME: for this machine we can just transfer ownership
      )
    }
  }
}

type Stack = Vec<Closure>;


struct Parameters {
    t: CTerm,
    e: Arc<Env>,
    s: Stack,
}
enum State {
  Initial(Parameters),
  WhileLoop {
    a: CTerm,
    s: Stack,
  },
  WhileLoopRet {
    a: CTerm,
    s: Stack,
    ret: CTerm,
  },
  LamEval,
  LamEvalRet{ s: Stack, ret: CTerm },
}

impl std::fmt::Display for State {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
    use State::*;
    match self {
      Initial(_) => write!(f, "Initial"),
      WhileLoop {..} => write!(f, "WhileLoop"),
      WhileLoopRet {..} => write!(f, "WhileLoopRet"),
      LamEval => write!(f, "LamEval"),
      LamEvalRet {..} => write!(f, "LamEvalRet"),
    }
  }
}

enum Op {
  Return(CTerm),
  ReturnLam(CTerm, Stack),
  RecCall(State, Parameters),
  NewState(State),
}

fn eval_flat(t: CTerm, e: Arc<Env>, s: Stack) -> CTerm {
  let mut exec_stack = vec![];
  let mut state = State::Initial(Parameters{t, e, s});
  loop {
    match eval_cont(state) {
      Op::NewState(s) => {
        state = s;
      }
      Op::RecCall(s, params) => {
        exec_stack.push(s);
        state = State::Initial(params);
      }
      Op::Return(ret) => {
        match exec_stack.pop() {
          Some(State::WhileLoop {a, s}) => {
            state = State::WhileLoopRet { a, s, ret };
          }
          Some(State::LamEval) => {
            state = State::LamEvalRet { s: vec![], ret };
          }
          None => {
            return ret;
          }
          Some(s) => {
            panic!("Invalid state for op Return: {}", s);
          }
        }
      }
      Op::ReturnLam(ret, s) => {
        match exec_stack.pop() {
          Some(State::LamEval) => {
            state = State::LamEvalRet { s, ret };
          }
          None => {
            return ret;
          }
          Some(s) => {
            panic!("Invalid state for op ReturnLam: {}", s);
          }
        }
      }
    }
  }
}

fn eval_cont(state: State) -> Op {
  use State::*;

  match state {
    Initial(Parameters { mut t, mut e, mut s }) => {
      let mut a;
      loop {
        match t.into_inner() {
          Var(i) => {
            match fetch(i, &e) {
              Some(closure) => match closure {
                Closure(term, env) => {
                  // eval_aux(term, env, s)
                  t = term;
                  e = env;
                  continue;
                },
                NilClosure(env) => {
                  // here we unroll `eval_aux(Var(0), Nil, s)`
                  if s.len() == 0 {
                    return Op::Return(var(level(&env)));
                  } else {
                    a = var(level(&env));
                    break;
                  }
                }
              },
              None => {
                if s.len() == 0 {
                  return Op::Return(var(i + level(&e)));
                } else {
                  a = var(i + level(&e));
                  break;
                }
              }
            }
          },
          Lam(t1) => {
            match s.pop() {
              Some(c) => {
                // eval_aux(t1, Rc::new(Env(c, e)), s)
                t = t1;
                e = Arc::new(Env(c, e));
                continue;
              },
              None => {
                return Op::RecCall(LamEval, Parameters{ t: t1, e: Arc::new(Env(NilClosure(Arc::new(Nil)), Arc::new(Lift(e)))), s });
              }
            }
          },
          App(u, v) => {
            s.push(Closure(v, Arc::clone(&e)));
            // eval_aux(&u, e, s)
            t = u;
            continue;
          },
          Free(name) => {
            if s.len() == 0 {
              return Op::Return(CTerm::from(Free(name))); // FIXME: can we reuse `t` here?
            } else {
              a = CTerm::from(Free(name));
              break;
            }
          }
        } // match t.into_inner()
      } // loop
      return Op::NewState(State::WhileLoop{ s, a });
    } // Initial state
    LamEval => {
      panic!("Wrong state");
    }
    LamEvalRet { s, ret } => {
      if !s.is_empty() {
        panic!("Nonempty stack on LamEvalRet");
      }
      return Op::Return(lam(ret));
    }
    WhileLoop { s, a } => {
      let mut s = s;
      if s.len() > 2 {
        use rayon::iter::IntoParallelIterator;
        let par_iter = s.into_par_iter();
        let terms: Vec<_> = par_iter.map(|c| {
          if let Closure(term, env) = c {
            eval_flat(term, env, vec![])
          } else {
            panic!("NilClosure!")
          }
        })
        .collect();
        let ret = terms.into_iter().rev().fold(a, |a, term| {
          app(a, term)
        });
        return Op::Return(ret);
      } else {
        if let Some(c) = s.pop() {
          if let Closure(term, env) = c {
            return Op::RecCall(WhileLoop { s, a }, Parameters { t: term, e: env, s: vec![] });
          } else {
            panic!("NilClosure!")
          }
        } else {
          return Op::Return(a);
        }
      }
    }
    WhileLoopRet{ a, s, ret } => {
      return Op::NewState(WhileLoop{ s, a: app(a, ret) });
    }
  }
}


fn eval_aux(t_in: CTerm, e_in: Arc<Env>, s: &mut Stack) -> CTerm {
  let mut t = t_in;
  let mut e = e_in;
  let mut a;
  loop {
    match t.into_inner() {
      Var(i) => {
        match fetch(i, &e) {
          Some(closure) => match closure {
            Closure(term, env) => {
              // eval_aux(term, env, s)
              t = term;
              e = env;
              continue;
            },
            NilClosure(env) => {
              // here we unroll `eval_aux(Var(0), Nil, s)`
              if s.len() == 0 {
                return var(level(&env))
              } else {
                a = var(level(&env));
                break;
              }
            }
          },
          None => {
            if s.len() == 0 {
              return var(i + level(&e))
            } else {
              a = var(i + level(&e));
              break;
            }
          }
        }
      },
      Lam(t1) => {
        match s.pop() {
          Some(c) => {
            // eval_aux(t1, Rc::new(Env(c, e)), s)
            t = t1;
            e = Arc::new(Env(c, e));
            continue;
          },
          None => return lam(eval_aux(t1, Arc::new(Env(NilClosure(Arc::new(Nil)), Arc::new(Lift(e)))), s))
        }
      },
      App(u, v) => {
        s.push(Closure(v, Arc::clone(&e)));
        // eval_aux(&u, e, s)
        t = u;
        continue;
      },
      Free(name) => {
        if s.len() == 0 {
          return CTerm::from(Free(name)) // FIXME: can we reuse `t` here?
        } else {
          a = CTerm::from(Free(name));
          break;
        }
      }
    } // match t.into_inner()
  } // loop
  while let Some(c) = s.pop() {
    if let Closure(term, env) = c {
      a = app(a, eval_aux(term, env, &mut Vec::new()));
    } else {
      panic!("NilClosure!")
    }
  }
  a
}

fn fetch<'b>(i: u32, e: &'b Arc<Env>) -> Option<Closure> {
  match **e {
    Nil => None,
    Env(ref c, ref p) => {
      if i == 0 {
        Some(c.clone())
      } else {
        fetch(i-1, p)
      }
    },
    Lift(ref p) => {
      match fetch(i, p) {
        None => None,
        Some(c) => match c {
          Closure(term, env) => Some(Closure(term, Arc::new(Lift(env)))),
          NilClosure(env) => Some(NilClosure(Arc::new(Lift(env))))
        }
      }
    }
  }
}

// pub fn eval(t: CTerm) -> CTerm {
//   let mut s = Vec::new();
//   eval_aux(t, Rc::new(Nil), &mut s)
// }

pub fn eval(t: CTerm) -> CTerm {
  eval_flat(t, Arc::new(Nil), vec![])
}


#[cfg(test)]
mod tests {
  #[test]
  fn it_works() {
    assert_eq!(2 + 2, 4);
  }
}

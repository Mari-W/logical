use logical::Formula;

fn main() {
    sheet06_task1()
}

fn sheet06_task1() {
    let f = Formula::parse("(((((~A0 & ~A1) & A2) | ((~A0 & A1) & A2)) | ((A0 & ~A1) & A2)) | ((A0 & A1) & A2))").unwrap();
    let g = Formula::parse("~(((~A0 & ~A1) & A2) | ((A0 & ~A1) & A2))").unwrap();
    println!("F = {}", f);
    println!("G = {}", g);
    println!("1) {}", Formula::and(Formula::negate(f.clone()), Formula::negate(g.clone())).is_satisfiable());
    println!("2) {}", Formula::and(f.clone(), g.clone()).is_satisfiable());
    println!("3) {}", Formula::or(f.clone(), g.clone()).is_tautology());
    println!("4) {}", !Formula::and(f.clone(), g.clone()).is_satisfiable());
    println!("5) {}", Formula::implies(Formula::negate(f.clone()), g.clone()).is_tautology());
    println!("6) {}", !Formula::implies(f.clone(), g.clone()).is_tautology());
    println!("7) {}", Formula::equivalence(f.clone(), g.clone()).is_satisfiable());
    println!("8) {}", !Formula::implies(f.clone(), Formula::variable("A2")).is_tautology());
}


#[derive(Debug,PartialEq,Clone,Copy)]
pub enum Dimension {
    Int,
}

#[derive(Debug,PartialEq,Copy,Clone)]
pub enum Value {
    Int { value: i64, grain: u8, group: bool }, /*    Number(NumberKind), Float(f64), Temperature, Time, DurationUnit, Duration, Price, Cycle, Unit
                                                 * */
    Unknown,
}

impl Value {
    pub fn dim(&self) -> Dimension {
        match self {
            &Value::Int { .. } => Dimension::Int,
            &Value::Unknown => panic!("Unknown"),
        }
    }
}

//pub mod en;

use std::{collections::HashMap, ffi::{c_void, CString}, rc::Rc};

use libffi::middle::{arg, Arg, CodePtr, Cif, Type};
use libloading::Library;

use crate::ospl::Value;

#[derive(Debug, Clone)]
pub struct ForeignFunction {
    pub library: Rc<Library>,
    pub symbol: String,
    pub symbol_ptr: CodePtr,
    pub arg_types: Vec<String>,
    pub return_type: String,
    pub cif: Cif,
}

#[derive(Debug, Clone)]
pub struct FfiRegistry {
    libraries: HashMap<String, Rc<Library>>,
    functions: HashMap<String, Rc<ForeignFunction>>,
}

impl Default for FfiRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl FfiRegistry {
    pub fn new() -> Self {
        Self {
            libraries: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    pub fn has_library(&self, name: &str) -> bool {
        self.libraries.contains_key(name)
    }

    pub fn clone(&self) -> FfiRegistry {
        Self {
            libraries: self.libraries.clone(),
            functions: self.functions.clone(),
        }
    }

    pub fn load_library(&mut self, name: String, path: &str) -> Result<(), String> {
        if self.libraries.contains_key(&name) {
            return Ok(());
        }

        let library = Rc::new(unsafe { Library::new(path) }
            .map_err(|e| format!("Failed to load library '{}': {}", path, e))?);

        self.libraries.insert(name, library);
        Ok(())
    }

    pub fn register_function(
        &mut self,
        lib_name: String,
        func_name: String,
        symbol_name: String,
        arg_types: Vec<String>,
        return_type: String,
    ) -> Result<Rc<ForeignFunction>, String> {
        let key = format!("{}::{}", lib_name, func_name);
        if let Some(existing) = self.functions.get(&key) {
            return Ok(existing.clone());
        }

        let library = self.libraries.get(&lib_name)
            .ok_or_else(|| format!("Library '{}' not loaded", lib_name))?
            .clone();

        let symbol_ptr = unsafe {
            *library.get::<*const c_void>(symbol_name.as_bytes())
                .map_err(|e| format!("Failed to load symbol '{}': {}", symbol_name, e))?
        };

        let cif = build_cif(&arg_types, &return_type)
            .map_err(|e| format!("Failed to build CIF: {}", e))?;

        let function = Rc::new(ForeignFunction {
            library,
            symbol: symbol_name,
            symbol_ptr: CodePtr::from_ptr(symbol_ptr),
            arg_types,
            return_type,
            cif,
        });

        self.functions.insert(key, function.clone());
        Ok(function)
    }

    pub fn get_function(&self, lib_name: &str, func_name: &str) -> Option<Rc<ForeignFunction>> {
        let key = format!("{}::{}", lib_name, func_name);
        self.functions.get(&key).cloned()
    }
}

fn build_cif(arg_types: &[String], return_type: &str) -> Result<Cif, &'static str> {
    let mut args = Vec::new();
    for t in arg_types {
        args.push(map_type(t)?);
    }

    let ret = map_type(return_type)?;
    Ok(Cif::new(args.into_iter(), ret))
}

fn map_type(name: &str) -> Result<Type, &'static str> {
    match name {
        "void" => Ok(Type::void()),
        "bool" => Ok(Type::u8()),
        "u8" => Ok(Type::u8()),
        "i8" => Ok(Type::i8()),
        "u16" => Ok(Type::u16()),
        "i16" => Ok(Type::i16()),
        "u32" => Ok(Type::u32()),
        "i32" => Ok(Type::i32()),
        "u64" => Ok(Type::u64()),
        "i64" => Ok(Type::i64()),
        "f32" => Ok(Type::f32()),
        "f64" => Ok(Type::f64()),
        "usize" => Ok(Type::usize()),
        "ptr" | "pointer" | "cstr" => Ok(Type::pointer()),
        _ => Err("unsupported type"),
    }
}

pub fn call_foreign_function(
    registry: &mut FfiRegistry,
    library: &str,
    symbol: &str,
    args: &[Value],
) -> Result<Value, String> {
    let function = registry
        .get_function(library, symbol)
        .ok_or_else(|| format!("Foreign function '{}::{}' not registered", library, symbol))?;

    if args.len() != function.arg_types.len() {
        return Err(format!(
            "Argument count mismatch: expected {}, got {}",
            function.arg_types.len(),
            args.len()
        ));
    }

    let mut arg_values: Vec<Box<dyn RawValueHolder>> = Vec::new();
    let mut raw_args: Vec<Arg> = Vec::new();

    for (value, ty) in args.iter().zip(function.arg_types.iter()) {
        let holder = box_value(value.clone(), ty)?;
        raw_args.push(arg(holder.raw_ptr()));
        arg_values.push(holder);
    }

    unsafe {
        match function.return_type.as_str() {
            "void" => {
                function.cif.call::<()>(function.symbol_ptr, &raw_args);
                Ok(Value::Void)
            }
            "bool" | "u8" => {
                let result = function.cif.call::<u8>(function.symbol_ptr, &raw_args);
                Ok(Value::Byte(result))
            }
            "i8" => {
                let result = function.cif.call::<i8>(function.symbol_ptr, &raw_args);
                Ok(Value::SignedByte(result))
            }
            "u16" => {
                let result = function.cif.call::<u16>(function.symbol_ptr, &raw_args);
                Ok(Value::Word(result))
            }
            "i16" => {
                let result = function.cif.call::<i16>(function.symbol_ptr, &raw_args);
                Ok(Value::SignedWord(result))
            }
            "u32" => {
                let result = function.cif.call::<u32>(function.symbol_ptr, &raw_args);
                Ok(Value::DoubleWord(result))
            }
            "i32" => {
                let result = function.cif.call::<i32>(function.symbol_ptr, &raw_args);
                Ok(Value::SignedDoubleWord(result))
            }
            "u64" => {
                let result = function.cif.call::<u64>(function.symbol_ptr, &raw_args);
                Ok(Value::QuadrupleWord(result))
            }
            "i64" => {
                let result = function.cif.call::<i64>(function.symbol_ptr, &raw_args);
                Ok(Value::SignedQuadrupleWord(result))
            }
            "f32" => {
                let result = function.cif.call::<f32>(function.symbol_ptr, &raw_args);
                Ok(Value::Single(result))
            }
            "f64" => {
                let result = function.cif.call::<f64>(function.symbol_ptr, &raw_args);
                Ok(Value::Float(result))
            }
            "ptr" | "pointer" | "cstr" => {
                let result = function.cif.call::<*mut c_void>(function.symbol_ptr, &raw_args);
                Ok(Value::QuadrupleWord(result as u64))
            }
            _ => Err("unsupported return type".to_string()),
        }
    }
}

trait RawValueHolder {
    fn raw_ptr(&self) -> &dyn std::any::Any;
}

struct TypedHolder<T> {
    value: Box<T>,
}

struct CStringHolder {
    _cstring_keepalive: CString,       // keeps memory alive
    ptr: u64,                          // pointer for libffi
}

impl RawValueHolder for CStringHolder {
    fn raw_ptr(&self) -> &dyn std::any::Any {
        &self.ptr as &dyn std::any::Any
    }
}

impl<T: 'static> RawValueHolder for TypedHolder<T> {
    fn raw_ptr(&self) -> &dyn std::any::Any {
        self.value.as_ref()
    }
}

fn box_value(value: Value, ty: &str) -> Result<Box<dyn RawValueHolder>, String> {
    match (&value, ty) {
        (Value::Byte(v), "bool" | "u8") => Ok(Box::new(TypedHolder { value: Box::new(*v) })),
        (Value::SignedByte(v), "i8") => Ok(Box::new(TypedHolder { value: Box::new(*v) })),
        (Value::Word(v), "u16") => Ok(Box::new(TypedHolder { value: Box::new(*v) })),
        (Value::SignedWord(v), "i16") => Ok(Box::new(TypedHolder { value: Box::new(*v) })),
        (Value::DoubleWord(v), "u32") => Ok(Box::new(TypedHolder { value: Box::new(*v) })),
        (Value::DoubleWord(v), "i32") => Ok(Box::new(TypedHolder { value: Box::new(*v as i32) })),
        (Value::SignedDoubleWord(v), "i32") => Ok(Box::new(TypedHolder { value: Box::new(*v) })),
        (Value::SignedDoubleWord(v), "u32") => Ok(Box::new(TypedHolder { value: Box::new(*v as u32) })),
        (Value::QuadrupleWord(v), "u64" | "ptr" | "pointer") => Ok(Box::new(TypedHolder { value: Box::new(*v) })),
        (Value::QuadrupleWord(v), "i64") => Ok(Box::new(TypedHolder { value: Box::new(*v as i64) })),
        (Value::SignedQuadrupleWord(v), "i64") => Ok(Box::new(TypedHolder { value: Box::new(*v) })),
        (Value::SignedQuadrupleWord(v), "u64" | "ptr" | "pointer") => Ok(Box::new(TypedHolder { value: Box::new(*v as u64) })),
        (Value::Single(v), "f32") => Ok(Box::new(TypedHolder { value: Box::new(*v) })),
        (Value::Float(v), "f64") => Ok(Box::new(TypedHolder { value: Box::new(*v) })),

        // this... exists... Also it is VERY BAD
        // might not even work
        (Value::String(s), "ptr" | "pointer" | "cstr") => {
            let c_str = CString::new(s.as_str())
                .expect("failed to allocate C string");

            // println!("C strnig: {:?}", c_str);

            // If you're not 64-bit you're out, fuck the 32-bitters!
            let ptr = c_str.as_ptr() as u64;

            // println!("@: {:?}", ptr);

            // This MAY or MAY NOT leak, we'll see
            Ok(Box::new(CStringHolder { _cstring_keepalive: c_str, ptr }))
        },

        (Value::Ref(rc_refcell), "ptr" | "pointer" | "cstr") => {
            let mut_ref: &mut Value = &mut *rc_refcell.borrow_mut();
            let ptr: *mut Value = mut_ref as *mut Value;

            Ok(Box::new(TypedHolder { value: Box::new(ptr) }))
        },

        _ => Err(format!("unsupported argument type: {} for value {:?}", ty, value)),
    }
}

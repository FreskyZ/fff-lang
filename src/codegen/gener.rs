
// Generater
// Input program, dispatch statement to control flow statement generater or expression statement generater
// Recording generate process and provide utilities
// Get result and assemble to codegened program

use common::StringPosition;
use message::MessageEmitter;

use syntax::Program as SyntaxProgram;

use codegen::VMCode;
use codegen::TypeCollection;
use codegen::FunctionCollection;
use codegen::VMCodeCollection;

pub struct CodeGenerater {
    msgs: MessageEmitter,
    types: TypeCollection,
    funcs: FunctionCollection,
    codes: VMCodeCollection,
}

impl CodeGenerater {

    pub fn new() -> CodeGenerater {
        CodeGenerater{
            msgs: MessageEmitter::new(),
            types: TypeCollection::new(),
            funcs: FunctionCollection::new(),
            codes: VMCodeCollection::new(),
        }
    }
}

impl CodeGenerater {

    pub fn generate(&mut self, program: SyntaxProgram) {

        self.funcs.generate(program.functions);
    }
}

#[cfg(test)]
mod tests {

    fn gen_all() {

        // Function name same error 

    }
}
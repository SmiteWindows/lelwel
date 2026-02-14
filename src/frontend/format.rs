use super::ast::*;
use crate::{Cst, NodeRef};

pub struct LlwFormatter {
    indent_level: usize,
    output: String,
    line_width: usize,
}

impl Default for LlwFormatter {
    fn default() -> Self {
        Self::new()
    }
}

impl LlwFormatter {
    pub fn new() -> Self {
        Self {
            indent_level: 0,
            output: String::new(),
            line_width: 0,
        }
    }

    pub fn format_file(&mut self, cst: &Cst<'_>, file: File) -> String {
        self.output.clear();
        self.indent_level = 0;
        self.line_width = 0;

        // Format declarations
        let mut decls: Vec<Decl> = Vec::new();
        // Collect all declarations and convert to Decl enum
        decls.extend(file.start_decls(cst).map(Decl::Start));
        decls.extend(file.right_decls(cst).map(Decl::Right));
        decls.extend(file.skip_decls(cst).map(Decl::Skip));
        decls.extend(file.part_decls(cst).map(Decl::Part));
        decls.extend(file.token_decls(cst).map(Decl::Token));
        decls.extend(file.rule_decls(cst).map(Decl::Rule));

        for (i, decl) in decls.iter().enumerate() {
            self.format_decl(cst, decl);
            if i < decls.len() - 1 {
                self.writeln();
                self.writeln();
            }
        }

        self.output.clone()
    }

    fn format_decl(&mut self, cst: &Cst<'_>, decl: &Decl) {
        match decl {
            Decl::Start(start) => self.format_start_decl(cst, *start),
            Decl::Right(right) => self.format_right_decl(cst, *right),
            Decl::Skip(skip) => self.format_skip_decl(cst, *skip),
            Decl::Part(part) => self.format_part_decl(cst, *part),
            Decl::Token(token) => self.format_token_decl(cst, *token),
            Decl::Rule(rule) => self.format_rule_decl(cst, *rule),
        }
    }

    fn format_start_decl(&mut self, cst: &Cst<'_>, decl: StartDecl) {
        self.write("start ");
        if let Some((name, _)) = decl.rule_name(cst) {
            self.write(name);
        }
        self.write(";");
    }

    fn format_right_decl(&mut self, cst: &Cst<'_>, decl: RightDecl) {
        self.write("right ");
        let mut first = true;
        decl.token_names(cst, |(name, _)| {
            if !first {
                self.write(" ");
            }
            self.write(name);
            first = false;
        });
        self.write(";");
    }

    fn format_skip_decl(&mut self, cst: &Cst<'_>, decl: SkipDecl) {
        self.write("skip ");
        let mut first = true;
        decl.token_names(cst, |(name, _)| {
            if !first {
                self.write(" ");
            }
            self.write(name);
            first = false;
        });
        self.write(";");
    }

    fn format_part_decl(&mut self, cst: &Cst<'_>, decl: PartDecl) {
        self.write("part ");
        let mut first = true;
        decl.rule_names(cst, |(name, _)| {
            if !first {
                self.write(" ");
            }
            self.write(name);
            first = false;
        });
        self.write(";");
    }

    fn format_token_decl(&mut self, cst: &Cst<'_>, decl: TokenDecl) {
        self.write("token ");
        if let Some((name, _)) = decl.name(cst) {
            self.write(name);
        }
        if let Some((symbol, _)) = decl.symbol(cst) {
            self.write(" = ");
            self.write(symbol);
        }
        self.write(";");
    }

    fn format_rule_decl(&mut self, cst: &Cst<'_>, decl: RuleDecl) {
        if let Some((name, _)) = decl.name(cst) {
            self.write(name);
        }

        if decl.is_elided(cst) {
            self.write("^");
        }

        self.write(": ");

        if let Some(regex) = decl.regex(cst) {
            self.format_regex(cst, regex);
        }

        self.write(";");
    }

    fn format_regex(&mut self, cst: &Cst<'_>, regex: Regex) {
        match regex {
            Regex::Alternation(alt) => {
                let operands: Vec<_> = alt.operands(cst).collect();
                for (i, op) in operands.iter().enumerate() {
                    if i > 0 {
                        self.write(" | ");
                    }
                    self.format_regex(cst, *op);
                }
            }
            Regex::OrderedChoice(choice) => {
                let operands: Vec<_> = choice.operands(cst).collect();
                for (i, op) in operands.iter().enumerate() {
                    if i > 0 {
                        self.write(" / ");
                    }
                    self.format_regex(cst, *op);
                }
            }
            Regex::Concat(concat) => {
                let operands: Vec<_> = concat.operands(cst).collect();
                for op in operands {
                    self.format_regex(cst, op);
                }
            }
            Regex::Paren(paren) => {
                self.write("(");
                if let Some(inner) = paren.inner(cst) {
                    self.format_regex(cst, inner);
                }
                self.write(")");
            }
            Regex::Optional(opt) => {
                self.write("[");
                if let Some(operand) = opt.operand(cst) {
                    self.format_regex(cst, operand);
                }
                self.write("]");
            }
            Regex::Star(star) => {
                if let Some(operand) = star.operand(cst) {
                    self.format_regex(cst, operand);
                }
                self.write("*");
            }
            Regex::Plus(plus) => {
                if let Some(operand) = plus.operand(cst) {
                    self.format_regex(cst, operand);
                }
                self.write("+");
            }
            Regex::Name(name) => {
                if let Some((value, _)) = name.value(cst) {
                    self.write(value);
                }
            }
            Regex::Symbol(symbol) => {
                if let Some((value, _)) = symbol.value(cst) {
                    self.write(value);
                }
            }
            Regex::Predicate(pred) => {
                if let Some((value, _)) = pred.value(cst) {
                    self.write(value);
                }
            }
            Regex::Action(action) => {
                if let Some((value, _)) = action.value(cst) {
                    self.write(value);
                }
            }
            Regex::Assertion(assertion) => {
                if let Some((value, _)) = assertion.value(cst) {
                    self.write(value);
                }
            }
            Regex::NodeRename(rename) => {
                if let Some((value, _)) = rename.value(cst) {
                    self.write(value);
                }
            }
            Regex::NodeMarker(marker) => {
                if let Some((value, _)) = marker.value(cst) {
                    self.write(value);
                }
            }
            Regex::NodeCreation(creation) => {
                if let Some((value, _)) = creation.value(cst) {
                    self.write(value);
                }
            }
            Regex::NodeElision(_) => {
                self.write("^");
            }
            Regex::Commit(_) => {
                self.write("~");
            }
            Regex::Return(_) => {
                self.write("&");
            }
        }
    }

    fn write(&mut self, text: &str) {
        self.output.push_str(text);
        self.line_width += text.len();
    }

    fn writeln(&mut self) {
        self.output.push('\n');
        self.output.push_str(&"  ".repeat(self.indent_level));
        self.line_width = self.indent_level * 2;
    }
}

enum Decl {
    Start(StartDecl),
    Right(RightDecl),
    Skip(SkipDecl),
    Part(PartDecl),
    Token(TokenDecl),
    Rule(RuleDecl),
}

pub fn format_llw(cst: &Cst<'_>) -> Option<String> {
    if let Some(file) = File::cast(cst, NodeRef::ROOT) {
        let mut formatter = LlwFormatter::new();
        Some(formatter.format_file(cst, file))
    } else {
        None
    }
}

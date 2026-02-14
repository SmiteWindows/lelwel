use super::ast::*;
use crate::{Cst, NodeRef, Span};

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

        // Collect all declarations using iterator chains
        let decls: Vec<Decl> = file
            .token_decls(cst)
            .map(Decl::Token)
            .chain(file.skip_decls(cst).map(Decl::Skip))
            .chain(file.right_decls(cst).map(Decl::Right))
            .chain(file.start_decls(cst).map(Decl::Start))
            .chain(file.rule_decls(cst).map(Decl::Rule))
            .chain(file.part_decls(cst).map(Decl::Part))
            .collect();

        for (i, decl) in decls.iter().enumerate() {
            self.format_decl(cst, decl);
            if i < decls.len() - 1 {
                self.writeln();
                self.writeln();
            }
        }

        self.output.clone()
    }

    /// Format a file while preserving comments
    pub fn format_file_with_comments(&mut self, source: &str, cst: &Cst<'_>, file: File) -> String {
        self.output.clear();
        self.indent_level = 0;
        self.line_width = 0;

        // Collect all declarations with positions using iterator chains
        let mut declarations: Vec<(Span, Decl)> = file
            .token_decls(cst)
            .map(|d| (d.span(cst), Decl::Token(d)))
            .chain(file.skip_decls(cst).map(|d| (d.span(cst), Decl::Skip(d))))
            .chain(file.right_decls(cst).map(|d| (d.span(cst), Decl::Right(d))))
            .chain(file.start_decls(cst).map(|d| (d.span(cst), Decl::Start(d))))
            .chain(file.rule_decls(cst).map(|d| (d.span(cst), Decl::Rule(d))))
            .chain(file.part_decls(cst).map(|d| (d.span(cst), Decl::Part(d))))
            .collect();

        declarations.sort_by_key(|(span, _)| span.start);

        // Process declarations with modern iterator patterns
        let last_position =
            declarations
                .iter()
                .enumerate()
                .fold(0, |last_position, (i, (span, decl))| {
                    // Extract and preserve content between declarations
                    if span.start > last_position {
                        let gap = &source[last_position..span.start];
                        self.extract_and_format_comments(gap);
                    }

                    // Format the current declaration
                    self.format_decl(cst, decl);

                    // Add spacing between declarations
                    if i < declarations.len() - 1 {
                        self.writeln();
                        self.writeln();
                    }

                    span.end
                });

        // Preserve any content after the last declaration using modern syntax
        if let Some(remaining) = source
            .get(last_position..) { self.extract_and_format_comments(remaining) }

        self.output.clone()
    }

    /// Extract and format comments with intelligent whitespace handling
    fn extract_and_format_comments(&mut self, text: &str) {
        if text.trim().is_empty() {
            // Preserve newlines for pure whitespace
            text.chars()
                .filter(|&c| c == '\n')
                .for_each(|_| self.writeln());
        } else {
            // Process text with comments using modern iterator patterns
            let mut processed = 0;
            let bytes = text.as_bytes();

            while processed < bytes.len() {
                // Detect single-line comments using pattern matching
                if let Some(comment_end) = self.detect_single_line_comment(&text[processed..]) {
                    let comment = &text[processed..processed + comment_end];
                    self.write(comment);
                    self.writeln();
                    processed += comment_end;
                }
                // Detect multi-line comments
                else if let Some(comment_end) = self.detect_multi_line_comment(&text[processed..])
                {
                    let comment = &text[processed..processed + comment_end];
                    self.write(comment);
                    self.writeln();
                    processed += comment_end;
                }
                // Handle non-comment content
                else if let Some(next_comment) = text[processed..].find('/') {
                    let content = &text[processed..processed + next_comment];
                    if !content.trim().is_empty() {
                        self.write(content);
                    }
                    processed += next_comment;
                } else {
                    // Write remaining content
                    let remaining = &text[processed..];
                    if !remaining.trim().is_empty() {
                        self.write(remaining);
                    }
                    break;
                }
            }
        }
    }

    /// Detect single-line comments using modern string methods
    fn detect_single_line_comment(&self, text: &str) -> Option<usize> {
        text.starts_with("//").then(|| {
            text.char_indices()
                .find(|(_, c)| *c == '\n')
                .map(|(i, _)| i)
                .unwrap_or(text.len())
        })
    }

    /// Detect multi-line comments using modern string methods
    fn detect_multi_line_comment(&self, text: &str) -> Option<usize> {
        text.starts_with("/*")
            .then(|| text.find("*/").map(|i| i + 2).unwrap_or(text.len()))
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

        let mut names = Vec::new();
        decl.token_names(cst, |(name, _)| names.push(name));

        if !names.is_empty() {
            self.write(&names.join(" "));
        }
        self.write(";");
    }

    fn format_skip_decl(&mut self, cst: &Cst<'_>, decl: SkipDecl) {
        self.write("skip ");

        let mut names = Vec::new();
        decl.token_names(cst, |(name, _)| names.push(name));

        if !names.is_empty() {
            self.write(&names.join(" "));
        }
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

pub fn format_llw_with_comments(source: &str, cst: &Cst<'_>) -> Option<String> {
    if let Some(file) = File::cast(cst, NodeRef::ROOT) {
        let mut formatter = LlwFormatter::new();
        Some(formatter.format_file_with_comments(source, cst, file))
    } else {
        None
    }
}

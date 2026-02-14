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

        // Sort declarations by their position in the source
        declarations.sort_by_key(|(span, _)| span.start);

        let mut last_position = 0;

        for (i, (span, decl)) in declarations.iter().enumerate() {
            // Extract and preserve content between last declaration and current one
            if span.start > last_position {
                let gap = &source[last_position..span.start];
                self.preserve_comments_and_whitespace(gap);
            }

            // Format the current declaration
            self.format_decl(cst, decl);

            // Add spacing between declarations (but not after the last one)
            if i < declarations.len() - 1 {
                self.writeln();
                self.writeln();
            }

            last_position = span.end;
        }

        // Preserve any content after the last declaration
        source
            .get(last_position..)
            .map(|remaining| self.preserve_comments_and_whitespace(remaining));

        self.output.clone()
    }

    /// Preserve comments and whitespace from the original source
    fn preserve_comments_and_whitespace(&mut self, text: &str) {
        if text.trim().is_empty() {
            // If it's just whitespace, preserve the newlines using iterator
            text.chars()
                .filter(|&c| c == '\n')
                .for_each(|_| self.writeln());
        } else {
            // Preserve the text as-is (including comments)
            self.write(text);
        }
    }

    fn format_comments_before(&mut self, cst: &Cst<'_>, node: NodeRef, last_position: usize) {
        let node_span = cst.span(node);

        // Look for comments between last_position and current node start
        if node_span.start > last_position {
            let source = cst.source();
            let gap = &source[last_position..node_span.start];

            // Use iterator-based approach to process the gap
            let mut processed_chars = 0;
            let gap_bytes = gap.as_bytes();

            while processed_chars < gap_bytes.len() {
                // Check for single-line comments
                if processed_chars + 1 < gap_bytes.len()
                    && gap_bytes[processed_chars] == b'/'
                    && gap_bytes[processed_chars + 1] == b'/'
                {
                    // Find the end of single-line comment (end of line)
                    let comment_end = gap[processed_chars..]
                        .char_indices()
                        .find(|(_, c)| *c == '\n')
                        .map(|(i, _)| processed_chars + i)
                        .unwrap_or(gap.len());

                    // Write the comment
                    let comment = &gap[processed_chars..comment_end];
                    self.write(comment);
                    self.writeln();

                    processed_chars = comment_end;
                }
                // Check for multi-line comments
                else if processed_chars + 1 < gap_bytes.len()
                    && gap_bytes[processed_chars] == b'/'
                    && gap_bytes[processed_chars + 1] == b'*'
                {
                    // Find the end of multi-line comment (*/)
                    let comment_end = gap[processed_chars..]
                        .char_indices()
                        .find(|(i, c)| {
                            *c == '*'
                                && i + 1 < gap[processed_chars..].len()
                                && gap.as_bytes()[processed_chars + i + 1] == b'/'
                        })
                        .map(|(i, _)| processed_chars + i + 2)
                        .unwrap_or(gap.len());

                    // Write the comment
                    let comment = &gap[processed_chars..comment_end];
                    self.write(comment);
                    self.writeln();

                    processed_chars = comment_end;
                } else {
                    // Write non-comment content (whitespace, etc.)
                    let next_comment_start = gap[processed_chars..]
                        .char_indices()
                        .find(|(_, c)| *c == '/')
                        .map(|(i, _)| processed_chars + i)
                        .unwrap_or(gap.len());

                    if next_comment_start > processed_chars {
                        let content = &gap[processed_chars..next_comment_start];
                        if !content.trim().is_empty() {
                            self.write(content);
                        }
                        processed_chars = next_comment_start;
                    } else {
                        // No more content to process
                        break;
                    }
                }
            }
        }
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

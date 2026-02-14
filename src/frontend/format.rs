use super::ast::*;
use crate::{Cst, NodeRef, Span};

pub struct LlwFormatter {
    indent_level: usize,
    output: String,
    line_width: usize,
    max_line_width: usize,
    indent_size: usize,
    should_wrap: bool,
    compact_concat: bool,
    align_operators: bool,
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
            max_line_width: 80,
            indent_size: 2,
            should_wrap: true,
            compact_concat: false,
            align_operators: true,
        }
    }

    pub fn with_max_line_width(mut self, width: usize) -> Self {
        self.max_line_width = width;
        self
    }

    pub fn with_indent_size(mut self, size: usize) -> Self {
        self.indent_size = size;
        self
    }

    pub fn with_wrapping(mut self, wrap: bool) -> Self {
        self.should_wrap = wrap;
        self
    }

    pub fn with_compact_concat(mut self, compact: bool) -> Self {
        self.compact_concat = compact;
        self
    }

    pub fn with_align_operators(mut self, align: bool) -> Self {
        self.align_operators = align;
        self
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
        if let Some(remaining) = source.get(last_position..) {
            self.extract_and_format_comments(remaining)
        }

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
                    self.format_comment(comment);
                    processed += comment_end;
                }
                // Detect multi-line comments
                else if let Some(comment_end) = self.detect_multi_line_comment(&text[processed..])
                {
                    let comment = &text[processed..processed + comment_end];
                    self.format_comment(comment);
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

    /// Format comment with proper indentation
    fn format_comment(&mut self, comment: &str) {
        let trimmed = comment.trim();
        if trimmed.is_empty() {
            return;
        }

        // For single-line comments, preserve indentation
        if comment.starts_with("//") {
            self.write_indented(comment);
            self.writeln();
        }
        // For multi-line comments, format each line with proper indentation
        else if comment.starts_with("/*") {
            let lines: Vec<&str> = comment.split('\n').collect();
            for (i, line) in lines.iter().enumerate() {
                if i > 0 {
                    self.writeln();
                }
                // Preserve leading spaces for alignment in multi-line comments
                let leading_spaces = line.len() - line.trim_start().len();
                let indent = " ".repeat(self.indent_level * self.indent_size + leading_spaces);
                self.output.push_str(&indent);
                self.output.push_str(line.trim());
                self.line_width =
                    self.indent_level * self.indent_size + leading_spaces + line.trim().len();
            }
            self.writeln();
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
        let mut token_line = String::new();
        token_line.push_str("token ");

        if let Some((name, _)) = decl.name(cst) {
            token_line.push_str(name);
        }

        if let Some((symbol, _)) = decl.symbol(cst) {
            token_line.push_str(" = ");
            token_line.push_str(symbol);
        }

        token_line.push(';');

        // Check if line is too long and needs wrapping
        if self.should_wrap && token_line.len() > self.max_line_width {
            // Multi-line format for long token declarations
            self.write("token ");
            if let Some((name, _)) = decl.name(cst) {
                self.write(name);
            }
            if let Some((symbol, _)) = decl.symbol(cst) {
                self.writeln();
                self.write_indented("= ");
                self.write(symbol);
            }
            self.write(";");
        } else {
            // Single-line format
            self.write(&token_line);
        }
    }

    fn format_rule_decl(&mut self, cst: &Cst<'_>, decl: RuleDecl) {
        // Write rule name and modifiers
        let mut rule_header = String::new();
        if let Some((name, _)) = decl.name(cst) {
            rule_header.push_str(name);
        }

        if decl.is_elided(cst) {
            rule_header.push('^');
        }

        rule_header.push_str(": ");

        // Check if we need to wrap the regex
        if let Some(regex) = decl.regex(cst) {
            let regex_width = self.estimate_regex_width(cst, &regex);
            let total_width = self.line_width + rule_header.len() + regex_width;

            if self.should_wrap && total_width > self.max_line_width && regex_width > 20 {
                // Multi-line format
                self.write(&rule_header);
                self.indent_level += 1;
                self.writeln();
                self.format_regex(cst, regex);
                self.indent_level -= 1;
                self.writeln();
                self.write(";");
            } else {
                // Single-line format
                self.write(&rule_header);
                self.format_regex(cst, regex);
                self.write(";");
            }
        } else {
            self.write(&rule_header);
            self.write(";");
        }
    }

    fn format_regex(&mut self, cst: &Cst<'_>, regex: Regex) {
        self.format_regex_internal(cst, regex, false)
    }

    fn format_regex_internal(&mut self, cst: &Cst<'_>, regex: Regex, parenthesize: bool) {
        let needs_parentheses = parenthesize && self.should_parenthesize(&regex);

        if needs_parentheses {
            self.write("(");
        }

        match regex {
            Regex::Alternation(alt) => {
                let operands: Vec<_> = alt.operands(cst).collect();
                self.format_operator_separated(cst, operands, " | ", "alternation")
            }
            Regex::OrderedChoice(choice) => {
                let operands: Vec<_> = choice.operands(cst).collect();
                self.format_operator_separated(cst, operands, " / ", "ordered_choice")
            }
            Regex::Concat(concat) => {
                let operands: Vec<_> = concat.operands(cst).collect();
                self.format_concat(cst, operands)
            }
            Regex::Paren(paren) => {
                self.write("(");
                if let Some(inner) = paren.inner(cst) {
                    self.format_regex_internal(cst, inner, false);
                }
                self.write(")");
            }
            Regex::Optional(opt) => self.format_unary_operator(cst, opt.operand(cst), "[", "]"),
            Regex::Star(star) => self.format_unary_operator(cst, star.operand(cst), "", "*"),
            Regex::Plus(plus) => self.format_unary_operator(cst, plus.operand(cst), "", "+"),
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

        if needs_parentheses {
            self.write(")");
        }
    }

    fn format_operator_separated(
        &mut self,
        cst: &Cst<'_>,
        operands: Vec<Regex>,
        separator: &str,
        _operator_type: &str,
    ) {
        if operands.is_empty() {
            return;
        }

        let current_line_width = self.line_width;
        let separator_len = separator.len();

        // Check if we need to wrap
        let estimated_width = current_line_width
            + self.estimate_regex_width(cst, &operands[0])
            + operands
                .iter()
                .skip(1)
                .map(|op| separator_len + self.estimate_regex_width(cst, op))
                .sum::<usize>();

        let should_wrap =
            self.should_wrap && estimated_width > self.max_line_width && operands.len() > 1;

        // 操作符对齐：如果启用且需要换行，对齐操作符
        let operator_alignment = self.align_operators && should_wrap;

        if should_wrap {
            self.indent_level += 1;
        }

        for (i, op) in operands.iter().enumerate() {
            if i > 0 {
                if should_wrap {
                    self.writeln();
                    if operator_alignment {
                        // 对齐操作符到相同缩进级别
                        let alignment_spaces = self.indent_level * self.indent_size;
                        self.write(&" ".repeat(alignment_spaces));
                    } else {
                        self.write(&" ".repeat(self.indent_level * self.indent_size));
                    }
                } else {
                    self.write(separator);
                }
            }
            self.format_regex_internal(cst, *op, true);
        }

        if should_wrap {
            self.indent_level -= 1;
        }
    }

    fn format_concat(&mut self, cst: &Cst<'_>, operands: Vec<Regex>) {
        if operands.is_empty() {
            return;
        }

        // 紧凑串联格式：如果启用且操作数较少，保持在一行
        if self.compact_concat && operands.len() <= 3 {
            let total_width: usize = operands
                .iter()
                .map(|op| self.estimate_regex_width(cst, op))
                .sum();

            // 如果总宽度不超过限制，保持紧凑格式
            if self.line_width + total_width <= self.max_line_width {
                for op in operands {
                    self.format_regex_internal(cst, op, true);
                }
                return;
            }
        }

        // Check if concatenation needs wrapping
        let total_width: usize = operands
            .iter()
            .map(|op| self.estimate_regex_width(cst, op))
            .sum();

        let should_wrap = self.should_wrap
            && (self.line_width + total_width > self.max_line_width)
            && operands.len() > 1;

        if should_wrap {
            self.indent_level += 1;
        }

        for (i, op) in operands.iter().enumerate() {
            if i > 0 && should_wrap {
                self.writeln();
                self.write_indented("");
            }
            self.format_regex_internal(cst, *op, true);
        }

        if should_wrap {
            self.indent_level -= 1;
        }
    }

    fn format_unary_operator(
        &mut self,
        cst: &Cst<'_>,
        operand: Option<Regex>,
        prefix: &str,
        suffix: &str,
    ) {
        self.write(prefix);
        if let Some(op) = operand {
            self.format_regex_internal(cst, op, true);
        }
        self.write(suffix);
    }

    fn should_parenthesize(&self, regex: &Regex) -> bool {
        matches!(
            regex,
            Regex::Alternation(_) | Regex::OrderedChoice(_) | Regex::Concat(_)
        )
    }

    fn estimate_regex_width(&self, cst: &Cst<'_>, regex: &Regex) -> usize {
        match regex {
            Regex::Name(name) => name.value(cst).map(|(v, _)| v.len()).unwrap_or(0),
            Regex::Symbol(symbol) => symbol.value(cst).map(|(v, _)| v.len()).unwrap_or(0),
            Regex::Paren(_) => 2,                 // Minimum for parentheses
            Regex::Optional(_) => 2,              // Minimum for brackets
            Regex::Star(_) | Regex::Plus(_) => 1, // Single character operators
            _ => 10,                              // Conservative estimate for complex expressions
        }
    }

    fn write(&mut self, text: &str) {
        self.output.push_str(text);
        self.line_width += text.len();
    }

    fn writeln(&mut self) {
        self.output.push('\n');
        self.output
            .push_str(&" ".repeat(self.indent_level * self.indent_size));
        self.line_width = self.indent_level * self.indent_size;
    }

    fn write_indented(&mut self, text: &str) {
        if self.line_width == 0 {
            self.output
                .push_str(&" ".repeat(self.indent_level * self.indent_size));
            self.line_width = self.indent_level * self.indent_size;
        }
        self.write(text);
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
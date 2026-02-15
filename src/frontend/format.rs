//! Lelwel语法文件格式化模块
//!
//! 本模块提供LLW语法文件的格式化功能，基于lelwel.llw文件的默认风格。
//! 主要特点包括单行规则声明、保守的括号使用、简洁的串联格式和对齐操作符。
//!
//! # 设计原则
//! 1. 保持与lelwel.llw文件风格一致
//! 2. 优先使用单行格式，只在必要时换行
//! 3. 避免不必要的括号和空格
//! 4. 提供清晰的注释以便后续微调

use super::ast::*;
use crate::{Cst, NodeRef, Span};

/// Lelwel格式化器，基于lelwel.llw文件的默认风格
///
/// 主要特点：
/// - 单行规则声明为主（符合lelwel.llw的实际使用模式）
/// - 保守的括号使用（避免过度括号化）
/// - 简洁的串联格式
/// - 对齐操作符
pub struct LlwFormatter {
    /// 当前缩进级别
    indent_level: usize,
    /// 格式化输出缓冲区
    output: String,
    /// 当前行宽度（用于换行决策）
    line_width: usize,
    /// 最大行宽限制（默认80字符）
    max_line_width: usize,
    /// 缩进大小（默认2空格）
    indent_size: usize,
    /// 是否启用自动换行
    should_wrap: bool,
    /// 是否启用紧凑串联格式
    compact_concat: bool,
    /// 是否对齐操作符
    align_operators: bool,
}

impl Default for LlwFormatter {
    /// 提供默认格式化器实例
    ///
    /// 逻辑：调用`Self::new()`创建使用lelwel.llw默认风格的新实例
    fn default() -> Self {
        Self::new()
    }
}

impl LlwFormatter {
    /// 创建新的格式化器实例，使用lelwel.llw的默认风格
    ///
    /// 逻辑：初始化所有格式化参数为默认值
    /// - indent_level: 0 - 从零缩进开始
    /// - output: 空字符串 - 准备接收格式化内容
    /// - line_width: 0 - 当前行宽度为零
    /// - max_line_width: 80 - 标准最大行宽限制
    /// - indent_size: 2 - 标准缩进大小（2空格）
    /// - should_wrap: true - 启用自动换行
    /// - compact_concat: true - 基于lelwel.llw风格，启用紧凑格式
    /// - align_operators: true - 启用操作符对齐
    pub fn new() -> Self {
        Self {
            indent_level: 0,
            output: String::new(),
            line_width: 0,
            max_line_width: 80,
            indent_size: 2,
            should_wrap: true,
            compact_concat: true, // 基于lelwel.llw风格，启用紧凑格式
            align_operators: true,
        }
    }

    /// 设置最大行宽限制
    ///
    /// 逻辑：修改`max_line_width`字段，用于决定何时需要换行
    /// 返回修改后的self以支持方法链式调用
    ///
    /// # 参数
    /// - `width`: 最大行宽（字符数）
    pub fn with_max_line_width(mut self, width: usize) -> Self {
        self.max_line_width = width;
        self
    }

    /// 设置缩进大小
    ///
    /// 逻辑：修改`indent_size`字段，控制每次缩进的空格数
    /// 返回修改后的self以支持方法链式调用
    ///
    /// # 参数
    /// - `size`: 缩进空格数
    pub fn with_indent_size(mut self, size: usize) -> Self {
        self.indent_size = size;
        self
    }

    /// 设置是否启用自动换行
    ///
    /// 逻辑：修改`should_wrap`字段，控制是否在行过长时自动换行
    /// 返回修改后的self以支持方法链式调用
    ///
    /// # 参数
    /// - `wrap`: 是否启用换行
    pub fn with_wrapping(mut self, wrap: bool) -> Self {
        self.should_wrap = wrap;
        self
    }

    /// 设置是否启用紧凑串联格式
    ///
    /// 逻辑：修改`compact_concat`字段，控制串联表达式是否尽量保持单行
    /// 返回修改后的self以支持方法链式调用
    ///
    /// # 参数
    /// - `compact`: 是否启用紧凑格式
    pub fn with_compact_concat(mut self, compact: bool) -> Self {
        self.compact_concat = compact;
        self
    }

    /// 设置是否对齐操作符
    ///
    /// 逻辑：修改`align_operators`字段，控制换行时是否对齐操作符
    /// 返回修改后的self以支持方法链式调用
    ///
    /// # 参数
    /// - `align`: 是否对齐操作符
    pub fn with_align_operators(mut self, align: bool) -> Self {
        self.align_operators = align;
        self
    }

    /// 格式化整个文件，不保留注释
    ///
    /// 逻辑：
    /// 1. 重置格式化器状态（清空输出，重置缩进和行宽）
    /// 2. 收集所有声明类型（token、skip、right、start、rule、part）
    /// 3. 遍历每个声明并调用相应的格式化方法
    /// 4. 在声明之间添加空行分隔
    /// 5. 返回格式化后的字符串
    ///
    /// 注意：声明顺序基于lelwel.llw的常见模式，但顺序是自由的
    ///
    /// # 参数
    /// - `cst`: 语法树
    /// - `file`: 文件AST节点
    ///
    /// # 返回
    /// 格式化后的字符串
    pub fn format_file(&mut self, cst: &Cst<'_>, file: File) -> String {
        // 重置格式化器状态
        self.output.clear();
        self.indent_level = 0;
        self.line_width = 0;

        // 使用迭代器链式操作收集所有声明
        // 逻辑：按类型分组收集，但保持原始声明的相对顺序
        let decls: Vec<Decl> = file
            .token_decls(cst)
            .map(Decl::Token)
            .chain(file.skip_decls(cst).map(Decl::Skip))
            .chain(file.right_decls(cst).map(Decl::Right))
            .chain(file.start_decls(cst).map(Decl::Start))
            .chain(file.rule_decls(cst).map(Decl::Rule))
            .chain(file.part_decls(cst).map(Decl::Part))
            .collect();

        // 格式化每个声明，在声明之间添加空行
        // 逻辑：遍历所有声明，调用format_decl，在非最后一个声明后添加两个换行
        for (i, decl) in decls.iter().enumerate() {
            self.format_decl(cst, decl);
            if i < decls.len() - 1 {
                self.writeln();
                self.writeln(); // 声明之间用空行分隔，提高可读性
            }
        }

        // 返回格式化后的内容
        self.output.clone()
    }

    /// 格式化文件并保留注释
    ///
    /// 逻辑：
    /// 1. 重置格式化器状态
    /// 2. 收集所有声明及其位置信息
    /// 3. 按位置排序以保持原始顺序
    /// 4. 处理声明之间的注释内容
    /// 5. 格式化每个声明
    /// 6. 处理文件末尾的剩余内容
    ///
    /// 与format_file的主要区别：保留原始注释和空白
    ///
    /// # 参数
    /// - `source`: 原始源代码
    /// - `cst`: 语法树
    /// - `file`: 文件AST节点
    ///
    /// # 返回
    /// 格式化后的字符串（保留注释）
    pub fn format_file_with_comments(&mut self, source: &str, cst: &Cst<'_>, file: File) -> String {
        // 重置格式化器状态
        self.output.clear();
        self.indent_level = 0;
        self.line_width = 0;

        // 收集所有声明及其位置信息
        // 逻辑：使用迭代器链式操作收集所有声明类型，并记录其span位置
        let mut declarations: Vec<(Span, Decl)> = file
            .token_decls(cst)
            .map(|d| (d.span(cst), Decl::Token(d)))
            .chain(file.skip_decls(cst).map(|d| (d.span(cst), Decl::Skip(d))))
            .chain(file.right_decls(cst).map(|d| (d.span(cst), Decl::Right(d))))
            .chain(file.start_decls(cst).map(|d| (d.span(cst), Decl::Start(d))))
            .chain(file.rule_decls(cst).map(|d| (d.span(cst), Decl::Rule(d))))
            .chain(file.part_decls(cst).map(|d| (d.span(cst), Decl::Part(d))))
            .collect();

        // 按声明在源文件中的起始位置排序
        // 逻辑：确保按原始顺序处理声明，以正确保留注释位置
        declarations.sort_by_key(|(span, _)| span.start);

        // 使用fold处理所有声明，同时跟踪处理位置
        // 逻辑：遍历排序后的声明，处理声明之间的内容（主要是注释）
        let last_position =
            declarations
                .iter()
                .enumerate()
                .fold(0, |last_position, (i, (span, decl))| {
                    // 提取并保留声明之间的内容（主要是注释）
                    // 逻辑：如果当前声明开始位置大于上次处理位置，说明中间有内容
                    if span.start > last_position {
                        let gap = &source[last_position..span.start];
                        self.extract_and_format_comments(gap);
                    }

                    // 格式化当前声明
                    self.format_decl(cst, decl);

                    // 在声明之间添加间距
                    // 逻辑：非最后一个声明后添加两个换行
                    if i < declarations.len() - 1 {
                        self.writeln();
                        self.writeln();
                    }

                    // 返回当前声明的结束位置，作为下次处理的起点
                    span.end
                });

        // 保留最后一个声明之后的任何内容
        // 逻辑：处理文件末尾可能存在的注释或空白
        if let Some(remaining) = source.get(last_position..) {
            self.extract_and_format_comments(remaining)
        }

        self.output.clone()
    }

    /// 提取并格式化注释，智能处理空白
    ///
    /// 逻辑：
    /// 1. 检查文本是否只包含空白（trim后为空）
    /// 2. 如果是纯空白，只保留换行符
    /// 3. 否则，处理包含注释的文本
    /// 4. 使用迭代器模式检测和处理单行/多行注释
    /// 5. 保留非注释内容
    fn extract_and_format_comments(&mut self, text: &str) {
        if text.trim().is_empty() {
            // 纯空白处理逻辑：只保留换行符
            // 逻辑：过滤出所有换行符，为每个换行符调用writeln
            text.chars()
                .filter(|&c| c == '\n')
                .for_each(|_| self.writeln());
        } else {
            // 处理包含注释的文本
            // 逻辑：使用字节索引进行精确的位置跟踪
            let mut processed = 0;
            let bytes = text.as_bytes();

            // 循环处理直到处理完所有文本
            while processed < bytes.len() {
                // 检测单行注释（//）
                // 逻辑：使用detect_single_line_comment检测单行注释范围
                if let Some(comment_end) = self.detect_single_line_comment(&text[processed..]) {
                    let comment = &text[processed..processed + comment_end];
                    self.format_comment(comment);
                    processed += comment_end;
                }
                // 检测多行注释（/* */）
                // 逻辑：使用detect_multi_line_comment检测多行注释范围
                else if let Some(comment_end) = self.detect_multi_line_comment(&text[processed..])
                {
                    let comment = &text[processed..processed + comment_end];
                    self.format_comment(comment);
                    processed += comment_end;
                }
                // 处理非注释内容
                // 逻辑：查找下一个可能的注释开始位置（/字符）
                else if let Some(next_comment) = text[processed..].find('/') {
                    let content = &text[processed..processed + next_comment];
                    if !content.trim().is_empty() {
                        self.write(content);
                    }
                    processed += next_comment;
                } else {
                    // 写入剩余内容
                    // 逻辑：处理文本末尾的非注释内容
                    let remaining = &text[processed..];
                    if !remaining.trim().is_empty() {
                        self.write(remaining);
                    }
                    break;
                }
            }
        }
    }

    /// 格式化注释，使用适当的缩进
    ///
    /// 逻辑：
    /// 1. 检查注释是否为空（trim后）
    /// 2. 处理单行注释（//）
    /// 3. 处理多行注释（/* */）
    /// 4. 为多行注释保留原始对齐和缩进
    fn format_comment(&mut self, comment: &str) {
        let trimmed = comment.trim();
        if trimmed.is_empty() {
            return; // 空注释不处理
        }

        // 单行注释处理逻辑
        // 逻辑：使用write_indented保持缩进，然后换行
        if comment.starts_with("//") {
            self.write_indented(comment);
            self.writeln();
        }
        // 多行注释处理逻辑
        // 逻辑：分割为多行，为每行保留原始对齐
        else if comment.starts_with("/*") {
            let lines: Vec<&str> = comment.split('\n').collect();
            for (i, line) in lines.iter().enumerate() {
                if i > 0 {
                    self.writeln(); // 非第一行前添加换行
                }
                // 保留多行注释的原始对齐
                // 逻辑：计算前导空格数，保持原始缩进级别
                let leading_spaces = line.len() - line.trim_start().len();
                let indent = " ".repeat(self.indent_level * self.indent_size + leading_spaces);
                self.output.push_str(&indent);
                self.output.push_str(line.trim());
                // 更新当前行宽度
                self.line_width =
                    self.indent_level * self.indent_size + leading_spaces + line.trim().len();
            }
            self.writeln(); // 多行注释结束后换行
        }
    }

    /// 检测单行注释（//）使用现代字符串方法
    ///
    /// 逻辑：
    /// 1. 检查文本是否以"//"开头
    /// 2. 如果是，查找下一个换行符的位置
    /// 3. 如果找到换行符，返回其位置；否则返回文本长度
    ///
    /// 返回值：单行注释的结束位置（包含换行符）
    fn detect_single_line_comment(&self, text: &str) -> Option<usize> {
        text.starts_with("//").then(|| {
            text.char_indices()
                .find(|(_, c)| *c == '\n')
                .map(|(i, _)| i)
                .unwrap_or(text.len())
        })
    }

    /// 检测多行注释（/* */）使用现代字符串方法
    ///
    /// 逻辑：
    /// 1. 检查文本是否以"/*"开头
    /// 2. 如果是，查找"*/"结束标记的位置
    /// 3. 如果找到结束标记，返回其位置+2（包含*/）；否则返回文本长度
    ///
    /// 返回值：多行注释的结束位置（包含*/）
    fn detect_multi_line_comment(&self, text: &str) -> Option<usize> {
        text.starts_with("/*")
            .then(|| text.find("*/").map(|i| i + 2).unwrap_or(text.len()))
    }

    /// 格式化声明，根据声明类型分发到相应的格式化方法
    ///
    /// 逻辑：
    /// 1. 使用模式匹配识别声明类型
    /// 2. 调用对应的具体格式化方法
    /// 3. 支持所有声明类型：Rule、Part、Token、Start、Right、Skip
    fn format_decl(&mut self, cst: &Cst<'_>, decl: &Decl) {
        match decl {
            Decl::Rule(rule) => self.format_rule_decl(cst, *rule),
            Decl::Part(part) => self.format_part_decl(cst, *part),
            Decl::Token(token) => self.format_token_decl(cst, *token),
            Decl::Start(start) => self.format_start_decl(cst, *start),
            Decl::Right(right) => self.format_right_decl(cst, *right),
            Decl::Skip(skip) => self.format_skip_decl(cst, *skip),
        }
    }

    /// 格式化start声明（start <rule_name>;）
    ///
    /// 逻辑：
    /// 1. 写入"start "关键字
    /// 2. 写入规则名称（如果存在）
    /// 3. 写入分号结束
    fn format_start_decl(&mut self, cst: &Cst<'_>, decl: StartDecl) {
        self.write("start ");
        if let Some((name, _)) = decl.rule_name(cst) {
            self.write(name);
        }
        self.write(";");
    }

    /// 格式化right声明（right <token_names>;）
    ///
    /// 逻辑：
    /// 1. 写入"right "关键字
    /// 2. 收集所有token名称
    /// 3. 用空格连接token名称
    /// 4. 写入分号结束
    fn format_right_decl(&mut self, cst: &Cst<'_>, decl: RightDecl) {
        self.write("right ");

        let mut names = Vec::new();
        decl.token_names(cst, |(name, _)| names.push(name));

        if !names.is_empty() {
            self.write(&names.join(" "));
        }
        self.write(";");
    }

    /// 格式化skip声明（skip <token_names>;）
    ///
    /// 逻辑：
    /// 1. 写入"skip "关键字
    /// 2. 收集所有token名称
    /// 3. 用空格连接token名称
    /// 4. 写入分号结束
    fn format_skip_decl(&mut self, cst: &Cst<'_>, decl: SkipDecl) {
        self.write("skip ");

        let mut names = Vec::new();
        decl.token_names(cst, |(name, _)| names.push(name));

        if !names.is_empty() {
            self.write(&names.join(" "));
        }
        self.write(";");
    }

    /// 格式化part声明（part <rule_names>;）
    ///
    /// 逻辑：
    /// 1. 写入"part "关键字
    /// 2. 遍历所有规则名称，用空格分隔
    /// 3. 使用first标志避免第一个名称前加空格
    /// 4. 写入分号结束
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

    /// 格式化token声明（token <name> [= <symbol>];）
    ///
    /// 逻辑：
    /// 1. 构建完整的token声明字符串
    /// 2. 检查是否需要换行（基于行宽限制）
    /// 3. 如果过长，使用多行格式；否则使用单行格式
    ///
    /// 多行格式示例：
    /// token name
    ///   = "symbol";
    fn format_token_decl(&mut self, cst: &Cst<'_>, decl: TokenDecl) {
        // 首先构建完整的token声明字符串以检查长度
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

        // 检查行是否过长需要换行
        // 逻辑：如果启用换行且token声明长度超过最大行宽
        if self.should_wrap && token_line.len() > self.max_line_width {
            // 多行格式：适用于长的token声明
            self.write("token ");
            if let Some((name, _)) = decl.name(cst) {
                self.write(name);
            }
            if let Some((symbol, _)) = decl.symbol(cst) {
                self.writeln(); // 换行到符号定义
                self.write_indented("= "); // 缩进并写入等号
                self.write(symbol);
            }
            self.write(";");
        } else {
            // 单行格式：标准格式，保持简洁
            self.write(&token_line);
        }
    }

    /// 格式化规则声明（<name>^? : <regex>;）
    ///
    /// 逻辑：
    /// 1. 构建规则头部（名称和省略修饰符）
    /// 2. 检查正则表达式是否需要换行
    /// 3. 基于lelwel.llw风格：优先使用单行格式
    /// 4. 只在规则异常长时才使用多行格式
    ///
    /// 设计原则：基于lelwel.llw的实际使用模式，规则声明几乎总是单行
    fn format_rule_decl(&mut self, cst: &Cst<'_>, decl: RuleDecl) {
        // 构建规则头部（名称和修饰符）
        let mut rule_header = String::new();
        if let Some((name, _)) = decl.name(cst) {
            rule_header.push_str(name);
        }

        // 添加省略修饰符（如果存在）
        if decl.is_elided(cst) {
            rule_header.push('^');
        }

        rule_header.push_str(": ");

        // 检查是否需要换行
        if let Some(regex) = decl.regex(cst) {
            let regex_width = self.estimate_regex_width(cst, &regex);
            let total_width = self.line_width + rule_header.len() + regex_width;

            // 基于lelwel.llw的实际使用模式：
            // 规则声明几乎总是单行，多行声明极为罕见
            // 只在规则异常长时才换行（超过最大行宽+60字符的缓冲）
            let should_wrap = self.should_wrap && total_width > self.max_line_width + 60;

            if should_wrap {
                // 多行格式（极为罕见的情况）
                // 逻辑：规则头部单独一行，正则表达式缩进，分号单独一行
                self.write(&rule_header);
                self.writeln();
                self.indent_level += 1;
                self.write_indented("");
                self.format_regex(cst, regex);
                self.indent_level -= 1;
                self.writeln();
                self.write(";");
            } else {
                // 单行格式（LLW文件中的标准格式）
                // 逻辑：所有内容保持在一行，符合lelwel.llw的常见模式
                self.write(&rule_header);
                self.format_regex(cst, regex);
                self.write(";");
            }
        } else {
            // 空规则声明（也是单行）
            self.write(&rule_header);
            self.write(";");
        }
    }

    /// 格式化正则表达式（外部接口）
    ///
    /// 逻辑：调用内部格式化方法，不自动添加括号
    fn format_regex(&mut self, cst: &Cst<'_>, regex: Regex) {
        self.format_regex_internal(cst, regex, false)
    }

    /// 格式化正则表达式（内部实现）
    ///
    /// 逻辑：
    /// 1. 检查是否需要添加括号（基于运算符优先级）
    /// 2. 根据正则表达式类型分发到相应的格式化方法
    /// 3. 处理所有正则表达式变体
    /// 4. 在需要时添加结束括号
    ///
    /// # 参数
    /// - `parenthesize`: 是否需要在需要时添加括号
    fn format_regex_internal(&mut self, cst: &Cst<'_>, regex: Regex, parenthesize: bool) {
        // 检查是否需要添加括号（基于运算符优先级）
        let needs_parentheses = parenthesize && self.should_parenthesize(&regex);

        if needs_parentheses {
            self.write("(");
        }

        // 根据正则表达式类型进行模式匹配
        match regex {
            Regex::Alternation(alt) => {
                // 交替操作符 |
                // 逻辑：收集所有操作数，调用操作符分隔格式化方法
                let operands: Vec<_> = alt.operands(cst).collect();
                self.format_operator_separated(cst, operands, " | ", "alternation")
            }
            Regex::OrderedChoice(choice) => {
                // 有序选择操作符 /
                // 逻辑：收集所有操作数，调用操作符分隔格式化方法
                let operands: Vec<_> = choice.operands(cst).collect();
                self.format_operator_separated(cst, operands, " / ", "ordered_choice")
            }
            Regex::Concat(concat) => {
                // 串联操作（无显式操作符）
                // 逻辑：收集所有操作数，调用串联格式化方法
                let operands: Vec<_> = concat.operands(cst).collect();
                self.format_concat(cst, operands)
            }
            Regex::Paren(paren) => {
                // 括号表达式
                // 逻辑：直接写入括号，递归格式化内部表达式
                self.write("(");
                if let Some(inner) = paren.inner(cst) {
                    self.format_regex_internal(cst, inner, false);
                }
                self.write(")");
            }
            Regex::Optional(opt) => {
                // 可选操作符 []
                // 逻辑：调用一元操作符格式化方法
                self.format_unary_operator(cst, opt.operand(cst), "[", "]")
            }
            Regex::Star(star) => {
                // 零次或多次操作符 *
                // 逻辑：调用一元操作符格式化方法
                self.format_unary_operator(cst, star.operand(cst), "", "*")
            }
            Regex::Plus(plus) => {
                // 一次或多次操作符 +
                // 逻辑：调用一元操作符格式化方法
                self.format_unary_operator(cst, plus.operand(cst), "", "+")
            }
            Regex::Name(name) => {
                // 标识符
                // 逻辑：直接写入标识符名称
                if let Some((value, _)) = name.value(cst) {
                    self.write(value);
                }
            }
            Regex::Symbol(symbol) => {
                // 字符串字面量
                // 逻辑：直接写入字符串内容
                if let Some((value, _)) = symbol.value(cst) {
                    self.write(value);
                }
            }
            Regex::Predicate(pred) => {
                // 语义谓词
                // 逻辑：直接写入谓词内容
                if let Some((value, _)) = pred.value(cst) {
                    self.write(value);
                }
            }
            Regex::Action(action) => {
                // 语义动作
                // 逻辑：直接写入动作内容
                if let Some((value, _)) = action.value(cst) {
                    self.write(value);
                }
            }
            Regex::Assertion(assertion) => {
                // 语义断言
                // 逻辑：直接写入断言内容
                if let Some((value, _)) = assertion.value(cst) {
                    self.write(value);
                }
            }
            Regex::NodeRename(rename) => {
                // 节点重命名
                // 逻辑：直接写入重命名内容
                if let Some((value, _)) = rename.value(cst) {
                    self.write(value);
                }
            }
            Regex::NodeMarker(marker) => {
                // 节点标记
                // 逻辑：直接写入标记内容
                if let Some((value, _)) = marker.value(cst) {
                    self.write(value);
                }
            }
            Regex::NodeCreation(creation) => {
                // 节点创建
                // 逻辑：直接写入创建内容
                if let Some((value, _)) = creation.value(cst) {
                    self.write(value);
                }
            }
            Regex::NodeElision(_) => {
                // 节点省略操作符 ^
                // 逻辑：直接写入省略符号
                self.write("^");
            }
            Regex::Commit(_) => {
                // 提交操作符 ~
                // 逻辑：直接写入提交符号
                self.write("~");
            }
            Regex::Return(_) => {
                // 返回操作符 &
                // 逻辑：直接写入返回符号
                self.write("&");
            }
        }

        if needs_parentheses {
            self.write(")");
        }
    }

    /// 格式化操作符分隔的表达式（如 | 和 /）
    ///
    /// 逻辑：
    /// 1. 检查操作数是否为空
    /// 2. 计算估计宽度以决定是否需要换行
    /// 3. 根据换行需求设置缩进级别
    /// 4. 遍历操作数，在操作数之间插入分隔符或换行
    /// 5. 支持操作符对齐功能
    ///
    /// 基于lelwel.llw的观察：
    /// - 操作符分隔的表达式通常保持单行
    /// - 只在表达式过长时才换行
    /// - 支持操作符对齐以提高可读性
    fn format_operator_separated(
        &mut self,
        cst: &Cst<'_>,
        operands: Vec<Regex>,
        separator: &str,
        _operator_type: &str,
    ) {
        if operands.is_empty() {
            return; // 空操作数不处理
        }

        let current_line_width = self.line_width;
        let separator_len = separator.len();

        // 计算估计宽度以决定是否需要换行
        // 逻辑：当前行宽 + 第一个操作数宽度 + 后续操作数（分隔符+宽度）的总和
        let estimated_width = current_line_width
            + self.estimate_regex_width(cst, &operands[0])
            + operands
                .iter()
                .skip(1)
                .map(|op| separator_len + self.estimate_regex_width(cst, op))
                .sum::<usize>();

        // 只在表达式过长且操作数多于1个时才换行
        let should_wrap =
            self.should_wrap && estimated_width > self.max_line_width && operands.len() > 1;

        // 操作符对齐：如果启用且需要换行，对齐操作符
        let operator_alignment = self.align_operators && should_wrap;

        if should_wrap {
            self.indent_level += 1; // 增加缩进级别用于多行格式
        }

        // 格式化每个操作数
        for (i, op) in operands.iter().enumerate() {
            if i > 0 {
                if should_wrap {
                    // 多行格式：每个操作数单独一行
                    self.writeln();
                    if operator_alignment {
                        // 对齐操作符到相同缩进级别
                        let alignment_spaces = self.indent_level * self.indent_size;
                        self.write(&" ".repeat(alignment_spaces));
                    } else {
                        self.write(&" ".repeat(self.indent_level * self.indent_size));
                    }
                } else {
                    // 单行格式：使用分隔符连接操作数
                    self.write(separator);
                }
            }
            // 格式化当前操作数，可能需要括号
            self.format_regex_internal(cst, *op, true);
        }

        if should_wrap {
            self.indent_level -= 1; // 恢复缩进级别
        }
    }

    /// 格式化串联表达式
    ///
    /// 逻辑：
    /// 1. 检查操作数是否为空
    /// 2. 尝试紧凑格式（如果启用且操作数较少）
    /// 3. 计算总宽度决定是否需要换行
    /// 4. 格式化每个操作数，在需要时换行
    ///
    /// 基于lelwel.llw的观察：
    /// - 串联表达式通常保持紧凑格式
    /// - 操作数较少时尽量保持单行
    /// - 只在必要时才换行
    fn format_concat(&mut self, cst: &Cst<'_>, operands: Vec<Regex>) {
        if operands.is_empty() {
            return; // 空操作数不处理
        }

        // 紧凑串联格式：如果启用且操作数较少（≤3个），尝试保持在一行
        // 逻辑：优先尝试紧凑格式，避免不必要的换行
        if self.compact_concat && operands.len() <= 3 {
            let total_width: usize = operands
                .iter()
                .map(|op| self.estimate_regex_width(cst, op))
                .sum();

            // 如果总宽度不超过限制，保持紧凑格式
            // 逻辑：当前行宽+总宽度不超过最大行宽时使用紧凑格式
            if self.line_width + total_width <= self.max_line_width {
                for op in operands {
                    self.format_regex_internal(cst, op, true);
                }
                return; // 紧凑格式成功，直接返回
            }
        }

        // 检查串联是否需要换行
        let total_width: usize = operands
            .iter()
            .map(|op| self.estimate_regex_width(cst, op))
            .sum();

        // 只在表达式过长且操作数多于1个时才换行
        // 逻辑：启用换行 + 总宽度超限 + 多个操作数
        let should_wrap = self.should_wrap
            && (self.line_width + total_width > self.max_line_width)
            && operands.len() > 1;

        if should_wrap {
            self.indent_level += 1; // 增加缩进级别用于多行格式
        }

        for (i, op) in operands.iter().enumerate() {
            if i > 0 && should_wrap {
                self.writeln();
                self.write_indented("");
            }
            self.format_regex_internal(cst, *op, true);
        }

        if should_wrap {
            self.indent_level -= 1; // 恢复缩进级别
        }
    }

    /// 格式化一元操作符表达式
    ///
    /// 逻辑：
    /// 1. 写入前缀（如 "[" 或 ""）
    /// 2. 格式化操作数（如果存在）
    /// 3. 写入后缀（如 "]" 或 "*"）
    ///
    /// 支持的一元操作符：[]（可选）、*（零次或多次）、+（一次或多次）
    fn format_unary_operator(
        &mut self,
        cst: &Cst<'_>,
        operand: Option<Regex>,
        prefix: &str,
        suffix: &str,
    ) {
        self.write(prefix); // 写入前缀
        if let Some(op) = operand {
            self.format_regex_internal(cst, op, true); // 格式化操作数，可能需要括号
        }
        self.write(suffix); // 写入后缀
    }

    /// 判断正则表达式是否需要括号
    ///
    /// 逻辑：
    /// 1. 检查正则表达式类型
    /// 2. 只有交替（|）和有序选择（/）操作符需要括号
    /// 3. 基于lelwel.llw的保守括号使用策略
    ///
    /// 设计原则：避免过度括号化，只在运算符优先级需要时添加括号
    fn should_parenthesize(&self, regex: &Regex) -> bool {
        // 只有交替和有序选择操作符需要括号
        // 逻辑：这些操作符优先级较低，在嵌套表达式中需要括号
        matches!(regex, Regex::Alternation(_) | Regex::OrderedChoice(_))
    }

    /// 估计正则表达式的宽度（用于换行决策）
    ///
    /// 逻辑：
    /// 1. 根据正则表达式类型返回保守的宽度估计
    /// 2. 用于决定是否需要换行
    /// 3. 这是一个近似值，不是精确计算
    ///
    /// 返回值：表达式的估计字符宽度
    fn estimate_regex_width(&self, cst: &Cst<'_>, regex: &Regex) -> usize {
        match regex {
            Regex::Name(name) => name.value(cst).map(|(v, _)| v.len()).unwrap_or(0), // 标识符的实际长度
            Regex::Symbol(symbol) => symbol.value(cst).map(|(v, _)| v.len()).unwrap_or(0), // 字符串字面量的实际长度
            Regex::Paren(_) => 2,                 // 括号的最小宽度（()）
            Regex::Optional(_) => 2,              // 方括号的最小宽度（[]）
            Regex::Star(_) | Regex::Plus(_) => 1, // 单字符操作符（* +）
            _ => 10,                              // 复杂表达式的保守估计（默认值）
        }
    }

    /// 写入文本到输出缓冲区
    ///
    /// 逻辑：
    /// 1. 将文本追加到输出缓冲区
    /// 2. 更新当前行宽度计数器
    ///
    /// 注意：不处理换行或缩进，只处理纯文本内容
    fn write(&mut self, text: &str) {
        self.output.push_str(text);
        self.line_width += text.len();
    }

    /// 写入换行并应用当前缩进
    ///
    /// 逻辑：
    /// 1. 添加换行符到输出缓冲区
    /// 2. 根据当前缩进级别添加相应数量的空格
    /// 3. 更新当前行宽度为缩进宽度
    fn writeln(&mut self) {
        self.output.push('\n');
        self.output
            .push_str(&" ".repeat(self.indent_level * self.indent_size));
        self.line_width = self.indent_level * self.indent_size;
    }

    /// 写入带缩进的文本
    ///
    /// 逻辑：
    /// 1. 如果当前行没有内容，先应用缩进
    /// 2. 然后写入文本内容
    ///
    /// 用途：确保文本在正确的位置开始，保持缩进一致性
    fn write_indented(&mut self, text: &str) {
        if self.line_width == 0 {
            self.output
                .push_str(&" ".repeat(self.indent_level * self.indent_size));
            self.line_width = self.indent_level * self.indent_size;
        }
        self.write(text);
    }
}

/// 声明类型的枚举，用于统一处理不同类型的声明
///
/// 逻辑：将各种声明类型封装在统一的枚举中，便于迭代和处理
enum Decl {
    Start(StartDecl),
    Right(RightDecl),
    Skip(SkipDecl),
    Part(PartDecl),
    Token(TokenDecl),
    Rule(RuleDecl),
}

/// 格式化LLW文件（不保留注释）
///
/// 这是主要的格式化入口点，用于生成格式化的LLW语法文件。
///
/// 逻辑：
/// 1. 检查语法树是否包含有效的文件节点
/// 2. 创建格式化器实例
/// 3. 调用文件格式化方法
/// 4. 返回格式化后的字符串或None（如果格式化失败）
///
/// # 参数
/// - `cst`: 语法树，包含要格式化的LLW文件内容
///
/// # 返回
/// 格式化后的字符串，如果语法树无效或格式化失败返回None
///
/// # 示例
/// ```rust, ignore
/// let cst = parser.parse(source);
/// if let Some(formatted) = format_llw(&cst) {
///     println!("{}", formatted);
/// }
/// ```
pub fn format_llw(cst: &Cst<'_>) -> Option<String> {
    if let Some(file) = File::cast(cst, NodeRef::ROOT) {
        let mut formatter = LlwFormatter::new();
        Some(formatter.format_file(cst, file))
    } else {
        None // 无效的语法树，无法格式化
    }
}

/// 格式化LLW文件并保留注释
///
/// 这是保留注释的格式化入口点，用于生成格式化的LLW语法文件同时保留原始注释。
///
/// 逻辑：
/// 1. 检查语法树是否包含有效的文件节点
/// 2. 创建格式化器实例
/// 3. 调用带注释的文件格式化方法
/// 4. 返回格式化后的字符串或None（如果格式化失败）
///
/// # 参数
/// - `source`: 原始源代码，用于提取注释内容
/// - `cst`: 语法树，包含要格式化的LLW文件结构
///
/// # 返回
/// 格式化后的字符串（保留注释），如果语法树无效或格式化失败返回None
///
/// # 示例
/// ```rust, ignore
/// let source = "// 这是一个注释\ntoken A = 'a';";
/// let cst = parser.parse(source);
/// if let Some(formatted) = format_llw_with_comments(source, &cst) {
///     println!("{}", formatted);
/// }
/// ```
pub fn format_llw_with_comments(source: &str, cst: &Cst<'_>) -> Option<String> {
    if let Some(file) = File::cast(cst, NodeRef::ROOT) {
        let mut formatter = LlwFormatter::new();
        Some(formatter.format_file_with_comments(source, cst, file))
    } else {
        None // 无效的语法树，无法格式化
    }
}

/// 格式化配置结构体
#[derive(Debug, Clone, Default)]
pub struct FormatConfig {
    /// 是否保留注释
    pub preserve_comments: Option<bool>,
    /// 最大行宽限制
    pub max_line_width: Option<usize>,
    /// 缩进大小
    pub indent_size: Option<usize>,
    /// 是否启用自动换行
    pub enable_wrapping: Option<bool>,
    /// 是否启用紧凑串联格式
    pub compact_concat: Option<bool>,
    /// 是否对齐操作符
    pub align_operators: Option<bool>,
}

/// 配置化的LLW文件格式化函数
///
/// 这是主要的格式化入口点，支持配置选项和注释保留选择。
///
/// 逻辑：
/// 1. 检查语法树是否包含有效的文件节点
/// 2. 根据配置选项创建格式化器
/// 3. 根据preserve_comments选择格式化方法
/// 4. 返回格式化后的字符串或None
///
/// # 参数
/// - `source`: 原始源代码（用于带注释的格式化）
/// - `cst`: 语法树，包含要格式化的LLW文件结构
/// - `config`: 格式化配置选项
///
/// # 返回
/// 格式化后的字符串，如果语法树无效或格式化失败返回None
///
/// # 示例
/// ```rust, ignore
/// let source = "token A = 'a';";
/// let cst = parser.parse(source);
/// let config = FormatConfig {
///     preserve_comments: false,
///     max_line_width: Some(100),
///     indent_size: Some(4),
///     enable_wrapping: Some(true),
///     compact_concat: Some(false),
///     align_operators: Some(true),
/// };
/// let formatted = format_llw_with_config(source, &cst, config);
/// ```
pub fn format_llw_with_config(source: &str, cst: &Cst<'_>, config: FormatConfig) -> Option<String> {
    if let Some(file) = File::cast(cst, NodeRef::ROOT) {
        // 创建基础格式化器
        let mut formatter = LlwFormatter::new();

        // 应用配置选项
        if let Some(width) = config.max_line_width {
            formatter = formatter.with_max_line_width(width);
        }
        if let Some(size) = config.indent_size {
            formatter = formatter.with_indent_size(size);
        }
        if let Some(wrapping) = config.enable_wrapping {
            formatter = formatter.with_wrapping(wrapping);
        }
        if let Some(compact) = config.compact_concat {
            formatter = formatter.with_compact_concat(compact);
        }
        if let Some(align) = config.align_operators {
            formatter = formatter.with_align_operators(align);
        }

        // 根据注释保留选项选择格式化方法
        // 使用更简洁的条件表达式
        Some(if config.preserve_comments.unwrap_or(false) {
            formatter.format_file_with_comments(source, cst, file)
        } else {
            formatter.format_file(cst, file)
        })
    } else {
        None // 无效的语法树，无法格式化
    }
}

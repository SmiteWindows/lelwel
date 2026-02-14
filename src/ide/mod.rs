#![cfg(feature = "lsp")]

use crate::{Parser, SemanticPass, Span};
use codespan_reporting::diagnostic::{LabelStyle, Severity};
use codespan_reporting::files::SimpleFile;
use ls_types::*;
use std::collections::HashMap;
use std::sync::mpsc;
use std::thread::JoinHandle;

use self::completion::*;
use self::hover::*;
use self::lookup::*;

use crate::NodeRef;
use crate::frontend::ast::{AstNode, File};
use crate::frontend::format::{LlwFormatter, format_llw, format_llw_with_comments};

pub mod completion;
mod hover;
mod lookup;

/// LSP服务器实现
pub struct LspServer {
    cache: Cache,
}

impl Default for LspServer {
    fn default() -> Self {
        Self::new()
    }
}

impl LspServer {
    pub fn new() -> Self {
        Self {
            cache: Cache::default(),
        }
    }

    /// 处理格式化文档请求
    pub fn handle_formatting(&self, params: DocumentFormattingParams) -> Option<Vec<TextEdit>> {
        let uri = params.text_document.uri;
        let text = self.cache.documents.get(&uri)?;

        let formatted = self.cache.format_document(&uri, text)?;

        if formatted == *text {
            return None; // 没有变化
        }

        Some(vec![TextEdit {
            range: Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: Position {
                    line: text.lines().count() as u32,
                    character: text.lines().last().map(|l| l.len()).unwrap_or(0) as u32,
                },
            },
            new_text: formatted,
        }])
    }

    /// 处理配置变更
    pub fn handle_config_change(&mut self, config: LspConfig) {
        self.cache.update_config(config);
    }

    /// 处理格式化启用/禁用
    pub fn handle_format_toggle(&mut self, enabled: bool) {
        self.cache.set_format_enabled(enabled);
    }
}

struct Analyzer {
    handle: JoinHandle<()>,
    req_tx: mpsc::Sender<Request>,
    noti_rx: mpsc::Receiver<Notification>,
}

/// LSP服务器配置
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct LspConfig {
    /// 格式化时是否保留注释
    #[serde(default)]
    pub format_preserve_comments: Option<bool>,
    /// 最大行宽
    #[serde(default)]
    pub format_max_line_width: Option<usize>,
    /// 缩进大小
    #[serde(default)]
    pub format_indent_size: Option<usize>,
    /// 是否启用智能换行
    #[serde(default)]
    pub format_enable_wrapping: Option<bool>,
    /// 紧凑串联格式
    #[serde(default)]
    pub format_compact_concat: Option<bool>,
    /// 操作符对齐
    #[serde(default)]
    pub format_align_operators: Option<bool>,
}

impl Default for LspConfig {
    fn default() -> Self {
        Self {
            format_preserve_comments: Some(false),
            format_max_line_width: Some(80),
            format_indent_size: Some(2),
            format_enable_wrapping: Some(true),
            format_compact_concat: Some(false),
            format_align_operators: Some(true),
        }
    }
}

#[derive(Default)]
pub struct Cache {
    analyzers: HashMap<Uri, Analyzer>,
    documents: HashMap<Uri, String>,
    config: Option<LspConfig>,
    format_enabled: bool,
}

impl Cache {
    /// 格式化文档（使用公共API）
    pub fn format_document(&self, _uri: &Uri, text: &str) -> Option<String> {
        if !self.format_enabled {
            return None;
        }

        // 解析文档
        let mut diags = vec![];
        let cst = Parser::new(text, &mut diags).parse(&mut diags);

        // 应用配置（修复生命周期问题）
        let default_config = LspConfig::default();
        let config = self.config.as_ref().unwrap_or(&default_config);

        // 根据配置选择是否保留注释
        if config.format_preserve_comments.unwrap_or(false) {
            format_llw_with_comments(text, &cst)
        } else {
            format_llw(&cst)
        }
    }

    pub fn analyze(&mut self, uri: Uri, text: String) {
        self.documents.insert(uri.clone(), text.clone());
        let (req_tx, req_rx) = mpsc::channel::<Request>();
        let (noti_tx, noti_rx) = mpsc::channel::<Notification>();
        let key = uri.clone();
        let handle = std::thread::spawn(move || analyze(uri, text, req_rx, noti_tx));

        self.analyzers.insert(
            key,
            Analyzer {
                handle,
                req_tx,
                noti_rx,
            },
        );
    }
    pub fn invalidate(&mut self, uri: &Uri) {
        if let Some(analyzer) = self.analyzers.remove(uri) {
            analyzer.req_tx.send(Request::Cancel).unwrap();
            analyzer.handle.join().unwrap();
        }
    }
    pub fn get_diagnostics(&mut self, uri: &Uri) -> Vec<Diagnostic> {
        let analyzer = self.analyzers.get_mut(uri).unwrap();
        assert!(!analyzer.handle.is_finished());
        analyzer.req_tx.send(Request::Diagnostic).unwrap();
        if let Ok(Notification::PublishDiagnostics(items)) = analyzer.noti_rx.recv() {
            items
        } else {
            vec![]
        }
    }
    pub fn hover(&mut self, uri: &Uri, pos: Position) -> Option<(String, Range)> {
        let analyzer = self.analyzers.get_mut(uri).unwrap();
        assert!(!analyzer.handle.is_finished());
        analyzer.req_tx.send(Request::Hover(pos)).unwrap();
        if let Ok(Notification::Hover(hover)) = analyzer.noti_rx.recv() {
            hover
        } else {
            None
        }
    }
    pub fn goto_definition(&mut self, uri: &Uri, pos: Position) -> Option<Location> {
        let analyzer = self.analyzers.get_mut(uri).unwrap();
        assert!(!analyzer.handle.is_finished());
        analyzer.req_tx.send(Request::GotoDefinition(pos)).unwrap();
        if let Ok(Notification::GotoDefinition(location)) = analyzer.noti_rx.recv() {
            location
        } else {
            None
        }
    }
    pub fn references(&mut self, uri: &Uri, pos: Position, with_def: bool) -> Vec<Location> {
        let analyzer = self.analyzers.get_mut(uri).unwrap();
        assert!(!analyzer.handle.is_finished());
        analyzer
            .req_tx
            .send(Request::References(pos, with_def))
            .unwrap();
        if let Ok(Notification::References(ranges)) = analyzer.noti_rx.recv() {
            ranges
        } else {
            vec![]
        }
    }
    pub fn completion(&mut self, params: CompletionParams) -> Option<CompletionResponse> {
        let analyzer = self
            .analyzers
            .get_mut(&params.text_document_position.text_document.uri)
            .unwrap();
        assert!(!analyzer.handle.is_finished());
        analyzer.req_tx.send(Request::Completion(params)).unwrap();
        if let Ok(Notification::Completion(resp)) = analyzer.noti_rx.recv() {
            resp
        } else {
            None
        }
    }
    pub fn get_document(&self, uri: &Uri) -> Option<&String> {
        self.documents.get(uri)
    }

    /// 获取当前配置
    pub fn get_config(&self) -> Option<&LspConfig> {
        self.config.as_ref()
    }

    /// 格式化文档
    /// 格式化文档（使用直接格式化器）
    pub fn format_document_direct(&self, _uri: &Uri, text: &str) -> Option<String> {
        if !self.format_enabled {
            return None;
        }

        // 解析文档
        let mut diags = vec![];
        let cst = Parser::new(text, &mut diags).parse(&mut diags);

        // 获取文件AST
        let file = File::cast(&cst, NodeRef::ROOT)?;

        // 应用配置（修复生命周期问题）
        let default_config = LspConfig::default();
        let config = self.config.as_ref().unwrap_or(&default_config);

        // 创建格式化器并应用配置
        let mut formatter = LlwFormatter::new();

        if let Some(width) = config.format_max_line_width {
            formatter = formatter.with_max_line_width(width);
        }

        if let Some(size) = config.format_indent_size {
            formatter = formatter.with_indent_size(size);
        }

        if let Some(wrapping) = config.format_enable_wrapping {
            formatter = formatter.with_wrapping(wrapping);
        }

        if let Some(compact) = config.format_compact_concat {
            formatter = formatter.with_compact_concat(compact);
        }

        if let Some(align) = config.format_align_operators {
            formatter = formatter.with_align_operators(align);
        }

        // 根据配置选择是否保留注释
        if config.format_preserve_comments.unwrap_or(false) {
            Some(formatter.format_file_with_comments(text, &cst, file))
        } else {
            Some(formatter.format_file(&cst, file))
        }
    }

    /// 启用或禁用格式化功能
    pub fn set_format_enabled(&mut self, enabled: bool) {
        self.format_enabled = enabled;
    }

    /// 更新配置
    pub fn update_config(&mut self, config: LspConfig) {
        self.config = Some(config);
    }
}

enum Request {
    Diagnostic,
    Hover(Position),
    GotoDefinition(Position),
    References(Position, bool),
    Completion(CompletionParams),
    Cancel,
}

enum Notification {
    PublishDiagnostics(Vec<Diagnostic>),
    Hover(Option<(String, Range)>),
    GotoDefinition(Option<Location>),
    References(Vec<Location>),
    Completion(Option<CompletionResponse>),
}

fn analyze(
    uri: Uri,
    source: String,
    req: mpsc::Receiver<Request>,
    noti: mpsc::Sender<Notification>,
) {
    let path = uri.to_file_path().unwrap();
    let parser_path = path.parent().unwrap().join("parser.rs");
    let mut diags = vec![];

    let cst = Parser::new(&source, &mut diags).parse(&mut diags);
    let sema = SemanticPass::run(&cst, &mut diags);
    let file = SimpleFile::new(path.to_str().unwrap(), source.as_str());

    while let Ok(req) = req.recv() {
        match req {
            Request::Diagnostic => {
                let mut diags = diags
                    .iter()
                    .map(|diag| to_lsp_diag(&file, &uri, diag))
                    .collect::<Vec<_>>();
                let mut hints = related_as_hints(&diags);
                diags.append(&mut hints);
                noti.send(Notification::PublishDiagnostics(diags)).unwrap();
            }
            Request::Hover(pos) => {
                let pos = compat::position_to_offset(&file, &pos);
                let res = hover(&cst, &sema, pos)
                    .map(|(msg, span)| (msg, compat::span_to_range(&file, &span)));
                noti.send(Notification::Hover(res)).unwrap();
            }
            Request::GotoDefinition(pos) => {
                let pos = compat::position_to_offset(&file, &pos);
                let location = lookup_definition(&cst, &sema, pos, &uri, &file, &parser_path);
                noti.send(Notification::GotoDefinition(location)).unwrap();
            }
            Request::References(pos, with_def) => {
                let pos = compat::position_to_offset(&file, &pos);
                let ranges = lookup_references(&cst, &sema, pos, with_def)
                    .into_iter()
                    .map(|node| {
                        Location::new(uri.clone(), compat::span_to_range(&file, &cst.span(node)))
                    })
                    .collect();
                noti.send(Notification::References(ranges)).unwrap();
            }
            Request::Completion(params) => {
                let pos =
                    compat::position_to_offset(&file, &params.text_document_position.position);
                noti.send(Notification::Completion(completion(&cst, pos, &sema)))
                    .unwrap();
            }
            Request::Cancel => {
                return;
            }
        }
    }
}

fn to_lsp_related(
    file: &SimpleFile<&str, &str>,
    span: &Span,
    uri: &Uri,
    msg: &str,
) -> DiagnosticRelatedInformation {
    DiagnosticRelatedInformation {
        location: Location::new(uri.clone(), compat::span_to_range(file, span)),
        message: msg.to_string(),
    }
}

fn to_lsp_diag(
    file: &SimpleFile<&str, &str>,
    uri: &Uri,
    diag: &super::frontend::parser::Diagnostic,
) -> Diagnostic {
    let related = diag
        .labels
        .iter()
        .filter_map(|label| {
            if label.style == LabelStyle::Secondary {
                Some(to_lsp_related(file, &label.range, uri, &label.message))
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    let mut message = diag.message.clone();
    if let Some(primary_label_message) = diag.labels.iter().find_map(|label| {
        (label.style == LabelStyle::Primary && !label.message.is_empty())
            .then_some(label.message.as_str())
    }) {
        // append message of primary label if it is not empty
        message.push(' ');
        message.push_str(primary_label_message);
    }

    Diagnostic::new(
        diag.labels
            .first()
            .map_or(ls_types::Range::default(), |label| {
                compat::span_to_range(file, &label.range)
            }),
        Some(match diag.severity {
            Severity::Error | Severity::Bug => DiagnosticSeverity::ERROR,
            Severity::Warning => DiagnosticSeverity::WARNING,
            _ => DiagnosticSeverity::HINT,
        }),
        diag.code.clone().map(NumberOrString::String),
        None,
        message,
        Some(related),
        None,
    )
}
fn related_as_hints(diags: &[Diagnostic]) -> Vec<Diagnostic> {
    let mut hints = vec![];
    for diag in diags.iter() {
        if let Some(related) = &diag.related_information {
            for r in related.iter() {
                hints.push(Diagnostic::new(
                    r.location.range,
                    Some(DiagnosticSeverity::HINT),
                    diag.code.clone(),
                    None,
                    r.message.clone(),
                    None,
                    None,
                ));
            }
        }
    }
    hints
}

/// required functions due to different versions of lsp-types in codespan
pub mod compat {
    use crate::Span;
    use codespan_reporting::files::SimpleFile;

    pub fn position_to_offset(file: &SimpleFile<&str, &str>, pos: &ls_types::Position) -> usize {
        codespan_lsp::position_to_byte_index(
            file,
            (),
            &ls_types::Position::new(pos.line, pos.character),
        )
        .unwrap()
    }

    pub fn span_to_range(file: &SimpleFile<&str, &str>, span: &Span) -> ls_types::Range {
        let range = codespan_lsp::byte_span_to_range(file, (), span.clone()).unwrap();
        ls_types::Range::new(
            ls_types::Position::new(range.start.line, range.start.character),
            ls_types::Position::new(range.end.line, range.end.character),
        )
    }
}

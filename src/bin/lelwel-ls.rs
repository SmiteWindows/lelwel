#![cfg(feature = "lsp")]

use std::error::Error;

use lelwel::frontend::ast::{AstNode, File};
use lelwel::frontend::parser::{NodeRef, Parser};
use lelwel::frontend::sema::SemanticPass;
use lelwel::ide::Cache;
use lelwel::ide::completion;
use ls_types::{
    CompletionOptions, GotoDefinitionResponse, Hover, HoverContents, HoverProviderCapability,
    InitializeParams, MarkupContent, MarkupKind, OneOf, PublishDiagnosticsParams,
    ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind,
    notification::{
        DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, PublishDiagnostics,
    },
    request::{Completion, Formatting, GotoDefinition, HoverRequest, References},
};
use lsp_server::{Connection, ExtractError, Message, Notification, RequestId, Response};

macro_rules! request_match {
    ( $req_ty:ty, $cache:expr, $connection:expr, $req:expr ) => {
        match cast_request::<$req_ty>($req) {
            Ok((id, params)) => {
                let result = <$req_ty>::handle(&mut $cache, params);
                let result = serde_json::to_value(&result).unwrap();
                let resp = Response::new_ok(id, result);
                $connection.sender.send(Message::Response(resp))?;
                continue;
            }
            Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
            Err(ExtractError::MethodMismatch(req)) => req,
        }
    };
}

macro_rules! notification_match {
    ( $noti_ty:ty, $cache:expr, $connection:expr, $noti:expr ) => {
        match cast_notification::<$noti_ty>($noti) {
            Ok(params) => {
                if let Some(resp) = <$noti_ty>::handle(&mut $cache, params) {
                    $connection.sender.send(Message::Notification(resp))?;
                }
                continue;
            }
            Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
            Err(ExtractError::MethodMismatch(req)) => req,
        }
    };
}

trait RequestHandler: ls_types::request::Request {
    fn handle(cache: &mut Cache, params: Self::Params) -> Self::Result;
}

impl RequestHandler for Formatting {
    fn handle(cache: &mut Cache, params: Self::Params) -> Self::Result {
        use ls_types::{TextDocumentIdentifier, TextEdit};

        let TextDocumentIdentifier { uri } = params.text_document;

        // Get document content
        let document = cache.get_document(&uri)?;

        // Parse document
        let mut diags = vec![];
        let cst = lelwel::frontend::parser::Parser::new(document, &mut diags).parse(&mut diags);

        // Get file AST
        let file = match File::cast(&cst, NodeRef::ROOT) {
            Some(file) => file,
            None => return Some(vec![]), // Unable to parse, return empty edits
        };

        // Apply configuration (fix lifetime issues)
        let default_config = lelwel::ide::LspConfig::default();
        let config = cache.get_config().unwrap_or(&default_config);

        let mut formatter = lelwel::frontend::format::LlwFormatter::new();

        // Apply configuration options
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

        if let Some(compact) = config.format_compact_concat {
            formatter = formatter.with_compact_concat(compact);
        }

        if let Some(align) = config.format_align_operators {
            formatter = formatter.with_align_operators(align);
        }

        // Format document (choose whether to preserve comments based on configuration)
        let formatted_text = if config.format_preserve_comments.unwrap_or(false) {
            // Use comment-preserving formatting
            formatter.format_file_with_comments(document, &cst, file)
        } else {
            // Use basic formatting
            formatter.format_file(&cst, file)
        };

        // If formatted text is same as original, return empty edits
        if formatted_text == *document {
            return Some(vec![]);
        }

        // Create text edit to replace entire document
        let text_edit = TextEdit {
            range: ls_types::Range {
                start: ls_types::Position {
                    line: 0,
                    character: 0,
                },
                end: ls_types::Position {
                    line: document.lines().count() as u32,
                    character: 0,
                },
            },
            new_text: formatted_text,
        };

        Some(vec![text_edit])
    }
}

trait NotificationHandler: ls_types::notification::Notification {
    fn handle(cache: &mut Cache, params: Self::Params) -> Option<Notification>;
}

impl NotificationHandler for ls_types::notification::DidChangeConfiguration {
    fn handle(cache: &mut Cache, params: Self::Params) -> Option<Notification> {
        // Handle configuration changes
        if let Some(settings) = params.settings.get("lelwel")
            && let Ok(config) = serde_json::from_value::<lelwel::ide::LspConfig>(settings.clone())
        {
            cache.update_config(config);
        }
        None
    }
}

fn cast_request<R>(
    req: lsp_server::Request,
) -> Result<(RequestId, R::Params), ExtractError<lsp_server::Request>>
where
    R: ls_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}
fn cast_notification<N>(noti: Notification) -> Result<N::Params, ExtractError<Notification>>
where
    N: ls_types::notification::Notification,
    N::Params: serde::de::DeserializeOwned,
{
    noti.extract(N::METHOD)
}

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    eprintln!("Starting lelwel LSP server");

    let (connection, io_threads) = Connection::stdio();

    let server_capabilities = serde_json::to_value(&ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        completion_provider: Some(CompletionOptions::default()),
        definition_provider: Some(OneOf::Left(true)),
        references_provider: Some(OneOf::Left(true)),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        document_formatting_provider: Some(OneOf::Left(true)),
        ..Default::default()
    })
    .unwrap();
    let initialization_params = connection.initialize(server_capabilities)?;
    main_loop(connection, initialization_params)?;
    io_threads.join()?;

    eprintln!("Shutting down server");
    Ok(())
}

fn main_loop(
    connection: Connection,
    params: serde_json::Value,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let mut cache = lelwel::ide::Cache::default();
    let _params: InitializeParams = serde_json::from_value(params).unwrap();
    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                request_match!(HoverRequest, cache, connection, req.clone());
                request_match!(GotoDefinition, cache, connection, req.clone());
                request_match!(References, cache, connection, req.clone());
                request_match!(Completion, cache, connection, req.clone());
                request_match!(Formatting, cache, connection, req.clone());
            }
            Message::Response(_resp) => {}
            Message::Notification(noti) => {
                notification_match!(DidChangeTextDocument, cache, connection, noti.clone());
                notification_match!(DidOpenTextDocument, cache, connection, noti.clone());
                notification_match!(DidCloseTextDocument, cache, connection, noti.clone());
            }
        }
    }
    Ok(())
}

impl RequestHandler for HoverRequest {
    fn handle(cache: &mut Cache, params: Self::Params) -> Self::Result {
        let uri = params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        if let Some((msg, range)) = cache.hover(&uri, pos) {
            Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: msg,
                }),
                range: Some(range),
            })
        } else {
            None
        }
    }
}

impl RequestHandler for GotoDefinition {
    fn handle(cache: &mut Cache, params: Self::Params) -> Self::Result {
        let uri = params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        cache
            .goto_definition(&uri, pos)
            .map(GotoDefinitionResponse::Scalar)
    }
}

impl RequestHandler for References {
    fn handle(cache: &mut Cache, params: Self::Params) -> Self::Result {
        let uri = params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;
        let with_decl = params.context.include_declaration;
        let locs = cache.references(&uri, pos, with_decl);
        Some(locs)
    }
}

impl RequestHandler for Completion {
    fn handle(cache: &mut Cache, params: Self::Params) -> Self::Result {
        let uri = params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;

        // Get document content using public API
        let document = cache.get_document(&uri)?;

        // Create SimpleFile for position conversion
        use codespan_reporting::files::SimpleFile;
        let file = SimpleFile::new(uri.as_str(), document.as_str());

        // Use existing position conversion function
        let offset = lelwel::ide::compat::position_to_offset(&file, &pos);

        let mut diags = vec![];
        let cst = Parser::new(document, &mut diags).parse(&mut diags);

        // Use correct semantic analysis API
        let sema = SemanticPass::run(&cst, &mut diags);

        // Use enhanced completion function with formatting suggestions
        Some(completion::enhanced_completion(&cst, offset, &sema))
    }
}

impl NotificationHandler for DidOpenTextDocument {
    fn handle(cache: &mut Cache, params: Self::Params) -> Option<Notification> {
        let uri = params.text_document.uri;
        eprintln!("opened document {uri:?}");
        let text = params.text_document.text;
        let diagnostics = {
            cache.invalidate(&uri);
            cache.analyze(uri.clone(), text);
            cache.get_diagnostics(&uri)
        };
        let result = PublishDiagnosticsParams::new(uri, diagnostics, None);
        let params = serde_json::to_value(&result).unwrap();
        let method =
            <PublishDiagnostics as ls_types::notification::Notification>::METHOD.to_string();
        Some(Notification { method, params })
    }
}

impl NotificationHandler for DidChangeTextDocument {
    fn handle(cache: &mut Cache, params: Self::Params) -> Option<Notification> {
        let uri = params.text_document.uri;
        let text = params.content_changes.into_iter().next().unwrap().text;
        let diagnostics = {
            cache.invalidate(&uri);
            cache.analyze(uri.clone(), text);
            cache.get_diagnostics(&uri)
        };
        let result = PublishDiagnosticsParams::new(uri, diagnostics, None);
        let params = serde_json::to_value(&result).unwrap();
        let method =
            <PublishDiagnostics as ls_types::notification::Notification>::METHOD.to_string();
        Some(Notification { method, params })
    }
}

impl NotificationHandler for DidCloseTextDocument {
    fn handle(cache: &mut Cache, params: Self::Params) -> Option<Notification> {
        let uri = params.text_document.uri;
        eprintln!("closed document {uri:?}");
        cache.invalidate(&uri);
        None
    }
}

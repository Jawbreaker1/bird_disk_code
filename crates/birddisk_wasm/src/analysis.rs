use birddisk_core::ast::{Expr, ExprKind, Function, Program, Stmt, Type};

fn all_functions<'a>(program: &'a Program) -> Vec<&'a Function> {
    let mut functions = Vec::new();
    functions.extend(program.functions.iter());
    for book in &program.books {
        functions.extend(book.methods.iter());
    }
    functions
}

pub(crate) fn program_uses_objects(program: &Program) -> bool {
    for func in all_functions(program) {
        if matches!(func.return_type, Type::Book(_)) {
            return true;
        }
        for param in &func.params {
            if matches!(param.ty, Type::Book(_)) {
                return true;
            }
        }
        for stmt in &func.body {
            if stmt_has_object(stmt) {
                return true;
            }
        }
    }
    false
}

pub(crate) fn program_uses_arrays(program: &Program) -> bool {
    for func in all_functions(program) {
        if type_has_array(&func.return_type) {
            return true;
        }
        for param in &func.params {
            if type_has_array(&param.ty) {
                return true;
            }
        }
        for stmt in &func.body {
            if stmt_has_array(stmt) {
                return true;
            }
        }
    }
    false
}

pub(crate) fn program_uses_strings(program: &Program) -> bool {
    for func in all_functions(program) {
        if type_has_string(&func.return_type) {
            return true;
        }
        for param in &func.params {
            if type_has_string(&param.ty) {
                return true;
            }
        }
        for stmt in &func.body {
            if stmt_has_string(stmt) {
                return true;
            }
        }
    }
    false
}

pub(crate) fn program_uses_string_from_bytes(program: &Program) -> bool {
    for func in all_functions(program) {
        for stmt in &func.body {
            if stmt_has_string_from_bytes(stmt) {
                return true;
            }
        }
    }
    false
}

pub(crate) fn program_uses_bytes(program: &Program) -> bool {
    for func in all_functions(program) {
        for stmt in &func.body {
            if stmt_has_bytes(stmt) {
                return true;
            }
        }
    }
    false
}

pub(crate) fn program_uses_io(program: &Program) -> bool {
    for func in all_functions(program) {
        for stmt in &func.body {
            if stmt_has_io(stmt) {
                return true;
            }
        }
    }
    false
}

pub(crate) fn program_uses_time(program: &Program) -> bool {
    for func in all_functions(program) {
        for stmt in &func.body {
            if stmt_has_time(stmt) {
                return true;
            }
        }
    }
    false
}

pub(crate) fn program_uses_fs(program: &Program) -> bool {
    for func in all_functions(program) {
        for stmt in &func.body {
            if stmt_has_fs(stmt) {
                return true;
            }
        }
    }
    false
}

pub(crate) fn program_uses_path(program: &Program) -> bool {
    for func in all_functions(program) {
        for stmt in &func.body {
            if stmt_has_path(stmt) {
                return true;
            }
        }
    }
    false
}

pub(crate) fn program_uses_env(program: &Program) -> bool {
    for func in all_functions(program) {
        for stmt in &func.body {
            if stmt_has_env(stmt) {
                return true;
            }
        }
    }
    false
}

pub(crate) fn program_uses_json(program: &Program) -> bool {
    for func in all_functions(program) {
        for stmt in &func.body {
            if stmt_has_json(stmt) {
                return true;
            }
        }
    }
    false
}

fn type_has_array(ty: &Type) -> bool {
    match ty {
        Type::Array(_) => true,
        _ => false,
    }
}

fn type_has_string(ty: &Type) -> bool {
    match ty {
        Type::String => true,
        Type::Array(inner) => type_has_string(inner),
        Type::Book(_) => false,
        _ => false,
    }
}

fn stmt_has_array(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Set { ty, expr, .. } => {
            ty.as_ref().map_or(false, type_has_array) || expr_has_array(expr)
        }
        Stmt::Expr { expr, .. } => expr_has_array(expr),
        Stmt::Put { expr, .. } => expr_has_array(expr),
        Stmt::PutIndex { .. } => true,
        Stmt::PutField { expr, .. } => expr_has_array(expr),
        Stmt::Yield { expr, .. } => expr_has_array(expr),
        Stmt::Throw { expr, .. } => expr_has_array(expr),
        Stmt::Try {
            try_body,
            catch_body,
            ..
        } => try_body.iter().any(stmt_has_array) || catch_body.iter().any(stmt_has_array),
        Stmt::When {
            cond,
            then_body,
            else_body,
            ..
        } => {
            expr_has_array(cond)
                || then_body.iter().any(stmt_has_array)
                || else_body.iter().any(stmt_has_array)
        }
        Stmt::Repeat { cond, body, .. } => {
            expr_has_array(cond) || body.iter().any(stmt_has_array)
        }
    }
}

fn expr_has_array(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::ArrayLit(_) => true,
        ExprKind::ArrayNew { .. } => true,
        ExprKind::Index { base, index } => expr_has_array(base) || expr_has_array(index),
        ExprKind::Call { args, .. } => args.iter().any(expr_has_array),
        ExprKind::New { args, .. } => args.iter().any(expr_has_array),
        ExprKind::Unary { expr, .. } => expr_has_array(expr),
        ExprKind::Binary { left, right, .. } => {
            expr_has_array(left) || expr_has_array(right)
        }
        _ => false,
    }
}

fn stmt_has_string(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Set { ty, expr, .. } => {
            ty.as_ref().map_or(false, type_has_string) || expr_has_string(expr)
        }
        Stmt::Expr { expr, .. } => expr_has_string(expr),
        Stmt::Put { expr, .. } => expr_has_string(expr),
        Stmt::PutIndex { index, expr, .. } => {
            expr_has_string(index) || expr_has_string(expr)
        }
        Stmt::PutField { expr, .. } => expr_has_string(expr),
        Stmt::Yield { expr, .. } => expr_has_string(expr),
        Stmt::Throw { expr, .. } => expr_has_string(expr),
        Stmt::Try {
            try_body,
            catch_body,
            ..
        } => try_body.iter().any(stmt_has_string) || catch_body.iter().any(stmt_has_string),
        Stmt::When {
            cond,
            then_body,
            else_body,
            ..
        } => {
            expr_has_string(cond)
                || then_body.iter().any(stmt_has_string)
                || else_body.iter().any(stmt_has_string)
        }
        Stmt::Repeat { cond, body, .. } => {
            expr_has_string(cond) || body.iter().any(stmt_has_string)
        }
    }
}

fn expr_has_string(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::String(_) => true,
        ExprKind::Call { name, args } => {
            name.starts_with("std::string::")
                || name.starts_with("std::json::")
                || args.iter().any(expr_has_string)
        }
        ExprKind::New { args, .. } => args.iter().any(expr_has_string),
        ExprKind::ArrayLit(elements) => elements.iter().any(expr_has_string),
        ExprKind::ArrayNew { len } => expr_has_string(len),
        ExprKind::Index { base, index } => expr_has_string(base) || expr_has_string(index),
        ExprKind::Unary { expr, .. } => expr_has_string(expr),
        ExprKind::Binary { left, right, .. } => {
            expr_has_string(left) || expr_has_string(right)
        }
        _ => false,
    }
}

fn stmt_has_json(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Set { expr, .. } => expr_has_json(expr),
        Stmt::Expr { expr, .. } => expr_has_json(expr),
        Stmt::Put { expr, .. } => expr_has_json(expr),
        Stmt::PutIndex { index, expr, .. } => expr_has_json(index) || expr_has_json(expr),
        Stmt::PutField { expr, .. } => expr_has_json(expr),
        Stmt::Yield { expr, .. } => expr_has_json(expr),
        Stmt::Throw { expr, .. } => expr_has_json(expr),
        Stmt::Try {
            try_body,
            catch_body,
            ..
        } => try_body.iter().any(stmt_has_json) || catch_body.iter().any(stmt_has_json),
        Stmt::When {
            cond,
            then_body,
            else_body,
            ..
        } => {
            expr_has_json(cond)
                || then_body.iter().any(stmt_has_json)
                || else_body.iter().any(stmt_has_json)
        }
        Stmt::Repeat { cond, body, .. } => {
            expr_has_json(cond) || body.iter().any(stmt_has_json)
        }
    }
}

fn expr_has_json(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Call { name, args } => {
            name.starts_with("std::json::") || args.iter().any(expr_has_json)
        }
        ExprKind::New { args, .. } => args.iter().any(expr_has_json),
        ExprKind::ArrayLit(elements) => elements.iter().any(expr_has_json),
        ExprKind::ArrayNew { len } => expr_has_json(len),
        ExprKind::Index { base, index } => expr_has_json(base) || expr_has_json(index),
        ExprKind::Unary { expr, .. } => expr_has_json(expr),
        ExprKind::Binary { left, right, .. } => expr_has_json(left) || expr_has_json(right),
        _ => false,
    }
}

fn stmt_has_string_from_bytes(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Set { expr, .. } => expr_has_string_from_bytes(expr),
        Stmt::Expr { expr, .. } => expr_has_string_from_bytes(expr),
        Stmt::Put { expr, .. } => expr_has_string_from_bytes(expr),
        Stmt::PutIndex { index, expr, .. } => {
            expr_has_string_from_bytes(index) || expr_has_string_from_bytes(expr)
        }
        Stmt::PutField { expr, .. } => expr_has_string_from_bytes(expr),
        Stmt::Yield { expr, .. } => expr_has_string_from_bytes(expr),
        Stmt::Throw { expr, .. } => expr_has_string_from_bytes(expr),
        Stmt::Try {
            try_body,
            catch_body,
            ..
        } => {
            try_body.iter().any(stmt_has_string_from_bytes)
                || catch_body.iter().any(stmt_has_string_from_bytes)
        }
        Stmt::When {
            cond,
            then_body,
            else_body,
            ..
        } => {
            expr_has_string_from_bytes(cond)
                || then_body.iter().any(stmt_has_string_from_bytes)
                || else_body.iter().any(stmt_has_string_from_bytes)
        }
        Stmt::Repeat { cond, body, .. } => {
            expr_has_string_from_bytes(cond) || body.iter().any(stmt_has_string_from_bytes)
        }
    }
}

fn expr_has_string_from_bytes(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Call { name, args } => {
            name == "std::string::from_bytes" || args.iter().any(expr_has_string_from_bytes)
        }
        ExprKind::New { args, .. } => args.iter().any(expr_has_string_from_bytes),
        ExprKind::ArrayLit(elements) => elements.iter().any(expr_has_string_from_bytes),
        ExprKind::ArrayNew { len } => expr_has_string_from_bytes(len),
        ExprKind::Index { base, index } => {
            expr_has_string_from_bytes(base) || expr_has_string_from_bytes(index)
        }
        ExprKind::Unary { expr, .. } => expr_has_string_from_bytes(expr),
        ExprKind::Binary { left, right, .. } => {
            expr_has_string_from_bytes(left) || expr_has_string_from_bytes(right)
        }
        _ => false,
    }
}

fn stmt_has_bytes(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Set { expr, .. } => expr_has_bytes(expr),
        Stmt::Expr { expr, .. } => expr_has_bytes(expr),
        Stmt::Put { expr, .. } => expr_has_bytes(expr),
        Stmt::PutIndex { index, expr, .. } => expr_has_bytes(index) || expr_has_bytes(expr),
        Stmt::PutField { expr, .. } => expr_has_bytes(expr),
        Stmt::Yield { expr, .. } => expr_has_bytes(expr),
        Stmt::Throw { expr, .. } => expr_has_bytes(expr),
        Stmt::Try {
            try_body,
            catch_body,
            ..
        } => try_body.iter().any(stmt_has_bytes) || catch_body.iter().any(stmt_has_bytes),
        Stmt::When {
            cond,
            then_body,
            else_body,
            ..
        } => {
            expr_has_bytes(cond)
                || then_body.iter().any(stmt_has_bytes)
                || else_body.iter().any(stmt_has_bytes)
        }
        Stmt::Repeat { cond, body, .. } => {
            expr_has_bytes(cond) || body.iter().any(stmt_has_bytes)
        }
    }
}

fn expr_has_bytes(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Call { name, args } => {
            name.starts_with("std::bytes::") || args.iter().any(expr_has_bytes)
        }
        ExprKind::New { args, .. } => args.iter().any(expr_has_bytes),
        ExprKind::ArrayLit(elements) => elements.iter().any(expr_has_bytes),
        ExprKind::ArrayNew { len } => expr_has_bytes(len),
        ExprKind::Index { base, index } => expr_has_bytes(base) || expr_has_bytes(index),
        ExprKind::Unary { expr, .. } => expr_has_bytes(expr),
        ExprKind::Binary { left, right, .. } => {
            expr_has_bytes(left) || expr_has_bytes(right)
        }
        _ => false,
    }
}

fn stmt_has_io(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Set { expr, .. } => expr_has_io(expr),
        Stmt::Expr { expr, .. } => expr_has_io(expr),
        Stmt::Put { expr, .. } => expr_has_io(expr),
        Stmt::PutIndex { index, expr, .. } => expr_has_io(index) || expr_has_io(expr),
        Stmt::PutField { expr, .. } => expr_has_io(expr),
        Stmt::Yield { expr, .. } => expr_has_io(expr),
        Stmt::Throw { expr, .. } => expr_has_io(expr),
        Stmt::Try {
            try_body,
            catch_body,
            ..
        } => try_body.iter().any(stmt_has_io) || catch_body.iter().any(stmt_has_io),
        Stmt::When {
            cond,
            then_body,
            else_body,
            ..
        } => {
            expr_has_io(cond) || then_body.iter().any(stmt_has_io) || else_body.iter().any(stmt_has_io)
        }
        Stmt::Repeat { cond, body, .. } => expr_has_io(cond) || body.iter().any(stmt_has_io),
    }
}

fn expr_has_io(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Call { name, args } => {
            name.starts_with("std::io::") || args.iter().any(expr_has_io)
        }
        ExprKind::New { args, .. } => args.iter().any(expr_has_io),
        ExprKind::ArrayLit(elements) => elements.iter().any(expr_has_io),
        ExprKind::ArrayNew { len } => expr_has_io(len),
        ExprKind::Index { base, index } => expr_has_io(base) || expr_has_io(index),
        ExprKind::Unary { expr, .. } => expr_has_io(expr),
        ExprKind::Binary { left, right, .. } => expr_has_io(left) || expr_has_io(right),
        _ => false,
    }
}

fn stmt_has_time(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Set { expr, .. } => expr_has_time(expr),
        Stmt::Expr { expr, .. } => expr_has_time(expr),
        Stmt::Put { expr, .. } => expr_has_time(expr),
        Stmt::PutIndex { index, expr, .. } => expr_has_time(index) || expr_has_time(expr),
        Stmt::PutField { expr, .. } => expr_has_time(expr),
        Stmt::Yield { expr, .. } => expr_has_time(expr),
        Stmt::Throw { expr, .. } => expr_has_time(expr),
        Stmt::Try {
            try_body,
            catch_body,
            ..
        } => try_body.iter().any(stmt_has_time) || catch_body.iter().any(stmt_has_time),
        Stmt::When {
            cond,
            then_body,
            else_body,
            ..
        } => {
            expr_has_time(cond)
                || then_body.iter().any(stmt_has_time)
                || else_body.iter().any(stmt_has_time)
        }
        Stmt::Repeat { cond, body, .. } => expr_has_time(cond) || body.iter().any(stmt_has_time),
    }
}

fn expr_has_time(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Call { name, args } => {
            name.starts_with("std::time::") || args.iter().any(expr_has_time)
        }
        ExprKind::New { args, .. } => args.iter().any(expr_has_time),
        ExprKind::ArrayLit(elements) => elements.iter().any(expr_has_time),
        ExprKind::ArrayNew { len } => expr_has_time(len),
        ExprKind::Index { base, index } => expr_has_time(base) || expr_has_time(index),
        ExprKind::Unary { expr, .. } => expr_has_time(expr),
        ExprKind::Binary { left, right, .. } => {
            expr_has_time(left) || expr_has_time(right)
        }
        _ => false,
    }
}

fn stmt_has_fs(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Set { expr, .. } => expr_has_fs(expr),
        Stmt::Expr { expr, .. } => expr_has_fs(expr),
        Stmt::Put { expr, .. } => expr_has_fs(expr),
        Stmt::PutIndex { index, expr, .. } => expr_has_fs(index) || expr_has_fs(expr),
        Stmt::PutField { expr, .. } => expr_has_fs(expr),
        Stmt::Yield { expr, .. } => expr_has_fs(expr),
        Stmt::Throw { expr, .. } => expr_has_fs(expr),
        Stmt::Try {
            try_body,
            catch_body,
            ..
        } => try_body.iter().any(stmt_has_fs) || catch_body.iter().any(stmt_has_fs),
        Stmt::When {
            cond,
            then_body,
            else_body,
            ..
        } => {
            expr_has_fs(cond)
                || then_body.iter().any(stmt_has_fs)
                || else_body.iter().any(stmt_has_fs)
        }
        Stmt::Repeat { cond, body, .. } => expr_has_fs(cond) || body.iter().any(stmt_has_fs),
    }
}

fn expr_has_fs(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Call { name, args } => {
            name.starts_with("std::fs::") || args.iter().any(expr_has_fs)
        }
        ExprKind::New { args, .. } => args.iter().any(expr_has_fs),
        ExprKind::ArrayLit(elements) => elements.iter().any(expr_has_fs),
        ExprKind::ArrayNew { len } => expr_has_fs(len),
        ExprKind::Index { base, index } => expr_has_fs(base) || expr_has_fs(index),
        ExprKind::Unary { expr, .. } => expr_has_fs(expr),
        ExprKind::Binary { left, right, .. } => expr_has_fs(left) || expr_has_fs(right),
        _ => false,
    }
}

fn stmt_has_path(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Set { expr, .. } => expr_has_path(expr),
        Stmt::Expr { expr, .. } => expr_has_path(expr),
        Stmt::Put { expr, .. } => expr_has_path(expr),
        Stmt::PutIndex { index, expr, .. } => expr_has_path(index) || expr_has_path(expr),
        Stmt::PutField { expr, .. } => expr_has_path(expr),
        Stmt::Yield { expr, .. } => expr_has_path(expr),
        Stmt::Throw { expr, .. } => expr_has_path(expr),
        Stmt::Try {
            try_body,
            catch_body,
            ..
        } => try_body.iter().any(stmt_has_path) || catch_body.iter().any(stmt_has_path),
        Stmt::When {
            cond,
            then_body,
            else_body,
            ..
        } => {
            expr_has_path(cond)
                || then_body.iter().any(stmt_has_path)
                || else_body.iter().any(stmt_has_path)
        }
        Stmt::Repeat { cond, body, .. } => expr_has_path(cond) || body.iter().any(stmt_has_path),
    }
}

fn expr_has_path(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Call { name, args } => {
            name.starts_with("std::path::") || args.iter().any(expr_has_path)
        }
        ExprKind::New { args, .. } => args.iter().any(expr_has_path),
        ExprKind::ArrayLit(elements) => elements.iter().any(expr_has_path),
        ExprKind::ArrayNew { len } => expr_has_path(len),
        ExprKind::Index { base, index } => expr_has_path(base) || expr_has_path(index),
        ExprKind::Unary { expr, .. } => expr_has_path(expr),
        ExprKind::Binary { left, right, .. } => expr_has_path(left) || expr_has_path(right),
        _ => false,
    }
}

fn stmt_has_env(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Set { expr, .. } => expr_has_env(expr),
        Stmt::Expr { expr, .. } => expr_has_env(expr),
        Stmt::Put { expr, .. } => expr_has_env(expr),
        Stmt::PutIndex { index, expr, .. } => expr_has_env(index) || expr_has_env(expr),
        Stmt::PutField { expr, .. } => expr_has_env(expr),
        Stmt::Yield { expr, .. } => expr_has_env(expr),
        Stmt::Throw { expr, .. } => expr_has_env(expr),
        Stmt::Try {
            try_body,
            catch_body,
            ..
        } => try_body.iter().any(stmt_has_env) || catch_body.iter().any(stmt_has_env),
        Stmt::When {
            cond,
            then_body,
            else_body,
            ..
        } => {
            expr_has_env(cond)
                || then_body.iter().any(stmt_has_env)
                || else_body.iter().any(stmt_has_env)
        }
        Stmt::Repeat { cond, body, .. } => expr_has_env(cond) || body.iter().any(stmt_has_env),
    }
}

fn expr_has_env(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Call { name, args } => {
            name.starts_with("std::env::") || args.iter().any(expr_has_env)
        }
        ExprKind::New { args, .. } => args.iter().any(expr_has_env),
        ExprKind::ArrayLit(elements) => elements.iter().any(expr_has_env),
        ExprKind::ArrayNew { len } => expr_has_env(len),
        ExprKind::Index { base, index } => expr_has_env(base) || expr_has_env(index),
        ExprKind::Unary { expr, .. } => expr_has_env(expr),
        ExprKind::Binary { left, right, .. } => expr_has_env(left) || expr_has_env(right),
        _ => false,
    }
}

fn stmt_has_object(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Set { ty, expr, .. } => {
            ty.as_ref()
                .map(|ty| matches!(ty, Type::Book(_)))
                .unwrap_or(false)
                || expr_has_object(expr)
        }
        Stmt::Expr { expr, .. } => expr_has_object(expr),
        Stmt::Put { expr, .. } => expr_has_object(expr),
        Stmt::PutIndex { index, expr, .. } => expr_has_object(index) || expr_has_object(expr),
        Stmt::PutField { .. } => true,
        Stmt::Yield { expr, .. } => expr_has_object(expr),
        Stmt::Throw { expr, .. } => expr_has_object(expr),
        Stmt::Try {
            try_body,
            catch_body,
            ..
        } => try_body.iter().any(stmt_has_object) || catch_body.iter().any(stmt_has_object),
        Stmt::When {
            cond,
            then_body,
            else_body,
            ..
        } => {
            expr_has_object(cond)
                || then_body.iter().any(stmt_has_object)
                || else_body.iter().any(stmt_has_object)
        }
        Stmt::Repeat { cond, body, .. } => {
            expr_has_object(cond) || body.iter().any(stmt_has_object)
        }
    }
}

fn expr_has_object(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::New { .. } => true,
        ExprKind::MemberAccess { .. } => true,
        ExprKind::Call { args, .. } => args.iter().any(expr_has_object),
        ExprKind::ArrayLit(elements) => elements.iter().any(expr_has_object),
        ExprKind::ArrayNew { len } => expr_has_object(len),
        ExprKind::Index { base, index } => expr_has_object(base) || expr_has_object(index),
        ExprKind::Unary { expr, .. } => expr_has_object(expr),
        ExprKind::Binary { left, right, .. } => expr_has_object(left) || expr_has_object(right),
        _ => false,
    }
}

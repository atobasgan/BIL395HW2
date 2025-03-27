 use std::io::\{self, BufRead\};

/// Bir ifadedeki olas\uc0\u305  token t\'fcrleri\
#[derive(Debug, Clone)]\
enum Token \{\
    Number(f64),\
    Ident(String), // De\uc0\u287 i\u351 ken ad\u305 \
    Plus,\
    Minus,\
    Mul,\
    Div,\
    LParen,\
    RParen,\
\}\
\
/// Verilen metni (\'f6r. "x + 3", "(2+3)*4") token listesine d\'f6n\'fc\uc0\u351 t\'fcr\'fcr.\
/// Say\uc0\u305 lar\u305  ve de\u287 i\u351 ken isimlerini ay\u305 rt eder.\
fn tokenize(expr: &str) -> Result<Vec<Token>, String> \{\
    let mut tokens = Vec::new();\
    let mut chars = expr.chars().peekable();\
\
    while let Some(&ch) = chars.peek() \{\
        match ch \{\
            ' ' | '\\t' => \{\
                chars.next(); // bo\uc0\u351 luklar\u305  atla\
            \}\
            '+' => \{\
                tokens.push(Token::Plus);\
                chars.next();\
            \}\
            '-' => \{\
                tokens.push(Token::Minus);\
                chars.next();\
            \}\
            '*' => \{\
                tokens.push(Token::Mul);\
                chars.next();\
            \}\
            '/' => \{\
                tokens.push(Token::Div);\
                chars.next();\
            \}\
            '(' => \{\
                tokens.push(Token::LParen);\
                chars.next();\
            \}\
            ')' => \{\
                tokens.push(Token::RParen);\
                chars.next();\
            \}\
            // Rakam veya ondal\uc0\u305 k nokta -> say\u305 \
            '0'..='9' | '.' => \{\
                let mut num_str = String::new();\
                while let Some(&c) = chars.peek() \{\
                    if c.is_ascii_digit() || c == '.' \{\
                        num_str.push(c);\
                        chars.next();\
                    \} else \{\
                        break;\
                    \}\
                \}\
                match num_str.parse::<f64>() \{\
                    Ok(n) => tokens.push(Token::Number(n)),\
                    Err(_) => return Err(format!("Ge\'e7ersiz say\uc0\u305 : \{\}", num_str)),\
                \}\
            \}\
            // Harfle veya alt \'e7izgiyle ba\uc0\u351 layan -> de\u287 i\u351 ken\
            c if c.is_alphabetic() || c == '_' => \{\
                let mut ident = String::new();\
                while let Some(&c2) = chars.peek() \{\
                    if c2.is_alphanumeric() || c2 == '_' \{\
                        ident.push(c2);\
                        chars.next();\
                    \} else \{\
                        break;\
                    \}\
                \}\
                tokens.push(Token::Ident(ident));\
            \}\
            // Tan\uc0\u305 nmayan karakter\
            _ => \{\
                return Err(format!("Unknown character: \{\}", ch));\
            \}\
        \}\
    \}\
\
    Ok(tokens)\
\}\
\
/// De\uc0\u287 i\u351 kenleri bir vekt\'f6rde sakl\u305 yoruz. Bu fonksiyon, bir de\u287 i\u351 ken ad\u305 n\u305 \
/// bulursa g\'fcnceller, yoksa ekler.\
fn store_var(vars: &mut Vec<(String, f64)>, name: &str, value: f64) \{\
    // Basit do\uc0\u287 rusal arama\
    for &mut (ref mut var_name, ref mut var_val) in vars.iter_mut() \{\
        if var_name == name \{\
            *var_val = value;\
            return;\
        \}\
    \}\
    // Yoksa yeni ekle\
    vars.push((name.to_string(), value));\
\}\
\
/// Bu fonksiyon, varsa de\uc0\u287 i\u351 kenin de\u287 erini d\'f6nd\'fcr\'fcr, yoksa None\
fn get_var_value(vars: &Vec<(String, f64)>, name: &str) -> Option<f64> \{\
    for &(ref var_name, val) in vars.iter() \{\
        if var_name == name \{\
            return Some(val);\
        \}\
    \}\
    None\
\}\
\
/// Parser yap\uc0\u305 s\u305 : tokens dizisi, mevcut okuma konumu, de\u287 i\u351 ken tablosu\
struct Parser<'a> \{\
    tokens: Vec<Token>,\
    pos: usize,\
    vars: &'a Vec<(String, f64)>,\
\}\
\
impl<'a> Parser<'a> \{\
    fn new(tokens: Vec<Token>, vars: &'a Vec<(String, f64)>) -> Self \{\
        Parser \{ tokens, pos: 0, vars \}\
    \}\
\
    fn parse_expr(&mut self) -> Result<f64, String> \{\
        // parse_expr -> parse_term (('+' | '-') parse_term)*\
        let mut result = self.parse_term()?;\
        while self.pos < self.tokens.len() \{\
            match self.tokens[self.pos] \{\
                Token::Plus => \{\
                    self.pos += 1;\
                    let rhs = self.parse_term()?;\
                    result += rhs;\
                \}\
                Token::Minus => \{\
                    self.pos += 1;\
                    let rhs = self.parse_term()?;\
                    result -= rhs;\
                \}\
                _ => break,\
            \}\
        \}\
        Ok(result)\
    \}\
\
    fn parse_term(&mut self) -> Result<f64, String> \{\
        // parse_term -> parse_factor (('*' | '/') parse_factor)*\
        let mut result = self.parse_factor()?;\
        while self.pos < self.tokens.len() \{\
            match self.tokens[self.pos] \{\
                Token::Mul => \{\
                    self.pos += 1;\
                    let rhs = self.parse_factor()?;\
                    result *= rhs;\
                \}\
                Token::Div => \{\
                    self.pos += 1;\
                    let rhs = self.parse_factor()?;\
                    if rhs == 0.0 \{\
                        return Err("Division by zero".to_string());\
                    \}\
                    result /= rhs;\
                \}\
                _ => break,\
            \}\
        \}\
        Ok(result)\
    \}\
\
    fn parse_factor(&mut self) -> Result<f64, String> \{\
        // parse_factor -> Number | Ident | '(' parse_expr ')' | unary minus\
        if self.pos >= self.tokens.len() \{\
            return Err("Beklenmeyen ifade sonu".to_string());\
        \}\
\
        match &self.tokens[self.pos] \{\
            Token::Number(n) => \{\
                let val = *n;\
                self.pos += 1;\
                Ok(val)\
            \}\
            Token::Ident(name) => \{\
                let var_name = name.clone();\
                self.pos += 1;\
                if let Some(val) = get_var_value(self.vars, &var_name) \{\
                    Ok(val)\
                \} else \{\
                    Err(format!("Undefined variable: \{\}", var_name))\
                \}\
            \}\
            Token::LParen => \{\
                // '(' -> parse_expr -> ')'\
                self.pos += 1; // '(' t\'fcket\
                let val = self.parse_expr()?;\
                if self.pos >= self.tokens.len() \{\
                    return Err("Kapanan parantez eksik".to_string());\
                \}\
                match self.tokens[self.pos] \{\
                    Token::RParen => \{\
                        self.pos += 1; // ')' t\'fcket\
                        Ok(val)\
                    \}\
                    _ => Err("Kapanan parantez eksik".to_string()),\
                \}\
            \}\
            Token::Minus => \{\
                // unary minus\
                self.pos += 1;\
                let sub = self.parse_factor()?;\
                Ok(-sub)\
            \}\
            _ => Err("Beklenmeyen token".to_string()),\
        \}\
    \}\
\}\
\
/// Bu fonksiyon, tokens dizisini parse edip bir f64 sonu\'e7 d\'f6nd\'fcr\'fcr.\
/// De\uc0\u287 i\u351 kenleri okumak i\'e7in `vars`'a bakar (yazma yok).\
fn evaluate_expression(expr: &str, vars: &Vec<(String, f64)>) -> Result<f64, String> \{\
    let tokens = tokenize(expr)?;\
    let mut parser = Parser::new(tokens, vars);\
    let result = parser.parse_expr()?;\
    // E\uc0\u287 er h\'e2l\'e2 i\u351 lenmemi\u351  token kald\u305 ysa, fazladan veri var\
    if parser.pos < parser.tokens.len() \{\
        return Err("Fazladan token bulundu".to_string());\
    \}\
    Ok(result)\
\}\
\
fn main() \{\
    let stdin = io::stdin();\
    println!("Rust Calculator. Type 'quit' to exit.");\
    let mut vars = Vec::new(); // De\uc0\u287 i\u351 kenler: (isim, de\u287 er) ikilileri\
\
    for line in stdin.lock().lines() \{\
        let line = match line \{\
            Ok(l) => l.trim().to_string(),\
            Err(_) => continue,\
        \};\
        if line.eq_ignore_ascii_case("quit") \{\
            break;\
        \}\
        if line.is_empty() \{\
            continue;\
        \}\
\
        // Atama ifadesi var m\uc0\u305 ? (\'f6rn: x = 3 + 2)\
        if let Some(eq_pos) = line.find('=') \{\
            let var_name = line[..eq_pos].trim();   // soldaki k\uc0\u305 s\u305 m\
            let expr_str = line[eq_pos + 1..].trim(); // sa\uc0\u287 daki k\u305 s\u305 m\
\
            if var_name.is_empty() \{\
                println!("Error: Invalid assignment (no variable name).");\
                continue;\
            \}\
            // \uc0\u304 fadeyi de\u287 erlendir\
            match evaluate_expression(expr_str, &vars) \{\
                Ok(value) => \{\
                    store_var(&mut vars, var_name, value);\
                    println!("\{\} = \{\}", var_name, value);\
                \}\
                Err(e) => println!("Error: \{\}", e),\
            \}\
        \} else \{\
            // Sadece ifade\
            match evaluate_expression(&line, &vars) \{\
                Ok(value) => \{\
                    println!("\{\}", value);\
                \}\
                Err(e) => println!("Error: \{\}", e),\
            \}\
        \}\
    \}\
\
    println!("Goodbye!");\
\}\
}

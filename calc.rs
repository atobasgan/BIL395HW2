use std::io::{self, BufRead};

#[derive(Debug, Clone)]
enum Token {
    Number(f64),
    Ident(String),
    Plus,
    Minus,
    Mul,
    Div,
    LParen,
    RParen,
}

fn tokenize(expr: &str) -> Result<Vec<Token>, String> {
    let mut tokens = Vec::new();
    let mut chars = expr.chars().peekable();

    while let Some(&ch) = chars.peek() {
        match ch {
            ' ' | '\t' => { chars.next(); }
            '+' => { tokens.push(Token::Plus); chars.next(); }
            '-' => { tokens.push(Token::Minus); chars.next(); }
            '*' => { tokens.push(Token::Mul); chars.next(); }
            '/' => { tokens.push(Token::Div); chars.next(); }
            '(' => { tokens.push(Token::LParen); chars.next(); }
            ')' => { tokens.push(Token::RParen); chars.next(); }
            '0'..='9' | '.' => {
                let mut num_str = String::new();
                while let Some(&c) = chars.peek() {
                    if c.is_ascii_digit() || c == '.' {
                        num_str.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }
                match num_str.parse::<f64>() {
                    Ok(n) => tokens.push(Token::Number(n)),
                    Err(_) => return Err(format!("Geçersiz sayı: {}", num_str)),
                }
            }
            c if c.is_alphabetic() || c == '_' => {
                let mut ident = String::new();
                while let Some(&c2) = chars.peek() {
                    if c2.is_alphanumeric() || c2 == '_' {
                        ident.push(c2);
                        chars.next();
                    } else {
                        break;
                    }
                }
                tokens.push(Token::Ident(ident));
            }
            _ => return Err(format!("Bilinmeyen karakter: {}", ch)),
        }
    }

    Ok(tokens)
}

fn store_var(vars: &mut Vec<(String, f64)>, name: &str, value: f64) {
    for &mut (ref mut var_name, ref mut var_val) in vars.iter_mut() {
        if var_name == name {
            *var_val = value;
            return;
        }
    }
    vars.push((name.to_string(), value));
}

fn get_var_value(vars: &Vec<(String, f64)>, name: &str) -> Option<f64> {
    for &(ref var_name, val) in vars.iter() {
        if var_name == name {
            return Some(val);
        }
    }
    None
}

struct Parser<'a> {
    tokens: Vec<Token>,
    pos: usize,
    vars: &'a Vec<(String, f64)>,
}

impl<'a> Parser<'a> {
    fn new(tokens: Vec<Token>, vars: &'a Vec<(String, f64)>) -> Self {
        Parser { tokens, pos: 0, vars }
    }

    fn parse_expr(&mut self) -> Result<f64, String> {
        let mut result = self.parse_term()?;
        while self.pos < self.tokens.len() {
            match self.tokens[self.pos] {
                Token::Plus => {
                    self.pos += 1;
                    let rhs = self.parse_term()?;
                    result += rhs;
                }
                Token::Minus => {
                    self.pos += 1;
                    let rhs = self.parse_term()?;
                    result -= rhs;
                }
                _ => break,
            }
        }
        Ok(result)
    }

    fn parse_term(&mut self) -> Result<f64, String> {
        let mut result = self.parse_factor()?;
        while self.pos < self.tokens.len() {
            match self.tokens[self.pos] {
                Token::Mul => {
                    self.pos += 1;
                    let rhs = self.parse_factor()?;
                    result *= rhs;
                }
                Token::Div => {
                    self.pos += 1;
                    let rhs = self.parse_factor()?;
                    if rhs == 0.0 {
                        return Err("0'a bölme hatası".to_string());
                    }
                    result /= rhs;
                }
                _ => break,
            }
        }
        Ok(result)
    }

    fn parse_factor(&mut self) -> Result<f64, String> {
        if self.pos >= self.tokens.len() {
            return Err("Beklenmeyen ifade sonu".to_string());
        }

        match &self.tokens[self.pos] {
            Token::Number(n) => {
                let val = *n;
                self.pos += 1;
                Ok(val)
            }
            Token::Ident(name) => {
                let var_name = name.clone();
                self.pos += 1;
                if let Some(val) = get_var_value(self.vars, &var_name) {
                    Ok(val)
                } else {
                    Err(format!("Tanımsız değişken: {}", var_name))
                }
            }
            Token::LParen => {
                self.pos += 1;
                let val = self.parse_expr()?;
                if self.pos >= self.tokens.len() {
                    return Err("Kapanan parantez eksik".to_string());
                }
                match self.tokens[self.pos] {
                    Token::RParen => {
                        self.pos += 1;
                        Ok(val)
                    }
                    _ => Err("Kapanan parantez eksik".to_string()),
                }
            }
            Token::Minus => {
                self.pos += 1;
                let sub = self.parse_factor()?;
                Ok(-sub)
            }
            _ => Err("Beklenmeyen token".to_string()),
        }
    }
}

fn evaluate_expression(expr: &str, vars: &Vec<(String, f64)>) -> Result<f64, String> {
    let tokens = tokenize(expr)?;
    let mut parser = Parser::new(tokens, vars);
    let result = parser.parse_expr()?;
    if parser.pos < parser.tokens.len() {
        return Err("Fazladan token bulundu".to_string());
    }
    Ok(result)
}

fn main() {
    let stdin = io::stdin();
    println!("Rust Hesap Makinesi. Çıkmak için 'quit' yazın.");
    let mut vars = Vec::new();

    for line in stdin.lock().lines() {
        let line = match line {
            Ok(l) => l.trim().to_string(),
            Err(_) => continue,
        };
        if line.eq_ignore_ascii_case("quit") {
            break;
        }
        if line.is_empty() {
            continue;
        }

        if let Some(eq_pos) = line.find('=') {
            let var_name = line[..eq_pos].trim();
            let expr_str = line[eq_pos + 1..].trim();

            if var_name.is_empty() {
                println!("Hata: Geçersiz atama (boş değişken adı).");
                continue;
            }

            match evaluate_expression(expr_str, &vars) {
                Ok(value) => {
                    store_var(&mut vars, var_name, value);
                    println!("{} = {}", var_name, value);
                }
                Err(e) => println!("Hata: {}", e),
            }
        } else {
            match evaluate_expression(&line, &vars) {
                Ok(value) => println!("{}", value),
                Err(e) => println!("Hata: {}", e),
            }
        }
    }

    println!("Güle güle!");
}

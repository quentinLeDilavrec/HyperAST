#[cfg(target_arch = "wasm32")]
pub(crate) fn file_save(name: &str, ext: &str, content: &str) -> bool {
    use wasm_bindgen::prelude::wasm_bindgen;
    #[wasm_bindgen]
    extern "C" {
        fn alert(s: &str);
    }

    #[wasm_bindgen]
    extern "C" {
        fn download(data: &str, filename: &str, ext: &str, r#type: &str);
    }
    download(content, name, ext, "text/plain");

    // need to handle it async on JS side
    if false {
        eframe::web_sys::console::log_1(&content.into());
        alert(
            "(WIP) download failed, the content was logged in the debug console as a fallback :)",
        );
    }
    true
}

#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn file_save(_name: &str, _ext: &str, _content: &str) -> bool {
    // TODO
    println!("TODO save file");
    false
}

pub fn join<Item: ToString>(mut it: impl Iterator<Item = Item>, sep: &str) -> impl ToString {
    let mut res = String::default();
    if let Some(e) = it.next() {
        res.push_str(&e.to_string());
    }
    while let Some(e) = it.next() {
        res.push_str(sep);
        res.push_str(&e.to_string());
    }
    res
}

pub(crate) struct SecFmt(pub f64);

impl From<f64> for SecFmt {
    fn from(value: f64) -> Self {
        Self(value)
    }
}

impl std::fmt::Display for SecFmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // f.precision()
        let x = self.0;
        let (t, n) = if x > 60.0 * 60.0 {
            let n = if f.alternate() { "minutes" } else { "m" };
            (x / 60.0, n)
        } else if x > 60.0 * 60.0 * 24.0 {
            let n = if f.alternate() { "minutes" } else { "d" };
            (x / 60.0, n)
        } else if x > 60.0 * 60.0 {
            let n = if f.alternate() { "minutes" } else { "m" };
            (x / 60.0, n)
        } else if x > 60.0 {
            let n = if f.alternate() { "minutes" } else { "m" };
            (x / 60.0, n)
        } else if x == 0.0 {
            let n = if f.alternate() { "seconds" } else { "s" };
            (x, n)
        } else if x < 0.00_000_000_001 {
            let n = if f.alternate() { "pico seconds" } else { "ps" };
            (x * 1_000_000_000_000., n)
        } else if x < 0.00_000_001 {
            let n = if f.alternate() { "nano seconds" } else { "ns" };
            (x * 1_000_000_000., n)
        } else if x < 0.00_001 {
            let n = if f.alternate() { "micro seconds" } else { "us" };
            (x * 1_000_000., n)
        } else if x < 1.0 {
            let n = if f.alternate() { "milli seconds" } else { "ms" };
            (x * 1_000., n)
        } else {
            let n = if f.alternate() { "seconds" } else { "s" };
            (x, n)
        };
        if t == 0.0 {
            write!(f, "{:.1} {}", t, n)
        } else if let Some(prec) = f.precision() {
            write!(f, "{} {}", round_to_significant_digits3(t, prec), n)
        } else {
            write!(f, "{} {}", t, n)
        }
    }
}

pub fn round_to_significant_digits3(number: f64, significant_digits: usize) -> String {
    if number == 0.0 {
        return format!("{:.*}", significant_digits, number);
    }
    let abs = number.abs();
    let d = if abs == 1.0 {
        1.0
    } else {
        (abs.log10().ceil()).max(0.0)
    };
    let power = significant_digits - d as usize;

    let magnitude = 10.0_f64.powi(power as i32);
    let shifted = number * magnitude;
    let rounded_number = shifted.round();
    let unshifted = rounded_number as f64 / magnitude;
    dbg!(
        number,
        (number.abs() + 0.000001).log10().ceil(),
        significant_digits,
        power,
        d
    );
    format!("{:.*}", power, unshifted)
}

#[test]
fn seconde_formating_test() {
    assert_eq!(format!("{:.4}", SecFmt(0.0)), "0.0 s");
    assert_eq!(format!("{:.3}", SecFmt(1.0 / 1000.0)), "1.00 ms");
    assert_eq!(format!("{:.3}", SecFmt(1.0 / 1000.0 / 1000.0)), "1.00 us");
    assert_eq!(format!("{:.4}", SecFmt(0.00_000_000_1)), "1.000 ns");
    assert_eq!(format!("{:.4}", SecFmt(0.00_000_000_000_1)), "1.000 ps");
    assert_eq!(format!("{:.2}", SecFmt(0.0000000012)), "1.2 ns");
    assert_eq!(format!("{:.4}", SecFmt(10.43333)), "10.43 s");
    assert_eq!(format!("{:.3}", SecFmt(10.43333)), "10.4 s");
    assert_eq!(format!("{:.2}", SecFmt(10.43333)), "10 s");
    assert_eq!(format!("{:3e}", 10.43333), "1.043333e1");
}

#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn prepare_paste(
    ui: &mut egui::Ui,
    trigger: bool,
    await_response: &mut bool,
) -> Option<String> {
    if *await_response {
        let paste = ui.input(|i| {
            (i.events.iter())
                .find(|e| matches!(e, egui::Event::Paste(_)))
                .cloned()
        });
        if let Some(egui::Event::Paste(paste)) = paste {
            return Some(paste);
        }
    }
    if trigger {
        ui.ctx()
            .send_viewport_cmd(egui::ViewportCommand::RequestPaste);
        *await_response = true;
    }
    None
}

#[cfg(target_arch = "wasm32")]
#[allow(static_mut_refs)]
pub(crate) fn prepare_paste(
    ui: &mut egui::Ui,
    trigger: bool,
    await_response: &mut bool,
) -> Option<String> {
    static mut B: Option<String> = None;

    if *await_response {
        let paste = unsafe { B.take() };
        return paste;
    } else if trigger {
        use wasm_bindgen_futures::spawn_local;
        let _task = spawn_local(async move {
            let window = web_sys::window().expect("window");
            let nav = window.navigator().clipboard();
            let p = nav.read_text();
            let result = wasm_bindgen_futures::JsFuture::from(p)
                .await
                .expect("clipboard read");
            unsafe { B = Some(result.as_string().unwrap()) };
        });
        *await_response = true;
    }
    None
}

macro_rules! typed_vec {
    ($vis:vis $name:ident, $item:ty, $id:ident($ty:ty)) => {
        #[repr(transparent)]
        #[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
        #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
        $vis struct $id($ty);

        #[allow(dead_code)]
        impl $id {
            const INVALID: $id = $id(<$ty>::MAX);
            // TODO try to avoid it
            $vis fn to_usize(&self) -> usize {
                self.0 as usize
            }
        }

        #[derive(Debug, Default)]
        #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
        $vis struct $name(Vec<$item>);

        #[allow(dead_code)]
        impl $name {
            $vis fn new() -> Self {
                Self(Vec::new())
            }

            #[track_caller]
            $vis fn push(&mut self, value: $item) -> $id {
                debug_assert!(self.0.len() < <$ty>::MAX as usize);
                let id = $id(self.0.len() as $ty);
                self.0.push(value);
                id
            }

            #[track_caller]
            $vis fn get(&self, idx: $id) -> Option<&$item> {
                let idx = idx.0 as usize;
                self.0.get(idx)
            }

            #[track_caller]
            $vis fn get_mut(&mut self, idx: $id) -> Option<&mut $item> {
                let idx = idx.0 as usize;
                self.0.get_mut(idx)
            }

            $vis fn iter(&self) -> impl Iterator<Item = &$item> {
                self.0.iter()
            }

            $vis fn enumerate(&self) -> impl Iterator<Item = ($id, &$item)> {
                self.0.iter().enumerate().map(|(i, v)| ($id(i as $ty), v))
            }

            $vis fn find(&self, mut predicate: impl FnMut(&$item) -> bool) -> Option<($id, &$item)> {
                self.0.iter().enumerate().find(|(_, v)| predicate(v)).map(|(i, v)| ($id(i as $ty), v))
            }
        }

        impl std::ops::Index<$id> for $name {
            type Output = $item;
            #[track_caller]
            fn index(&self, idx: $id) -> &Self::Output {
                let idx = idx.0 as usize;
                debug_assert!(idx < self.0.len());
                &self.0[idx]
            }
        }

        impl std::ops::Index<$id> for &$name {
            type Output = $item;
            #[track_caller]
            fn index(&self, idx: $id) -> &Self::Output {
                let idx = idx.0 as usize;
                debug_assert!(idx < self.0.len());
                &self.0[idx]
            }
        }

        impl std::ops::IndexMut<$id> for $name {
            #[track_caller]
            fn index_mut(&mut self, idx: $id) -> &mut Self::Output {
                let idx = idx.0 as usize;
                debug_assert!(idx < self.0.len());
                &mut self.0[idx]
            }
        }

        // impl std::ops::Deref for $name {
        //     type Target = Vec<$item>;
        //     fn deref(&self) -> &Self::Target {
        //         &self.0
        //     }
        // }
        // impl std::ops::DerefMut for $name {
        //     fn deref_mut(&mut self) -> &mut Self::Target {
        //         &mut self.0
        //     }
        // }

        impl From<Vec<$item>> for $name {
            fn from(vec: Vec<$item>) -> Self {
                Self(vec)
            }
        }
    };
}
pub(crate) use typed_vec;
#[cfg(test)]
mod tests {
    use super::*;

    typed_vec!(Users, User, UserId(u16));

    #[derive(Debug)]
    #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
    pub struct User {
        pub name: String,
    }

    #[test]
    fn test_name() {
        let mut users = Users::new();
        let user_id = users.push(User {
            name: "John".to_string(),
        });
        assert_eq!(users[user_id].name, "John");
        users[user_id].name = "Jane".to_string();
        assert_eq!(users[user_id].name, "Jane");
        let user_id2 = users.push(User {
            name: "Jane Doe".to_string(),
        });
        assert_eq!(users[user_id2].name, "Jane Doe");
    }
}

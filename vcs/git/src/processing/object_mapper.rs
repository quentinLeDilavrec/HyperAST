pub trait ObjectMapper {
    type K;
    type V;
    fn get(&self, key: &Self::K) -> Option<&Self::V>;
    fn insert(&mut self, key: Self::K, value: Self::V) -> Option<Self::V>;
}

#[derive(Clone, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub struct ObjectName(Vec<u8>);

impl ObjectName {
    pub fn try_str(&self) -> Result<&str, std::str::Utf8Error> {
        self.try_into()
    }
    pub fn as_bytes(&self) -> &[u8] {
        &self.0
    }
    pub fn ends_with(&self, suffix: &str) -> bool {
        self.0.ends_with(suffix.as_bytes())
    }
    pub fn eq_str(&self, s: &str) -> bool {
        self.0.eq(s.as_bytes())
    }
}

impl From<&[u8]> for ObjectName {
    fn from(value: &[u8]) -> Self {
        Self(value.to_vec())
    }
}

impl<const L: usize> From<&[u8; L]> for ObjectName {
    fn from(value: &[u8; L]) -> Self {
        Self(value.to_vec())
    }
}

impl<'a> TryInto<&'a str> for &'a ObjectName {
    type Error = std::str::Utf8Error;

    fn try_into(self) -> Result<&'a str, Self::Error> {
        std::str::from_utf8(&self.0)
    }
}

impl TryInto<String> for &ObjectName {
    type Error = std::str::Utf8Error;

    fn try_into(self) -> Result<String, Self::Error> {
        std::str::from_utf8(&self.0).map(|x| x.to_string())
    }
}

impl TryInto<String> for ObjectName {
    type Error = std::str::Utf8Error;

    fn try_into(self) -> Result<String, Self::Error> {
        std::str::from_utf8(&self.0).map(|x| x.to_string())
    }
}

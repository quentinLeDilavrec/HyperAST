/// The npm scheme,
/// it contains a package.json then,
/// in its simplest form contains an index.js and a src/ directory,
/// or is a collection of packages that contains a packages/ directory where each package is located
#[cfg(feature = "npm")]
pub struct Npm;

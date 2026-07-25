use poll_promise::Promise;

use crate::app::types::{Commit, FileIdentifier};
use crate::utils_poll::Resource;

pub mod code_tracking;
pub mod detached_view;
pub mod long_tracking;

#[derive(serde::Deserialize, serde::Serialize)]
pub struct FetchedFile {
    pub content: String,
    pub line_breaks: Vec<usize>,
}

pub(super) type RemoteFile = crate::utils_poll::Remote<FetchedFile>;

pub(crate) type FetchedFiles = std::collections::HashMap<super::types::FileIdentifier, RemoteFile>;

pub(crate) fn try_fetch_remote_file<R>(
    file_result: &std::collections::hash_map::Entry<'_, FileIdentifier, RemoteFile>,
    mut f: impl FnMut(&FetchedFile) -> R,
) -> Option<Result<R, String>> {
    let std::collections::hash_map::Entry::Occupied(promise) = file_result else {
        return None;
    };
    let promise = promise.get();
    let result = promise.ready()?;
    match result {
        Ok(resource) => {
            let text = resource.content.as_ref()?;
            Some(Ok(f(text)))
        }
        Err(error) => Some(Err(error.to_string())),
    }
}

impl Resource<FetchedFile> {
    pub(crate) fn from_response(_ctx: &egui::Context, response: ehttp::Response) -> Self {
        let _content_type = response.content_type().unwrap_or_default();
        // let image = if content_type.starts_with("image/") {
        //     RetainedImage::from_image_bytes(&response.url, &response.bytes).ok()
        // } else {
        //     None
        // };

        let text = response.text();
        // let colored_text = text.and_then(|text| syntax_highlighting(ctx, &response, text));
        let text = text.map(|x| {
            let content = x.to_string();
            let line_breaks = content
                .bytes()
                .enumerate()
                .filter_map(|(i, b)| if b == b'\n' { Some(i) } else { None })
                .collect();
            FetchedFile {
                content,
                line_breaks,
            }
        });

        Self {
            response,
            content: text,
            // image,
            // text: colored_text,
        }
    }
}

pub(super) fn remote_fetch_file(
    ctx: &egui::Context,
    api_addr: &str,
    commit: &Commit,
    file_path: &str,
) -> RemoteFile {
    let ctx = ctx.clone();
    let (sender, promise) = Promise::new();
    let url = format!(
        "http://{}/file/github/{}/{}/{}/{}",
        api_addr, &commit.repo.user, &commit.repo.name, &commit.id, &file_path,
    );

    let request = ehttp::Request::get(&url);
    // request
    //     .headers
    //     .insert("Content-Type".to_string(), "text".to_string());

    ehttp::fetch(request, move |response| {
        ctx.request_repaint(); // wake up UI thread
        let resource =
            response.map(|response| Resource::<FetchedFile>::from_response(&ctx, response));
        sender.send(resource);
    });
    promise
}

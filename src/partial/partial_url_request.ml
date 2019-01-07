open Tea

type msg = [`OnUrlRequest of Router.url_request]

let update = function
  | `OnUrlRequest (Router.Internal url) -> Navigation.newUrl url
  | `OnUrlRequest (Router.External url) -> Router.open_tab url

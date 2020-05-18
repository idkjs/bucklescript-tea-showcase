open Tea;

type msg = [ | `OnUrlRequest(Router.url_request)];

let update =
  fun
  | `OnUrlRequest(Router.Internal(url)) => Navigation.newUrl(url)
  | `OnUrlRequest(Router.External(url)) => Router.open_tab(url);

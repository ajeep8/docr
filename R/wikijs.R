#' Login wiki.js
#'
#' get your user_token from https://your_wiki.js_url/graphql:
#' copy the following codes to graphql and get the long jwt user_token:
#' mutation {
#'   authentication {
#'     login (
#'       username: "your username(email)"
#'       password: "your password"
#'       strategy: "local"
#'     ) {jwt}
#'   }
#' }
#'
#' @param graphqlUrl wiki.js graphql url
#' @param schemaUrl wiki.js graphql schema url
#' @param userToken user_token
#'
#' @return the connect con
#' @export
#'
wikijsLogin <- function(userToken, graphqlUrl, schemaUrl) {
  library("ghql")

  token <- userToken
  con <- GraphqlClient$new(
    url = graphqlUrl,
    headers = list(Authorization = paste0("Bearer ", token))
  )

con$load_schema(schema_url = schemaUrl)

con

}


#' Fetch .md file from wiki.js by graphQL interface
#'
#' @param con connect to graphQL
#' @param Id the ID in the page history top-right
#'
#' @return text
#' @export
#'
fetchMd <- function(con, Id) {
	library("jsonlite")

  qry <- Query$new()
  qry$query('mydata', paste0('
query{
  pages{
    single(id: ',Id, '){
      path,
      title,
      description,
      content
    }
  }
}
  '))
  x <- con$exec(qry$queries$mydata)
  y <- jsonlite::fromJSON(x)

  filename <- y$data$pages$single$path
  content <- y$data$pages$single$content

  list(filename, content)
}

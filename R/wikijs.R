#' Login wiki.js
#'
#' @param url wiki.js graphql url
#' @param token user token
#'
#' @return text
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
#' @param Id pageId
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

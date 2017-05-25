source('load_packages.R')


# accessing your personal github statistics

base_url <- 'https://api.github.com/users/dipanjanS?'
my_profile_url <- paste0(base_url, api_id_param, arg_sep, api_pwd_param)
response <- GET(my_profile_url)

response

as.data.frame(response$headers)[,c('x.ratelimit.limit', 'x.ratelimit.remaining')]

me <- content(response)
me <- as.data.frame(t(as.matrix(me)))
View(me[,c('login', 'public_repos', 'public_gists', 'followers',
           'created_at', 'updated_at')])


me <- fromJSON(my_profile_url)
me <- as.data.frame(t(as.matrix(me)))
View(me[,c('login', 'public_repos', 'public_gists', 'followers',
           'created_at', 'updated_at')])
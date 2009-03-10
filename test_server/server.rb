#!ruby
require 'rubygems'
require 'net/http'
require 'sinatra'
require 'json'
require 'cgi'
require 'digest/md5'


enable :sessions
set :views, File.dirname(__FILE__) + '/views'
set :public, File.dirname(__FILE__) + '/public'


get '/' do
  redirect '/directory', 303
end

get '/directory' do
  response = Net::HTTP.get(URI.parse("http://#{request.env["SERVER_NAME"]}:6666/directory"))
  @dir_list = JSON.parse(response)
  erb :directory
end

get '/island' do
  @island_id = params[:id]
  @user_id = Digest::MD5.hexdigest(request.env["rack.request.cookie_string"])
  @host = request.env["SERVER_NAME"]
  @port = "6666"
  @policy_port = "6665"
  erb :island
end

get '/new_island' do
  Net::HTTP.get(URI.parse("http://#{request.env["SERVER_NAME"]}:6666/new_island?type=#{params[:type]}&desc=#{params[:desc]}"))
  redirect '/directory', 303
end



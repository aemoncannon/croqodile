#!ruby
require 'rubygems'
require 'net/http'
require 'sinatra'
require 'json'


get '/' do
  redirect '/directory', 303
end

get '/directory' do
  response = Net::HTTP.get(URI.parse('http://localhost:6666/directory'))
  @dir_list = JSON.parse(response)
  erb :directory
end

get '/island' do
  erb :island
end

get '/new_island' do
  Net::HTTP.get(URI.parse("http://localhost:6666/new_island?type=#{params[:type]}&desc#{params[:desc]}"))
  redirect '/directory', 303
end

set :views, File.dirname(__FILE__) + '/views'
set :public, File.dirname(__FILE__) + '/public'

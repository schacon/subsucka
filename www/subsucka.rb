$:.unshift File.dirname(__FILE__) + "/sinatra/lib"
require "sinatra"

require "client"

require "rubygems"
require "thin"

Subsucka = Rebar::Erlang.new(:subversion_import, '127.0.0.1', 5500)

get '/' do
  haml :index
end

post '/' do
  @uri = params[:uri]
  puts "Importing: #{@uri}"
  Subsucka.import_uri(@uri)
  haml :done
end

use_in_file_templates!

__END__

@@ index
#title Welcome to subsucka
%form{:action => '/', :method => 'post'}
  %input{:name => 'uri', :size => 100}
  %input{:type => 'submit'}
    
@@ done
#title Working on
= @uri


require 'mongrel'
require 'stringio'


module Rack
  module Handler
    class Mongrel < ::Mongrel::HttpHandler
      def self.run(app, options={})
        server = ::Mongrel::HttpServer.new(options[:Host] || '0.0.0.0',
                                           options[:Port] || 8080)
        # Acts like Rack::URLMap, utilizing Mongrel's own path finding methods.
        # Use is similar to #run, replacing the app argument with a hash of 
        # { path=>app, ... } or an instance of Rack::URLMap.
        if options[:map]
          if app.is_a? Hash
            app.each do |path, appl|
              path = '/'+path unless path[0] == ?/
              server.register(path, Rack::Handler::Mongrel.new(appl))
            end
          elsif app.is_a? URLMap
            app.instance_variable_get(:@mapping).each do |(host, path, appl)|
             next if !host.nil? && !options[:Host].nil? && options[:Host] != host
             path = '/'+path unless path[0] == ?/
             server.register(path, Rack::Handler::Mongrel.new(appl))
            end
          else
            raise ArgumentError, "first argument should be a Hash or URLMap"
          end
        else
          server.register('/', Rack::Handler::Mongrel.new(app))
        end
        yield server  if block_given?
        server.run.join
      end

      def initialize(app)
        @app = app
      end

      def process(request, response)
        env = {}.replace(request.params)
        env.delete "HTTP_CONTENT_TYPE"
        env.delete "HTTP_CONTENT_LENGTH"

        env["SCRIPT_NAME"] = ""  if env["SCRIPT_NAME"] == "/"

        env.update({"rack.version" => [0,1],
                     "rack.input" => request.body || StringIO.new(""),
                     "rack.errors" => STDERR,

                     "rack.multithread" => true,
                     "rack.multiprocess" => false, # ???
                     "rack.run_once" => false,

                     "rack.url_scheme" => "http",
                   })
        env["QUERY_STRING"] ||= ""
        env.delete "PATH_INFO"  if env["PATH_INFO"] == ""

        status, headers, body = @app.call(env)

        begin
          response.status = status.to_i
          headers.each { |k, vs|
            vs.each { |v|
              response.header[k] = v
            }
          }
          # taken from Merb until we have a better solution that conforms
          # to the Rack::Standard
          if Proc === body
            body.call(response)
          else
            body.each { |part|
              response.body << part
            }
          end
          response.finished
        ensure
          body.close  if body.respond_to? :close
        end
      end
    end
  end
end

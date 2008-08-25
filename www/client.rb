require 'rubygems'
require 'socket'
require 'json'

module Rebar
  class Erlang
    def initialize(mod, address, port)
      @mod = mod.to_s
      @address = address
      @port = port
    end
  
    def method_missing(*args)
      method, *params = args
      rpc(method, params)
    end
  
    def marshal(fun, args)
      {:method => @mod + ":" + fun.to_s, :params => args, :id => 0}.to_json
    end
  
    def demarshal(str)
      s = JSON.parse(str)
      p s
      s["result"]
    end
  
    def rpc(fun, args)
      json_request_string = marshal(fun, args)
      json_response_string = nil
      begin
        @sock = TCPSocket.new(@address, @port)
        @sock.write(json_request_string)
        json_response_string = @sock.gets
      rescue
        raise
      end
      demarshal(json_response_string)
    end
  end
end

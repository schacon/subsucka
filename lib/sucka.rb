#! /usr/bin/env ruby
require 'rubygems'
require 'EC2'
require 'hpricot'
require 'yaml'
require 'net/ssh'
require 'net/scp'

class Sucka
  
  def initialize(args, options)
    load
    @args = args
    @options = options
    @amazon = EC2::Base.new(:access_key_id => options[:access_key], 
                            :secret_access_key => @options[:secret_key])
  end
  
  def load
    if File.exists?('sucka.yml')
      @data = YAML::load( File.open( 'sucka.yml' ) )
    else
      @data = {}
    end
  end

  def save
    File.open('sucka.yml', 'w') { |f| f.write( @data.to_yaml) }
  end

  def node_data
    data = {}
    doc = @amazon.describe_instances
    if doc.reservationSet
      doc.reservationSet.item.each do |item|
        item.instancesSet.item.each do |inst|
          data[inst.instanceId] = inst
        end
      end
    end
    data
  end
  
  def list_nodes
    puts "domain : .compute-1.amazonaws.com"
    puts
    if @args[1] == 'loop'
      while true
        do_list_nodes
        puts
        sleep 5
      end
    else
      do_list_nodes
    end
  end
  
  def do_list_nodes
    data = node_data
    data.each do |key, inst|
      role = @data[inst.instanceId]['role'] rescue ''
      puts [role.ljust(10), inst.instanceId, 
        trunc(inst.privateDnsName,22), trunc(inst.dnsName, 18),
        inst.instanceType, inst.instanceState.name].join("\t")
    end
  end
  
  def trunc(name, size = 30)
    name[0, size] rescue nil
  end
  
  def redirect(command)
    "#{command} >>/tmp/log 2>>/tmp/log&"
  end
  
  def start_master
    node = @args[1]
    @data[node] ||= {}
    @data[node]['role'] = 'master'
    puts "attach #{node}"
  
    data = node_data
  
    scp_node(node) do |scp|
      scp.upload! "sucka.tgz", "/tmp/sucka.tgz"
    end
    
    puts command = ['cd /opt/sucka/;', 
                'erl', '-detached', '-boot start_sasl', "-name master@#{master_name}", 
                '-smp enable', '-setcookie subsucka', '-pa ./ebin', 
                '-run subsucka start'].join(' ')
                
    on_node(node) do |ssh|
      ssh.exec!(kill_command('erlang')) 
      ssh.exec!('mkdir -p /opt/sucka; mv /tmp/sucka.tgz /opt/sucka/')
      ssh.exec!('cd /opt/sucka; tar xzpvf sucka.tgz; rake')
      result = ssh.exec!(command)
    end
  end

  def start_node
    node = @args[1]
    
    data = node_data
    node_info = data[node]
    
    @data[node] ||= {}
    @data[node]['role'] = 'node'
    puts "attach #{node}"
    
    node_info = data[node]
    node_name = node + '@' + node_info.privateDnsName
       
    scp_node(node) do |scp|
      scp.upload! "sucka.tgz", "/tmp/sucka.tgz"
    end
    
    puts command = ['cd /opt/sucka/;', 
                'erl', '-detached', '-boot start_sasl', "-name #{node_name}", 
                '-smp enable', '-setcookie subsucka', '-pa ./ebin', 
                "-run net_adm world"].join(' ')
                
    on_node(node) do |ssh|
      ssh.exec!(kill_command('erlang')) 
      ssh.exec!('mkdir -p /opt/sucka; mv /tmp/sucka.tgz /opt/sucka/')
      ssh.exec!("cd /opt/sucka; tar xzpvf sucka.tgz; rake; echo \"'#{master_name}'.\" > .hosts.erlang")
      result = ssh.exec!(command)
    end
  end
  
  # this is not being used yet, but I'd like to get it in
  def reassociate
    reassociate = true
    
    resp = @amazon.describe_addresses
    resp.addressesSet.item.each do |it|
      puts it.inspect
      if it.publicIp == @options[:master][:ip]
        puts 'IP MATCH'
        if it.instanceId == node
          puts 'NODE MATCH'
          reassociate = false
        end
      end
    end
    
    # attach ip address
    if reassociate
      puts "dissassociate "
      @amazon.disassociate_address(:public_ip => @options[:master][:ip])
      puts "associate "
      @amazon.associate_address(:instance_id => node, :public_ip => @options[:master][:ip])
    end
  end
  
  def master_name
    data = node_data
    @data.each do |node, arr|
      node_info = data[node]
      if arr['role'] == 'master'
        return node_info.privateDnsName
      end
    end
  end


  def kill_command(match)
    "ps ax | grep #{match} | awk '{print $1}' | xargs kill -9"
  end
  
  def haproxy_config
    conf = "global
        log 127.0.0.1   local0
        log 127.0.0.1   local1 notice
        maxconn 4096

defaults
        log     global
        mode    http
        option  httplog
        option  dontlognull
        retries 3
        redispatch
        maxconn 2000
        contimeout      5000
        clitimeout      50000
        srvtimeout      50000

listen webfarm *:80
       mode http
       stats enable
       stats auth #{@options[:proxy][:stats_user]}:#{@options[:proxy][:stats_pwd]}
       balance roundrobin
       cookie JSESSIONID prefix
       option httpclose
       option forwardfor
       option httpchk HEAD /robots.txt HTTP/1.0
"
    data = node_data
    
    cnt = 0
    @data.each do |node, arr|
      node_info = data[node]
      if arr['role'] == 'faceplate'
        cnt += 1
        conf += "       server web#{cnt} #{node_info.privateDnsName}:8080 cookie A#{cnt} check\n"
      end
    end
    conf
  end

  def scp_node(node)
    addr, ssh_options = get_ssh_opt(node)
    Net::SCP.start(addr, "root", ssh_options) do |ssh|
      yield ssh
    end
  end
  
  def on_node(node)
    addr, ssh_options = get_ssh_opt(node)
    pp addr
    pp 'root'
    pp ssh_options
    Net::SSH.start(addr, "root", ssh_options) do |ssh|
      yield ssh
    end
  end

  def get_ssh_opt(node)
    data = node_data
    node_info = data[node]
    ssh_options = {}
    ssh_options[:keys] = [@options[:keypair]] 
    #ssh_options[:verbose] = :debug
    addr = node_info.dnsName
    [addr, ssh_options]
  end
  
  def spin
    if !@args[1]
      puts "available types:"
      puts "INST TYPE   RAM(GB)        CU      DISK      ARCH     COST/H"
      image_sizes.each do |size|
        puts size.map { |v| v.to_s.rjust(9) }.join(' ')
      end
    else
      type = @args[1] || 'm1.small'
      count = @args[2] || 1
    
      image = image_sizes.assoc(type)
      img_id = @options[:amis][image[4]]
      @amazon.run_instances( :image_id => img_id, 
                      :key_name => @options[:keyname],
                      :instance_type => type )
      puts "Instance #{img_id} started"
      list_nodes
    end
  end
  
  def test_load
    url = @args[1] || 'http://kronos.gitcasts.com/'
    number = 5000
    concur = 1000
    puts command = "ab -n #{number} -c #{concur} #{url}"
  end
  
  def go
    case @args[0]
    when 'list':
      list_nodes
    when 'start_master':
      start_master
    when 'start_node':
      start_node
    when 'test':
      test_load
    when 'spin':
      spin
    else
      puts 'not a command.  please choose one of these:'
      puts ['start_master', 'start_node', 'spin', 'list'].join(", ")
    end
    save
  end
  
  # handle, ram, cpu, disk, arch, cost
  def image_sizes
    [
      [ 'm1.small',  1.7,  1,  160, 32, 0.10],
      [ 'm1.large',  7.5,  4,  850, 64, 0.40],
      ['m1.xlarge', 15.0,  8, 1690, 64, 0.80],
      ['c1.medium',  1.7,  5,  350, 32, 0.20],
      ['c1.xlarge',  7.0, 20, 1690, 64, 0.80]
    ]
  end
  
end

=begin  
  <instanceId>i-d511dabc</instanceId>
  <imageId>ami-f21aff9b</imageId>
  <instanceState>
      <code>16</code>
      <name>running</name>
  </instanceState>
  <privateDnsName>domU-12-31-38-00-E0-52.compute-1.internal</privateDnsName>
  <dnsName>ec2-75-101-214-130.compute-1.amazonaws.com</dnsName>
  <reason/>
  <keyName>gsg-keypair</keyName>
  <amiLaunchIndex>0</amiLaunchIndex>
  <productCodes/>
  <instanceType>m1.large</instanceType>
  <launchTime>2008-06-05T02:11:12.000Z</launchTime>
  <placement>
      <availabilityZone>us-east-1a</availabilityZone>
  </placement>
  <kernelId>aki-b51cf9dc</kernelId>
  <ramdiskId>ari-b31cf9da</ramdiskId>
=end
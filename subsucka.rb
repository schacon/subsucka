require 'rubygems'
require 'peach'

require 'fileutils'
require 'tempfile'
require 'pp'

# SubSucka prototype

class SubSucka
  
  attr_accessor :url, :revisions, :result_url
  
  # gets a request with a svn repo url
  def initialize(url, clients = 5, run_real = true)
    @url = url
    if get_info && run_real
      federate_revisions(clients)
      run_clients
      @result_url = combine_results
    else
      puts 'Cannot contact server'
    end
  end
  
  # contacts the repo and queries max revision number
  def get_info
    results = `svn info #{@url}`
    if m = /Revision: (\d+)/.match(results)
      @revisions = m[1].to_i
      return true
    end
    false
  end
  
  # sees how many clients are live and federates the revisions
  def federate_revisions(clients)
    puts "rev: #{@revisions}"
    
    per_client = (@revisions / clients).round + 1
    end_rev = 0
    @client_revs = []
    while(end_rev < @revisions)
      start_rev = end_rev + 1
      end_rev += per_client
      end_rev = @revisions if end_rev > @revisions
      @client_revs << [start_rev, end_rev]
    end
  end
  
  # sends each client it’s job (url, start_rev, end_rev)
  def run_clients
    @repos = []
    @client_revs.peach do |start_rev, end_rev|
      dir = new_dir
      puts "#{dir} : #{start_rev}"

      command_with_dir('git init', dir)      
      command_with_dir('echo ".svn" > .gitignore', dir)      
      puts command_with_dir("svn co #{@url} -r #{start_rev} .", dir)
      command_with_dir("git add .; git commit -m \"r#{start_rev}\"", dir)
      (start_rev + 1).upto(end_rev) do |rev|
        puts command_with_dir("svn update -r #{rev}", dir)
        command_with_dir("git add .; git commit -m \"r#{rev}\"", dir)
      end
      @repos << [start_rev, dir]
    end
    
    pp @repos
  end

  
  def combine_results
    info = get_repo_log
    dir = new_dir
    Dir.chdir(dir) do
      `git init`
      r = @repos.sort
      first = r.shift
      `git clone #{first[1]} repo`
      branches = ['master']
      
      dir = File.join(dir, 'repo')
      Dir.chdir(dir) do
        r.each do |num, path|
          `git remote add r#{num} #{path}`
          `git fetch r#{num}`
          branches << "r#{num}/master"
        end
        
        last_commit = nil
        branches.each do |branch|
          commits = `git log --reverse --pretty=format:"%T:%s" #{branch}`.split("\n")
          commits = commits.map { |line| line.split(":") }
          commits.each do |tree_sha, rev_id|
            puts "rewriting : #{tree_sha} #{rev_id}"
            if (rev_data = info.assoc(rev_id))
              ENV['GIT_AUTHOR_NAME'] = ENV['GIT_COMMITTER_NAME'] = rev_data[1]
              ENV['GIT_AUTHOR_EMAIL'] = ENV['GIT_COMMITTER_EMAIL'] = rev_data[1] + '@email.com'
              ENV['GIT_AUTHOR_DATE'] = ENV['GIT_COMMITTER_DATE'] = rev_data[2]
              comment = Tempfile.new('comment')
              comment.write(rev_data[4])
              comment.close
              parent = ''
              parent = "-p #{last_commit}" if last_commit
              puts last_commit = `git commit-tree #{tree_sha} #{parent} < #{comment.path}`.strip
            end
          end
        end
        
        `git update-ref refs/heads/master #{last_commit}`
        
        branches.each do |branch|
          next if branch == 'master'
          `git remote rm #{branch.gsub('/master', '')}`
        end
        
        `git prune`
        `git pack-refs`
        `git gc --aggressive`
        
      end
    end
    dir
  end
  
  def combine_repos(arr)
    @repos = arr
    combine_results
  end
  
  def get_repo_log
    data = []
    log = `svn log #{@url}`
    commits = log.split('------------------------------------------------------------------------')
    commits.each do |commit|
      lines = commit.split("\n")
      next if !lines.shift
      info = lines.shift.split("|").map { |e| e.strip }
      rev, author, date, ln = info
      lines.shift #blank
      message = lines.join("\n")
      data << [rev, author, date, ln, message]
    end
    data
  end
  
  private
  
  def new_dir
    t = Tempfile.new('subsucka')
    path = t.path
    t.unlink
    FileUtils.mkdir_p(path)
    path
  end
  
  def command_with_dir(command, dir)
    c = []
    c << "cd #{dir}"
    c << command
    c = c.join('; ')
    `#{c}`
  end
  
end

#SubSucka.new('http://bluecove.googlecode.com/svn/trunk', 4)

s = SubSucka.new('http://garmini.googlecode.com/svn/trunk', 4)
puts
puts 'Git Repo:'
puts s.result_url

#arr = [[19, "/var/folders/rX/rXo+hpkxGhuGDjRsHj5Plk+++TI/-Tmp-/subsucka.97991.3"],
# [1, "/var/folders/rX/rXo+hpkxGhuGDjRsHj5Plk+++TI/-Tmp-/subsucka.97991.0"],
# [7, "/var/folders/rX/rXo+hpkxGhuGDjRsHj5Plk+++TI/-Tmp-/subsucka.97991.1"],
# [13, "/var/folders/rX/rXo+hpkxGhuGDjRsHj5Plk+++TI/-Tmp-/subsucka.97991.2"]]
#s = SubSucka.new('http://garmini.googlecode.com/svn/trunk', 4, false)
#puts s.combine_repos(arr)
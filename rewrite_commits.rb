#! /usr/bin/env ruby

require 'fileutils'
require 'tempfile'
require 'pp'

class SubSucka
  
  attr_accessor :repo, :svn_url
  
  def initialize(repo, svn_url)
    @repo = repo
    @svn_url = svn_url
  end
  
  def build
    build_repo_log
    build_tree_rev_shas    
    write_commits
    finish_repo
  end

  def get_branch_list(br)
    branches = `svn list #{@svn_url}/#{br}`.split("\n").map { |b| b.gsub('/', '') }
  end
    
  def write_commits
    last_commits = {}
    Dir.chdir(@repo) do     
      @repo_log.reverse.each do |commit|
        next if !(commit_sha = @tree_rev_shas[commit[0]])
        ENV['GIT_AUTHOR_NAME']  = ENV['GIT_COMMITTER_NAME']  = commit[1]
        ENV['GIT_AUTHOR_EMAIL'] = ENV['GIT_COMMITTER_EMAIL'] = commit[1] + '@email.com'
        ENV['GIT_AUTHOR_DATE']  = ENV['GIT_COMMITTER_DATE']  = commit[2]
        comment = Tempfile.new('comment')       # |
        comment.write(commit[4])                # |
        comment.close                           # `-- prepare commit meta-data

        ref = commit[5]
        name = commit[6]
        ref = File.join(ref, name) if ref != 'trunk'
        
        parent = ''
        last_commit = last_commits[ref] || last_commits['trunk']
        parent = "-p #{last_commit}" if last_commit

        tree_sha = `git rev-parse #{commit_sha}:#{ref}`.strip
        
        last_commits[ref] = `git commit-tree #{tree_sha} #{parent} < #{comment.path}`.strip
      end
      
      # write the last commits heads
      last_commits.each do |ref, sha|
        if ref == 'trunk'
          ref = 'heads/master' 
        else
          ref = ref.gsub('branches', 'heads')
        end
        `git update-ref refs/#{ref} #{sha}`
      end
      
    end
  end
  
  def build_tree_rev_shas
    tree_exists = true
    @tree_rev_shas = {}
    @remotes = []
    Dir.chdir(@repo) do 
      remotes = `git remote`.split("\n")
      remotes.each do |remote|
        @remotes << remote
        branch = "#{remote}/master"
        commits = `git log --reverse --pretty=format:"%H:%s" #{branch}`.split("\n")
        commits = commits.map { |line| line.split(":") }
        commits.each do |tree_sha, rev_id|
          @tree_rev_shas[rev_id] = tree_sha
        end
      end
    end
    @tree_rev_shas
  end
  
  def build_repo_log(branch = '')
    @repo_log = []
    log = `svn log --verbose #{@svn_url}`
    commits = log.split('------------------------------------------------------------------------')
    commits.each do |commit|
      lines = commit.split("\n")
      next if !lines.shift
      info = lines.shift.split("|").map { |e| e.strip }
      rev, author, date, ln = info
      line = lines.shift # Changed paths:
      ref = ''
      while ((line = lines.shift.chomp) != '') do
        ign, ref, name = line.split('/')
      end rescue nil
      message = lines.join("\n")
      if (ref == 'trunk')
        name = nil 
      else
        name = name.split(' ').first if name
      end
      @repo_log << [rev, author, date, ln, message, ref, name]
    end
    @repo_log
  end
  
  def finish_repo
    Dir.chdir(@repo) do     
      # remove all the temporary remote branches we were using
      @remotes.each do |remote|
        `git remote rm #{remote}`
      end
  
      # pack up the repository and remove any unreferenced objects we've left behind
      `git prune`
      `git pack-refs`
      `git gc --aggressive`
    end
  end
  
end

repo_dir = ARGV[0]
svn_url  = ARGV[1]

sk = SubSucka.new(repo_dir, svn_url)
sk.build

# go through all the remotes and list tree subshas -> rX

# get author mapping

# get list for trunk

# get list for branches
# svn list branches
# get log for each branch

# get list for tags
# svn list branches
# get log for each tag



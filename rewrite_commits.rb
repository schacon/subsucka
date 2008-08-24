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
    build_tree_rev_shas
    write_trunk
    write_refs('branches', 'heads')
    write_refs('tags', 'tags')
    finish_repo
  end

  def write_trunk
    info = get_repo_log('trunk')
    write_commits(info, 'heads/master')
  end

  def write_refs(svn, git)
    pp branches = get_branch_list(svn)
    branches.each do |branch|
      info = get_repo_log("#{svn}/#{branch}")
      puts branch
      puts info.size
      write_commits(info, "#{git}/#{branch}")
    end
  end

  def get_branch_list(br)
    branches = `svn list #{@svn_url}/#{br}`.split("\n")
  end
    
  def write_commits(info, ref)
    return if info.size < 1
    last_commit = nil
    Dir.chdir(@repo) do     
      info.reverse.each do |commit|
        next if !(tree_sha = @tree_rev_shas[commit[0]])
        ENV['GIT_AUTHOR_NAME']  = ENV['GIT_COMMITTER_NAME']  = commit[1]
        ENV['GIT_AUTHOR_EMAIL'] = ENV['GIT_COMMITTER_EMAIL'] = commit[1] + '@email.com'
        ENV['GIT_AUTHOR_DATE']  = ENV['GIT_COMMITTER_DATE']  = commit[2]
        comment = Tempfile.new('comment')       # |
        comment.write(commit[4])                # |
        comment.close                           # `-- prepare commit meta-data
        parent = ''
        parent = "-p #{last_commit}" if last_commit
        last_commit = `git commit-tree #{tree_sha} #{parent} < #{comment.path}`.strip
      end
      `git update-ref refs/#{ref} #{last_commit}`
    end
  end
  
  def build_tree_rev_shas
    @tree_rev_shas = {}
    @remotes = []
    Dir.chdir(@repo) do 
      remotes = `git remote`.split("\n")
      remotes.each do |remote|
        @remotes << remote
        branch = "#{remote}/master"
        commits = `git log --reverse --pretty=format:"%T:%s" #{branch}`.split("\n")
        commits = commits.map { |line| line.split(":") }
        commits.each do |tree_sha, rev_id|
          @tree_rev_shas[rev_id] = tree_sha
        end
      end
    end
  end
  
  def get_repo_log(branch = '')
    data = []
    log = `svn log #{@svn_url}/#{branch}`
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
  
  def finish_repo
    return false
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



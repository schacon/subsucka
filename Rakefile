task :default do
  sh "erlc +debug_info -W2 -o ebin #{Dir["src/*.erl"].join(" ")}"
end

task :c => [:default] do
  sh "erl -boot start_sasl -pa ./ebin -pa ./ebin/*/ebin"
end

task :slave => [:default] do
  sh "erl -sname slave1 -setcookie subsucka -pa ./ebin"
end

task :slave2 => [:default] do
  sh "erl -sname slave2 -setcookie subsucka -pa ./ebin"
end

task :server => [:default] do
  sh "erl -sname master1 -setcookie subsucka -pa ./ebin"
end

task :default do
  sh "erlc +debug_info -W2 -o ebin #{Dir["src/*.erl"].join(" ")}"
end

task :c => [:default] do
  sh "erl -boot start_sasl -pa ./ebin -pa ./ebin/*/ebin"
end

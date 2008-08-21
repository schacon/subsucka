{application, subsucka_server,
 [{description, "Moving you from SVN even faster!"},
  {vsn, "0.1.0"},
  {modules, [subsucka_server, subsucka_serer_app, slave_manager]},
  {registered, [slave_manager]},
  {applications, [kernel, stdlib]},
  {mod, {subsucka_server_app, []}}
]}.

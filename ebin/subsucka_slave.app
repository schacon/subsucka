{application, subsucka_slave,
 [{description, "Moving you from SVN even faster!"},
  {vsn, "0.1.0"},
  {modules, [subsucka_slave, subsucka_slave_app, subversion_import]},
  {registered, [subversion_import]},
  {applications, [kernel, stdlib]},
  {mod, {subsucka_slave_app, []}}
]}.
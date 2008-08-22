{application, subsucka,
 [{description, "Moving you from SVN even faster!"},
  {vsn, "0.1.0"},
  {modules, [subsucka, subsucka_app, subversion_import]},
  {registered, [subversion_import]},
  {applications, [kernel, stdlib]},
  {mod, {subsucka_app, []}}
]}.

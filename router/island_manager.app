{application, island_manager,
 [{description, "The island manager application."},
  {vsn, "0.1"},
  {modules, [island_manager, island_manager_sup, island_manager_http_server]},
  {registered, [island_manager]},
  {env, [
    {port, 6666},
    {working_dir, "/tmp"}
  ]},
  {applications, [kernel, stdlib]},
  {mod, {island_manager, []}}
 ]}.

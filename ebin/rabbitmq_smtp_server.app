{application,rabbitmq_smtp_server,
 [{description,"RabbitMQ SMTP Server"},
  {vsn, "0.0"},
  {modules,[
	    generic_tcp_server,
	    smtp_server,
	    smtp_server_session,
	    rabbitmq_smtp_server
	   ]},
  {registered, []},
  {applications,[kernel,stdlib]},
  {mod, {rabbitmq_smtp_server, []}},
  {env, [{listen_host, "0.0.0.0"},
	 {listen_port, 8025}]}
 ]}.

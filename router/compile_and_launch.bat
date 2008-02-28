erlc croq_utils.erl croq_logger.erl croq_policy_server.erl croq_router.erl croq_snapshot_server.erl
erl -eval "croq_router:start(5000,5001,5002)."
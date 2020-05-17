require 'pedant/rspec/common'
require 'mixlib/shellout'
require 'base64'

describe "/_stats API endpoint", :stats do

  ERLANG_RESPONSE_TYPE_MAP = {
    "erlang_vm_time_correction" => "UNTYPED",
    "erlang_vm_thread_pool_size" => "GAUGE",
    "erlang_vm_threads" => "UNTYPED",
    "erlang_vm_smp_support" => "UNTYPED",
    "erlang_vm_schedulers_online" => "GAUGE",
    "erlang_vm_schedulers" => "GAUGE",
    "erlang_vm_process_limit" => "GAUGE",
    "erlang_vm_process_count" => "GAUGE",
    "erlang_vm_port_limit" => "GAUGE",
    "erlang_vm_port_count" => "GAUGE",
    "erlang_vm_logical_processors_online" => "GAUGE",
    "erlang_vm_logical_processors_available" => "GAUGE",
    "erlang_vm_logical_processors" => "GAUGE",
    "erlang_vm_ets_limit" => "GAUGE",
    "erlang_vm_statistics_wallclock_time_milliseconds" => "COUNTER",
    "erlang_vm_statistics_runtime_milliseconds" => "COUNTER",
    "erlang_vm_statistics_run_queues_length_total" => "GAUGE",
    "erlang_vm_statistics_reductions_total" => "COUNTER",
    "erlang_vm_statistics_bytes_output_total" => "COUNTER",
    "erlang_vm_statistics_bytes_received_total" => "COUNTER",
    "erlang_vm_statistics_garbage_collection_bytes_reclaimed" => "COUNTER",
    "erlang_vm_statistics_garbage_collection_words_reclaimed" => "COUNTER",
    "erlang_vm_statistics_garbage_collection_number_of_gcs" => "COUNTER",
    "erlang_vm_statistics_context_switches" => "COUNTER",
    "erlang_vm_memory_system_bytes_total" => "GAUGE",
    "erlang_vm_memory_processes_bytes_total" => "GAUGE",
    "erlang_vm_memory_ets_tables" => "GAUGE",
    "erlang_vm_memory_dets_tables" => "GAUGE",
    "erlang_vm_memory_bytes_total" => "GAUGE",
    "erlang_vm_memory_atom_bytes_total" => "GAUGE",
    "erchef_pooler_queued_requestors_max" => "GAUGE",
    "erchef_pooler_queued_requestors" => "GAUGE",
    "erchef_pooler_members_max" => "GAUGE",
    "erchef_pooler_members_free" => "GAUGE",
    "erchef_pooler_members_in_use" => "GAUGE",
    "erlang_vm_allocators" => "GAUGE",
    "erlang_vm_atom_count" => "GAUGE",
    "erlang_vm_atom_limit" => "GAUGE",
    "erlang_vm_dirty_cpu_schedulers" => "GAUGE",
    "erlang_vm_dirty_cpu_schedulers_online" => "GAUGE",
    "erlang_vm_dirty_io_schedulers" => "GAUGE",
    "erlang_vm_msacc_aux_seconds_total" => "COUNTER",
    "erlang_vm_msacc_check_io_seconds_total" => "COUNTER",
    "erlang_vm_msacc_emulator_seconds_total" => "COUNTER",
    "erlang_vm_msacc_gc_seconds_total" => "COUNTER",
    "erlang_vm_msacc_other_seconds_total" => "COUNTER",
    "erlang_vm_msacc_port_seconds_total" => "COUNTER",
    "erlang_vm_msacc_sleep_seconds_total" => "COUNTER",
    # "erlang_vm_msacc_alloc_seconds_total" => "COUNTER",
    # "erlang_vm_msacc_bif_seconds_total" => "COUNTER",
    # "erlang_vm_msacc_busy_wait_seconds_total" => "COUNTER",
    # "erlang_vm_msacc_ets_seconds_total" => "COUNTER",
    # "erlang_vm_msacc_gc_full_seconds_total" => "COUNTER",
    # "erlang_vm_msacc_nif_seconds_total" => "COUNTER",
    # "erlang_vm_msacc_send_seconds_total" => "COUNTER",
    # "erlang_vm_msacc_timers_seconds_total" => "COUNTER",
    "erlang_vm_statistics_dirty_cpu_run_queue_length" => "GAUGE",
    "erlang_vm_statistics_dirty_io_run_queue_length" => "GAUGE",
    "erlang_vm_wordsize_bytes" => "GAUGE",
  }

  PGSTATS_RESPONSE_TYPE_MAP = {
    "pg_stat_n_conns" => "GAUGE",
    "pg_stat_n_active_conns" => "GAUGE",
    "pg_stat_tidx_blks_hit" => "COUNTER",
    "pg_stat_tidx_blks_read" => "COUNTER",
    "pg_stat_toast_blks_hit" => "COUNTER",
    "pg_stat_toast_blks_read" => "COUNTER",
    "pg_stat_idx_blks_hit" => "COUNTER",
    "pg_stat_idx_blks_read" => "COUNTER",
    "pg_stat_heap_blocks_hit" => "COUNTER",
    "pg_stat_heap_blocks_read" => "COUNTER",
    "pg_stat_n_dead_tup" => "GAUGE",
    "pg_stat_n_live_tup" => "GAUGE",
    "pg_stat_n_tup_del" => "COUNTER",
    "pg_stat_n_tup_upd" => "COUNTER",
    "pg_stat_n_tup_ins" => "COUNTER",
    "pg_stat_tup_fetch" => "COUNTER",
    "pg_stat_idx_scan" => "COUNTER",
    "pg_stat_seq_tup_read" => "COUNTER",
    "pg_stat_seq_scan" => "COUNTER",
  }

  MNESIA_RESPONSE_TYPE_MAP = {
    "erlang_mnesia_held_locks" => "GAUGE",
    "erlang_mnesia_lock_queue" => "GAUGE",
    "erlang_mnesia_transaction_participants" => "GAUGE",
    "erlang_mnesia_transaction_coordinators" => "GAUGE",
    "erlang_mnesia_failed_transactions" => "COUNTER",
    "erlang_mnesia_committed_transactions" => "GAUGE",
    "erlang_mnesia_logged_transactions" => "COUNTER",
    "erlang_mnesia_restarted_transactions" => "COUNTER"
  }

  CHEF_INDEX_TYPE_MAP = {
    "chef_elasticsearch_update_count" => "COUNTER",
    "chef_elasticsearch_search_count" => "COUNTER",
    "chef_elasticsearch_delete_search_db_count" => "COUNTER",
    "chef_elasticsearch_delete_search_db_by_type_count" => "COUNTER",
    "chef_elasticsearch_search_with_scroll_count" => "COUNTER",
    # These counters have labels and thus prometheus doesn't output
    # data for them unless we actually increment the counter with a
    # label. Our tests don't seem to hit these code paths currently.
    #
    # "chef_index_http_req_failure_total" => "COUNTER"
    # "chef_elasticsearch_search_with_scroll_resp_count" => "COUNTER",
    "chef_elasticsearch_search_resp_count" => "COUNTER",
    "chef_index_batch_current_batch_size_bytes" => "GAUGE",
    "chef_index_batch_current_batch_doc_count" => "GAUGE",
    "chef_index_batch_inflight_flushes_count" => "GAUGE",
    "chef_index_batch_mailbox_size" => "GAUGE",
    "chef_index_batch_heap_size_bytes" => "GAUGE",
    "chef_index_batch_stack_size_bytes" => "GAUGE",
    "chef_index_batch_memory_size_bytes" => "GAUGE",
    "chef_index_batch_successful_docs_total" => "COUNTER",
    "chef_index_batch_failed_docs_total" => "COUNTER",
    "chef_index_http_req_success_total" => "COUNTER",
  }

  CHEF_INDEX_JSON_TYPE_MAP = {
    "chef_index_batch_queue_latency_ms" => "HISTOGRAM",
    "chef_index_batch_completed_latency_ms" => "HISTOGRAM",
    "chef_index_http_req_duration_ms" => "HISTOGRAM",
    "chef_index_expand_make_doc_for_index_duration_ms" => "HISTOGRAM",
  }

  CHEF_INDEX_PROMETHEUS_TYPE_MAP = {
    "chef_index_batch_queue_latency_ms_bucket" => "HISTOGRAM",
    "chef_index_batch_queue_latency_ms_count" => "HISTOGRAM",
    "chef_index_batch_queue_latency_ms_sum" => "HISTOGRAM",

    "chef_index_batch_completed_latency_ms_bucket" => "HISTOGRAM",
    "chef_index_batch_completed_latency_ms_count" => "HISTOGRAM",
    "chef_index_batch_completed_latency_ms_sum" => "HISTOGRAM",

    "chef_index_http_req_duration_ms_bucket" => "HISTOGRAM",
    "chef_index_http_req_duration_ms_count" => "HISTOGRAM",
    "chef_index_http_req_duration_ms_sum" => "HISTOGRAM",

    "chef_index_expand_make_doc_for_index_duration_ms_bucket" => "HISTOGRAM",
    "chef_index_expand_make_doc_for_index_duration_ms_count" => "HISTOGRAM",
    "chef_index_expand_make_doc_for_index_duration_ms_sum" => "HISTOGRAM",
  }

  SHARED_TYPE_MAP = ERLANG_RESPONSE_TYPE_MAP.merge(CHEF_INDEX_TYPE_MAP)
  RESPONSE_TYPE_MAP = SHARED_TYPE_MAP.merge(CHEF_INDEX_JSON_TYPE_MAP)
  PROMETHEUS_RESPONSE_TYPE_MAP = SHARED_TYPE_MAP.merge(MNESIA_RESPONSE_TYPE_MAP).merge(CHEF_INDEX_PROMETHEUS_TYPE_MAP)

  if Pedant::Config.chef_pgsql_collector
    RESPONSE_TYPE_MAP = RESPONSE_TYPE_MAP.merge(PGSTATS_RESPONSE_TYPE_MAP)
    PROMETHEUS_RESPONSE_TYPE_MAP = PROMETHEUS_RESPONSE_TYPE_MAP.merge(PGSTATS_RESPONSE_TYPE_MAP)
  end

  let(:request_url) { "#{platform.server}/_stats" }
  let(:response_body) do
    RESPONSE_TYPE_MAP.map do |name, type|
      {
        "name" => name,
        "type" => type
      }
    end
  end

  let(:boolean_stats) { ["erlang_vm_time_correction", "erlang_vm_threads", "erlang_vm_smp_support"] }
  let(:auth_headers) do
    { "Authorization" => "Basic " + Base64.strict_encode64("#{platform.stats_user}:#{platform.stats_password}") }
  end

  # Don't turn on any of the tests unless we have a password.
  # These test should not run on the backend of a tiered setup.
  if Pedant::Config.pedant_platform.stats_password && !(Pedant::Config.topology == "tiered" || Pedant::Config.role == "backend")
    it "returns a list of collected statistics", :smoke do
      get(request_url, nil, auth_headers: auth_headers).should look_like({
        :status => 200,
        :body => response_body
      })
    end

    it "returns json when ?format=json", :smoke do
      get(request_url + "?format=json", nil, auth_headers: auth_headers).should look_like({
        :status => 200,
        :body => response_body
      })
    end

    it "returns prometheus output ?format=text", :smoke do
      response = get(request_url + "?format=text", nil, auth_headers: auth_headers,
          headers: { "Accept" => "*/*" })
      names = response.body.split("\n").reduce([]) do |acc, str|
        m = str.strip.match(/^\w+/)
        acc << m[0] if m
        acc
      end
      expect(names.uniq).to match_array(PROMETHEUS_RESPONSE_TYPE_MAP.keys)
    end

    RESPONSE_TYPE_MAP.each do |name, type|
      it "returns metrics for #{name} typed as #{type}" do
        response = JSON.parse(get(request_url, nil, auth_headers: auth_headers))
        stat = response.find { |s| s["name"] == name }
        expect(stat).not_to be_nil
        expect(stat["metrics"]).not_to be_empty
        expect(stat["type"]).to eq(type)
      end
    end
  end
end

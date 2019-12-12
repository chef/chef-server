require_relative '../../libraries/haproxy.rb'

#
# FakeSocket pretends to be an HAProxy staus socket.  It understand
# the following commands:
#
# - show stat
# - show stat -1 4 -1
#
class FakeSocket
  FULL_OUTPUT = <<~OUTPUT.freeze
    # pxname,svname,qcur,qmax,scur,smax,slim,stot,bin,bout,dreq,dresp,ereq,econ,eresp,wretr,wredis,status,weight,act,bck,chkfail,chkdown,lastchg,downtime,qlimit,pid,iid,sid,throttle,lbtot,tracked,type,rate,rate_lim,rate_max,check_status,check_code,check_duration,hrsp_1xx,hrsp_2xx,hrsp_3xx,hrsp_4xx,hrsp_5xx,hrsp_other,hanafail,req_rate,req_rate_max,req_tot,cli_abrt,srv_abrt,comp_in,comp_out,comp_byp,comp_rsp,lastsess,last_chk,last_agt,qtime,ctime,rtime,ttime,
    postgresql,FRONTEND,,,0,0,2000,0,0,0,0,0,0,,,,,OPEN,,,,,,,,,1,2,0,,,,0,0,0,0,,,,,,,,,,,0,0,0,,,0,0,0,0,,,,,,,,
    elasticsearch,FRONTEND,,,0,0,2000,0,0,0,0,0,0,,,,,OPEN,,,,,,,,,1,3,0,,,,0,0,0,0,,,,,,,,,,,0,0,0,,,0,0,0,0,,,,,,,,
    chef_backend_postgresql,4ba4ae0a6686cf6b67590e621459f1e2,0,0,0,0,,0,0,0,,0,,0,0,0,0,DOWN,1,1,0,1,1,535,535,,1,4,1,,0,,2,0,,0,L7STS,503,0,,,,,,,0,,,,0,0,,,,,-1,Service Unavailable,,0,0,0,0,
    chef_backend_postgresql,89cc3a0c2e3e51bdc5db1f97f63deb99,0,0,0,0,,0,0,0,,0,,0,0,0,0,DOWN,1,1,0,1,1,535,535,,1,4,2,,0,,2,0,,0,L7STS,503,1,,,,,,,0,,,,0,0,,,,,-1,Service Unavailable,,0,0,0,0,
    chef_backend_postgresql,7caffa2c440aa682a2abbf4902d35fbd,0,0,0,0,,0,0,0,,0,,0,0,0,0,UP,1,1,0,0,0,535,0,,1,4,3,,0,,2,0,,0,L7OK,200,0,,,,,,,0,,,,0,0,,,,,-1,OK,,0,0,0,0,
    chef_backend_postgresql,BACKEND,0,0,0,0,200,0,0,0,0,0,,0,0,0,0,UP,1,1,0,,0,535,0,,1,4,0,,0,,1,0,,0,,,,,,,,,,,,,,0,0,0,0,0,0,-1,,,0,0,0,0,
    chef_backend_elasticsearch,4ba4ae0a6686cf6b67590e621459f1e2,0,0,0,0,,0,0,0,,0,,0,0,0,0,DOWN,1,1,0,1,1,534,534,,1,5,1,,0,,2,0,,0,L7STS,503,1,,,,,,,0,,,,0,0,,,,,-1,Service Unavailable,,0,0,0,0,
    chef_backend_elasticsearch,89cc3a0c2e3e51bdc5db1f97f63deb99,0,0,0,0,,0,0,0,,0,,0,0,0,0,DOWN,1,1,0,1,1,534,534,,1,5,2,,0,,2,0,,0,L7STS,503,1,,,,,,,0,,,,0,0,,,,,-1,Service Unavailable,,0,0,0,0,
    chef_backend_elasticsearch,7caffa2c440aa682a2abbf4902d35fbd,0,0,0,0,,0,0,0,,0,,0,0,0,0,UP,1,1,0,0,0,535,0,,1,5,3,,0,,2,0,,0,L7OK,200,0,,,,,,,0,,,,0,0,,,,,-1,OK,,0,0,0,0,
    chef_backend_elasticsearch,BACKEND,0,0,0,0,200,0,0,0,0,0,,0,0,0,0,UP,1,1,0,,0,535,0,,1,5,0,,0,,1,0,,0,,,,,,,,,,,,,,0,0,0,0,0,0,-1,,,0,0,0,0,

  OUTPUT

  SERVER_ONLY_OUTPUT = <<~OUTPUT.freeze
    # pxname,svname,qcur,qmax,scur,smax,slim,stot,bin,bout,dreq,dresp,ereq,econ,eresp,wretr,wredis,status,weight,act,bck,chkfail,chkdown,lastchg,downtime,qlimit,pid,iid,sid,throttle,lbtot,tracked,type,rate,rate_lim,rate_max,check_status,check_code,check_duration,hrsp_1xx,hrsp_2xx,hrsp_3xx,hrsp_4xx,hrsp_5xx,hrsp_other,hanafail,req_rate,req_rate_max,req_tot,cli_abrt,srv_abrt,comp_in,comp_out,comp_byp,comp_rsp,lastsess,last_chk,last_agt,qtime,ctime,rtime,ttime,
    chef_backend_postgresql,4ba4ae0a6686cf6b67590e621459f1e2,0,0,0,0,,0,0,0,,0,,0,0,0,0,DOWN,1,1,0,1,1,890,890,,1,4,1,,0,,2,0,,0,L7STS,503,0,,,,,,,0,,,,0,0,,,,,-1,Service Unavailable,,0,0,0,0,
    chef_backend_postgresql,89cc3a0c2e3e51bdc5db1f97f63deb99,0,0,0,0,,0,0,0,,0,,0,0,0,0,DOWN,1,1,0,1,1,890,890,,1,4,2,,0,,2,0,,0,L7STS,503,0,,,,,,,0,,,,0,0,,,,,-1,Service Unavailable,,0,0,0,0,
    chef_backend_postgresql,7caffa2c440aa682a2abbf4902d35fbd,0,0,0,0,,0,0,0,,0,,0,0,0,0,UP,1,1,0,0,0,890,0,,1,4,3,,0,,2,0,,0,L7OK,200,0,,,,,,,0,,,,0,0,,,,,-1,OK,,0,0,0,0,
    chef_backend_elasticsearch,4ba4ae0a6686cf6b67590e621459f1e2,0,0,0,0,,0,0,0,,0,,0,0,0,0,DOWN,1,1,0,1,1,889,889,,1,5,1,,0,,2,0,,0,L7STS,503,0,,,,,,,0,,,,0,0,,,,,-1,Service Unavailable,,0,0,0,0,
    chef_backend_elasticsearch,89cc3a0c2e3e51bdc5db1f97f63deb99,0,0,0,0,,0,0,0,,0,,0,0,0,0,DOWN,1,1,0,1,1,889,889,,1,5,2,,0,,2,0,,0,L7STS,503,0,,,,,,,0,,,,0,0,,,,,-1,Service Unavailable,,0,0,0,0,
    chef_backend_elasticsearch,7caffa2c440aa682a2abbf4902d35fbd,0,0,0,0,,0,0,0,,0,,0,0,0,0,UP,1,1,0,0,0,890,0,,1,5,3,,0,,2,0,,0,L7OK,200,0,,,,,,,0,,,,0,0,,,,,-1,OK,,0,0,0,0,

  OUTPUT

  def initialize
    @output = StringIO.new
  end

  def puts(args)
    @output = case args
              when 'show stat'
                StringIO.new(FULL_OUTPUT)
              when 'show stat -1 4 -1'
                StringIO.new(SERVER_ONLY_OUTPUT)
              else
                @output
              end
    args.length
  end

  def gets
    @output.gets
  end
end

describe HAProxyStatus do
  let(:socket) do
    FakeSocket.new
  end
  let(:subject) { HAProxyStatus.new(socket) }

  describe '#stats' do
    it 'returns the parsed version of each line of data' do
      expect(subject.stats).to eq([{ pxname: 'postgresql', svname: 'FRONTEND', status: 'OPEN' },
                                   { pxname: 'elasticsearch', svname: 'FRONTEND', status: 'OPEN' },
                                   { pxname: 'chef_backend_postgresql',
                                     svname: '4ba4ae0a6686cf6b67590e621459f1e2',
                                     status: 'DOWN' },
                                   { pxname: 'chef_backend_postgresql',
                                     svname: '89cc3a0c2e3e51bdc5db1f97f63deb99',
                                     status: 'DOWN' },
                                   { pxname: 'chef_backend_postgresql',
                                     svname: '7caffa2c440aa682a2abbf4902d35fbd',
                                     status: 'UP' },
                                   { pxname: 'chef_backend_postgresql', svname: 'BACKEND', status: 'UP' },
                                   { pxname: 'chef_backend_elasticsearch',
                                     svname: '4ba4ae0a6686cf6b67590e621459f1e2',
                                     status: 'DOWN' },
                                   { pxname: 'chef_backend_elasticsearch',
                                     svname: '89cc3a0c2e3e51bdc5db1f97f63deb99',
                                     status: 'DOWN' },
                                   { pxname: 'chef_backend_elasticsearch',
                                     svname: '7caffa2c440aa682a2abbf4902d35fbd',
                                     status: 'UP' },
                                   { pxname: 'chef_backend_elasticsearch', svname: 'BACKEND', status: 'UP' }])
    end
  end

  describe '#server_stats' do
    it "sends the 'show stat' with the correct arguments" do
      expect(socket).to receive(:puts).with('show stat -1 4 -1')
      subject.server_stats
    end

    it 'returns the parsed version of each line of data' do
      expect(subject.server_stats).to eq([{ pxname: 'chef_backend_postgresql',
                                            svname: '4ba4ae0a6686cf6b67590e621459f1e2',
                                            status: 'DOWN' },
                                          { pxname: 'chef_backend_postgresql',
                                            svname: '89cc3a0c2e3e51bdc5db1f97f63deb99',
                                            status: 'DOWN' },
                                          { pxname: 'chef_backend_postgresql',
                                            svname: '7caffa2c440aa682a2abbf4902d35fbd',
                                            status: 'UP' },
                                          { pxname: 'chef_backend_elasticsearch',
                                            svname: '4ba4ae0a6686cf6b67590e621459f1e2',
                                            status: 'DOWN' },
                                          { pxname: 'chef_backend_elasticsearch',
                                            svname: '89cc3a0c2e3e51bdc5db1f97f63deb99',
                                            status: 'DOWN' },
                                          { pxname: 'chef_backend_elasticsearch',
                                            svname: '7caffa2c440aa682a2abbf4902d35fbd',
                                            status: 'UP' }])
    end
  end
end

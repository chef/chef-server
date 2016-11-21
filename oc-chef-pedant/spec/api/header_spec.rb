# -*- coding: utf-8 -*-
#
# Author:: Douglas Triggs (<doug@chef.io>)
# Copyright:: Copyright (c) 2014 Chef, Inc.

describe "Headers", :headers do
  let (:request_url) { api_url("users") }
  let (:requestor) { platform.admin_user }


  context "Request Headers" do

    # Right, now, we're just checking header versions, this can also be a template
    # for future header tests if we need to test anything else.

    # TODO: add some more digits when Chef Client reaches version 999
    # I may be retired by then.
    context "X-Chef-Version" do
      let (:high_version_headers) { {"X-Chef-Version" => "999.0.0" } }
      let (:low_version_headers) { {"X-Chef-Version" => "9.0.1" } }

      it "Accepts High Version" do
        #Pended until backport into 11
        get(request_url, requestor, :headers => high_version_headers).should look_like({
            :status => 200
          })
      end

      it "Rejects Low Version", :validation do
        get(request_url, requestor, :headers => low_version_headers).should look_like({
            :status => 400
          })
      end
    end
    %w{Host For Server}.each do |header|
      context "when the server sees header 'X-Forwarded-#{header}' with multiple hosts" do
        it "accepts it" do
          expect(get(request_url, requestor,
                     :headers => {"X-Forwarded-#{header}" => "abc:443,def"}))
            .to have_status_code 200
        end
      end
    end
  end # context "Request Headers"
end # describe "Headers"

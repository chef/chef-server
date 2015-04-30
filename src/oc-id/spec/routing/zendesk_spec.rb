require 'spec_helper'

describe 'routing to the zendesk controller' do
  it 'routes to show' do
    expect(get('/id/zendesk')).to route_to(
      controller: 'zendesks',
      action: 'show',
    )
  end

  it 'routes to sign_out' do
    expect(get('/id/zendesk/signout')).to route_to(
      controller: 'zendesks',
      action: 'signout',
    )
  end
end

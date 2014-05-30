describe 'routing to the zendesk controller' do
  it 'routes to show' do
    expect(get('/id/zendesk')).to route_to(
      controller: 'zendesks',
      action: 'show',
    )
  end
end

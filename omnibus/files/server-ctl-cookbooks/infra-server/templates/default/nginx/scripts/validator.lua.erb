module("validator", package.seeall)

-- Map to translate HTTP verbs to ngx.HTTP_* numericals
local HTTP_REQ_MAP = {
  GET = ngx.HTTP_GET,
  POST = ngx.HTTP_POST,
}

-- Validate the request against the `/validate` endpoint.
-- method: The allowed HTTP verb as a string. e.g: "GET", "POST"
function validator.validate(method)
  if ngx.var.request_method ~= method then
    ngx.exit(ngx.HTTP_NOT_ALLOWED)
  end

  -- ngx.req.read_body() is required when using ngx.location.capture()
  ngx.req.read_body()

  local res = ngx.location.capture(
    "/organizations/" .. ngx.var.request_org .. "/validate" .. ngx.var.request_uri,
    {
      method = HTTP_REQ_MAP[ngx.req.get_method()],
      always_forward_body = true,
      copy_all_vars = true
    }
  )

  if res.status == ngx.HTTP_OK then
    return
  else
    -- return the validation sub-request error message
    ngx.status = res.status
    ngx.say(res.body)
    ngx.exit(res.status)
  end
end

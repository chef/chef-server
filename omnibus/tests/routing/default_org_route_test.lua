-- Default Org tests.

-- Default Org
table.insert(shared_chef_tests, {"/nodes",                      {TEST_ORG, "erchef", "nodes"}})
table.insert(shared_chef_tests, {"/nodes/",                     {TEST_ORG, "erchef", "nodes"}})
table.insert(shared_chef_tests, {"/environments/envname/nodes",     {TEST_ORG, "erchef", "environments", "envname"}})
table.insert(shared_chef_tests, {"/environments/envname/nodes/",     {TEST_ORG, "erchef", "environments", "envname"}})
table.insert(shared_chef_tests, {"/environments/envname/node",      {TEST_ORG, "erchef", "environments", "envname"}})
table.insert(shared_chef_tests, {"/environments/envname/nodesbad",  {TEST_ORG, "erchef", "environments", "envname"}})
table.insert(shared_chef_tests, {"/search",                     {TEST_ORG, "erchef", "search"}})
table.insert(shared_chef_tests, {"/search/",                    {TEST_ORG, "erchef", "search"}})
table.insert(shared_chef_tests, {"/search/blah",                {TEST_ORG, "erchef", "search"}})
table.insert(shared_chef_tests, {"/search?x=1",                 {TEST_ORG, "erchef", "search"}})

for _k, val in pairs{ "cookbooks", "data" ,"roles", "sandboxes", "environments", "clients", "nodes" } do
table.insert(shared_chef_tests, {"/" .. val,           {TEST_ORG, "erchef", val}})
table.insert(shared_chef_tests, {"/" .. val .. "/",    {TEST_ORG, "erchef", val}})
table.insert(shared_chef_tests, {"/" .. val .. "/abc", {TEST_ORG, "erchef", val, "abc"}})
table.insert(shared_chef_tests, {"/" .. val .. "/abc/subcomponent", {TEST_ORG, "erchef", val, "abc"}})
table.insert(shared_chef_tests, {"/" .. val .. "/b@d!dent", {TEST_ORG, "erchef", val, "b@d!dent"}})
end

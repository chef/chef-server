## Suppressing Credentials Info in Umbrella

You may need to do special testing using AWS credentials which you don't wish to expose in either the branch you have pushed, or in Buildkite logs.  This document outlines one approach to deal with the issue.

1. Add `- aws/chef-engineering` to the `accounts:` sections of the scenarios you are interested in within `chef-server/pipeline.yml` or `chef-server/pipeline-full.yml` or both.

```
     expeditor:
       accounts:
         - aws/chef-cd
+        - aws/chef-engineering
```

2. Add credentials variables to `chef-server/pipeline.sh` (make the appropriate substitutions).  Consider using team or org credentials vs personal credentials if you are adding a permanent pipeline scenario to be used by more than one person.
```
+TF_VAR_aws_access_key="$(vault read -field=access_key_id account/static/aws/chef-engineering/umbrella/chef-server/YOUR_AWS_ACCESS_KEY_ID)"
+TF_VAR_aws_secret_token="$(vault read -field=secret_access_key account/static/aws/chef-engineering/umbrella/chef-server/YOUR_AWS_SECRET_ACCESS_KEY)"
+export TF_VAR_aws_access_key
+export TF_VAR_aws_secret_token
```

3. Add the variables you added in the step above to the `variables.tf` files of the scenarios you are interested in, minus the `TF_VAR_` prefix.
```
chef-server/scenarios/aws/standalone-upgrade/variables.tf
@@ -1,6 +1,19 @@
 #########################################################################
 # AWS
 #########################################################################
+
+variable "aws_access_key" {
+  type        = string
+  description = "AWS access key id"
+  default     = ""
+}
+
+variable "aws_secret_token" {
+  type        = string
+  description = "AWS secret token"
+  default     = ""
+}
+
```

4. Modify the `main.tf` files of the scenarios you are interested in to use your credentials.  Credentials are accessed using `${var.YOUR-VARIABLE-NAME}` (make appropriate substitutions).  Use commands as appropriate to suppress sensitive output such as credentials (`set +exv` and `set -exv`, dump output to `/dev/null`, etc.).
```
resource "null_resource" "chef_server_config" {
       "sudo chown root:root /tmp/dhparam.pem",
       "sudo mv /tmp/chef-server.rb /etc/opscode",
       "sudo mv /tmp/dhparam.pem /etc/opscode",
+      "sudo chmod a+rwx /etc",
+      "sudo chmod a+rwx /etc/opscode",
+      "sudo chmod a+rwx /etc/opscode/chef-server.rb",
+      "sudo chmod a+rwx /etc/environment",
+      "set +exv",
+      "sudo echo -e '\nbookshelf[\"enable\"] = false\nbookshelf[\"vip\"] = \"s3.us-west-2.amazonaws.com\"\nbookshelf[\"external_url\"] = \"https://s3.us-west-2.amazonaws.com\"\nopscode_erchef[\"s3_bucket\"] = \"lbaker-us-west-2\"\nbookshelf[\"access_key_id\"] = \"${var.aws_access_key}\"\nbookshelf[\"secret_access_key\"] = \"${var.aws_secret_token}\"'>>/etc/opscode/chef-server.rb",
+"sudo echo -e '\nAWS_ACCESS_KEY_ID=\"${var.aws_access_key}\"\nAWS_SECRET_ACCESS_KEY=\"${var.aws_secret_token}\"'>>/etc/environment",
       "sudo chef-server-ctl reconfigure --chef-license=accept>>/dev/null",
       "sleep 120",
+      "sudo chef-server-ctl set-secret bookshelf access_key_id ${var.aws_access_key}",
+      "sudo chef-server-ctl set-secret bookshelf secret_access_key ${var.aws_secret_token}",
+      "set -exv",
+      "sudo chef-server-ctl stop opscode-erchef",
+      "sudo chef-server-ctl start opscode-erchef",
+      "sudo chef-server-ctl status",
       "echo -e '\nEND INSTALL CHEF SERVER\n'",
     ]
   }

```

5. Open a ticket with Progress to place your secrets (AWS `access_key_id` and `secret_access_key`) in the vault.

6. Profit.

The Chef Infra Server includes the following object permissions:

| Permission | Description |
| --- | --- |
| **Delete** | Use the **Delete** permission to define which users and groups may delete an object. This permission is required for any user who uses the `knife [object] delete [object_name]` argument to interact with objects on the Chef Infra Server. |
| **Grant** | Use the **Grant** permission to define which users and groups may configure permissions on an object. This permission is required for any user who configures permissions using the **Administration** tab in the Chef management console. |
| **Read** | Use the **Read** permission to define which users and groups may view the details of an object. This permission is required for any user who uses the `knife [object] show [object_name]` argument to interact with objects on the Chef Infra Server. |
| **Update** | Use the **Update** permission to define which users and groups may edit the details of an object. This permission is required for any user who uses the `knife [object] edit [object_name]` argument to interact with objects on the Chef Infra Server and for any Chef Infra Client to save node data to the Chef Infra Server at the conclusion of a Chef Infra Client run. |

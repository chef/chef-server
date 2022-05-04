Use the `layouts/shortcodes/chef-server` directory to store shortcodes for reusable text.

Shortcodes from this repository can be used in documentation from any Chef repository. Store all shortcodes
in the `chef-server` subdirectory to prevent naming collisions and to make shortcodes from this repository easier to find if they
are used in other repositories.

To add a chef-server shortcode to a Markdown page, add `{{% chef-server/FILENAME %}}` without the `.md` file suffix to a page.

See our [shortcode guide](https://docs.chef.io/style_guide/reuse/) for more information.

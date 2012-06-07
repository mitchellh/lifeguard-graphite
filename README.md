# Lifeguard Graphite Data Source

This is a [Lifeguard](https://github.com/mitchellh/lifeguard) plugin that
adds a data source for accessing information from [Graphite](http://graphite.wikidot.com/).

## Installation

To install the plugin, compile it and generate the release folder:

    $ make deps
    $ make rel

There will now be a folder `rel/lifeguard_graphite`. Copy this folder
into your Lifeguard plugin directory and add `lifeguard_graphite` to the
list of enabled plugins for Lifeguard. That's it!

## Usage

Once the plugin is installed, you can use the graphite data source with
your watches. Example to enable this in your `sys.config`:

```erlang
{data_sources, [
    {"graphite", lifeguard_ds_graphite, []}
  ]},
{plugins, [lifeguard_graphite]}
```

Then, you can query it like this from a watch:

```javascript
```

# Hilda

Create a template for your computing infrastructure, register it
with `hilda`, and then tell `hilda` to push/deploy it to an
infrastructure provider like docker or AWS.


## Example

Here is an example template, called `mystack.template`:

```
- Network:
  - Subnet: 172.19.0.1/24
  - Hosts:
    - Host:
        Name: host1
        Boot: ubuntu:bionic
    - Host:
        Name: host2
        Boot: alpine:latest
  - Links:
    - Link:
        Src: host1
        Dst: host2
    - Link:
        Src: host2
        Dst: host1
```

Register the template:

    hilda template create --name mystack --file /path/to/stack.template

See the details of the template:

    hilda template details --name mystack

See all registered templates:

    hilda template list

Delete the template:

    hilda template delete --name mystack

Deploy to the local docker daemon:

    hilda deploy create \
      --name mystack.1 \
      --template mystack \
      --provider docker

Delete the deployment:

    hilda deploy delete \
      --name mystack.1 \
      --template mystack \
      --provider docker

For more, see the help:

    hilda --help


## Developing/contributing

Download the project, e.g.:

    git clone https://.../hilda.git
    cd hilda

Configure the project:

    make setup

Build it:

    make build

Run it:

    cabal new-run hilda

To specify arguments, add two dashes, and then the arguments.
For example, to run the equivalent of `hilda --help`, do this:

    cabal new-run hilda -- --help

To run the equivalent of `hilda template list`, do this:

    cabal new-run hilda -- template list

Change something:

    vim src/Main.hs
    ...

Rebuild:

    make build

Or just re-run (cabal will rebuild automatically if needed):

    cabal new-run hilda ...


## Basic design

The `hilda` tool is a CLI tool. You can give it stack templates (i.e.,
templates that specify what your dev stack should look like), and then
you can have it deploy instantiantions of those templates to various
infrastructure providers (currently you can only deploy to a local
docker daemon, but in principle one could write a library that could
deploy your template to AWS, or GCP, or whatever).

So, the `hilda` tool tracks two things:

* Templates
* Deployments 

It stores data about these things in the XDG app data directory appropriate 
to the runtime system (for instance, it might be `~/conf/.hilda`, or 
`~/.hilda/`, etc). It stores two types of things in the app data directory:

* _Records_ about the templates and deployments it is tracking.
  These records are kept in a local database, which currently is
  just a set of CSV files, stored in the app data directory.
* _Artifacts_ such as templates.

For example, when you register a template with `hilda`, `hilda` will take
the template file you provide, and stash a copy in its local `artifacts`
store (in the app data directory). Then, `hilda` will add a record to a
`templates` table that it keeps in the app data directory too. In that
record, it will note the name you provided for the template, and the
path to where it's stored the template in its local artifacts store.
Later, when you ask `hilda` to list all the templates that are registered
with it, it will look in the `templates` table to get the list of all
templates it knows about. Similarly, when you ask `hilda` to show you
the details/contents of a template, it will look up in the `templates`
table where it has kept its copy of the template in its local artifacts
store. Then it will go to that location, and read the contents of the file.

When you ask `hilda` to deploy a template, it will again look up the
template in the `templates` table, find its copy of the template file,
and read the contents of that file. Then it will pass that information
along to a provider library, which knows how to instantiate the template.
For instance, if you tell `hilda` to deploy a template to docker,
`hilda` will parse the template and pass that off to its docker provider
library, which will proceed to create the appropriate network/containers. 


## Organization of the code

The `src/` folder is broken down as follows:

* `Main.hs` - This is the main entry point into the program.
* `Conf` - General configuration for the application.
  * `Constants.hs` - Constant values are specified here.
  * `Paths.hs` - Various paths that the application needs are defined here.
  * `CLI.hs` - The command line options/flags for the program are defined here.
  * `Dispatch.hs` - This is responsible for dispatching requests from the CLI
    to `Handler`s. It pairs up handlers with command line requests.
* `App` - General application specific code. The modules in here should not
   have to know anything about the interface (be it an API, a CLI, etc.).
   The functions defined here are the core operations that the application
   performs. Any interface that is written should be able to invoke these
   functions to get things done.
* `Handler` - These modules handle requests from the CLI. They gather
  information from the arguments provided by the command line, and then
  dispatch requests off to the `App` libraries. When they get the results
  back, they format the results for presentation back to the CLI.
* `Provider` - These modules are an interface to particular infrastructure
  providers. There is currently a very minimal library for deploying
  to local docker, but one could create more libraries for, say, AWS,
  GCP, Azure, etc. 
* `Lib` - Basic libraries. Think of these as vendor/3rd party libraries.
  * `CommandLine` - Libraries for parsing and dispatching command line args.
  * `DB` - Libraries for interacting with a local database (e.g., a CSV).
  * `IO` - Libraries for doing local I/O (e.g., working with files).
  * `Utils` - Some general utilities, e.g., for strings, result types, etc.


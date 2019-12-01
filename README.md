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
        BootImg: ubuntu:bionic
    - Host:
        Name: host2
        BootImg: alpine:latest
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

Check the status of the deployment:

    hilda deploy status --name mystack.1

See all deployments:

    hilda deploy list

Delete the deployment:

    hilda deploy delete --name mystack.1

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


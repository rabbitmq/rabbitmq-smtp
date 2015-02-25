# RabbitMQ-SMTP, a Legacy SMTP receiver plugin for RabbitMQ

## Legacy Software Warning

`rabbitmq-smtp` **has not seen updates in more than 3 years and will not work with recent versions of RabbitMQ**.

It has since been superseded by [rabbitmq-email](https://github.com/gotthardp/rabbitmq-email).
Please consider using that project first.


## What It Does

The RabbitMQ-SMTP plugin listens on a configurable TCP port for
incoming email message traffic, and routes the traffic on via
RabbitMQ's exchanges.

There is no support at this time for sending outgoing email from
RabbitMQ.

## Building the plugin

You will need:

 - a recent Erlang
 - a setup for building RabbitMQ plugins. Please see [the instructions
   for setting this up][pluginguide].

Once you are able to build the server and its plugins, you can build
the SMTP plugin. Within the `rabbitmq-public-umbrella` directory,

    hg clone http://hg.rabbitmq.com/rabbitmq-smtp
    cd rabbitmq-smtp
    make

At this point, the `Makefile` will retrieve a copy of
[erlang-smtp](http://hg.opensource.lshift.net/erlang-smtp/) if it
hasn't done so already. It will then compile everything, resulting (if
all goes well) in the presence of
`rabbitmq-smtp/dist/rabbitmq-smtp.ez` and
`rabbitmq-smtp/dist/erlang-smtp.ez`. The plugins can then be activated
as per [the instructions][pluginguide].

## Mapping between SMTP and AMQP

When an email arrives at the adapter, its SMTP "To" address is
examined to determine how it should be routed through the
system. First, the address is split into a mailbox name and a domain
part. The mailbox name is then split at the first hyphen into two
pieces.

 - the domain part (e.g. "`@rabbitmq.com`") is used to map to an AMQP virtual-host
 - if the mailbox name contained a hyphen,
    - the first part of the name is mapped to an AMQP exchange name, and
    - the second part is mapped to an AMQP routing key
 - otherwise, if no hyphen was present in the mailbox name, the whole
   mailbox name is used as the AMQP exchange name, and the AMQP
   routing key is set to the empty string.

For example, `tonyg@rabbitmq.com` is mapped to

 - the virtual host configured in the `vhost_map` (see below on configuration) for `rabbitmq.com`
 - the exchange "`tonyg`"
 - the routing key ""

and `tonyg-foo@rabbitmq.com` is mapped to

 - the virtual host configured in the `vhost_map` for `rabbitmq.com`
 - the exchange "`tonyg`"
 - the routing key "`foo`"

## Configuring the plugin

The plugin is configured using the normal Erlang application
configuration mechanism. RabbitMQ has a [standard configuration
file](http://www.rabbitmq.com/install.html#configfile) that can be
used to configure the plugin. The application name is
`rabbitmq_smtp_server`, and the individual keys are described below.

Here is an example snippet of a RabbitMQ configuration file containing
`rabbitmq_smtp_server` settings:

    ...
    {rabbitmq_smtp_server, vhost_map, [{"localhost", <<"/">>}]},
    {rabbitmq_smtp_server, default_vhosts, true},
    {rabbitmq_smtp_server, listen_port, 8025},
    ...

## Configuring the way SMTP domains are mapped to AMQP virtual-hosts

Two configuration variables control this process. The first,
`vhost_map`, should be set to a list of tuples with the first element
set to a string for the domain to map from, and the second element set
to a *binary* for the virtual-host to map to. For example, the default
value for `vhost_map` is

    [{"localhost", <<"/">>}]

which maps the SMTP "`@localhost`" domain onto the usual preconfigured
virtual-host "`/`".

In cases where the plugin searches for a domain in the `vhost_map` but
cannot find one, the second configuration variable, `default_vhosts`
is consulted. If it is set to the atom `true`, then the SMTP domain is
used *directly* as the name of the AMQP virtual-host. If it is set to
the atom `false`, then the plugin will instead return an error for
uses of the missing domain.

## Configuring the IP address and port number that the plugin listens on

Two configuration variables are used here:

 - `listen_host`, which should be set to a string containing an IP
   address. The default setting is `"0.0.0.0"`.

 - `listen_port`, which should be set to an integer containing a TCP
   port number. The default setting is `8025`.

## Example session

(tbd)

## Copyright and Licensing

Copyright (c) 2007--2010 Tony Garnock-Jones <tonyg@kcbbs.gen.nz>
Copyright (c) 2007--2010 LShift Ltd. <query@lshift.net>

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


  [pluginguide]: http://www.rabbitmq.com/plugin-development.html

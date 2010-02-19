# RabbitMQ-SMTP, an SMTP receiver plugin for RabbitMQ

The RabbitMQ-SMTP plugin listens on a configurable TCP port for
incoming email message traffic, and routes the traffic on via
RabbitMQ's exchanges. A normal AMQP (or STOMP, or Comet, or
PubSubHubBub, or XMPP) connection can be used to configure routing and
to retrieve messages as usual.

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

## Configuring the plugin

(tbd)

## Example session

(tbd)



  [pluginguide]: http://www.rabbitmq.com/plugin-development.html

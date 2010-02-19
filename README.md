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

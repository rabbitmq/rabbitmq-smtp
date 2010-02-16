require 'rubygems'
require 'socket'
require 'timeout'
require 'bunny'

describe "RabbitMQ SMTP Adapter" do
  it "should issue a 220 greeting upon connection"  do
    against_adapter do |conn|
      welcome = conn.readline
      welcome.should match /220 .*/
    end
  end

  it "should accept a HELO, and respond appropriately" do
    against_adapter do |conn|
      conn.readline # Discard welcome
      conn.write("HELO test.example.org\r\n")
      conn.readline.should match /250 .*/
    end
  end

  it "should accept a FROM line" do
    against_adapter do |conn|
      handshake conn
      command(conn, "MAIL FROM:user@test.example.org")
    end
  end

  it "should accept a TO line for the current host to a valid exchange" do
     against_adapter do |conn|
      handshake conn
      command(conn, "MAIL FROM:user@test.example.org")
      command(conn, "RCPT TO:amq.topic@localhost")
    end
  end

  it "should reject a TO line for the current host to an invalid exchange" do
    against_adapter do |conn|
      handshake conn
      command(conn, "MAIL FROM:user@test.example.org")
      command(conn, "RCPT TO:invalid@localhost", "^550.*")
    end
  end

  it "should reject a TO line for the wrong host to a valid exchange" do
    against_adapter do |conn|
      handshake conn
      command(conn, "MAIL FROM:user@test.example.org")
      command(conn, "RCPT TO:amq.topic@otherhost", "^550.*")
    end
  end

  it "should respond to a DATA command with a 354 response" do
     against_adapter do |conn|
      handshake conn
      command(conn, "MAIL FROM:user@test.example.org")
      command(conn, "RCPT TO:amq.topic@localhost")
      command(conn, "DATA", "^354 .*")
    end
  end

  it "should accept a complete data request with a 250 response" do
    against_adapter do |conn|
      handshake conn
      command(conn, "MAIL FROM:user@test.example.org")
      command(conn, "RCPT TO:amq.topic@localhost")
      command(conn, "DATA", "^354 .*")
      command(conn, "Subject: Test Message\r\n\r\nHello World\r\nAnd again!\r\n.")
    end
  end

  it "should deliver messages to the exchange" do
    against_rabbit do |mq|
      x = mq.exchange("mailuser", :type => :topic)
      q = mq.queue('mailqueue')
      q.purge
      q.bind(x, :key => '#')

      against_adapter do |conn|
        handshake conn
        command(conn, "MAIL FROM:user@test.example.org")
        command(conn, "RCPT TO:mailuser@localhost")
        command(conn, "DATA", "^354 .*")
        command(conn, "Subject: Test Message\r\n\r\nHello World\r\nAnd again!\r\n.")
      end

      msg = wait_for_message(q)[:payload]
      msg.should == "Hello World\r\nAnd again!\r\n"
    end
  end
end

def against_adapter
  socket = TCPSocket.new("127.0.0.1", 8025)
  begin
    timeout(5) do 
      yield socket
    end
  ensure
    socket.close
  end
end

def handshake(conn)
  conn.readline # Discard welcome
  conn.write("HELO test.example.org\r\n")
  conn.readline # Discard acceptance
end

def command(conn, text, expected_response="^250 .*")
  conn.write(text + "\r\n")
  unless expected_response.nil?
    conn.readline.should match Regexp.new(expected_response)
  end
end

def against_rabbit
  bunny = Bunny.new
  bunny.start
  begin
    yield bunny
  ensure
    bunny.stop
  end
end

def wait_for_message(q, retry_delay=0.1, retry_count=10)
  (1..retry_count).each do |i|
    msg = q.pop
    return msg if msg
    sleep retry_delay
  end
end

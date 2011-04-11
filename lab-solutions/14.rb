#!/usr/bin/ruby

require 'net/http'

Net::HTTP.get_print URI.parse('http://api.twitter.com/1/statuses/user_timeline.json?screen_name=jdanbrown')

require 'net/http'
require 'rexml/document'

class NewsError < Exception
end

def gimme
  extract_and_process(call_http_xml('http://www.nytimes.com/services/xml/rss/nyt/HomePage.xml'))
end

def call_http_xml(uri, redirection_limit = 10)
  raise NewsError, "too many redirects" if redirection_limit == 0

  url = URI.parse(uri)
  client = Net::HTTP.new(url.host, url.port)
  res = client.request(Net::HTTP::Get.new(uri))
  case res
  when Net::HTTPSuccess then
    return REXML::Document.new(res.body)
  when Net::HTTPRedirection then
    location = res['location']
    warn "Redirected to #{location}"
    call_http_xml(location, redirection_limit - 1)
  else
    raise NewsError, "Bad response code", res.value
  end
end

def extract_and_process(doc)
  titles = []
  doc.elements.each('rss/channel/item/title') do |ele|
    titles << ele.text
  end
  titles
end

puts gimme

require 'net/http'
require 'rexml/document'

module Gimme
  def self.gimme
    extract_and_process(self.call_http_xml('http://feeds.nytimes.com/nyt/rss/HomePage'))
  end

  def self.call_http_xml(uri)
    url = URI.parse(uri)
    client = Net::HTTP.new(url.host, url.port)
    res = client.request(Net::HTTP::Get.new(uri))
    doc = REXML::Document.new(res.body)
    doc
  end

  def self.extract_and_process(doc)
    titles = []
    doc.elements.each('rss/channel/item/title') do |ele|
      titles << ele.text
    end
    titles
  end

end

puts Gimme.gimme

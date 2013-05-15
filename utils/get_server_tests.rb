#! /usr/bin/env ruby
# -*- coding: UTF-8 -*-

# Quick'n'dirty script to extract tests from the server output
# Usage :
#
#   ./get_server_tests.rb < mail.txt
#
# Assuming that you copied the whole mail from the server to mail.txt
#

$root = File.expand_path(File.dirname(__FILE__) + '/../tests')

class ClapTest

    attr_accessor :title, :category, :code, :expected

    def initialize(o={})
        self.title    = o[:title].split('.clap')[0]
        self.category = o[:category]
        self.code     = o[:code]
        self.expected = o[:expected] || ''
    end

    def path_root
        "#{$root}/#{self.category}/#{self.title}"
    end

    def code_path
        self.path_root + '.clap'
    end

    def expected_path
        self.path_root + '.expected'
    end

    def write
        File.open(self.code_path,   'w')   { |f| f.write self.code }
        File.open(self.expected_path, 'w') { |f| f.write self.expected }
    end

end

def parse_server_output(s)

    clap_tests = []

    re_cat  = /^In\s+(?<category>[a-z]+\/[a-z]+)/
    re_test = /^\[.*?(?:-->)?\s+(?<title>[0-9]+[-\w]+\.clap)\n\nINPUT: ?\n(?<input>.+)OUTPUT: ?\n.*/m
    re_test_expected = /^\[.*?(?:-->)?\s+(?<title>[0-9]+[-\w]+\.clap)\n\nINPUT: ?\n(?<input>.+)OUTPUT: ?\n.*?EXPECTED: ?\n(?<expected>.*)/m

    s.split(/=====+\n/).drop(1).each do |suit_s|

        sep = suit_s.index "\n["

        header = suit_s[0..sep]
        suit   = suit_s[sep+1..suit_s.length].split(/------+\n/)

        suit.pop

        category = re_cat.match(header)[:category]
        tests    = suit.keep_if { |l| l[0] == '[' }

        tests.each do |test|

            t = (test.index('EXPECTED') ? re_test_expected : re_test).match(test)

            next if t.nil?

            clap_tests << ClapTest.new(
                :title    => t[:title],
                :category => category,
                :code     => t[:input],
                :expected => t.captures.length >= 3 ? t[:expected] : ''
            )

        end

    end
    
    clap_tests

end


if $0 == __FILE__

    require 'fileutils'
    FileUtils.mkdir_p "#{$root}/bad/lexical"
    FileUtils.mkdir_p "#{$root}/bad/syntax"
    FileUtils.mkdir_p "#{$root}/good/syntax"
    FileUtils.mkdir_p "#{$root}/good/lexical"
    FileUtils.mkdir_p "#{$root}/good/memo"
    FileUtils.mkdir_p "#{$root}/good/bench"
    FileUtils.mkdir_p "#{$root}/good/interpret"

    cts = parse_server_output STDIN.read

    cts.each { |c| c.write }

end

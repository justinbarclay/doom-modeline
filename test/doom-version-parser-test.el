;;; doom-version-parser-test.el --- Unit tests for doom-version-parser -*- lexical-binding: t; -*-

(ert-deftest doom-version-parser--ruby/parse-ruby-version-string ()
  (should
   (string= (ruby-version-parser "ruby 2.3.3p222 (2016-11-21 revision 56859) [x86_64-darwin16]")
            "2.3.3")))

(ert-deftest doom-version-parser--elixir/parse-elixr-version-string ()
  (should
   (string= (elixir-version-parser "IEx 1.7.4 (compiled with Erlang/OTP 21)")
            "1.7.4")))

(ert-deftest doom-version-parser--rustc/parse-rustc-version-string ()
  (should
   (string= (rustc-version-parser "rustc 1.32.0-nightly (14997d56a 2018-12-05)" )
            "1.32.0")))

(ert-deftest doom-version-parser--go/parse-go-version-string ()
  (should
   (string= (go-version-parser "go version go1.11.4 darwin/amd64" )
            "1.11.4")))

(ert-deftest doom-version-parser--perl/parse-perl-version-string ()
  (let ((perl-string "This is perl 5, version 18, subversion 2 (v5.18.2) built for darwin-thread-multi-2level
(with 2 registered patches, see perl -V for more detail)

Copyright 1987-2013, Larry Wall

Perl may be copied only under the terms of either the Artistic License or the
GNU General Public License, which may be found in the Perl 5 source kit.

Complete documentation for Perl, including FAQ lists, should be found on
this system using \"man perl\" or \"perldoc perl\".  If you have access to the
Internet, point your browser at http://www.perl.org/, the Perl Home Page."))
    (should
     (string= (perl-version-parser perl-string)
              "5.18.2"))))

(ert-deftest doom-version-parser--perl/parse-perl-version-string ()
  (should
   (string= (python-version-parser "Python 2.7.15")
            "2.7.15")))

;;; doom-version-parser-test.el ends here

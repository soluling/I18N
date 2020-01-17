jQuery Validation Globalize
===========================

[![Build Status](https://travis-ci.org/johnnyreilly/jquery-validation-globalize.svg?branch=master)](https://travis-ci.org/johnnyreilly/jquery-validation-globalize)

An extension to the jQuery Validation Plugin which makes it use Globalize for number and date parsing (enabling simple internationalized validation).

This extension has the following dependencies:
- jQuery Validation (which itself depends on jQuery) - any version to my knowledge
- Globalize v0.1.1 

##Getting started

Simply include jquery.validate.globalize.js on a page **after** jquery.validate.js and globalize.js.  Now you are validating using Globalize to do your number and date parsing.  Lucky you!

So what's different?  Well, for example, if you're catering for German users then you will be presumably using the "de-DE" Globalize culture.  If this culture has been selected at the time of validation then "Dienstag, 27. August 2013" will be successfully validated as a date and "10,5" will be successfully validated as a number.

The following validator methods are patched by jQuery Validation Globalize:

- number
- min
- max
- range
- date

## Install

On Bower (`bower install jquery-validation-globalize --save` ), NuGet (`Install-Package jQuery.Validation.Globalize`) and.... well here!  

By the way - ignore the version number on NuGet - it says 1.0.1 - more properly it should be 0.1.1 (to match the Globalize version).  I'm not planning to maintain the version on NuGet any further but I thought I should note this down somewhere to avoid confusion.

## Author
**John Reilly**

+ http://twitter.com/johnny_reilly

## Credits
Inspired by [Scott Hanselman's blog post](http://www.hanselman.com/blog/GlobalizationInternationalizationAndLocalizationInASPNETMVC3JavaScriptAndJQueryPart1.aspx) and evolved from [my blog post](http://icanmakethiswork.blogspot.com/2012/09/globalize-and-jquery-validate.html).  Entirely dependent upon [jQuery Validation](https://github.com/jzaefferer/jquery-validation) and [Globalize](https://github.com/jquery/globalize/).

## Copyright
Copyright � 2013 [John Reilly](mailto:johnny_reilly@hotmail.com).

##License

MIT license - [http://www.opensource.org/licenses/mit-license.php](http://www.opensource.org/licenses/mit-license.php)

##Changelog

###1.0.1 / 2013-09-28

- Changed min, max and range to defer to original validation methods in jQuery Validation once parsing has taken place. Should the implementations of these ever change then we no longer need to propogate those changes manually into jQuery Validation Globalize.  
- Removed direct dependency on jQuery - not needed as jQuery Validation has (and will always have) this dependency itself.

###1.0.0 / 2013-08-27

- Initial release

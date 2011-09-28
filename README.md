# Intro

This project is designed to output previews of go problems in SGF format.

It currently outputs in PNG, and a text file that says whose move it is.

There is also part of handling for another output format that would use
Javascript to display a dynamic thing with moving stones on mouseover,
but that part is not finished.

# How to use

You should be able to just edit the Config.hs file, and then put this
directory anywhere in your web root.

You will need to run

~~~~~~~~~~~~~~~~~~~~~~~~~~
ghc --make generate_preview
~~~~~~~~~~~~~~~~~~~~~~~~~~

in the directory also.  Finding the right libraries should not be all
that hard, but feel free to contact me if you need help.

Then, all you need to do is put SGF files in the sgfDirectory, and
\<img> or link tags in your html content that point to the generated
files.  The htaccess rewrite rule should handle automatically
generating the file if it does not already exist, or serving the
cached copy if it is there.

Not terribly user friendly, but you should be able to figure it out.

# License

This project is copyrighted 2011 by Joshua Simmons.  It is available
under the [AGPLv3 license](http://www.gnu.org/licenses/agpl.html)
which is included in text form in the LICENSE file

# Disclaimer

This work comes with no warranty of any kind, to the extent allowed by
law.  It was written with security in mind, but you should do your own
security audit on this code before using it in any way.  Anything that
goes wrong, in any way, is your responsibility, not mine.

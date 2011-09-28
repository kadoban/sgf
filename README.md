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

Prerequisites
-------------

1. You need htlatex executable on your path. 
   http://www.tug.org/applications/tex4ht/mn-commands.html   

2. You need jsMath if you intend to use Math in your latex content and
   expect html to render math correctly.
   http://www.math.union.edu/~dpvc/jsMath/authors/welcome.html  

Notes
-----

I have tested this only on SBCL on Mac OS X 10.6.x. So if you plan to
run this on some other implementation, you may face issues that I do not
know of. However if you try to fix it or let me know, I will try to help.
The only SBCL specific code that I have used is to execute external
program using SBCL:ext-run. As such you will have to tweak this part to
get it running on another CL implementation.

Instructions to load and run
----------------------------

1. Ensure website.asd is in your ASDF search path.

2. You can use this application to generate a hmtl website from latex
   source files. You write all your content in latex sources and
   generate the htmls. By using Latex, you can also generate your
   content in other output formats.

3. Before proceeding, clone wwwc repository which you can use as a
   sample of latex sources.

   hg clone ssh://hg@bitbucket.org/dmsurti/wwwc

   Please refer to wwwc for more details.

4. To generate the website run:
   (website:publish-website)

5. To generate html for some content run:
   (website:publish-page #P"dir/to/latex-content"
  	  		 #P"file.latex")


This is a simple ID3 implementation made in Haskell for a course 
project. It's been made with separation in mind, so it should be 
relatively easy to use it for any other tests, just keeping in mind that 
datasets should have the result class at the end.

To test the implementation, a pair of test and train datasets has been
provided: *dstest.csv* and *dstrain.csv*. They are duplicates of those
inside *dataset* folder.

If you are on a Linux environment, you can just run the script 
*mkdset.sh* and the csv files will be automatically parsed to a Haskell
understandable version and the test will run.

Otherwise, you can manually run the *csv2hs.php* on the csv files to
parse them. Anyway, they're already parsed for convenience, so a 
*ghci Main.hs* should do the work.


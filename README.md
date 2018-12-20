## dmp

## Welcome!

#### What's this repo for?

This is the working repository for my Distinguished Majors Program thesis for the UVa English Department: *Reading DT Leaves: A Computational Analysis of the Lyrical Novel*. Better contextualize yourself by trawling [the blog](timschott.github.io) I set up using `HTML,` `Jekyll,` and `CSS.`

#### Do you know what you're doing?

...kind of. 

I gained exposure to this type of work in a few excellent courses at UVa. [Brad Pasanek's](http://english.as.virginia.edu/people/profile/bmp7e) "Hacking for Humanists" class, a rudimentary introduction to Text Cleaning with `R,` sparked my interest in text analysis when I was just a first year. More recently, I spent the just-passed fall semester enjoying the excellent combination of [Abigail Flower's](https://dsi.virginia.edu/people/abigail-flower) "Practice of Data Science" and [Yanjun Qi's](https://www.cs.virginia.edu/yanjun/) ["Machine Learning"](https://qiyanjun.github.io/2018fUVA-CS4501MachineLearning/) courses. Both of these classes approached machine learning and, more broadly, computational tasks, from a richly theoretical standpoint. I am directly applying many of the skills I picked up in all of these classes in this project. I also serve as research assistant at [UVa IATH](http://www.iath.virginia.edu/) and there are some examples of the heavy lifting I've done for them interspersed throughout some of my other repos, like [this one](https://github.com/timschott/POStagging). Lastly, I am a Computer Science and English major; this carries a unique perspective into this endeavor.

#### What's inside?
This repository contains my corpus, a database of said corpus (in cleaned, machine readable form), various text cleaning scripts that help grow said databas. It also contains my supervised (SVM) and unsupervised (RNN) classification work. 

##### Can you be a little more specific?
The `scriptsAndDatabase` folder contains my work in `R.` Of interest you'll find me breaking my corpus down into *sentences* and *words.* To break down their paragraphs, the initial work is carried out in `Python` and then kicked back to `R` for final tune-ups. This is because of new-line character discrepancies (fun). My database is the big `SQL` guy, `textTable.sqlite.` My unparsed texts are inside `rawTexts.`


The `PythonScripts` folder contains my work in `Python.`



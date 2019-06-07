# Ensemble Prediction of Time to Event Outcomes with Competing Risks: Code for simulation study

This repository contains the code necessary to run and summarize the simulation study in the forthcoming paper: "Ensemble Prediction of Time to Event Outcomes with Competing Risks: A Case Study of Surgical Complications in Crohn's Disease".

It is organized as an R package. The functions are defined in the files contained in the R subdirectory. In the tests subdirectory, you will find the file `simulate-compare-to-binary.R` which runs the simulation. You will find that Scenario E does not work, because I am not allowed to distribute that data.

To run on your own computer, you can download the repo. Then using the `devtools::load_all()` function, you will be able to run the code.


## License

Copyright 2018 Michael C Sachs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
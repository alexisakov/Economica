# Economica
The collection of economics and econometrics related codes

## Installation

### Using Leonid Shifrin's [ProjectInstall](https://github.com/lshifr/ProjectInstaller)

This should install the code formatter project to your repository. 
    
    Import["https://raw.github.com/lshifr/ProjectInstaller/master/BootstrapInstall.m"]
    << ProjectInstaller`
    ProjectInstall[URL["https://github.com/alexisakov/Economica/archive/master.zip"]]

You can start using it, e.g. by calling

    <<Economica`
    
## Updating

To update again using the ProgectInstall[] package:

    Needs["ProjectInstaller`"]
    ProjectUninstall["Economica"]
    ProjectInstall[URL["https://github.com/alexisakov/Economica/archive/master.zip"]]

# Steps to be completed
# https://cimentadaj.github.io/blog/2018-05-25-installing
#-rjava-on-windows-10/installing-rjava-on-windows-10/

# Install required package
install.packages("rJava")

# Set JAVA_HOME environment
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-17/") 

# load required library
library(rJava)

### It works !!!

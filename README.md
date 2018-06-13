# Social Graph Analytics with R-Shiny

A Rshiny App with the Graph Analytics on opensource datasets.
The app was initially developed as a project for a University Course: Analysis of Large Scale Social Networks, KU LEUVEN, 2018. The app will be further extended with other interesting features. 

The app consist of 4 modules:
* Graph Stats & Pruning
* Graph Visualizations
* Paths
* Community Detection

## Installation

The following R packages need to be installed:

~~~~~~~
install.packages(shiny)
install.packages(igraph)
install.packages(visNetwork)
install.packages(dplyr)
install.packages(DT)
install.packages(plotly)
~~~~~~~

## Running

Option 1: 
* open app.r in Rstudio
* push "run" button
* the App will launch in a new window

Option 2: 
* Deploy the App to a Rshiny Server
* The app can run on website 

## In the Pipeline for Future Development

To be added: 
* [General] Improve layout of the App
* [General] Introduce a module development of the App
* [Graph Data] New module to upload datasets, prune data, ...
* [Graph Stats] Add power of Law
* [Community Detection] Find important nodes in communities
* [Community Detection] Find important nodes that link different communities
* [Community Detection] Add brokage information about the role of nodes in a community




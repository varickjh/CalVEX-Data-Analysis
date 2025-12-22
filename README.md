# CalVEX

This app allows users to perform simple visual analysis with data from the [California Violence Experiences Survey (CalVEX)](https://geh.ucsd.edu/california-violence-experiences-survey-calvex/). More information on the survey can be found at [https://geh.ucsd.edu/resources/](https://geh.ucsd.edu/resources/) under 'California Violence Experiences Survey (CalVEX)'. 

## Contents

The folder CalVEX-Data-Analysis contains:
- **app.R**, **server.R**, **ui.R**: These are the files that make up the app itself. **server.R** is the code for the back-end data analysis, **ui.R** is the code for the front-end user interaction, and **app.R** connects the two files together. 
- The ***data*** folder contains four files: **CalVEX2020.csv**, **CalVEX2021.csv**, **CalVEX2022.csv**, and **CalVEX2023.csv**. These files contain the raw data from each of the CalVEX surveys conducted in [2020](https://www.openicpsr.org/openicpsr/project/204403/version/V1/view), [2021](https://www.openicpsr.org/openicpsr/project/204402/version/V1/view), [2022](https://www.openicpsr.org/openicpsr/project/204401/version/V1/view), and [2023](https://www.openicpsr.org/openicpsr/project/199087/version/V1/view). The data in these files are unedited except for an additional column added to each entry denoting the year that the data was collected. 
- **requirements.txt** are the libraries required to run the program on one's personal computer.
- **.gitignore** tells git which files to ignore when connecting repositories between the programmer's local computer & the public repository (not required to use for the app itself). 

## Installation

These instructions are made for specifically running the CalVEX Data Visualization app on a PC. 

1. Download / pull the ***CalVEX-Data-Analysis*** folder onto your personal computer, and navigate to that folder using whichever programming app you are using (Visual Studio Code recommended).
2. Create a virtual environment (strongly recommended) using `python -m calvex_venv` in the terminal and activate it using `calvex_venv/Scripts/activate`. (To deactivate the virtual environment, use `deactivate`.)
3. Install the packages described in **requirements.txt** using `pip install -r requirements.txt`. 
4. Open a R terminal by navigating to **app.R** and running the program (the 'play' button in the top right of the screen, or hitting `CTRL + SHIFT + S`). (To quit R, use `q()`.)
5. Run the app by using `runApp("CalVEX-Data-Analysis")`. 
6. To stop the script from running, hit `CTRL + C`. 

## Usage

Once running, the app should automatically respond to the user's inputs. Select the demographics you want displayed and the graph should automatically update. 
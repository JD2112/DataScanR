# <div align="center">DataScanR<br>[![DOI](https://zenodo.org/badge/490592846.svg)](https://doi.org/10.5281/zenodo.11105016)[![](https://img.shields.io/badge/Shiny-shinyapps.io-447099"?style=flat&labelColor=white&logo=Posit&logoColor=447099")](https://jyotirmoydas.shinyapps.io/datascanr/)
</div>

Data exploration analysis tool. It can help to clean the data, report the missing/NA values (using pareto graph), perform a set of various statistical analyses (e.g., correlation, t-test, MW-test, KS-test, and more) resulting in tables and graphs.
<br>![DataCleaning1](https://github.com/user-attachments/assets/4c22043f-859a-4415-9f3f-d44d7f32edaa)
<br>

## How to use
We provide a simple web application that allows to explore the data that is stored in a standard comma separated file format.<br> Check our [Quick Guide](https://github.com/ilo21/DataScanR/blob/main/DataScanR/www/DataScanR_Quick_Guide.pdf) if you would like to start working with DataScanR or learn more about the tests it can perform.
<br><br> <p align="center">
![workflow_gray](https://github.com/user-attachments/assets/d9410ca8-54fb-4a5c-bb3c-f74f0933bb4a)
</p>

### Web application
The application is currently hosted on the [SciLifeLab Serve](https://serve.scilifelab.se/).
<br>You can access it under the following link:<br>
- [DataScanR](https://datascanr.serve.scilifelab.se/app/datascanr)

### Local use
User can use the docker image -

```
docker run --rm -p 3838:3838 ilo24/datascanr:latest
```

## Example Data
The user can test DataScanR using the [example data](https://github.com/ilo21/DataScanR/blob/main/example_data/heartfailure.csv) that is available to download through the application interface. That data comes from [dlookr](https://github.com/choonghyunryu/dlookr/tree/master) package, and additional variables were [simulated](https://github.com/ilo21/DataScanR/blob/main/example_data/convert_2csv.R) to be able to demonstrate all options of DataScanR.


## Credits
- Main Author: 
    - Ilona Szczot ([@ilo21](https://github.com/ilo21))    

- Collaborators:
    - Jyotirmoy Das ([@JD2112](https://github.com/JD2112))

## Citation

WILL BE UPDATED SOON

## Acknowledgement

We would like to acknowledge the **Core Facility, Faculty of Medicine and Health Sciences, Linköping University, Linköping, Sweden**, **Clinical Genomics Linköping, Science for Life Laboratory, Sweden** and **Center for Social and Affective Neuroscience (CSAN), Linköping University, Linköping, Sweden** for their support.

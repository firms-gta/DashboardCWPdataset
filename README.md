# CWP Quick Plots Dashboard

This **Shiny** application provides a quick way to visualize selected plots in the **CWP format**.  
It is built on top of the [`bastienird/CWP.dataset`](https://github.com/bastienird/CWP.dataset) package, meaning that **any update to this package will be automatically reflected in the dashboard**.

⚠️ **Note**:  

- The app is intended only for quick exploratory use.  
- It allows you to view some results in a simple way, but it does **not support advanced analyses or complex mapping**.  
- For this Dashboard you will need as an input a dataset following the CWP format.

👉 For more advanced features (detailed analyses, interactive visualizations, and full mapping capabilities), please refer to the following application:  

[`firms-gta/DashboardCWPdataset`](https://github.com/firms-gta/DashboardCWPdataset)

---

## Installation & Usage

Clone the repository and launch the Shiny app:

```r
shiny::runApp()
```

Make sure you have installed the [`bastienird/CWP.dataset`](https://github.com/bastienird/CWP.dataset) package beforehand.




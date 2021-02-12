# Climate Data Management Systems - Data Model Review

## Introduction

Historically, a Climate Data Management System (CDMS) has been defined as "an integrated computer-based system that facilitates the effective archival, management, analysis, delivery and utilization of a wide range of integrated climate data" (WMO 2014).

> Discussion of the future – to include reference to Earth Systems Approach (WMO 2019) and Reference Implementation.

The `opencdms-data-model` repository contains physical data models for multiple systems. Physical models are defined in terms of the specific database management systems that has been used for implementation. Database schema described in SQL Data Definition Language (DDL)

This report focuses on existing data models, focusing on systems that are in PRODUCTION USE IN NMHSs and are OpenCDMS focus systems

## Data model types

The WMO Guidelines on climate data management ([WMO 2007](#wmo_2007)) establish three data model types that can be used to classify the approach taken in CDMS implementations.

| Model name                 | Description |
|----------------------------|-------------|
| **Element Model (EM)**     | An Element Model represents data in tables, having, in each row, *different values* of *one element* observed at one station *at different times*. |
| **Observation Model (OM)** | An Observation Model represents data in tables having, in each row, the values of *different elements* observed at one station *at a given time*. |
| **Value Model (VM)**       | A Value Model will represent the data values in tables having, in each row, *only one value* of *one element* observed at one station *at a specific time*. |

The following summaries are taken from ([WMO 2007](#wmo_2007)).

### Element Model

| Station’s ID | Month/Year | Element | Value day 1 | Value day 2 | Value day 3 | Value day 4 | Value day 5 | … | … | Value day 31 |
|--------------|------------|---------|-------------|-------------|-------------|-------------|-------------|---|---|--------------|
| 95123        | 01/2002    | Tmin    | 23.4        | 25.2        | 28.3        | 26.5        | 27.8        | … | … | 24.9         |
| 66202        | 01/2003    | Tmax    |             |             |             |             |             | … | … |              |
|              |            |         |             |             |             |             |             | … | … |              |

**Advantages:** It is easy to add new elements; the data model remains the same even if a new element is added.\
**Disadvantages:** Performance for real-time applications may be poor; many operations on the database can be more complex than would otherwise be the case.

### Observation Model

| Station’s ID | Day/Month/Year | Tmin | Tmax | Rain | Min Humidity | Min MSL Pressure | Max Wind Speed | … | … | Max Wind Direction |
|--------------|----------------|------|------|------|--------------|------------------|----------------|---|---|--------------------|
| 33220        | 01/01/2002     | 24.5 | 33.4 | 0    | 72           | 1015.6           | 2.2            | … | … | 160                |
| 42500        | 01/01/2003     | 15.2 | 22.3 | 10.2 | 80           | 1013.4           | 3.3            | … | … | 210                |

**Advantages:** High performance for real-time applications; optimisation of data storage.\
**Disadvantages:** Need to update the table structure if a new element that has not been included during the database design stage has to be added.

### Value Model

| Station’s ID | Time       |Element | Value |
|--------------|------------|--------|-------|
| 33220        | 01/01/2002 | Tmin   | 23.4  |
| 42500        | 01/01/2003 | Tmax   | 16.3  |
| 22222        | 01/01/2003 |        |       |

**Advantages:** It is easy to add new elements, the model is adaptable to a large range of data types.\
**Disadvantages:** Optimization of data storage will not be done well, so this approach is not suitable for tables with huge amounts of data; also shares the disadvantages of the Element model.

## Focus systems

The `opencdms-data-model` repository currently contains database schemas and documentation for CliDE, Climsoft, MCH and MIDAS. Documentation is also available for BDCLIM.

In addition, further work is being undertaken by the OpenCDMS Reference Implementation Working Group to review a wider range of existing systems.

## Interoperability

## Recommendations

-	ongoing research to evaluate approaches – including assessment of components of the WMO CDMS specifications that require provisions to be present within data models (FUTURE)
-	work on a reference implementation data model that incorporates current best practices (FUTURE)

Top-down RI data model design begins with conceptual data model defining what the systems contains, followed by logical data model defining how the system should be implemented (regardless of the specifics of the physical implementation).
<!--
  https://en.wikipedia.org/wiki/Data_model#Three_perspectives
  https://en.wikipedia.org/wiki/Data_model#Entity-relationship_model
  https://www.tutorialspoint.com/dbms/dbms_data_models.htm
  https://www.guru99.com/data-modelling-conceptual-logical.html
  Entity relationship model vs UML
-->

The OpenCDMS Project Technical Team recommend following a Domain Driven Design (DDD) approach to the creation of the Reference Implementation data model to ensure that the terminology used in the Reference Implementation matches the language of the domain.
<!-- https://stackoverflow.com/questions/3835169/uml-domain-modeling/3835214#comment4077822_3835214 -->


## References

<span id="wmo_2007">[WMO (2007)](https://library.wmo.int/index.php?lvl=notice_display&id=16656) Guidelines on climate data management. WMO/TD- No. 1376. WMO Geneva</span>\
<span id="wmo_2014">[WMO (2014)](https://library.wmo.int/index.php?lvl=notice_display&id=16300) Climate Data Management System Specifications. WMO-No. 1131. WMO Geneva</span>\
<span id="wmo_2019">[WMO (2019)](https://library.wmo.int/?lvl=notice_display&id=21440) World Meteorological Congress. Abridged Final Report of the Eighteenth Session. WMO-No. 1236, res. 22 p88. WMO Geneva</span>

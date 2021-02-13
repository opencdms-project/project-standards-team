    Climate Data Management Systems
# Data Model Review: OpenCDMS Focus Systems

## Introduction

Historically, a Climate Data Management System (CDMS) has been defined as "an integrated computer-based system that facilitates the effective archival, management, analysis, delivery and utilization of a wide range of integrated climate data" ([WMO 2014](#wmo_2014)).

> Broader discussion to include GCOS definition of climate variables, reference to WMO Earth Systems Approach (WMO 2019), CDMS Specifications and OpenCDMS Reference Implementation.

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

CliDE, Climsoft and MCH are all CDMS solutions that are used extensively in developing countries. It is essential for the OpenCDMS project to support these projects and their users where possible. MIDAS is a custom CDMS developed and used by the UK Met Office. The system is of particular interest to OpenCDMS because extensive datasets, with rich and complete metadata, are available as open data.

In addition, further work is being undertaken by the OpenCDMS Reference Implementation Working Group to review a wider range of existing systems.

## Data ingestion and utilization

> Long vs wide, Key entry forms, Analysis

[#10](https://github.com/opencdms/opencdms-data-model/issues/10) discussion of normalization, optimization for common scenarios and analysis that requires "tidy data"
<!-- 3rd normal form? -->

([Wickham 2014](#wickham_2014))

## Date period

[#11](https://github.com/opencdms/opencdms-data-model/issues/11)

## Primary keys and indexing 

[#9](https://github.com/opencdms/opencdms-data-model/issues/9)

Composite natural keys vs synthetic keys.

<!--The web: RESTful APIs and Object Relational Mapping (ORM) use of unique (single) keys. -->

## Dynamic schema modifications

[#7](https://github.com/opencdms/opencdms-data-model/issues/7)

MCH allows variations in the database definition. Example include:

a) Support for table and field names in multiple languages (e.g., Spanish and English)
b) Creation of a set of new database tables for each new parameter that is added

## Interoperability

![Mappings](https://raw.githubusercontent.com/opencdms/opencdms-data-model/master/data_model_review/images/field_mappings.png)
*Figure:* Illustration of the process of achieving interoperability among supported systems

The definition and use of "Station Name" varies between solutions and the formal definition of "station/platform name" existing in the WIGOS metadata standard.

```
filters = {
    'src_id': 838,
    'period': 'hourly',
    'year': 1991,
    'elements': ['wind_speed', 'wind_direction'],
}
```

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

### Hypertables

![Hypertable](https://raw.githubusercontent.com/opencdms/opencdms-data-model/master/data_model_review/images/timescaledb_hypertable_chunk.png)
*Figure:* TimescaleDB uses the "hypertable" abstraction as a virtual view of many individual tables holding the data, called chunks.

### Domain Driven Design

The OpenCDMS Project Technical Team recommend following a Domain Driven Design (DDD) approach to the creation of the Reference Implementation data model to ensure that the terminology used in the Reference Implementation matches the language of the domain.
<!-- https://stackoverflow.com/questions/3835169/uml-domain-modeling/3835214#comment4077822_3835214 -->

### Research Questions

Before making final recommendations for next generation climate data models, we propose a number of research questions that must be investigated:
- Flexibility vs efficiency – measure the implication of the transposing data stored in “long format”
- Time series data retrieval, with and without hypertables (research must already exist)

## References

<span id="wmo_2007">[WMO (2007)](https://library.wmo.int/index.php?lvl=notice_display&id=16656) Guidelines on climate data management. *WMO/TD- No. 1376*. WMO Geneva</span>\
<span id="wickham_2014">[Wickham, Hadley (2014)](https://www.jstatsoft.org/article/view/v059i10) Tidy Data. *Journal of Statistical Software 59*(10)</span>\
<span id="wmo_2014">[WMO (2014)](https://library.wmo.int/index.php?lvl=notice_display&id=16300) Climate Data Management System Specifications. *WMO-No. 1131*. WMO Geneva</span>\
<span id="wmo_2019">[WMO (2019)](https://library.wmo.int/?lvl=notice_display&id=21440) World Meteorological Congress. Abridged Final Report of the Eighteenth Session. *WMO-No. 1236, res. 22 p88*. WMO Geneva</span>

<!--

Bannerman, B and Palmer, S (2015) Open-CDMS Roadmap. Australian Bureau of Meteorology
Fowler, M (2010) Utility Vs Strategic Dichotomy 29 July 2010. Retrieved from https://www.martinfowler.com/bliki/UtilityVsStrategicDichotomy.html
Fowler, M (2019) Refactoring: Improving the Design of Existing Code. Addison-Wesley Professional; 2 edition 
Gentile, G (2012) Counterinsurgency and War. In: Lindley-French, J and Boyer Y (eds) The Oxford Handbook of War. Oxford University Press
Haase M. et al. (2018) Hydrometeorological Time Series Management—A Case Study from the Western Balkans. In: Bungartz HJ., Kranzlmüller D., Weinberg V., Weismüller J., Wohlgemuth V. (eds) Advances and New Trends in Environmental Informatics. Progress in IS. Springer, Cham
Martin, D. J. et. al. (2015) Development and implementation of a climate data management system for western Pacific small island developing states.  Meteorological Applications 22: 273–287
Stuber, D et. al. (2011) Climate Data Management Systems: status of implementation in developing countries. Climate Research, Vol. 47: 13–20
Tandy, J (2017) Spatial Data on the Web Best Practices. OGC 15-107. Open Geospatial Consortium
WMO (1986) CLICOM Project: Climate Data Management System. WMO/TD- No. 131; WCP- No. 119. WMO Geneva
WMO (1999) Report of the training seminar on Climate Data Management focusing on CLICOM/CLIPS development and evaluation. WMO/TD-No.973. WMO Geneva

WMO (2015) Meeting of the CCl Expert Team Climate Data Management Systems (ET-CDMS). Final Report, Annex 4. WMO Geneva
WMO (2018) Commission for Climatology. Abridged Final Report of the Seventeenth Session. WMO-No. 1216, resolution 4 page 12. WMO Geneva
WMO (2019a) Climsoft – MCH co-ordination meeting Outcome Report. WMO Geneva.
WMO (2019b) World Meteorological Congress. Abridged Final Report of the Eighteenth Session. WMO-No. 1236, resolution 22 page 88. WMO Geneva
Wright, W (2019) Co-chair on Data - Commission for Climatology, WMO. Personal Communication 18th November 2019

-->

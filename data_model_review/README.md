# Climate Data Management Systems - Data Model Review

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

## Observation Model

| Station’s ID | Day/Month/Year | Tmin | Tmax | Rain | Min Humidity | Min MSL Pressure | Max Wind Speed | … | … | Max Wind Direction |
| 33220        | 01/01/2002     | 24.5 | 33.4 | 0    | 72           | 1015.6           | 2.2            | … | … | 160                |
| 42500        | 01/01/2003     | 15.2 | 22.3 | 10.2 | 80           | 1013.4           | 3.3            | … | … | 210                |

**Advantages:** High performance for real-time applications; optimisation of data storage.\
**Disadvantages:** Need to update the table structure if a new element that has not been included during the database design stage has to be added.

## Value Model

| Station’s ID | Time       |Element | Value |
|--------------|------------|--------|-------|
| 33220        | 01/01/2002 | Tmin   | 23.4  |
| 42500        | 01/01/2003 | Tmax   | 16.3  |
| 22222        | 01/01/2003 |        |       |

**Advantages:** It is easy to add new elements, the model is adaptable to a large range of data types.\
**Disadvantages:** Optimization of data storage will not be done well, so this approach is not suitable for tables with huge amounts of data; also shares the disadvantages of the Element model.

## References

<span id="wmo_2007">WMO (2007) Guidelines on climate data management. WMO/TD- No. 1376. WMO Geneva</span> [🔗](https://library.wmo.int/index.php?lvl=notice_display&id=16656)

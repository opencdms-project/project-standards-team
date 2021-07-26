    Climate Data Management Systems
# Data Model Review: OpenCDMS Focus Systems

[Ian Edwards](https://github.com/isedwards)

<!--
The people below have contributed to the content of the document through discussions in GitHub issue comments
See: https://github.com/opencdms/dms-wg/issues

[Ian Edwards](https://github.com/isedwards),
[Jose Guijarro](https://github.com/jaggh),
[Dan Hollis](https://github.com/DanHollis),
[Steve Palmer](https://github.com/Steve-Palmer),
[Eduardo Porras](https://github.com/eporrasc),
[Martin Schweitzer](https://github.com/martinschweitzer),
[Denis Stuber](https://github.com/denisstuber)
-->

## Introduction

Climate Data Management Systems (CDMS) have been defined as "an integrated computer-based system that facilitates the effective archival, management, analysis, delivery and utilization of a wide range of integrated climate data" ([WMO 2014](#wmo_2014)), where "climate" is considered to include a broard range of environmental variables ([Bojinski 2014](#bojinski_2014)).

The OpenCDMS `reference-implementation` repository contains physical data models for multiple systems. Physical models are defined in SQL Data Definition Language (DDL) for the specific database management systems that has been used for implementation.

This report considers data models from existing systems that are in production use in NMHSs and are currently OpenCDMS focus systems.

## Data model types

The WMO Guidelines on climate data management ([WMO 2007](#wmo_2007)) establish three data model types that can be used to classify the approach taken in CDMS implementations.

| Model name                 | Description |
|----------------------------|-------------|
| **Element Model (EM)**     | An Element Model represents data in tables, having, in each row, *different values* of *one element* observed at one station *at different times*. |
| **Observation Model (OM)** | An Observation Model represents data in tables having, in each row, the values of *different elements* observed at one station *at a given time*. |
| **Value Model (VM)**       | A Value Model will represent the data values in tables having, in each row, *only one value* of *one element* observed at one station *at a specific time*. |

The following summaries are taken from ([WMO 2007](#wmo_2007)).

#### Element Model

| Station’s ID | Month/Year | Element | Value day 1 | Value day 2 | Value day 3 | Value day 4 | Value day 5 | … | … | Value day 31 |
|--------------|------------|---------|-------------|-------------|-------------|-------------|-------------|---|---|--------------|
| 95123        | 01/2002    | Tmin    | 23.4        | 25.2        | 28.3        | 26.5        | 27.8        | … | … | 24.9         |
| 66202        | 01/2003    | Tmax    |             |             |             |             |             | … | … |              |
|              |            |         |             |             |             |             |             | … | … |              |

**Advantages:** It is easy to add new elements; the data model remains the same even if a new element is added.\
**Disadvantages:** Performance for real-time applications may be poor; many operations on the database can be more complex than would otherwise be the case.

#### Observation Model

| Station’s ID | Day/Month/Year | Tmin | Tmax | Rain | Min Humidity | Min MSL Pressure | Max Wind Speed | … | … | Max Wind Direction |
|--------------|----------------|------|------|------|--------------|------------------|----------------|---|---|--------------------|
| 33220        | 01/01/2002     | 24.5 | 33.4 | 0    | 72           | 1015.6           | 2.2            | … | … | 160                |
| 42500        | 01/01/2003     | 15.2 | 22.3 | 10.2 | 80           | 1013.4           | 3.3            | … | … | 210                |

**Advantages:** High performance for real-time applications; optimisation of data storage.\
**Disadvantages:** Need to update the table structure if a new element that has not been included during the database design stage has to be added.

#### Value Model

| Station’s ID | Time       |Element | Value |
|--------------|------------|--------|-------|
| 33220        | 01/01/2002 | Tmin   | 23.4  |
| 42500        | 01/01/2003 | Tmax   | 16.3  |
| 22222        | 01/01/2003 |        |       |

**Advantages:** It is easy to add new elements, the model is adaptable to a large range of data types.\
**Disadvantages:** Optimization of data storage will not be done well, so this approach is not suitable for tables with huge amounts of data; also shares the disadvantages of the Element model.

#### Extensible Observation Model vs Collapsed Value Model 

| Station’s ID | Day/Month/Year | Tmin | Tmax | Rain | Min Humidity | Min MSL Pressure | Max Wind Speed | Additional Elements/values |
|--------------|----------------|------|------|------|--------------|------------------|----------------|----------------------------|
| 33220        | 01/01/2002     | 24.5 | 33.4 | 0    | 72           | 1015.6           | 2.2            | { [JSONB](https://www.postgresql.org/docs/current/functions-json.html) } |
| 42500        | 01/01/2003     | 15.2 | 22.3 | 10.2 | 80           | 1013.4           | 3.3            | { JSONB }                  |

... vs ...

| Station’s ID | Time       |Elements/Values |
|--------------|------------|----------------|
| 33220        | 01/01/2002 | { Tmin: 23.4, Tmax: 16.3, ... } |
| 42500        | 01/01/2003 |                |
| 22222        | 01/01/2003 |                |

**Advantages:** High performance, no longer need to update table structure if a new element is added, improved optimisation
**Disadvantages:** Not yet tested/evaluated.

## Focus systems

The OpenCDMS `reference-implementation` repository currently contains database schemas and documentation for CliDE, Climsoft, MCH and MIDAS. Documentation is also available for BDCLIM.

CliDE, Climsoft and MCH are all free/open-source CDMS solutions that are used extensively in developing countries. It is essential for the OpenCDMS project to support these projects and their users where possible. MIDAS is a bespoke CDMS developed and used by the UK Met Office. The MIDAS system is of particular interest to OpenCDMS because extensive datasets, with rich and complete metadata, are available as open data.

Support for other systems like CLIDATA that are in widespread use would also be desirable.

- CLIDATA (like CLICOM before it) implements the Element Model
- CliDE predominently implements the Observation Model
- Climsoft, MCH and MIDAS predominently implement the Value Model

In addition, further work is being undertaken by the OpenCDMS Reference Implementation Working Group to review a wider range of existing systems.

## Data ingestion and utilization

### Ingestion

Data can arrive in may formats. Climsoft 4 has temporary tables that are used during the ingestion of data in Element, Observation and Value Model formats. In the case of Climsoft, these are later transfered to a single `observationfinal` table that follows the Value model approach.

![Climsoft Element Model Example](https://raw.githubusercontent.com/opencdms/reference-implementation/master/data_model_review/images/climsoft4_form_daily2.png)\
*Figure:* Climsoft 4 Daily Key Entry form stores data for a single element at multiple times in a temporary table following the Element Model approach

![Climsoft Observation Model Example](https://raw.githubusercontent.com/opencdms/reference-implementation/master/data_model_review/images/climsoft4_form_synop_ra1.png)\
*Figure:* Climsoft 4 Synoptic Key Entry form stores data for multiple element for a single time in a temporary table following the Observation Model approach

```sql
CREATE TABLE IF NOT EXISTS `aws_rwanda1` (
  `Cols` int(11) DEFAULT '0',
  `Element_abbreviation` varchar(50) DEFAULT NULL,
  `Element_Name` varchar(50) DEFAULT NULL,
  `Element_Details` varchar(50) DEFAULT NULL,
  `Climsoft_Element` varchar(6) DEFAULT NULL,
  `Bufr_Element` varchar(6) DEFAULT NULL,
  `unit` varchar(15) DEFAULT NULL,
  `lower_limit` varchar(50) DEFAULT NULL,
  `upper_limit` varchar(50) DEFAULT NULL,
  `obsv` varchar(50) DEFAULT NULL,
  KEY `identification` (`Element_Name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
```
*Figure:* Example SQL Data Definition Language (DDL) for a temporary database table used to ingest automatic weather station data following the Value Model approach

The figures above illustrate that, in order to achieve full read support for existing Climsoft 4 installations alone, all model types must be supported.

Other systems, including CliDE, also implement a mix of data model types[⬞](https://github.com/opencdms/reference-implementation/blob/33a4b7d5b999b6257478d06bfa9e59db211b5d18/clide/README.md#readme).

### Utilization

Data arrangement is important for data processing and analysis. "Wide", or "unstacked", data is presented with each different data variable in a separate column (comparible to Observation Model). "Narrow", "stacked", or "long" data is presented with one column containing all the values and another column listing the context of the value (comparible to Value Model).

> Many statistical and data processing systems have functions to convert between these two presentations, for instance the R programming language has several packages such as the tidyr package. The pandas package in Python implements this operation as "melt" function which converts a wide table to a narrow one. The process of converting a narrow table to wide table is generally referred to as "pivoting" in the context of data transformations. The "pandas" python package provides a "pivot" method which provides for a narrow to wide transformation.
-- ["wide and narrow data"](https://en.m.wikipedia.org/wiki/Wide_and_narrow_data)

The wide arrangement is commonly used for analysis and is often refered to as "tidy data" where each observation, or case, is a row with each variable as a column. See relationship to [Boyce–Codd 3rd normal form](https://en.m.wikipedia.org/wiki/Tidy_data) ([Wickham 2014](#wickham_2014)).

For many queries, where users are selecting across a row, it is simpler to work with "tidy data". Wind is the most evident example of a bidimensional variable which would be easier to analyse if direction and speed were stored in the same table row[⬞](https://github.com/opencdms/reference-implementation/issues/10#issuecomment-667932508). A simple example would be the creation of a windrose plot where, for each time and location, both wind speed and wind direction are needed. If the data is arranged as "tidy data" then the analysis is more straight-forward for the user.

## Date period <!-- and partial dates-->

[#11](https://github.com/opencdms/reference-implementation/issues/11)

## Primary keys and indexing 

Unlike MCH and Climsoft, which both use composite natural keys, all tables in the [CliDE database schema](https://github.com/opencdms/datamodel/blob/master/clide/clidedb_schema.sql) contain an `id` synthetic primary key that can uniquely reference each row (e.g. see [obs_daily](https://github.com/opencdms/datamodel/blob/master/clide/obs_daily.md#obs_daily))

Both approaches have merit. However, when users interact with the database (e.g. via a Web API) there is broad agreement that the API should expose 'natural' selectors - i.e. that map onto real world entities and that are discoverable from the API[⬞](https://github.com/opencdms/reference-implementation/issues/9#issuecomment-667962467). Furthermore, since the OpenCDMS APIs will act as an abstraction layer for accessing multiple different systems, these interfaces will have to rely on selectors that all supported systems can implement (i.e. composite keys).

Note that composite natural keys may not be unique in systems that store more than one version of an observation. In the version is not selected then the system should be configurable as to which value to return by default (e.g. the original observation, the most recent, our current best value, etc.).

## Dynamic schema modifications

MCH allows variations in the database definition[⬞](https://github.com/opencdms/reference-implementation/issues/7#issuecomment-655578366). Example include:
* Support for table and field names in multiple languages (e.g., Spanish and English)
* Creation of a set of new database tables for each new variable that is added

## Interoperability

![Mappings](https://raw.githubusercontent.com/opencdms/reference-implementation/master/data_model_review/images/field_mappings.png)
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

### Data Model Type

For most initial use cases, speed of retrieval or the space taken should be the major issue when considering which data model type to adopt. If the element list needs to be very flexible, then adopting a Value Model would be more flexible than implementing Element or Observation Models[⬞](https://github.com/opencdms/reference-implementation/issues/10#issuecomment-667896059). For example, in Guatemala, MCH is used to store air quality variables and the data manager is able to easily define new elements such as NOx concentrations, etc[⬞](https://github.com/opencdms/reference-implementation/issues/10#issuecomment-667932508).

For the CDMS Reference Implementation, "read only" Element and Observation Model data model types can be achieved through the use of database views. We're unlikely to be able to retrospectively add views to existing systems that implement the the Value Model data model type. However, the same outcome can be achieved through the use of more complicated queries.

The recommentation for a future data model type requires further discussion.

<!--
Further discussion from @Steve-Palmer's comment onwards has yet to be incorporated here:
https://github.com/opencdms/reference-implementation/issues/10#issuecomment-667945076
-->

#### Hypertables

MCH makes use of manual partitioning by creating a separate observations table for each element. All data is still held in the same database instance, but split into separate tables. For some installations this may have performance benefits due to the reduction in index size in each table, which in turn results in improved search performance. Other systems 

However, in a time-series databases where indexing through time is essential, only partitioning the data into a relative small number of different variables would not be as effective as using a solution that partitions based on observation time (and optionally other values).

![Hypertable](https://raw.githubusercontent.com/opencdms/reference-implementation/master/data_model_review/images/timescaledb_hypertable_chunk.png)
*Figure:* TimescaleDB uses the "hypertable" abstraction as a virtual view of many individual tables holding the data, called chunks.

Like database sharding, hypertable partitioning allows the database to scale-out across multiple nodes.
<!-- however, also allows elasticity, reordering, tiering, ... https://blog.timescale.com/blog/building-a-distributed-time-series-database-on-postgresql/ -->

#### Domain Driven Design

The OpenCDMS Project Technical Team suggest following a Domain Driven Design (DDD) approach to the creation of the Reference Implementation data model to ensure that the terminology used in the Reference Implementation matches the language of the domain.
<!-- https://stackoverflow.com/questions/3835169/uml-domain-modeling/3835214#comment4077822_3835214 -->

#### Further research

The OpenCDMS Reference Implementation Working Group recommends further research to evaluate the optimum approach, including completing the assessment of components of the WMO CDMS specifications ([WMO 2014](#wmo_2014)) that require provisions to be present within data models and following other best practice.

The top-down design of the CDMS Reference Implementation data model will begin with a conceptual data model defining what the systems contains, followed by logical data model defining how the system should be implemented (regardless of the specifics of the physical implementation).
<!--
  https://en.wikipedia.org/wiki/Data_model#Three_perspectives
  https://en.wikipedia.org/wiki/Data_model#Entity-relationship_model
  https://www.tutorialspoint.com/dbms/dbms_data_models.htm
  https://www.guru99.com/data-modelling-conceptual-logical.html
  Entity relationship model vs UML
-->

Before making final recommendations for next generation climate data models, we propose a number of research questions that must be investigated:
- Flexibility vs efficiency – measure the implication of the transposing data stored in Value Model implementations for large data sets that are consistent with data volumns expected over the next 20-30 years.
- Time series data retrieval, with and without hypertables <!--(research must already exist)-->

## References

<span id="bojinski_2014">[Bojinski, S (2014)](https://journals.ametsoc.org/view/journals/bams/95/9/bams-d-13-00047.1.xml) The Concept of Essential Climate Variables in Support of Climate Research, Applications, and Policy, *Bulletin of the American Meteorological Society, 95*(9), 1431-1443.</span>\
<span id="wickham_2014">[Wickham, H (2014)](https://www.jstatsoft.org/article/view/v059i10) Tidy Data. *Journal of Statistical Software 59*(10)</span>\
<span id="wmo_2007">[WMO (2007)](https://library.wmo.int/index.php?lvl=notice_display&id=16656) Guidelines on climate data management. *WMO/TD- No. 1376*. WMO Geneva</span>\
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

## Annex A: Summary of database tables

#### CliDE

The following table lists the tables found in a a typical installation of CliDE

| Table name | Description |
|------------|-------------|
|codes_simple|List of codes used in CliDE|
|datums|Geodetic datums|
|equipment|Stores equipment master information.|
|station_types|Stores allowed values for stations.type_id|
|obs_aero|METAR / SPECI Aero message observations|
|obs_aws|AWS observations|
|obs_daily|Daily surface observations|
|obs_subdaily|Sub Daily surface observations|
|obs_monthly|Stores monthly data not available as daily or subdaily|
|obs_subdaily_cloud_layers|Sub Daily surface observations|
|obs_subdaily_soil_temps|Sub Daily surface observations|
|obs_upper_air|Upper Air observations|
|station_audit|Audit trail of all changes to station Station.|
|station_audit_types|Stores allowed values for station_audit.type_id|
|stations|Stores station data.|
|station_class|Stores contacts (people) for station|
|station_equipment|Stores equipment installed at station.|
|land_use|Stores allowed values for stations.soil_type_id|
|soil_types|Stores allowed values for stations.soil_type_id|
|station_countries|Stores countries that stations can belong to.|
|station_status|Stores allowed values for stations.status_id|
|station_timezones|Stores time zone that stations can be in.|
|surface_types|Stores allowed values for stations.surface_type_id|
|gui_users|User data for web GUI|
|ingest_monitor|Stores file ingestion stats for data ingests|
|key_settings|Stores key entry settings: Default units, disable flag|
|obs_audit|Audit trail of all changes to station Station.|
|obs_averages|Normals and other monthly long term averages of observations.|
|obs_clicom_element_map|Mapping Clicom Codes to CLDB table, column|
|obscodes_cloud_amt_conv|Cloud Amount conversions|
|obscodes_cloud_conv_1677|Cloud Height conversions for WMO 1677|
|obscodes_cloud_ht_conv|Cloud Height conversions|
|obscodes_cloud_type_conv|Cloud Type conversions|
|obscodes_visibility|Visibility conversions: Aero, non-Aero, Km, yards. WMO 4300|
|obscodes_wind_dir|Wind Direction conversions: Compass points to degrees|
|obscodes_wind_speed|Wind speed conversions: Beaufort, m/s, knots|
|obscodes_wx|WMO Code 4677 (WX codes)|
|obsconv_factors|WMO Code 4677 (WX codes)|
|pivot|Utility table of sequential integers|
|spatial_ref_sys|Spatial reference system definitions from PostGIS|
|station_contacts|Stores contacts (people) for station|
|station_files|Stores address of files such as images, pdfs, Word docs, etc. for station.|
|timezone_diffs|Stores timezone differences due to daylight savings|
|user_sessions|Stores User session information|

#### Climsoft 4

The following table lists the primary tables found in a a typical installation of Climsoft 4.

> Update (February 2021): The Climsoft team are currently in the process of adding table descriptions.

| Table name | Description |
|------------|-------------|
|abc||
|acquisitiontype||
|aws1||
|aws_basestation||
|aws_elements||
|aws_lsi||
|aws_lsi1||
|aws_malawi1||
|aws_malawi12||
|aws_mss||
|aws_process_parameters||
|aws_rema1||
|aws_rwanda1||
|aws_rwanda4||
|aws_rwanda_rain||
|aws_sasscal1||
|aws_sites||
|aws_stations||
|aws_structures||
|aws_tahmo||
|aws_test||
|aws_toa5_bw1||
|aws_toa5_mg2||
|bufr_crex_data||
|bufr_crex_master||
|bufr_indicators||
|ccitt||
|climsoftusers||
|code_flag||
|data_forms||
|faultresolution||
|featuregeographicalposition||
|flags||
|flagtable||
|form_agro1||
|form_daily2||
|form_hourly||
|form_hourlywind||
|form_hourly_time_selection||
|form_monthly||
|form_synoptic2_tdcf||
|form_synoptic_2_ra1||
|gaps||
|instrument||
|instrumentfaultreport||
|instrumentinspection||
|language_translation||
|missing_data||
|missing_stats||
|obselement||
|observationfinal||
|observationinitial||
|observationschedule||
|obsscheduleclass||
|paperarchive||
|paperarchivedefinition||
|physicalfeature||
|physicalfeatureclass||
|qcabslimits||
|qcstatusdefinition||
|qctype||
|qc_interelement_1||
|qc_interelement_2||
|qc_interelement_relationship_definition||
|regkeys||
|routinereportdefinition||
|routinereporttransmission||
|seq_daily_element||
|seq_day||
|seq_element||
|seq_hour||
|seq_leap_year||
|seq_month||
|seq_monthly_element||
|seq_month_day||
|seq_month_day_element||
|seq_month_day_element_leap_yr||
|seq_month_day_leap_yr||
|seq_month_day_synoptime||
|seq_month_day_synoptime_leap_yr||
|seq_year||
|ss||
|station||
|stationelement||
|stationidalias||
|stationlocationhistory||
|stationnetworkdefinition||
|stationqualifier||
|synopfeature||
|tblproducts||
|tdcf_indicators||
|testing_aws||
|testing_aws1||
|tm_307073||
|tm_307080||
|tm_307081||
|tm_307082||
|tm_307083||
|tm_307084||
|tm_307086||
|tm_307089||
|tm_307091||
|tm_307092||
|tm_309052||
|userrecords||

#### MCH

The following table lists the tables found in a a typical installation of MCH

| Table names | Description |
|-------------|-------------|
|Codes||
|Basins||
|Definiclineascontorno||
|DisponibDD||
|Estacautoma||
|stations||
|estacionesinstrum||
|metadatastations||
|states||
|Ftpbitacproc||
|regManager||
|StationGroups||
|TimeZones||
|Isolinbitac||
|Logbitacproc||
|Maps||
|MapasCroquis||
|mapsstations||
|MapasGenxCoord||
|mapsgroups||
|MapasMchxCoord||
|Mapaspixelgeogr||
|Mapaspixelgeogr4||
|mapasbycoord||
|Mapasxcoordclrs||
|Mapasxcoordgeogr||
|Mapasxcoordzonas||
|MensajesMetar||
|MensajesSynop||
|Counties||
|Opcionesmapasintxxnet||
|Opcxvariabautom||
|DataSources||
|Recepdefs||
|Recepsdatos||
|Recepsping||
|Hydrregions||
|subbasins||
|synopcrexdatos||
|synopcrexplant||
|TransfTables||
|Tablaswebconst||
|Tablaswebdef||
|tipoEstacionVariable||
|Transftp||
|Transmchamch||
|Units||
|users||
|typeusers||
|validdata||
|Valsvariabaut||
|Variabautomatv||
|Variabautomaxfecha||
|VariabDeriv2||
|VariabDeriv3||
|Variables||
|Variablestransf||
|VerifCerca||
|Verific||
|Webbitacoraproc||
|Webcontadores||
|ZonasAreas||

#### MIDAS

MIDAS contains over 100 tables, the SQL DDL has been made available but has not yet been uploaded. The first 15 tables are described below.

| Table names | Description |
|-------------|-------------|
| acquisitions_log | acquisitions_log IS 'Records recent data acquistions (ingestion) into the database |
| app_error_messages ||
| background_value | Contains NWP forecast model hourly background values, and MetDB time of receipt, for UK land and marine based automatic weather stations |
| british_summer_time | Contains start and end dates for British Summer Time |
| calendar_event | Special events (e.g. Wimbeldon, Easter, etc.) and their start and end dates |
| calendar_event_place | Resolves the many-to-many relationship between Calendar_Event (e.g. British Grand Prix) and the Places (e.g. Silverstone) where they occur |
| calendar_event_type | Defines and constrains the allowable values of calendar_event_type |
| calendar_weather_event | Resolves the many-to-many relationship between Calendar_Event (e.g. British Grand Prix) and notable Weather_Events |
| cdl_quality | Created for denormalised data from weather hourly obs, source and background values. Only original obs are stored |
| climate_area | Manages climate regions within the UK, other than Country and County |
| climate_area_place | Resolves the many-to-many relationship between Climate_Area and Place (i.e. gazeteer entry). The table will be used to record Places occurring within a defined Climate_Area |
| climate_area_post_code | Resolves the many-to-many relationship between Climate_Area and Post_Code.  The table will be used to record Post_Codes occurring within a defined Climate_Area |
| climate_area_statistics | Contains monthly areal statistics against regions |
| climate_area_type | Defines and constrains the allowable values for climate_area_type |
| climate_geographic_area | Resolves the many-to-many relationship between a climate area and a geographic area (e.g county) |
| ... | ... |

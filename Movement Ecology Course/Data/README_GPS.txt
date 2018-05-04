README

This data file is published by the Movebank Data Repository (www.datarepository.movebank.org). As of the time of publication, a version of the published animal tracking data set can be viewed on Movebank (www.movebank.org) in the study "Ya Ha Tinda elk project, Banff National Park (data from Hebblewhite et al. 2008)". Individual attributes in the data files are defined below and in the Movebank Attribute Dictionary, available at www.movebank.org/node/2381.

This data package includes the following data files:
Ya Ha Tinda elk project, Banff National Park (data from Hebblewhite et al. 2008)-gps.csv
Ya Ha Tinda elk project, Banff National Park (data from Hebblewhite et al. 2008)-radio-transmitter.csv
Ya Ha Tinda elk project, Banff National Park (data from Hebblewhite et al. 2008)-reference-data.csv

These data comprise part or all of the elk tracking data described in the following written publications:
Eggeman S, Hebblewhite M, Bohm H, Whittington J, Merril E (2016) Behavioral flexibility in migratory behavior in a long-lived large herbivore. Journal of Animal Ecology. doi:10.1111/1365-2656.12495

Hebblewhite M, Merrill EH (2009) Trade-offs between predation risk and forage differ between migrant strategies in a migratory ungulate. Ecology 90(12):3445-3454. doi:10.1890/08-2090.1

Hebblewhite M, Merrill EH (2008) Modelling wildlife-human relationships for social species with mixed-effects resource selection models. Journal of Applied Ecology 45(3):834-844. doi:10.1111/j.1365-2664.2008.01466.x

Hebblewhite M, Merrill EH, McDermid G (2008) A multi-scale test of the forage maturation hypothesis in a partially migratory ungulate population. Ecological Monographs 78(2):141-166. doi:10.1890/06-1708.1

Hebblewhite M, Merrill EH (2007) Multiscale wolf predation risk for elk: does migration reduce risk? Oecologia 152(2):377-387. doi:10.1007/s00442-007-0661-y

Hebblewhite M, Merrill EH, Morgantini LE, White CA, Allen JR, Bruns E, Thurston L, Hurd TE (2006) Is the migratory behaviour of montane elk herds in peril? The case of Alberta’s Ya Ha Tinda elk herd. Wildlife Society Bulletin 34(5):1280-1294. doi:10.2193/0091-7648(2006)34[1280:ITMBOM]2.0.CO;2

Hebblewhite (2006) Linking predation risk and forage to ungulate population dynamics. Dissertation, University of Alberta, Edmonton, Alberta.

Data package citation:
Hebblewhite M, Merrill E (2015) Data from: A multi-scale test of the forage maturation hypothesis in a partially migratory ungulate population. Movebank Data Repository. doi:10.5441/001/1.k8s2g5v7

Note that there are no missing GPS tags in the data package; 25 is the total number for the dataset.

-----------

Terms of Use
This data file is licensed by the Creative Commons Zero (CC0 1.0) license. The intent of this license is to facilitate the re-use of works. The Creative Commons Zero license is a "no rights reserved" license that allows copyright holders to opt out of copyright protections automatically extended by copyright and other laws, thus placing works in the public domain with as little legal restriction as possible. However, works published with this license must still be appropriately cited following professional and ethical standards for academic citation.

We highly recommend that you contact the data creator if possible if you will be re-using or re-analyzing data in this file. Researchers will likely be interested in learning about new uses of their data, might also have important insights about how to properly analyze and interpret their data, and/or might have additional data they would be willing to contribute to your project. Feel free to contact us at support@movebank.org if you need assistance contacting data owners.

See here for the full description of this license
http://creativecommons.org/publicdomain/zero/1.0

-----------

Data Attributes
These definitions come from the Movebank Attribute Dictionary, available at www.movebank.org/node/2381.

animal ID: An individual identifier for the animal, provided by the data owner. This identifier can be a ring number, a name, the same as the associated tag ID, etc. If the data owner does not provide an Animal ID, an internal Movebank animal identifier may sometimes be shown.
	example: 91876A, Gary
	same as: individual-local-identifier

attachment type: The way a tag is attached to an animal. Values are chosen from a controlled list:
	collar: The tag is attached by a collar around the animal's neck.
	glue: The tag is attached to the animal using glue.
	harness: The tag is attached to the animal using a harness.
	implant: The tag is placed under the skin of the an animal.
	tape: The tag is attached to the animal using tape.
	other: user specified

deploy off timestamp: The timestamp when the tag deployment ended.
	example: 2009-10-01 12:00:00.000
	format: yyyy-MM-dd HH:mm:ss.sss
	units: UTC (Coordinated Universal Time) or GPS time, which is a few leap seconds different from UTC
	same as: deploy off date
THIS DATASET: Values are not provided for all deployments.

deploy on timestamp: The timestamp when the tag deployment started.
	example: 2008-08-30 18:00:00.000
	format: yyyy-MM-dd HH:mm:ss.sss
	units: UTC (Coordinated Universal Time) or GPS time, which is a few leap seconds different from UTC
	same as: deploy on date
THIS DATASET: Values are not provided for all deployments.

event comments: Additional information about individual records or events in a data set that is not described by other reference data terms.
	example: we observed the animal foraging (see photo BT25)
	same as: comments

event ID: An identifier for the set of information associated with each record or event in a data set. A unique event ID is assigned to every time-location or other time-measurement record in Movebank.
	example: 6340565
	units: none

GPS fix type: The type of GPS fix. 1 = no fix; 2 = 2D fix (altitude typically not valid); 3 = 3D fix (altitude typically valid).
	example: 3
	units: none

GPS satellite count: The number of GPS satellites used to estimate the location.
	example: 8
	units: none

height above mean sea level: The estimated height of the tag above mean sea level returned by the GPS unit. (If altitudes are calculated as height above an ellipsoid, use height above ellipsoid.)
	example: 34
	units: meters
	same as: height above msl

latitude (decimal degree): The geographic longitude of a location along an animal track as estimated by the processed sensor data. Positive values are east of the Greenwich Meridian, negative values are west of it.
	example: -121.1761111
	units: decimal degrees, WGS84 reference system
	same as: location lat

longitude (decimal degree): The geographic longitude of a location along an animal track as estimated by the processed sensor data. Positive values are east of the Greenwich Meridian, negative values are west of it.
	example: -121.1761111
	units: decimal degrees, WGS84 reference system
	same as: location long

manipulation type: The way in which the animal was manipulated during the deployment. Additional details about the manipulation can be provided using manipulation comments. Values are chosen from a controlled list:
	confined: The animal's movement was restricted to within a defined area.
	none: The animal received no treatment other than the tag attachment.
	relocated: The animal was released from a site other than the one at which it was captured.
	manipulated other: The animal was manipulated in some other way, such as a physiological manipulation.

migration stage custom: The migration stage of the animal. Values are specific to the study. To use a controlled list of migration stages that can be compared across studies, use migration stage standard.
	example: Stopover #1d
	same as: migration stage
THIS DATASET: 0 = resident, 1 = migrant, defined following Hebblewhite et al. 2008

sensor type: The type of sensor with which data were collected. Values are chosen from a controlled list:
	acceleration: The sensor collects acceleration data.
	accessory measurements: The sensor collects accessory measurements, such as battery voltage.
	Argos Doppler shift: The sensor is using Argos Doppler shift for determining position.
	barometer: The sensor records air or water pressure.
	bird ring: The animal is identified by a ring that has a unique ID.
	GPS: The sensor uses GPS to find location and stores these.
	magnetometer: The sensor records the magnetic field.
	natural mark: The animal is identified by a natural marking.
	radio transmitter: The sensor is a classical radio transmitter.
	solar geolocator: The sensor collects light levels, which are used to determine position (for processed locations).
	solar geolocator raw: The sensor collects light levels, which are used to determine position (for raw light-level measurements).

sex: The sex of the biological individual(s) represented in the Occurrence. Values are from a controlled list:
	m: male
	f: female

study: The name of the study in Movebank in which data are stored.

study-specific measurement: Values for a study-specific attribute.
	units: undefined
THIS DATASET: Values for GPS records contain 4 entries separated by ":"
(1) year
(2) season
(3) bioseason (a value combining year/s and season)
(4) day/night
Values for radio transmitter records contain the year.

tag comments: Additional information about the tag that is not described by other reference data terms.
	example: custom-made Doppler shift Argos tag with a special altitude sensor

tag ID: A unique identifier for the tag, provided by the data owner. If the data owner does not provide a tag ID, an internal Movebank tag identifier may sometimes be shown.
	example: 2342, ptt_4532
	same as: tag local identifier

tag manufacturer name: The company or person that produced the tag.
	example: Holohil
	same as: manufacturer

tag readout method: The way the data are received from the tag. Values are chosen from a controlled list:
	satellite: Data are transferred via satellite.
	phone network: Data are transferred via a phone network, such as GSM or AMPS.
	other wireless: Data are transferred via another form of wireless data transfer, such as a VHF radio transmitter/receiver.
	tag retrieval: The tag must be physically retrieved in order to obtain the data.

tag technical specification: Values for a tag-specific technical attribute.
	example: 8.31, YES
	units: undefined
	same as: tag tech. spec.
THIS DATASET: Values include three entries separated by ":"
(1) dilution of precision (DOP) measured by the GPS
(2) Lotek GPS axis 1 activity count
(3) Lotek GPS axis 2 activity count

taxon: The scientific name of the species on which the tag was deployed, as defined by the Integrated Taxonomic Information System (ITIS, www.itis.gov). If the species name can not be provided, this should be the lowest level taxonomic rank that can be determined and that is used in the ITIS taxonomy. Additional information can be provided using the term taxon detail.
	example: Buteo swainsoni
	same as: species, animal taxon, individual taxon canonical name

temperature external: The temperature measured by the tag (different from ambient temperature or internal body temperature of the animal).
	example: 32.1
	units: degrees Celsius

timestamp: The date and time a sensor measurement was taken.
	example: 2008-08-14 18:31:00.000
	format: yyyy-MM-dd HH:mm:ss.sss
	units: UTC (Coordinated Universal Time) or GPS time, which is a few leap seconds different from UTC

visible: Determines whether an event is visible on the Movebank Search map. Values are calculated automatically, with FALSE indicating that the event has been marked as an outlier by manually marked outlier or algorithm marked outlier. Allowed values are TRUE or FALSE.

-----------

More Information
For more information about this repository, see www.movebank.org/node/15294, the FAQ at www.movebank.org/node/2220, or contact us at support@movebank.org.
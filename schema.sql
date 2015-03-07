/*Server storage schema

All right, forget it... I just put in CREATE TABLES commands.

the idea is that this table will just pile up sensor events. There's a
temptation to implement some kind of run-length-encoding to avoid piles
and piles of data, but I think for now this is the easier solution.

Also, note that I'm specifying the reading as an integer; I think this strikes
a good balance between filterability (give me all the readings higher than x)
and flexibility (different sensors can encode their readings in different
ways).*/

--
-- Table structure for table `sensorevents`
--

CREATE TABLE `sensorevents` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `device` varchar(64) NOT NULL,
  `timestamp` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  `reading` bigint(20) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `device` (`device`),
  CONSTRAINT `sensorevents_ibfk_1` FOREIGN KEY (`device`) REFERENCES `devices` (`name`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `controlevents`
--

CREATE TABLE `controlevents` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `device` varchar(64) NOT NULL,
  `timestamp` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  `setting` bigint(20) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `device` (`device`),
  CONSTRAINT `controlevents_ibfk_1` FOREIGN KEY (`device`) REFERENCES `devices` (`name`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `controleventresults`
--

CREATE TABLE `controleventresults` (
  `id` int(11) NOT NULL,
  `code` int(11) NOT NULL,
  `details` varchar(256) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `code` (`code`),
  CONSTRAINT `controleventresults_ibfk_1` FOREIGN KEY (`id`) REFERENCES `controlevents` (`id`),
  CONSTRAINT `controleventresults_ibfk_2` FOREIGN KEY (`code`) REFERENCES `controleventresultcodes` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `egauge`
--

CREATE TABLE `egauge` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `timestamp` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  `usage` decimal(6, 4) NOT NULL,
  `generation` decimal(6, 4) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `controleventresultcodes`
--

CREATE TABLE `controleventresultcodes` (
  `id` int(11) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;



--
-- Table structure for table `devices`
--

CREATE TABLE `devices` (
  `name` varchar(64) NOT NULL,
  PRIMARY KEY (`name`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
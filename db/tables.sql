-- MySQL dump 10.11
--
-- Host: localhost    Database: autoan
-- ------------------------------------------------------
-- Server version	5.0.51a-24+lenny5

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `admin`
--

DROP TABLE IF EXISTS `admin`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `admin` (
  `AdNr` int(10) unsigned NOT NULL auto_increment,
  `Name` varchar(20) NOT NULL default '',
  `Passwort` varchar(20) NOT NULL default '',
  PRIMARY KEY  (`AdNr`)
) ENGINE=MyISAM AUTO_INCREMENT=2 DEFAULT CHARSET=latin1;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `aufgabe`
--

DROP TABLE IF EXISTS `aufgabe`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `aufgabe` (
  `ANr` int(11) NOT NULL auto_increment,
  `Name` varchar(30) NOT NULL default '',
  `VNr` int(10) NOT NULL default '0',
  `Highscore` enum('low','high','no') NOT NULL default 'no',
  `Von` datetime NOT NULL default '2008-01-01 00:00:00',
  `Bis` datetime NOT NULL default '2008-01-01 00:00:00',
  `Config` blob,
  `Remark` text,
  `Typ` blob,
  `Status` enum('demo','mandatory','optional') default NULL,
  `server` blob,
  `signature` blob,
  PRIMARY KEY  (`ANr`)
) ENGINE=MyISAM AUTO_INCREMENT=2071 DEFAULT CHARSET=latin1 COMMENT='aufgabe';
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `direktor`
--

DROP TABLE IF EXISTS `direktor`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `direktor` (
  `SNr` int(10) default NULL,
  `UNr` int(10) default NULL
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `gruppe`
--

DROP TABLE IF EXISTS `gruppe`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `gruppe` (
  `GNr` int(10) unsigned NOT NULL auto_increment,
  `VNr` int(10) unsigned NOT NULL default '0',
  `Name` varchar(100) default 'GruppenName <empty>',
  `MaxStudents` int(10) unsigned NOT NULL default '0',
  `Referent` varchar(60) NOT NULL default 'unbekannt',
  PRIMARY KEY  (`GNr`)
) ENGINE=MyISAM AUTO_INCREMENT=349 DEFAULT CHARSET=latin1;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `minister`
--

DROP TABLE IF EXISTS `minister`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `minister` (
  `SNr` int(10) default NULL
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `punkte`
--

DROP TABLE IF EXISTS `punkte`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `punkte` (
  `Rubrik` varchar(25) default NULL,
  `MNr` varchar(12) default NULL,
  `punkte` int(11) default NULL
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `schule`
--

DROP TABLE IF EXISTS `schule`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `schule` (
  `UNr` int(11) NOT NULL default '0',
  `Name` varchar(50) NOT NULL default '',
  `Mail_Suffix` text NOT NULL
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `semester`
--

DROP TABLE IF EXISTS `semester`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `semester` (
  `ENr` int(11) NOT NULL auto_increment,
  `UNr` int(10) NOT NULL default '0',
  `Name` varchar(30) NOT NULL default '',
  `Von` datetime NOT NULL default '2008-01-01 00:00:00',
  `Bis` datetime NOT NULL default '2008-01-01 00:00:00',
  PRIMARY KEY  (`ENr`)
) ENGINE=MyISAM AUTO_INCREMENT=70 DEFAULT CHARSET=latin1 COMMENT='semester';
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `stud_aufg`
--

DROP TABLE IF EXISTS `stud_aufg`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `stud_aufg` (
  `SNr` int(10) unsigned NOT NULL default '0',
  `ANr` int(10) unsigned NOT NULL default '0',
  `Ok` int(11) default NULL,
  `No` int(11) default NULL,
  `size` int(11) default NULL,
  `Scoretime` datetime NOT NULL default '0000-00-00 00:00:00',
  `Input` text,
  `Report` text,
  `Result` text,
  `Instant` text,
  PRIMARY KEY  (`SNr`,`ANr`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COMMENT='Aufgaben Student';
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `stud_grp`
--

DROP TABLE IF EXISTS `stud_grp`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `stud_grp` (
  `SNr` int(10) unsigned NOT NULL default '0',
  `GNr` int(10) unsigned NOT NULL default '0',
  PRIMARY KEY  (`SNr`,`GNr`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `student`
--

DROP TABLE IF EXISTS `student`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `student` (
  `SNr` int(10) unsigned NOT NULL auto_increment,
  `MNr` varchar(12) NOT NULL default '',
  `Name` varchar(25) NOT NULL default '',
  `Vorname` varchar(25) NOT NULL default '',
  `Email` varchar(150) NOT NULL default '',
  `Status` varchar(10) NOT NULL default 'inaktiv',
  `Passwort` varchar(64) default NULL,
  `UNr` int(11) NOT NULL default '0',
  `Next_Passwort` varchar(64) NOT NULL default '',
  PRIMARY KEY  (`SNr`)
) ENGINE=MyISAM AUTO_INCREMENT=3193 DEFAULT CHARSET=latin1 COMMENT='angemeldete Studenten';
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `tutor`
--

DROP TABLE IF EXISTS `tutor`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `tutor` (
  `SNr` int(10) default NULL,
  `VNr` int(10) default NULL
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `vorlesung`
--

DROP TABLE IF EXISTS `vorlesung`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `vorlesung` (
  `VNr` int(10) unsigned NOT NULL auto_increment,
  `Name` varchar(50) NOT NULL default '',
  `EinschreibVon` datetime NOT NULL default '2005-01-01 00:00:00',
  `EinschreibBis` datetime NOT NULL default '2005-01-01 00:00:00',
  `unr` int(11) NOT NULL default '0',
  `motd` text NOT NULL,
  `ENr` int(10) default NULL,
  PRIMARY KEY  (`VNr`)
) ENGINE=MyISAM AUTO_INCREMENT=197 DEFAULT CHARSET=latin1 COMMENT='vorlesung';
SET character_set_client = @saved_cs_client;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2014-01-27 12:01:01


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
DROP TABLE IF EXISTS `checksums`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `checksums` (
  `org_id` char(32) COLLATE utf8_bin NOT NULL,
  `checksum` char(32) COLLATE utf8_bin NOT NULL,
  PRIMARY KEY (`org_id`,`checksum`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `clients`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `clients` (
  `id` char(32) COLLATE utf8_bin NOT NULL,
  `org_id` char(32) COLLATE utf8_bin NOT NULL,
  `authz_id` char(32) COLLATE utf8_bin NOT NULL,
  `name` varchar(255) COLLATE utf8_bin NOT NULL,
  `public_key` text COLLATE utf8_bin,
  `validator` tinyint(1) NOT NULL,
  `last_updated_by` char(32) COLLATE utf8_bin NOT NULL,
  `created_at` datetime NOT NULL,
  `updated_at` datetime NOT NULL,
  `pubkey_version` smallint(6) NOT NULL,
  `admin` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  UNIQUE KEY `authz_id` (`authz_id`),
  UNIQUE KEY `org_id_name_unique` (`org_id`,`name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `cookbook_version_checksums`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `cookbook_version_checksums` (
  `cookbook_version_id` char(32) COLLATE utf8_bin NOT NULL,
  `org_id` char(32) COLLATE utf8_bin NOT NULL,
  `checksum` char(32) COLLATE utf8_bin NOT NULL,
  KEY `cookbook_version_id` (`cookbook_version_id`),
  KEY `cookbook_version_checksums_org_id_fkey` (`org_id`,`checksum`),
  CONSTRAINT `cookbook_version_checksums_org_id_fkey` FOREIGN KEY (`org_id`, `checksum`) REFERENCES `checksums` (`org_id`, `checksum`) ON UPDATE CASCADE,
  CONSTRAINT `cookbook_version_checksums_ibfk_1` FOREIGN KEY (`cookbook_version_id`) REFERENCES `cookbook_versions` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `cookbook_version_dependencies`;
/*!50001 DROP VIEW IF EXISTS `cookbook_version_dependencies`*/;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
/*!50001 CREATE TABLE `cookbook_version_dependencies` (
  `org_id` char(32),
  `name` varchar(255),
  `major` int(11),
  `minor` int(11),
  `patch` int(11),
  `dependencies` varchar(255)
) ENGINE=MyISAM */;
SET character_set_client = @saved_cs_client;
DROP TABLE IF EXISTS `cookbook_versions`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `cookbook_versions` (
  `id` char(32) COLLATE utf8_bin NOT NULL,
  `major` int(11) NOT NULL,
  `minor` int(11) NOT NULL,
  `patch` int(11) NOT NULL,
  `frozen` tinyint(1) NOT NULL,
  `meta_attributes` mediumblob NOT NULL,
  `meta_deps` varchar(255) COLLATE utf8_bin NOT NULL,
  `meta_long_desc` mediumblob NOT NULL,
  `metadata` mediumblob NOT NULL,
  `serialized_object` mediumblob NOT NULL,
  `updated_at` datetime NOT NULL,
  `created_at` datetime NOT NULL,
  `last_updated_by` char(32) COLLATE utf8_bin NOT NULL,
  `cookbook_id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `cookbook_id` (`cookbook_id`,`major`,`minor`,`patch`),
  CONSTRAINT `cookbook_versions_ibfk_1` FOREIGN KEY (`cookbook_id`) REFERENCES `cookbooks` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `cookbooks`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `cookbooks` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `org_id` char(32) COLLATE utf8_bin NOT NULL,
  `name` varchar(255) COLLATE utf8_bin NOT NULL,
  `authz_id` char(32) COLLATE utf8_bin NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `authz_id` (`authz_id`),
  UNIQUE KEY `org_id` (`org_id`,`name`),
  KEY `cookbooks_org_id_index` (`org_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `data_bag_items`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `data_bag_items` (
  `id` char(32) COLLATE utf8_bin NOT NULL,
  `org_id` char(32) COLLATE utf8_bin NOT NULL,
  `data_bag_name` varchar(255) COLLATE utf8_bin NOT NULL,
  `item_name` varchar(255) COLLATE utf8_bin NOT NULL,
  `last_updated_by` char(32) COLLATE utf8_bin NOT NULL,
  `created_at` datetime NOT NULL,
  `updated_at` datetime NOT NULL,
  `serialized_object` mediumblob,
  PRIMARY KEY (`id`),
  UNIQUE KEY `org_id` (`org_id`,`data_bag_name`,`item_name`),
  CONSTRAINT `data_bag_items_ibfk_1` FOREIGN KEY (`org_id`, `data_bag_name`) REFERENCES `data_bags` (`org_id`, `name`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `data_bags`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `data_bags` (
  `id` char(32) COLLATE utf8_bin NOT NULL,
  `authz_id` char(32) COLLATE utf8_bin NOT NULL,
  `org_id` char(32) COLLATE utf8_bin NOT NULL,
  `name` varchar(255) COLLATE utf8_bin NOT NULL,
  `last_updated_by` char(32) COLLATE utf8_bin NOT NULL,
  `created_at` datetime NOT NULL,
  `updated_at` datetime NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `authz_id` (`authz_id`),
  UNIQUE KEY `org_id` (`org_id`,`name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `environments`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `environments` (
  `id` char(32) COLLATE utf8_bin NOT NULL,
  `authz_id` char(32) COLLATE utf8_bin NOT NULL,
  `org_id` char(32) COLLATE utf8_bin NOT NULL,
  `name` varchar(255) COLLATE utf8_bin NOT NULL,
  `last_updated_by` char(32) COLLATE utf8_bin NOT NULL,
  `created_at` datetime NOT NULL,
  `updated_at` datetime NOT NULL,
  `serialized_object` mediumblob,
  PRIMARY KEY (`id`),
  UNIQUE KEY `authz_id` (`authz_id`),
  UNIQUE KEY `org_id` (`org_id`,`name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `joined_cookbook_version`;
/*!50001 DROP VIEW IF EXISTS `joined_cookbook_version`*/;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
/*!50001 CREATE TABLE `joined_cookbook_version` (
  `major` int(11),
  `minor` int(11),
  `patch` int(11),
  `version` varchar(35),
  `serialized_object` mediumblob,
  `id` char(32),
  `org_id` char(32),
  `name` varchar(255)
) ENGINE=MyISAM */;
SET character_set_client = @saved_cs_client;
DROP TABLE IF EXISTS `nodes`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `nodes` (
  `id` char(32) COLLATE utf8_bin NOT NULL,
  `authz_id` char(32) COLLATE utf8_bin NOT NULL,
  `org_id` char(32) COLLATE utf8_bin NOT NULL,
  `name` varchar(255) COLLATE utf8_bin NOT NULL,
  `environment` varchar(255) COLLATE utf8_bin NOT NULL,
  `last_updated_by` char(32) COLLATE utf8_bin NOT NULL,
  `created_at` datetime NOT NULL,
  `updated_at` datetime NOT NULL,
  `serialized_object` mediumblob,
  PRIMARY KEY (`id`),
  UNIQUE KEY `authz_id` (`authz_id`),
  UNIQUE KEY `org_id` (`org_id`,`name`),
  KEY `nodes_org_id_environment_index` (`org_id`,`environment`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `roles`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `roles` (
  `id` char(32) COLLATE utf8_bin NOT NULL,
  `authz_id` char(32) COLLATE utf8_bin NOT NULL,
  `org_id` char(32) COLLATE utf8_bin NOT NULL,
  `name` varchar(255) COLLATE utf8_bin NOT NULL,
  `last_updated_by` char(32) COLLATE utf8_bin NOT NULL,
  `created_at` datetime NOT NULL,
  `updated_at` datetime NOT NULL,
  `serialized_object` mediumblob,
  PRIMARY KEY (`id`),
  UNIQUE KEY `authz_id` (`authz_id`),
  UNIQUE KEY `org_id` (`org_id`,`name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `sandboxed_checksums`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `sandboxed_checksums` (
  `org_id` char(32) COLLATE utf8_bin NOT NULL DEFAULT '',
  `sandbox_id` char(32) COLLATE utf8_bin NOT NULL DEFAULT '',
  `checksum` char(32) COLLATE utf8_bin NOT NULL DEFAULT '',
  `created_at` datetime NOT NULL,
  PRIMARY KEY (`sandbox_id`,`org_id`,`checksum`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `schema_info`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `schema_info` (
  `version` int(11) NOT NULL DEFAULT '0'
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `users`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `users` (
  `id` char(32) COLLATE utf8_bin NOT NULL,
  `authz_id` char(32) COLLATE utf8_bin NOT NULL,
  `username` varchar(255) COLLATE utf8_bin NOT NULL,
  `email` varchar(255) COLLATE utf8_bin DEFAULT NULL,
  `pubkey_version` int(11) NOT NULL,
  `public_key` text COLLATE utf8_bin,
  `serialized_object` text COLLATE utf8_bin,
  `last_updated_by` char(32) COLLATE utf8_bin NOT NULL,
  `created_at` datetime NOT NULL,
  `updated_at` datetime NOT NULL,
  `external_authentication_uid` varchar(255) COLLATE utf8_bin DEFAULT NULL,
  `recovery_authentication_enabled` tinyint(1) DEFAULT NULL,
  `admin` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  UNIQUE KEY `authz_id` (`authz_id`),
  UNIQUE KEY `username` (`username`),
  UNIQUE KEY `email` (`email`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin;
/*!40101 SET character_set_client = @saved_cs_client */;
/*!50003 DROP PROCEDURE IF EXISTS `cookbook_recipes` */;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8 */ ;
/*!50003 SET character_set_results = utf8 */ ;
/*!50003 SET collation_connection  = utf8_general_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = '' */ ;
DELIMITER ;;
/*!50003 CREATE*/ /*!50020 DEFINER=CURRENT_USER*/ /*!50003 PROCEDURE `cookbook_recipes`(IN organization CHAR(32))
BEGIN
    -- Set up the information in the temporary table...
    CALL prepare_latest_cookbook_data(organization, 1);

    -- ... then query the temporary table in order to return the results
    --
    -- The column aliases are important, because these are what the Erchef code
    -- is expecting in order to properly process the results
    SELECT cookbook_name AS name, serialized_object
    FROM latest_cb_versions_temp
    ORDER BY name;
END */;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;
/*!50003 DROP PROCEDURE IF EXISTS `latest_cookbook_versions` */;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8 */ ;
/*!50003 SET character_set_results = utf8 */ ;
/*!50003 SET collation_connection  = utf8_general_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = '' */ ;
DELIMITER ;;
/*!50003 CREATE*/ /*!50020 DEFINER=CURRENT_USER*/ /*!50003 PROCEDURE `latest_cookbook_versions`(IN organization CHAR(32), IN num_latest INTEGER)
BEGIN
    -- Set up the information in the temporary table...
    CALL prepare_latest_cookbook_data(organization, num_latest);

    -- ... then query the temporary table in order to return the results
    --
    -- The column aliases are important, because these are what the Erchef code
    -- is expecting in order to properly process the results
    SELECT cookbook_name AS name, CONCAT_WS('.', major, minor, patch) AS version
    FROM latest_cb_versions_temp
    ORDER BY name, major DESC, minor DESC, patch DESC;
END */;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;
/*!50003 DROP PROCEDURE IF EXISTS `prepare_latest_cookbook_data` */;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8 */ ;
/*!50003 SET character_set_results = utf8 */ ;
/*!50003 SET collation_connection  = utf8_general_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = '' */ ;
DELIMITER ;;
/*!50003 CREATE*/ /*!50020 DEFINER=CURRENT_USER*/ /*!50003 PROCEDURE `prepare_latest_cookbook_data`(IN organization CHAR(32), IN num_latest INTEGER)
BEGIN
    -- This stored procedure ultimately generates a temporary table filled
    -- with the cookbook name, version information, and serialized object for
    -- the `num_latest` most recent versions of all cookbooks in an organization.

    -- This temporary table, named 'latest_cb_versions_temp' is then utilized in
    -- other stored procedures that either return the version information or the
    -- recipe information.  This separation is undesirable, but necessary, because
    -- MySQL stored procedures cannot actually return result sets directly, and
    -- cannot be used as table expressions in queries.

    -- The INSTANT that MySQL ever gets window query support, this stored
    -- procedure and the two that follow it should IMMEDIATELY be replaced
    -- with a view like we have on Postgres

    -- -----------------------------------------------------------------------------

    -- These variables correspond to the columns we select in the cursor, and what
    -- we insert into the temporary table.
    --
    -- Should we decide to change the types of the corresponding columns in the
    -- cookbooks or cookbook_versions table, these declarations and the definition
    -- of the temporary table should be altered as appropriate.
    DECLARE current_cookbook VARCHAR(255);
    DECLARE major INTEGER;
    DECLARE minor INTEGER;
    DECLARE patch INTEGER;
    DECLARE serialized_object MEDIUMBLOB;

    -- These are bookkeeping / incidental variables needed to make this all work
    DECLARE last_cookbook VARCHAR(255) DEFAULT '';
    DECLARE current_rank INTEGER;
    DECLARE no_more_rows BOOLEAN;

    -- NOTE: All variables must be declared BEFORE cursors or handlers, or MySQL freaks out

    -- The query that makes it all happen
    DECLARE cur CURSOR FOR
        SELECT c.name, v.major, v.minor, v.patch, v.serialized_object
        FROM cookbook_versions AS v
        JOIN cookbooks AS c
          ON v.cookbook_id = c.id
        WHERE c.org_id = organization
        ORDER BY c.name, v.major DESC, v.minor DESC, v.patch DESC
    ;

    -- Gotta do this so MySQL doesn't freak out when there are no more results
    DECLARE CONTINUE HANDLER FOR NOT FOUND SET no_more_rows = TRUE;

    DROP TEMPORARY TABLE IF EXISTS latest_cb_versions_temp;
    CREATE TEMPORARY TABLE latest_cb_versions_temp(
        cookbook_name VARCHAR(255),
        major INTEGER,
        minor INTEGER,
        patch INTEGER,
        serialized_object MEDIUMBLOB
    );

    OPEN cur;
    the_loop: LOOP

        FETCH cur INTO current_cookbook, major, minor, patch, serialized_object;

        IF no_more_rows THEN
            CLOSE cur;
            LEAVE the_loop;
        END IF;

        -- As we go through the cursor's results, we need to pay attention to
        -- when we "cross the boundary" between cookbooks.  We also need to be
        -- aware of how many versions we've already seen

        IF current_cookbook != last_cookbook THEN
            -- We are encountering this cookbook for the first time
            -- We always want to capture at least one version for a cookbook,
            -- so we'll go ahead and insert this one into the temp table
            SET current_rank = 1;
            INSERT INTO latest_cb_versions_temp
                VALUES(current_cookbook, major, minor, patch, serialized_object);
        ELSE
            -- We are in a block of cookbooks that we have already gotten at least one version from.
            -- Check to see if we've already taken the requisite number of versions for this cookbook
            IF current_rank < num_latest THEN
                SET current_rank = current_rank + 1;
                INSERT INTO latest_cb_versions_temp
                    VALUES(current_cookbook, major, minor, patch, serialized_object);
            END IF;
        END IF;

        -- Each time through the loop we need to record what cookbook we saw last
        SET last_cookbook = current_cookbook;

    END LOOP the_loop;
END */;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;
/*!50001 DROP TABLE IF EXISTS `cookbook_version_dependencies`*/;
/*!50001 DROP VIEW IF EXISTS `cookbook_version_dependencies`*/;
/*!50001 SET @saved_cs_client          = @@character_set_client */;
/*!50001 SET @saved_cs_results         = @@character_set_results */;
/*!50001 SET @saved_col_connection     = @@collation_connection */;
/*!50001 SET character_set_client      = utf8 */;
/*!50001 SET character_set_results     = utf8 */;
/*!50001 SET collation_connection      = utf8_general_ci */;
/*!50001 CREATE ALGORITHM=UNDEFINED */
/*!50013 DEFINER=CURRENT_USER SQL SECURITY DEFINER */
/*!50001 VIEW `cookbook_version_dependencies` AS select `c`.`org_id` AS `org_id`,`c`.`name` AS `name`,`v`.`major` AS `major`,`v`.`minor` AS `minor`,`v`.`patch` AS `patch`,`v`.`meta_deps` AS `dependencies` from (`cookbooks` `c` join `cookbook_versions` `v` on((`c`.`id` = `v`.`cookbook_id`))) */;
/*!50001 SET character_set_client      = @saved_cs_client */;
/*!50001 SET character_set_results     = @saved_cs_results */;
/*!50001 SET collation_connection      = @saved_col_connection */;
/*!50001 DROP TABLE IF EXISTS `joined_cookbook_version`*/;
/*!50001 DROP VIEW IF EXISTS `joined_cookbook_version`*/;
/*!50001 SET @saved_cs_client          = @@character_set_client */;
/*!50001 SET @saved_cs_results         = @@character_set_results */;
/*!50001 SET @saved_col_connection     = @@collation_connection */;
/*!50001 SET character_set_client      = utf8 */;
/*!50001 SET character_set_results     = utf8 */;
/*!50001 SET collation_connection      = utf8_general_ci */;
/*!50001 CREATE ALGORITHM=UNDEFINED */
/*!50013 DEFINER=CURRENT_USER SQL SECURITY DEFINER */
/*!50001 VIEW `joined_cookbook_version` AS select `v`.`major` AS `major`,`v`.`minor` AS `minor`,`v`.`patch` AS `patch`,concat_ws('.',`v`.`major`,`v`.`minor`,`v`.`patch`) AS `version`,`v`.`serialized_object` AS `serialized_object`,`v`.`id` AS `id`,`c`.`org_id` AS `org_id`,`c`.`name` AS `name` from (`cookbooks` `c` join `cookbook_versions` `v` on((`c`.`id` = `v`.`cookbook_id`))) */;
/*!50001 SET character_set_client      = @saved_cs_client */;
/*!50001 SET character_set_results     = @saved_cs_results */;
/*!50001 SET collation_connection      = @saved_col_connection */;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;


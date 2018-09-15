-- MySQL Script generated by MySQL Workbench
-- 09/21/14 13:15:46
-- Model: New Model    Version: 1.0
SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0;
SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0;
SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='TRADITIONAL,ALLOW_INVALID_DATES';

-- -----------------------------------------------------
-- Schema TestDB
-- -----------------------------------------------------
CREATE SCHEMA IF NOT EXISTS `TestDB` DEFAULT CHARACTER SET utf8 COLLATE utf8_general_ci ;
USE `TestDB` ;

-- -----------------------------------------------------
-- Table `TestDB`.`Person`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `TestDB`.`Person` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `name` VARCHAR(45) NULL,
  `country` VARCHAR(45) NULL,
  PRIMARY KEY (`id`))
ENGINE = InnoDB;


SET SQL_MODE=@OLD_SQL_MODE;
SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS;
SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS;

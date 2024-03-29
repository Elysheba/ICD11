-- MySQL Script generated by MySQL Workbench
-- Mon Jul 22 10:24:30 2019
-- Model: New Model    Version: 1.0
-- MySQL Workbench Forward Engineering

SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0;
SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0;
SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='ONLY_FULL_GROUP_BY,STRICT_TRANS_TABLES,NO_ZERO_IN_DATE,NO_ZERO_DATE,ERROR_FOR_DIVISION_BY_ZERO,NO_ENGINE_SUBSTITUTION';

-- -----------------------------------------------------
-- Schema EFO
-- -----------------------------------------------------
-- Table storing multiple disease ontology ids for Monarch Initiative

-- -----------------------------------------------------
-- Schema EFO
--
-- Table storing multiple disease ontology ids for Monarch Initiative
-- -----------------------------------------------------
CREATE SCHEMA IF NOT EXISTS `EFO` DEFAULT CHARACTER SET utf8 ;
USE `EFO` ;

-- -----------------------------------------------------
-- Table `EFO`.`ICD10_entryId`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `EFO`.`ICD10_entryId` (
  `DB` VARCHAR(45) NOT NULL COMMENT 'Name original database/ontology',
  `id` VARCHAR(45) NOT NULL COMMENT 'Disease ontology identifier from EFO',
  `def` VARCHAR(45) NOT NULL,
  `level` INT NOT NULL,
  PRIMARY KEY (`DB`, `id`))
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `EFO`.`ICD10_parentId`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `EFO`.`ICD10_parentId` (
  `DB` VARCHAR(45) NOT NULL COMMENT 'Database for id',
  `id` VARCHAR(45) NOT NULL COMMENT 'Disease ontology identifier from EFO',
  `pDB` VARCHAR(45) NOT NULL COMMENT 'Name database for parent id',
  `parent` VARCHAR(45) NOT NULL COMMENT 'Parent ontology for id in EFO',
  `origin` VARCHAR(45) NOT NULL,
  PRIMARY KEY (`DB`, `id`),
  INDEX `DB, id_idx` (`pDB` ASC, `parent` ASC) VISIBLE,
  CONSTRAINT `fk_table1_entryId1`
    FOREIGN KEY (`DB` , `id`)
    REFERENCES `EFO`.`ICD10_entryId` (`DB` , `id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `DB, id`
    FOREIGN KEY (`pDB` , `parent`)
    REFERENCES `EFO`.`ICD10_entryId` (`DB` , `id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `EFO`.`ICD10_idNames`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `EFO`.`ICD10_idNames` (
  `DB` VARCHAR(45) NOT NULL COMMENT 'Name original database',
  `id` VARCHAR(45) NOT NULL COMMENT 'Disease ontology identifier from EFO',
  `syn` VARCHAR(45) NOT NULL COMMENT 'Term (synonym or label) to describe the disease',
  `canonical` TINYINT NOT NULL COMMENT 'Current label for the entry',
  PRIMARY KEY (`DB`, `id`),
  CONSTRAINT `fk_table1_entryId2`
    FOREIGN KEY (`DB` , `id`)
    REFERENCES `EFO`.`ICD10_entryId` (`DB` , `id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `EFO`.`ICD10_sourceFiles`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `EFO`.`ICD10_sourceFiles` (
  `url` VARCHAR(100) NOT NULL,
  `current` VARCHAR(45) NOT NULL,
  PRIMARY KEY (`url`))
ENGINE = InnoDB;


SET SQL_MODE=@OLD_SQL_MODE;
SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS;
SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS;

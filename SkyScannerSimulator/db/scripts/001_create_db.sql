CREATE DATABASE [SimulatorService]

GO

USE [SimulatorService]

GO

CREATE TABLE [Plane] (
    [Name] NVARCHAR(100) NOT NULL
)

GO

INSERT INTO [Plane] ([Name]) VALUES ('Airbus'), ('Boeing'), ('JamboJet'), ('Jet')

GO
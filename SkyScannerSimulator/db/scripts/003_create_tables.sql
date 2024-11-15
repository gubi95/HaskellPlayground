USE [SimulatorService]

CREATE TABLE [SimulatorService].[Plane] (
    [Id] INT NOT NULL IDENTITY PRIMARY KEY,
    [Model] NVARCHAR(100) NOT NULL,
    [Airlines] NVARCHAR(100) NULL,
    [DistancePerTick] INT NOT NULL
)

INSERT INTO [SimulatorService].[Plane] ([Model], [Airlines], [DistancePerTick]) VALUES 
('Boeing 777', 'WizzAir', 10), ('Airbus A320', 'WizzAir', 12), ('Airbus A380', 'Ryanair', 14)

CREATE TABLE [SimulatorService].[Flights] (
    [From] NVARCHAR(100) NOT NULL,
    [To] NVARCHAR(100) NOT NULL,
    [PlaneId] INT NOT NULL,

    CONSTRAINT FK_Plane_Flights FOREIGN KEY ([PlaneId]) REFERENCES SimulatorService.Plane([Id])
)

INSERT INTO [SimulatorService].[Flights] (
    [From], [To], [PlaneId]
)
VALUES ('New York', 'Los Angeles', (SELECT [Id] FROM [SimulatorService].[Plane] WHERE [Model] = 'Boeing 777'))
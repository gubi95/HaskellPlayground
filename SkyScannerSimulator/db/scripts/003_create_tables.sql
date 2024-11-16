USE [SimulatorService]

CREATE TABLE [SimulatorService].[Plane] (
    [Id] INT NOT NULL IDENTITY PRIMARY KEY,
    [Model] NVARCHAR(100) NOT NULL,
    [Airlines] NVARCHAR(100) NULL,
    [DistancePerTick] INT NOT NULL
)

INSERT INTO [SimulatorService].[Plane] ([Model], [Airlines], [DistancePerTick]) VALUES 
('Boeing 777', 'WizzAir', 10), ('Airbus A320', 'WizzAir', 12), ('Airbus A380', 'Ryanair', 14)

CREATE TABLE [SimulatorService].[Airport] (
    [Id] INT NOT NULL IDENTITY PRIMARY KEY,
    [Code] NVARCHAR(10) NOT NULL,
    [Lat] NVARCHAR(30) NOT NULL,
    [Lon] NVARCHAR(30) NOT NULL
)

INSERT INTO [SimulatorService].[Airport] ([Code], [Lat], [Lon])
VALUES ('JFK', '40_38_23', '73_46_44'), ('LAX', '33_56_33', '118_24_29')

CREATE TABLE [SimulatorService].[Flights] (
    [PlaneId] INT NOT NULL,
    [DepartureAirportId] INT NOT NULL,
    [ArrivalAirportId] INT NOT NULL,

    CONSTRAINT FK_Plane_Flights FOREIGN KEY ([PlaneId]) REFERENCES SimulatorService.Plane([Id]),
    CONSTRAINT FK_AirportDeparture_Flights FOREIGN KEY ([DepartureAirportId]) REFERENCES SimulatorService.Airport([Id]),
    CONSTRAINT FK_AirportArrival_Flights FOREIGN KEY ([ArrivalAirportId]) REFERENCES SimulatorService.Airport([Id])
)

INSERT INTO [SimulatorService].[Flights] ([PlaneId], [DepartureAirportId], [ArrivalAirportId])
VALUES (
    (SELECT [Id] FROM [SimulatorService].[Airport] WHERE [Code] = 'JFK'),
    (SELECT [Id] FROM [SimulatorService].[Airport] WHERE [Code] = 'LAX'),
    (SELECT [Id] FROM [SimulatorService].[Plane] WHERE [Model] = 'Boeing 777'))
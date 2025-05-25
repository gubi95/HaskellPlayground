USE [SimulatorService]

CREATE TABLE [SimulatorService].[Plane] (
    [Id] INT NOT NULL IDENTITY PRIMARY KEY,
    [Model] NVARCHAR(100) NOT NULL,
    [Airlines] NVARCHAR(100) NULL,
    [KilometersPerTick] INT NOT NULL
)

INSERT INTO [SimulatorService].[Plane] ([Model], [Airlines], [KilometersPerTick]) VALUES 
('Boeing 777', 'WizzAir', 10), ('Airbus A320', 'WizzAir', 10.0), ('Airbus A380', 'Ryanair', 12.0)

CREATE TABLE [SimulatorService].[Airport] (
    [Id] INT NOT NULL IDENTITY PRIMARY KEY,
    [Code] NVARCHAR(10) NOT NULL,
    [Lat] FLOAT(24) NOT NULL,
    [Lon] FLOAT(24) NOT NULL
)

INSERT INTO [SimulatorService].[Airport] ([Code], [Lat], [Lon])
VALUES 
('JFK', 40.646149, -73.785964),
('LAX', 33.940325, -118.412331),
('WAW', 52.165833, 20.967222),
('NBO', -1.319167, 36.927778),
('SYD', -33.946111, 151.177222),
('PEK', 40.08, 116.584444)

CREATE TABLE [SimulatorService].[Flights] (
    [Id] INT NOT NULL IDENTITY PRIMARY KEY,
    [PlaneId] INT NOT NULL,
    [DepartureAirportId] INT NOT NULL,
    [ArrivalAirportId] INT NOT NULL,
    [Lat] FLOAT(24) NOT NULL,
    [Lon] FLOAT(24) NOT NULL,
    [Progress] FLOAT(10) NOT NULL,

    CONSTRAINT FK_Plane_Flights FOREIGN KEY ([PlaneId]) REFERENCES SimulatorService.Plane([Id]),
    CONSTRAINT FK_AirportDeparture_Flights FOREIGN KEY ([DepartureAirportId]) REFERENCES SimulatorService.Airport([Id]),
    CONSTRAINT FK_AirportArrival_Flights FOREIGN KEY ([ArrivalAirportId]) REFERENCES SimulatorService.Airport([Id])
)

INSERT INTO [SimulatorService].[Flights] ([PlaneId], [DepartureAirportId], [ArrivalAirportId], [Lat], [Lon], [Progress])
VALUES (
    (SELECT [Id] FROM [SimulatorService].[Plane] WHERE [Model] = 'Boeing 777'),
    (SELECT [Id] FROM [SimulatorService].[Airport] WHERE [Code] = 'LAX'),
    (SELECT [Id] FROM [SimulatorService].[Airport] WHERE [Code] = 'JFK'),    
    (SELECT [Lat] FROM [SimulatorService].[Airport] WHERE [Code] = 'LAX'),
    (SELECT [Lon] FROM [SimulatorService].[Airport] WHERE [Code] = 'LAX'),
    0.0);

INSERT INTO [SimulatorService].[Flights] ([PlaneId], [DepartureAirportId], [ArrivalAirportId], [Lat], [Lon], [Progress])
VALUES (
    (SELECT [Id] FROM [SimulatorService].[Plane] WHERE [Model] = 'Airbus A320'),
    (SELECT [Id] FROM [SimulatorService].[Airport] WHERE [Code] = 'WAW'),
    (SELECT [Id] FROM [SimulatorService].[Airport] WHERE [Code] = 'NBO'),    
    (SELECT [Lat] FROM [SimulatorService].[Airport] WHERE [Code] = 'WAW'),
    (SELECT [Lon] FROM [SimulatorService].[Airport] WHERE [Code] = 'WAW'),
    0.0);

INSERT INTO [SimulatorService].[Flights] ([PlaneId], [DepartureAirportId], [ArrivalAirportId], [Lat], [Lon], [Progress])
VALUES (
    (SELECT [Id] FROM [SimulatorService].[Plane] WHERE [Model] = 'Airbus A380'),
    (SELECT [Id] FROM [SimulatorService].[Airport] WHERE [Code] = 'SYD'),
    (SELECT [Id] FROM [SimulatorService].[Airport] WHERE [Code] = 'PEK'),    
    (SELECT [Lat] FROM [SimulatorService].[Airport] WHERE [Code] = 'SYD'),
    (SELECT [Lon] FROM [SimulatorService].[Airport] WHERE [Code] = 'SYD'),
    0.0);
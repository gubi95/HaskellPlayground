\connect simulatorservice;

CREATE TABLE simulatorservice.PLANE (
    Id SERIAL PRIMARY KEY,
    Model VARCHAR(100) NOT NULL,
    Airlines VARCHAR(100) NULL,
    KilometersPerTick INT NOT NULL
);

INSERT INTO SimulatorService.Plane (Model, Airlines, KilometersPerTick) VALUES 
('Boeing 777', 'WizzAir', 10), ('Airbus A320', 'WizzAir', 10.0), ('Airbus A380', 'Ryanair', 12.0);

CREATE TABLE SimulatorService.Airport (
    Id SERIAL PRIMARY KEY,
    Code VARCHAR(10) NOT NULL,
    Lat FLOAT(24) NOT NULL,
    Lon FLOAT(24) NOT NULL
);

INSERT INTO SimulatorService.Airport (Code, Lat, Lon)
VALUES 
('JFK', 40.64614, -73.78596),
('LAX', 33.94032, -118.41233),
('WAW', 52.16583, 20.96722),
('NBO', -1.31916, 36.92777),
('SYD', -33.94611, 151.17722),
('PEK', 40.08000, 116.58444);

CREATE TABLE SimulatorService.Flights (
    Id int GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    PlaneId INT NOT NULL,
    DepartureAirportId INT NOT NULL,
    ArrivalAirportId INT NOT NULL,
    Lat FLOAT(24) NOT NULL,
    Lon FLOAT(24) NOT NULL,
    Progress FLOAT(10) NOT NULL,

    CONSTRAINT FK_Plane_Flights FOREIGN KEY (PlaneId) REFERENCES SimulatorService.Plane(Id),
    CONSTRAINT FK_AirportDeparture_Flights FOREIGN KEY (DepartureAirportId) REFERENCES SimulatorService.Airport(Id),
    CONSTRAINT FK_AirportArrival_Flights FOREIGN KEY (ArrivalAirportId) REFERENCES SimulatorService.Airport(Id)
);

INSERT INTO SimulatorService.Flights (PlaneId, DepartureAirportId, ArrivalAirportId, Lat, Lon, Progress)
VALUES (
    (SELECT Id FROM SimulatorService.Plane WHERE Model = 'Boeing 777'),
    (SELECT Id FROM SimulatorService.Airport WHERE Code = 'LAX'),
    (SELECT Id FROM SimulatorService.Airport WHERE Code = 'JFK'),    
    (SELECT Lat FROM SimulatorService.Airport WHERE Code = 'LAX'),
    (SELECT Lon FROM SimulatorService.Airport WHERE Code = 'LAX'),
    0.0);

INSERT INTO SimulatorService.Flights (PlaneId, DepartureAirportId, ArrivalAirportId, Lat, Lon, Progress)
VALUES (
    (SELECT Id FROM SimulatorService.Plane WHERE Model = 'Airbus A320'),
    (SELECT Id FROM SimulatorService.Airport WHERE Code = 'WAW'),
    (SELECT Id FROM SimulatorService.Airport WHERE Code = 'NBO'),    
    (SELECT Lat FROM SimulatorService.Airport WHERE Code = 'WAW'),
    (SELECT Lon FROM SimulatorService.Airport WHERE Code = 'WAW'),
    0.0);

INSERT INTO SimulatorService.Flights (PlaneId, DepartureAirportId, ArrivalAirportId, Lat, Lon, Progress)
VALUES (
    (SELECT Id FROM SimulatorService.Plane WHERE Model = 'Airbus A380'),
    (SELECT Id FROM SimulatorService.Airport WHERE Code = 'SYD'),
    (SELECT Id FROM SimulatorService.Airport WHERE Code = 'PEK'),
    (SELECT Lat FROM SimulatorService.Airport WHERE Code = 'SYD'),
    (SELECT Lon FROM SimulatorService.Airport WHERE Code = 'SYD'),
    0.0);

GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA simulatorservice TO appuser;
GRANT USAGE ON SCHEMA simulatorservice TO appuser;
import React, { useEffect, useState } from 'react';
import './App.css';
import { ComposableMap, Geographies, Geography, Line, Marker } from 'react-simple-maps';
import axios from 'axios';

function App() {
  const calculatePlaneRotation = (lat1: number, lon1: number, lat2: number, lon2: number) => {
    const degreesPlaneImageOffset = -45;
    const degreesToRads = (deg: number) => (deg * Math.PI) / 180.0;
    const radsToDegrees = (rad: number) => (rad * 180.0) / Math.PI;

    const dLon = degreesToRads(lon2 - lon1);

    const y = Math.sin(dLon) * Math.cos(degreesToRads(lat2));
    const x = Math.cos(degreesToRads(lat1)) * Math.sin(degreesToRads(lat2)) - Math.sin(degreesToRads(lat1))
      * Math.cos(degreesToRads(lat2)) * Math.cos(dLon);

    let brng = Math.atan2(y, x);

    brng = radsToDegrees(brng);
    brng = (brng + 360) % 360;

    return brng + degreesPlaneImageOffset;
  }

  const [flights, setFlights] = useState<Flight[]>([]);
  let isUpdatingFlights = false;

  type Flight = {
    id: number;
    fromLat: number,
    fromLon: number,
    toLat: number,
    toLon: number,
    currentLat: number,
    currentLon: number
  }

  useEffect(() => {
    const interval = setInterval(async () => {
      try {
        const response = await axios.get<Flight[]>("http://localhost:3000/flights");

        if (!isUpdatingFlights) {
          isUpdatingFlights = true;
          await Promise.all(
            response.data.map(async (flight) => {
              await axios.post(`http://localhost:3000/flights/${flight.id}`);
            })
          );
          isUpdatingFlights = false;
        }

        setFlights(response.data);
      } catch (err) {
        console.error(err);
      }
    }, 1000);

    return () => clearInterval(interval);
  }, []);

  return (
    <div className="App">
      <div className="MapWrapper">
        <ComposableMap projectionConfig={{
          scale: 150
        }}>
          <Geographies geography="/features.json">
            {({ geographies }) =>
              geographies.map((geo) => (
                <Geography key={geo.rsmKey} geography={geo} fill="#FF5533" stroke="#000000" />
              ))
            }
          </Geographies>
          {flights.map((flight) =>
            <>
              <Line
                from={[flight.fromLon, flight.fromLat]}
                to={[flight.toLon, flight.toLat]}
                stroke="green"
                strokeWidth={2}
                strokeLinecap="square" />
              <Marker coordinates={[flight.currentLon, flight.currentLat]}>
                <svg x="-6" y="-6" fill="yellow" width="12px" height="12px" viewBox="0 0 23.5 23.5">
                  <g stroke-width="0"></g>
                  <g stroke-linecap="round" stroke-linejoin="round"></g>
                  <g>
                    <g>
                      <path transform={"rotate(" + calculatePlaneRotation(flight.currentLat, flight.currentLon, flight.toLat, flight.toLon) + ", 12 12)"}
                        d="M22.925,2.564l-3.193,3.191c0.104,0.75-0.123,1.536-0.698,2.112L16.75,10.15l2.715,11.664 c0.041,0.072,0.062,0.155,0.062,0.244c0,0.277-0.269,0.486-0.509,0.5c-0.004,0-0.008,0-0.012,0c-0.13,0-0.257-0.05-0.354-0.146 l-7.083-7.084l-1.804,1.807l1.275,5.482c0.039,0.071,0.062,0.154,0.062,0.243c0,0.279-0.225,0.522-0.51,0.5 c-0.004,0-0.006,0-0.01,0c-0.129,0-0.258-0.05-0.354-0.146L6.283,19.27c-0.021-0.002-0.042-0.003-0.062-0.006l-0.592,0.592 c-0.293,0.293-0.678,0.439-1.062,0.439c-0.384,0-0.769-0.146-1.062-0.439c-0.586-0.586-0.586-1.535,0-2.121l0.594-0.593 c-0.004-0.021-0.004-0.041-0.006-0.062l-3.946-3.945c-0.158-0.158-0.192-0.401-0.084-0.598c0.108-0.195,0.327-0.297,0.551-0.244 L6.228,13.6l1.805-1.807L0.952,4.712c-0.157-0.157-0.192-0.4-0.084-0.598C0.976,3.919,1.197,3.818,1.419,3.87l11.794,2.744 l2.281-2.283c0.578-0.578,1.364-0.805,2.115-0.698l3.19-3.191c0.586-0.586,1.534-0.586,2.12,0 C23.507,1.027,23.511,1.979,22.925,2.564z"></path>
                    </g>
                  </g>
                </svg>
              </Marker><Marker coordinates={[flight.toLon, flight.toLat]}>
                <circle r={3} fill="yellow" />
              </Marker>
            </>
          )}
        </ComposableMap>
      </div>
    </div>
  );
}

export default App;

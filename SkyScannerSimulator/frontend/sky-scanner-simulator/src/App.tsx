import React from 'react';
import './App.css';
import { ComposableMap, Geographies, Geography } from 'react-simple-maps';

function App() {
  return (
    <div className="App">
      <div className="MapWrapper">
        <ComposableMap >
          <Geographies geography="/features.json">
            {({ geographies }) =>
              geographies.map((geo) => (
                <Geography key={geo.rsmKey} geography={geo} fill="#FF5533" stroke="#000000" />
              ))
            }
          </Geographies>
        </ComposableMap>
      </div>
    </div>
  );
}

export default App;

#lang racket

(require "../generate-sensor-names.rkt"
         "../mysql-socket.rkt"
         db)

(define conn
  (virtual-connection
   (connection-pool
    (lambda ()
      (mysql-connect #:user "clements"
                     #:database "sodec2"
                     #:password "aoeuidht"
                     #:socket mysql-socket
                     )))))

(define description-text
#<<|
- `s-temp-out` (Outside Temperature) : the temperature in the outside north wall
- `s-temp-bed` (Bedroom Temperature) : the temperature in the bedroom
- `s-temp-bath` (Bathroom Temperature) : the temperature in the bathroom
- `s-temp-lr` (Living Room Temperature) : the temperature in the living room
- `s-temp-kit` (Kitchen Temperature) : the temperature in the kitchen
- `s-hum-out` (Outside Humidity) : the humidity in the outside north wall
- `s-hum-bed` (Bedroom Humidity) : the humidity in the bedroom
- `s-hum-bath` (Bathroom Humidity) : the humidity in the bathroom
- `s-hum-lr` (Living Room Humidity) : the humidity in the living room
- `s-hum-kit` (Kitchen Humidity) : the humidity in the kitchen
- `s-occ-bed` (Bedroom Occupancy) : whether the bedroom is occupied
- `s-occ-mech` (Mechanical Room Occupancy) : whether the mechanical room is occupied
- `s-occ-lr` (Living Room Occupancy) : whether the living room is occupied
- `s-occ-bath` (Bathroom Occupancy) : whether the bathroom is occupied
- `s-amb-bed` (Bedroom Ambient Light) : the ambient light level in the bedroom
- `s-amb-mech` (Mechanical Room Ambient Light) : the ambient light level in the mechanical room
- `s-amb-lr` (Living Room Ambient Light) : the ambient light level in the living room
- `s-amb-bath` (Bathroom Ambient Light) : the ambient light level in the bathroom
- `c-light-bed` (Bedroom Light Control) : control of the lights in the bedroom
- `c-light-mech` (Mechanical Room Light Control) : control of the lights in the mechanical room
- `c-light-lr` (Living Room Light Control) : control of the lights in the living room
- `c-light-bath` (Bathroom Light Control) : control of the lights in the bathroom
- `c-light-kit` (Kitchen Light Control) : control of the lights in the kitchen
- `s-elec-used-laundry` : energy consumption for dryer/washer circuit
- `s-elec-used-dishwasher` : energy consumption for dishwasher circuit
- `s-elec-used-refrigerator` : energy consumption for refrigerator circuit
- `s-elec-used-induction-stove` : energy consumption for induction stove circuit
- `s-elec-used-ewh-solar-water-heater` : energy consumption for ewh solar water heater circuit
- `s-elec-used-kitchen-receps-1` : energy consumption for kitchen receps 1 circuit
- `s-elec-used-kitchen-receps-2` : energy consumption for kitchen receps 2 circuit
- `s-elec-used-living-receps` : energy consumption for living receps circuit
- `s-elec-used-dining-receps-1` : energy consumption for dining receps 1 circuit
- `s-elec-used-dining-receps-2` : energy consumption for dining receps 2 circuit
- `s-elec-used-bathroom-receps` : energy consumption for bathroom receps circuit
- `s-elec-used-bedroom-receps-1` : energy consumption for bedroom receps 1 circuit
- `s-elec-used-bedroom-receps-2` : energy consumption for bedroom receps 2 circuit
- `s-elec-used-mechanical-receps` : energy consumption for mechanical receps circuit
- `s-elec-used-entry-receps` : energy consumption for entry receps circuit
- `s-elec-used-exterior-receps` : energy consumption for exterior recep circuit
- `s-elec-used-grey-water-pump-recep` : energy consumption for grey water pump recep circuit
- `s-elec-used-black-water-pump-recep` : energy consumption for black water pump recep circuit
- `s-elec-used-thermal-loop-pump-recep` : energy consumption for thermal loop pump recep circuit
- `s-elec-used-water-supply-pump-recep` : energy consumption for water supply pump recep circuit
- `s-elec-used-water-supply-booster-pump-recep` : energy consumption for water supply booster pump recep circuit
- `s-elec-used-vehicle-charging-recep` : energy consumption for vehicle charging recep circuit
- `s-elec-used-heat-pump-recep` : energy consumption for heat pump recep circuit
- `s-elec-used-air-handler-recep` : energy consumption for air handler recep circuit
- `s-temp-testing-blackhole` (Blackhole Temperature) : a temperature bin to test recording
- `s-temp-testing-empty` (Empty Temperature) : a temperature bin that's always empty
- `s-light-entry-bookend-1A` (Entry Bookend Light)
- `s-light-chandelier-1B` (Chandelier Light)
- `s-light-tv-light-2A` (TV Light)
- `s-light-kitchen-uplight-3A` (Kitchen Uplight Light)
- `s-light-under-counter-3B` (Kitchen Under-Counter Light)
- `s-light-pendant-bar-lights-3C` (Kitchen Pendant Bar Lights)
- `s-light-bathroom-ambient-4A` (Bathroom Light)
- `s-light-mirror-4B` (Bathroom Mirror Light)
- `s-light-flexspace-uplight-5A` (Flexspace Uplight Light)
- `s-light-flexspace-cabinet-5B` (Flexspace Cabinet Light)
- `s-light-bedroom-uplight-6A` (Bedroom Uplight Light)
- `s-light-bedroom-cabinet-6B` (Bedroom Cabinet Light)
- `s-light-porch-lights-8A` (Porch Lights)
- `s-light-uplights-and-pot-lights-8B` (Uplights/Pot Lights)
|
)

(for/list ([l (regexp-split #px"\n" description-text)])
  l)

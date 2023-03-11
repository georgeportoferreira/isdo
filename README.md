# ISDO
Individualized speed of deforestation occurrences

Data used in this code came from the folowing sources:

## Glad Alerts

https://storage.googleapis.com/earthenginepartnershansen/GLADalert/C2/current/ \[**key**\]

using the flowing \[**key**\]:
alertDateYY_DD1H_D1H_DD2H_D2H.tif

where,
YY is the year (from 21 to 23); 
DD1 is three numeric digits representing the left limit longitude (from 000 to 180);
D1 is two numeric digits representing the bottom limit latitude (from 00 to 90);
DD2 is three numeric digits representing the right limit longitude (from 000 to 180);
D2 is two numeric digits representing the upper limit latitude (from 00 to 90);
H is a one-character letter representing de hemisphere (N/S/E/W). N or S for latitude, E or W for longitude.

http://glad-forest-alert.appspot.com/

## Natural Earth 

Grids 10x10 degrees
https://naciscdn.org/naturalearth/110m/physical/ne_110m_graticules_10.zip

Countries Boundaries
https://raw.githubusercontent.com/nvkelso/natural-earth-vector/master/geojson/ne_110m_admin_0_countries.geojson

https://www.naturalearthdata.com/

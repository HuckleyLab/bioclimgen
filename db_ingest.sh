psql -d climate -f /Users/tony/Dropbox/Projects/UW/bioclimgen/db_ingest.sql

for file in *.csv
	psql -d climate -c "\copy bioclim_geom(index, lat, lon, year, bio1, bio2, bio3, bio4, bio5, bio6, bio7, bio8, bio9, bio10, bio11, bio12, bio13, bio14, bio15, bio16, bio17, bio18, bio19) from '$file' delimiter ',' csv header"
end


psql -d climate -c "update bioclim_geom set location = ST_GeomFromText('POINT(' || lon || ' ' || lat || ')', 4326);"

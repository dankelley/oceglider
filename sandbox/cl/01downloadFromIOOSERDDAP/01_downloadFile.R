# download a file from the glider IOOS website.
# I had a look through the list of missions and found the most recent
#   delayed mode dataset

# long url, there's a lot of variables !
# but if you look at the very end of it, you'll see that I specified only downloading
#   about 10 days of data. The date ranges for many of the time variables was roughly 2015-11 to 2016-10
url <- 'https://gliders.ioos.us/erddap/tabledap/ga_538-20151124T1730-delayed.nc?trajectory%2Cwmo_id%2Cprofile_id%2Ctime%2Clatitude%2Clongitude%2Cdepth%2Cbackscatter%2Cchlorophyll%2Cconductivity%2Cconductivity_qc%2Ccrs%2Cctd_timestamp%2Cdensity%2Cdensity_qc%2Cdepth_qc%2Cdissolved_oxygen%2Cinstrument_ctd%2Cinstrument_flbb%2Cinstrument_oxygen%2Clat_uv%2Clat_uv_qc%2Clatitude_qc%2Clon_uv%2Clon_uv_qc%2Clongitude_qc%2Coxygen_saturation%2Cpitch%2Cplatform_meta%2Cprecise_lat%2Cprecise_lat_qc%2Cprecise_lon%2Cprecise_lon_qc%2Cprecise_time%2Cprecise_time_qc%2Cpressure%2Cpressure_qc%2Cqartod_conductivity_flat_line_flag%2Cqartod_conductivity_gross_range_flag%2Cqartod_conductivity_primary_flag%2Cqartod_conductivity_rate_of_change_flag%2Cqartod_conductivity_spike_flag%2Cqartod_density_flat_line_flag%2Cqartod_density_gross_range_flag%2Cqartod_density_primary_flag%2Cqartod_density_rate_of_change_flag%2Cqartod_density_spike_flag%2Cqartod_monotonic_pressure_flag%2Cqartod_pressure_flat_line_flag%2Cqartod_pressure_gross_range_flag%2Cqartod_pressure_primary_flag%2Cqartod_pressure_rate_of_change_flag%2Cqartod_pressure_spike_flag%2Cqartod_salinity_flat_line_flag%2Cqartod_salinity_gross_range_flag%2Cqartod_salinity_primary_flag%2Cqartod_salinity_rate_of_change_flag%2Cqartod_salinity_spike_flag%2Cqartod_temperature_flat_line_flag%2Cqartod_temperature_gross_range_flag%2Cqartod_temperature_primary_flag%2Cqartod_temperature_rate_of_change_flag%2Cqartod_temperature_spike_flag%2Cradiation_wavelength%2Croll%2Csalinity%2Csalinity_qc%2Csource_file%2Ctemperature%2Ctemperature_qc%2Ctime_qc%2Ctime_uv%2Ctime_uv_qc%2Cu%2Cu_qc%2Cv%2Cv_qc&precise_time%3E=2016-08-01T00%3A00%3A00&precise_time%3C=2016-08-02T00%3A00%3A00'
# we'll download the data to the working directory
destdir <- './sandbox/cl/01downloadFromIOOSERDDAP/'
destfile <- paste(destdir, 'ga_538-20151124T1730-delayed.nc', sep = '/') # usually I'd auto create this, but for this example it's fine
download.file(url = url,
              destfile = destfile,
              mode = 'wb')

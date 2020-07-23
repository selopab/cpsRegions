import delimited D:\Dropbox\Dropbox\Documentos_\Joyce\regionesFebrero\out\tablaPJoyce.csv, varn(1) clear

bysort cp: gen renglon = _n

foreach var in domicilios domicilios_group expedientes{
replace `var' = 0 if renglon>1
}

sort región cp

export excel D:\Dropbox\Dropbox\Documentos_\Joyce\regionesFebrero\out\tablaPJoyce.xlsx, first(var)

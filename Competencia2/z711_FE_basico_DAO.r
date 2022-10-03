#Necesita para correr en Google Cloud
# 32 GB de memoria RAM
#256 GB de espacio en el disco local
#8 vCPU


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")


options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})



setwd( "~/buckets/b1/" )

#cargo el dataset
dataset  <- fread( "./datasets/competencia2_2022.csv.gz")

#creo la carpeta donde va el experimento
# FE  representa  Feature Engineering
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/FE7110/", showWarnings = FALSE )
setwd("./exp/FE7110/")   #Establezco el Working Directory DEL EXPERIMENTO

#INICIO de la seccion donde se deben hacer cambios con variables nuevas

#creo un ctr_quarter que tenga en cuenta cuando los clientes hace 3 menos meses que estan
dataset[  , ctrx_quarter_normalizado := ctrx_quarter ]
dataset[ cliente_antiguedad==1 , ctrx_quarter_normalizado := ctrx_quarter * 5 ]
dataset[ cliente_antiguedad==2 , ctrx_quarter_normalizado := ctrx_quarter * 2 ]
dataset[ cliente_antiguedad==3 , ctrx_quarter_normalizado := ctrx_quarter * 1.2 ]

#variable extraida de una tesis de maestria de Irlanda
dataset[  , mpayroll_sobre_edad  := mpayroll / cliente_edad ]

#se crean los nuevos campos para MasterCard  y Visa, teniendo en cuenta los NA's
#varias formas de combinar Visa_status y Master_status
dataset[ , mv_status01       := pmax( Master_status,  Visa_status, na.rm = TRUE) ]
dataset[ , mv_status02       := Master_status +  Visa_status ]
dataset[ , mv_status03       := pmax( ifelse( is.na(Master_status), 10, Master_status) , ifelse( is.na(Visa_status), 10, Visa_status) ) ]
dataset[ , mv_status04       := ifelse( is.na(Master_status), 10, Master_status)  +  ifelse( is.na(Visa_status), 10, Visa_status)  ]
dataset[ , mv_status05       := ifelse( is.na(Master_status), 10, Master_status)  +  100*ifelse( is.na(Visa_status), 10, Visa_status)  ]

dataset[ , mv_status06       := ifelse( is.na(Visa_status), 
                                        ifelse( is.na(Master_status), 10, Master_status), 
                                        Visa_status)  ]

dataset[ , mv_status07       := ifelse( is.na(Master_status), 
                                        ifelse( is.na(Visa_status), 10, Visa_status), 
                                        Master_status)  ]


#combino MasterCard y Visa
dataset[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]

dataset[ , mv_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
dataset[ , mv_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
dataset[ , mv_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
dataset[ , mv_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
dataset[ , mv_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
dataset[ , mv_mconsumospesos       := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
dataset[ , mv_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
dataset[ , mv_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
dataset[ , mv_madelantopesos       := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
dataset[ , mv_madelantodolares     := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
dataset[ , mv_fultimo_cierre       := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
dataset[ , mv_mpagado              := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
dataset[ , mv_mpagospesos          := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
dataset[ , mv_mpagosdolares        := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
dataset[ , mv_fechaalta            := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
dataset[ , mv_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
dataset[ , mv_cconsumos            := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ]
dataset[ , mv_cadelantosefectivo   := rowSums( cbind( Master_cadelantosefectivo,  Visa_cadelantosefectivo) , na.rm=TRUE ) ]
dataset[ , mv_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]

#a partir de aqui juego con la suma de Mastercard y Visa
dataset[ , mvr_Master_mlimitecompra:= Master_mlimitecompra / mv_mlimitecompra ]
dataset[ , mvr_Visa_mlimitecompra  := Visa_mlimitecompra / mv_mlimitecompra ]
dataset[ , mvr_msaldototal         := mv_msaldototal / mv_mlimitecompra ]
dataset[ , mvr_msaldopesos         := mv_msaldopesos / mv_mlimitecompra ]
dataset[ , mvr_msaldopesos2        := mv_msaldopesos / mv_msaldototal ]
dataset[ , mvr_msaldodolares       := mv_msaldodolares / mv_mlimitecompra ]
dataset[ , mvr_msaldodolares2      := mv_msaldodolares / mv_msaldototal ]
dataset[ , mvr_mconsumospesos      := mv_mconsumospesos / mv_mlimitecompra ]
dataset[ , mvr_mconsumosdolares    := mv_mconsumosdolares / mv_mlimitecompra ]
dataset[ , mvr_madelantopesos      := mv_madelantopesos / mv_mlimitecompra ]
dataset[ , mvr_madelantodolares    := mv_madelantodolares / mv_mlimitecompra ]
dataset[ , mvr_mpagado             := mv_mpagado / mv_mlimitecompra ]
dataset[ , mvr_mpagospesos         := mv_mpagospesos / mv_mlimitecompra ]
dataset[ , mvr_mpagosdolares       := mv_mpagosdolares / mv_mlimitecompra ]
dataset[ , mvr_mconsumototal       := mv_mconsumototal  / mv_mlimitecompra ]
dataset[ , mvr_mpagominimo         := mv_mpagominimo  / mv_mlimitecompra ]

#Aqui debe usted agregar sus propias nuevas variables


dataset[ , propia_mcuenta :=  mcuentas_saldo+mcuenta_corriente ]
dataset[ , propia_mcomisiones :=  mcomisiones+mcomisiones_mantenimiento+mcomisiones_otras ]
dataset[ , propia_ccomisiones :=  ccomisiones_otras+ccomisiones_mantenimiento ]
dataset[ , propia_limite_tarjeta :=  Master_mfinanciacion_limite+Visa_mfinanciacion_limite ]
# dataset[ , propia_msaldototal :=  Master_msaldototal+Visa_msaldototal ]
# dataset[ , propia_msaldopesos :=  Master_msaldopesos+Visa_msaldopesos ]
# dataset[ , propia_msaldodolares :=  Master_msaldodolares+Visa_msaldodolares ]
# dataset[ , propia_mconsumosdolares :=  Master_mconsumosdolares+Visa_mconsumosdolares ]
# dataset[ , propia_msaldototal :=  Master_msaldototal+Visa_msaldototal ]
# dataset[ , propia_mconsumototal :=  Master_mconsumototal+Visa_mconsumototal ]
# dataset[ , propia_mconsumospesos :=  Master_mconsumospesos+Visa_mconsumospesos ]
# dataset[ , propia_mpagospesos :=  Master_mpagospesos+Visa_mpagospesos ]
# dataset[ , propia_ctarjeta :=  ctarjeta_master+ctarjeta_visa ]
# dataset[ , propia_cconsumos :=  Master_cconsumos+Visa_cconsumos ]
# dataset[ , propia_debitos_automaticos :=  mcuenta_debitos_automaticos+mttarjeta_visa_debitos_automaticos+mttarjeta_master_debitos_automaticos ]
dataset[ , propia_margen :=  mactivos_margen+mpasivos_margen ]
dataset[ , propia_mov :=  ctrx_quarter+active_quarter ]
dataset[ , propia_prestamos_personales :=  ifelse( cprestamos_personales==0, 0, mprestamos_personales/cprestamos_personales ) ]
dataset[ , propia_prestamos_prendarios :=  ifelse( cprestamos_prendarios==0, 0, mprestamos_personales/cprestamos_prendarios )]
dataset[ , propia_prestamos_hipotecarios :=  ifelse( cprestamos_hipotecarios==0, 0, mprestamos_personales/cprestamos_hipotecarios )]
dataset[ , propia_mrentabilidad :=  mrentabilidad/cliente_antiguedad ]
dataset[ , propia_mrentabilidad_annual :=  mrentabilidad_annual/cliente_antiguedad ]
dataset[ , propia_antiguedad :=  cliente_edad/cliente_antiguedad ]
dataset[ , propia_saldo_tarjetas :=  Master_msaldototal+Visa_msaldototal ]
dataset[ , propia_limite_tarjetas_edad :=  (Master_mlimitecompra+Visa_mlimitecompra)/cliente_edad ]
dataset[ , propia_limite_tarjetas_ant :=  (Master_mlimitecompra+Visa_mlimitecompra)/cliente_antiguedad ]
dataset[ , propia_rank_mcuentas_saldo :=frank(-mcuentas_saldo,ties.method = "dense")]
dataset[ , propia_rank_mcuenta_corriente :=frank(-mcuenta_corriente,ties.method = "dense")]
dataset[ , propia_rank_mcaja_ahorro :=frank(-mcaja_ahorro,ties.method = "dense")]
dataset[ , propia_rank_mcaja_ahorro_dolares :=frank(-mcaja_ahorro_dolares,ties.method = "dense")]

dataset[,caja_ahorro_total := mcaja_ahorro + mcaja_ahorro_dolares + mcaja_ahorro_adicional + mcaja_ahorro_adicional]
dataset[,saldo_total := mcuenta_corriente + mcaja_ahorro + mcaja_ahorro_dolares + mcaja_ahorro_adicional + mcaja_ahorro_adicional]
dataset[,inversiones_total := mplazo_fijo_dolares+mplazo_fijo_pesos + minversion1_pesos + minversion1_dolares + minversion2]
dataset[mcuenta_corriente<0,descubierto:= -1*mcuenta_corriente ]
dataset[,prestamos := mprestamos_personales+mprestamos_prendarios+mprestamos_hipotecarios]
dataset[,consumo_tarjeta_total:= mtarjeta_visa_consumo+mtarjeta_master_consumo]
dataset[,financiamiento_corto:= descubierto + consumo_tarjeta_total]
dataset[,financiamiento_total := financiamiento_corto + prestamos]

dataset[,inversiones_saldo_total:= inversiones_total/saldo_total]
dataset[,inversiones_caja_ahorro_total:= inversiones_total/caja_ahorro_total]
dataset[,prestamos_saldo_total := prestamos / saldo_total ]
dataset[,prestamos_caja_ahorro := prestamos / caja_ahorro_total] 
dataset[,financiamiento_corto_saldo_total := financiamiento_corto / saldo_total ]
dataset[,financiamiento_corto_caja_ahorro := financiamiento_corto / caja_ahorro_total] 
dataset[,financiamiento_corto_saldo_total := financiamiento_corto / saldo_total ]
dataset[,financiamiento_corto_caja_ahorro := financiamiento_corto / caja_ahorro_total] 
dataset[,financiamiento_total_saldo_total := financiamiento_total / saldo_total]
dataset[,financiamiento_total_ahorro := financiamiento_total / caja_ahorro_total]
dataset[,inversion_antiguedad:= inversiones_total /cliente_antiguedad]
dataset[,antiguedad_relativa := (cliente_antiguedad/12)/cliente_edad]
dataset[,seguros:= cseguro_vida+cseguro_auto+cseguro_vivienda+cseguro_accidentes_personales]


##data driffting

var.drifting.ranking <- c(
  "mpasivos_margen"
  ,"mcaja_ahorro_dolares"
  ,"mtarjeta_visa_consumo"
  ,"mcuenta_debitos_automaticos"
  ,"chomebanking_transacciones"
  ,"ccajas_otras"
  ,"Visa_mconsumospesos"
  ,"Visa_mconsumototal"
  ,"mcuentas_saldo"
  ,"Visa_msaldototal"
)

var.drifting.random <- c(
  "mcaja_ahorro"
  ,"mcomisiones"
  ,"ccomisiones_otras"
  ,"mcomisiones_otras"
  ,"Visa_msaldopesos"
  ,"mrentabilidad"
  ,"mrentabilidad_annual"
  ,"mactivos_margen"
)


rank.prefix <- "r_"
mes.ini <- 202103
mes.fin <- 202105

for (var in variables.drifting.ranking) {
  new.var.name <- paste0(rank.prefix, var)
  dataset[foto_mes==mes.ini, (new.var.name) := (frankv(dataset[foto_mes==mes.ini], cols = var, na.last = TRUE, ties.method = "dense") - 1) / (.N - 1)]
  dataset[foto_mes==mes.fin, (new.var.name) := (frankv(dataset[foto_mes==mes.fin], cols = var, na.last = TRUE, ties.method = "dense") - 1) / (.N - 1)]
  dataset[, (var) := NULL]
}

for (var in variables.drifting.random) {
  new.var.name <- paste0(rank.prefix, var)
  dataset[foto_mes==mes.ini, (new.var.name) := (frankv(dataset[foto_mes==mes.ini], cols = var, na.last = TRUE, ties.method = "random") - 1) / (.N - 1)]
  dataset[foto_mes==mes.fin, (new.var.name) := (frankv(dataset[foto_mes==mes.fin], cols = var, na.last = TRUE, ties.method = "random") - 1) / (.N - 1)]
  dataset[, (var) := NULL]
}



#valvula de seguridad para evitar valores infinitos
#paso los infinitos a NULOS
infinitos      <- lapply(names(dataset),function(.name) dataset[ , sum(is.infinite(get(.name)))])
infinitos_qty  <- sum( unlist( infinitos) )
if( infinitos_qty > 0 )
{
  cat( "ATENCION, hay", infinitos_qty, "valores infinitos en tu dataset. Seran pasados a NA\n" )
  dataset[mapply(is.infinite, dataset)] <- NA
}


#valvula de seguridad para evitar valores NaN  que es 0/0
#paso los NaN a 0 , decision polemica si las hay
#se invita a asignar un valor razonable segun la semantica del campo creado
nans      <- lapply(names(dataset),function(.name) dataset[ , sum(is.nan(get(.name)))])
nans_qty  <- sum( unlist( nans) )
if( nans_qty > 0 )
{
  cat( "ATENCION, hay", nans_qty, "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n" )
  cat( "Si no te gusta la decision, modifica a gusto el programa!\n\n")
  dataset[mapply(is.nan, dataset)] <- 0
}




#--------------------------------------
#grabo el dataset
fwrite( dataset,
        "dataset_7110.csv.gz",
        logical01= TRUE,
        sep= "," )

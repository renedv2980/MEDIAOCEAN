*          DATA SET PPLNK13    AT LEVEL 041 AS OF 01/25/21                      
*PHASE T41413A                                                                  
PPLNK13  TITLE '- INSERTION UPLOAD MAPS'                                        
         PRINT NOGEN                                                            
INSUPL   CSECT                                                                  
         LKSVR TYPE=US,REQUEST=*,WORKERKEY=PPIU,SYSTEM=PRTSYSQ,        +        
               IBLOCK=*,IFROM=(PRI,LIN),ITO=(PRI,BUY),ILEN=1500                 
                                                                                
***********************************************************************         
* REQUEST MAP FOR DRAFT INSERTION UPLOAD                              *         
***********************************************************************         
                                                                                
REQDFT   LKMAP H,M#ULDFT,NEWREC=Y                                               
                                                                                
         LKMAP F,D#MEDCOD,CHAR,PP#MED,COL=001,OLEN=L'PAGYKMED                   
         LKMAP F,D#CLTCOD,CHAR,PP#CLTC,COL=002,OLEN=L'PCLTKCLT                  
         LKMAP F,D#PRDCOD,CHAR,PP#PRDC,COL=003,OLEN=L'PPRDKPRD                  
         LKMAP F,D#ESTNUM,VSTR,PP#ESTNO,COL=004                                 
         LKMAP F,D#PUBCOD,VSTR,PP#PUBCD,COL=005                                 
         LKMAP F,D#SPCDSC,VSTR,PP#SDESC,COL=006                                 
         LKMAP F,D#SHOWGS,VSTR,PP#SHGRP,COL=007                                 
         LKMAP F,D#REGDSP,VSTR,PP#RDISP,COL=008                                 
         LKMAP F,D#ILLPAN,VSTR,PP#IDISP,COL=009                                 
         LKMAP F,D#UNTRAT,VSTR,PP#RATE,COL=010                                  
         LKMAP F,D#PREMUM,VSTR,PP#PREM,COL=011                                  
         LKMAP F,D#ALLOCS,VSTR,PP#ALLOC,COL=012                                 
         LKMAP F,D#ADCODE,VSTR,PP#ADCOD,COL=013                                 
         LKMAP F,D#INSSTA,CHAR,PP#INSTA,COL=014,OLEN=1                          
         LKMAP F,D#BUYERC,CHAR,PP#BUYER,COL=016,OLEN=L'PBDBUYER                 
         LKMAP F,D#INSDAT,VSTR,PP#IDATE,COL=017                                 
         LKMAP F,D#INSDA2,VSTR,PP#IDAT2,COL=018                                 
         LKMAP F,D#SPCDAT,VSTR,PP#SCLDT,COL=019,OLEN=8                          
         LKMAP F,D#ONSDAT,VSTR,PP#ONSDT,COL=020,OLEN=8                          
         LKMAP F,D#BBLDAT,VSTR,PP#BBLDT,COL=021,OLEN=8                          
         LKMAP F,D#PBLDAT,VSTR,PP#PBLDT,COL=022,OLEN=8                          
         LKMAP F,D#SHPDAT,VSTR,PP#SHPDT,COL=023,OLEN=8                          
         LKMAP F,D#IORDAT,VSTR,PP#IORDT,COL=024,OLEN=8                          
         LKMAP F,D#MCLDAT,VSTR,PP#MCLDT,COL=025,OLEN=8                          
         LKMAP F,D#REGCO1,VSTR,PP#RCOM1,COL=026,OLEN=L'PBYOCOM1                 
         LKMAP F,D#REGCO2,VSTR,PP#RCOM2,COL=027,OLEN=L'PBYOCOM2                 
         LKMAP F,D#REGCO3,VSTR,PP#RCOM3,COL=028,OLEN=L'PBYOCOM3                 
         LKMAP F,D#REGCO4,VSTR,PP#RCOM4,COL=029,OLEN=L'PBYOCOM4                 
         LKMAP F,D#REGCO5,VSTR,PP#RCOM5,COL=030,OLEN=L'PBYOCOM5                 
         LKMAP F,D#INSCO1,VSTR,PP#ICOM1,COL=031,OLEN=L'PBYOCOM1                 
         LKMAP F,D#INSCO2,VSTR,PP#ICOM2,COL=032,OLEN=L'PBYOCOM2                 
         LKMAP F,D#INSCO3,VSTR,PP#ICOM3,COL=033,OLEN=L'PBYOCOM3                 
         LKMAP F,D#INSCO4,VSTR,PP#ICOM4,COL=034,OLEN=L'PBYOCOM4                 
         LKMAP F,D#INSCO5,VSTR,PP#ICOM5,COL=035,OLEN=L'PBYOCOM5                 
         LKMAP F,D#POSIN1,VSTR,PP#POSI1,COL=036,OLEN=L'PBYOCOM1                 
         LKMAP F,D#POSIN2,VSTR,PP#POSI2,COL=037,OLEN=L'PBYOCOM2                 
         LKMAP F,D#POSIN3,VSTR,PP#POSI3,COL=038,OLEN=L'PBYOCOM3                 
         LKMAP F,D#POSIN4,VSTR,PP#POSI4,COL=039,OLEN=L'PBYOCOM4                 
         LKMAP F,D#POSIN5,VSTR,PP#POSI5,COL=040,OLEN=L'PBYOCOM5                 
         LKMAP F,D#COMPCT,VSTR,PP#AGCDP,COL=041,OLEN=8                          
         LKMAP F,D#DSCPCT,VSTR,PP#CDPCT,COL=042,OLEN=8                          
         LKMAP F,D#TAXPCT,VSTR,PP#TAXP,COL=043,OLEN=8                           
         LKMAP F,D#PLCOST,VSTR,PP#PLCST,COL=044,OLEN=10                         
         LKMAP F,D#SPREP,VSTR,PP#SREP,COL=045,OLEN=10                           
         LKMAP F,D#SPECFH,VSTR,PP#SFH,COL=046,OLEN=10                           
         LKMAP F,D#DEFCIR,VSTR,PP#DEC,COL=055,OLEN=10                           
         LKMAP F,D#REPNTS,VSTR,PP#RPNTS,COL=056,OLEN=10                         
         LKMAP F,D#ESTIMP,VSTR,PP#EIMPS,COL=057,OLEN=10                         
         LKMAP F,D#ACTIMP,VSTR,PP#AIMPS,COL=058,OLEN=10                         
         LKMAP F,D#CLICK,VSTR,PP#CLICK,COL=059,OLEN=10                          
         LKMAP F,D#VIEWS,VSTR,PP#PVIEW,COL=060,OLEN=10                          
         LKMAP F,D#FSINS,VSTR,PP#FSINS,COL=061,OLEN=10                          
         LKMAP F,D#CONUVL,VSTR,PP#CUV,COL=062,OLEN=12                           
         LKMAP F,D#CONLIE,VSTR,PP#CLE,COL=063,OLEN=12                           
         LKMAP F,D#REFNUM,VSTR,PP#REFNO,COL=064,OLEN=10                         
         LKMAP F,D#ACHCOD,VSTR,PP#ACCOD,COL=065,OLEN=2                          
         LKMAP F,D#ACHGRS,VSTR,PP#ACAMT,COL=066,OLEN=12                         
         LKMAP F,D#ACHSAC,VSTR,PP#ACCBL,COL=067,OLEN=1                          
         LKMAP F,D#ACHCPT,VSTR,D#ACHCPT,COL=068,OLEN=12                         
         LKMAP F,D#ACHCDA,VSTR,PP#ACCDL,COL=069,OLEN=1                          
         LKMAP F,D#ACPM,VSTR,PP#ACPM,COL=070,OLEN=12                            
         LKMAP F,D#ECPM,VSTR,PP#ECPM,COL=071,OLEN=12                            
         LKMAP F,D#ICNUM,VSTR,PP#ICCOD,COL=072,OLEN=8                           
         LKMAP F,D#ICINUM,VSTR,PP#ICINS,COL=073,OLEN=8                          
         LKMAP F,D#TOTIMP,VSTR,PP#ICIMP,COL=074,OLEN=10                         
         LKMAP F,D#TOTCPM,VSTR,PP#ICCPM,COL=075,OLEN=12                         
         LKMAP F,D#TOTRAT,VSTR,PP#ICRAT,COL=076,OLEN=12                         
         LKMAP F,D#SITELO,VSTR,PP#SITLO,COL=077,OLEN=20                         
         LKMAP F,D#GST,VSTR,PP#GST,COL=078,OLEN=1                               
         LKMAP F,D#PST,VSTR,PP#PST,COL=080,OLEN=4                               
         LKMAP F,D#MCLXDT,VSTR,PP#MCXD,COL=088,OLEN=8                           
         LKMAP F,D#MCLXDY,VSTR,PP#MCXDY,COL=090,OLEN=8                          
         LKMAP F,D#ISSNM,VSTR,PP#ISSNM,COL=092                                  
         LKMAP F,D#AD_ID,VSTR,PP#AD_ID,COL=100                                  
         LKMAP F,D#CCSEQN,VSTR,PP#CCSQ#,COL=*                                   
         LKMAP F,D#CCFDAT,VSTR,PP#CCFDA,COL=*                                   
         LKMAP F,D#SRCOM1,VSTR,PP#SRCCM,COL=*,OLEN=L'PBYOCOM1                   
         LKMAP F,D#SRCOM2,VSTR,PP#SRCCM,COL=*,OLEN=L'PBYOCOM2                   
         LKMAP F,D#SRCOM3,VSTR,PP#SRCCM,COL=*,OLEN=L'PBYOCOM3                   
         LKMAP F,D#SRCOM4,VSTR,PP#SRCCM,COL=*,OLEN=L'PBYOCOM4                   
         LKMAP F,D#SRCOM5,VSTR,PP#SRCCM,COL=*,OLEN=L'PBYOCOM5                   
         LKMAP F,D#COS2$$,VSTR,PP#GRP9,COL=*,OLEN=20                            
         LKMAP F,002,CHAR,(*,TOKENLIT),COL=*,OLEN=6                             
                                                                                
         LKMAP E                                                                
                                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* REQUEST MAP FOR LIVE INSERTION UPLOAD                               *         
***********************************************************************         
                                                                                
REQNEW   LKMAP H,M#ULNEW,NEWREC=Y                                               
                                                                                
         LKMAP F,D#MEDCOD,CHAR,PP#MED,COL=001,OLEN=L'PAGYKMED                   
         LKMAP F,D#CLTCOD,CHAR,PP#CLTC,COL=002,OLEN=L'PCLTKCLT                  
         LKMAP F,D#PRDCOD,CHAR,PP#PRDC,COL=003,OLEN=L'PPRDKPRD                  
         LKMAP F,D#ESTNUM,VSTR,PP#ESTNO,COL=004                                 
         LKMAP F,D#PUBCOD,VSTR,PP#PUBCD,COL=005                                 
         LKMAP F,D#SPCDSC,VSTR,PP#SDESC,COL=006                                 
         LKMAP F,D#SHOWGS,VSTR,PP#SHGRP,COL=007                                 
         LKMAP F,D#REGDSP,VSTR,PP#RDISP,COL=008                                 
         LKMAP F,D#ILLPAN,VSTR,PP#IDISP,COL=009                                 
         LKMAP F,D#UNTRAT,VSTR,PP#RATE,COL=010                                  
         LKMAP F,D#PREMUM,VSTR,PP#PREM,COL=011                                  
         LKMAP F,D#ALLOCS,VSTR,PP#ALLOC,COL=012                                 
         LKMAP F,D#ADCODE,VSTR,PP#ADCOD,COL=013                                 
         LKMAP F,D#INSSTA,CHAR,PP#INSTA,COL=014,OLEN=1                          
         LKMAP F,D#INSIOR,VSTR,PP#LIONO,COL=015                                 
         LKMAP F,D#BUYERC,CHAR,PP#BUYER,COL=016,OLEN=L'PBDBUYER                 
         LKMAP F,D#INSDAT,VSTR,PP#IDATE,COL=017                                 
         LKMAP F,D#INSDA2,VSTR,PP#IDAT2,COL=018                                 
         LKMAP F,D#SPCDAT,VSTR,PP#SCLDT,COL=019,OLEN=8                          
         LKMAP F,D#ONSDAT,VSTR,PP#ONSDT,COL=020,OLEN=8                          
         LKMAP F,D#BBLDAT,VSTR,PP#BBLDT,COL=021,OLEN=8                          
         LKMAP F,D#PBLDAT,VSTR,PP#PBLDT,COL=022,OLEN=8                          
         LKMAP F,D#SHPDAT,VSTR,PP#SHPDT,COL=023,OLEN=8                          
         LKMAP F,D#IORDAT,VSTR,PP#IORDT,COL=024,OLEN=8                          
         LKMAP F,D#MCLDAT,VSTR,PP#MCLDT,COL=025,OLEN=8                          
         LKMAP F,D#REGCO1,VSTR,PP#RCOM1,COL=026,OLEN=L'PBYOCOM1                 
         LKMAP F,D#REGCO2,VSTR,PP#RCOM2,COL=027,OLEN=L'PBYOCOM2                 
         LKMAP F,D#REGCO3,VSTR,PP#RCOM3,COL=028,OLEN=L'PBYOCOM3                 
         LKMAP F,D#REGCO4,VSTR,PP#RCOM4,COL=029,OLEN=L'PBYOCOM4                 
         LKMAP F,D#REGCO5,VSTR,PP#RCOM5,COL=030,OLEN=L'PBYOCOM5                 
         LKMAP F,D#INSCO1,VSTR,PP#ICOM1,COL=031,OLEN=L'PBYOCOM1                 
         LKMAP F,D#INSCO2,VSTR,PP#ICOM2,COL=032,OLEN=L'PBYOCOM2                 
         LKMAP F,D#INSCO3,VSTR,PP#ICOM3,COL=033,OLEN=L'PBYOCOM3                 
         LKMAP F,D#INSCO4,VSTR,PP#ICOM4,COL=034,OLEN=L'PBYOCOM4                 
         LKMAP F,D#INSCO5,VSTR,PP#ICOM5,COL=035,OLEN=L'PBYOCOM5                 
         LKMAP F,D#POSIN1,VSTR,PP#POSI1,COL=036,OLEN=L'PBYOCOM1                 
         LKMAP F,D#POSIN2,VSTR,PP#POSI2,COL=037,OLEN=L'PBYOCOM2                 
         LKMAP F,D#POSIN3,VSTR,PP#POSI3,COL=038,OLEN=L'PBYOCOM3                 
         LKMAP F,D#POSIN4,VSTR,PP#POSI4,COL=039,OLEN=L'PBYOCOM4                 
         LKMAP F,D#POSIN5,VSTR,PP#POSI5,COL=040,OLEN=L'PBYOCOM5                 
         LKMAP F,D#COMPCT,VSTR,PP#AGCDP,COL=041,OLEN=8                          
         LKMAP F,D#DSCPCT,VSTR,PP#CDPCT,COL=042,OLEN=8                          
         LKMAP F,D#TAXPCT,VSTR,PP#TAXP,COL=043,OLEN=8                           
         LKMAP F,D#PLCOST,VSTR,PP#PLCST,COL=044,OLEN=10                         
         LKMAP F,D#SPREP,VSTR,PP#SREP,COL=045,OLEN=10                           
         LKMAP F,D#SPECFH,VSTR,PP#SFH,COL=046,OLEN=10                           
         LKMAP F,D#TSHAPR,VSTR,PP#TSHAP,COL=047,OLEN=1                          
         LKMAP F,D#TSHSTA,VSTR,PP#TSHST,COL=048,OLEN=8                          
         LKMAP F,D#TSHNOT,VSTR,PP#TSHPN,COL=049,OLEN=L'PTSHPAGE                 
         LKMAP F,D#REPROQ,VSTR,PP#TSHRQ,COL=050,OLEN=2                          
         LKMAP F,D#TSHCO1,VSTR,PP#TSHC1,COL=051,OLEN=TSCOMMAX                   
         LKMAP F,D#TSHCO2,VSTR,PP#TSHC2,COL=052,OLEN=TSCOMMAX                   
         LKMAP F,D#TSHCO3,VSTR,PP#TSHC3,COL=053,OLEN=TSCOMMAX                   
         LKMAP F,D#TSHCO4,VSTR,PP#TSHC4,COL=054,OLEN=TSCOMMAX                   
         LKMAP F,D#DEFCIR,VSTR,PP#DEC,COL=055,OLEN=10                           
         LKMAP F,D#REPNTS,VSTR,PP#RPNTS,COL=056,OLEN=10                         
         LKMAP F,D#ESTIMP,VSTR,PP#EIMPS,COL=057,OLEN=10                         
         LKMAP F,D#ACTIMP,VSTR,PP#AIMPS,COL=058,OLEN=10                         
         LKMAP F,D#CLICK,VSTR,PP#CLICK,COL=059,OLEN=10                          
         LKMAP F,D#VIEWS,VSTR,PP#PVIEW,COL=060,OLEN=10                          
         LKMAP F,D#FSINS,VSTR,PP#FSINS,COL=061,OLEN=10                          
         LKMAP F,D#CONUVL,VSTR,PP#CUV,COL=062,OLEN=12                           
         LKMAP F,D#CONLIE,VSTR,PP#CLE,COL=063,OLEN=12                           
         LKMAP F,D#REFNUM,VSTR,PP#REFNO,COL=064,OLEN=10                         
         LKMAP F,D#ACHCOD,VSTR,PP#ACCOD,COL=065,OLEN=2                          
         LKMAP F,D#ACHGRS,VSTR,PP#ACAMT,COL=066,OLEN=12                         
         LKMAP F,D#ACHSAC,VSTR,PP#ACCBL,COL=067,OLEN=1                          
         LKMAP F,D#ACHCPT,VSTR,PP#ACCPT,COL=068,OLEN=12                         
         LKMAP F,D#ACHCDA,VSTR,PP#ACCDL,COL=069,OLEN=1                          
         LKMAP F,D#ACPM,VSTR,PP#ACPM,COL=070,OLEN=12                            
         LKMAP F,D#ECPM,VSTR,PP#ECPM,COL=071,OLEN=12                            
         LKMAP F,D#ICNUM,VSTR,PP#ICCOD,COL=072,OLEN=8                           
         LKMAP F,D#ICINUM,VSTR,PP#ICINS,COL=073,OLEN=8                          
         LKMAP F,D#TOTIMP,VSTR,PP#ICIMP,COL=074,OLEN=10                         
         LKMAP F,D#TOTCPM,VSTR,PP#ICCPM,COL=075,OLEN=12                         
         LKMAP F,D#TOTRAT,VSTR,PP#ICRAT,COL=076,OLEN=12                         
         LKMAP F,D#SITELO,VSTR,PP#SITLO,COL=077,OLEN=20                         
         LKMAP F,D#GST,VSTR,PP#GST,COL=078,OLEN=1                               
         LKMAP F,D#PST,VSTR,PP#PST,COL=080,OLEN=4                               
         LKMAP F,D#ACHCOD,VSTR,PP#ACCOD,COL=082,OLEN=2                          
         LKMAP F,D#ACHGRS,VSTR,PP#ACAMT,COL=083,OLEN=12                         
         LKMAP F,D#ACHSAC,VSTR,PP#ACCBL,COL=084,OLEN=1                          
         LKMAP F,D#ACHCPT,VSTR,PP#ACCPT,COL=085,OLEN=12                         
         LKMAP F,D#ACHCDA,VSTR,PP#ACCDL,COL=086,OLEN=1                          
         LKMAP F,D#MCLXDT,VSTR,PP#MCXD,COL=088,OLEN=8                           
         LKMAP F,D#MCLXDY,VSTR,PP#MCXDY,COL=090,OLEN=8                          
         LKMAP F,D#DACTN,CHAR,PP#ACTN,COL=092,OLEN=1                            
         LKMAP F,D#ADBKEY,CHAR,PP#KEY,COL=094,OLEN=15                           
         LKMAP F,D#ISSNM,VSTR,PP#ISSNM,COL=096                                  
         LKMAP F,D#CCSEQN,VSTR,PP#CCSQ#,COL=097                                 
         LKMAP F,D#CCFDAT,VSTR,PP#CCFDA,COL=098                                 
         LKMAP F,D#TEAREC,VSTR,PP#TSHRD,COL=099                                 
         LKMAP F,D#AD_ID,VSTR,PP#AD_ID,COL=100                                  
         LKMAP F,D#C2FACT,VSTR,PP#@COS2,COL=101                                 
         LKMAP F,D#FROINS,CHAR,PP#KEY,COL=102,OLEN=24                           
         LKMAP F,D#PO#PRD,CHAR,PP#PRDC,COL=103,OLEN=L'PPRDKPRD                  
         LKMAP F,D#PO#OLD,VSTR,PP#PONUM,COL=104,OLEN=25                         
         LKMAP F,D#SRCOM1,VSTR,PP#SRCCM,COL=*,OLEN=L'PBYOCOM1                   
         LKMAP F,D#SRCOM2,VSTR,PP#SRCCM,COL=*,OLEN=L'PBYOCOM2                   
         LKMAP F,D#SRCOM3,VSTR,PP#SRCCM,COL=*,OLEN=L'PBYOCOM3                   
         LKMAP F,D#SRCOM4,VSTR,PP#SRCCM,COL=*,OLEN=L'PBYOCOM4                   
         LKMAP F,D#SRCOM5,VSTR,PP#SRCCM,COL=*,OLEN=L'PBYOCOM5                   
         LKMAP F,D#COS2$$,VSTR,PP#GRP9,COL=*,OLEN=20                            
         LKMAP F,D#INSORG,UBIN,(*,ORIGNLIT),COL=*,OLEN=1                        
                                                                                
         LKMAP E                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* REQUEST MAP FOR IDESK INSERTION UPLOAD (Prisma/Radia)               *         
***********************************************************************         
                                                                                
REQIDK   LKMAP H,M#ULIDK,NEWREC=Y                                               
                                                                                
         LKMAP F,D#CURDAT,VSTR,PP#DATE,COL=*                                    
         LKMAP F,D#CURTIM,CHAR,PP#TIME,COL=*,OLEN=6                             
         LKMAP F,D#DACTN,CHAR,PP#ACTN,COL=*,OLEN=3                              
         LKMAP F,D#INSKEY,CHAR,PP#KEY,COL=*,OLEN=24                             
         LKMAP F,D#SUBVER,CHAR,PP#SUBVN,COL=*,OLEN=8                            
         LKMAP F,00000012,CHAR,(*,ORIGNLIT),COL=*,OLEN=10                       
         LKMAP F,D#PRMIVE,CHAR,(*,INVECAMP),COL=*,OLEN=10                       
         LKMAP F,D#MEDCOD,CHAR,PP#MED,COL=*,OLEN=L'PAGYKMED                     
         LKMAP F,D#CLTCOD,CHAR,PP#CLTC,COL=*,OLEN=L'PCLTKCLT                    
         LKMAP F,D#PRDCOD,CHAR,PP#PRDC,COL=*,OLEN=L'PPRDKPRD                    
         LKMAP F,D#ESTNUM,VSTR,PP#ESTNO,COL=*                                   
         LKMAP F,D#PUBCOD,VSTR,PP#PUBCD,COL=*                                   
         LKMAP F,D#BUYERC,CHAR,PP#BUYER,COL=*,OLEN=L'PBDBUYER                   
         LKMAP F,D#STRDAT,VSTR,PP#STDAT,COL=*                                   
         LKMAP F,D#ENDDAT,VSTR,PP#ENDAT,COL=*                                   
         LKMAP F,D#SPCDSC,VSTR,PP#SDESC,COL=*                                   
         LKMAP F,D#UNTRAT,VSTR,PP#RATE,COL=*                                    
         LKMAP F,D#PLCOST,VSTR,PP#PLCST,COL=*                                   
         LKMAP F,D#TOTIMP,VSTR,PP#TIMPS,COL=*                                   
         LKMAP F,D#SPREP,VSTR,PP#SREP,COL=*,OLEN=10                             
         LKMAP F,D#CCSEQN,VSTR,PP#CCSQ#,COL=*                                   
         LKMAP F,D#CCFDAT,VSTR,PP#CCFDA,COL=*                                   
         LKMAP F,D#REGCO1,VSTR,PP#RCOM1,COL=*,OLEN=L'PBYOCOM1                   
         LKMAP F,D#REGCO2,VSTR,PP#RCOM2,COL=*,OLEN=L'PBYOCOM2                   
         LKMAP F,D#REGCO3,VSTR,PP#RCOM3,COL=*,OLEN=L'PBYOCOM3                   
         LKMAP F,D#REGCO4,VSTR,PP#RCOM4,COL=*,OLEN=L'PBYOCOM4                   
         LKMAP F,D#DEFCIR,VSTR,PP#DEC,COL=*,OLEN=10                             
         LKMAP F,D#REGDSP,VSTR,PP#RDISP,COL=*                                   
         LKMAP F,D#ILLPAN,VSTR,PP#IDISP,COL=*                                   
         LKMAP F,D#POSIN1,VSTR,PP#POSI1,COL=*,OLEN=L'PBYOCOM1                   
         LKMAP F,D#POSIN2,VSTR,PP#POSI2,COL=*,OLEN=L'PBYOCOM2                   
         LKMAP F,D#POSIN3,VSTR,PP#POSI3,COL=*,OLEN=L'PBYOCOM3                   
         LKMAP F,D#POSIN4,VSTR,PP#POSI4,COL=*,OLEN=L'PBYOCOM4                   
         LKMAP F,D#POSIN5,VSTR,PP#POSI5,COL=*,OLEN=L'PBYOCOM5                   
         LKMAP F,D#COS2$$,VSTR,PP#GRP9,COL=*,OLEN=20                            
         LKMAP F,D#PNTOTC,VSTR,PP#TRATE,COL=*,OLEN=10                           
         LKMAP F,D#ONSDAT,VSTR,PP#ONSDT,COL=*,OLEN=8                            
         LKMAP F,D#MCLDAT,VSTR,PP#MCLDT,COL=*,OLEN=8                            
         LKMAP F,D#INSCO1,VSTR,PP#ICOM1,COL=*,OLEN=L'PBYOCOM1                   
         LKMAP F,D#INSCO2,VSTR,PP#ICOM2,COL=*,OLEN=L'PBYOCOM2                   
         LKMAP F,D#INSCO3,VSTR,PP#ICOM3,COL=*,OLEN=L'PBYOCOM3                   
         LKMAP F,D#INSCO4,VSTR,PP#ICOM4,COL=*,OLEN=L'PBYOCOM4                   
         LKMAP F,D#INSCO5,VSTR,PP#ICOM5,COL=*,OLEN=L'PBYOCOM5                   
         LKMAP F,D#TEAREC,VSTR,PP#TSHRD,COL=*                                   
                                                                                
         LKMAP E                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* REQUEST MAP FOR CHANGE INSERTION UPLOAD                             *         
***********************************************************************         
                                                                                
REQCHA   LKMAP H,M#ULCHA,NEWREC=Y                                               
                                                                                
         LKMAP F,D#DACTN,CHAR,PP#ACTN,COL=001,OLEN=1                            
         LKMAP F,D#INSKEY,CHAR,PP#KEY,COL=002,OLEN=24                           
         LKMAP F,D#SPCDSC+0,VSTR,PP#SDESC,COL=003,OLEN=L'PBDSPACE               
         LKMAP F,D#SPCDSC+1,VSTR,PP#SDESC,COL=004,OLEN=L'PBDSPACE               
         LKMAP F,D#SHOWGS+0,VSTR,PP#SHGRP,COL=005,OLEN=8                        
         LKMAP F,D#SHOWGS+1,VSTR,PP#SHGRP,COL=006,OLEN=8                        
         LKMAP F,D#REGDSP+0,VSTR,PP#RDISP,COL=007,OLEN=8                        
         LKMAP F,D#REGDSP+1,VSTR,PP#RDISP,COL=008,OLEN=8                        
         LKMAP F,D#ILLPAN+0,VSTR,PP#IDISP,COL=009,OLEN=8                        
         LKMAP F,D#ILLPAN+1,VSTR,PP#IDISP,COL=010,OLEN=8                        
         LKMAP F,D#UNTRAT+0,VSTR,PP#RATE,COL=011,OLEN=12                        
         LKMAP F,D#UNTRAT+1,VSTR,PP#RATE,COL=012,OLEN=12                        
         LKMAP F,D#PREMUM+0,VSTR,PP#PREM,COL=013,OLEN=L'PBYOPRM                 
         LKMAP F,D#PREMUM+1,VSTR,PP#PREM,COL=014,OLEN=L'PBYOPRM                 
         LKMAP F,D#ALLOCS+0,VSTR,PP#ALLOC,COL=015,OLEN=L'PBYOZZZ                
         LKMAP F,D#ALLOCS+1,VSTR,PP#ALLOC,COL=016,OLEN=L'PBYOZZZ                
         LKMAP F,D#ADCODE+0,VSTR,PP#ADCOD,COL=017,OLEN=L'PBDJOB                 
         LKMAP F,D#ADCODE+1,VSTR,PP#ADCOD,COL=018,OLEN=L'PBDJOB                 
         LKMAP F,D#INSSTA+0,VSTR,PP#INSTA,COL=019,OLEN=4                        
         LKMAP F,D#INSSTA+1,VSTR,PP#INSTA,COL=020,OLEN=4                        
         LKMAP F,D#INSIOR+0,VSTR,PP#LIONO,COL=021,OLEN=5                        
         LKMAP F,D#INSIOR+1,VSTR,PP#LIONO,COL=022,OLEN=5                        
         LKMAP F,D#BUYERC+0,VSTR,PP#BUYER,COL=023,OLEN=L'PBDBUYER               
         LKMAP F,D#BUYERC+1,VSTR,PP#BUYER,COL=024,OLEN=L'PBDBUYER               
         LKMAP F,D#INSDAT+0,VSTR,PP#IDATE,COL=025,OLEN=8                        
         LKMAP F,D#INSDAT+1,VSTR,PP#IDATE,COL=026,OLEN=8                        
         LKMAP F,D#INSDA2+0,VSTR,PP#IDAT2,COL=027,OLEN=8                        
         LKMAP F,D#INSDA2+1,VSTR,PP#IDAT2,COL=028,OLEN=8                        
         LKMAP F,D#SPCDAT+0,VSTR,PP#SCLDT,COL=029,OLEN=8                        
         LKMAP F,D#SPCDAT+1,VSTR,PP#SCLDT,COL=030,OLEN=8                        
         LKMAP F,D#ONSDAT+0,VSTR,PP#ONSDT,COL=031,OLEN=8                        
         LKMAP F,D#ONSDAT+1,VSTR,PP#ONSDT,COL=032,OLEN=8                        
         LKMAP F,D#BBLDAT+0,VSTR,PP#BBLDT,COL=033,OLEN=8                        
         LKMAP F,D#BBLDAT+1,VSTR,PP#BBLDT,COL=034,OLEN=8                        
         LKMAP F,D#PBLDAT+0,VSTR,PP#PBLDT,COL=035,OLEN=8                        
         LKMAP F,D#PBLDAT+1,VSTR,PP#PBLDT,COL=036,OLEN=8                        
         LKMAP F,D#SHPDAT+0,VSTR,PP#SHPDT,COL=037,OLEN=8                        
         LKMAP F,D#SHPDAT+1,VSTR,PP#SHPDT,COL=038,OLEN=8                        
         LKMAP F,D#IORDAT+0,VSTR,PP#IORDT,COL=039,OLEN=8                        
         LKMAP F,D#IORDAT+1,VSTR,PP#IORDT,COL=040,OLEN=8                        
         LKMAP F,D#MCLDAT+0,VSTR,PP#MCLDT,COL=041,OLEN=8                        
         LKMAP F,D#MCLDAT+1,VSTR,PP#MCLDT,COL=042,OLEN=8                        
         LKMAP F,D#REGCO1+0,VSTR,PP#RCOM1,COL=043,OLEN=L'PBYOCOM1               
         LKMAP F,D#REGCO1+1,VSTR,PP#RCOM1,COL=044,OLEN=L'PBYOCOM1               
         LKMAP F,D#REGCO2+0,VSTR,PP#RCOM2,COL=045,OLEN=L'PBYOCOM2               
         LKMAP F,D#REGCO2+1,VSTR,PP#RCOM2,COL=046,OLEN=L'PBYOCOM2               
         LKMAP F,D#REGCO3+0,VSTR,PP#RCOM3,COL=047,OLEN=L'PBYOCOM3               
         LKMAP F,D#REGCO3+1,VSTR,PP#RCOM3,COL=048,OLEN=L'PBYOCOM3               
         LKMAP F,D#REGCO4+0,VSTR,PP#RCOM4,COL=049,OLEN=L'PBYOCOM4               
         LKMAP F,D#REGCO4+1,VSTR,PP#RCOM4,COL=050,OLEN=L'PBYOCOM4               
         LKMAP F,D#REGCO5+0,VSTR,PP#RCOM5,COL=051,OLEN=L'PBYOCOM5               
         LKMAP F,D#REGCO5+1,VSTR,PP#RCOM5,COL=052,OLEN=L'PBYOCOM5               
         LKMAP F,D#INSCO1+0,VSTR,PP#ICOM1,COL=053,OLEN=L'PBYOCOM1               
         LKMAP F,D#INSCO1+1,VSTR,PP#ICOM1,COL=054,OLEN=L'PBYOCOM1               
         LKMAP F,D#INSCO2+0,VSTR,PP#ICOM2,COL=055,OLEN=L'PBYOCOM2               
         LKMAP F,D#INSCO2+1,VSTR,PP#ICOM2,COL=056,OLEN=L'PBYOCOM2               
         LKMAP F,D#INSCO3+0,VSTR,PP#ICOM3,COL=057,OLEN=L'PBYOCOM3               
         LKMAP F,D#INSCO3+1,VSTR,PP#ICOM3,COL=058,OLEN=L'PBYOCOM3               
         LKMAP F,D#INSCO4+0,VSTR,PP#ICOM4,COL=059,OLEN=L'PBYOCOM4               
         LKMAP F,D#INSCO4+1,VSTR,PP#ICOM4,COL=060,OLEN=L'PBYOCOM4               
         LKMAP F,D#INSCO5+0,VSTR,PP#ICOM5,COL=061,OLEN=L'PBYOCOM5               
         LKMAP F,D#INSCO5+1,VSTR,PP#ICOM5,COL=062,OLEN=L'PBYOCOM5               
         LKMAP F,D#POSIN1+0,VSTR,PP#POSI1,COL=063,OLEN=L'PBYOCOM1               
         LKMAP F,D#POSIN1+1,VSTR,PP#POSI1,COL=064,OLEN=L'PBYOCOM1               
         LKMAP F,D#POSIN2+0,VSTR,PP#POSI2,COL=065,OLEN=L'PBYOCOM2               
         LKMAP F,D#POSIN2+1,VSTR,PP#POSI2,COL=066,OLEN=L'PBYOCOM2               
         LKMAP F,D#POSIN3+0,VSTR,PP#POSI3,COL=067,OLEN=L'PBYOCOM3               
         LKMAP F,D#POSIN3+1,VSTR,PP#POSI3,COL=068,OLEN=L'PBYOCOM3               
         LKMAP F,D#POSIN4+0,VSTR,PP#POSI4,COL=069,OLEN=L'PBYOCOM4               
         LKMAP F,D#POSIN4+1,VSTR,PP#POSI4,COL=070,OLEN=L'PBYOCOM4               
         LKMAP F,D#POSIN5+0,VSTR,PP#POSI5,COL=071,OLEN=L'PBYOCOM5               
         LKMAP F,D#POSIN5+1,VSTR,PP#POSI5,COL=072,OLEN=L'PBYOCOM5               
         LKMAP F,D#COMPCT+0,VSTR,PP#AGCDP,COL=073,OLEN=8                        
         LKMAP F,D#COMPCT+1,VSTR,PP#AGCDP,COL=074,OLEN=8                        
         LKMAP F,D#DSCPCT+0,VSTR,PP#CDPCT,COL=075,OLEN=8                        
         LKMAP F,D#DSCPCT+1,VSTR,PP#CDPCT,COL=076,OLEN=8                        
         LKMAP F,D#TAXPCT+0,VSTR,PP#TAXP,COL=077,OLEN=8                         
         LKMAP F,D#TAXPCT+1,VSTR,PP#TAXP,COL=078,OLEN=8                         
         LKMAP F,D#PLCOST+0,VSTR,PP#PLCST,COL=079,OLEN=10                       
         LKMAP F,D#PLCOST+1,VSTR,PP#PLCST,COL=080,OLEN=10                       
         LKMAP F,D#SPREP+0,VSTR,PP#SREP,COL=081,OLEN=10                         
         LKMAP F,D#SPREP+1,VSTR,PP#SREP,COL=082,OLEN=10                         
         LKMAP F,D#SPECFH+0,VSTR,PP#SFH,COL=083,OLEN=10                         
         LKMAP F,D#SPECFH+1,VSTR,PP#SFH,COL=084,OLEN=10                         
         LKMAP F,D#TSHAPR+0,VSTR,PP#TSHAP,COL=085,OLEN=1                        
         LKMAP F,D#TSHAPR+1,VSTR,PP#TSHAP,COL=086,OLEN=1                        
         LKMAP F,D#TSHSTA+0,VSTR,PP#TSHST,COL=087,OLEN=8                        
         LKMAP F,D#TSHSTA+1,VSTR,PP#TSHST,COL=088,OLEN=8                        
         LKMAP F,D#TSHNOT+0,VSTR,PP#TSHPN,COL=089,OLEN=L'PTSHPAGE               
         LKMAP F,D#TSHNOT+1,VSTR,PP#TSHPN,COL=090,OLEN=L'PTSHPAGE               
         LKMAP F,D#REPROQ+0,VSTR,PP#TSHRQ,COL=091,OLEN=2                        
         LKMAP F,D#REPROQ+1,VSTR,PP#TSHRQ,COL=092,OLEN=2                        
         LKMAP F,D#TSHCO1+0,VSTR,PP#TSHC1,COL=093,OLEN=TSCOMMAX                 
         LKMAP F,D#TSHCO1+1,VSTR,PP#TSHC1,COL=094,OLEN=TSCOMMAX                 
         LKMAP F,D#TSHCO2+0,VSTR,PP#TSHC2,COL=095,OLEN=TSCOMMAX                 
         LKMAP F,D#TSHCO2+1,VSTR,PP#TSHC2,COL=096,OLEN=TSCOMMAX                 
         LKMAP F,D#TSHCO3+0,VSTR,PP#TSHC3,COL=097,OLEN=TSCOMMAX                 
         LKMAP F,D#TSHCO3+1,VSTR,PP#TSHC3,COL=098,OLEN=TSCOMMAX                 
         LKMAP F,D#TSHCO4+0,VSTR,PP#TSHC4,COL=099,OLEN=TSCOMMAX                 
         LKMAP F,D#TSHCO4+1,VSTR,PP#TSHC4,COL=100,OLEN=TSCOMMAX                 
         LKMAP F,D#DEFCIR+0,VSTR,PP#DEC,COL=101,OLEN=10                         
         LKMAP F,D#DEFCIR+1,VSTR,PP#DEC,COL=102,OLEN=10                         
         LKMAP F,D#REPNTS+0,VSTR,PP#RPNTS,COL=103,OLEN=10                       
         LKMAP F,D#REPNTS+1,VSTR,PP#RPNTS,COL=104,OLEN=10                       
         LKMAP F,D#ESTIMP+0,VSTR,PP#EIMPS,COL=105,OLEN=10                       
         LKMAP F,D#ESTIMP+1,VSTR,PP#EIMPS,COL=106,OLEN=10                       
         LKMAP F,D#ACTIMP+0,VSTR,PP#AIMPS,COL=107,OLEN=10                       
         LKMAP F,D#ACTIMP+1,VSTR,PP#AIMPS,COL=108,OLEN=10                       
         LKMAP F,D#CLICK+0,VSTR,PP#CLICK,COL=109,OLEN=10                        
         LKMAP F,D#CLICK+1,VSTR,PP#CLICK,COL=110,OLEN=10                        
         LKMAP F,D#VIEWS+0,VSTR,PP#PVIEW,COL=111,OLEN=10                        
         LKMAP F,D#VIEWS+1,VSTR,PP#PVIEW,COL=112,OLEN=10                        
         LKMAP F,D#FSINS+0,VSTR,PP#FSINS,COL=113,OLEN=10                        
         LKMAP F,D#FSINS+1,VSTR,PP#FSINS,COL=114,OLEN=10                        
         LKMAP F,D#CONUVL+0,VSTR,PP#CUV,COL=115,OLEN=12                         
         LKMAP F,D#CONUVL+1,VSTR,PP#CUV,COL=116,OLEN=12                         
         LKMAP F,D#CONLIE+0,VSTR,PP#CLE,COL=117,OLEN=12                         
         LKMAP F,D#CONLIE+1,VSTR,PP#CLE,COL=118,OLEN=12                         
         LKMAP F,D#REFNUM+0,VSTR,PP#REFNO,COL=119,OLEN=10                       
         LKMAP F,D#REFNUM+1,VSTR,PP#REFNO,COL=120,OLEN=10                       
         LKMAP F,D#ACHCOD+0,VSTR,PP#ACCOD,COL=121,OLEN=2                        
         LKMAP F,D#ACHCOD+1,VSTR,PP#ACCOD,COL=122,OLEN=2                        
         LKMAP F,D#ACHGRS+0,VSTR,PP#ACAMT,COL=123,OLEN=12                       
         LKMAP F,D#ACHGRS+1,VSTR,PP#ACAMT,COL=124,OLEN=12                       
         LKMAP F,D#ACHSAC+0,VSTR,PP#ACCBL,COL=125,OLEN=1                        
         LKMAP F,D#ACHSAC+1,VSTR,PP#ACCBL,COL=126,OLEN=1                        
         LKMAP F,D#ACHCPT+0,VSTR,PP#ACCPT,COL=127,OLEN=12                       
         LKMAP F,D#ACHCPT+1,VSTR,PP#ACCPT,COL=128,OLEN=12                       
         LKMAP F,D#ACHCDA+0,VSTR,PP#ACCDL,COL=129,OLEN=1                        
         LKMAP F,D#ACHCDA+1,VSTR,PP#ACCDL,COL=130,OLEN=1                        
         LKMAP F,D#ACPM+0,VSTR,PP#ACPM,COL=131,OLEN=12                          
         LKMAP F,D#ACPM+1,VSTR,PP#ACPM,COL=132,OLEN=12                          
         LKMAP F,D#ECPM+0,VSTR,PP#ECPM,COL=133,OLEN=12                          
         LKMAP F,D#ECPM+1,VSTR,PP#ECPM,COL=134,OLEN=12                          
         LKMAP F,D#SITELO+0,VSTR,PP#SITLO,COL=135,OLEN=20                       
         LKMAP F,D#SITELO+1,VSTR,PP#SITLO,COL=136,OLEN=20                       
         LKMAP F,D#ICNUM+0,VSTR,PP#ICCOD,COL=137,OLEN=8                         
         LKMAP F,D#ICNUM+1,VSTR,PP#ICCOD,COL=138,OLEN=8                         
         LKMAP F,D#ICINUM+0,VSTR,PP#ICINS,COL=139,OLEN=8                        
         LKMAP F,D#ICINUM+1,VSTR,PP#ICINS,COL=140,OLEN=8                        
         LKMAP F,D#TOTIMP+0,VSTR,PP#ICIMP,COL=141,OLEN=10                       
         LKMAP F,D#TOTIMP+1,VSTR,PP#ICIMP,COL=142,OLEN=10                       
         LKMAP F,D#TOTCPM+0,VSTR,PP#ICCPM,COL=143,OLEN=12                       
         LKMAP F,D#TOTCPM+1,VSTR,PP#ICCPM,COL=144,OLEN=12                       
         LKMAP F,D#TOTRAT+0,VSTR,PP#ICRAT,COL=145,OLEN=12                       
         LKMAP F,D#TOTRAT+1,VSTR,PP#ICRAT,COL=146,OLEN=12                       
         LKMAP F,D#GST+0,VSTR,PP#GST,COL=147,OLEN=1                             
         LKMAP F,D#GST+1,VSTR,PP#GST,COL=148,OLEN=1                             
         LKMAP F,D#PST+0,VSTR,PP#PST,COL=149,OLEN=4                             
         LKMAP F,D#PST+1,VSTR,PP#PST,COL=150,OLEN=4                             
         LKMAP F,D#MCLXDT+0,VSTR,PP#MCXD,COL=151,OLEN=8                         
         LKMAP F,D#MCLXDT+1,VSTR,PP#MCXD,COL=152,OLEN=8                         
         LKMAP F,D#MCLXDY+0,VSTR,PP#MCXDY,COL=153,OLEN=3                        
         LKMAP F,D#MCLXDY+1,VSTR,PP#MCXDY,COL=154,OLEN=3                        
         LKMAP F,D#ISSNM+0,VSTR,PP#ISSNM,COL=155                                
         LKMAP F,D#ISSNM+1,VSTR,PP#ISSNM,COL=156                                
         LKMAP F,D#MATSTA+0,VSTR,PP#IMSTA,COL=157                               
         LKMAP F,D#MATSTA+1,VSTR,PP#IMSTA,COL=158                               
         LKMAP F,D#DISSTA+0,VSTR,PP#IDSTA,COL=159                               
         LKMAP F,D#DISSTA+1,VSTR,PP#IDSTA,COL=160                               
         LKMAP F,D#TEAREC+0,VSTR,PP#TSHRD,COL=161                               
         LKMAP F,D#TEAREC+1,VSTR,PP#TSHRD,COL=162                               
                                                                                
         LKMAP F,D#CCSEQN,VSTR,PP#CCSQ#,COL=163                                 
         LKMAP F,D#CCFDAT+0,VSTR,PP#CCFDA,COL=164                               
         LKMAP F,D#CCFDAT+1,VSTR,PP#CCFDA,COL=165                               
                                                                                
         LKMAP F,D#AD_ID+0,VSTR,PP#AD_ID,COL=166                                
         LKMAP F,D#AD_ID+1,VSTR,PP#AD_ID,COL=167                                
         LKMAP F,D#C2FACT+0,VSTR,PP#@COS2,COL=168                               
         LKMAP F,D#C2FACT+1,VSTR,PP#@COS2,COL=169                               
                                                                                
         LKMAP F,D#REVINV,VSTR,PP#INVTT,COL=170                                 
                                                                                
         LKMAP F,D#PO#PRD,CHAR,PP#PRDC,COL=171,OLEN=L'PPRDKPRD                  
         LKMAP F,D#PO#OLD+0,VSTR,PP#PONUM,COL=172,OLEN=25                       
         LKMAP F,D#PO#OLD+1,VSTR,PP#PONUM,COL=173,OLEN=25                       
                                                                                
         LKMAP F,D#SRCOM1+0,VSTR,PP#SRCCM,COL=*,OLEN=L'PBYOCOM1                 
         LKMAP F,D#SRCOM1+1,VSTR,PP#SRCCM,COL=*,OLEN=L'PBYOCOM1                 
         LKMAP F,D#SRCOM2+0,VSTR,PP#SRCCM,COL=*,OLEN=L'PBYOCOM2                 
         LKMAP F,D#SRCOM2+1,VSTR,PP#SRCCM,COL=*,OLEN=L'PBYOCOM2                 
         LKMAP F,D#SRCOM3+0,VSTR,PP#SRCCM,COL=*,OLEN=L'PBYOCOM3                 
         LKMAP F,D#SRCOM3+1,VSTR,PP#SRCCM,COL=*,OLEN=L'PBYOCOM3                 
         LKMAP F,D#SRCOM4+0,VSTR,PP#SRCCM,COL=*,OLEN=L'PBYOCOM4                 
         LKMAP F,D#SRCOM4+1,VSTR,PP#SRCCM,COL=*,OLEN=L'PBYOCOM4                 
         LKMAP F,D#SRCOM5+0,VSTR,PP#SRCCM,COL=*,OLEN=L'PBYOCOM5                 
         LKMAP F,D#SRCOM5+1,VSTR,PP#SRCCM,COL=*,OLEN=L'PBYOCOM5                 
                                                                                
         LKMAP F,D#C2$OLD,VSTR,PP#GRP9,COL=*,OLEN=20                            
         LKMAP F,D#C2$NEW,VSTR,PP#GRP9,COL=*,OLEN=20                            
                                                                                
         LKMAP E                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* REQUEST MAP FOR DELETE INSERTION UPLOAD                             *         
***********************************************************************         
                                                                                
REQDEL   LKMAP H,M#ULDEL,NEWREC=Y                                               
                                                                                
         LKMAP F,D#INSKEY,CHAR,PP#KEY,COL=001,OLEN=24                           
         LKMAP F,D#BUYERC,CHAR,PP#BUYER,COL=002,OLEN=L'PBDBUYER                 
                                                                                
         LKMAP E                                                                
                                                                                
         LKMAP X                                                                
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                               
TSCOMMAX EQU   66        MAX LENGTH OF TEARSHEET COMMENT                        
*                                                                               
TOKENLIT DC    C'Token'                                                         
ORIGNLIT DC    C'Insertion Origin'                                              
INVECAMP DC    C'Invoice enabled campaign'                                      
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPLNKWRK                                                       
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
*                                                                               
* PRINT BUY RECORD ELEMENT DSECTS                                               
*                                                                               
       ++INCLUDE PBDELEM                                                        
       ++INCLUDE PTSHTEL                                                        
       ++INCLUDE PPGENPBMAT                                                     
         PRINT ON                                                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPBYOUTD                                                       
         ORG   PBYOCOMS                                                         
PBYOCOM1 DS    CL47                COMMENT LINE 1                               
PBYOCOM2 DS    CL47                ............ 2                               
PBYOCOM3 DS    CL47                ............ 3                               
PBYOCOM4 DS    CL47                ............ 4                               
PBYOCOM5 DS    CL47                ............ 5                               
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'041PPLNK13   01/25/21'                                      
         END                                                                    

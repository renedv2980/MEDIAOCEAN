*          DATA SET PPLNK16    AT LEVEL 050 AS OF 06/12/18                      
*PHASE T41416A                                                                  
PPLNK16  TITLE '- INVOICE UPLOAD MAPS'                                          
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=US,REQUEST=*,WORKERKEY=PPVU,SYSTEM=PRTSYSQ,        *        
               IBLOCK=*,IFROM=(PRI,LIN),ITO=(PRI,INV),ILEN=1500                 
                                                                                
***********************************************************************         
* REQUEST MAP FOR ADD INVOICE HEADER UPLOAD                           *         
***********************************************************************         
*                                                                               
REQAIH   LKMAP H,M#ULAIH,NEWREC=Y                                               
*                                                                               
         LKMAP F,D#MEDCOD,CHAR,PP#MED,COL=001,OLEN=L'PNVKMED                    
         LKMAP F,D#CLTCOD,CHAR,PP#CLTC,COL=002,OLEN=L'PNV1CLT                   
         LKMAP F,D#PUBCOD,VSTR,PP#PUBCD,COL=003                                 
         LKMAP F,D#VINVNO,VSTR,PP#VINVN,COL=004                                 
         LKMAP F,D#INVSTA,CHAR,PP#INVST,COL=005,OLEN=1                          
         LKMAP F,D#STEND,VSTR,PP#STEND,COL=006                                  
         LKMAP F,D#INVDAT,VSTR,PP#INVDT,COL=007                                 
         LKMAP F,D#INVTOT,VSTR,PP#INVTO,COL=008                                 
         LKMAP F,D#ITOTYP,CHAR,PP#INVTT,COL=009,OLEN=1                          
         LKMAP F,D#SPREP,VSTR,PP#SREP,COL=010                                   
         LKMAP F,D#COMMNT,VSTR,PP#@COMM,COL=011                                 
         LKMAP F,D#INVSRC,CHAR,PP#FROM,COL=012,OLEN=1                           
         LKMAP F,D#INVTAX,VSTR,PP#@TAX,COL=013                                  
         LKMAP F,D#IVPSID,CHAR,PP#IDSK,COL=014,OLEN=L'PNVHPSID                  
*                                                                               
         LKMAP E                                                                
         EJECT                                                                  
*                                                                               
***********************************************************************         
* REQUEST MAP FOR ADD INVOICE DETAIL UPLOAD                           *         
***********************************************************************         
*                                                                               
REQAID   LKMAP H,M#ULAII,NEWREC=Y                                               
*                                                                               
         LKMAP F,D#INVKEY,CHAR,PP#INVKY,COL=001,OLEN=11                         
         LKMAP F,D#INSKEY,CHAR,PP#PAYKY,COL=002,OLEN=13                         
         LKMAP F,D#UNTRAT,VSTR,PP#RATE,COL=003                                  
         LKMAP F,D#PREMUM,VSTR,PP#PREM,COL=004                                  
         LKMAP F,D#NETORD,VSTR,PP#NET,COL=005                                   
         LKMAP F,D#GRSORD,VSTR,PP#GROSS,COL=006                                 
         LKMAP F,D#NUMLIN,VSTR,PP#LINE,COL=007                                  
         LKMAP F,D#INSDAT,VSTR,PP#IDATE,COL=008                                 
         LKMAP F,D#SPCDSC,VSTR,PP#SDESC,COL=009                                 
         LKMAP F,D#ADCAP,VSTR,PP#ADCAP,COL=010                                  
         LKMAP F,D#CLTCOD,CHAR,PP#CLTC,COL=011,OLEN=L'PNVDCLT                   
         LKMAP F,D#PRDCOD,CHAR,PP#PRDC,COL=012,OLEN=L'PNVDPRD                   
         LKMAP F,D#PUBCOD,VSTR,PP#PUBCD,COL=013                                 
         LKMAP F,D#ESTIMP,VSTR,PP#EIMPS,COL=014                                 
         LKMAP F,D#ACTIMP,VSTR,PP#AIMPS,COL=015                                 
         LKMAP F,D#ECPM,VSTR,PP#ECPM,COL=016                                    
         LKMAP F,D#ACPM,VSTR,PP#ACPM,COL=017                                    
         LKMAP F,D#DCOMCD,VSTR,PP#DCM,COL=018                                   
         LKMAP F,D#COMMNT,VSTR,PP#@COMM,COL=019                                 
         LKMAP F,D#INVSRC,CHAR,PP#FROM,COL=020,OLEN=1                           
         LKMAP F,D#IVPSID,CHAR,PP#IDSK,COL=021,OLEN=L'PNVHPSID                  
*                                                                               
         LKMAP E                                                                
         EJECT                                                                  
*                                                                               
***********************************************************************         
* REQUEST MAP FOR CHANGE INVOICE HEADER UPLOAD                        *         
***********************************************************************         
*                                                                               
REQCIH   LKMAP H,M#ULCIH,NEWREC=Y                                               
*                                                                               
         LKMAP F,D#INVKEY,CHAR,PP#INVKY,COL=001,OLEN=11                         
         LKMAP F,D#IVPSID,CHAR,PP#IDSK,COL=002,OLEN=L'PNVHPSID                  
         LKMAP F,D#INVSTA+0,VSTR,PP#INVST,COL=003                               
         LKMAP F,D#INVSTA+1,VSTR,PP#INVST,COL=004                               
         LKMAP F,D#STEND+0,VSTR,PP#STEND,COL=005                                
         LKMAP F,D#STEND+1,VSTR,PP#STEND,COL=006                                
         LKMAP F,D#INVDAT+0,VSTR,PP#INVDT,COL=007                               
         LKMAP F,D#INVDAT+1,VSTR,PP#INVDT,COL=008                               
         LKMAP F,D#INVTOT+0,VSTR,PP#INVTO,COL=009                               
         LKMAP F,D#INVTOT+1,VSTR,PP#INVTO,COL=010                               
         LKMAP F,D#ITOTYP+0,VSTR,PP#INVTT,COL=011                               
         LKMAP F,D#ITOTYP+1,VSTR,PP#INVTT,COL=012                               
         LKMAP F,D#SPREP+0,VSTR,PP#SREP,COL=013                                 
         LKMAP F,D#SPREP+1,VSTR,PP#SREP,COL=014                                 
         LKMAP F,D#COMMNT,VSTR,PP#@COMM,COL=015                                 
         LKMAP F,D#INVSRC,CHAR,PP#FROM,COL=016,OLEN=1                           
         LKMAP F,D#INVTAX+0,VSTR,PP#@TAX,COL=017                                
         LKMAP F,D#INVTAX+1,VSTR,PP#@TAX,COL=017                                
*                                                                               
         LKMAP E                                                                
         EJECT                                                                  
*                                                                               
***********************************************************************         
* REQUEST MAP FOR CHANGE INVOICE DETAIL UPLOAD                        *         
***********************************************************************         
*                                                                               
REQCID   LKMAP H,M#ULCII,NEWREC=Y                                               
*                                                                               
         LKMAP F,D#INVKEY,CHAR,PP#INVKY,COL=001,OLEN=11                         
         LKMAP F,D#IVPSID,CHAR,PP#IDSK,COL=002,OLEN=L'PNVHPSID                  
         LKMAP F,D#ITMSQN,VSTR,PP#DSQNO,COL=003                                 
         LKMAP F,D#INSKEY+0,VSTR,PP#PAYKY,COL=005                               
         LKMAP F,D#INSKEY+1,VSTR,PP#PAYKY,COL=006                               
         LKMAP F,D#UNTRAT+0,VSTR,PP#RATE,COL=007                                
         LKMAP F,D#UNTRAT+1,VSTR,PP#RATE,COL=008                                
         LKMAP F,D#PREMUM+0,VSTR,PP#PREM,COL=009                                
         LKMAP F,D#PREMUM+1,VSTR,PP#PREM,COL=010                                
         LKMAP F,D#NETORD+0,VSTR,PP#NET,COL=011                                 
         LKMAP F,D#NETORD+1,VSTR,PP#NET,COL=012                                 
         LKMAP F,D#GRSORD+0,VSTR,PP#GROSS,COL=013                               
         LKMAP F,D#GRSORD+1,VSTR,PP#GROSS,COL=014                               
         LKMAP F,D#NUMLIN+0,VSTR,PP#LINE,COL=015                                
         LKMAP F,D#NUMLIN+1,VSTR,PP#LINE,COL=016                                
         LKMAP F,D#INSDAT+0,VSTR,PP#IDATE,COL=017                               
         LKMAP F,D#INSDAT+1,VSTR,PP#IDATE,COL=018                               
         LKMAP F,D#SPCDSC+0,VSTR,PP#SDESC,COL=019                               
         LKMAP F,D#SPCDSC+1,VSTR,PP#SDESC,COL=020                               
         LKMAP F,D#ADCAP+0,VSTR,PP#ADCAP,COL=021                                
         LKMAP F,D#ADCAP+1,VSTR,PP#ADCAP,COL=022                                
         LKMAP F,D#CLTCOD+0,VSTR,PP#CLTC,COL=023                                
         LKMAP F,D#CLTCOD+1,VSTR,PP#CLTC,COL=024                                
         LKMAP F,D#PRDCOD+0,VSTR,PP#PRDC,COL=025                                
         LKMAP F,D#PRDCOD+1,VSTR,PP#PRDC,COL=026                                
         LKMAP F,D#PUBCOD+0,VSTR,PP#PUBCD,COL=027                               
         LKMAP F,D#PUBCOD+1,VSTR,PP#PUBCD,COL=028                               
         LKMAP F,D#ESTIMP+0,VSTR,PP#EIMPS,COL=029                               
         LKMAP F,D#ESTIMP+1,VSTR,PP#EIMPS,COL=030                               
         LKMAP F,D#ACTIMP+0,VSTR,PP#AIMPS,COL=031                               
         LKMAP F,D#ACTIMP+1,VSTR,PP#AIMPS,COL=032                               
         LKMAP F,D#ECPM+0,VSTR,PP#ECPM,COL=033                                  
         LKMAP F,D#ECPM+1,VSTR,PP#ECPM,COL=034                                  
         LKMAP F,D#ACPM+0,VSTR,PP#ACPM,COL=035                                  
         LKMAP F,D#ACPM+1,VSTR,PP#ACPM,COL=036                                  
         LKMAP F,D#DCOMCD+0,VSTR,PP#DCM,COL=037                                 
         LKMAP F,D#DCOMCD+1,VSTR,PP#DCM,COL=038                                 
         LKMAP F,D#COMMNT,VSTR,PP#@COMM,COL=039                                 
         LKMAP F,D#INVSRC,CHAR,PP#FROM,COL=040,OLEN=1                           
*                                                                               
         LKMAP E                                                                
         EJECT                                                                  
*                                                                               
***********************************************************************         
* REQUEST MAP FOR DELETE INVOICE HEADER UPLOAD                        *         
***********************************************************************         
*                                                                               
REQDLH   LKMAP H,M#ULDIH,NEWREC=Y                                               
*                                                                               
         LKMAP F,D#INVKEY,CHAR,PP#INVKY,COL=001,OLEN=11                         
         LKMAP F,D#INVSRC,CHAR,PP#FROM,COL=002,OLEN=1                           
         LKMAP F,D#IVPSID,CHAR,PP#IDSK,COL=003,OLEN=L'PNVHPSID                  
*                                                                               
         LKMAP E                                                                
         EJECT                                                                  
*                                                                               
***********************************************************************         
* REQUEST MAP FOR DELETE INVOICE DETAIL UPLOAD                        *         
***********************************************************************         
*                                                                               
REQDLD   LKMAP H,M#ULDII,NEWREC=Y                                               
*                                                                               
         LKMAP F,D#INVKEY,CHAR,PP#INVKY,COL=001,OLEN=11                         
         LKMAP F,D#ITMSQN,VSTR,PP#DSQNO,COL=002                                 
         LKMAP F,D#INVSRC,CHAR,PP#FROM,COL=003,OLEN=1                           
         LKMAP F,D#IVPSID,CHAR,PP#IDSK,COL=004,OLEN=L'PNVHPSID                  
*                                                                               
         LKMAP E                                                                
         EJECT                                                                  
*                                                                               
***********************************************************************         
* REQUEST MAP FOR UNLINK INVOICE AND INSERTION UPLOAD                 *         
***********************************************************************         
*                                                                               
REQULK   LKMAP H,M#ULULK,NEWREC=Y                                               
*                                                                               
         LKMAP F,D#INVKEY,CHAR,PP#INVKY,COL=001,OLEN=11                         
         LKMAP F,D#ITMSQN,VSTR,PP#DSQNO,COL=002                                 
*                                                                               
         LKMAP E                                                                
         EJECT                                                                  
*                                                                               
***********************************************************************         
* REQUEST MAP FOR DELETE MATCHED AND CLEARED INVOICE ITEM             *         
***********************************************************************         
*                                                                               
REQDMT   LKMAP H,M#ULDMT,NEWREC=Y                                               
*                                                                               
         LKMAP F,D#INVKEY,CHAR,PP#INVKY,COL=001,OLEN=11                         
         LKMAP F,D#ITMSQN,VSTR,PP#DSQNO,COL=002                                 
*                                                                               
         LKMAP E                                                                
         EJECT                                                                  
*                                                                               
***********************************************************************         
* REQUEST MAP FOR INVOICE HISTORY                                     *         
***********************************************************************         
*                                                                               
REQHST   LKMAP H,M#ULNVHS,NEWREC=Y                                              
*                                                                               
         LKMAP F,D#INVKEY,CHAR,PP#INVKY,COL=001,OLEN=11                         
         LKMAP F,D#HISTYP,CHAR,PP#HSTID,COL=002,OLEN=1                          
         LKMAP F,D#ITMSQN,VSTR,PP#DSQNO,COL=003                                 
*                                                                               
         LKMAP E                                                                
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                               
         LKMAP X                                                                
*                                                                               
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPLNKWRK                                                       
         PRINT ON                                                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPGENPNV          PRINT INVOICE                                
         PRINT ON                                                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PSERELEM          PRINT INSERTION SERIAL# ELEM                 
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'050PPLNK16   06/12/18'                                      
         END                                                                    

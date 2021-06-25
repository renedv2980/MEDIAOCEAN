*          DATA SET GELNK12    AT LEVEL 001 AS OF 11/16/09                      
*PHASE TA0612A                                                                  
GELNK12  TITLE '- MFM/MediaVantage downloads'                                   
         PRINT NOGEN                                                            
SVRDEF   LKSVR TYPE=US,REQUEST=*,SERVERLIST=SERVERS,USERIDS=Y,         *        
               WORKERKEY=CTSD,SEGMENT=Y,SYSTEM=CTLSYSQ                          
                                                                                
SERVERS  DC    AL1(TSTSTEW,TSTSPOT,TSTADBY,0)                                   
                                                                                
REQCFMC  LKMAP H,I#MFMCLT,NEWREC=Y                                              
SSLim    LKMAP F,1,ACC1,CT#SSLIM,PARM=TSTSPOT,MAXLEN=L'LP_ACCS                  
SUsId    LKMAP F,11,USID,CT#SUSID,PARM=TSTSPOT,MAXLEN=L'CTIKID                  
NSLim    LKMAP F,2,ACC1,CT#NSLIM,PARM=TSTSTEW,MAXLEN=L'LP_ACCS                  
NUsId    LKMAP F,12,USID,CT#NUSID,PARM=TSTSTEW,MAXLEN=L'CTIKID                  
PSLim    LKMAP F,3,ACC1,CT#PSLIM,PARM=TSTADBY,MAXLEN=L'LP_ACCS                  
PUsId    LKMAP F,13,USID,CT#PUSID,PARM=TSTADBY,MAXLEN=L'CTIKID                  
         LKMAP E                                                                
                                                                                
REQCFMB  LKMAP H,I#MFMBRD,NEWREC=Y                                              
SSLim    LKMAP F,1,ACC1,CT#SSLIM,PARM=TSTSPOT,MAXLEN=L'LP_ACCS                  
SUsId    LKMAP F,11,USID,CT#SUSID,PARM=TSTSPOT,MAXLEN=L'CTIKID                  
NSLim    LKMAP F,2,ACC1,CT#NSLIM,PARM=TSTSTEW,MAXLEN=L'LP_ACCS                  
NUsId    LKMAP F,12,USID,CT#NUSID,PARM=TSTSTEW,MAXLEN=L'CTIKID                  
PSLim    LKMAP F,3,ACC1,CT#PSLIM,PARM=TSTADBY,MAXLEN=L'LP_ACCS                  
PUsId    LKMAP F,13,USID,CT#PUSID,PARM=TSTADBY,MAXLEN=L'CTIKID                  
SysCd    LKMAP F,4,CHAR,CT#SYS,ARRAY=S,OLEN=1                                   
MedCd    LKMAP F,5,CHAR,CT#MEDC,OLEN=1                                          
CltCd    LKMAP F,6,CHAR,CT#CLI,ARRAY=E,OLEN=3                                   
         LKMAP E                                                                
                                                                                
REQCFMV  LKMAP H,I#MFMVEN,NEWREC=Y                                              
SUsId    LKMAP F,11,USID,CT#SUSID,PARM=TSTSPOT,MAXLEN=L'CTIKID                  
NUsId    LKMAP F,12,USID,CT#NUSID,PARM=TSTSTEW,MAXLEN=L'CTIKID                  
PUsId    LKMAP F,13,USID,CT#PUSID,PARM=TSTADBY,MAXLEN=L'CTIKID                  
SysCd    LKMAP F,1,CHAR,CT#SYS,ARRAY=S,OLEN=1                                   
MedCd    LKMAP F,2,CHAR,CT#MEDC,ARRAY=E,OLEN=1                                  
         LKMAP E                                                                
                                                                                
REQCFMM  LKMAP H,I#MFMMKT,NEWREC=Y                                              
SUsId    LKMAP F,11,USID,CT#SUSID,PARM=TSTSPOT,MAXLEN=L'CTIKID                  
MedCd    LKMAP F,1,CHAR,CT#MEDC,LIST=Y,OLEN=1                                   
         LKMAP E                                                                
                                                                                
REQSTAT  LKMAP H,I#MVANDL,NEWREC=Y                                              
SSLim    LKMAP F,1,ACC1,CT#SSLIM,PARM=TSTSPOT,MAXLEN=L'LP_ACCS                  
SUsId    LKMAP F,11,USID,CT#SUSID,PARM=TSTSPOT,MAXLEN=L'CTIKID                  
NSLim    LKMAP F,2,ACC1,CT#NSLIM,PARM=TSTSTEW,MAXLEN=L'LP_ACCS                  
NUsId    LKMAP F,12,USID,CT#NUSID,PARM=TSTSTEW,MAXLEN=L'CTIKID                  
PSLim    LKMAP F,3,ACC1,CT#PSLIM,PARM=TSTADBY,MAXLEN=L'LP_ACCS                  
PUsId    LKMAP F,13,USID,CT#PUSID,PARM=TSTADBY,MAXLEN=L'CTIKID                  
MedNd    LKMAP F,21,LBIN,CT#MCONT,OLEN=L'CFMKRNOD,LIST=F                        
AdvNd    LKMAP F,22,LBIN,CT#ACONT,OLEN=L'CFMKRNOD,LIST=F                        
BrdNd    LKMAP F,30,LBIN,CT#BCONT,OLEN=L'CFMKRNOD,ARRAY=S                       
CliCd    LKMAP F,31,CHAR,CT#CFMCM,OLEN=L'CFMKBCLT                               
BrdCd    LKMAP F,32,CHAR,CT#CFMBM,OLEN=L'CFMKBRDE,ARRAY=E                       
VenNd    LKMAP F,23,LBIN,CT#SCONT,OLEN=L'CFMKRNOD,LIST=F                        
StrDt    LKMAP F,24,EDAT,CT#STRDT,OLEN=6                                        
EndDt    LKMAP F,25,EDAT,CT#ENDDT,OLEN=6                                        
CalOp    LKMAP F,26,CHAR,CT#CALOP,OLEN=1                                        
SplOp    LKMAP F,27,CHAR,CT#SPLIT,OLEN=1                                        
Sumrz    LKMAP F,28,CHAR,CT#SUMRZ,OLEN=1                                        
PNmTm    LKMAP F,29,CHAR,CT#PNMTM,OLEN=1                                        
EstOp    LKMAP F,33,CHAR,CT#ESTOP,OLEN=1                                        
EstNm    LKMAP F,34,CHAR,(*,ENMLIT),OLEN=1                                      
         LKMAP E                                                                
                                                                                
         LKMAP X                                                                
                                                                                
ENMLIT   DC    C'Estimate names required?'                                      
                                                                                
* GELNKWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE GELNKWRK                                                       
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001GELNK12   11/16/09'                                      
         END                                                                    

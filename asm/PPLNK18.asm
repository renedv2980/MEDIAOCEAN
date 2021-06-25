*          DATA SET PPLNK18    AT LEVEL 006 AS OF 12/17/07                      
*PHASE T41418A                                                                  
PPLNK18  TITLE '- WEBIO UPLOAD MAPS'                                            
SVRDEF   CSECT                                                                  
         DC    (RSVRDEFL)X'00'                                                  
*                                                                               
         ORG   SVRDEF                                                           
         DC    C'*SERVER**SERVER**SERVER**SERVER*'                              
         DC    AL2(0)              NO CODE                                      
         DC    AL2(0)              NO FILE LIST                                 
         DC    AL2(0)              NO FACILITIES LIST                           
         DC    AL2(REQUEST-SVRDEF) REQUEST MAP                                  
         DC    AL2(IBLOCK-SVRDEF)  PROGRAM INTERFACE BLOCK                      
         ORG   SVRDEF+(RSVRSYSC-RSVRDEFD)                                       
         DC    C'PP'               SYSTEM                                       
         DC    C'WU'               PROGRAM                                      
         ORG   SVRDEF+(RSVRIND1-RSVRDEFD)                                       
         DC    AL1(RSVRILNK)                                                    
         ORG                                                                    
*                                                                               
***********************************************************************         
* PROGRAM INTERFACE BLOCK                                             *         
***********************************************************************         
*                                                                               
IBLOCK   DS    0X                                                               
         DS    (LI_LNQ)X'00'                                                    
         ORG   IBLOCK                                                           
         DC    AL2(1500)           L'RETURNED AREA                              
         DC    C'PRI',C'LIN'       FROM SYSTEM/PROGRAM                          
         DC    C'PRI',C'WIO'       TO SYSTEM/PROGRAM                            
         ORG                                                                    
         EJECT                                                                  
REQUEST  DS    0X                                                               
*                                                                               
***********************************************************************         
* REQUEST MAP FOR DELETE INSERION ORDER RECORD                        *         
***********************************************************************         
*                                                                               
REQIODL  DS    0XL(LH_LNQ)                                                      
         DC    AL2(REQIODLX+1-*)   END OF TABLE                                 
         DC    AL2(M#ULIODL)       REQUEST INDENTIFER - DELETE IO               
         DC    AL1(LH_IANUR)       ADD NEW UPLOAD RECORD FOR THIS MAP           
         DC    AL2(0)              NO OUTPUT MAP ASSOCIATED                     
         DC    XL4'00'             VERSION NUMBER                               
*                                                                               
DI#IOKY  EQU   1                                                                
         DC    AL2(D#IOLKEY)       LONG IO KEY MAP CODE                         
         DC    CL5'IOKEY'          TITLE                                        
         DC    AL1(DI#IOKY)        SEQUENTIAL FIELD NUMBER                      
         DC    AL1(LD_IAWPQ)       ADD TO WORK POOL                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(19)             DATA LENGTH                                  
         DC    AL1(LD_CHARQ)       DATA TYPE IS CHARACTER                       
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#LIOKY)  PRINT SYSTEM, LITERAL                
         DC    XL4'00'                                                          
*                                                                               
DI#PUBC  EQU   2                                                                
         DC    AL2(D#PUBCOD)       PUB MAP CODE                                 
         DC    CL5'PUBCD'          TITLE                                        
         DC    AL1(DI#PUBC)        SEQUENTIAL FIELD NUMBER                      
         DC    AL1(LD_IAWPQ)       ADD TO WORK POOL                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(LD_VSTRQ)       DATA TYPE IS VARIABLE STRING                 
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#PUBCD)  PRINT SYSTEM, LITERAL                
         DC    XL4'00'                                                          
*                                                                               
REQIODLX DC    AL1(LD_EOTQ)                                                     
         EJECT                                                                  
*                                                                               
***********************************************************************         
* REQUEST MAP FOR ADD STATUS TO INSERTION ORDER RECORD                *         
***********************************************************************         
*                                                                               
REQIOST  DS    0XL(LH_LNQ)                                                      
         DC    AL2(REQIOSTX+1-*)   END OF TABLE                                 
         DC    AL2(M#ULIOST)       REQUEST INDENTIFER - STATUS ADD              
         DC    AL1(LH_IANUR)       ADD NEW UPLOAD RECORD FOR THIS MAP           
         DC    AL2(0)              NO OUTPUT MAP ASSOCIATED                     
         DC    XL4'00'             VERSION NUMBER                               
*                                                                               
ST#IOKY  EQU   1                                                                
         DC    AL2(D#IOLKEY)       LONG IO KEY MAP CODE                         
         DC    CL5'IOKEY'          TITLE                                        
         DC    AL1(ST#IOKY)        SEQUENTIAL FIELD NUMBER                      
         DC    AL1(LD_IAWPQ)       ADD TO WORK POOL                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(19)             DATA LENGTH                                  
         DC    AL1(LD_CHARQ)       DATA TYPE IS CHARACTER                       
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#LIOKY)  PRINT SYSTEM, LITERAL                
         DC    XL4'00'                                                          
*                                                                               
ST#HSTID EQU   2                                                                
         DC    AL2(D#HSTYID)       UPLOAD HISTORY ID                            
         DC    CL5'HSTID'          TITLE                                        
         DC    AL1(ST#HSTID)       SEQUENTIAL FIELD NUMBER                      
         DC    AL1(LD_IAWPQ)       ADD TO WORK POOL                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(12)             DATA LENGTH                                  
         DC    AL1(LD_CHARQ)       DATA TYPE IS CHARACTER                       
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#HSTID)  PRINT SYSTEM, LITERAL                
         DC    XL4'00'                                                          
*                                                                               
ST#PUBC  EQU   3                                                                
         DC    AL2(D#PUBCOD)       PUB MAP CODE                                 
         DC    CL5'PUBCD'          TITLE                                        
         DC    AL1(ST#PUBC)        SEQUENTIAL FIELD NUMBER                      
         DC    AL1(LD_IAWPQ)       ADD TO WORK POOL                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(LD_VSTRQ)       DATA TYPE IS VARIABLE STRING                 
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#PUBCD)  PRINT SYSTEM, LITERAL                
         DC    XL4'00'                                                          
*                                                                               
ST#STAT  EQU   4                                                                
         DC    AL2(D#IORSTA)       STATUS                                       
         DC    CL5'STAT '          TITLE                                        
         DC    AL1(ST#STAT)        SEQUENTIAL FIELD NUMBER                      
         DC    AL1(LD_IAWPQ)       ADD TO WORK POOL                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(LD_VSTRQ)       DATA TYPE IS VARIABLE STRING                 
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#STAT)  PRINT SYSTEM, LITERAL                 
         DC    XL4'00'                                                          
*                                                                               
REQIOSTX DC    AL1(LD_EOTQ)                                                     
         EJECT                                                                  
*                                                                               
***********************************************************************         
* REQUEST MAP FOR SENDING FAX                                         *         
***********************************************************************         
*                                                                               
REQIOFX  DS    0XL(LH_LNQ)                                                      
         DC    AL2(REQIOFXX+1-*)   END OF TABLE                                 
         DC    AL2(M#ULIOFX)       REQUEST INDENTIFER - STATUS ADD              
         DC    AL1(LH_IANUR)       ADD NEW UPLOAD RECORD FOR THIS MAP           
         DC    AL2(0)              NO OUTPUT MAP ASSOCIATED                     
         DC    XL4'00'             VERSION NUMBER                               
*                                                                               
FX#IOKY  EQU   1                                                                
         DC    AL2(D#IOLKEY)       LONG IO KEY MAP CODE                         
         DC    CL5'IOKEY'          TITLE                                        
         DC    AL1(FX#IOKY)        SEQUENTIAL FIELD NUMBER                      
         DC    AL1(LD_IAWPQ)       ADD TO WORK POOL                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(19)             DATA LENGTH                                  
         DC    AL1(LD_CHARQ)       DATA TYPE IS CHARACTER                       
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#LIOKY)  PRINT SYSTEM, LITERAL                
         DC    XL4'00'                                                          
*                                                                               
FX#HSTID EQU   2                                                                
         DC    AL2(D#HSTYID)       UPLOAD HISTORY ID                            
         DC    CL5'HSTID'          TITLE                                        
         DC    AL1(FX#HSTID)       SEQUENTIAL FIELD NUMBER                      
         DC    AL1(LD_IAWPQ)       ADD TO WORK POOL                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(12)             DATA LENGTH                                  
         DC    AL1(LD_CHARQ)       DATA TYPE IS CHARACTER                       
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#HSTID)  PRINT SYSTEM, LITERAL                
         DC    XL4'00'                                                          
*                                                                               
FX#CPYFX EQU   3                                                                
         DC    AL2(D#CPYFXP)       COPY FAX TO PQ                               
         DC    CL5'CPYFX'          TITLE                                        
         DC    AL1(FX#CPYFX)       SEQUENTIAL FIELD NUMBER                      
         DC    AL1(LD_IAWPQ)       ADD TO WORK POOL                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(01)             DATA LENGTH                                  
         DC    AL1(LD_CHARQ)       DATA TYPE IS CHARACTER                       
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#CPYFX)  PRINT SYSTEM, LITERAL                
         DC    XL4'00'                                                          
*                                                                               
FX#PUBC  EQU   4                                                                
         DC    AL2(D#PUBCOD)       PUB MAP CODE                                 
         DC    CL5'PUBCD'          TITLE                                        
         DC    AL1(FX#PUBC)        SEQUENTIAL FIELD NUMBER                      
         DC    AL1(LD_IAWPQ)       ADD TO WORK POOL                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(LD_VSTRQ)       DATA TYPE IS VARIABLE STRING                 
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#PUBCD)  PRINT SYSTEM, LITERAL                
         DC    XL4'00'                                                          
*                                                                               
FX#MANSD EQU   5                                                                
         DC    AL2(D#MANSEN)       MANUAL SEND FLAG                             
         DC    CL5'MANSD'          TITLE                                        
         DC    AL1(FX#MANSD)       SEQUENTIAL FIELD NUMBER                      
         DC    AL1(LD_IAWPQ)       ADD TO WORK POOL                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(LD_VSTRQ)       DATA TYPE IS VARIABLE STRING                 
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#MANSD) PRINT SYSTEM, LITERAL                 
         DC    XL4'00'                                                          
*                                                                               
FX#EADDR EQU   6                                                                
         DC    AL2(D#E_MAIL)       E-MAIL ADDRESS                               
         DC    CL5'EADDR'          TITLE                                        
         DC    AL1(FX#EADDR)       SEQUENTIAL FIELD NUMBER                      
         DC    AL1(LD_IAWPQ)       ADD TO WORK POOL                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(LD_VSTRQ)       DATA TYPE IS VARIABLE STRING                 
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#EADR)  PRINT SYSTEM, LITERAL                 
         DC    XL4'00'                                                          
*                                                                               
FX#FAXNM EQU   7                                                                
         DC    AL2(D#RECPNM)       FAX NAME                                     
         DC    CL5'FAXNM'          TITLE                                        
         DC    AL1(FX#FAXNM)       SEQUENTIAL FIELD NUMBER                      
         DC    AL1(LD_IAWPQ)       ADD TO WORK POOL                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(LD_VSTRQ)       DATA TYPE IS VARIABLE STRING                 
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#FAXNM)  PRINT SYSTEM, LITERAL                
         DC    XL4'00'                                                          
*                                                                               
FX#FAXTP EQU   8                                                                
         DC    AL2(D#TYPE_2)       FAX NUMBER TYPE                              
         DC    CL5'FAXTP'          TITLE                                        
         DC    AL1(FX#FAXTP)       SEQUENTIAL FIELD NUMBER                      
         DC    AL1(LD_IAWPQ)       ADD TO WORK POOL                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(LD_VSTRQ)       DATA TYPE IS VARIABLE STRING                 
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#FAXTP)  PRINT SYSTEM, LITERAL                
         DC    XL4'00'                                                          
*                                                                               
FX#FAX#  EQU   9                                                                
         DC    AL2(D#ADRFAX)       FAX NUMBER                                   
         DC    CL5'FAX# '          TITLE                                        
         DC    AL1(FX#FAX#)        SEQUENTIAL FIELD NUMBER                      
         DC    AL1(LD_IAWPQ)       ADD TO WORK POOL                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(LD_VSTRQ)       DATA TYPE IS VARIABLE STRING                 
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#FAX#)  PRINT SYSTEM, LITERAL                 
         DC    XL4'00'                                                          
*                                                                               
FX#SPCST EQU   10                                                               
         DC    AL2(D#SUPCOS)       SUPPRESS COST INDICATOR                      
         DC    CL5'SPCST'          TITLE                                        
         DC    AL1(FX#SPCST)       SEQUENTIAL FIELD NUMBER                      
         DC    AL1(LD_IAWPQ)       ADD TO WORK POOL                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(LD_VSTRQ)       DATA TYPE IS VARIABLE STRING                 
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#SPCST) PRINT SYSTEM, LITERAL                 
         DC    XL4'00'                                                          
*                                                                               
REQIOFXX DC    AL1(LD_EOTQ)                                                     
*                                                                               
***********************************************************************         
* REQUEST MAP FOR FAX LINE OF TEXT                                    *         
***********************************************************************         
*                                                                               
REQIOFL  DS    0XL(LH_LNQ)                                                      
         DC    AL2(REQIOFLX+1-*)   END OF TABLE                                 
         DC    AL2(E#IOFXLN)       REQUEST INDENTIFER - FAX TEXT                
         DC    AL1(LH_IANUR)       ADD NEW UPLOAD RECORD FOR THIS MAP           
         DC    AL2(0)              NO OUTPUT MAP ASSOCIATED                     
         DC    XL4'00'             VERSION NUMBER                               
*                                                                               
FX#FXLTX EQU   1                                                                
         DC    AL2(D#LINTXT)       FAX TEXT                                     
         DC    CL5'FAXTX'          TITLE                                        
         DC    AL1(FX#FXLTX)       SEQUENTIAL FIELD NUMBER                      
         DC    AL1(LD_IAWPQ)       ADD TO WORK POOL                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(LD_VSTRQ)       DATA TYPE IS VARIABLE STRING                 
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#FAXTX)  PRINT SYSTEM, LITERAL                
         DC    XL4'00'                                                          
*                                                                               
REQIOFLX DC    AL1(LD_EOTQ)                                                     
*                                                                               
***********************************************************************         
* REQUEST MAP FOR INSERTION ORDER STATUS INQUIRY                      *         
***********************************************************************         
*                                                                               
REQIOIQ  DS    0XL(LH_LNQ)                                                      
         DC    AL2(REQIOIQX+1-*)   END OF TABLE                                 
         DC    AL2(M#ULIOIQ)       REQUEST INDENTIFER - STATUS INQUIRY          
         DC    AL1(LH_IANUR)       ADD NEW UPLOAD RECORD FOR THIS MAP           
         DC    AL2(0)              NO OUTPUT MAP ASSOCIATED                     
         DC    XL4'00'             VERSION NUMBER                               
*                                                                               
IQ#REQTP EQU   1                                                                
         DC    AL2(D#HISTYP)       HISTORY TYPE CODE                            
         DC    CL5'REQTP'          TITLE                                        
         DC    AL1(IQ#REQTP)       SEQUENTIAL FIELD NUMBER                      
         DC    AL1(LD_IAWPQ)       ADD TO WORK POOL                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(1)              DATA LENGTH                                  
         DC    AL1(LD_CHARQ)       DATA TYPE IS CHARACTER                       
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#REQFD)  PRINT SYSTEM, LITERAL                
         DC    XL4'00'                                                          
*                                                                               
IQ#STATP EQU   2                                                                
         DC    AL2(D#INQMET)       INQUIRY METHOD CODE                          
         DC    CL5'STATP'          TITLE                                        
         DC    AL1(IQ#STATP)       SEQUENTIAL FIELD NUMBER                      
         DC    AL1(LD_IAWPQ)       ADD TO WORK POOL                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(1)              DATA LENGTH                                  
         DC    AL1(LD_CHARQ)       DATA TYPE IS CHARACTER                       
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#STAT)  PRINT SYSTEM, LITERAL                 
         DC    XL4'00'                                                          
*                                                                               
IQ#DATTP EQU   3                                                                
         DC    AL2(D#DTYRPY)       LONG IO KEY MAP CODE                         
         DC    CL5'DATTP'          TITLE                                        
         DC    AL1(IQ#DATTP)       SEQUENTIAL FIELD NUMBER                      
         DC    AL1(LD_IAWPQ)       ADD TO WORK POOL                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(10)             DATA LENGTH                                  
         DC    AL1(LD_CHARQ)       DATA TYPE IS CHARACTER                       
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#DATA)  PRINT SYSTEM, LITERAL                 
         DC    XL4'00'                                                          
*                                                                               
IQ#FLTRS EQU   4                                                                
         DC    AL2(D#FLTTYP)       FILTER TYPE                                  
         DC    CL5'FLTRS'          TITLE                                        
         DC    AL1(IQ#FLTRS)       SEQUENTIAL FIELD NUMBER                      
         DC    AL1(LD_IAWPQ)       ADD TO WORK POOL                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(1)              DATA LENGTH                                  
         DC    AL1(LD_CHARQ)       DATA TYPE IS CHARACTER                       
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#FLTRS)  PRINT SYSTEM, LITERAL                
         DC    XL4'00'                                                          
*                                                                               
IQ#IOKY  EQU   5                                                                
         DC    AL2(D#IOLKEY)       LONG IO KEY MAP CODE                         
         DC    CL5'IOKEY'          TITLE                                        
         DC    AL1(IQ#IOKY)        SEQUENTIAL FIELD NUMBER                      
         DC    AL1(LD_IAWPQ)       ADD TO WORK POOL                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(19)             DATA LENGTH                                  
         DC    AL1(LD_CHARQ)       DATA TYPE IS CHARACTER                       
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#LIOKY)  PRINT SYSTEM, LITERAL                
         DC    XL4'00'                                                          
*                                                                               
IQ#PUBC  EQU   6                                                                
         DC    AL2(D#PUBCOD)       PUB MAP CODE                                 
         DC    CL5'PUBCD'          TITLE                                        
         DC    AL1(IQ#PUBC)        SEQUENTIAL FIELD NUMBER                      
         DC    AL1(LD_IAWPQ)       ADD TO WORK POOL                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(LD_VSTRQ)       DATA TYPE IS VARIABLE STRING                 
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#PUBCD)  PRINT SYSTEM, LITERAL                
         DC    XL4'00'                                                          
*                                                                               
REQIOIQX DC    AL1(LD_EOTQ)                                                     
*                                                                               
***********************************************************************         
* REQUEST MAP FOR INSERTION ORDER DOWNLOAD                            *         
***********************************************************************         
*                                                                               
REQIODW  DS    0XL(LH_LNQ)                                                      
         DC    AL2(REQIODWX+1-*)   END OF TABLE                                 
         DC    AL2(M#ULIODW)       REQUEST INDENTIFER - IO DOWNLOAD             
         DC    AL1(LH_IANUR)       ADD NEW UPLOAD RECORD FOR THIS MAP           
         DC    AL2(0)              NO OUTPUT MAP ASSOCIATED                     
         DC    XL4'00'             VERSION NUMBER                               
*                                                                               
DW#REQTP EQU   1                                                                
         DC    AL2(D#HISTYP)       HISTORY TYPE CODE                            
         DC    CL5'REQTP'          TITLE                                        
         DC    AL1(DW#REQTP)       SEQUENTIAL FIELD NUMBER                      
         DC    AL1(LD_IAWPQ)       ADD TO WORK POOL                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(1)              DATA LENGTH                                  
         DC    AL1(LD_CHARQ)       DATA TYPE IS CHARACTER                       
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#REQFD)  PRINT SYSTEM, LITERAL                
         DC    XL4'00'                                                          
*                                                                               
DW#STATP EQU   2                                                                
         DC    AL2(D#INQMET)       INQUIRY METHOD CODE                          
         DC    CL5'STATP'          TITLE                                        
         DC    AL1(DW#STATP)       SEQUENTIAL FIELD NUMBER                      
         DC    AL1(LD_IAWPQ)       ADD TO WORK POOL                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(1)              DATA LENGTH                                  
         DC    AL1(LD_CHARQ)       DATA TYPE IS CHARACTER                       
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#STAT)  PRINT SYSTEM, LITERAL                 
         DC    XL4'00'                                                          
*                                                                               
DW#DATTP EQU   3                                                                
         DC    AL2(D#DTYRPY)       DATA TO BE REPLIED                           
         DC    CL5'DATTP'          TITLE                                        
         DC    AL1(DW#DATTP)       SEQUENTIAL FIELD NUMBER                      
         DC    AL1(LD_IAWPQ)       ADD TO WORK POOL                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(10)             DATA LENGTH                                  
         DC    AL1(LD_CHARQ)       DATA TYPE IS CHARACTER                       
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#DATA)  PRINT SYSTEM, LITERAL                 
         DC    XL4'00'                                                          
*                                                                               
DW#FLTRS EQU   4                                                                
         DC    AL2(D#FLTTYP)       FILTER TYPE                                  
         DC    CL5'FLTRS'          TITLE                                        
         DC    AL1(DW#FLTRS)       SEQUENTIAL FIELD NUMBER                      
         DC    AL1(LD_IAWPQ)       ADD TO WORK POOL                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(1)              DATA LENGTH                                  
         DC    AL1(LD_CHARQ)       DATA TYPE IS CHARACTER                       
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#FLTRS)  PRINT SYSTEM, LITERAL                
         DC    XL4'00'                                                          
*                                                                               
DW#MEDCD EQU   5                                                                
         DC    AL2(D#MEDCOD)       MEDIA CODE                                   
         DC    CL5'MEDIA'          TITLE                                        
         DC    AL1(DW#MEDCD)       SEQUENTIAL FIELD NUMBER                      
         DC    AL1(LD_IAWPQ)       ADD TO WORK POOL                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(LD_VSTRQ)       DATA TYPE IS VARIABLE STRING                 
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#MED)  PRINT SYSTEM, LITERAL                  
         DC    XL4'00'                                                          
*                                                                               
DW#CLTCD EQU   6                                                                
         DC    AL2(D#CLTCOD)       CLIENT CODE                                  
         DC    CL5'CLT  '          TITLE                                        
         DC    AL1(DW#CLTCD)       SEQUENTIAL FIELD NUMBER                      
         DC    AL1(LD_IAWPQ)       ADD TO WORK POOL                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(LD_VSTRQ)       DATA TYPE IS VARIABLE STRING                 
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#CLT)  PRINT SYSTEM, LITERAL                  
         DC    XL4'00'                                                          
*                                                                               
DW#PRDCD EQU   7                                                                
         DC    AL2(D#PRDCOD)       PRODUCT CODE                                 
         DC    CL5'PRD  '          TITLE                                        
         DC    AL1(DW#PRDCD)       SEQUENTIAL FIELD NUMBER                      
         DC    AL1(LD_IAWPQ)       ADD TO WORK POOL                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(LD_VSTRQ)       DATA TYPE IS VARIABLE STRING                 
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#PRD)  PRINT SYSTEM, LITERAL                  
         DC    XL4'00'                                                          
*                                                                               
DW#PUBC  EQU   8                                                                
         DC    AL2(D#PUBCOD)       PUB MAP CODE                                 
         DC    CL5'PUBCD'          TITLE                                        
         DC    AL1(DW#PUBC)        SEQUENTIAL FIELD NUMBER                      
         DC    AL1(LD_IAWPQ)       ADD TO WORK POOL                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(LD_VSTRQ)       DATA TYPE IS VARIABLE STRING                 
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#PUBCD)  PRINT SYSTEM, LITERAL                
         DC    XL4'00'                                                          
*                                                                               
DW#PER   EQU   9                                                                
         DC    AL2(D#STEND)        PERIOD MAP CODE                              
         DC    CL5'PER  '          TITLE                                        
         DC    AL1(DW#PER)         SEQUENTIAL FIELD NUMBER                      
         DC    AL1(LD_IAWPQ)       ADD TO WORK POOL                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(LD_VSTRQ)       DATA TYPE IS VARIABLE STRING                 
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#PERD)  PRINT SYSTEM, LITERAL                 
         DC    XL4'00'                                                          
*                                                                               
DW#EOR   EQU   10                                                               
         DC    AL2(D#EOR)          DATA TO BE REPLIED                           
         DC    CL5'EOR  '          TITLE                                        
         DC    AL1(DW#EOR)         SEQUENTIAL FIELD NUMBER                      
         DC    AL1(LD_IAWPQ)       ADD TO WORK POOL                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(1)              DATA LENGTH                                  
         DC    AL1(LD_CHARQ)       DATA TYPE IS CHARACTER                       
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#DATA)  PRINT SYSTEM, LITERAL                 
         DC    XL4'00'                                                          
*                                                                               
REQIODWX DC    AL1(LD_EOTQ)                                                     
*                                                                               
***********************************************************************         
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
**PAN#1  DC    CL21'006PPLNK18   12/17/07'                                      
         END                                                                    

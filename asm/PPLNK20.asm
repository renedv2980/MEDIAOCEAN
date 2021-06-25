*          DATA SET PPLNK20    AT LEVEL 013 AS OF 12/17/07                      
*PHASE T41420A                                                                  
PPLNK20  TITLE '- ESR UPLOAD MAPS'                                              
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
         DC    C'SU'               PROGRAM                                      
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
         DC    C'PRI',C'ESR'       TO SYSTEM/PROGRAM                            
         ORG                                                                    
         EJECT                                                                  
REQUEST  DS    0X                                                               
*                                                                               
***********************************************************************         
* REQUEST MAP FOR DELETE SPACE RESERVATION RECORD                     *         
***********************************************************************         
*                                                                               
REQSRDL  DS    0XL(LH_LNQ)                                                      
         DC    AL2(REQSRDLX+1-*)   END OF TABLE                                 
         DC    AL2(M#ULSRDL)       REQUEST INDENTIFER - DELETE IO               
         DC    AL1(LH_IANUR)       ADD NEW UPLOAD RECORD FOR THIS MAP           
         DC    AL2(0)              NO OUTPUT MAP ASSOCIATED                     
         DC    XL4'00'             VERSION NUMBER                               
*                                                                               
DI#SRKY  EQU   1                                                                
         DC    AL2(D#ESRLKY)       LONG IO KEY MAP CODE                         
         DC    CL5'SRKEY'          TITLE                                        
         DC    AL1(DI#SRKY)        SEQUENTIAL FIELD NUMBER                      
         DC    AL1(LD_IAWPQ)       ADD TO WORK POOL                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(22)             DATA LENGTH                                  
         DC    AL1(LD_CHARQ)       DATA TYPE IS CHARACTER                       
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#LSRKY)  PRINT SYSTEM, LITERAL                
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
REQSRDLX DC    AL1(LD_EOTQ)                                                     
         EJECT                                                                  
*                                                                               
***********************************************************************         
* REQUEST MAP FOR ADD STATUS TO SPACE RESERVATION RECORD              *         
***********************************************************************         
*                                                                               
REQSRST  DS    0XL(LH_LNQ)                                                      
         DC    AL2(REQSRSTX+1-*)   END OF TABLE                                 
         DC    AL2(M#ULSRST)       REQUEST INDENTIFER - STATUS ADD              
         DC    AL1(LH_IANUR)       ADD NEW UPLOAD RECORD FOR THIS MAP           
         DC    AL2(0)              NO OUTPUT MAP ASSOCIATED                     
         DC    XL4'00'             VERSION NUMBER                               
*                                                                               
ST#SRKY  EQU   1                                                                
         DC    AL2(D#ESRLKY)       LONG IO KEY MAP CODE                         
         DC    CL5'SRKEY'          TITLE                                        
         DC    AL1(ST#SRKY)        SEQUENTIAL FIELD NUMBER                      
         DC    AL1(LD_IAWPQ)       ADD TO WORK POOL                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(22)             DATA LENGTH                                  
         DC    AL1(LD_CHARQ)       DATA TYPE IS CHARACTER                       
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#LSRKY)  PRINT SYSTEM, LITERAL                
         DC    XL4'00'                                                          
*                                                                               
ST#HSTID EQU   2                                                                
         DC    AL2(D#HSTYID)       PUB MAP CODE                                 
         DC    CL5'HSTID'          TITLE                                        
         DC    AL1(ST#HSTID)       SEQUENTIAL FIELD NUMBER                      
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
REQSRSTX DC    AL1(LD_EOTQ)                                                     
         EJECT                                                                  
*                                                                               
***********************************************************************         
* REQUEST MAP FOR SENDING FAX                                         *         
***********************************************************************         
*                                                                               
REQSRFX  DS    0XL(LH_LNQ)                                                      
         DC    AL2(REQSRFXX+1-*)   END OF TABLE                                 
         DC    AL2(M#ULSRFX)       REQUEST INDENTIFER - STATUS ADD              
         DC    AL1(LH_IANUR)       ADD NEW UPLOAD RECORD FOR THIS MAP           
         DC    AL2(0)              NO OUTPUT MAP ASSOCIATED                     
         DC    XL4'00'             VERSION NUMBER                               
*                                                                               
FX#SRKY  EQU   1                                                                
         DC    AL2(D#ESRLKY)       LONG IO KEY MAP CODE                         
         DC    CL5'SRKEY'          TITLE                                        
         DC    AL1(FX#SRKY)        SEQUENTIAL FIELD NUMBER                      
         DC    AL1(LD_IAWPQ)       ADD TO WORK POOL                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(22)             DATA LENGTH                                  
         DC    AL1(LD_CHARQ)       DATA TYPE IS CHARACTER                       
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#LSRKY)  PRINT SYSTEM, LITERAL                
         DC    XL4'00'                                                          
*                                                                               
FX#HSTID EQU   2                                                                
         DC    AL2(D#HSTYID)       PUB MAP CODE                                 
         DC    CL5'HSTID'          TITLE                                        
         DC    AL1(FX#HSTID)       SEQUENTIAL FIELD NUMBER                      
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
         DC    AL1(PRTSYSQ),AL2(PP#HSTID)  PRINT SYSTEM, LITERAL                
         DC    XL4'00'                                                          
*                                                                               
FX#PUBC  EQU   3                                                                
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
FX#MANSD EQU   4                                                                
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
FX#EADDR EQU   5                                                                
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
FX#FAXNM EQU   6                                                                
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
FX#FAXTP EQU   7                                                                
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
FX#FAX#  EQU   8                                                                
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
FX#SPCST EQU   9                                                                
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
REQSRFXX DC    AL1(LD_EOTQ)                                                     
*                                                                               
***********************************************************************         
* REQUEST MAP FOR FAX LINE OF TEXT                                    *         
***********************************************************************         
*                                                                               
REQSRFL  DS    0XL(LH_LNQ)                                                      
         DC    AL2(REQSRFLX+1-*)   END OF TABLE                                 
         DC    AL2(E#SRFXLN)       REQUEST INDENTIFER - FAX TEXT                
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
REQSRFLX DC    AL1(LD_EOTQ)                                                     
*                                                                               
***********************************************************************         
* REQUEST MAP FOR SPACE RESERVATION STATUS INQUIRY                    *         
***********************************************************************         
*                                                                               
REQSRIQ  DS    0XL(LH_LNQ)                                                      
         DC    AL2(REQSRIQX+1-*)   END OF TABLE                                 
         DC    AL2(M#ULSRIQ)       REQUEST INDENTIFER - STATUS INQUIRY          
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
IQ#SRKY  EQU   5                                                                
         DC    AL2(D#ESRLKY)       LONG IO KEY MAP CODE                         
         DC    CL5'SRKEY'          TITLE                                        
         DC    AL1(IQ#SRKY)        SEQUENTIAL FIELD NUMBER                      
         DC    AL1(LD_IAWPQ)       ADD TO WORK POOL                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(22)             DATA LENGTH                                  
         DC    AL1(LD_CHARQ)       DATA TYPE IS CHARACTER                       
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#LSRKY)  PRINT SYSTEM, LITERAL                
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
REQSRIQX DC    AL1(LD_EOTQ)                                                     
*                                                                               
***********************************************************************         
* REQUEST MAP FOR SPACE RESERVATION DOWNLOAD                          *         
***********************************************************************         
*                                                                               
REQSRDW  DS    0XL(LH_LNQ)                                                      
         DC    AL2(REQSRDWX+1-*)   END OF TABLE                                 
         DC    AL2(M#ULSRDW)       REQUEST INDENTIFER - IO DOWNLOAD             
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
REQSRDWX DC    AL1(LD_EOTQ)                                                     
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
**PAN#1  DC    CL21'013PPLNK20   12/17/07'                                      
         END                                                                    

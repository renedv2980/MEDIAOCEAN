*          DATA SET SPFIN01    AT LEVEL 052 AS OF 11/14/06                      
*PHASE T20901A,*                                                                
         TITLE 'SPFIN01 - NEW FINANCIAL INFORMATION -CONTROLLER TABLES'         
T20901   CSECT                                                                  
         SPACE 2                                                                
ARECTAB  DC    AL4(RECTAB-T20901)  RECORD TABLE                                 
AACTTAB  DC    AL4(ACTTAB-T20901)  ACTION TABLE                                 
AMIXTAB  DC    AL4(MIXTAB-T20901)  REC/ACT MIX TABLE                            
AKEYTAB  DC    AL4(KEYTAB-T20901)  KEY COMPONENT TABLE                          
AOPTTAB  DC    AL4(OPTTAB-T20901)  OPTION TABLE                                 
ASELTAB  DC    AL4(SELTAB-T20901)  SELECTABLE ACTION TABLE                      
AOPTVAL  DC    AL4(TBASE-T20901)   OPTION VALIDATION TABLES                     
         EJECT                                                                  
************************************                                            
* RECORD TYPE TABLE (SEE RECTABD)  *                                            
************************************                                            
*                                                                               
RECTAB   DS    0X                                                               
*                                                                               
RFIN     DC    AL1(RFINX-RFIN)                                                  
         DC    C'FIS     ',C'FIS',AL1(0)                                        
         DC    AL1(0,0,RECFIN)                                                  
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
RFINX    EQU   *                                                                
*                                                                               
RECTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
********************************                                                
* ACTION TABLE (SEE ACTTABD)   *                                                
********************************                                                
*                                                                               
ACTTAB   DS    0X                                                               
*                                                                               
         DC    C'DISPLAY ',AL1(0,0,0,ACTLFM8)                                   
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DC    C'LIST    ',AL1(0,0,0,ACTLIS)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DC    C'SELECT  ',AL1(0,0,0,ACTSEL)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
ACTTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
*******************************************                                     
* RECORD/ACTION COMBO TABLE (SEE MIXTABD) *                                     
*******************************************                                     
*                                                                               
MIXTAB   DS    0X                                                               
*                                                                               
MFINDIS  DC    AL1(MFINDISX-MFINDIS)                                            
         DC    AL1(RECFIN,ACTLFM8)                                              
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    AL1(0,0)                                                         
         DC    X'FE02'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MFINDISX EQU   *                                                                
*                                                                               
MFINLST  DC    AL1(MFINLSTX-MFINLST)                                            
         DC    AL1(RECFIN,ACTLIS)                                               
         DC    AL1(MIXILST,0)                                                   
         DC    AL1(0,0)                                                         
         DC    X'FD03'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
*********DC    AL4(0,OPTWKB)                                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MFINLSTX EQU   *                                                                
*                                                                               
MFINSEL  DC    AL1(MFINSELX-MFINSEL)                                            
         DC    AL1(RECFIN,ACTSEL)                                               
         DC    AL1(MIXISEL,0)                                                   
         DC    AL1(0,0)                                                         
         DC    X'FE03'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MFINSELX EQU   *                                                                
*                                                                               
MIXTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
**************************************                                          
* KEY COMPONENT TABLE (SEE KEYTABD)  *                                          
**************************************                                          
*                                                                               
KEYTAB   DS    0X                                                               
         DC    C'MED    ',AL1(KEYMED)                                           
         DC    C'CLIENT ',AL1(KEYCLT)                                           
         DC    C'PRODUCT',AL1(KEYPRD)                                           
         DC    C'EST    ',AL1(KEYEST)                                           
         DC    C'MKT/STA',AL1(KEYMOS)                                           
         DC    C'DATES  ',AL1(KEYDTS)                                           
KEYTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
********************************                                                
* OPTION TABLE (SEE OPTTABD)   *                                                
********************************                                                
*                                                                               
OPTTAB   DS    0X                                                               
*                                                                               
NETOPT   DC    AL1(NETOPTX-NETOPT)                                              
         DC    C'NET     ',C'   ',AL1(OPTATAB+OPTBOOL+OPTITXT,0)                
         DC    AL1(0,0)                                                         
         DC    AL1(0,0)                                                         
         DC    AL1(3,0,0,L'INOIND)                                              
         DC    AL4(OPTNETB)                                                     
         DC    AL1(OPTNETN)                                                     
         DC    AL2(VALNET-TBASE)                                                
         DC    AL2(INOIND-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(0)                                                           
NETOPTX  EQU   *                                                                
*                                                                               
SPLOPT   DC    AL1(SPLOPTX-SPLOPT)                                              
         DC    C'SPL     ',C'   ',AL1(OPTATAB+OPTTABH+OPTITXT,0)                
         DC    AL1(0,0)                                                         
         DC    AL1(0,0)                                                         
         DC    AL1(3,1,3,L'INOSPL)                                              
         DC    AL4(OPTSPLB)                                                     
         DC    AL1(OPTSPLN)                                                     
         DC    AL2(VALSPL-TBASE)                                                
         DC    AL2(INOSPL-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(0)                                                           
SPLOPTX  EQU   *                                                                
*                                                                               
REPOPT   DC    AL1(REPOPTX-REPOPT)                                              
         DC    C'SREP    ',C'   ',AL1(OPTNRTN+OPTITXT,0)                        
         DC    AL1(0,0)                                                         
         DC    AL1(0,0)                                                         
         DC    AL1(2,1,3,L'INOREP)                                              
         DC    AL4(OPTREPB)                                                     
         DC    AL1(OPTREPN)                                                     
         DC    AL2(OPTREPR)                                                     
         DC    AL2(INOREP-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(OPTBILB)                                                     
         DC    AL2(0)                                                           
REPOPTX  EQU   *                                                                
*                                                                               
CANOPT   DC    AL1(CANOPTX-CANOPT)                                              
         DC    C'$C      ',C'   ',AL1(OPTATAB+OPTBOOL+OPTITXT,0)                
         DC    AL1(0,0)                                                         
         DC    AL1(0,0)                                                         
         DC    AL1(2,0,0,L'INOIND)                                              
         DC    AL4(OPTCANB)                                                     
         DC    AL1(OPTCANN)                                                     
         DC    AL2(VALCAN-TBASE)                                                
         DC    AL2(INOIND-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(0)                                                           
CANOPTX  EQU   *                                                                
*                                                                               
USAOPT   DC    AL1(USAOPTX-USAOPT)                                              
         DC    C'$U      ',C'   ',AL1(OPTATAB+OPTBOOL+OPTITXT,0)                
         DC    AL1(0,0)                                                         
         DC    AL1(0,0)                                                         
         DC    AL1(2,0,0,L'INOIND)                                              
         DC    AL4(OPTUSAB)                                                     
         DC    AL1(OPTUSAN)                                                     
         DC    AL2(VALUSA-TBASE)                                                
         DC    AL2(INOIND-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(0)                                                           
USAOPTX  EQU   *                                                                
*                                                                               
PREOPT   DC    AL1(PREOPTX-PREOPT)                                              
         DC    C'PREVIOUS',C'   ',AL1(OPTATAB+OPTBOOL+OPTITXT,0)                
         DC    AL1(0,0)                                                         
         DC    AL1(0,0)                                                         
         DC    AL1(3,0,0,L'INOIND)                                              
         DC    AL4(OPTPREB)                                                     
         DC    AL1(OPTPREN)                                                     
         DC    AL2(VALPRE-TBASE)                                                
         DC    AL2(INOIND-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(0)                                                           
PREOPTX  EQU   *                                                                
*                                                                               
POSOPT   DC    AL1(POSOPTX-POSOPT)                                              
         DC    C'POST    ',C'   ',AL1(OPTATAB+OPTBOOL+OPTITXT,0)                
         DC    AL1(0,0)                                                         
         DC    AL1(0,0)                                                         
         DC    AL1(3,0,0,L'INOIND)                                              
         DC    AL4(OPTPOSB)                                                     
         DC    AL1(OPTPOSN)                                                     
         DC    AL2(VALPOS-TBASE)                                                
         DC    AL2(INOIND-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(0)                                                           
POSOPTX  EQU   *                                                                
*                                                                               
WKOPT    DC    AL1(WKOPTX-WKOPT)                                                
         DC    C'WKLY    ',C'   ',AL1(OPTATAB+OPTBOOL+OPTITXT,0)                
         DC    AL1(0,0)                                                         
         DC    AL1(0,0)                                                         
         DC    AL1(4,0,0,L'INOIND)                                              
         DC    AL4(OPTWKB)                                                      
         DC    AL1(OPTWKN)                                                      
         DC    AL2(VALWK-TBASE)                                                 
         DC    AL2(INOIND-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(OPTBILB)                                                     
         DC    AL2(0)                                                           
WKOPTX   EQU   *                                                                
*                                                                               
AFFOPT   DC    AL1(AFFOPTX-AFFOPT)                                              
         DC    C'AFF     ',C'   ',AL1(OPTNRTN+OPTITXT,0)                        
         DC    AL1(0,0)                                                         
         DC    AL1(0,0)                                                         
         DC    AL1(3,3,4,L'INOAFF)                                              
         DC    AL4(OPTAFFB)                                                     
         DC    AL1(OPTAFFN)                                                     
         DC    AL2(OPTAFFR)                                                     
         DC    AL2(INOAFF-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(0)                                                           
AFFOPTX  EQU   *                                                                
*                                                                               
BRDOPT   DC    AL1(BRDOPTX-BRDOPT)                                              
         DC    C'BRD     ',C'   ',AL1(OPTATAB+OPTBOOL+OPTITXT,0)                
         DC    AL1(0,0)                                                         
         DC    AL1(0,0)                                                         
         DC    AL1(2,0,0,L'INOIND)                                              
         DC    AL4(OPTBRDB)                                                     
         DC    AL1(OPTBRDN)                                                     
         DC    AL2(VALBRD-TBASE)                                                
         DC    AL2(INOIND-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(0)                                                           
BRDOPTX  EQU   *                                                                
*                                                                               
BILOPT   DC    AL1(BILOPTX-BILOPT)                                              
         DC    C'BILL    ',C'   ',AL1(OPTATAB+OPTBOOL+OPTITXT,0)                
         DC    AL1(0,0)                                                         
         DC    AL1(0,0)                                                         
         DC    AL1(4,0,0,L'INOIND2)                                             
         DC    AL4(OPTBILB)                                                     
         DC    AL1(OPTBILN)                                                     
         DC    AL2(VALBIL-TBASE)                                                
         DC    AL2(INOIND2-WORKD)                                               
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(OPTREPB+OPTWKB)                                              
         DC    AL2(0)                                                           
BILOPTX  EQU   *                                                                
*                                                                               
MKTOPT   DC    AL1(MKTOPTX-MKTOPT)                                              
         DC    C'MARKET  ',C'MKT',AL1(OPTNRTN+OPTITXT,0)                        
         DC    AL1(0,0)                                                         
         DC    AL1(0,0)                                                         
         DC    AL1(3,1,4,L'INOMKT)                                              
         DC    AL4(OPTMKTB)                                                     
         DC    AL1(OPTMKTN)                                                     
         DC    AL2(OPTMKTR)                                                     
         DC    AL2(INOMKT-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(0)                                                           
MKTOPTX  EQU   *                                                                
*                                                                               
COS2OPT  DC    AL1(COS2OPTX-COS2OPT)                                            
         DC    C'COS2    ',C'   ',AL1(OPTATAB+OPTBOOL+OPTITXT,0)                
         DC    AL1(0,0)                                                         
         DC    AL1(0,0)                                                         
         DC    AL1(3,0,0,L'INOIND2)                                             
         DC    AL4(OPTCOS2B)                                                    
         DC    AL1(OPTCOS2N)                                                    
         DC    AL2(VALCOS2-TBASE)                                               
         DC    AL2(INOIND2-WORKD)                                               
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(0)                                                           
COS2OPTX EQU   *                                                                
*                                                                               
C2OPT    DC    AL1(C2OPTX-C2OPT)                                                
         DC    C'C2      ',C'   ',AL1(OPTATAB+OPTBOOL+OPTITXT,0)                
         DC    AL1(0,0)                                                         
         DC    AL1(0,0)                                                         
         DC    AL1(2,0,0,L'INOIND2)                                             
         DC    AL4(OPTCOS2B)                                                    
         DC    AL1(OPTCOS2N)                                                    
         DC    AL2(VALCOS2-TBASE)                                               
         DC    AL2(INOIND2-WORKD)                                               
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(0)                                                           
C2OPTX   EQU   *                                                                
*                                                                               
TRDOPT   DC    AL1(TRDOPTX-TRDOPT)                                              
         DC    C'TRD     ',C'   ',AL1(OPTNRTN+OPTITXT+OPTBOOL,0)                
         DC    AL1(0,0)                                                         
         DC    AL1(0,0)                                                         
         DC    AL1(3,1,1,L'INOIND2)                                             
         DC    AL4(OPTTRDB)                                                     
         DC    AL1(OPTTRDN)                                                     
         DC    AL2(OPTTRDR)                                                     
         DC    AL2(INOIND2-WORKD)                                               
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(0)                                                           
TRDOPTX  EQU   *                                                                
*                                                                               
OPTTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* OPTION VALIDATION TABLES                                            *         
***********************************************************************         
         SPACE 1                                                                
TBASE    DS    0H                  ** TABLE BASE **                             
*                                                                               
* VALIDATE NET DOLLAR OPTION                                                    
*                                                                               
VALNET   DS    0H                                                               
         DC    X'0101'                                                          
         DC    C' ',AL1(INOINET)                                                
         DC    X'00'                                                            
*                                                                               
* VALIDATE SPLIT PIGGYBACK OPTION                                               
*                                                                               
VALSPL   DS    CL60'YES, NO'                                                    
         DC    X'0301'                                                          
         DC    CL3'YES',CL1'Y'                                                  
         DC    CL3'NO ',CL1'N'                                                  
         DC    X'00'                                                            
*                                                                               
* VALIDATE CANADIAN DOLLAR OPTION                                               
*                                                                               
VALCAN   DS    0H                                                               
         DC    X'0101'                                                          
         DC    C' ',AL1(INOICAN)                                                
         DC    X'00'                                                            
*                                                                               
* VALIDATE USA DOLLAR OPTION                                                    
*                                                                               
VALUSA   DS    0H                                                               
         DC    X'0101'                                                          
         DC    C' ',AL1(INOIUSA)                                                
         DC    X'00'                                                            
*                                                                               
* VALIDATE PREVIOUS TO START DATES                                              
*                                                                               
VALPRE   DS    0H                                                               
         DC    X'0101'                                                          
         DC    C' ',AL1(INOIPRE)                                                
         DC    X'00'                                                            
*                                                                               
* VALIDATE POST TO END DATES                                                    
*                                                                               
VALPOS   DS    0H                                                               
         DC    X'0101'                                                          
         DC    C' ',AL1(INOIPOS)                                                
         DC    X'00'                                                            
*                                                                               
* VALIDATE WEEKLY DATES OPTION                                                  
*                                                                               
VALWK    DS    0H                                                               
         DC    X'0101'                                                          
         DC    C' ',AL1(INOIWK)                                                 
         DC    X'00'                                                            
*                                                                               
* VALIDATE BRD OPTION                                                           
*                                                                               
VALBRD   DS    0H                                                               
         DC    X'0101'                                                          
         DC    C' ',AL1(INOIBRD)                                                
         DC    X'00'                                                            
*                                                                               
* VALIDATE BILL OPTION                                                          
*                                                                               
VALBIL   DS    0H                                                               
         DC    X'0101'                                                          
         DC    C' ',AL1(INOIBIL)                                                
         DC    X'00'                                                            
*                                                                               
* VALIDATE COST2 VALUES                                                         
*                                                                               
VALCOS2  DS    0H                                                               
         DC    X'0101'                                                          
         DC    C' ',AL1(INOICOS2)                                               
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* SELECTABLE ACTION TABLE (SEE SELTABD)                               *         
***********************************************************************         
         SPACE 1                                                                
SELTAB   DS    0X                                                               
         DC    C'S',AL1(RECFIN,ACTLIS,RECFIN,ACTLFM8)                           
SELTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
* SPFINWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPFINWRK                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'052SPFIN01   11/14/06'                                      
         END                                                                    

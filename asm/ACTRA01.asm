*          DATA SET ACTRA01    AT LEVEL 014 AS OF 03/22/13                      
*PHASE T62201A,*                                                                
         TITLE 'ACTRA01 - T62201 BILLING TRANSFER -CONTROLLER TABLES'           
T62201   CSECT                                                                  
         SPACE 2                                                                
ARECTAB  DC    AL4(RECTAB-T62201)  RECORD TABLE                                 
AACTTAB  DC    AL4(ACTTAB-T62201)  ACTION TABLE                                 
AMIXTAB  DC    AL4(MIXTAB-T62201)  REC/ACT MIX TABLE                            
AKEYTAB  DC    AL4(KEYTAB-T62201)  KEY COMPONENT TABLE                          
AOPTTAB  DC    AL4(OPTTAB-T62201)  OPTION TABLE                                 
ASELTAB  DC    AL4(SELTAB-T62201)  SELECTABLE ACTION TABLE                      
AOPTVAL  DC    AL4(TBASE-T62201)   OPTION VALIDATION TABLES                     
         EJECT                                                                  
************************************                                            
* RECORD TYPE TABLE (SEE RECTABD)  *                                            
************************************                                            
*                                                                               
RECTAB   DS    0X                                                               
*                                                                               
* -- REGULAR                                                                    
*                                                                               
RPOST    DC    AL1(RPOSTX-RPOST)                                                
         DC    C'POST    ',C'POS',AL1(0)                                        
         DC    AL1(0,0,RECPOST)                                                 
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
RPOSTX   EQU   *                                                                
*                                                                               
*                                                                               
* -- DIFF                                                                       
*                                                                               
RDPOST   DC    AL1(RDPOSTX-RDPOST)                                              
         DC    C'DPOST   ',C'DIF',AL1(0)                                        
         DC    AL1(0,0,RECDPOST)                                                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
RDPOSTX  EQU   *                                                                
*                                                                               
RPROF    DC    AL1(RPROFX-RPROF)                                                
         DC    C'PROF    ',C'PRO',AL1(0)                                        
         DC    AL1(0,0,RECPROF)                                                 
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
RPROFX   EQU   *                                                                
*                                                                               
* -- AOR                                                                        
*                                                                               
RAPOST   DC    AL1(RAPOSTX-RAPOST)                                              
         DC    C'APOST   ',C'APO',AL1(0)                                        
         DC    AL1(0,0,RECAPOST)                                                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
RAPOSTX  EQU   *                                                                
*                                                                               
* -- AOR2                                                                       
*                                                                               
RAPOS2   DC    AL1(RAPOS2X-RAPOS2)                                              
         DC    C'APOS2   ',C'AP2',AL1(0)                                        
         DC    AL1(0,0,RECAPOS2)                                                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
RAPOS2X  EQU   *                                                                
*                                                                               
* -- UPFRONT COMMISSION AND NET BILLS                                           
*                                                                               
RUCPOST  DC    AL1(RUCPOSTX-RUCPOST)                                            
         DC    C'UCPOST  ',C'UNC',AL1(0)                                        
         DC    AL1(0,0,RECUCPST)                                                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
RUCPOSTX EQU   *                                                                
*                                                                               
RUNPOST  DC    AL1(RUNPOSTX-RUNPOST)                                            
         DC    C'UNPOST  ',C'UNP',AL1(0)                                        
         DC    AL1(0,0,RECUNPST)                                                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
RUNPOSTX EQU   *                                                                
*                                                                               
* -- AOR UPFRONT COMMISSION AND NET BILLS                                       
*                                                                               
RUACPST  DC    AL1(RUACPSTX-RUACPST)                                            
         DC    C'UACPOST ',C'UAC',AL1(0)                                        
         DC    AL1(0,0,RECUACPT)                                                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
RUACPSTX EQU   *                                                                
*                                                                               
RUANPST  DC    AL1(RUANPSTX-RUANPST)                                            
         DC    C'UANPOST ',C'UAN',AL1(0)                                        
         DC    AL1(0,0,RECUANPT)                                                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
RUANPSTX EQU   *                                                                
*                                                                               
* -- RETAIL                                                                     
*                                                                               
RRPOST   DC    AL1(RRPOSTX-RRPOST)                                              
         DC    C'RPOST   ',C'RPO',AL1(0)                                        
         DC    AL1(0,0,RECRPOST)                                                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
RRPOSTX  EQU   *                                                                
*                                                                               
RRPROF   DC    AL1(RRPROFX-RRPROF)                                              
         DC    C'RPROF   ',C'RPR',AL1(0)                                        
         DC    AL1(0,0,RECRPROF)                                                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
RRPROFX  EQU   *                                                                
*                                                                               
BRPOST   DC    AL1(BRPOSTX-BRPOST)                                              
         DC    C'BPOST   ',C'BPO',AL1(0)                                        
         DC    AL1(0,0,RECBPOST)                                                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
BRPOSTX  EQU   *                                                                
*                                                                               
* -- PST                                                                        
*                                                                               
RTPOST   DC    AL1(RTPOSTX-RTPOST)                                              
         DC    C'TPOST   ',C'TPO',AL1(0)                                        
         DC    AL1(0,0,RECTPOST)                                                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
RTPOSTX  EQU   *                                                                
*                                                                               
*                                                                               
* --  PRINT PROD & FINANCIAL                                                    
*                                                                               
RSPOST   DC    AL1(RSPOSTX-RSPOST)                                              
         DC    C'SPOST   ',C'SPO',AL1(0)                                        
         DC    AL1(0,0,RECSPOST)                                                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
RSPOSTX  EQU   *                                                                
*                                                                               
RSPROF   DC    AL1(RSPROFX-RSPROF)                                              
         DC    C'SPROF   ',C'SPR',AL1(0)                                        
         DC    AL1(0,0,RECSPROF)                                                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
RSPROFX  EQU   *                                                                
*                                                                               
*-- BILL                                                                        
*                                                                               
RBIL     DC    AL1(RBILX-RBIL)                                                  
         DC    C'BILL    ',C'BIL',AL1(0)                                        
         DC    AL1(0,0,RECBILL)                                                 
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
RBILX    EQU   *                                                                
*                                                                               
*-- XPROF                                                                       
*                                                                               
RXPROF   DC    AL1(RXPROFX-RXPROF)                                              
         DC    C'XPROF   ',C'XPR',AL1(0)                                        
         DC    AL1(0,0,RECXPROF)                                                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
RXPROFX  EQU   *                                                                
*                                                                               
*-- XPOST                                                                       
*                                                                               
RXPOST   DC    AL1(RXPOSTX-RXPOST)                                              
         DC    C'XPOST   ',C'XPO',AL1(0)                                        
         DC    AL1(0,0,RECXPST)                                                 
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
RXPOSTX  EQU   *                                                                
*                                                                               
*-- XAPOST                                                                      
*                                                                               
RXAPST   DC    AL1(RXAPSTX-RXAPST)                                              
         DC    C'XAPOST  ',C'XAP',AL1(0)                                        
         DC    AL1(0,0,RECXAPST)                                                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
RXAPSTX  EQU   *                                                                
*                                                                               
*-- XAPOS2                                                                      
*                                                                               
RXAPS2   DC    AL1(RXAPS2X-RXAPS2)                                              
         DC    C'XAPOS2  ',C'XA2',AL1(0)                                        
         DC    AL1(0,0,RECXAPS2)                                                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
RXAPS2X  EQU   *                                                                
*                                                                               
*                                                                               
*-- MPROF                                                                       
*                                                                               
RMPROF   DC    AL1(RMPROFX-RMPROF)                                              
         DC    C'MPROF   ',C'MPR',AL1(0)                                        
         DC    AL1(0,0,RECMPROF)                                                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
RMPROFX  EQU   *                                                                
*                                                                               
*-- MPOST                                                                       
*                                                                               
RMPOST   DC    AL1(RMPOSTX-RMPOST)                                              
         DC    C'MPOST   ',C'MPO',AL1(0)                                        
         DC    AL1(0,0,RECMPST)                                                 
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
RMPOSTX  EQU   *                                                                
*                                                                               
RECTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
********************************                                                
* ACTION TABLE (SEE ACTTABD)   *                                                
********************************                                                
*                                                                               
ACTTAB   DS    0X                                                               
*                                                                               
         DC    C'MAINT   ',AL1(0,0,0,ACTMAI)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DC    C'MAINT   ',AL1(0,0,0,ACTMNT)                                    
         DC    AL4(0,0)                                                         
         DC    AL1(ACTMAI),XL7'00'                                              
*                                                                               
         DC    C'TRACE   ',AL1(0,0,0,ACTRACE)                                   
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DC    C'TRACE   ',AL1(0,0,0,ACTTRC)                                    
         DC    AL4(0,0)                                                         
         DC    AL1(ACTRACE),XL7'00'                                             
*                                                                               
         DC    C'DISPLAY ',AL1(0,0,0,ACTBDIS)                                   
         DC    AL4(0,0)                                                         
         DC    AL1(ACTDIS),XL7'00'                                              
*                                                                               
         DC    C'DISPLAY ',AL1(0,0,0,ACTDIS2)                                   
         DC    AL4(0,0)                                                         
         DC    AL1(ACTBDIS),XL7'00'                                             
*                                                                               
         DC    C'LIST    ',AL1(0,0,0,ACTLIS)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DC    C'SELECT  ',AL1(0,0,0,ACTSEL)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DC    C'REPORT  ',AL1(0,0,0,ACTREP)                                    
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
* -- REGULAR POST MAINTS                                                        
*                                                                               
MPOSTD   DC    AL1(MPOSTDX-MPOSTD)                                              
         DC    AL1(RECPOST,ACTMAI)                                              
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'F902'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MPOSTDX  EQU   *                                                                
*                                                                               
MPOSTD2  DC    AL1(MPOSTD2X-MPOSTD2)                                            
         DC    AL1(RECPOST,ACTMAI)                                              
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'F902'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MPOSTD2X EQU   *                                                                
*                                                                               
MPOSTM   DC    AL1(MPOSTMX-MPOSTM)                                              
         DC    AL1(RECPOST,ACTMNT)                                              
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'F902'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MPOSTMX  EQU   *                                                                
*                                                                               
MPOSTR   DC    AL1(MPOSTRX-MPOSTR)                                              
         DC    AL1(RECPOST,ACTREP)                                              
         DC    AL1(MIXIREP,MIXIOKS)                                             
         DC    X'0000'                                                          
         DC    X'F809'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    CL8'ACPXPX'                                                      
         DC    AL2(0)                                                           
MPOSTRX  EQU   *                                                                
         EJECT                                                                  
*                                                                               
* -- REGULAR PROF MAINTS                                                        
*                                                                               
MPROFD   DC    AL1(MPROFDX-MPROFD)                                              
         DC    AL1(RECPROF,ACTMAI)                                              
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FB05'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MPROFDX  EQU   *                                                                
*                                                                               
MPROFD2  DC    AL1(MPROFD2X-MPROFD2)                                            
         DC    AL1(RECPROF,ACTMAI)                                              
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'FB05'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MPROFD2X EQU   *                                                                
*                                                                               
MPROFM   DC    AL1(MPROFMX-MPROFM)                                              
         DC    AL1(RECPROF,ACTMNT)                                              
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FB05'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MPROFMX  EQU   *                                                                
                                                                                
MPROFR   DC    AL1(MPROFRX-MPROFR)                                              
         DC    AL1(RECPROF,ACTREP)                                              
         DC    AL1(MIXIREP,MIXIOKS)                                             
         DC    X'0000'                                                          
         DC    X'F80A'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    CL8'ACPXPX'                                                      
         DC    AL2(0)                                                           
MPROFRX  EQU   *                                                                
                                                                                
                                                                                
                                                                                
         EJECT                                                                  
*                                                                               
* -- DPOST MAINTS                                                               
*                                                                               
MDPOSTD  DC    AL1(MDPOSTDX-MDPOSTD)                                            
         DC    AL1(RECDPOST,ACTMAI)                                             
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FE02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MDPOSTDX EQU   *                                                                
*                                                                               
MDPSTD2  DC    AL1(MDPSTD2X-MDPSTD2)                                            
         DC    AL1(RECDPOST,ACTMAI)                                             
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'FE02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MDPSTD2X EQU   *                                                                
*                                                                               
MDPOSTM  DC    AL1(MDPOSTMX-MDPOSTM)                                            
         DC    AL1(RECDPOST,ACTMNT)                                             
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FE02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MDPOSTMX EQU   *                                                                
         EJECT                                                                  
*                                                                               
* -- UPFRONT COMMISSION POST MAINT                                              
*                                                                               
MCPOSTD  DC    AL1(MCPOSTDX-MCPOSTD)                                            
         DC    AL1(RECUCPST,ACTMAI)                                             
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FE02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MCPOSTDX EQU   *                                                                
*                                                                               
MCPSTD2  DC    AL1(MCPSTD2X-MCPSTD2)                                            
         DC    AL1(RECUCPST,ACTMAI)                                             
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'FE02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MCPSTD2X EQU   *                                                                
*                                                                               
MCPOSTM  DC    AL1(MCPOSTMX-MCPOSTM)                                            
         DC    AL1(RECUCPST,ACTMNT)                                             
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FE02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MCPOSTMX EQU   *                                                                
         EJECT                                                                  
*                                                                               
* -- UPFRONT NET POST MAINT                                                     
*                                                                               
MNPOSTD  DC    AL1(MNPOSTDX-MNPOSTD)                                            
         DC    AL1(RECUNPST,ACTMAI)                                             
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FE02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MNPOSTDX EQU   *                                                                
*                                                                               
MNPSTD2  DC    AL1(MNPSTD2X-MNPSTD2)                                            
         DC    AL1(RECUNPST,ACTMAI)                                             
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'FE02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MNPSTD2X EQU   *                                                                
*                                                                               
MNPOSTM  DC    AL1(MNPOSTMX-MNPOSTM)                                            
         DC    AL1(RECUNPST,ACTMNT)                                             
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FE02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MNPOSTMX EQU   *                                                                
         EJECT                                                                  
*                                                                               
* -- AOR POST MAINTS                                                            
*                                                                               
MAPSTD   DC    AL1(MAPSTDX-MAPSTD)                                              
         DC    AL1(RECAPOST,ACTMAI)                                             
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'EE02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MAPSTDX  EQU   *                                                                
*                                                                               
MAPSTD2  DC    AL1(MAPSTD2X-MAPSTD2)                                            
         DC    AL1(RECAPOST,ACTMAI)                                             
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'EE02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MAPSTD2X EQU   *                                                                
*                                                                               
MAPSTM   DC    AL1(MAPSTMX-MAPSTM)                                              
         DC    AL1(RECAPOST,ACTMNT)                                             
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'EE02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MAPSTMX  EQU   *                                                                
         EJECT                                                                  
*                                                                               
* -- AOR POST2 MAINTS                                                           
*                                                                               
MAPS2D   DC    AL1(MAPS2DX-MAPS2D)                                              
         DC    AL1(RECAPOS2,ACTMAI)                                             
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'ED02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MAPS2DX  EQU   *                                                                
*                                                                               
MAPS2D2  DC    AL1(MAPS2D2X-MAPS2D2)                                            
         DC    AL1(RECAPOS2,ACTMAI)                                             
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'ED02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MAPS2D2X EQU   *                                                                
*                                                                               
MAPS2M   DC    AL1(MAPS2MX-MAPS2M)                                              
         DC    AL1(RECAPOS2,ACTMNT)                                             
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'ED02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MAPS2MX  EQU   *                                                                
         EJECT                                                                  
*                                                                               
* -- AOR UPFRONT COMMISSION POST MAINT                                          
*                                                                               
MUACPTD  DC    AL1(MUACPTDX-MUACPTD)                                            
         DC    AL1(RECUACPT,ACTMAI)                                             
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'EE02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MUACPTDX EQU   *                                                                
*                                                                               
MUACPD2  DC    AL1(MUACPD2X-MUACPD2)                                            
         DC    AL1(RECUACPT,ACTMAI)                                             
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'EE02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MUACPD2X EQU   *                                                                
*                                                                               
MUACPTM  DC    AL1(MUACPTMX-MUACPTM)                                            
         DC    AL1(RECUACPT,ACTMNT)                                             
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'EE02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MUACPTMX EQU   *                                                                
         EJECT                                                                  
*                                                                               
* -- AOR UPFRONT NET POST MAINT                                                 
*                                                                               
MUANPTD  DC    AL1(MUANPTDX-MUANPTD)                                            
         DC    AL1(RECUANPT,ACTMAI)                                             
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'EE02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MUANPTDX EQU   *                                                                
*                                                                               
MUANPD2  DC    AL1(MUANPD2X-MUANPD2)                                            
         DC    AL1(RECUANPT,ACTMAI)                                             
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'EE02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MUANPD2X EQU   *                                                                
*                                                                               
MUANPTM  DC    AL1(MUANPTMX-MUANPTM)                                            
         DC    AL1(RECUANPT,ACTMNT)                                             
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'EE02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MUANPTMX EQU   *                                                                
         EJECT                                                                  
*                                                                               
* -- RETAIL POST MAINTS                                                         
*                                                                               
MRPSTD   DC    AL1(MRPSTDX-MRPSTD)                                              
         DC    AL1(RECRPOST,ACTMAI)                                             
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'EC02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MRPSTDX  EQU   *                                                                
*                                                                               
MRPSTD2  DC    AL1(MRPSTD2X-MRPSTD2)                                            
         DC    AL1(RECRPOST,ACTMAI)                                             
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'EC02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MRPSTD2X EQU   *                                                                
*                                                                               
MRPSTM   DC    AL1(MRPSTMX-MRPSTM)                                              
         DC    AL1(RECRPOST,ACTMNT)                                             
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'EC02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MRPSTMX  EQU   *                                                                
         EJECT                                                                  
*                                                                               
* -- RETAIL PROF MAINTS                                                         
*                                                                               
MRPRFD   DC    AL1(MRPRFDX-MRPRFD)                                              
         DC    AL1(RECRPROF,ACTMAI)                                             
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'DE05'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MRPRFDX  EQU   *                                                                
*                                                                               
MRPRFD2  DC    AL1(MRPRFD2X-MRPRFD2)                                            
         DC    AL1(RECRPROF,ACTMAI)                                             
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'DE05'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MRPRFD2X EQU   *                                                                
*                                                                               
MRPRFM   DC    AL1(MRPRFMX-MRPRFM)                                              
         DC    AL1(RECRPROF,ACTMNT)                                             
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'DE05'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MRPRFMX  EQU   *                                                                
         EJECT                                                                  
*                                                                               
* -- REGIONAL/LMG POST MAINTS  (BPOST)                                          
*                                                                               
MBPSTD   DC    AL1(MBPSTDX-MBPSTD)                                              
         DC    AL1(RECBPOST,ACTMAI)                                             
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'EC02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MBPSTDX  EQU   *                                                                
*                                                                               
MBPSTD2  DC    AL1(MBPSTD2X-MBPSTD2)                                            
         DC    AL1(RECBPOST,ACTMAI)                                             
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'EC02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MBPSTD2X EQU   *                                                                
*                                                                               
MBPSTM   DC    AL1(MBPSTMX-MBPSTM)                                              
         DC    AL1(RECBPOST,ACTMNT)                                             
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'EC02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MBPSTMX  EQU   *                                                                
         EJECT                                                                  
*                                                                               
* -- PST POST MAINTS                                                            
*                                                                               
MTPSTD   DC    AL1(MTPSTDX-MTPSTD)                                              
         DC    AL1(RECTPOST,ACTMAI)                                             
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'CA02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MTPSTDX  EQU   *                                                                
*                                                                               
MTPSTD2  DC    AL1(MTPSTD2X-MTPSTD2)                                            
         DC    AL1(RECTPOST,ACTMAI)                                             
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'CA02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MTPSTD2X EQU   *                                                                
*                                                                               
MTPSTM   DC    AL1(MTPSTMX-MTPSTM)                                              
         DC    AL1(RECTPOST,ACTMNT)                                             
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'CA02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MTPSTMX  EQU   *                                                                
         EJECT                                                                  
*                                                                               
* -- PRINT PRODUCTION AND FINANCIAL POST MAINTS                                 
*                                                                               
MSPSTD   DC    AL1(MSPSTDX-MSPSTD)                                              
         DC    AL1(RECSPOST,ACTMAI)                                             
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'DD02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MSPSTDX  EQU   *                                                                
*                                                                               
MSPSTD2  DC    AL1(MSPSTD2X-MSPSTD2)                                            
         DC    AL1(RECSPOST,ACTMAI)                                             
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'DD02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MSPSTD2X EQU   *                                                                
*                                                                               
MSPSTM   DC    AL1(MSPSTMX-MSPSTM)                                              
         DC    AL1(RECSPOST,ACTMNT)                                             
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'DD02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MSPSTMX  EQU   *                                                                
         EJECT                                                                  
*                                                                               
* -- PRINT PROD/FIN PROF MAINT                                                  
*                                                                               
MPPRFD   DC    AL1(MPPRFDX-MPPRFD)                                              
         DC    AL1(RECSPROF,ACTMAI)                                             
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'DF05'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MPPRFDX  EQU   *                                                                
*                                                                               
MPPRFD2  DC    AL1(MPPRFD2X-MPPRFD2)                                            
         DC    AL1(RECSPROF,ACTMAI)                                             
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'DF05'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MPPRFD2X EQU   *                                                                
*                                                                               
MPPRFM   DC    AL1(MPPRFMX-MPPRFM)                                              
         DC    AL1(RECSPROF,ACTMNT)                                             
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'DF05'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MPPRFMX  EQU   *                                                                
         EJECT                                                                  
*                                                                               
* -- TRADE BILLING MAINTS (XPROF)                                               
*                                                                               
MXPRFD   DC    AL1(MXPRFDX-MXPRFD)                                              
         DC    AL1(RECXPROF,ACTMAI)                                             
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'CD05'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXPRFDX  EQU   *                                                                
*                                                                               
MXPRFD2  DC    AL1(MXPRFD2X-MXPRFD2)                                            
         DC    AL1(RECXPROF,ACTMAI)                                             
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'CD05'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXPRFD2X EQU   *                                                                
*                                                                               
MXPRFM   DC    AL1(MXPRFMX-MXPRFM)                                              
         DC    AL1(RECXPROF,ACTMNT)                                             
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'CD05'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXPRFMX  EQU   *                                                                
         EJECT                                                                  
*                                                                               
* -- TRADE BILLING MAINTS (XPOST)                                               
*                                                                               
MXPSTD   DC    AL1(MXPSTDX-MXPSTD)                                              
         DC    AL1(RECXPST,ACTMAI)                                              
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'CB02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXPSTDX  EQU   *                                                                
*                                                                               
MXPSTD2  DC    AL1(MXPSTD2X-MXPSTD2)                                            
         DC    AL1(RECXPST,ACTMAI)                                              
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'CB02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXPSTD2X EQU   *                                                                
*                                                                               
MXPSTM   DC    AL1(MXPSTMX-MXPSTM)                                              
         DC    AL1(RECXPST,ACTMNT)                                              
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'CB02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXPSTMX  EQU   *                                                                
*                                                                               
* -- TRADE BILLING MAINTS (XAPOST)                                              
*                                                                               
MXAPSTD  DC    AL1(MXAPSTDX-MXAPSTD)                                            
         DC    AL1(RECXAPST,ACTMAI)                                             
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'CC02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXAPSTDX EQU   *                                                                
*                                                                               
MXAPST2  DC    AL1(MXAPST2X-MXAPST2)                                            
         DC    AL1(RECXAPST,ACTMAI)                                             
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'CC02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXAPST2X EQU   *                                                                
*                                                                               
MXAPSTM  DC    AL1(MXAPSTMX-MXAPSTM)                                            
         DC    AL1(RECXAPST,ACTMNT)                                             
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'CC02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXAPSTMX EQU   *                                                                
*                                                                               
* -- TRADE BILLING MAINTS (XAPOS2) EXTENSION TO XAPOST                          
*                                                                               
MXAPS2D  DC    AL1(MXAPS2DX-MXAPS2D)                                            
         DC    AL1(RECXAPS2,ACTMAI)                                             
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'CF02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXAPS2DX EQU   *                                                                
*                                                                               
MXAPS22  DC    AL1(MXAPS22X-MXAPS22)                                            
         DC    AL1(RECXAPS2,ACTMAI)                                             
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'CF02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXAPS22X EQU   *                                                                
*                                                                               
MXAPS2M  DC    AL1(MXAPS2MX-MXAPS2M)                                            
         DC    AL1(RECXAPS2,ACTMNT)                                             
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'CF02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXAPS2MX EQU   *                                                                
*                                                                               
* -- BARTER BILLING MAINTS (MPROF)                                              
*                                                                               
MXMPRFD  DC    AL1(MXMPRFX-MXMPRFD)                                             
         DC    AL1(RECMPROF,ACTMAI)                                             
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'DB05'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXMPRFX  EQU   *                                                                
*                                                                               
MXMPRFD2 DC    AL1(MXMPRF2X-MXMPRFD2)                                           
         DC    AL1(RECMPROF,ACTMAI)                                             
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'DB05'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXMPRF2X EQU   *                                                                
*                                                                               
MXMPRFM  DC    AL1(MXMPRFMX-MXMPRFM)                                            
         DC    AL1(RECMPROF,ACTMNT)                                             
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'DB05'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXMPRFMX EQU   *                                                                
         EJECT                                                                  
*                                                                               
* -- BARTER BILLING MAINTS (MPOST)                                              
*                                                                               
MXMPSTD  DC    AL1(MXMPSTX-MXMPSTD)                                             
         DC    AL1(RECMPST,ACTMAI)                                              
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'DC02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXMPSTX  EQU   *                                                                
*                                                                               
MXMPSTD2 DC    AL1(MXMPST2X-MXMPSTD2)                                           
         DC    AL1(RECMPST,ACTMAI)                                              
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'DC02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXMPST2X EQU   *                                                                
*                                                                               
MXMPSTM  DC    AL1(MXMPSTMX-MXMPSTM)                                            
         DC    AL1(RECMPST,ACTMNT)                                              
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'DC02'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXMPSTMX EQU   *                                                                
         EJECT                                                                  
*                                                                               
* -- BILL DISPLAY                                                               
*                                                                               
MBILDIS  DC    AL1(MBILDISX-MBILDIS)                                            
         DC    AL1(RECBILL,ACTBDIS)                                             
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'EB08'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYCLT,KEYPRD,KEYEST,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MBILDISX EQU   *                                                                
*                                                                               
* -- BILL DISPLAY                                                               
*                                                                               
MBILDS2  DC    AL1(MBILDS2X-MBILDS2)                                            
         DC    AL1(RECBILL,ACTBDIS)                                             
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'EB08'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYCLT,KEYPRD,KEYEST,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MBILDS2X EQU   *                                                                
*                                                                               
* -- BILL DISPLAY                                                               
*                                                                               
MBILDS3  DC    AL1(MBILDS3X-MBILDS3)                                            
         DC    AL1(RECBILL,ACTDIS2)                                             
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'EB08'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYCLT,KEYPRD,KEYEST,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MBILDS3X EQU   *                                                                
         EJECT                                                                  
*                                                                               
* -- POST TRACE (DISPLAY)                                                       
*                                                                               
MPSTRC   DC    AL1(MPSTRCX-MPSTRC)                                              
         DC    AL1(RECPOST,ACTRACE)                                             
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MPSTRCX  EQU   *                                                                
*                                                                               
MPSTRC2  DC    AL1(MPSTRC2X-MPSTRC2)                                            
         DC    AL1(RECPOST,ACTRACE)                                             
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MPSTRC2X EQU   *                                                                
*                                                                               
MPSTRCM  DC    AL1(MPSTRCMX-MPSTRCM)                                            
         DC    AL1(RECPOST,ACTTRC)                                              
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MPSTRCMX EQU   *                                                                
*                                                                               
* -- PROF TRACE (DISPLAY)                                                       
*                                                                               
MPRTRC   DC    AL1(MPRTRCX-MPRTRC)                                              
         DC    AL1(RECPROF,ACTRACE)                                             
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FA06'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MPRTRCX  EQU   *                                                                
*                                                                               
MPRTRC2  DC    AL1(MPRTRC2X-MPRTRC2)                                            
         DC    AL1(RECPROF,ACTRACE)                                             
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'FA06'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MPRTRC2X EQU   *                                                                
*                                                                               
MPRTRCM  DC    AL1(MPRTRCMX-MPRTRCM)                                            
         DC    AL1(RECPROF,ACTTRC)                                              
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FA06'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MPRTRCMX EQU   *                                                                
         EJECT                                                                  
*                                                                               
* -- POST TRACE (DISPLAY)                                                       
*                                                                               
MDPSTRC  DC    AL1(MDPSTRCX-MDPSTRC)                                            
         DC    AL1(RECDPOST,ACTRACE)                                            
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MDPSTRCX EQU   *                                                                
*                                                                               
MDPTRC2  DC    AL1(MDPTRC2X-MDPTRC2)                                            
         DC    AL1(RECDPOST,ACTRACE)                                            
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MDPTRC2X EQU   *                                                                
*                                                                               
MDPTRCM  DC    AL1(MDPTRCMX-MDPTRCM)                                            
         DC    AL1(RECDPOST,ACTTRC)                                             
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MDPTRCMX EQU   *                                                                
         EJECT                                                                  
*                                                                               
* -- APOST TRACE (DISPLAY)                                                      
*                                                                               
MAPSTR   DC    AL1(MAPSTRX-MAPSTR)                                              
         DC    AL1(RECAPOST,ACTRACE)                                            
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MAPSTRX  EQU   *                                                                
*                                                                               
MAPSTR2  DC    AL1(MAPSTR2X-MAPSTR2)                                            
         DC    AL1(RECAPOST,ACTRACE)                                            
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MAPSTR2X EQU   *                                                                
*                                                                               
MASTRCM  DC    AL1(MASTRCMX-MASTRCM)                                            
         DC    AL1(RECAPOST,ACTTRC)                                             
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MASTRCMX EQU   *                                                                
         EJECT                                                                  
*                                                                               
* -- APOS2 TRACE (DISPLAY)                                                      
*                                                                               
MAPS2R   DC    AL1(MAPS2RX-MAPS2R)                                              
         DC    AL1(RECAPOS2,ACTRACE)                                            
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MAPS2RX  EQU   *                                                                
*                                                                               
MAPS2R2  DC    AL1(MAPS2R2X-MAPS2R2)                                            
         DC    AL1(RECAPOS2,ACTRACE)                                            
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MAPS2R2X EQU   *                                                                
*                                                                               
MAS2RCM  DC    AL1(MAS2RCMX-MAS2RCM)                                            
         DC    AL1(RECAPOS2,ACTTRC)                                             
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MAS2RCMX EQU   *                                                                
         EJECT                                                                  
*                                                                               
* -- UCPOST TRACE (DISPLAY)                                                     
*                                                                               
MUCTRC   DC    AL1(MUCTRCX-MUCTRC)                                              
         DC    AL1(RECUCPST,ACTRACE)                                            
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MUCTRCX  EQU   *                                                                
*                                                                               
MUCTRC2  DC    AL1(MUCTRC2X-MUCTRC2)                                            
         DC    AL1(RECUCPST,ACTRACE)                                            
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MUCTRC2X EQU   *                                                                
*                                                                               
MUCTRCM  DC    AL1(MUCTRCMX-MUCTRCM)                                            
         DC    AL1(RECUCPST,ACTTRC)                                             
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MUCTRCMX EQU   *                                                                
         EJECT                                                                  
*                                                                               
* -- UNPOST TRACE (DISPLAY)                                                     
*                                                                               
MUNTRC   DC    AL1(MUNTRCX-MUNTRC)                                              
         DC    AL1(RECUNPST,ACTRACE)                                            
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MUNTRCX  EQU   *                                                                
*                                                                               
MUNTRC2  DC    AL1(MUNTRC2X-MUNTRC2)                                            
         DC    AL1(RECUNPST,ACTRACE)                                            
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MUNTRC2X EQU   *                                                                
*                                                                               
MUNTRCM  DC    AL1(MUNTRCMX-MUNTRCM)                                            
         DC    AL1(RECUNPST,ACTTRC)                                             
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MUNTRCMX EQU   *                                                                
         EJECT                                                                  
*                                                                               
* -- UACPOST TRACE (DISPLAY)                                                    
*                                                                               
MUACTRC  DC    AL1(MUACTRCX-MUACTRC)                                            
         DC    AL1(RECUACPT,ACTRACE)                                            
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MUACTRCX EQU   *                                                                
*                                                                               
MUACT2   DC    AL1(MUACT2X-MUACT2)                                              
         DC    AL1(RECUACPT,ACTRACE)                                            
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MUACT2X  EQU   *                                                                
*                                                                               
MUACTM   DC    AL1(MUACTMX-MUACTM)                                              
         DC    AL1(RECUACPT,ACTTRC)                                             
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MUACTMX  EQU   *                                                                
         EJECT                                                                  
*                                                                               
* -- UANPOST TRACE (DISPLAY)                                                    
*                                                                               
MUANTRC  DC    AL1(MUANTRCX-MUANTRC)                                            
         DC    AL1(RECUANPT,ACTRACE)                                            
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MUANTRCX EQU   *                                                                
*                                                                               
MUANT2   DC    AL1(MUANT2X-MUANT2)                                              
         DC    AL1(RECUANPT,ACTRACE)                                            
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MUANT2X  EQU   *                                                                
*                                                                               
MUANTM   DC    AL1(MUANTMX-MUANTM)                                              
         DC    AL1(RECUANPT,ACTTRC)                                             
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MUANTMX  EQU   *                                                                
         EJECT                                                                  
*                                                                               
* -- RPOST TRACE (DISPLAY)                                                      
*                                                                               
MRPSTR   DC    AL1(MRPSTRX-MRPSTR)                                              
         DC    AL1(RECRPOST,ACTRACE)                                            
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MRPSTRX  EQU   *                                                                
*                                                                               
MRPSTR2  DC    AL1(MRPSTR2X-MRPSTR2)                                            
         DC    AL1(RECRPOST,ACTRACE)                                            
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MRPSTR2X EQU   *                                                                
*                                                                               
MRSTRCM  DC    AL1(MRSTRCMX-MRSTRCM)                                            
         DC    AL1(RECRPOST,ACTTRC)                                             
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MRSTRCMX EQU   *                                                                
         EJECT                                                                  
*                                                                               
* -- RPROF TRACE (DISPLAY)                                                      
*                                                                               
MRRTRC   DC    AL1(MRRTRCX-MRRTRC)                                              
         DC    AL1(RECRPROF,ACTRACE)                                            
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FA06'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MRRTRCX  EQU   *                                                                
*                                                                               
MRRTRC2  DC    AL1(MRRTRC2X-MRRTRC2)                                            
         DC    AL1(RECRPROF,ACTRACE)                                            
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'FA06'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MRRTRC2X EQU   *                                                                
*                                                                               
MRRTRCM  DC    AL1(MRRTRCMX-MRRTRCM)                                            
         DC    AL1(RECRPROF,ACTTRC)                                             
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FA06'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MRRTRCMX EQU   *                                                                
*                                                                               
* -- BPOST TRACE (DISPLAY)                                                      
*                                                                               
MBPTRC   DC    AL1(MBPTRCX-MBPTRC)                                              
         DC    AL1(RECBPOST,ACTRACE)                                            
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MBPTRCX  EQU   *                                                                
*                                                                               
MBPTRC2  DC    AL1(MBPTRC2X-MBPTRC2)                                            
         DC    AL1(RECBPOST,ACTRACE)                                            
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MBPTRC2X EQU   *                                                                
*                                                                               
MBPTRCM  DC    AL1(MBPTRCMX-MBPTRCM)                                            
         DC    AL1(RECBPOST,ACTTRC)                                             
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MBPTRCMX EQU   *                                                                
         EJECT                                                                  
*                                                                               
* -- XPROF TRACE (DISPLAY)                                                      
*                                                                               
MXPRFC   DC    AL1(MXPRFCX-MXPRFC)                                              
         DC    AL1(RECXPROF,ACTRACE)                                            
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FA04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXPRFCX  EQU   *                                                                
*                                                                               
MXPRFC2  DC    AL1(MXPRFC2X-MXPRFC2)                                            
         DC    AL1(RECXPROF,ACTRACE)                                            
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'FA04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXPRFC2X EQU   *                                                                
*                                                                               
MXPRFCM  DC    AL1(MXPRFCMX-MXPRFCM)                                            
         DC    AL1(RECXPROF,ACTTRC)                                             
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FA04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXPRFCMX EQU   *                                                                
*                                                                               
* -- XPOST TRACE (DISPLAY)                                                      
*                                                                               
MXPSTC   DC    AL1(MXPSTCX-MXPSTC)                                              
         DC    AL1(RECXPST,ACTRACE)                                             
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXPSTCX  EQU   *                                                                
*                                                                               
MXPSTC2  DC    AL1(MXPSTC2X-MXPSTC2)                                            
         DC    AL1(RECXPST,ACTRACE)                                             
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXPSTC2X EQU   *                                                                
*                                                                               
MXPSTCM  DC    AL1(MXPSTCMX-MXPSTCM)                                            
         DC    AL1(RECXPST,ACTTRC)                                              
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXPSTCMX EQU   *                                                                
*                                                                               
* -- XAPOST TRACE (DISPLAY)                                                     
*                                                                               
MXAPSTC  DC    AL1(MXAPSTCX-MXAPSTC)                                            
         DC    AL1(RECXAPST,ACTRACE)                                            
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXAPSTCX EQU   *                                                                
*                                                                               
MXAPSTC2 DC    AL1(MXAPSTXX-MXAPSTC2)                                           
         DC    AL1(RECXAPST,ACTRACE)                                            
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXAPSTXX EQU   *                                                                
*                                                                               
MXAPSTRM DC    AL1(MXAPSTRX-MXAPSTRM)                                           
         DC    AL1(RECXAPST,ACTTRC)                                             
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXAPSTRX EQU   *                                                                
*                                                                               
* -- XAPOS2 TRACE (DISPLAY) XAPOST EXTENSION                                    
*                                                                               
MXAPS2C  DC    AL1(MXAPS2CX-MXAPS2C)                                            
         DC    AL1(RECXAPS2,ACTRACE)                                            
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXAPS2CX EQU   *                                                                
*                                                                               
MXAPS2C2 DC    AL1(MXAPS2XX-MXAPS2C2)                                           
         DC    AL1(RECXAPS2,ACTRACE)                                            
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXAPS2XX EQU   *                                                                
*                                                                               
MXAPS2RM DC    AL1(MXAPS2RX-MXAPS2RM)                                           
         DC    AL1(RECXAPS2,ACTTRC)                                             
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXAPS2RX EQU   *                                                                
*                                                                               
* -- MPROF TRACE (DISPLAY)                                                      
*                                                                               
MXMPRFC  DC    AL1(MXMPRFCX-MXMPRFC)                                            
         DC    AL1(RECMPROF,ACTRACE)                                            
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FA04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXMPRFCX EQU   *                                                                
*                                                                               
MXMPRFC2 DC    AL1(MXMPRC2X-MXMPRFC2)                                           
         DC    AL1(RECMPROF,ACTRACE)                                            
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'FA04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXMPRC2X EQU   *                                                                
*                                                                               
MXMPRFCM DC    AL1(MXMPRCMX-MXMPRFCM)                                           
         DC    AL1(RECMPROF,ACTTRC)                                             
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FA04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXMPRCMX EQU   *                                                                
*                                                                               
* -- MPOST TRACE (DISPLAY)                                                      
*                                                                               
MXMPSTC  DC    AL1(MXMPSTCX-MXMPSTC)                                            
         DC    AL1(RECMPST,ACTRACE)                                             
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXMPSTCX EQU   *                                                                
*                                                                               
MXMPSTC2 DC    AL1(MXMPSC2X-MXMPSTC2)                                           
         DC    AL1(RECMPST,ACTRACE)                                             
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXMPSC2X EQU   *                                                                
*                                                                               
MXMPSTCM DC    AL1(MXMPSCMX-MXMPSTCM)                                           
         DC    AL1(RECMPST,ACTTRC)                                              
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXMPSCMX EQU   *                                                                
*                                                                               
* -- RPOST TRACE (DISPLAY)                                                      
*                                                                               
MTPSTR   DC    AL1(MTPSTRX-MTPSTR)                                              
         DC    AL1(RECTPOST,ACTRACE)                                            
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MTPSTRX  EQU   *                                                                
*                                                                               
MTPSTR2  DC    AL1(MTPSTR2X-MTPSTR2)                                            
         DC    AL1(RECTPOST,ACTRACE)                                            
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MTPSTR2X EQU   *                                                                
*                                                                               
MTSTRCM  DC    AL1(MTSTRCMX-MTSTRCM)                                            
         DC    AL1(RECTPOST,ACTTRC)                                             
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MTSTRCMX EQU   *                                                                
         EJECT                                                                  
*                                                                               
* -- SPOST TRACE (DISPLAY)                                                      
*                                                                               
MSPSTR   DC    AL1(MSPSTRX-MSPSTR)                                              
         DC    AL1(RECSPOST,ACTRACE)                                            
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MSPSTRX  EQU   *                                                                
*                                                                               
MSPSTR2  DC    AL1(MSPSTR2X-MSPSTR2)                                            
         DC    AL1(RECSPOST,ACTRACE)                                            
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MSPSTR2X EQU   *                                                                
*                                                                               
MSSTRCM  DC    AL1(MSSTRCMX-MSSTRCM)                                            
         DC    AL1(RECSPOST,ACTTRC)                                             
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FC04'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MSSTRCMX EQU   *                                                                
         EJECT                                                                  
*                                                                               
* -- SPROF TRACE (DISPLAY)                                                      
*                                                                               
MSPRTR   DC    AL1(MSPRTRX-MSPRTR)                                              
         DC    AL1(RECSPROF,ACTRACE)                                            
         DC    AL1(MIXISEL+MIXILFM,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FA06'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MSPRTRX  EQU   *                                                                
*                                                                               
MSPRTR2  DC    AL1(MSPRTR2X-MSPRTR2)                                            
         DC    AL1(RECSPROF,ACTRACE)                                            
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    X'0000'                                                          
         DC    X'FA06'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MSPRTR2X EQU   *                                                                
*                                                                               
MSPRTCM  DC    AL1(MSPRTCMX-MSPRTCM)                                            
         DC    AL1(RECSPROF,ACTTRC)                                             
         DC    AL1(MIXILFM+MIXKREQ,MIXILSM)                                     
         DC    X'0000'                                                          
         DC    X'FA06'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MSPRTCMX EQU   *                                                                
         EJECT                                                                  
*                                                                               
* - REGULAR POST LIST                                                           
*                                                                               
MPOSTL   DC    AL1(MPOSTLX-MPOSTL)                                              
         DC    AL1(RECPOST,ACTLIS)                                              
         DC    AL1(MIXILST,0)                                                   
         DC    X'0000'                                                          
         DC    X'FD03'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MPOSTLX  EQU   *                                                                
*                                                                               
* - REGULAR PROF LIST                                                           
*                                                                               
MPROFL   DC    AL1(MPROFLX-MPROFL)                                              
         DC    AL1(RECPROF,ACTLIS)                                              
         DC    AL1(MIXILST,0)                                                   
         DC    X'0000'                                                          
         DC    X'EF03'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MPROFLX  EQU   *                                                                
*                                                                               
* -- BILL LIST                                                                  
*                                                                               
MBILLST  DC    AL1(MBILLSTX-MBILLST)                                            
         DC    AL1(RECBILL,ACTLIS)                                              
         DC    AL1(MIXILST,0)                                                   
         DC    X'0000'                                                          
         DC    X'EA07'                                                          
         DC    AL1(6,6,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,KEYEST,0,0)           
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MBILLSTX EQU   *                                                                
         EJECT                                                                  
*                                                                               
* --  REGULAR POST SELECT                                                       
*                                                                               
MPOSSEL  DC    AL1(MPOSSELX-MPOSSEL)                                            
         DC    AL1(RECPOST,ACTSEL)                                              
         DC    AL1(MIXISEL,0)                                                   
         DC    X'0000'                                                          
         DC    X'0000'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MPOSSELX EQU   *                                                                
*                                                                               
* --  REGULAR PROF SELECT                                                       
*                                                                               
MPROSEL  DC    AL1(MPROSELX-MPROSEL)                                            
         DC    AL1(RECPROF,ACTSEL)                                              
         DC    AL1(MIXISEL,0)                                                   
         DC    X'0000'                                                          
         DC    X'0000'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MPROSELX EQU   *                                                                
*                                                                               
* --  DPOST SELECT                                                              
*                                                                               
MDPOSSEL DC    AL1(MDPOSELX-MDPOSSEL)                                           
         DC    AL1(RECDPOST,ACTSEL)                                             
         DC    AL1(MIXISEL,0)                                                   
         DC    X'0000'                                                          
         DC    X'0000'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MDPOSELX EQU   *                                                                
*                                                                               
* --  UPFRONT COMMISSION POST SELECT                                            
*                                                                               
MUCPSEL  DC    AL1(MUCPSELX-MUCPSEL)                                            
         DC    AL1(RECUCPST,ACTSEL)                                             
         DC    AL1(MIXISEL,0)                                                   
         DC    X'0000'                                                          
         DC    X'0000'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MUCPSELX EQU   *                                                                
*                                                                               
* --  UPFRONT NET POST SELECT                                                   
*                                                                               
MUNPSEL  DC    AL1(MUNPSELX-MUNPSEL)                                            
         DC    AL1(RECUNPST,ACTSEL)                                             
         DC    AL1(MIXISEL,0)                                                   
         DC    X'0000'                                                          
         DC    X'0000'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MUNPSELX EQU   *                                                                
         EJECT                                                                  
*                                                                               
* --  AOR POST SELECT                                                           
*                                                                               
MAPSTSL  DC    AL1(MAPSTSLX-MAPSTSL)                                            
         DC    AL1(RECAPOST,ACTSEL)                                             
         DC    AL1(MIXISEL,0)                                                   
         DC    X'0000'                                                          
         DC    X'0000'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MAPSTSLX EQU   *                                                                
*                                                                               
* --  AOR2 POST SELECT                                                          
*                                                                               
MA2PSTSL DC    AL1(MA2PSTSX-MA2PSTSL)                                           
         DC    AL1(RECAPOS2,ACTSEL)                                             
         DC    AL1(MIXISEL,0)                                                   
         DC    X'0000'                                                          
         DC    X'0000'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MA2PSTSX EQU   *                                                                
*                                                                               
* --  AOR UPFRONT COMMISSION POST SELECT                                        
*                                                                               
MUACSEL  DC    AL1(MUACSELX-MUACSEL)                                            
         DC    AL1(RECUACPT,ACTSEL)                                             
         DC    AL1(MIXISEL,0)                                                   
         DC    X'0000'                                                          
         DC    X'0000'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MUACSELX EQU   *                                                                
*                                                                               
* --  AOR UPFRONT NET POST SELECT                                               
*                                                                               
MUANSEL  DC    AL1(MUANSELX-MUANSEL)                                            
         DC    AL1(RECUANPT,ACTSEL)                                             
         DC    AL1(MIXISEL,0)                                                   
         DC    X'0000'                                                          
         DC    X'0000'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MUANSELX EQU   *                                                                
         EJECT                                                                  
*                                                                               
* --  RETAIL POST SELECT                                                        
*                                                                               
MRPSTSL  DC    AL1(MRPSTSLX-MRPSTSL)                                            
         DC    AL1(RECRPOST,ACTSEL)                                             
         DC    AL1(MIXISEL,0)                                                   
         DC    X'0000'                                                          
         DC    X'0000'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MRPSTSLX EQU   *                                                                
*                                                                               
* --  BPOST SELECT                                                              
*                                                                               
MBPSTSL  DC    AL1(MBPSTSLX-MBPSTSL)                                            
         DC    AL1(RECBPOST,ACTSEL)                                             
         DC    AL1(MIXISEL,0)                                                   
         DC    X'0000'                                                          
         DC    X'0000'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MBPSTSLX EQU   *                                                                
*                                                                               
* --  RETAIL PROF SELECT                                                        
*                                                                               
MRPRFSL  DC    AL1(MRPRFSLX-MRPRFSL)                                            
         DC    AL1(RECRPROF,ACTSEL)                                             
         DC    AL1(MIXISEL,0)                                                   
         DC    X'0000'                                                          
         DC    X'0000'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MRPRFSLX EQU   *                                                                
         EJECT                                                                  
*                                                                               
* --  PST POST SELECT                                                           
*                                                                               
MTPSTSL  DC    AL1(MTPSTSLX-MTPSTSL)                                            
         DC    AL1(RECTPOST,ACTSEL)                                             
         DC    AL1(MIXISEL,0)                                                   
         DC    X'0000'                                                          
         DC    X'0000'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MTPSTSLX EQU   *                                                                
         EJECT                                                                  
*                                                                               
* --  PRINT PROD PROF SELECT                                                    
*                                                                               
MPPRFSL  DC    AL1(MPPRFSLX-MPPRFSL)                                            
         DC    AL1(RECSPROF,ACTSEL)                                             
         DC    AL1(MIXISEL,0)                                                   
         DC    X'0000'                                                          
         DC    X'0000'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MPPRFSLX EQU   *                                                                
*                                                                               
* -- BILL SELECT                                                                
*                                                                               
MBILSEL  DC    AL1(MBILSELX-MBILSEL)                                            
         DC    AL1(RECBILL,ACTSEL)                                              
         DC    AL1(MIXISEL,0)                                                   
         DC    X'0000'                                                          
         DC    X'0000'                                                          
         DC    AL1(1,1,KEYSYS,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MBILSELX EQU   *                                                                
*                                                                               
* --  XPROF SELECT                                                              
*                                                                               
MXPRFSL  DC    AL1(MXPRFSLX-MXPRFSL)                                            
         DC    AL1(RECXPROF,ACTSEL)                                             
         DC    AL1(MIXISEL,0)                                                   
         DC    X'0000'                                                          
         DC    X'0000'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXPRFSLX EQU   *                                                                
*                                                                               
* --  XPOST SELECT                                                              
*                                                                               
MXPSTSL  DC    AL1(MXPSTSLX-MXPSTSL)                                            
         DC    AL1(RECXPST,ACTSEL)                                              
         DC    AL1(MIXISEL,0)                                                   
         DC    X'0000'                                                          
         DC    X'0000'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXPSTSLX EQU   *                                                                
*                                                                               
* --  XAPOST SELECT                                                             
*                                                                               
MXAPSTSL DC    AL1(MXAPSTSX-MXAPSTSL)                                           
         DC    AL1(RECXAPST,ACTSEL)                                             
         DC    AL1(MIXISEL,0)                                                   
         DC    X'0000'                                                          
         DC    X'0000'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXAPSTSX EQU   *                                                                
*                                                                               
* --  XAPOS2 SELECT                                                             
*                                                                               
MXAPS2SL DC    AL1(MXAPS2SX-MXAPS2SL)                                           
         DC    AL1(RECXAPS2,ACTSEL)                                             
         DC    AL1(MIXISEL,0)                                                   
         DC    X'0000'                                                          
         DC    X'0000'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXAPS2SX EQU   *                                                                
MIXTABX  DC    AL1(EOT)                                                         
*                                                                               
* --  MPROF SELECT                                                              
*                                                                               
MXMPRFSL DC    AL1(MXMPRFLX-MXMPRFLX)                                           
         DC    AL1(RECMPROF,ACTSEL)                                             
         DC    AL1(MIXISEL,0)                                                   
         DC    X'0000'                                                          
         DC    X'0000'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXMPRFLX EQU   *                                                                
*                                                                               
* --  MPOST SELECT                                                              
*                                                                               
MXMPSTSL DC    AL1(MXMPSTSL-MXMPSTLX)                                           
         DC    AL1(RECMPST,ACTSEL)                                              
         DC    AL1(MIXISEL,0)                                                   
         DC    X'0000'                                                          
         DC    X'0000'                                                          
         DC    AL1(5,5,KEYSYS,KEYMED,KEYOFF,KEYCLT,KEYPRD,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MXMPSTLX EQU   *                                                                
         EJECT                                                                  
**************************************                                          
* KEY COMPONENT TABLE (SEE KEYTABD)  *                                          
**************************************                                          
*                                                                               
KEYTAB   DS    0X                                                               
         DC    C'SYSTEM ',AL1(KEYSYS)                                           
         DC    C'MEDIA  ',AL1(KEYMED)                                           
         DC    C'OFFICE ',AL1(KEYOFF)                                           
         DC    C'CLIENT ',AL1(KEYCLT)                                           
         DC    C'PRODUCT',AL1(KEYPRD)                                           
KEYTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
********************************                                                
* OPTION TABLE (SEE OPTTABD)   *                                                
********************************                                                
*                                                                               
OPTTAB   DS    0X                                                               
*                                                                               
TRANOPT  DC    AL1(TRANOPTX-TRANOPT)                                            
         DC    C'TRANSFER',C'TRA',AL1(OPTATAB+OPTBOOL+OPTITXT,0)                
         DC    AL1(0,0)                                                         
         DC    AL1(RECBILL,0)                                                   
         DC    AL1(3,0,0,L'INOIND)                                              
         DC    AL4(OPTTRAB)                                                     
         DC    AL1(OPTTRAN)                                                     
         DC    AL2(VALTRA-TBASE)                                                
         DC    AL2(INOIND-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(OPTUTRAB)                                                    
         DC    AL2(0)                                                           
TRANOPTX EQU   *                                                                
*                                                                               
UTRAOPT  DC    AL1(UTRAOPTX-UTRAOPT)                                            
         DC    C'*TRANS  ',C'*TR',AL1(OPTATAB+OPTBOOL+OPTITXT,0)                
         DC    AL1(0,0)                                                         
         DC    AL1(RECBILL,0)                                                   
         DC    AL1(2,0,0,L'INOIND)                                              
         DC    AL4(OPTUTRAB)                                                    
         DC    AL1(OPTUTRAN)                                                    
         DC    AL2(VALUTRA-TBASE)                                               
         DC    AL2(INOIND-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(OPTTRAB)                                                     
         DC    AL2(0)                                                           
UTRAOPTX EQU   *                                                                
*                                                                               
SRETOPT  DC    AL1(SRETOPTX-SRETOPT)                                            
         DC    C'*SPRET  ',C'*SP',AL1(OPTATAB+OPTBOOL+OPTITXT,0)                
         DC    AL1(0,0)                                                         
         DC    AL1(RECBILL,0)                                                   
         DC    AL1(2,0,0,L'INOIND)                                              
         DC    AL4(OPTSRETB)                                                    
         DC    AL1(OPTSRETN)                                                    
         DC    AL2(VALSRET-TBASE)                                               
         DC    AL2(INOIND-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(0)                                                           
SRETOPTX EQU   *                                                                
*                                 ONLY SHOW THESE TIMES IN LIST                 
TYPEOPT  DC    AL1(TYPEOPTX-TYPEOPT)                                            
         DC    C'TYPE    ',C'   ',AL1(OPTNRTN+OPTITXT,0)                        
         DC    AL1(0,0)                                                         
         DC    AL1(RECBILL,0)                                                   
         DC    AL1(2,2,40,L'INOTYPE)                                            
         DC    AL4(OPTTYPB)                                                     
         DC    AL1(OPTTYPN)                                                     
         DC    AL2(OPTTYPR)                                                     
         DC    AL2(INOTYPE-WORKD)                                               
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(OPTNTYB)       DON'T ALLOW USE WITH *TY=                     
         DC    AL2(0)                                                           
TYPEOPTX EQU   *                                                                
*                                 EXCLUDE THESE TYPES FROM LIST                 
NOTYOPT  DC    AL1(NOTYOPTX-NOTYOPT)                                            
         DC    C'*TYPE   ',C'   ',AL1(OPTNRTN+OPTITXT,0)                        
         DC    AL1(0,0)                                                         
         DC    AL1(RECBILL,0)                                                   
         DC    AL1(2,2,40,L'INOTYPE)                                            
         DC    AL4(OPTNTYB)                                                     
         DC    AL1(OPTNTYN)                                                     
         DC    AL2(OPTNTYR)                                                     
         DC    AL2(INOTYPE-WORKD)                                               
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(OPTTYPB)       DON'T ALLOW USE WITH TY=                      
         DC    AL2(0)                                                           
NOTYOPTX EQU   *                                                                
*                                                                               
MDATOPT  DC    AL1(MDATOPTX-MDATOPT)                                            
         DC    C'XDTE    ',C'   ',AL1(OPTNRTN+OPTITXT,0)                        
         DC    AL1(0,0)                                                         
         DC    AL1(RECBILL,0)                                                   
         DC    AL1(2,6,8,L'INOXDAT)                                             
         DC    AL4(OPTXDTB)                                                     
         DC    AL1(OPTXDTN)                                                     
         DC    AL2(OPTXDTR)                                                     
         DC    AL2(INOXDAT-WORKD)                                               
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(0)                                                           
MDATOPTX EQU   *                                                                
*                                                                               
MVALOPT  DC    AL1(MVALOPTX-MVALOPT)                                            
         DC    C'VALUE   ',C'   ',AL1(OPTNRTN+OPTITXT,0)                        
         DC    AL1(0,0)                                                         
         DC    AL1(0,0)                                                         
         DC    AL1(1,2,3,L'INODISP)                                             
         DC    AL4(OPTVALB)                                                     
         DC    AL1(OPTVALN)                                                     
         DC    AL2(OPTVALR)                                                     
         DC    AL2(INODISP-WORKD)                                               
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(0)                                                           
MVALOPTX EQU   *                                                                
*                                                                               
MACCOPT  DC    AL1(MACCOPTX-MACCOPT)                                            
         DC    C'ACCT    ',C'   ',AL1(OPTNRTN+OPTITXT,0)                        
         DC    AL1(0,0)                                                         
         DC    AL1(0,0)                                                         
         DC    AL1(2,2,3,L'INODISP)                                             
         DC    AL4(OPTACCB)                                                     
         DC    AL1(OPTACCN)                                                     
         DC    AL2(OPTACCR)                                                     
         DC    AL2(INODISP-WORKD)                                               
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(OPTAMTB+OPTMEMB)     CAN'T USE WITH AMT=/OR MEM=             
         DC    AL2(0)                                                           
MACCOPTX EQU   *                                                                
*                                                                               
MAMTOPT  DC    AL1(MAMTOPTX-MAMTOPT)                                            
         DC    C'AMOUNT  ',C'AMT',AL1(OPTNRTN+OPTITXT,0)                        
         DC    AL1(0,0)                                                         
         DC    AL1(0,0)                                                         
         DC    AL1(2,2,3,L'INODISP)                                             
         DC    AL4(OPTAMTB)                                                     
         DC    AL1(OPTAMTN)                                                     
         DC    AL2(OPTAMTR)                                                     
         DC    AL2(INODISP-WORKD)                                               
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(OPTMEMB+OPTACCB)     CAN'T USE WITH MEMO=/OR ACCT=           
         DC    AL2(0)                                                           
MAMTOPTX EQU   *                                                                
*                                                                               
MMEMOPT  DC    AL1(MMEMOPTX-MMEMOPT)                                            
         DC    C'MEMO    ',C'   ',AL1(OPTNRTN+OPTITXT,0)                        
         DC    AL1(0,0)                                                         
         DC    AL1(0,0)                                                         
         DC    AL1(1,2,3,L'INODISP)                                             
         DC    AL4(OPTMEMB)                                                     
         DC    AL1(OPTMEMN)                                                     
         DC    AL2(OPTMEMR)                                                     
         DC    AL2(INODISP-WORKD)                                               
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(OPTAMTB+OPTACCB)     CAN'T USE WITH AMT=/OR ACCT=            
         DC    AL2(0)                                                           
MMEMOPTX EQU   *                                                                
*                                                                               
OPTTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* OPTION VALIDATION TABLES                                            *         
***********************************************************************         
         SPACE 1                                                                
TBASE    DS    0H                  ** TABLE BASE **                             
*                                                                               
VALTRA   DS    0H                                                               
         DC    X'0101'                                                          
         DC    C' ',AL1(INOTRA)                                                 
         DC    X'00'                                                            
*                                                                               
VALUTRA  DS    0H                                                               
         DC    X'0101'                                                          
         DC    C' ',AL1(INOUTRA)                                                
         DC    X'00'                                                            
*                                                                               
VALSRET  DS    0H                                                               
         DC    X'0101'                                                          
         DC    C' ',AL1(INOSPRET)                                               
         DC    X'00'                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
* SELECTABLE ACTION TABLE (SEE SELTABD) -EXTENDED TABLE               *         
***********************************************************************         
         SPACE 1                                                                
SELTAB   DS    0X                                                               
         DC    C'P',AL1(RECPOST,ACTLIS,RECPOST,ACTMAI)                          
         DC    XL8'00',AL2(0),AL1(0,0,0),XL6'00'                                
         DC    C'D',AL1(RECPOST,ACTLIS,RECDPOST,ACTMAI)                         
         DC    XL8'00',AL2(0),AL1(0,0,0),XL6'00'                                
         DC    C'A',AL1(RECPOST,ACTLIS,RECAPOST,ACTMAI)                         
         DC    XL8'00',AL2(0),AL1(0,0,0),XL6'00'                                
         DC    C'I',AL1(RECPOST,ACTLIS,RECAPOS2,ACTMAI)                         
         DC    XL8'00',AL2(0),AL1(0,0,0),XL6'00'                                
         DC    C'R',AL1(RECPOST,ACTLIS,RECRPOST,ACTMAI)                         
         DC    XL8'00',AL2(0),AL1(0,0,0),XL6'00'                                
         DC    C'T',AL1(RECPOST,ACTLIS,RECTPOST,ACTMAI)                         
         DC    XL8'00',AL2(0),AL1(0,0,0),XL6'00'                                
         DC    C'S',AL1(RECPOST,ACTLIS,RECSPOST,ACTMAI)                         
         DC    XL8'00',AL2(0),AL1(0,0,0),XL6'00'                                
         DC    C'C',AL1(RECPOST,ACTLIS,RECUCPST,ACTMAI)                         
         DC    XL8'00',AL2(0),AL1(0,0,0),XL6'00'                                
         DC    C'N',AL1(RECPOST,ACTLIS,RECUNPST,ACTMAI)                         
         DC    XL8'00',AL2(0),AL1(0,0,0),XL6'00'                                
         DC    C'1',AL1(RECPOST,ACTLIS,RECUACPT,ACTMAI)                         
         DC    XL8'00',AL2(0),AL1(0,0,0),XL6'00'                                
         DC    C'2',AL1(RECPOST,ACTLIS,RECUANPT,ACTMAI)                         
         DC    XL8'00',AL2(0),AL1(0,0,0),XL6'00'                                
         DC    C'P',AL1(RECPROF,ACTLIS,RECPROF,ACTMAI)                          
         DC    XL8'00',AL2(0),AL1(0,0,0),XL6'00'                                
         DC    C'R',AL1(RECPROF,ACTLIS,RECRPROF,ACTMAI)                         
         DC    XL8'00',AL2(0),AL1(0,0,0),XL6'00'                                
         DC    C'S',AL1(RECPROF,ACTLIS,RECSPROF,ACTMAI)                         
         DC    XL8'00',AL2(0),AL1(0,0,0),XL6'00'                                
         DC    C'S',AL1(RECBILL,ACTLIS,RECBILL,ACTBDIS)                         
         DC    XL8'00',XL2'0800',AL1(0,0,0),XL6'00'                             
         DC    C'R',AL1(RECBILL,ACTLIS,RECBILL,ACTBDIS)                         
         DC    XL8'00',XL2'8000',AL1(0,0,0),XL6'00'                             
SELTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
* ACTRAWRK                                                                      
       ++INCLUDE ACTRAWRK                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014ACTRA01   03/22/13'                                      
         END                                                                    

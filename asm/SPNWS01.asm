*          DATA SET SPNWS01    AT LEVEL 111 AS OF 02/26/07                      
*PHASE T20701C,*                                                                
         TITLE 'SPNWS01 - NEW BUYERS WORKSHEET - CONTROLLER TABLES'             
T20701   CSECT                                                                  
         SPACE 2                                                                
ARECTAB  DC    AL4(RECTAB-T20701)  RECORD TABLE                                 
AACTTAB  DC    AL4(ACTTAB-T20701)  ACTION TABLE                                 
AMIXTAB  DC    AL4(MIXTAB-T20701)  REC/ACT MIX TABLE                            
AKEYTAB  DC    AL4(KEYTAB-T20701)  KEY COMPONENT TABLE                          
AOPTTAB  DC    AL4(OPTTAB-T20701)  OPTION TABLE                                 
ASELTAB  DC    AL4(SELTAB-T20701)  SELECTABLE ACTION TABLE                      
AOPTVAL  DC    AL4(TBASE-T20701)   OPTION VALIDATION TABLES                     
         EJECT                                                                  
***********************************************************************         
* RECORD TYPE TABLE (SEE RECTABD)                                     *         
***********************************************************************         
         SPACE 1                                                                
RECTAB   DS    0X                                                               
*                                                                               
RBUY     DC    AL1(RBUYX-RBUY)                                                  
         DC    C'BUY     ',C'   ',AL1(RECITXT+RECINOP)                          
         DC    AL1(0,0,RECBUY)                                                  
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(184)                                                         
RBUYX    EQU   *                                                                
*                                                                               
RBYR     DC    AL1(RBYRX-RBYR)                                                  
         DC    C'BUYER   ',C'BYR',AL1(RECITXT)                                  
         DC    AL1(0,0,RECBYR)                                                  
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(130)                                                         
RBYRX    EQU   *                                                                
*                                                                               
RCAM     DC    AL1(RCAMX-RCAM)                                                  
         DC    C'CAMPAIGN',C'   ',AL1(RECITXT)                                  
         DC    AL1(0,0,RECCAM)                                                  
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(140)                                                         
RCAMX    EQU   *                                                                
*                                                                               
RWRK     DC    AL1(RWRKX-RWRK)                                                  
         DC    C'WORK    ',C'WRK',AL1(RECITXT+RECINOP)                          
         DC    AL1(0,0,RECWRK)                                                  
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(150)                                                         
RWRKX    EQU   *                                                                
*                                                                               
RSID     DC    AL1(RSIDX-RSID)     ** INTERNAL USE ONLY **                      
         DC    C'WORK    ',C'   ',AL1(RECINOH)                                  
         DC    AL1(0,0,RECSID)                                                  
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
RSIDX    EQU   *                                                                
*                                                                               
RPKG     DC    AL1(RPKGX-RPKG)                                                  
         DC    C'PACKAGE ',C'PKG',AL1(RECITXT)                                  
         DC    AL1(0,0,RECPKG)                                                  
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(170)                                                         
RPKGX    EQU   *                                                                
*                                                                               
RORB     DC    AL1(RORBX-RORB)                                                  
         DC    C'ORBIT   ',C'   ',AL1(RECITXT)                                  
         DC    AL1(0,0,RECORB)                                                  
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(180)                                                         
RORBX    EQU   *                                                                
*                                                                               
RSPL     DC    AL1(RSPLX-RSPL)                                                  
         DC    C'NETWORK ',C'   ',AL1(RECITXT)                                  
         DC    AL1(0,0,RECNET)                                                  
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(190)                                                         
RSPLX    EQU   *                                                                
*                                                                               
RTST     DC    AL1(RTSTX-RTST)                                                  
         DC    C'TEST    ',C'TST',AL1(RECITXT+RECINOP+RECIDDS)                  
         DC    AL1(0,0,RECTST)                                                  
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(150)                                                         
RTSTX    EQU   *                                                                
*                                                                               
RCOM     DC    AL1(RCOMX-RCOM)                                                  
         DC    C'COMMENT ',C'   ',AL1(RECITXT+RECINOP+RECIDDS)                  
         DC    AL1(0,0,RECCOM)                                                  
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(215)                                                         
RCOMX    EQU   *                                                                
*                                                                               
RGOAL    DC    AL1(RGOALX-RGOAL)                                                
         DC    C'GOAL    ',C'   ',AL1(RECITXT)                                  
         DC    AL1(0,0,RECGOL)                                                  
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(216)                                                         
RGOALX   EQU   *                                                                
*                               ADDED HERE SO 'COM' STILL MEANS COMMENT         
RCMB     DC    AL1(RCMBX-RCMB)  SYNONYM FOR NETWORK                             
         DC    C'COMBIN  ',C'   ',AL1(RECITXT)                                  
         DC    AL1(0,0,RECNET)                                                  
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(190)                                                         
RCMBX    EQU   *                                                                
*                                                                               
RECTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* ACTION TABLE (SEE ACTTABD)                                          *         
***********************************************************************         
         SPACE 1                                                                
ACTTAB   DS    0X                                                               
*                                                                               
         DC    C'ADD     ',AL1(0,0,0,ACTADD)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DC    C'DISPLAY ',AL1(0,0,0,ACTDIS)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DC    C'CHANGE  ',AL1(0,0,0,ACTCHA)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DC    C'DELETE  ',AL1(0,0,0,ACTDEL)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DC    C'RESTORE ',AL1(0,0,0,ACTRES)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DC    C'DUP     ',AL1(0,0,0,ACTDUP)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DC    C'UPDATE  ',AL1(0,0,0,ACTUDT)                                    
         DC    AL4(0,0)                                                         
         DC    XL7'00',X'01'                                                    
*                                                                               
         DC    C'SKED    ',AL1(0,0,0,ACTSKD)                                    
         DC    AL4(0,0)                                                         
         DC    XL7'00',X'02'                                                    
*                                                                               
         DC    C'SSKED   ',AL1(0,0,0,ACTSSK)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DC    C'X+CHANGE',AL1(0,0,0,ACTTFR)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DC    C'ESTIMATE',AL1(0,0,0,ACTEST)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DC    C'TRANSFER',AL1(0,0,0,ACTXFR)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DC    C'DEMO    ',AL1(0,0,0,ACTDEM)                                    
         DC    AL4(0,0)                                                         
         DC    XL7'00',X'03'                                                    
*                                                                               
         DC    C'LIST    ',AL1(0,0,0,ACTLST)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DC    C'REPORT  ',AL1(0,0,0,ACTREP)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DC    C'UPREPORT',AL1(0,0,0,ACTUPR)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DC    C'RECAP   ',AL1(0,0,0,ACTDRP)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DC    C'COPY    ',AL1(0,0,0,ACTCCP)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DC    C'COMP    ',AL1(0,0,0,ACTCOM)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DC    C'ERASE   ',AL1(0,0,0,ACTERA)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DC    C'FIX     ',AL1(0,0,0,ACTFIX)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DC    C'SPILL   ',AL1(0,0,0,ACTSPL)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DC    C'SEND    ',AL1(0,0,0,ACTSEN)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DC    C'SELECT  ',AL1(0,0,0,ACTSEL)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DC    C'XMIT    ',AL1(0,0,0,ACTTRA)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DC    C'X2      ',AL1(0,0,0,ACTTRB)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DC    C'MSPILL  ',AL1(0,0,0,ACTMKT)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DC    C'CALL    ',AL1(0,0,0,ACTCAL)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DC    C'LILO    ',AL1(0,0,0,ACTLED)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DC    C'ROT     ',AL1(0,0,0,ACTROT)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DC    C'AUDCOMP ',AL1(0,0,0,ACTAUD)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DC    C'BUYADD  ',AL1(0,0,0,ACTBYA)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DC    C'!@#$    ',AL1(0,0,0,ACTRLD)                                    
         DC    AL4(0,0)                                                         
         DC    AL1(ACTCHA,0,0,0)                                                
         DC    AL1(0,0,0,0)                                                     
*                                                                               
ACTTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* RECORD/ACTION COMBO TABLE (SEE MIXTABD)                             *         
***********************************************************************         
         SPACE 1                                                                
MIXTAB   DS    0X                                                               
*                                                                               
MBYRADD  DC    AL1(MBYRADDX-MBYRADD)                                            
         DC    AL1(RECBYR,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXDREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'FE02'                                                          
         DC    AL1(2,2,KEYMED,KEYBYR,0,0,0,0,0,0)                               
         DC    AL4(0,OPTNOGB+OPTRTGB+OPTRNKB+OPTSIDB)                           
         DC    XL8'00'                                                          
         DC    AL2(131)                                                         
MBYRADDX EQU   *                                                                
*                                                                               
MBYRDIS  DC    AL1(MBYRDISX-MBYRDIS)                                            
         DC    AL1(RECBYR,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'FE02'                                                          
         DC    AL1(2,2,KEYMED,KEYBYR,0,0,0,0,0,0)                               
         DC    AL4(0,OPTNOGB+OPTRTGB+OPTRNKB+OPTSIDB)                           
         DC    XL8'00'                                                          
         DC    AL2(132)                                                         
MBYRDISX EQU   *                                                                
*                                                                               
MBYRCHA  DC    AL1(MBYRCHAX-MBYRCHA)                                            
         DC    AL1(RECBYR,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'FE02'                                                          
         DC    AL1(2,2,KEYMED,KEYBYR,0,0,0,0,0,0)                               
         DC    AL4(0,OPTNOGB+OPTRTGB+OPTRNKB+OPTSIDB)                           
         DC    XL8'00'                                                          
         DC    AL2(133)                                                         
MBYRCHAX EQU   *                                                                
*                                                                               
MBYRLST  DC    AL1(MBYRLSTX-MBYRLST)                                            
         DC    AL1(RECBYR,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'DE02'                                                          
         DC    AL1(2,2,KEYMED,KEYBYR,0,0,0,0,0,0)                               
         DC    AL4(0,OPTNOGB+OPTRTGB+OPTRNKB+OPTSIDB)                           
         DC    XL8'00'                                                          
         DC    AL2(134)                                                         
MBYRLSTX EQU   *                                                                
*                                                                               
MBYRSEL  DC    AL1(MBYRSELX-MBYRSEL)                                            
         DC    AL1(RECBYR,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'FE02'                                                          
         DC    AL1(2,2,KEYMED,KEYBYR,0,0,0,0,0,0)                               
         DC    AL4(0,OPTNOGB+OPTRTGB+OPTRNKB+OPTSIDB)                           
         DC    XL8'00'                                                          
         DC    AL2(135)                                                         
MBYRSELX EQU   *                                                                
*                                                                               
MCAMADD  DC    AL1(MCAMADDX-MCAMADD)                                            
         DC    AL1(RECCAM,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXDREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'FD03'                                                          
         DC    AL1(3,3,KEYMED,KEYBYR,KEYCAM,0,0,0,0,0)                          
         DC    AL4(0,OPTNOGB+OPTRTGB+OPTRNKB+OPTSIDB)                           
         DC    XL8'00'                                                          
         DC    AL2(141)                                                         
MCAMADDX EQU   *                                                                
*                                                                               
MCAMDIS  DC    AL1(MCAMDISX-MCAMDIS)                                            
         DC    AL1(RECCAM,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'FD03'                                                          
         DC    AL1(3,3,KEYMED,KEYBYR,KEYCAM,0,0,0,0,0)                          
         DC    AL4(0,OPTNOGB+OPTRTGB+OPTRNKB+OPTSIDB)                           
         DC    XL8'00'                                                          
         DC    AL2(142)                                                         
MCAMDISX EQU   *                                                                
*                                                                               
MCAMCHA  DC    AL1(MCAMCHAX-MCAMCHA)                                            
         DC    AL1(RECCAM,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'FD03'                                                          
         DC    AL1(3,3,KEYMED,KEYBYR,KEYCAM,0,0,0,0,0)                          
         DC    AL4(0,OPTNOGB+OPTRTGB+OPTRNKB+OPTSIDB)                           
         DC    XL8'00'                                                          
         DC    AL2(143)                                                         
MCAMCHAX EQU   *                                                                
*                                                                               
MCAMLST  DC    AL1(MCAMLSTX-MCAMLST)                                            
         DC    AL1(RECCAM,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'DD03'                                                          
         DC    AL1(5,2,KEYMED,KEYBYR,0,0,0,0,0,0)                               
         DC    AL4(0,OPTNOGB+OPTRTGB+OPTRNKB+OPTSIDB)                           
         DC    XL8'00'                                                          
         DC    AL2(144)                                                         
MCAMLSTX EQU   *                                                                
*                                                                               
MCAMSEL  DC    AL1(MCAMSELX-MCAMSEL)                                            
         DC    AL1(RECCAM,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'FD03'                                                          
         DC    AL1(3,3,KEYMED,KEYBYR,KEYCAM,0,0,0,0,0)                          
         DC    AL4(0,OPTNOGB+OPTRTGB+OPTRNKB+OPTSIDB)                           
         DC    XL8'00'                                                          
         DC    AL2(145)                                                         
MCAMSELX EQU   *                                                                
*                                                                               
MCAMREP  DC    AL1(MCAMREPX-MCAMREP)                                            
         DC    AL1(RECCAM,ACTREP)                                               
         DC    AL1(MIXIREP,MIXIOKN+MIXIOKO+MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'ED03'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,OPTNOGB+OPTRTGB+OPTRNKB+OPTSIDB)                           
         DC    C'SPCLWK  '                                                      
         DC    AL2(146)                                                         
MCAMREPX EQU   *                                                                
*                                                                               
MCAMCPY  DC    AL1(MCAMCPYX-MCAMCPY)                                            
         DC    AL1(RECCAM,ACTCCP)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F611'                                                          
         DC    AL1(4,4,KEYMED,KEYBYR,KEYCAM,KEYMKT,0,0,0,0)                     
         DC    AL4(0,OPTNOGB+OPTRTGB+OPTRNKB+OPTSIDB)                           
         DC    AL1(ACTLFM8),XL7'00'                                             
         DC    AL2(147)                                                         
MCAMCPYX EQU   *                                                                
*                                                                               
MCOMADD  DC    AL1(MCOMADDX-MCOMADD)                                            
         DC    AL1(RECCOM,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXDREQ+MIXIDDS,MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'DB19'                                                          
         DC    AL1(5,5,KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYSTA,0,0,0)                
         DC    AL4(0,OPTSIDB+OPTPRGB+OPTRTGB+OPTDEMB+OPTPRDB+OPTRNKB+OPT        
               TNCSB+OPTNOGB+OPTCPMB+OPTDPTB+OPTSKDB)                           
         DC    XL8'00'                                                          
         DC    AL2(210)                                                         
MCOMADDX EQU   *                                                                
*                                                                               
MCOMDIS  DC    AL1(MCOMDISX-MCOMDIS)                                            
         DC    AL1(RECCOM,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'DB19'                                                          
         DC    AL1(5,5,KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYSTA,0,0,0)                
         DC    AL4(0,OPTSIDB+OPTPRGB+OPTRTGB+OPTDEMB+OPTPRDB+OPTRNKB+OPT        
               TNCSB+OPTNOGB+OPTCPMB+OPTDPTB+OPTSKDB)                           
         DC    XL8'00'                                                          
         DC    AL2(211)                                                         
MCOMDISX EQU   *                                                                
*                                                                               
MCOMCHA  DC    AL1(MCOMCHAX-MCOMCHA)                                            
         DC    AL1(RECCOM,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'DB19'                                                          
         DC    AL1(5,5,KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYSTA,0,0,0)                
         DC    AL4(0,OPTSIDB+OPTPRGB+OPTRTGB+OPTDEMB+OPTPRDB+OPTRNKB+OPT        
               TNCSB+OPTNOGB+OPTCPMB+OPTDPTB+OPTSKDB)                           
         DC    XL8'00'                                                          
         DC    AL2(212)                                                         
MCOMCHAX EQU   *                                                                
*                                                                               
MCOMDEL  DC    AL1(MCOMDELX-MCOMDEL)                                            
         DC    AL1(RECCOM,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'DB19'                                                          
         DC    AL1(5,5,KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYSTA,0,0,0)                
         DC    AL4(0,OPTSIDB+OPTPRGB+OPTRTGB+OPTDEMB+OPTPRDB+OPTRNKB+OPT        
               TNCSB+OPTNOGB+OPTCPMB+OPTDPTB+OPTSKDB)                           
         DC    XL8'00'                                                          
         DC    AL2(213)                                                         
MCOMDELX EQU   *                                                                
*                                                                               
MCOMRES  DC    AL1(MCOMRESX-MCOMRES)                                            
         DC    AL1(RECCOM,ACTRES)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'DB19'                                                          
         DC    AL1(5,5,KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYSTA,0,0,0)                
         DC    AL4(0,OPTSIDB+OPTPRGB+OPTRTGB+OPTDEMB+OPTPRDB+OPTRNKB+OPT        
               TNCSB+OPTNOGB+OPTCPMB+OPTDPTB+OPTSKDB)                           
         DC    XL8'00'                                                          
         DC    AL2(214)                                                         
MCOMRESX EQU   *                                                                
*                                                                               
MWRKUPD  DC    AL1(MWRKUPDX-MWRKUPD)                                            
         DC    AL1(RECWRK,ACTUDT)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'FB05'                                                          
         DC    AL1(0,5,KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDPL,0,0,0)                
         DC    AL4(0,OPTPRDB+OPTDPTB+OPTSKDB)                                   
         DC    XL8'00'                                                          
         DC    AL2(151)                                                         
MWRKUPDX EQU   *                                                                
*                                                                               
MWRKSEL  DC    AL1(MWRKSELX-MWRKSEL)                                            
         DC    AL1(RECWRK,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'0000'                                                          
         DC    AL1(8,7,KEYMED,KEYBYR,KEYCAM,KEYMKT)                             
         DC    AL1(KEYDAY,KEYTIM,KEYDPL,0)                                      
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(152)                                                         
MWRKSELX EQU   *                                                                
*                                                                               
MWRKADD  DC    AL1(MWRKADDX-MWRKADD)                                            
         DC    AL1(RECWRK,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXDREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'FC04'                                                          
         DC    AL1(8,7,KEYMED,KEYBYR,KEYCAM,KEYMKT)                             
         DC    AL1(KEYDAY,KEYTIM,KEYDPL,0)                                      
         DC    AL4(0,OPTSIDB+OPTPRGB+OPTRTGB+OPTDEMB+OPTPRDB+OPTRNKB+OPT        
               TNCSB+OPTNOGB+OPTCPMB+OPTDPTB+OPTSKDB)                           
         DC    XL8'00'                                                          
         DC    AL2(153)                                                         
MWRKADDX EQU   *                                                                
*                                                                               
MWRKDIS  DC    AL1(MWRKDISX-MWRKDIS)                                            
         DC    AL1(RECWRK,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'FC04'                                                          
         DC    AL1(8,7,KEYMED,KEYBYR,KEYCAM,KEYMKT)                             
         DC    AL1(KEYDAY,KEYTIM,KEYDPL,0)                                      
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(154)                                                         
MWRKDISX EQU   *                                                                
*                                                                               
MWRKCHA  DC    AL1(MWRKCHAX-MWRKCHA)                                            
         DC    AL1(RECWRK,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'FC04'                                                          
         DC    AL1(8,7,KEYMED,KEYBYR,KEYCAM,KEYMKT)                             
         DC    AL1(KEYDAY,KEYTIM,KEYDPL,0)                                      
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(155)                                                         
MWRKCHAX EQU   *                                                                
*                                                                               
MWRKDEL  DC    AL1(MWRKDELX-MWRKDEL)                                            
         DC    AL1(RECWRK,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'FC04'                                                          
         DC    AL1(8,7,KEYMED,KEYBYR,KEYCAM,KEYMKT)                             
         DC    AL1(KEYDAY,KEYTIM,KEYDPL,0)                                      
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(156)                                                         
MWRKDELX EQU   *                                                                
*                                                                               
MWRKERA  DC    AL1(MWRKERAX-MWRKERA)                                            
         DC    AL1(RECWRK,ACTERA)                                               
         DC    AL1(MIXISEL,0)                                                   
         DC    AL1(0,0)                                                         
         DC    X'0004'                                                          
         DC    AL1(8,7,KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDAY)                      
         DC    AL1(KEYTIM,KEYDPL,0)                                             
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
MWRKERAX EQU   *                                                                
*                                                                               
MWRKDUP  DC    AL1(MWRKDUPX-MWRKDUP)                                            
         DC    AL1(RECWRK,ACTDUP)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'EC04'                                                          
         DC    AL1(8,7,KEYMED,KEYBYR,KEYCAM,KEYMKT)                             
         DC    AL1(KEYDAY,KEYTIM,KEYDPL,0)                                      
         DC    AL4(0,OPTSIDB+OPTPRGB+OPTDEMB+OPTPRDB+OPTRNKB+OPTNCSB+OPT        
               TNOGB+OPTCPMB+OPTDPTB+OPTSKDB)                                   
         DC    XL8'00'                                                          
         DC    AL2(183)                                                         
MWRKDUPX EQU   *                                                                
*                                                                               
MWRKSKD  DC    AL1(MWRKSKDX-MWRKSKD)                                            
         DC    AL1(RECWRK,ACTSKD)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'FB05'                                                          
         DC    AL1(0,5,KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDPL,0,0,0)                
         DC    AL4(0,OPTSIDB+OPTPRGB+OPTPRDB+OPTNCSB)                           
         DC    XL8'00'                                                          
         DC    AL2(157)                                                         
MWRKSKDX EQU   *                                                                
*                                                                               
MWRKDEM  DC    AL1(MWRKDEMX-MWRKDEM)                                            
         DC    AL1(RECWRK,ACTDEM)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'FB05'                                                          
         DC    AL1(0,5,KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDPL,0,0,0)                
         DC    AL4(0,OPTPRDB+OPTRNKB+OPTDPTB+OPTSKDB)                           
         DC    XL8'00'                                                          
         DC    AL2(158)                                                         
MWRKDEMX EQU   *                                                                
*                                                                               
MWRKSSK  DC    AL1(MWRKSSKX-MWRKSSK)                                            
         DC    AL1(RECWRK,ACTSSK)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'FA05'                                                          
         DC    AL1(0,5,KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDPL,0,0,0)                
         DC    AL4(0,OPTSIDB+OPTPRGB+OPTRTGB+OPTPRDB+OPTNCSB+OPTRNKB+OPT        
               TCPMB)                                                           
         DC    XL8'00'                                                          
         DC    AL2(159)                                                         
MWRKSSKX EQU   *                                                                
*                                                                               
MWRKXFR  DC    AL1(MWRKXFRX-MWRKXFR)                                            
         DC    AL1(RECWRK,ACTXFR)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'0005'                                                          
         DC    AL1(0,5,KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDPL,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(160)                                                         
MWRKXFRX EQU   *                                                                
*                                                                               
MWRKBYA  DC    AL1(MWRKBYAX-MWRKBYA)                                            
         DC    AL1(RECWRK,ACTBYA)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'0005'                                                          
         DC    AL1(0,5,KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDPL,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(284)                                                         
MWRKBYAX EQU   *                                                                
*                                                                               
MWRKRCP  DC    AL1(MWRKRCPX-MWRKRCP)                                            
         DC    AL1(RECWRK,ACTDRP)                                               
         DC    AL1(MIXIREP+MIXKREQ,MIXIOKN+MIXITXT+MIXIDRV)                     
         DC    AL1(0,0)                                                         
         DC    X'F910'                                                          
         DC    AL1(0,5,KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDPL,0,0,0)                
         DC    AL4(0,OPTSIDB+OPTPRGB+OPTRTGB+OPTPRDB+OPTRNKB+OPTNCSB+OPT        
               TCPMB+OPTSKDB)                                                   
         DC    XL7'00'                                                          
         DC    AL1(MIXIDRLS)                                                    
         DC    AL2(161)                                                         
MWRKRCPX EQU   *                                                                
*                                                                               
MWRKLST  DC    AL1(MWRKLSTX-MWRKLST)                                            
         DC    AL1(RECWRK,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'DC16'                                                          
         DC    AL1(3,3,KEYMED,KEYBYR,KEYCAM,0,0,0,0,0)                          
         DC    AL4(0,OPTSIDB+OPTPRGB+OPTDEMB+OPTRTGB+OPTPRDB+OPTRNKB+OPT        
               TNCSB+OPTNOGB+OPTCPMB+OPTDPTB+OPTSKDB)                           
         DC    XL8'00'                                                          
         DC    AL2(162)                                                         
MWRKLSTX EQU   *                                                                
*                                                                               
MWRKREP  DC    AL1(MWRKREPX-MWRKREP)                                            
         DC    AL1(RECWRK,ACTREP)                                               
         DC    AL1(MIXIREP,MIXIOKN+MIXIOKO+MIXIOKS+MIXITXT+MIXIDRV)             
         DC    AL1(0,0)                                                         
         DC    X'F820'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,OPTSIDB+OPTPRGB+OPTRTGB+OPTDEMB+OPTPRDB+OPTRNKB+OPT        
               TNCSB+OPTNOGB+OPTCPMB+OPTDPTB+OPTSKDB)                           
         DC    C'SPWLWK  '                                                      
         DC    AL2(163)                                                         
MWRKREPX EQU   *                                                                
*                                                                               
MWRKUPR  DC    AL1(MWRKUPRX-MWRKUPR)                                            
         DC    AL1(RECWRK,ACTUPR)                                               
         DC    AL1(MIXIREP,MIXIOKN+MIXIOKS+MIXIOKO+MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'EF21'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,OPTSIDB+OPTPRGB+OPTRTGB+OPTDEMB+OPTPRDB+OPTRNKB+OPT        
               TNCSB+OPTNOGB+OPTCPMB+OPTDPTB+OPTSKDB)                           
         DC    C'SPWUWK  '                                                      
         DC    AL2(164)                                                         
MWRKUPRX EQU   *                                                                
*                                                                               
MWRKEST  DC    AL1(MWRKESTX-MWRKEST)                                            
         DC    AL1(RECWRK,ACTEST)                                               
         DC    AL1(MIXILFM,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'F712'                                                          
         DC    AL1(7,7,KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDAY)                      
         DC    AL1(KEYTIM,KEYDPL,0)                                             
         DC    AL4(0,0)                                                         
         DC    AL1(ACTLFM8),XL7'00'                                             
         DC    AL2(165)                                                         
MWRKESTX EQU   *                                                                
*                                                                               
MWRKCOM  DC    AL1(MWRKCOMX-MWRKCOM)                                            
         DC    AL1(RECWRK,ACTCOM)                                               
         DC    AL1(MIXILFM,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'F514'                                                          
         DC    AL1(7,7,KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDAY)                      
         DC    AL1(KEYTIM,KEYDPL,0)                                             
         DC    AL4(0,0)                                                         
         DC    AL1(ACTLFM8),XL7'00'                                             
         DC    AL2(166)                                                         
MWRKCOMX EQU   *                                                                
*                                                                               
MWRKMKT  DC    AL1(MWRKMKTX-MWRKMKT)                                            
         DC    AL1(RECWRK,ACTMKT)                                               
         DC    AL1(MIXILFM,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'E515'                                                          
         DC    AL1(7,7,KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDAY)                      
         DC    AL1(KEYTIM,KEYDPL,0)                                             
         DC    AL4(0,0)                                                         
         DC    AL1(ACTLFM8),XL7'00'                                             
         DC    AL2(173)                                                         
MWRKMKTX EQU   *                                                                
*                                                                               
MWRKLED  DC    AL1(MWRKLEDX-MWRKLED)                                            
         DC    AL1(RECWRK,ACTLED)                                               
         DC    AL1(MIXILFM,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'E828'                                                          
         DC    AL1(7,7,KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDAY)                      
         DC    AL1(KEYTIM,KEYDPL,0)                                             
         DC    AL4(0,0)                                                         
         DC    AL1(ACTLFM8),XL7'00'                                             
         DC    AL2(174)                                                         
MWRKLEDX EQU   *                                                                
*                                                                               
MWRKROT  DC    AL1(MWRKROTX-MWRKROT)                                            
         DC    AL1(RECWRK,ACTROT)                                               
         DC    AL1(MIXILFM,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'E928'                                                          
         DC    AL1(7,7,KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDAY)                      
         DC    AL1(KEYTIM,KEYDPL,0)                                             
         DC    AL4(0,0)                                                         
         DC    AL1(ACTLFM8),XL7'00'                                             
         DC    AL2(175)                                                         
MWRKROTX EQU   *                                                                
*                                                                               
MWRKSPL  DC    AL1(MWRKSPLX-MWRKSPL)                                            
         DC    AL1(RECWRK,ACTSPL)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'0005'                                                          
         DC    AL1(0,4,KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDPL,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(167)                                                         
MWRKSPLX EQU   *                                                                
*                                                                               
MWRKSEN  DC    AL1(MWRKSENX-MWRKSEN)                                            
         DC    AL1(RECWRK,ACTSEN)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'0005'                                                          
         DC    AL1(0,4,KEYMED,KEYBYR,KEYCAM,KEYMKT,0,0,0,0)                     
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(168)                                                         
MWRKSENX EQU   *                                                                
*                                                                               
MWRKTRA  DC    AL1(MWRKTRAX-MWRKTRA)                                            
         DC    AL1(RECWRK,ACTTRA)                                               
         DC    AL1(MIXIREP,MIXIOKN+MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'0017'                                                          
         DC    AL1(0,4,KEYMED,KEYBYR,KEYCAM,KEYMKT,0,0,0,0)                     
         DC    AL4(0,0)                                                         
         DC    C'SPTRWK  '                                                      
         DC    AL2(169)                                                         
MWRKTRAX EQU   *                                                                
*                                                                               
MWRKTRB  DC    AL1(MWRKTRBX-MWRKTRB)                                            
         DC    AL1(RECWRK,ACTTRB)                                               
         DC    AL1(MIXIREP,MIXIOKN+MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'0017'                                                          
         DC    AL1(0,4,KEYMED,KEYBYR,KEYCAM,KEYMKT,0,0,0,0)                     
         DC    AL4(0,0)                                                         
         DC    C'SPTRWK  '                                                      
         DC    AL2(169)                                                         
MWRKTRBX EQU   *                                                                
*                                                                               
MWRKFIX  DC    AL1(MWRKFIXX-MWRKFIX)                                            
         DC    AL1(RECWRK,ACTFIX)                                               
         DC    AL1(MIXILST,0)                                                   
         DC    AL1(0,0)                                                         
         DC    X'0005'                                                          
         DC    AL1(0,4,KEYMED,KEYBYR,KEYCAM,KEYMKT,0,0,0,0)                     
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
MWRKFIXX EQU   *                                                                
*                                                                               
MWRKRLD  DC    AL1(MWRKRLDX-MWRKRLD)                                            
         DC    AL1(RECWRK,ACTRLD)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'FC31'                                                          
         DC    AL1(4,4,KEYMED,KEYBYR,KEYCAM,KEYMKT,0,0,0,0)                     
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(156)                                                         
MWRKRLDX EQU   *                                                                
*                                                                               
MWRKTST  DC    AL1(MWRKTSTX-MWRKTST)                                            
         DC    AL1(RECTST,ACTDRP)                                               
         DC    AL1(MIXIREP+MIXIDDS,MIXIOKN+MIXITXT+MIXIDRV)                     
         DC    AL1(0,0)                                                         
         DC    X'F199'                                                          
         DC    AL4(0,0)                                                         
         DC    AL4(0,0)                                                         
         DC    XL7'00'                                                          
         DC    AL1(MIXIDRLS)                                                    
         DC    AL2(161)                                                         
MWRKTSTX EQU   *                                                                
*                                                                               
MWRKCAL  DC    AL1(MWRKCALX-MWRKCAL)                                            
         DC    AL1(RECWRK,ACTCAL)                                               
         DC    AL1(MIXILST+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'0005'                                                          
         DC    AL1(0,4,KEYMED,KEYBYR,KEYCAM,KEYMKT,0,0,0,0)                     
         DC    AL4(OPTSTAB,0)                                                   
         DC    XL8'00'                                                          
         DC    CL18'CALL LETTER CHANGE'                                         
MWRKCALX EQU   *                                                                
*                                                                               
MWRKAUD  DC    AL1(MWRKAUDX-MWRKAUD)                                            
         DC    AL1(RECWRK,ACTAUD)                                               
         DC    AL1(MIXILFM,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'D322'                                                          
         DC    AL1(7,7,KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDAY)                      
         DC    AL1(KEYTIM,KEYDPL,0)                                             
         DC    AL4(0,0)                                                         
         DC    AL1(ACTLFM8),XL7'00'                                             
         DC    AL2(278)                                                         
MWRKAUDX EQU   *                                                                
*                                                                               
MSIDSEL  DC    AL1(MSIDSELX-MSIDSEL)                                            
         DC    AL1(RECSID,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'0000'                                                          
         DC    AL1(8,7,KEYMED,KEYBYR,KEYCAM,KEYMKT)                             
         DC    AL1(KEYDAY,KEYTIM,KEYDPL,0)                                      
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(152)                                                         
MSIDSELX EQU   *                                                                
*                                                                               
MSIDTFR  DC    AL1(MSIDTFRX-MSIDTFR)                                            
         DC    AL1(RECSID,ACTTFR)                                               
         DC    AL1(MIXISEL,0)                                                   
         DC    AL1(0,0)                                                         
         DC    X'FC04'                                                          
         DC    AL1(7,7,KEYMED,KEYBYR,KEYCAM,KEYSTA,KEYDPL)                      
         DC    AL1(KEYDAY,KEYTIM,0)                                             
         DC    AL4(0,0)                                                         
         DC    AL1(ACTCHA),XL7'00'                                              
MSIDTFRX EQU   *                                                                
*                                                                               
MSIDXFR  DC    AL1(MSIDXFRX-MSIDXFR)                                            
         DC    AL1(RECSID,ACTXFR)                                               
         DC    AL1(MIXISEL,0)                                                   
         DC    AL1(0,0)                                                         
         DC    X'FB05'                                                          
         DC    AL1(7,7,KEYMED,KEYBYR,KEYCAM,KEYSTA,KEYDPL)                      
         DC    AL1(KEYDAY,KEYTIM,0)                                             
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
MSIDXFRX EQU   *                                                                
*                                                                               
MSIDEST  DC    AL1(MSIDESTX-MSIDEST)                                            
         DC    AL1(RECSID,ACTEST)                                               
         DC    AL1(MIXILFM,0)                                                   
         DC    AL1(0,0)                                                         
         DC    X'F712'                                                          
         DC    AL1(7,7,KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDPL)                      
         DC    AL1(KEYDAY,KEYTIM,0)                                             
         DC    AL4(0,0)                                                         
         DC    AL1(ACTLFM8),XL7'00'                                             
MSIDESTX EQU   *                                                                
*                                                                               
MSIDCOM  DC    AL1(MSIDCOMX-MSIDCOM)                                            
         DC    AL1(RECSID,ACTCOM)                                               
         DC    AL1(MIXILFM,0)                                                   
         DC    AL1(0,0)                                                         
         DC    X'F514'                                                          
         DC    AL1(7,7,KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDPL)                      
         DC    AL1(KEYDAY,KEYTIM,0)                                             
         DC    AL4(0,0)                                                         
         DC    AL1(ACTLFM8),XL7'00'                                             
MSIDCOMX EQU   *                                                                
*                                                                               
MSIDMKT  DC    AL1(MSIDMKTX-MSIDMKT)                                            
         DC    AL1(RECSID,ACTMKT)                                               
         DC    AL1(MIXILFM,0)                                                   
         DC    AL1(0,0)                                                         
         DC    X'E515'                                                          
         DC    AL1(7,7,KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDPL)                      
         DC    AL1(KEYDAY,KEYTIM,0)                                             
         DC    AL4(0,0)                                                         
         DC    AL1(ACTLFM8),XL7'00'                                             
MSIDMKTX EQU   *                                                                
*                                                                               
MPKGADD  DC    AL1(MPKGADDX-MPKGADD)                                            
         DC    AL1(RECPKG,ACTADD)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'F405'                                                          
         DC    AL1(0,5,KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDPL,0,0,0)                
         DC    AL4(0,OPTSIDB)                                                   
         DC    XL8'00'                                                          
         DC    AL2(171)                                                         
MPKGADDX EQU   *                                                                
*                                                                               
MPKGDIS  DC    AL1(MPKGDISX-MPKGDIS)                                            
         DC    AL1(RECPKG,ACTDIS)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'F405'                                                          
         DC    AL1(0,5,KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDPL,0,0,0)                
         DC    AL4(0,OPTSIDB)                                                   
         DC    XL8'00'                                                          
         DC    AL2(172)                                                         
MPKGDISX EQU   *                                                                
*                                                                               
MPKGCHA  DC    AL1(MPKGCHAX-MPKGCHA)                                            
         DC    AL1(RECPKG,ACTCHA)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'F405'                                                          
         DC    AL1(0,5,KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDPL,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(172)                                                         
MPKGCHAX EQU   *                                                                
*                                                                               
MORBADD  DC    AL1(MORBADDX-MORBADD)                                            
         DC    AL1(RECORB,ACTADD)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'F205'                                                          
         DC    AL1(0,5,KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDPL,0,0,0)                
         DC    AL4(0,OPTSIDB)                                                   
         DC    XL8'00'                                                          
         DC    AL2(181)                                                         
MORBADDX EQU   *                                                                
*                                                                               
MORBDIS  DC    AL1(MORBDISX-MORBDIS)                                            
         DC    AL1(RECORB,ACTDIS)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'F205'                                                          
         DC    AL1(0,5,KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDPL,0,0,0)                
         DC    AL4(0,OPTSIDB)                                                   
         DC    XL8'00'                                                          
         DC    AL2(182)                                                         
MORBDISX EQU   *                                                                
*                                                                               
MORBCHA  DC    AL1(MORBCHAX-MORBCHA)                                            
         DC    AL1(RECORB,ACTCHA)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'F205'                                                          
         DC    AL1(0,5,KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDPL,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(182)                                                         
MORBCHAX EQU   *                                                                
*                                                                               
MNETDIS  DC    AL1(MNETDISX-MNETDIS)                                            
         DC    AL1(RECNET,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F318'                                                          
         DC    AL1(4,4,KEYMED,KEYBYR,KEYCAM,KEYMKT,0,0,0,0)                     
         DC    AL4(0,OPTNOGB+OPTRTGB+OPTRNKB+OPTSIDB)                           
         DC    XL8'00'                                                          
         DC    AL2(191)                                                         
MNETDISX EQU   *                                                                
*                                                                               
MNETADD  DC    AL1(MNETADDX-MNETADD)                                            
         DC    AL1(RECNET,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F318'                                                          
         DC    AL1(4,4,KEYMED,KEYBYR,KEYCAM,KEYMKT,0,0,0,0)                     
         DC    AL4(0,OPTNOGB+OPTRTGB+OPTRNKB+OPTSIDB)                           
         DC    XL8'00'                                                          
         DC    AL2(191)                                                         
MNETADDX EQU   *                                                                
*                                                                               
MNETDEL  DC    AL1(MNETDELX-MNETDEL)                                            
         DC    AL1(RECNET,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F318'                                                          
         DC    AL1(4,4,KEYMED,KEYBYR,KEYCAM,KEYMKT,0,0,0,0)                     
         DC    AL4(0,OPTNOGB+OPTRTGB+OPTRNKB+OPTSIDB)                           
         DC    XL8'00'                                                          
         DC    AL2(191)                                                         
MNETDELX EQU   *                                                                
*                                                                               
MGOLADD  DC    AL1(MGOLADDX-MGOLADD)                                            
         DC    AL1(RECGOL,ACTADD)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'0005'                                                          
         DC    AL1(0,4,KEYMED,KEYBYR,KEYCAM,KEYMKT,0,0,0,0)                     
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(203)                                                         
MGOLADDX EQU   *                                                                
*                                                                               
MGOLLST  DC    AL1(MGOLLSTX-MGOLLST)                                            
         DC    AL1(RECGOL,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'DA26'                                                          
         DC    AL1(3,3,KEYMED,KEYBYR,KEYCAM,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(217)                                                         
MGOLLSTX EQU   *                                                                
*                                                                               
MBUYUPD  DC    AL1(MBUYUPDX-MBUYUPD)                                            
         DC    AL1(RECBUY,ACTUDT)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'D535'                                                          
         DC    AL1(0,4,KEYMED,KEYBYR,KEYCAM,KEYMKT,0,0,0,0)                     
         DC    AL4(0,OPTDPTB+OPTSKDB)                                           
         DC    XL8'00'                                                          
         DC    AL2(151)                                                         
MBUYUPDX EQU   *                                                                
*                                                                               
MBUYSEL  DC    AL1(MBUYSELX-MBUYSEL)                                            
         DC    AL1(RECBUY,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'0000'                                                          
         DC    AL1(8,7,KEYMED,KEYBYR,KEYCAM,KEYMKT)                             
         DC    AL1(KEYDAY,KEYTIM,KEYDPL,0)                                      
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(152)                                                         
MBUYSELX EQU   *                                                                
*                                                                               
MBUYADD  DC    AL1(MBUYADDX-MBUYADD)                                            
         DC    AL1(RECBUY,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXDREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'D736'                                                          
         DC    AL1(8,7,KEYMED,KEYBYR,KEYCAM,KEYMKT)                             
         DC    AL1(KEYDAY,KEYTIM,KEYDPL,0)                                      
         DC    AL4(0,OPTSIDB+OPTPRGB+OPTRTGB+OPTDEMB+OPTPRDB+OPTRNKB+OPT        
               TNCSB+OPTNOGB+OPTCPMB+OPTDPTB+OPTSKDB)                           
         DC    XL8'00'                                                          
         DC    AL2(153)                                                         
MBUYADDX EQU   *                                                                
*                                                                               
MBUYDIS  DC    AL1(MBUYDISX-MBUYDIS)                                            
         DC    AL1(RECBUY,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'D736'                                                          
         DC    AL1(8,7,KEYMED,KEYBYR,KEYCAM,KEYMKT)                             
         DC    AL1(KEYDAY,KEYTIM,KEYDPL,0)                                      
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(154)                                                         
MBUYDISX EQU   *                                                                
*                                                                               
MBUYCHA  DC    AL1(MBUYCHAX-MBUYCHA)                                            
         DC    AL1(RECBUY,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'D736'                                                          
         DC    AL1(8,7,KEYMED,KEYBYR,KEYCAM,KEYMKT)                             
         DC    AL1(KEYDAY,KEYTIM,KEYDPL,0)                                      
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(155)                                                         
MBUYCHAX EQU   *                                                                
*                                                                               
MBUYDEL  DC    AL1(MBUYDELX-MBUYDEL)                                            
         DC    AL1(RECBUY,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'D736'                                                          
         DC    AL1(8,7,KEYMED,KEYBYR,KEYCAM,KEYMKT)                             
         DC    AL1(KEYDAY,KEYTIM,KEYDPL,0)                                      
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(156)                                                         
MBUYDELX EQU   *                                                                
*                                                                               
MBUYSKD  DC    AL1(MBUYSKDX-MBUYSKD)                                            
         DC    AL1(RECBUY,ACTSKD)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'D535'                                                          
         DC    AL1(0,4,KEYMED,KEYBYR,KEYCAM,KEYMKT,0,0,0,0)                     
         DC    AL4(0,OPTSIDB+OPTPRGB+OPTNCSB)                                   
         DC    XL8'00'                                                          
         DC    AL2(157)                                                         
MBUYSKDX EQU   *                                                                
*                                                                               
MBUYDEM  DC    AL1(MBUYDEMX-MBUYDEM)                                            
         DC    AL1(RECBUY,ACTDEM)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'D535'                                                          
         DC    AL1(0,5,KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDPL,0,0,0)                
         DC    AL4(0,OPTPRDB+OPTRNKB+OPTDPTB+OPTSKDB)                           
         DC    XL8'00'                                                          
         DC    AL2(158)                                                         
MBUYDEMX EQU   *                                                                
*                                                                               
MBUYSSK  DC    AL1(MBUYSSKX-MBUYSSK)                                            
         DC    AL1(RECBUY,ACTSSK)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'D635'                                                          
         DC    AL1(0,4,KEYMED,KEYBYR,KEYCAM,KEYMKT,0,0,0,0)                     
         DC    AL4(0,OPTSIDB+OPTPRGB+OPTRTGB+OPTNCSB+OPTRNKB+OPTCPMB)           
         DC    XL8'00'                                                          
         DC    AL2(159)                                                         
MBUYSSKX EQU   *                                                                
*                                                                               
MBUYXFR  DC    AL1(MBUYXFRX-MBUYXFR)                                            
         DC    AL1(RECBUY,ACTXFR)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'0035'                                                          
         DC    AL1(0,5,KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDPL,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(160)                                                         
MBUYXFRX EQU   *                                                                
*                                                                               
MBUYRCP  DC    AL1(MBUYRCPX-MBUYRCP)                                            
         DC    AL1(RECBUY,ACTDRP)                                               
         DC    AL1(MIXIREP+MIXKREQ,MIXIOKN+MIXITXT+MIXIDRV)                     
         DC    AL1(0,0)                                                         
         DC    X'F933'                                                          
         DC    AL1(0,5,KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDPL,0,0,0)                
         DC    AL4(0,OPTSIDB+OPTPRGB+OPTRTGB+OPTPRDB+OPTRNKB+OPTNCSB+OPT        
               TCPMB+OPTSKDB)                                                   
         DC    XL7'00'                                                          
         DC    AL1(MIXIDRLS)                                                    
         DC    AL2(161)                                                         
MBUYRCPX EQU   *                                                                
*                                                                               
MBUYREP  DC    AL1(MBUYREPX-MBUYREP)                                            
         DC    AL1(RECBUY,ACTREP)                                               
         DC    AL1(MIXIREP,MIXIOKN+MIXIOKO+MIXIOKS+MIXITXT+MIXIDRV)             
         DC    AL1(0,0)                                                         
         DC    X'F83A'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,OPTSIDB+OPTPRGB+OPTRTGB+OPTDEMB+OPTPRDB+OPTRNKB+OPT        
               TNCSB+OPTNOGB+OPTCPMB+OPTDPTB+OPTSKDB)                           
         DC    C'SPWLWK  '                                                      
         DC    AL2(163)                                                         
MBUYREPX EQU   *                                                                
*                                                                               
MBUYUPR  DC    AL1(MBUYUPRX-MBUYUPR)                                            
         DC    AL1(RECBUY,ACTUPR)                                               
         DC    AL1(MIXIREP,MIXIOKN+MIXIOKS+MIXIOKO+MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'EF21'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,OPTSIDB+OPTPRGB+OPTRTGB+OPTDEMB+OPTPRDB+OPTRNKB+OPT        
               TNCSB+OPTNOGB+OPTCPMB+OPTDPTB+OPTSKDB)                           
         DC    C'SPWUWK  '                                                      
         DC    AL2(164)                                                         
MBUYUPRX EQU   *                                                                
*                                                                               
MBUYEST  DC    AL1(MBUYESTX-MBUYEST)                                            
         DC    AL1(RECBUY,ACTEST)                                               
         DC    AL1(MIXILFM,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'E738'                                                          
         DC    AL1(7,7,KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDAY)                      
         DC    AL1(KEYTIM,KEYDPL,0)                                             
         DC    AL4(0,0)                                                         
         DC    AL1(ACTLFM8),XL7'00'                                             
         DC    AL2(165)                                                         
MBUYESTX EQU   *                                                                
*                                                                               
MBUYCOM  DC    AL1(MBUYCOMX-MBUYCOM)                                            
         DC    AL1(RECBUY,ACTCOM)                                               
         DC    AL1(MIXILFM,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'E439'                                                          
         DC    AL1(7,7,KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDAY)                      
         DC    AL1(KEYTIM,KEYDPL,0)                                             
         DC    AL4(0,0)                                                         
         DC    AL1(ACTLFM8),XL7'00'                                             
         DC    AL2(166)                                                         
MBUYCOMX EQU   *                                                                
*                                                                               
MBUYMKT  DC    AL1(MBUYMKTX-MBUYMKT)                                            
         DC    AL1(RECBUY,ACTMKT)                                               
         DC    AL1(MIXILFM,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'E515'                                                          
         DC    AL1(7,7,KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDAY)                      
         DC    AL1(KEYTIM,KEYDPL,0)                                             
         DC    AL4(0,0)                                                         
         DC    AL1(ACTLFM8),XL7'00'                                             
         DC    AL2(173)                                                         
MBUYMKTX EQU   *                                                                
*                                                                               
MBUYLED  DC    AL1(MBUYLEDX-MBUYLED)                                            
         DC    AL1(RECBUY,ACTLED)                                               
         DC    AL1(MIXILFM,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'E134'                                                          
         DC    AL1(7,7,KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDAY)                      
         DC    AL1(KEYTIM,KEYDPL,0)                                             
         DC    AL4(0,0)                                                         
         DC    AL1(ACTLFM8),XL7'00'                                             
         DC    AL2(174)                                                         
MBUYLEDX EQU   *                                                                
*                                                                               
MBUYROT  DC    AL1(MBUYROTX-MBUYROT)                                            
         DC    AL1(RECBUY,ACTROT)                                               
         DC    AL1(MIXILFM,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'E234'                                                          
         DC    AL1(7,7,KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDAY)                      
         DC    AL1(KEYTIM,KEYDPL,0)                                             
         DC    AL4(0,0)                                                         
         DC    AL1(ACTLFM8),XL7'00'                                             
         DC    AL2(175)                                                         
MBUYROTX EQU   *                                                                
*                                                                               
MBUYSPL  DC    AL1(MBUYSPLX-MBUYSPL)                                            
         DC    AL1(RECBUY,ACTSPL)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'0035'                                                          
         DC    AL1(0,4,KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDPL,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(167)                                                         
MBUYSPLX EQU   *                                                                
*                                                                               
MBUYAUD  DC    AL1(MBUYAUDX-MBUYAUD)                                            
         DC    AL1(RECBUY,ACTAUD)                                               
         DC    AL1(MIXILFM,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'D43B'                                                          
         DC    AL1(7,7,KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDAY)                      
         DC    AL1(KEYTIM,KEYDPL,0)                                             
         DC    AL4(0,0)                                                         
         DC    AL1(ACTLFM8),XL7'00'                                             
         DC    AL2(278)                                                         
MBUYAUDX EQU   *                                                                
*                                                                               
MBUYTST  DC    AL1(MBUYTSTX-MBUYTST)                                            
         DC    AL1(RECTST,ACTDRP)                                               
         DC    AL1(MIXIREP+MIXIDDS,MIXIOKN+MIXITXT+MIXIDRV)                     
         DC    AL1(0,0)                                                         
         DC    X'F199'                                                          
         DC    AL4(0,0)                                                         
         DC    AL4(0,0)                                                         
         DC    XL7'00'                                                          
         DC    AL1(MIXIDRLS)                                                    
         DC    AL2(161)                                                         
MBUYTSTX EQU   *                                                                
*                                                                               
MIXTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* KEY COMPONENT TABLE (SEE KEYTABD)                                   *         
***********************************************************************         
         SPACE 1                                                                
KEYTAB   DS    0X                                                               
*                                                                               
         DC    C'MED    ',AL1(KEYMED)                                           
         DC    C'BUYER  ',AL1(KEYBYR)                                           
         DC    C'CAMP   ',AL1(KEYCAM)                                           
         DC    C'MKT    ',AL1(KEYMKT)                                           
         DC    C'STA    ',AL1(KEYSTA)                                           
         DC    C'DPTLEN ',AL1(KEYDPL)                                           
         DC    C'DAYS   ',AL1(KEYDAY)                                           
         DC    C'TIMES  ',AL1(KEYTIM)                                           
*                                                                               
KEYTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* OPTION TABLE (SEE OPTTABD)                                          *         
***********************************************************************         
         SPACE 1                                                                
OPTTAB   DS    0X                                                               
*                                                                               
SIDOPT   DC    AL1(SIDOPTX-SIDOPT)                                              
         DC    C'SID     ',C'   ',AL1(OPTNRTN+OPTITXT,0)                        
         DC    AL1(0,0)                                                         
         DC    AL1(0,0)                                                         
         DC    AL1(1,1,6,L'INOSID)                                              
         DC    AL4(OPTSIDB)                                                     
         DC    AL1(OPTSIDN)                                                     
         DC    AL2(OPTSIDR)                                                     
         DC    AL2(INOSID-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(OPTRNKB)                                                     
         DC    AL2(100,101)                                                     
SIDOPTX  EQU   *                                                                
*                                                                               
PRGOPT   DC    AL1(PRGOPTX-PRGOPT)                                              
         DC    C'PROGRAM ',C'PRG',AL1(OPTNRTN+OPTITXT,0)                        
         DC    AL1(0,0)                                                         
         DC    AL1(RECWRK,0)                                                    
         DC    AL1(2,1,L'INOPRG,L'INOPRG)                                       
         DC    AL4(OPTPRGB)                                                     
         DC    AL1(OPTPRGN)                                                     
         DC    AL2(OPTPRGR)                                                     
         DC    AL2(INOPRG-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(OPTSIDB)                                                     
         DC    AL4(0)                                                           
         DC    AL2(102,103)                                                     
PRGOPTX  EQU   *                                                                
*                                                                               
RTGOPT   DC    AL1(RTGOPTX-RTGOPT)                                              
         DC    C'RATING  ',C'RTG',AL1(OPTNRTN+OPTITXT,0)                        
         DC    AL1(0,0)                                                         
         DC    AL1(0,0)                                                         
         DC    AL1(1,1,10,L'INORTG)                                             
         DC    AL4(OPTRTGB)                                                     
         DC    AL1(OPTRTGN)                                                     
         DC    AL2(OPTRTGR)                                                     
         DC    AL2(INORTG-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(104,105)                                                     
RTGOPTX  EQU   *                                                                
*                                                                               
DEMOPT   DC    AL1(DEMOPTX-DEMOPT)                                              
         DC    C'DEMOS   ',C'   ',AL1(OPTNRTN+OPTITXT,0)                        
         DC    AL1(0,0)                                                         
         DC    AL1(RECWRK,0)                                                    
         DC    AL1(1,1,30,L'INODEM)                                             
         DC    AL4(OPTDEMB)                                                     
         DC    AL1(OPTDEMN)                                                     
         DC    AL2(OPTDEMR)                                                     
         DC    AL2(INODEM-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(106,107)                                                     
DEMOPTX  EQU   *                                                                
*                                                                               
PRDOPT   DC    AL1(PRDOPTX-PRDOPT)                                              
         DC    C'PRODUCT ',C'PRD',AL1(OPTNRTN+OPTITXT,0)                        
         DC    AL1(0,0)                                                         
         DC    AL1(RECWRK,0)                                                    
         DC    AL1(3,2,15,L'INOPRD)                                             
         DC    AL4(OPTPRDB)                                                     
         DC    AL1(OPTPRDN)                                                     
         DC    AL2(OPTPRDR)                                                     
         DC    AL2(INOPRD-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(108,109)                                                     
PRDOPTX  EQU   *                                                                
*                                                                               
PR2OPT   DC    AL1(PR2OPTX-PR2OPT)                                              
         DC    C'PRODUCT ',C'PRD',AL1(OPTNRTN+OPTITXT,0)                        
         DC    AL1(0,0)                                                         
         DC    AL1(RECBUY,0)                                                    
         DC    AL1(3,2,15,L'INOPRD)                                             
         DC    AL4(OPTPRDB)                                                     
         DC    AL1(OPTPRDN)                                                     
         DC    AL2(OPTPRDR)                                                     
         DC    AL2(INOPRD-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(108,109)                                                     
PR2OPTX  EQU   *                                                                
*                                                                               
PEROPT   DC    AL1(PEROPTX-PEROPT)                                              
         DC    C'PERIOD  ',C'   ',AL1(OPTNRTN+OPTITXT,0)                        
         DC    AL1(0,0)                                                         
         DC    AL1(RECWRK,ACTDRP)                                               
         DC    AL1(1,6,17,L'INOPER)                                             
         DC    AL4(OPTPERB)                                                     
         DC    AL1(OPTPERN)                                                     
         DC    AL2(OPTPERR)                                                     
         DC    AL2(INOPER-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(112,113)                                                     
PEROPTX  EQU   *                                                                
*                                                                               
PER2OPT  DC    AL1(PER2OPTX-PER2OPT)                                            
         DC    C'PERIOD  ',C'   ',AL1(OPTNRTN+OPTITXT,0)                        
         DC    AL1(0,0)                                                         
         DC    AL1(RECBUY,ACTDRP)                                               
         DC    AL1(1,6,17,L'INOPER)                                             
         DC    AL4(OPTPERB)                                                     
         DC    AL1(OPTPERN)                                                     
         DC    AL2(OPTPERR)                                                     
         DC    AL2(INOPER-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(112,113)                                                     
PER2OPTX EQU   *                                                                
*                                                                               
IDOPT    DC    AL1(IDOPTX-IDOPT)                                                
         DC    C'ID      ',C'   ',AL1(OPTNRTN+OPTITXT,0)                        
         DC    AL1(0,0)                                                         
         DC    AL1(RECWRK,ACTXFR)                                               
         DC    AL1(2,1,12,L'INOBUYID)                                           
         DC    AL4(OPTIDB)                                                      
         DC    AL1(OPTIDN)                                                      
         DC    AL2(OPTIDR)                                                      
         DC    AL2(INOBUYID-WORKD)                                              
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(114,115)                                                     
IDOPTX   EQU   *                                                                
*                                                                               
ID2OPT   DC    AL1(ID2OPTX-ID2OPT)                                              
         DC    C'ID      ',C'   ',AL1(OPTNRTN+OPTITXT,0)                        
         DC    AL1(0,0)                                                         
         DC    AL1(RECBUY,ACTXFR)                                               
         DC    AL1(2,1,12,L'INOBUYID)                                           
         DC    AL4(OPTIDB)                                                      
         DC    AL1(OPTIDN)                                                      
         DC    AL2(OPTIDR)                                                      
         DC    AL2(INOBUYID-WORKD)                                              
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(114,115)                                                     
ID2OPTX  EQU   *                                                                
*                                                                               
ID3OPT   DC    AL1(ID3OPTX-ID3OPT)                                              
         DC    C'ID      ',C'   ',AL1(OPTNRTN+OPTITXT,0)                        
         DC    AL1(0,0)                                                         
         DC    AL1(RECBUY,ACTDIS)                                               
         DC    AL1(2,1,12,L'INOBUYID)                                           
         DC    AL4(OPTIDB)                                                      
         DC    AL1(OPTIDN)                                                      
         DC    AL2(OPTIDR)                                                      
         DC    AL2(INOBUYID-WORKD)                                              
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(114,115)                                                     
ID3OPTX  EQU   *                                                                
*                                                                               
ID4OPT   DC    AL1(ID4OPTX-ID4OPT)                                              
         DC    C'ID      ',C'   ',AL1(OPTNRTN+OPTITXT,0)                        
         DC    AL1(0,0)                                                         
         DC    AL1(RECBUY,ACTSKD)                                               
         DC    AL1(2,1,12,L'INOBUYID)                                           
         DC    AL4(OPTIDB)                                                      
         DC    AL1(OPTIDN)                                                      
         DC    AL2(OPTIDR)                                                      
         DC    AL2(INOBUYID-WORKD)                                              
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(114,115)                                                     
ID4OPTX  EQU   *                                                                
*                                                                               
SLNOPT   DC    AL1(SLNOPTX-SLNOPT)                                              
         DC    C'SLN     ',C'   ',AL1(OPTNRTN+OPTITXT,0)                        
         DC    AL1(0,0)                                                         
         DC    AL1(RECWRK,0)                                                    
         DC    AL1(2,2,12,L'INOSLN)                                             
         DC    AL4(OPTSLNB)                                                     
         DC    AL1(OPTSLNN)                                                     
         DC    AL2(OPTSLNR)                                                     
         DC    AL2(INOSLN-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(OPTSIDB)                                                     
         DC    AL4(0)                                                           
         DC    AL2(122,123)                                                     
SLNOPTX  EQU   *                                                                
*                                                                               
FRMOPT   DC    AL1(FRMOPTX-FRMOPT)                                              
         DC    C'FORMAT  ',C'   ',AL1(OPTATAB+OPTTABH+OPTITXT,0)                
         DC    AL1(0,0)                                                         
         DC    AL1(RECWRK,ACTDRP)                                               
         DC    AL1(1,1,3,L'INOFRM)                                              
         DC    AL4(OPTFRMB)                                                     
         DC    AL1(OPTFRMN)                                                     
         DC    AL2(VALFRM-TBASE)                                                
         DC    AL2(INOFRM-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(116)                                                         
FRMOPTX  EQU   *                                                                
*                                                                               
FR2OPT   DC    AL1(FR2OPTX-FR2OPT)                                              
         DC    C'FORMAT  ',C'   ',AL1(OPTATAB+OPTTABH+OPTITXT,0)                
         DC    AL1(0,0)                                                         
         DC    AL1(RECBUY,ACTDRP)                                               
         DC    AL1(1,1,3,L'INOFRM)                                              
         DC    AL4(OPTFRMB)                                                     
         DC    AL1(OPTFRMN)                                                     
         DC    AL2(VALFRM-TBASE)                                                
         DC    AL2(INOFRM-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(116)                                                         
FR2OPTX  EQU   *                                                                
*                                                                               
RNKOPT   DC    AL1(RNKOPTX-RNKOPT)                                              
         DC    C'RANK    ',C'RNK'                                               
         DC    AL1(OPTATAB+OPTTABH+OPTDFLT+OPTITXT,0)                           
         DC    AL1(0,0)                                                         
         DC    AL1(0,0)                                                         
         DC    AL1(3,1,4,L'INORNK)                                              
         DC    AL4(OPTRNKB)                                                     
         DC    AL1(OPTRNKN)                                                     
         DC    AL2(VALRNK-TBASE)                                                
         DC    AL2(INORNK-WORKD)                                                
         DC    AL1(INORNKC),XL3'00'                                             
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(119)                                                         
RNKOPTX  EQU   *                                                                
*                                                                               
NSPOPT   DC    AL1(NSPOPTX-NSPOPT)                                              
         DC    C'NOSPILL ',C'NSP',AL1(OPTATAB+OPTBOOL+OPTITXT,0)                
         DC    AL1(0,0)                                                         
         DC    AL1(RECWRK,ACTXFR)                                               
         DC    AL1(3,0,0,L'INOXFI)                                              
         DC    AL4(OPTNSPB)                                                     
         DC    AL1(OPTNSPN)                                                     
         DC    AL2(VALNSP-TBASE)                                                
         DC    AL2(INOXFI-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(118)                                                         
NSPOPTX  EQU   *                                                                
*                                                                               
NS2OPT   DC    AL1(NS2OPTX-NS2OPT)                                              
         DC    C'NOSPILL ',C'NSP',AL1(OPTATAB+OPTBOOL+OPTITXT,0)                
         DC    AL1(0,0)                                                         
         DC    AL1(RECBUY,ACTXFR)                                               
         DC    AL1(3,0,0,L'INOXFI)                                              
         DC    AL4(OPTNSPB)                                                     
         DC    AL1(OPTNSPN)                                                     
         DC    AL2(VALNSP-TBASE)                                                
         DC    AL2(INOXFI-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(118)                                                         
NS2OPTX  EQU   *                                                                
*                                                                               
NCSOPT   DC    AL1(NCSOPTX-NCSOPT)                                              
         DC    C'NOCOST  ',C'   ',AL1(OPTATAB+OPTBOOL+OPTITXT,0)                
         DC    AL1(0,0)                                                         
         DC    AL1(RECWRK,0)                                                    
         DC    AL1(3,0,0,L'INOXFI)                                              
         DC    AL4(OPTNCSB)                                                     
         DC    AL1(OPTNCSN)                                                     
         DC    AL2(VALNCS-TBASE)                                                
         DC    AL2(INOXFI-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(OPTSIDB)                                                     
         DC    AL4(0)                                                           
         DC    AL2(120)                                                         
NCSOPTX  EQU   *                                                                
*                                                                               
NOGOPT   DC    AL1(NOGOPTX-NOGOPT)                                              
         DC    C'NOGOALS ',C'NFG',AL1(OPTATAB+OPTBOOL+OPTITXT,0)                
         DC    AL1(0,0)                                                         
         DC    AL1(0,0)                                                         
         DC    AL1(3,0,0,L'INOIND)                                              
         DC    AL4(OPTNOGB)                                                     
         DC    AL1(OPTNOGN)                                                     
         DC    AL2(VALNOG-TBASE)                                                
         DC    AL2(INOIND-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(121)                                                         
NOGOPTX  EQU   *                                                                
*                                                                               
ALGOPT   DC    AL1(ALGOPTX-ALGOPT)                                              
         DC    C'ALLGOALS',C'AFG',AL1(OPTATAB+OPTBOOL+OPTITXT,0)                
         DC    AL1(0,0)                                                         
         DC    AL1(RECWRK,0)                                                    
         DC    AL1(3,0,0,L'INOIND)                                              
         DC    AL4(OPTALGB)                                                     
         DC    AL1(OPTALGN)                                                     
         DC    AL2(VALALG-TBASE)                                                
         DC    AL2(INOIND-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(274)                                                         
ALGOPTX  EQU   *                                                                
*                                                                               
AL2OPT   DC    AL1(AL2OPTX-AL2OPT)                                              
         DC    C'ALLGOALS',C'AFG',AL1(OPTATAB+OPTBOOL+OPTITXT,0)                
         DC    AL1(0,0)                                                         
         DC    AL1(RECBUY,0)                                                    
         DC    AL1(3,0,0,L'INOIND)                                              
         DC    AL4(OPTALGB)                                                     
         DC    AL1(OPTALGN)                                                     
         DC    AL2(VALALG-TBASE)                                                
         DC    AL2(INOIND-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(274)                                                         
AL2OPTX  EQU   *                                                                
*                                                                               
XGLOPT   DC    AL1(XGLOPTX-XGLOPT)                                              
         DC    C'XGOAL   ',C'   ',AL1(OPTATAB+OPTBOOL+OPTITXT,0)                
         DC    AL1(0,0)                                                         
         DC    AL1(0,0)                                                         
         DC    AL1(1,0,0,L'INOIND)                                              
         DC    AL4(OPTXGLB)                                                     
         DC    AL1(OPTXGLN)                                                     
         DC    AL2(VALXGL-TBASE)                                                
         DC    AL2(INOIND-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(300)                                                         
XGLOPTX  EQU   *                                                                
*                                                                               
NOTOPT   DC    AL1(NOTOPTX-NOTOPT)                                              
         DC    C'NOTRANS ',C'   ',AL1(OPTATAB+OPTBOOL+OPTITXT,0)                
         DC    AL1(0,0)                                                         
         DC    AL1(0,0)                                                         
         DC    AL1(3,0,0,L'INOIND)                                              
         DC    AL4(OPTNOTB)                                                     
         DC    AL1(OPTNOTN)                                                     
         DC    AL2(VALNOT-TBASE)                                                
         DC    AL2(INOIND-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(OPTSIDB)                                                     
         DC    AL4(0)                                                           
         DC    AL2(124)                                                         
NOTOPTX  EQU   *                                                                
*                                                                               
CPMOPT   DC    AL1(CPMOPTX-CPMOPT)                                              
         DC    C'CPM     ',C'   ',AL1(OPTATAB+OPTBOOL+OPTITXT,0)                
         DC    AL1(0,0)                                                         
         DC    AL1(RECWRK,0)                                                    
         DC    AL1(3,0,0,L'INOIND)                                              
         DC    AL4(OPTCPMB)                                                     
         DC    AL1(OPTCPMN)                                                     
         DC    AL2(VALCPM-TBASE)                                                
         DC    AL2(INOIND-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(125)                                                         
CPMOPTX  EQU   *                                                                
*                                                                               
CP2OPT   DC    AL1(CP2OPTX-CP2OPT)                                              
         DC    C'CPM     ',C'   ',AL1(OPTATAB+OPTBOOL+OPTITXT,0)                
         DC    AL1(0,0)                                                         
         DC    AL1(RECBUY,0)                                                    
         DC    AL1(3,0,0,L'INOIND)                                              
         DC    AL4(OPTCPMB)                                                     
         DC    AL1(OPTCPMN)                                                     
         DC    AL2(VALCPM-TBASE)                                                
         DC    AL2(INOIND-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(125)                                                         
CP2OPTX  EQU   *                                                                
*                                                                               
TSTOPT   DC    AL1(TSTOPTX-TSTOPT)                                              
         DC    C'TEST    ',C'TST'                                               
         DC    AL1(OPTATAB+OPTBOOL+OPTITXT+OPTIDDS,0)                           
         DC    AL1(0,0)                                                         
         DC    AL1(0,0)                                                         
         DC    AL1(1,0,0,L'INOIND)                                              
         DC    AL4(OPTTSTB)                                                     
         DC    AL1(OPTTSTN)                                                     
         DC    AL2(VALTST-TBASE)                                                
         DC    AL2(INOIND-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(126)                                                         
TSTOPTX  EQU   *                                                                
*                                                                               
ADJOPT   DC    AL1(ADJOPTX-ADJOPT)                                              
         DC    C'AUTOADJ ',C'ADJ'                                               
         DC    AL1(OPTATAB+OPTTABH+OPTBOOL+OPTITXT,0)                           
         DC    AL1(0,0)                                                         
         DC    AL1(RECWRK,0)                                                    
         DC    AL1(3,1,3,L'INFIND)                                              
         DC    AL4(OPTADJB)                                                     
         DC    AL1(OPTADJN)                                                     
         DC    AL2(VALADJ-TBASE)                                                
         DC    AL2(INFIND-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(127)                                                         
ADJOPTX  EQU   *                                                                
*                                                                               
AD2OPT   DC    AL1(AD2OPTX-AD2OPT)                                              
         DC    C'AUTOADJ ',C'ADJ'                                               
         DC    AL1(OPTATAB+OPTTABH+OPTBOOL+OPTITXT,0)                           
         DC    AL1(0,0)                                                         
         DC    AL1(RECBUY,0)                                                    
         DC    AL1(3,1,3,L'INFIND)                                              
         DC    AL4(OPTADJB)                                                     
         DC    AL1(OPTADJN)                                                     
         DC    AL2(VALADJ-TBASE)                                                
         DC    AL2(INFIND-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(127)                                                         
AD2OPTX  EQU   *                                                                
*                                                                               
DPTOPT   DC    AL1(DPTOPTX-DPTOPT)                                              
         DC    C'DPT     ',C'   '                                               
         DC    AL1(OPTATAB+OPTTABH+OPTITXT,0)                                   
         DC    AL1(0,0)                                                         
         DC    AL1(RECWRK,0)                                                    
         DC    AL1(3,1,3,L'INODPT)                                              
         DC    AL4(OPTDPTB)                                                     
         DC    AL1(OPTDPTN)                                                     
         DC    AL2(VALDPT-TBASE)                                                
         DC    AL2(INODPT-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(128)                                                         
DPTOPTX  EQU   *                                                                
*                                                                               
DP2OPT   DC    AL1(DP2OPTX-DP2OPT)                                              
         DC    C'DPT     ',C'   '                                               
         DC    AL1(OPTATAB+OPTTABH+OPTITXT,0)                                   
         DC    AL1(0,0)                                                         
         DC    AL1(RECBUY,0)                                                    
         DC    AL1(3,1,3,L'INODPT)                                              
         DC    AL4(OPTDPTB)                                                     
         DC    AL1(OPTDPTN)                                                     
         DC    AL2(VALDPT-TBASE)                                                
         DC    AL2(INODPT-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(128)                                                         
DP2OPTX  EQU   *                                                                
*                                                                               
SKDOPT   DC    AL1(SKDOPTX-SKDOPT)                                              
         DC    C'SKED    ',C'SKD'                                               
         DC    AL1(OPTATAB+OPTBOOL+OPTITXT,0)                                   
         DC    AL1(0,0)                                                         
         DC    AL1(RECWRK,0)                                                    
         DC    AL1(2,0,0,L'INOIND)                                              
         DC    AL4(OPTSKDB)                                                     
         DC    AL1(OPTSKDN)                                                     
         DC    AL2(VALSKD-TBASE)                                                
         DC    AL2(INOIND-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(129)                                                         
SKDOPTX  EQU   *                                                                
*                                                                               
SNDOPT   DC    AL1(SNDOPTX-SNDOPT)                                              
         DC    C'SEND    ',C'   '                                               
         DC    AL1(OPTATAB+OPTTABH+OPTBOOL+OPTITXT,0)                           
         DC    AL1(0,0)                                                         
         DC    AL1(RECWRK,0)                                                    
         DC    AL1(4,1,8,L'INFIND)                                              
         DC    AL4(OPTSNDB)                                                     
         DC    AL1(OPTSNDN)                                                     
         DC    AL2(VALSND-TBASE)                                                
         DC    AL2(INFIND-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(200)                                                         
SNDOPTX  EQU   *                                                                
*                                                                               
DATOPT   DC    AL1(DATOPTX-DATOPT)                                              
         DC    C'DATES   ',C'DT ',AL1(OPTNRTN+OPTITXT,0)                        
         DC    AL1(0,0)                                                         
         DC    AL1(RECWRK,ACTXFR)                                               
         DC    AL1(2,4,17,L'INFDATES)                                           
         DC    AL4(OPTDATB)                                                     
         DC    AL1(OPTDATN)                                                     
         DC    AL2(OPTDATR)                                                     
         DC    AL2(INFDATES-WORKD)                                              
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(201)                                                         
DATOPTX  EQU   *                                                                
*                                                                               
DT2OPT   DC    AL1(DT2OPTX-DT2OPT)                                              
         DC    C'DATES   ',C'DT ',AL1(OPTNRTN+OPTITXT,0)                        
         DC    AL1(0,0)                                                         
         DC    AL1(RECBUY,ACTXFR)                                               
         DC    AL1(2,4,17,L'INFDATES)                                           
         DC    AL4(OPTDATB)                                                     
         DC    AL1(OPTDATN)                                                     
         DC    AL2(OPTDATR)                                                     
         DC    AL2(INFDATES-WORKD)                                              
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(201)                                                         
DT2OPTX  EQU   *                                                                
*                                                                               
DT3OPT   DC    AL1(DT3OPTX-DT3OPT)                                              
         DC    C'DATES   ',C'DT ',AL1(OPTNRTN+OPTITXT,0)                        
         DC    AL1(0,0)                                                         
         DC    AL1(RECCAM,ACTLST)                                               
         DC    AL1(2,4,17,L'INFDATES)                                           
         DC    AL4(OPTDATB)                                                     
         DC    AL1(OPTDATN)                                                     
         DC    AL2(OPTDATR)                                                     
         DC    AL2(INFDATES-WORKD)                                              
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(275)                                                         
DT3OPTX  EQU   *                                                                
*                                                                               
LINOPT   DC    AL1(LINOPTX-LINOPT)                                              
         DC    C'LINE    ',C'   ',AL1(OPTNRTN+OPTITXT,0)                        
         DC    AL1(0,0)                                                         
         DC    AL1(RECWRK,ACTTRA)                                               
         DC    AL1(1,2,4,L'INFLINE)                                             
         DC    AL4(OPTLINB)                                                     
         DC    AL1(OPTLINN)                                                     
         DC    AL2(OPTLINR)                                                     
         DC    AL2(INFLINE-WORKD)                                               
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(202)                                                         
LINOPTX  EQU   *                                                                
*                                                                               
STAOPT   DC    AL1(STAOPTX-STAOPT)                                              
         DC    C'STA     ',C'   ',AL1(OPTIDDS+OPTNRTN,0)                        
         DC    AL1(0,0)                                                         
         DC    AL1(RECWRK,ACTCAL)                                               
         DC    AL1(3,3,8,L'INFCHSTA)                                            
         DC    AL4(OPTSTAB)                                                     
         DC    AL1(OPTSTAN)                                                     
         DC    AL2(OPTSTAR)                                                     
         DC    AL2(INFCHSTA-WORKD)                                              
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL1(1,1)                                                         
STAOPTX  EQU   *                                                                
*                                                                               
SDTOPT   DC    AL1(SDTOPTX-SDTOPT)     ROUTINE IN VALSDT IN NWS00               
         DC    C'STDATE  ',C'SDT',AL1(OPTNRTN,0)                                
         DC    AL1(0,0)                                                         
         DC    AL1(0,0)                                                         
*****    DC    AL1(3,8,8,L'INOSTDTE)                                            
         DC    AL1(3,5,8,L'INOSTDTE+L'INOSTDTD)                                 
         DC    AL4(OPTSDTB)                                                     
         DC    AL1(OPTSDTN)                                                     
         DC    AL2(OPTSDTR)                                                     
         DC    AL2(INOSTDTE-WORKD)                                              
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL1(1,1)                                                         
SDTOPTX  EQU   *                                                                
*                                                                               
OPTTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* OPTION VALIDATION TABLES                                            *         
***********************************************************************         
         SPACE 1                                                                
TBASE    DS    0H                  ** TABLE BASE **                             
         SPACE 1                                                                
* VALIDATE THE RECAP SCREEN FORMAT OPTION                                       
*                                                                               
VALFRM   DC    CL60'GVP,DEM,STA,ST2,SD,AD,DPT,DAY,DD,ALL,AL2 or WK'             
         DC    X'0302'                                                          
         DC    CL3'GVP',CL2'G '                                                 
         DC    CL3'DEM',CL2'D '                                                 
         DC    CL3'AD ',CL2'AD'                                                 
         DC    CL3'STA',CL2'S '                                                 
         DC    CL3'ST2',CL2'S2'                                                 
         DC    CL3'SD ',CL2'SD'                                                 
         DC    CL3'DPT',CL2'PT'                                                 
         DC    CL3'DAY',CL2'Y '                                                 
         DC    CL3'DD ',CL2'YD'                                                 
         DC    CL3'ALL',CL2'AL'                                                 
         DC    CL3'AL2',CL2'A2'                                                 
         DC    CL3'WK ',CL2'W '                                                 
         DC    X'00'                                                            
         SPACE 1                                                                
* VALIDATE NOSPILL OPTION                                                       
*                                                                               
VALNSP   DS    0H                                                               
         DC    X'0101'                                                          
         DC    C' ',AL1(INOXFINS)                                               
         DC    X'00'                                                            
         SPACE 1                                                                
* VALIDATE RANK OPTION                                                          
*                                                                               
VALRNK   DS    0H                                                               
         DC    CL60'CPP=dptlen/cpp,PCPP=cpp,DEMO=rtg,STA=sta/day/time,DT        
               T=day/tim'                                                       
         DC    X'0401'                                                          
         DC    CL4'CPP ',AL1(INORNKC) DPTLEN/CPP                                
         DC    CL4'DEMO',AL1(INORNKD) DPTLEN/RATING                             
         DC    CL4'STA ',AL1(INORNKS) DPTLEN/STATION/DAY/TIME                   
         DC    CL4'PCPP',AL1(INORNKP) CPP/DPTLEN                                
         DC    CL4'DT  ',AL1(INORNKT) DAY/TIME/STATION/DPTLEN                   
         DC    CL4'DTS ',AL1(INORNKU) DAY/TIME (SPECIAL)                        
         DC    X'00'                                                            
         SPACE 1                                                                
* VALIDATE NOCOST OPTION                                                        
*                                                                               
VALNCS   DS    0H                                                               
         DC    X'0101'                                                          
         DC    C' ',AL1(INOXFINC)                                               
         DC    X'00'                                                            
         SPACE 1                                                                
* VALIDATE NOGOALS OPTION                                                       
*                                                                               
VALNOG   DS    0H                                                               
         DC    X'0101'                                                          
         DC    C' ',AL1(INOINOG)                                                
         DC    X'00'                                                            
         SPACE 1                                                                
* VALIDATE ALLGOALS OPTION                                                      
*                                                                               
VALALG   DS    0H                                                               
         DC    X'0101'                                                          
         DC    C' ',AL1(INOIALG)                                                
         DC    X'00'                                                            
         SPACE 1                                                                
* VALIDATE GOALS=N OPTION                                                       
*                                                                               
VALXGL   DS    0H                                                               
         DC    X'0101'                                                          
         DC    C' ',AL1(INOIXGL)                                                
         DC    X'00'                                                            
         SPACE 1                                                                
* VALIDATE NOTRANS OPTION                                                       
*                                                                               
VALNOT   DS    0H                                                               
         DC    X'0101'                                                          
         DC    C' ',AL1(INOINOT)                                                
         DC    X'00'                                                            
         SPACE 1                                                                
* VALIDATE CPM OPTION                                                           
*                                                                               
VALCPM   DS    0H                                                               
         DC    X'0101'                                                          
         DC    C' ',AL1(INOICPM)                                                
         DC    X'00'                                                            
         SPACE 1                                                                
* VALIDATE TEST OPTION                                                          
*                                                                               
VALTST   DS    0H                                                               
         DC    X'0101'                                                          
         DC    C' ',AL1(INOITST)                                                
         DC    X'00'                                                            
         SPACE 1                                                                
* VALIDATE AUTOADJ OPTION                                                       
*                                                                               
VALADJ   DS    0H                                                               
         DC    CL60'NO or OFF turns off auto demo adjusting'                    
         DC    X'0301'                                                          
         DC    CL3'NO ',AL1(INFINOAD)                                           
         DC    CL3'OFF',AL1(INFINOAD)                                           
         DC    X'00'                                                            
         SPACE 1                                                                
* VALIDATE DPT OPTION                                                           
*                                                                               
VALDPT   DS    0H                                                               
         DC    CL60'MAS=include all sub dayparts of master'                     
         DC    X'0301'                                                          
         DC    CL3'MAS',CL1'M'                                                  
         DC    X'00'                                                            
         SPACE 1                                                                
* VALIDATE SKED OPTION                                                          
*                                                                               
VALSKD   DS    0H                                                               
         DC    X'0101'                                                          
         DC    C' ',AL1(INOISKD)                                                
         DC    X'00'                                                            
         SPACE 1                                                                
* VALIDATE SEND OPTION                                                          
*                                                                               
VALSND   DS    0H                                                               
         DC    CL60'CAMPAIGN sends only campaign details'                       
         DC    X'0301'                                                          
         DC    CL8'CAMPAIGN',AL1(INFISEND)                                      
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* SELECTABLE ACTION TABLE (SEE SELTABD)                               *         
***********************************************************************         
         SPACE 1                                                                
SELTAB   DS    0X                                                               
*                                                                               
         DC    C'S',AL1(RECBYR,ACTLST,RECBYR,ACTCHA),XL19'00'                   
         DC    C'I',AL1(RECBYR,ACTLST,RECBYR,ACTDIS),XL19'00'                   
*                                                                               
         DC    C'S',AL1(RECCAM,ACTLST,RECCAM,ACTCHA),XL19'00'                   
         DC    C'I',AL1(RECCAM,ACTLST,RECCAM,ACTDIS),XL19'00'                   
*                                                                               
         DC    C'S',AL1(RECWRK,ACTUDT,RECWRK,ACTCHA),XL19'00'                   
         DC    C'D',AL1(RECWRK,ACTUDT,RECWRK,ACTDEL),XL19'00'                   
         DC    C'I',AL1(RECWRK,ACTUDT,RECWRK,ACTDIS),XL19'00'                   
         DC    C'E',AL1(RECWRK,ACTUDT,RECWRK,ACTEST),XL19'00'                   
         DC    C'C',AL1(RECWRK,ACTUDT,RECWRK,ACTCOM),XL19'00'                   
         DC    C'N',AL1(RECWRK,ACTUDT,RECWRK,ACTCOM),XL19'00'                   
         DC    C'M',AL1(RECWRK,ACTUDT,RECWRK,ACTMKT)                            
         DC    XL8'00',X'0800',XL9'00'                                          
         DC    C'K',AL1(RECWRK,ACTUDT,RECWRK,ACTERA),XL19'00'                   
         DC    C'L',AL1(RECWRK,ACTUDT,RECWRK,ACTLED),XL19'00'                   
         DC    C'R',AL1(RECWRK,ACTUDT,RECWRK,ACTROT),XL19'00'                   
         DC    C'>',AL1(RECWRK,ACTUDT,RECBUY,ACTADD),XL19'00'                   
         DC    C'=',AL1(RECWRK,ACTUDT,RECWRK,ACTADD),XL19'00'                   
         DC    C'A',AL1(RECWRK,ACTUDT,RECWRK,ACTAUD),XL19'00'                   
*                                                                               
         DC    C'S',AL1(RECWRK,ACTSKD,RECWRK,ACTCHA),XL19'00'                   
         DC    C'D',AL1(RECWRK,ACTSKD,RECWRK,ACTDEL),XL19'00'                   
         DC    C'I',AL1(RECWRK,ACTSKD,RECWRK,ACTDIS),XL19'00'                   
         DC    C'E',AL1(RECWRK,ACTSKD,RECWRK,ACTEST),XL19'00'                   
         DC    C'C',AL1(RECWRK,ACTSKD,RECWRK,ACTCOM),XL19'00'                   
         DC    C'N',AL1(RECWRK,ACTSKD,RECWRK,ACTCOM),XL19'00'                   
         DC    C'M',AL1(RECWRK,ACTSKD,RECWRK,ACTMKT)                            
         DC    XL8'00',X'0800',XL9'00'                                          
         DC    C'W',AL1(RECWRK,ACTSKD,RECWRK,ACTSKD)                            
         DC    AL1(KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDPL,0,0,0)                    
         DC    X'1000',XL9'00'                                                  
         DC    C'K',AL1(RECWRK,ACTSKD,RECWRK,ACTERA),XL19'00'                   
         DC    C'L',AL1(RECWRK,ACTSKD,RECWRK,ACTLED),XL19'00'                   
         DC    C'R',AL1(RECWRK,ACTSKD,RECWRK,ACTROT),XL19'00'                   
         DC    C'>',AL1(RECWRK,ACTSKD,RECBUY,ACTADD),XL19'00'                   
         DC    C'=',AL1(RECWRK,ACTSKD,RECWRK,ACTADD),XL19'00'                   
         DC    C'A',AL1(RECWRK,ACTSKD,RECWRK,ACTAUD),XL19'00'                   
*                                                                               
         DC    C'S',AL1(RECWRK,ACTSSK,RECWRK,ACTCHA),XL19'00'                   
         DC    C'D',AL1(RECWRK,ACTSSK,RECWRK,ACTDEL),XL19'00'                   
         DC    C'I',AL1(RECWRK,ACTSSK,RECWRK,ACTDIS),XL19'00'                   
         DC    C'E',AL1(RECWRK,ACTSSK,RECWRK,ACTEST),XL19'00'                   
         DC    C'C',AL1(RECWRK,ACTSSK,RECWRK,ACTCOM),XL19'00'                   
         DC    C'N',AL1(RECWRK,ACTSSK,RECWRK,ACTCOM),XL19'00'                   
         DC    C'M',AL1(RECWRK,ACTSSK,RECWRK,ACTMKT)                            
         DC    XL8'00',X'0800',XL9'00'                                          
         DC    C'W',AL1(RECWRK,ACTSSK,RECWRK,ACTSSK)                            
         DC    AL1(KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDPL,0,0,0)                    
         DC    X'1000',XL9'00'                                                  
         DC    C'K',AL1(RECWRK,ACTSSK,RECWRK,ACTERA),XL19'00'                   
         DC    C'L',AL1(RECWRK,ACTSSK,RECWRK,ACTLED),XL19'00'                   
         DC    C'R',AL1(RECWRK,ACTSSK,RECWRK,ACTROT),XL19'00'                   
         DC    C'>',AL1(RECWRK,ACTSSK,RECBUY,ACTADD),XL19'00'                   
         DC    C'=',AL1(RECWRK,ACTSSK,RECWRK,ACTADD),XL19'00'                   
         DC    C'A',AL1(RECWRK,ACTSSK,RECWRK,ACTAUD),XL19'00'                   
*                                                                               
         DC    C'S',AL1(RECWRK,ACTDEM,RECWRK,ACTCHA),XL19'00'                   
         DC    C'D',AL1(RECWRK,ACTDEM,RECWRK,ACTDEL),XL19'00'                   
         DC    C'I',AL1(RECWRK,ACTDEM,RECWRK,ACTDIS),XL19'00'                   
         DC    C'E',AL1(RECWRK,ACTDEM,RECWRK,ACTEST),XL19'00'                   
         DC    C'C',AL1(RECWRK,ACTDEM,RECWRK,ACTCOM),XL19'00'                   
         DC    C'N',AL1(RECWRK,ACTDEM,RECWRK,ACTCOM),XL19'00'                   
         DC    C'M',AL1(RECWRK,ACTDEM,RECWRK,ACTMKT)                            
         DC    XL8'00',X'0800',XL9'00'                                          
         DC    C'L',AL1(RECWRK,ACTDEM,RECWRK,ACTLED),XL19'00'                   
         DC    C'R',AL1(RECWRK,ACTDEM,RECWRK,ACTROT),XL19'00'                   
         DC    C'A',AL1(RECWRK,ACTDEM,RECWRK,ACTAUD),XL19'00'                   
*                                                                               
         DC    C'S',AL1(RECWRK,ACTDUP,RECWRK,ACTCHA),XL19'00'                   
         DC    C'D',AL1(RECWRK,ACTDUP,RECWRK,ACTDEL),XL19'00'                   
         DC    C'I',AL1(RECWRK,ACTDUP,RECWRK,ACTDIS),XL19'00'                   
         DC    C'E',AL1(RECWRK,ACTDUP,RECWRK,ACTEST),XL19'00'                   
         DC    C'C',AL1(RECWRK,ACTDUP,RECWRK,ACTCOM),XL19'00'                   
         DC    C'N',AL1(RECWRK,ACTDUP,RECWRK,ACTCOM),XL19'00'                   
         DC    C'M',AL1(RECWRK,ACTDUP,RECWRK,ACTMKT)                            
         DC    XL8'00',X'0800',XL9'00'                                          
         DC    C'L',AL1(RECWRK,ACTDUP,RECWRK,ACTLED),XL19'00'                   
         DC    C'R',AL1(RECWRK,ACTDUP,RECWRK,ACTROT),XL19'00'                   
         DC    C'A',AL1(RECWRK,ACTDUP,RECWRK,ACTAUD),XL19'00'                   
*                                                                               
         DC    C'X',AL1(RECSID,ACTUDT,RECSID,ACTXFR),XL19'00'                   
         DC    C'T',AL1(RECSID,ACTUDT,RECSID,ACTTFR),XL19'00'                   
         DC    C'C',AL1(RECSID,ACTUDT,RECSID,ACTCOM),XL19'00'                   
         DC    C'E',AL1(RECSID,ACTUDT,RECSID,ACTEST),XL19'00'                   
         DC    C'M',AL1(RECSID,ACTUDT,RECSID,ACTMKT)                            
         DC    XL8'00',X'0800',XL9'00'                                          
         DC    C'L',AL1(RECSID,ACTUDT,RECWRK,ACTLED),XL19'00'                   
         DC    C'R',AL1(RECSID,ACTUDT,RECWRK,ACTROT),XL19'00'                   
*                                                                               
         DC    C'X',AL1(RECSID,ACTDEM,RECSID,ACTXFR),XL19'00'                   
         DC    C'T',AL1(RECSID,ACTDEM,RECSID,ACTTFR),XL19'00'                   
         DC    C'C',AL1(RECSID,ACTDEM,RECSID,ACTCOM),XL19'00'                   
         DC    C'E',AL1(RECSID,ACTDEM,RECSID,ACTEST),XL19'00'                   
         DC    C'M',AL1(RECSID,ACTDEM,RECSID,ACTMKT)                            
         DC    XL8'00',X'0800',XL9'00'                                          
         DC    C'L',AL1(RECSID,ACTDEM,RECWRK,ACTLED),XL19'00'                   
         DC    C'R',AL1(RECSID,ACTDEM,RECWRK,ACTROT),XL19'00'                   
*                                                                               
         DC    C'I',AL1(RECPKG,ACTDIS,RECWRK,ACTDIS),XL19'00'                   
         DC    C'D',AL1(RECPKG,ACTDIS,RECWRK,ACTDEL),XL19'00'                   
*                                                                               
         DC    C'I',AL1(RECPKG,ACTCHA,RECWRK,ACTDIS),XL19'00'                   
         DC    C'D',AL1(RECPKG,ACTCHA,RECWRK,ACTDEL),XL19'00'                   
*                                                                               
         DC    C'S',AL1(RECPKG,ACTUDT,RECPKG,ACTCHA)                            
         DC    AL1(KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDPL,0,0,0),XL11'00'           
         DC    C'D',AL1(RECPKG,ACTUDT,RECWRK,ACTDEL),XL19'00'                   
*                                                                               
         DC    C'S',AL1(RECPKG,ACTSKD,RECPKG,ACTCHA)                            
         DC    AL1(KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDPL,0,0,0),XL11'00'           
         DC    C'D',AL1(RECPKG,ACTSKD,RECWRK,ACTDEL),XL19'00'                   
*                                                                               
         DC    C'S',AL1(RECPKG,ACTSSK,RECPKG,ACTCHA)                            
         DC    AL1(KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDPL,0,0,0),XL11'00'           
         DC    C'D',AL1(RECPKG,ACTSSK,RECWRK,ACTDEL),XL19'00'                   
*                                                                               
         DC    C'S',AL1(RECPKG,ACTDEM,RECPKG,ACTCHA)                            
         DC    AL1(KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDPL,0,0,0),XL11'00'           
         DC    C'D',AL1(RECPKG,ACTDEM,RECWRK,ACTDEL),XL19'00'                   
*                                                                               
         DC    C'I',AL1(RECORB,ACTDIS,RECWRK,ACTDIS),XL19'00'                   
         DC    C'D',AL1(RECORB,ACTDIS,RECWRK,ACTDEL),XL19'00'                   
*                                                                               
         DC    C'I',AL1(RECORB,ACTCHA,RECWRK,ACTDIS),XL19'00'                   
         DC    C'D',AL1(RECORB,ACTCHA,RECWRK,ACTDEL),XL19'00'                   
*                                                                               
         DC    C'S',AL1(RECORB,ACTUDT,RECORB,ACTCHA)                            
         DC    AL1(KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDPL,0,0,0),XL11'00'           
         DC    C'D',AL1(RECORB,ACTUDT,RECWRK,ACTDEL),XL19'00'                   
         DC    C'I',AL1(RECORB,ACTUDT,RECWRK,ACTDIS),XL19'00'                   
*                                                                               
         DC    C'S',AL1(RECORB,ACTSKD,RECORB,ACTCHA)                            
         DC    AL1(KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDPL,0,0,0),XL11'00'           
         DC    C'D',AL1(RECORB,ACTSKD,RECWRK,ACTDEL),XL19'00'                   
         DC    C'I',AL1(RECORB,ACTSKD,RECWRK,ACTDIS),XL19'00'                   
*                                                                               
         DC    C'S',AL1(RECORB,ACTSSK,RECORB,ACTCHA)                            
         DC    AL1(KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDPL,0,0,0),XL11'00'           
         DC    C'D',AL1(RECORB,ACTSSK,RECWRK,ACTDEL),XL19'00'                   
         DC    C'I',AL1(RECORB,ACTSSK,RECWRK,ACTDIS),XL19'00'                   
*                                                                               
         DC    C'S',AL1(RECORB,ACTDEM,RECORB,ACTCHA)                            
         DC    AL1(KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDPL,0,0,0),XL11'00'           
         DC    C'D',AL1(RECORB,ACTDEM,RECWRK,ACTDEL),XL19'00'                   
         DC    C'I',AL1(RECORB,ACTDEM,RECWRK,ACTDIS),XL19'00'                   
*                                                                               
         DC    C'S',AL1(RECBUY,ACTUDT,RECBUY,ACTCHA),XL19'00'                   
         DC    C'D',AL1(RECBUY,ACTUDT,RECBUY,ACTDEL),XL19'00'                   
         DC    C'I',AL1(RECBUY,ACTUDT,RECBUY,ACTDIS),XL19'00'                   
         DC    C'E',AL1(RECBUY,ACTUDT,RECBUY,ACTEST),XL19'00'                   
         DC    C'C',AL1(RECBUY,ACTUDT,RECBUY,ACTCOM),XL19'00'                   
****     DC    C'N',AL1(RECBUY,ACTUDT,RECBUY,ACTCOM),XL19'00'                   
         DC    C'M',AL1(RECBUY,ACTUDT,RECBUY,ACTMKT)                            
         DC    XL8'00',X'0800',XL9'00'                                          
         DC    C'K',AL1(RECBUY,ACTUDT,RECBUY,ACTERA),XL19'00'                   
         DC    C'L',AL1(RECBUY,ACTUDT,RECBUY,ACTLED),XL19'00'                   
         DC    C'R',AL1(RECBUY,ACTUDT,RECBUY,ACTROT),XL19'00'                   
         DC    C'=',AL1(RECBUY,ACTUDT,RECBUY,ACTADD),XL19'00'                   
         DC    C'A',AL1(RECBUY,ACTUDT,RECBUY,ACTAUD),XL19'00'                   
*                                                                               
         DC    C'S',AL1(RECBUY,ACTSKD,RECBUY,ACTCHA),XL19'00'                   
         DC    C'D',AL1(RECBUY,ACTSKD,RECBUY,ACTDEL),XL19'00'                   
         DC    C'I',AL1(RECBUY,ACTSKD,RECBUY,ACTDIS),XL19'00'                   
         DC    C'E',AL1(RECBUY,ACTSKD,RECBUY,ACTEST),XL19'00'                   
         DC    C'C',AL1(RECBUY,ACTSKD,RECBUY,ACTCOM),XL19'00'                   
****     DC    C'N',AL1(RECBUY,ACTSKD,RECBUY,ACTCOM),XL19'00'                   
         DC    C'M',AL1(RECBUY,ACTSKD,RECBUY,ACTMKT)                            
         DC    XL8'00',X'0800',XL9'00'                                          
         DC    C'W',AL1(RECBUY,ACTSKD,RECBUY,ACTSKD)                            
         DC    AL1(KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDPL,0,0,0)                    
         DC    X'1000',XL9'00'                                                  
         DC    C'K',AL1(RECBUY,ACTSKD,RECBUY,ACTERA),XL19'00'                   
         DC    C'L',AL1(RECBUY,ACTSKD,RECBUY,ACTLED),XL19'00'                   
         DC    C'R',AL1(RECBUY,ACTSKD,RECBUY,ACTROT),XL19'00'                   
         DC    C'=',AL1(RECBUY,ACTSKD,RECBUY,ACTADD),XL19'00'                   
         DC    C'A',AL1(RECBUY,ACTSKD,RECBUY,ACTAUD),XL19'00'                   
*                                                                               
         DC    C'S',AL1(RECBUY,ACTSSK,RECBUY,ACTCHA),XL19'00'                   
         DC    C'D',AL1(RECBUY,ACTSSK,RECBUY,ACTDEL),XL19'00'                   
         DC    C'I',AL1(RECBUY,ACTSSK,RECBUY,ACTDIS),XL19'00'                   
         DC    C'E',AL1(RECBUY,ACTSSK,RECBUY,ACTEST),XL19'00'                   
         DC    C'C',AL1(RECBUY,ACTSSK,RECBUY,ACTCOM),XL19'00'                   
****     DC    C'N',AL1(RECBUY,ACTSSK,RECBUY,ACTCOM),XL19'00'                   
         DC    C'M',AL1(RECBUY,ACTSSK,RECBUY,ACTMKT)                            
         DC    XL8'00',X'0800',XL9'00'                                          
         DC    C'W',AL1(RECBUY,ACTSSK,RECBUY,ACTSSK)                            
         DC    AL1(KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDPL,0,0,0)                    
         DC    X'1000',XL9'00'                                                  
         DC    C'K',AL1(RECBUY,ACTSSK,RECBUY,ACTERA),XL19'00'                   
         DC    C'L',AL1(RECBUY,ACTSSK,RECBUY,ACTLED),XL19'00'                   
         DC    C'R',AL1(RECBUY,ACTSSK,RECBUY,ACTROT),XL19'00'                   
         DC    C'=',AL1(RECBUY,ACTSSK,RECBUY,ACTADD),XL19'00'                   
         DC    C'A',AL1(RECBUY,ACTSSK,RECBUY,ACTAUD),XL19'00'                   
*                                                                               
         DC    C'S',AL1(RECBUY,ACTDEM,RECBUY,ACTCHA),XL19'00'                   
         DC    C'D',AL1(RECBUY,ACTDEM,RECBUY,ACTDEL),XL19'00'                   
         DC    C'I',AL1(RECBUY,ACTDEM,RECBUY,ACTDIS),XL19'00'                   
         DC    C'E',AL1(RECBUY,ACTDEM,RECBUY,ACTEST),XL19'00'                   
         DC    C'C',AL1(RECBUY,ACTDEM,RECBUY,ACTCOM),XL19'00'                   
****     DC    C'N',AL1(RECBUY,ACTDEM,RECBUY,ACTCOM),XL19'00'                   
         DC    C'M',AL1(RECBUY,ACTDEM,RECBUY,ACTMKT)                            
         DC    XL8'00',X'0800',XL9'00'                                          
         DC    C'L',AL1(RECBUY,ACTDEM,RECBUY,ACTLED),XL19'00'                   
         DC    C'R',AL1(RECBUY,ACTDEM,RECBUY,ACTROT),XL19'00'                   
         DC    C'A',AL1(RECBUY,ACTDEM,RECBUY,ACTAUD),XL19'00'                   
*                                                                               
         DC    C'S',AL1(RECBUY,ACTDUP,RECBUY,ACTCHA),XL19'00'                   
         DC    C'D',AL1(RECBUY,ACTDUP,RECBUY,ACTDEL),XL19'00'                   
         DC    C'I',AL1(RECBUY,ACTDUP,RECBUY,ACTDIS),XL19'00'                   
         DC    C'E',AL1(RECBUY,ACTDUP,RECBUY,ACTEST),XL19'00'                   
         DC    C'C',AL1(RECBUY,ACTDUP,RECBUY,ACTCOM),XL19'00'                   
****     DC    C'N',AL1(RECBUY,ACTDUP,RECBUY,ACTCOM),XL19'00'                   
         DC    C'M',AL1(RECBUY,ACTDUP,RECBUY,ACTMKT)                            
         DC    XL8'00',X'0800',XL9'00'                                          
         DC    C'L',AL1(RECBUY,ACTDUP,RECBUY,ACTLED),XL19'00'                   
         DC    C'R',AL1(RECBUY,ACTDUP,RECBUY,ACTROT),XL19'00'                   
         DC    C'A',AL1(RECBUY,ACTDUP,RECBUY,ACTAUD),XL19'00'                   
*                                                                               
SELTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
* SPNWSWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSWRK                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'111SPNWS01   02/26/07'                                      
         END                                                                    

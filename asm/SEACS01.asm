*          DATA SET SEACS01    AT LEVEL 116 AS OF 12/06/19                      
*PHASE TA0D01A                                                                  
*                                                                               
         TITLE 'SEACS01 - SECURITY ACCESS PROGRAM - CONTROLLER TABLES'          
ACS01    CSECT                                                                  
*                                                                               
ARECTAB  DC    AL4(RECTAB-ACS01)   RECORD TABLE                                 
AACTTAB  DC    AL4(ACTTAB-ACS01)   ACTION TABLE                                 
AMIXTAB  DC    AL4(MIXTAB-ACS01)   REC/ACT MIX TABLE                            
AKEYTAB  DC    AL4(KEYTAB-ACS01)   KEY COMPONENT TABLE                          
AOPTTAB  DC    AL4(OPTTAB-ACS01)   OPTION TABLE                                 
ASELTAB  DC    AL4(SELTAB-ACS01)   SELECTABLE ACTION TABLE                      
APFKTAB  DC    AL4(PFKTAB-ACS01)   PF KEY TABLE                                 
ATYPTAB  DC    AL4(TYPTAB-ACS01)   RECORD SUB-TYPE TABLE                        
         EJECT                                                                  
                                                                                
***********************************************************************         
* RECORD TYPE TABLE (SEE RECTABD)                                               
***********************************************************************         
RECTAB   DS    0X                                                               
*                                                                               
RACS     DC    AL1(RACSX-RACS)                                                  
         DCDD  CT#ACCS,8                                                        
         DC    C'   ',AL1(RECITXT+RECISET)                                      
         DC    AL1(0,0,RECACS)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(CTFILEQ),XL7'00'                                             
         DC    AL2(3000)                                                        
RACSX    EQU   *                                                                
*                                                                               
RPGM     DC    AL1(RPGMX-RPGM)                                                  
         DCDD  CT#PROG,8                                                        
         DC    C'   ',AL1(RECIDDS+RECITXT+RECISET)                              
         DC    AL1(0,0,RECPGM)                                                  
         DC    AL4(0,OPTAGYB)                                                   
         DC    AL1(CTFILEQ),XL7'00'                                             
         DC    AL2(3010)                                                        
RPGMX    EQU   *                                                                
*                                                                               
RACT     DC    AL1(RACTX-RACT)                                                  
         DCDD  CT#ACT,8                                                         
         DC    C'   ',AL1(RECIDDS+RECITXT+RECISET)                              
         DC    AL1(0,0,RECACT)                                                  
         DC    AL4(0,OPTAGYB)                                                   
         DC    AL1(CTFILEQ),XL7'00'                                             
         DC    AL2(3020)                                                        
RACTX    EQU   *                                                                
*                                                                               
RREC     DC    AL1(RRECX-RREC)                                                  
         DCDD  CT#REC,8                                                         
         DC    C'   ',AL1(RECIDDS+RECITXT+RECISET)                              
         DC    AL1(0,0,RECREC)                                                  
         DC    AL4(0,OPTAGYB)                                                   
         DC    AL1(CTFILEQ),XL7'00'                                             
         DC    AL2(3030)                                                        
RRECX    EQU   *                                                                
*                                                                               
RFLD     DC    AL1(RFLDX-RFLD)                                                  
         DCDD  CT#FIELD,8                                                       
         DC    C'   ',AL1(RECIDDS+RECITXT+RECISET)                              
         DC    AL1(0,0,RECFLD)                                                  
         DC    AL4(0,OPTAGYB)                                                   
         DC    AL1(CTFILEQ),XL7'00'                                             
         DC    AL2(3040)                                                        
RFLDX    EQU   *                                                                
*                                                                               
ROPT     DC    AL1(ROPTX-ROPT)                                                  
         DCDD  CT#OPTS,8                                                        
         DC    C'   ',AL1(RECIDDS+RECITXT+RECISET)                              
         DC    AL1(0,0,RECOPT)                                                  
         DC    AL4(0,OPTAGYB)                                                   
         DC    AL1(CTFILEQ),XL7'00'                                             
         DC    AL2(3050)                                                        
ROPTX    EQU   *                                                                
*                                                                               
ROFF     DC    AL1(ROFFX-ROFF)                                                  
         DCDD  CT#OFF,8                                                         
         DC    C'   ',AL1(RECITXT+RECISET)                                      
         DC    AL1(0,0,RECOFF)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(CTFILEQ),XL7'00'                                             
         DC    AL2(3060)                                                        
ROFFX    EQU   *                                                                
*                                                                               
RDPT     DC    AL1(RDPTX-RDPT)                                                  
         DCDD  CT#DPT,8                                                         
         DC    C'   ',AL1(RECITXT+RECISET)                                      
         DC    AL1(0,0,RECDPT)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(CTFILEQ),XL7'00'                                             
         DC    AL2(3070)                                                        
RDPTX    EQU   *                                                                
*                                                                               
RPER     DC    AL1(RPERX-RPER)                                                  
         DCDD  CT#PRSN,8                                                        
         DC    C'   ',AL1(RECITXT+RECISET)                                      
         DC    AL1(0,0,RECPER)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(CTFILEQ),AL1(RECUXACT),XL6'00'                               
         DC    AL2(3080)                                                        
RPERX    EQU   *                                                                
*                                                                               
RSYS     DC    AL1(RSYSX-RSYS)                                                  
         DCDD  CT#SYS,8                                                         
         DC    C'   ',AL1(RECITXT+RECISET)                                      
         DC    AL1(0,0,RECSYS)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(CTFILEQ),AL1(RECUXACT),XL6'00'                               
         DC    AL2(3090)                                                        
RSYSX    EQU   *                                                                
*                                                                               
RPWD     DC    AL1(RPWDX-RPWD)                                                  
         DCDD  CT#PSWD,8                                                        
         DC    C'   ',AL1(RECITXT+RECISET)                                      
         DC    AL1(0,0,RECPWD)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(CTFILEQ),XL7'00'                                             
         DC    AL2(3100)                                                        
RPWDX    EQU   *                                                                
*                                                                               
RAGR     DC    AL1(RAGRX-RAGR)                                                  
         DCDD  CT#GROUP,8                                                       
         DC    C'   ',AL1(RECITXT+RECISET+RECIDIS2)                             
         DC    AL1(0,0,RECAGR)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(CTFILEQ),XL7'00'                                             
         DC    AL2(3110)                                                        
RAGRX    EQU   *                                                                
*                                                                               
RFCT     DC    AL1(RFCTX-RFCT)                                                  
         DCDD  CT#FCON,8                                                        
         DC    C'   ',AL1(RECITXT+RECISET+RECIDIS2)                             
         DC    AL1(0,0,RECFCT)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(CTFILEQ),XL7'00'                                             
         DC    AL2(3120)                                                        
RFCTX    EQU   *                                                                
*                                                                               
ROCT     DC    AL1(ROCTX-ROCT)                                                  
         DCDD  CT#OCON,8                                                        
         DC    C'   ',AL1(RECITXT+RECISET+RECIDIS2)                             
         DC    AL1(0,0,RECOCT)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(CTFILEQ),XL7'00'                                             
         DC    AL2(3130)                                                        
ROCTX    EQU   *                                                                
*                                                                               
RAGE     DC    AL1(RAGEX-RAGE)                                                  
         DCDD  CT#AGY,8                                                         
         DC    C'   ',AL1(RECIDDS+RECITXT+RECISET)                              
         DC    AL1(0,0,RECAGE)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(CTFILEQ),XL7'00'                                             
         DC    AL2(3150)                                                        
RAGEX    EQU   *                                                                
*                                                                               
RLIM     DC    AL1(RLIMX-RLIM)                                                  
         DCDD  CT#LIMIT,8                                                       
         DC    C'   ',AL1(RECITXT+RECISET)                                      
         DC    AL1(0,0,RECLIM)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(CTFILEQ),XL7'00'                                             
         DC    AL2(3160)                                                        
RLIMX    EQU   *                                                                
*                                                                               
RDDS     DC    AL1(RDDSX-RDDS)                                                  
*&&UK*&& DCDD  CT#DDS,8                                                         
*&&US*&& DC    CL8'DDS     '                                                    
         DC    C'   ',AL1(RECIDDS+RECITXT+RECISET)                              
         DC    AL1(0,0,RECDDS)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(CTFILEQ),XL7'00'                                             
         DC    AL2(3170)                                                        
RDDSX    EQU   *                                                                
*                                                                               
RTSA     DC    AL1(RTSAX-RTSA)                                                  
         DCDD  CT#APPR,8                                                        
         DC    C'   ',AL1(RECITXT+RECISET+RECIDIS2)                             
         DC    AL1(0,0,RECTSA)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(CTFILEQ),XL7'00'                                             
         DC    AL2(3179)                                                        
RTSAX    EQU   *                                                                
*                                                                               
RLAL     DC    AL1(RLALX-RLAL)                                                  
         DCDD  CT#TXLIM,8                                                       
         DC    C'   ',AL1(RECITXT+RECISET)                                      
         DC    AL1(0,0,RECLAL)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(CTFILEQ),XL7'00'                                             
         DC    AL2(3190)                                                        
RLALX    EQU   *                                                                
*&&US                                                                           
RDAC     DC    AL1(RDACX-RDAC)           DATA - DATA GROUP RECORDS              
         DCDD  CT#DATAR,8                                                       
         DC    C'   ',AL1(RECITXT+RECISET)                                      
         DC    AL1(0,0,RECDAC)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(CTFILEQ+GENFILQ),XL7'00'                                     
         DC    AL2(3190)                                                        
RDACX    EQU   *                                                                
*                                                                               
RDA2     DC    AL1(RDA2X-RDA2)           DATA2 - DATA ACCESS RECORDS            
         DCDD  CT#DATA2,8                                                       
         DC    C'D2 ',AL1(RECITXT+RECISET)                                      
         DC    AL1(0,0,RECDA2)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(CTFILEQ+GENFILQ),XL7'00'                                     
         DC    AL2(3190)                                                        
RDA2X    EQU   *                                                                
*                                                                               
RMOF     DC    AL1(RMOFX-RMOF)           MOFFICE - MEDIA OFFICE RECORDS         
         DCDD  CT#OACS,8                                                        
         DC    C'   ',AL1(RECITXT+RECISET)                                      
         DC    AL1(0,0,RECMOF)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(GENFILQ),XL7'00'                                             
         DC    AL2(3400)                                                        
RMOFX    EQU   *                                                                
*                                                                               
RMOL     DC    AL1(RMOLX-RMOL)           MOLIST - MEDIA OFFICE LISTS            
         DC    CL8'MOLIST  '                                                    
         DC    C'   ',AL1(RECITXT+RECISET)                                      
         DC    AL1(0,0,RECMOL)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(CTFILEQ),XL7'00'                                             
         DC    AL2(3400)                                                        
RMOLX    EQU   *                                                                
*                                                                               
RMCY     DC    AL1(RMCYX-RMCY)           MCOMPANY - MONEYFLOW COMPANY           
         DCDD  CT#MCPY,8                                                        
         DC    C'MCY',AL1(RECISET)                                              
         DC    AL1(0,0,RECMCY)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(GENFILQ),XL7'00'                                             
         DC    AL2(3400)                                                        
RMCYX    EQU   *                                                                
*                                                                               
RDOM     DC    AL1(RDOMX-RDOM)           DOMAIN                                 
         DCDD  CT#DOM,8                                                         
         DC    C'DOM',AL1(RECISET)                                              
         DC    AL1(0,0,RECDOM)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(GENFILQ),XL7'00'                                             
         DC    AL2(3400)                                                        
RDOMX    EQU   *                                                                
*                                                                               
RTOK     DC    AL1(RTOKX-RTOK)           TOKEN                                  
         DC    CL8'TOKEN   '                                                    
         DC    C'TOK',AL1(RECISET)                                              
         DC    AL1(0,0,RECTOK)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(GENFILQ),XL7'00'                                             
         DC    AL2(3400)                                                        
RTOKX    EQU   *                                                                
*                                                                               
RCID     DC    AL1(RCIDX-RCID)                                                  
         DC    C'ID      '                                                      
         DC    C'   ',AL1(RECITXT+RECISET+RECISET2)                             
         DC    AL1(0,0,RECCID)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(CTFILEQ),XL7'00'                                             
         DC    AL2(0400)                                                        
RCIDX    EQU   *                                                                
*                                                                               
RBAG     DC    AL1(RBAGX-RBAG)                                                  
         DC    C'BUYAGY  '                                                      
         DC    C'BAG',AL1(RECITXT+RECISET)                                      
         DC    AL1(0,0,RECBAG)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(GENFILQ),XL7'00'                                             
         DC    AL2(0400)                                                        
RBAGX    EQU   *                                                                
*                                                                               
RECTABX  DC    AL1(EOT)                                                         
*                                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* ACTION TABLE (SEE ACTTABD)                                                    
***********************************************************************         
ACTTAB   DS    0X                                                               
*                                                                               
         DCDD  CT#ADD,8                                                         
         DC    AL1(0,0,0,ACTADD)                                                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DCDD  CT#DSP,8                                                         
         DC    AL1(0,0,0,ACTDIS)                                                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DCDD  CT#CHG,8                                                         
         DC    AL1(0,0,0,ACTCHA)                                                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DCDD  CT#DEL,8                                                         
         DC    AL1(0,0,0,ACTDEL)                                                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DCDD  CT#RSR,8                                                         
         DC    AL1(0,0,0,ACTRES)                                                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DCDD  CT#LIST,8                                                        
         DC    AL1(0,0,0,ACTLST)                                                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DCDD  CT#SEL,8                                                         
         DC    AL1(0,0,0,ACTSEL)                                                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DCDD  CT#RPT,8                                                         
         DC    AL1(0,0,0,ACTREP)                                                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DCDD  CT#COPY,8                                                        
         DC    AL1(0,0,0,ACTCPY)                                                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DCDD  CT#CONV,8                                                        
         DC    AL1(0,0,0,ACTCNV)                                                
         DC    AL4(0,0)                                                         
         DC    AL1(ACTCHA),XL7'00'                                              
*                                                                               
         DCDD  CT#GRID,8                                                        
         DC    AL1(0,0,0,ACTGRD)                                                
         DC    AL4(0,0)                                                         
         DC    AL1(ACTDIS),XL7'00'                                              
*                                                                               
         DCDD  CT#RST,8                                                         
         DC    AL1(0,0,0,ACTRST)                                                
         DC    AL4(0,0)                                                         
         DC    AL1(ACTCHA),XL7'00'                                              
*                                                                               
ACTTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
                                                                                
***********************************************************************         
* RECORD/ACTION COMBO TABLE (SEE MIXTABD)                                       
***********************************************************************         
MIXTAB   DS    0X                                                               
*                                                                               
MACSADD  DC    AL1(MACSADDX-MACSADD)                                            
         DC    AL1(RECACS,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F609'                                                          
         DC    AL1(4,4,KEYSYS,KEYPGM,KEYUID,KEYAGR,0,0,0,0)                     
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3001)                                                        
MACSADDX EQU   *                                                                
*                                                                               
MACSDIS  DC    AL1(MACSDISX-MACSDIS)                                            
         DC    AL1(RECACS,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F609'                                                          
         DC    AL1(4,4,KEYSYS,KEYPGM,KEYUID,KEYAGR,0,0,0,0)                     
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3002)                                                        
MACSDISX EQU   *                                                                
*                                                                               
MACSCHA  DC    AL1(MACSCHAX-MACSCHA)                                            
         DC    AL1(RECACS,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F609'                                                          
         DC    AL1(4,4,KEYSYS,KEYPGM,KEYUID,KEYAGR,0,0,0,0)                     
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3003)                                                        
MACSCHAX EQU   *                                                                
*                                                                               
MACSDEL  DC    AL1(MACSDELX-MACSDEL)                                            
         DC    AL1(RECACS,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F609'                                                          
         DC    AL1(4,4,KEYSYS,KEYPGM,KEYUID,KEYAGR,0,0,0,0)                     
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3004)                                                        
MACSDELX EQU   *                                                                
*                                                                               
MACSRES  DC    AL1(MACSRESX-MACSRES)                                            
         DC    AL1(RECACS,ACTRES)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F609'                                                          
         DC    AL1(4,4,KEYSYS,KEYPGM,KEYUID,KEYAGR,0,0,0,0)                     
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3005)                                                        
MACSRESX EQU   *                                                                
*                                                                               
MACSLST  DC    AL1(MACSLSTX-MACSLST)                                            
         DC    AL1(RECACS,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'D609'                                                          
         DC    AL1(4,4,KEYSYS,KEYPGM,KEYUID,KEYAGR,0,0,0,0)                     
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3006)                                                        
MACSLSTX EQU   *                                                                
*                                                                               
MACSREP  DC    AL1(MACSREPX-MACSREP)                                            
         DC    AL1(RECACS,ACTREP)                                               
*&&UK*&& DC    AL1(MIXIREP,MIXIOKS+MIXITXT)                                     
*&&US*&& DC    AL1(MIXIREP,MIXIOKS+MIXIOKO+MIXIOKN+MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'B609'                                                          
         DC    AL1(4,4,KEYSYS,KEYPGM,KEYUID,KEYAGR,0,0,0,0)                     
         DC    AL4(0,0)                                                         
         DC    CL8'CTSEAC'                                                      
         DC    AL2(3007)                                                        
MACSREPX EQU   *                                                                
*                                                                               
MACSSEL  DC    AL1(MACSSELX-MACSSEL)                                            
         DC    AL1(RECACS,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'F609'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3008)                                                        
MACSSELX EQU   *                                                                
*                                                                               
MACSGRD  DC    AL1(MACSGRDX-MACSGRD)                                            
         DC    AL1(RECACS,ACTGRD)                                               
         DC    AL1(MIXILFM,MIXINOH)                                             
         DC    AL1(0,0)                                                         
         DC    X'C609'                                                          
         DC    AL1(4,4,KEYSYS,KEYPGM,KEYUID,KEYAGR,0,0,0,0)                     
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MACSGRDX EQU   *                                                                
*                                                                               
MPGMADD  DC    AL1(MPGMADDX-MPGMADD)                                            
         DC    AL1(RECPGM,ACTADD)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ+MIXDREQ,MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'FD02'                                                          
         DC    AL1(2,2,KEYSYS,KEYPGM,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(3011)                                                        
MPGMADDX EQU   *                                                                
*                                                                               
MPGMDIS  DC    AL1(MPGMDISX-MPGMDIS)                                            
         DC    AL1(RECPGM,ACTDIS)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'FD02'                                                          
         DC    AL1(2,2,KEYSYS,KEYPGM,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3012)                                                        
MPGMDISX EQU   *                                                                
*                                                                               
MPGMCHA  DC    AL1(MPGMCHAX-MPGMCHA)                                            
         DC    AL1(RECPGM,ACTCHA)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'FD02'                                                          
         DC    AL1(2,2,KEYSYS,KEYPGM,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(3013)                                                        
MPGMCHAX EQU   *                                                                
*                                                                               
MPGMDEL  DC    AL1(MPGMDELX-MPGMDEL)                                            
         DC    AL1(RECPGM,ACTDEL)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'FD02'                                                          
         DC    AL1(2,2,KEYSYS,KEYPGM,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(3014)                                                        
MPGMDELX EQU   *                                                                
*                                                                               
MPGMRES  DC    AL1(MPGMRESX-MPGMRES)                                            
         DC    AL1(RECPGM,ACTRES)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'FD02'                                                          
         DC    AL1(2,2,KEYSYS,KEYPGM,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(3015)                                                        
MPGMRESX EQU   *                                                                
*                                                                               
MPGMLST  DC    AL1(MPGMLSTX-MPGMLST)                                            
         DC    AL1(RECPGM,ACTLST)                                               
         DC    AL1(MIXIDDS+MIXILST,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'DD02'                                                          
         DC    AL1(2,2,KEYSYS,KEYPGM,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3016)                                                        
MPGMLSTX EQU   *                                                                
*                                                                               
MPGMSEL  DC    AL1(MPGMSELX-MPGMSEL)                                            
         DC    AL1(RECPGM,ACTSEL)                                               
         DC    AL1(MIXIDDS+MIXISEL,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'FD02'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(3018)                                                        
MPGMSELX EQU   *                                                                
*                                                                               
MACTADD  DC    AL1(MACTADDX-MACTADD)                                            
         DC    AL1(RECACT,ACTADD)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'FC03'                                                          
         DC    AL1(3,3,KEYSYS,KEYPGM,KEYACT,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(3021)                                                        
MACTADDX EQU   *                                                                
*                                                                               
MACTDIS  DC    AL1(MACTDISX-MACTDIS)                                            
         DC    AL1(RECACT,ACTDIS)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'FC03'                                                          
         DC    AL1(3,3,KEYSYS,KEYPGM,KEYACT,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3022)                                                        
MACTDISX EQU   *                                                                
*                                                                               
MACTCHA  DC    AL1(MACTCHAX-MACTCHA)                                            
         DC    AL1(RECACT,ACTCHA)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'FC03'                                                          
         DC    AL1(3,3,KEYSYS,KEYPGM,KEYACT,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(3023)                                                        
MACTCHAX EQU   *                                                                
*                                                                               
MACTDEL  DC    AL1(MACTDELX-MACTDEL)                                            
         DC    AL1(RECACT,ACTDEL)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'FC03'                                                          
         DC    AL1(3,3,KEYSYS,KEYPGM,KEYACT,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(3024)                                                        
MACTDELX EQU   *                                                                
*                                                                               
MACTRES  DC    AL1(MACTRESX-MACTRES)                                            
         DC    AL1(RECACT,ACTRES)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'FC03'                                                          
         DC    AL1(3,3,KEYSYS,KEYPGM,KEYACT,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(3025)                                                        
MACTRESX EQU   *                                                                
*                                                                               
MACTLST  DC    AL1(MACTLSTX-MACTLST)                                            
         DC    AL1(RECACT,ACTLST)                                               
         DC    AL1(MIXIDDS+MIXILST,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'DC03'                                                          
         DC    AL1(2,2,KEYSYS,KEYPGM,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3026)                                                        
MACTLSTX EQU   *                                                                
*                                                                               
MACTSEL  DC    AL1(MACTSELX-MACTSEL)                                            
         DC    AL1(RECACT,ACTSEL)                                               
         DC    AL1(MIXIDDS+MIXISEL,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'FC03'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(3028)                                                        
MACTSELX EQU   *                                                                
*                                                                               
MRECADD  DC    AL1(MRECADDX-MRECADD)                                            
         DC    AL1(RECREC,ACTADD)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'FB04'                                                          
         DC    AL1(3,3,KEYSYS,KEYPGM,KEYREC,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(3031)                                                        
MRECADDX EQU   *                                                                
*                                                                               
MRECDIS  DC    AL1(MRECDISX-MRECDIS)                                            
         DC    AL1(RECREC,ACTDIS)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'FB04'                                                          
         DC    AL1(3,3,KEYSYS,KEYPGM,KEYREC,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3032)                                                        
MRECDISX EQU   *                                                                
*                                                                               
MRECCHA  DC    AL1(MRECCHAX-MRECCHA)                                            
         DC    AL1(RECREC,ACTCHA)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'FB04'                                                          
         DC    AL1(3,3,KEYSYS,KEYPGM,KEYREC,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(3033)                                                        
MRECCHAX EQU   *                                                                
*                                                                               
MRECDEL  DC    AL1(MRECDELX-MRECDEL)                                            
         DC    AL1(RECREC,ACTDEL)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'FB04'                                                          
         DC    AL1(3,3,KEYSYS,KEYPGM,KEYREC,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(3034)                                                        
MRECDELX EQU   *                                                                
*                                                                               
MRECRES  DC    AL1(MRECRESX-MRECRES)                                            
         DC    AL1(RECREC,ACTRES)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'FB04'                                                          
         DC    AL1(3,3,KEYSYS,KEYPGM,KEYREC,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(3035)                                                        
MRECRESX EQU   *                                                                
*                                                                               
MRECLST  DC    AL1(MRECLSTX-MRECLST)                                            
         DC    AL1(RECREC,ACTLST)                                               
         DC    AL1(MIXIDDS+MIXILST,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'DB04'                                                          
         DC    AL1(2,2,KEYSYS,KEYPGM,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3036)                                                        
MRECLSTX EQU   *                                                                
*                                                                               
MRECSEL  DC    AL1(MRECSELX-MRECSEL)                                            
         DC    AL1(RECREC,ACTSEL)                                               
         DC    AL1(MIXIDDS+MIXISEL,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'FB04'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(3038)                                                        
MRECSELX EQU   *                                                                
*                                                                               
MFLDADD  DC    AL1(MFLDADDX-MFLDADD)                                            
         DC    AL1(RECFLD,ACTADD)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'FA05'                                                          
         DC    AL1(3,3,KEYSYS,KEYPGM,KEYFLD,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(3041)                                                        
MFLDADDX EQU   *                                                                
*                                                                               
MFLDDIS  DC    AL1(MFLDDISX-MFLDDIS)                                            
         DC    AL1(RECFLD,ACTDIS)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'FA05'                                                          
         DC    AL1(3,3,KEYSYS,KEYPGM,KEYFLD,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3042)                                                        
MFLDDISX EQU   *                                                                
*                                                                               
MFLDCHA  DC    AL1(MFLDCHAX-MFLDCHA)                                            
         DC    AL1(RECFLD,ACTCHA)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'FA05'                                                          
         DC    AL1(3,3,KEYSYS,KEYPGM,KEYFLD,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(3043)                                                        
MFLDCHAX EQU   *                                                                
*                                                                               
MFLDDEL  DC    AL1(MFLDDELX-MFLDDEL)                                            
         DC    AL1(RECFLD,ACTDEL)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'FA05'                                                          
         DC    AL1(3,3,KEYSYS,KEYPGM,KEYFLD,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(3044)                                                        
MFLDDELX EQU   *                                                                
*                                                                               
MFLDRES  DC    AL1(MFLDRESX-MFLDRES)                                            
         DC    AL1(RECFLD,ACTRES)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'FA05'                                                          
         DC    AL1(3,3,KEYSYS,KEYPGM,KEYFLD,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(3045)                                                        
MFLDRESX EQU   *                                                                
*                                                                               
MFLDLST  DC    AL1(MFLDLSTX-MFLDLST)                                            
         DC    AL1(RECFLD,ACTLST)                                               
         DC    AL1(MIXIDDS+MIXILST,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'DA05'                                                          
         DC    AL1(2,2,KEYSYS,KEYPGM,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3046)                                                        
MFLDLSTX EQU   *                                                                
*                                                                               
MFLDSEL  DC    AL1(MFLDSELX-MFLDSEL)                                            
         DC    AL1(RECFLD,ACTSEL)                                               
         DC    AL1(MIXIDDS+MIXISEL,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'FA05'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(3048)                                                        
MFLDSELX EQU   *                                                                
*                                                                               
MOPTADD  DC    AL1(MOPTADDX-MOPTADD)                                            
         DC    AL1(RECOPT,ACTADD)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'F20D'                                                          
         DC    AL1(3,3,KEYSYS,KEYPGM,KEYOPT,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(3051)                                                        
MOPTADDX EQU   *                                                                
*                                                                               
MOPTDIS  DC    AL1(MOPTDISX-MOPTDIS)                                            
         DC    AL1(RECOPT,ACTDIS)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'F20D'                                                          
         DC    AL1(3,3,KEYSYS,KEYPGM,KEYOPT,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3052)                                                        
MOPTDISX EQU   *                                                                
*                                                                               
MOPTCHA  DC    AL1(MOPTCHAX-MOPTCHA)                                            
         DC    AL1(RECOPT,ACTCHA)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'F20D'                                                          
         DC    AL1(3,3,KEYSYS,KEYPGM,KEYOPT,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(3053)                                                        
MOPTCHAX EQU   *                                                                
*                                                                               
MOPTDEL  DC    AL1(MOPTDELX-MOPTDEL)                                            
         DC    AL1(RECOPT,ACTDEL)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'F20D'                                                          
         DC    AL1(3,3,KEYSYS,KEYPGM,KEYOPT,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(3054)                                                        
MOPTDELX EQU   *                                                                
*                                                                               
MOPTRES  DC    AL1(MOPTRESX-MOPTRES)                                            
         DC    AL1(RECOPT,ACTRES)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'F20D'                                                          
         DC    AL1(3,3,KEYSYS,KEYPGM,KEYOPT,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(3055)                                                        
MOPTRESX EQU   *                                                                
*                                                                               
MOPTLST  DC    AL1(MOPTLSTX-MOPTLST)                                            
         DC    AL1(RECOPT,ACTLST)                                               
         DC    AL1(MIXIDDS+MIXILST,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'D20D'                                                          
         DC    AL1(2,2,KEYSYS,KEYPGM,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3056)                                                        
MOPTLSTX EQU   *                                                                
*                                                                               
MOPTSEL  DC    AL1(MOPTSELX-MOPTSEL)                                            
         DC    AL1(RECOPT,ACTSEL)                                               
         DC    AL1(MIXIDDS+MIXISEL,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F20D'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(3057)                                                        
MOPTSELX EQU   *                                                                
*                                                                               
MOFFADD  DC    AL1(MOFFADDX-MOFFADD)                                            
         DC    AL1(RECOFF,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F906'                                                          
         DC    AL1(1,1,KEYOID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3061)                                                        
MOFFADDX EQU   *                                                                
*                                                                               
MOFFDIS  DC    AL1(MOFFDISX-MOFFDIS)                                            
         DC    AL1(RECOFF,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F906'                                                          
         DC    AL1(1,1,KEYOID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3062)                                                        
MOFFDISX EQU   *                                                                
*                                                                               
MOFFCHA  DC    AL1(MOFFCHAX-MOFFCHA)                                            
         DC    AL1(RECOFF,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F906'                                                          
         DC    AL1(1,1,KEYOID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3063)                                                        
MOFFCHAX EQU   *                                                                
*                                                                               
MOFFDEL  DC    AL1(MOFFDELX-MOFFDEL)                                            
         DC    AL1(RECOFF,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F906'                                                          
         DC    AL1(1,1,KEYOID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3064)                                                        
MOFFDELX EQU   *                                                                
*                                                                               
MOFFRES  DC    AL1(MOFFRESX-MOFFRES)                                            
         DC    AL1(RECOFF,ACTRES)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F906'                                                          
         DC    AL1(1,1,KEYOID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3065)                                                        
MOFFRESX EQU   *                                                                
*                                                                               
MOFFLST  DC    AL1(MOFFLSTX-MOFFLST)                                            
         DC    AL1(RECOFF,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'D906'                                                          
         DC    AL1(1,1,KEYOID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3066)                                                        
MOFFLSTX EQU   *                                                                
*                                                                               
MOFFREP  DC    AL1(MOFFREPX-MOFFREP)                                            
         DC    AL1(RECOFF,ACTREP)                                               
*&&UK*&& DC    AL1(MIXIREP,MIXIOKS+MIXITXT)                                     
*&&US*&& DC    AL1(MIXIREP,MIXIOKS+MIXIOKO+MIXIOKN+MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'B906'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    CL8'CTSEOF'                                                      
         DC    AL2(3067)                                                        
MOFFREPX EQU   *                                                                
*                                                                               
MOFFSEL  DC    AL1(MOFFSELX-MOFFSEL)                                            
         DC    AL1(RECOFF,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'F906'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3068)                                                        
MOFFSELX EQU   *                                                                
*                                                                               
MDPTADD  DC    AL1(MDPTADDX-MDPTADD)                                            
         DC    AL1(RECDPT,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F807'                                                          
         DC    AL1(2,2,KEYOID,KEYDID,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3071)                                                        
MDPTADDX EQU   *                                                                
*                                                                               
MDPTDIS  DC    AL1(MDPTDISX-MDPTDIS)                                            
         DC    AL1(RECDPT,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F807'                                                          
         DC    AL1(2,2,KEYOID,KEYDID,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3072)                                                        
MDPTDISX EQU   *                                                                
*                                                                               
MDPTCHA  DC    AL1(MDPTCHAX-MDPTCHA)                                            
         DC    AL1(RECDPT,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F807'                                                          
         DC    AL1(2,2,KEYOID,KEYDID,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3073)                                                        
MDPTCHAX EQU   *                                                                
*                                                                               
MDPTDEL  DC    AL1(MDPTDELX-MDPTDEL)                                            
         DC    AL1(RECDPT,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F807'                                                          
         DC    AL1(2,2,KEYOID,KEYDID,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3074)                                                        
MDPTDELX EQU   *                                                                
*                                                                               
MDPTRES  DC    AL1(MDPTRESX-MDPTRES)                                            
         DC    AL1(RECDPT,ACTRES)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F807'                                                          
         DC    AL1(2,2,KEYOID,KEYDID,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3075)                                                        
MDPTRESX EQU   *                                                                
*                                                                               
MDPTLST  DC    AL1(MDPTLSTX-MDPTLST)                                            
         DC    AL1(RECDPT,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'D807'                                                          
         DC    AL1(2,2,KEYOID,KEYDID,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3076)                                                        
MDPTLSTX EQU   *                                                                
*                                                                               
MDPTREP  DC    AL1(MDPTREPX-MDPTREP)                                            
         DC    AL1(RECDPT,ACTREP)                                               
*&&UK*&& DC    AL1(MIXIREP,MIXIOKS+MIXITXT)                                     
*&&US*&& DC    AL1(MIXIREP,MIXIOKS+MIXIOKO+MIXIOKN+MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'B807'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    CL8'CTSEDE'                                                      
         DC    AL2(3077)                                                        
MDPTREPX EQU   *                                                                
*                                                                               
MDPTSEL  DC    AL1(MDPTSELX-MDPTSEL)                                            
         DC    AL1(RECDPT,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'F807'                                                          
         DC    AL1(2,2,KEYOID,KEYDID,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3078)                                                        
MDPTSELX EQU   *                                                                
                                                                                
*----------------------------------------                                       
* PERSON                                                                        
*----------------------------------------                                       
MPERADD  DC    AL1(MPERADDX-MPERADD)                                            
         DC    AL1(RECPER,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F708'                                                          
         DC    AL1(2,2,KEYPID,KEYPWD,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3081)                                                        
MPERADDX EQU   *                                                                
*                                                                               
MPERDIS  DC    AL1(MPERDISX-MPERDIS)                                            
         DC    AL1(RECPER,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F708'                                                          
         DC    AL1(2,2,KEYPID,KEYPWD,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3082)                                                        
MPERDISX EQU   *                                                                
*                                                                               
MPERCHA  DC    AL1(MPERCHAX-MPERCHA)                                            
         DC    AL1(RECPER,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F708'                                                          
         DC    AL1(2,2,KEYPID,KEYPWD,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3083)                                                        
MPERCHAX EQU   *                                                                
*                                                                               
MPERDEL  DC    AL1(MPERDELX-MPERDEL)                                            
         DC    AL1(RECPER,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F708'                                                          
         DC    AL1(2,2,KEYPID,KEYPWD,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3084)                                                        
MPERDELX EQU   *                                                                
*                                                                               
MPERRES  DC    AL1(MPERRESX-MPERRES)                                            
         DC    AL1(RECPER,ACTRES)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F708'                                                          
         DC    AL1(2,2,KEYPID,KEYPWD,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3085)                                                        
MPERRESX EQU   *                                                                
*                                                                               
MPERLST  DC    AL1(MPERLSTX-MPERLST)                                            
         DC    AL1(RECPER,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'D708'                                                          
         DC    AL1(1,8,KEYPID,0,0,0,0,KEYAGR,KEYOID,KEYDID)                     
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3086)                                                        
MPERLSTX EQU   *                                                                
*                                                                               
MPERREP  DC    AL1(MPERREPX-MPERREP)                                            
         DC    AL1(RECPER,ACTREP)                                               
*&&UK*&& DC    AL1(MIXIREP,MIXIOKS+MIXITXT)                                     
*&&US*&& DC    AL1(MIXIREP,MIXIOKS+MIXIOKO+MIXIOKN+MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'B711'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    CL8'CTSEPE'                                                      
         DC    AL2(3087)                                                        
MPERREPX EQU   *                                                                
*                                                                               
MPERSEL  DC    AL1(MPERSELX-MPERSEL)                                            
         DC    AL1(RECPER,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'F708'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MPERSELX EQU   *                                                                
*                                                                               
MPERCPY  DC    AL1(MPERCPYX-MPERCPY)                                            
         DC    AL1(RECPER,ACTCPY)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F708'                                                          
         DC    AL1(2,2,KEYPID,KEYPWD,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3089)                                                        
MPERCPYX EQU   *                                                                
*                                                                               
MPERCNV  DC    AL1(MPERCNVX-MPERCNV)                                            
         DC    AL1(RECPER,ACTCNV)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F708'                                                          
         DC    AL1(2,2,KEYPID,KEYPWD,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3088)                                                        
MPERCNVX EQU   *                                                                
*                                                                               
MPERGRD  DC    AL1(MPERGRDX-MPERGRD)                                            
         DC    AL1(RECPER,ACTGRD)                                               
         DC    AL1(MIXILFM,MIXINOH)                                             
         DC    AL1(0,0)                                                         
         DC    X'E708'                                                          
         DC    AL1(1,1,KEYPID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MPERGRDX EQU   *                                                                
                                                                                
*----------------------------------------                                       
* DATA ACCESS GROUP (DATA)                                                      
*----------------------------------------                                       
MDA1ADD  DC    AL1(MDA1ADDX-MDA1ADD)                                            
         DC    AL1(RECDAC,ACTADD)                                               
         DC    AL1(MIXILFM+MIXDREQ+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'A722'                                                          
         DC    AL1(1,1,KEYAGR,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3111)                                                        
MDA1ADDX EQU   *                                                                
*                                                                               
MDA1DIS  DC    AL1(MDA1DISX-MDA1DIS)                                            
         DC    AL1(RECDAC,ACTDIS)                                               
         DC    AL1(MIXILFM,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'A722'                                                          
         DC    AL1(1,1,KEYAGR,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3112)                                                        
MDA1DISX EQU   *                                                                
*                                                                               
MDA1CHA  DC    AL1(MDA1CHAX-MDA1CHA)                                            
         DC    AL1(RECDAC,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'A722'                                                          
         DC    AL1(1,1,KEYAGR,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3113)                                                        
MDA1CHAX EQU   *                                                                
*                                                                               
MDA1DEL  DC    AL1(MDA1DELX-MDA1DEL)                                            
         DC    AL1(RECDAC,ACTDEL)                                               
         DC    AL1(MIXILFM,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'A722'                                                          
         DC    AL1(1,1,KEYAGR,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3114)                                                        
MDA1DELX EQU   *                                                                
*                                                                               
MDA1RES  DC    AL1(MDA1RESX-MDA1RES)                                            
         DC    AL1(RECDAC,ACTRES)                                               
         DC    AL1(MIXILFM,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'A722'                                                          
         DC    AL1(1,1,KEYAGR,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3115)                                                        
MDA1RESX EQU   *                                                                
*                                                                               
MDA1LST  DC    AL1(MDA1LSTX-MDA1LST)                                            
         DC    AL1(RECDAC,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'A822'                                                          
         DC    AL1(1,1,KEYAGR,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3116)                                                        
MDA1LSTX EQU   *                                                                
*                                                                               
MDA1SEL  DC    AL1(MDA1SELX-MDA1SEL)                                            
         DC    AL1(RECDAC,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'A722'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3118)                                                        
MDA1SELX EQU   *                                                                
*                                                                               
MDA1GRD  DC    AL1(MDA1GRDX-MDA1GRD)                                            
         DC    AL1(RECDAC,ACTGRD)                                               
         DC    AL1(MIXILFM,MIXINOH)                                             
         DC    AL1(0,0)                                                         
         DC    X'A422'                                                          
         DC    AL1(1,1,KEYAGR,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MDA1GRDX EQU   *                                                                
                                                                                
*----------------------------------------                                       
* DATA ACCESS LIST (DATA2)                                                      
*----------------------------------------                                       
MDA2DIS  DC    AL1(MDA2DISX-MDA2DIS)                                            
         DC    AL1(RECDA2,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'A621'                                                          
         DC    AL1(3,3,KEYSYS,KEYAGR,KEYDEF,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3082)                                                        
MDA2DISX EQU   *                                                                
*                                                                               
MDA2CHA  DC    AL1(MDA2CHAX-MDA2CHA)                                            
         DC    AL1(RECDA2,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'A621'                                                          
         DC    AL1(3,3,KEYSYS,KEYAGR,KEYDEF,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3083)                                                        
MDA2CHAX EQU   *                                                                
*                                                                               
MDA2LST  DC    AL1(MDA2LSTX-MDA2LST)                                            
         DC    AL1(RECDA2,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'A822'                                                          
         DC    AL1(1,1,KEYAGR,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3116)                                                        
MDA2LSTX EQU   *                                                                
*                                                                               
MDA2SEL  DC    AL1(MDA2SELX-MDA2SEL)                                            
         DC    AL1(RECDA2,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'A621'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3086)                                                        
MDA2SELX EQU   *                                                                
*                                                                               
*----------------------------------------                                       
* SYSTEM                                                                        
*----------------------------------------                                       
MSYSDIS  DC    AL1(MSYSDISX-MSYSDIS)                                            
         DC    AL1(RECSYS,ACTDIS)                                               
         DC    AL1(MIXILFM,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'F50A'                                                          
         DC    AL1(1,1,KEYPID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3092)                                                        
MSYSDISX EQU   *                                                                
*                                                                               
MSYSCHA  DC    AL1(MSYSCHAX-MSYSCHA)                                            
         DC    AL1(RECSYS,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F50A'                                                          
         DC    AL1(1,1,KEYPID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3093)                                                        
MSYSCHAX EQU   *                                                                
*                                                                               
MSYSLST  DC    AL1(MSYSLSTX-MSYSLST)                                            
         DC    AL1(RECSYS,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'D50A'                                                          
         DC    AL1(1,1,KEYPID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3096)                                                        
MSYSLSTX EQU   *                                                                
*                                                                               
MSYSREP  DC    AL1(MSYSREPX-MSYSREP)                                            
         DC    AL1(RECSYS,ACTREP)                                               
*&&UK*&& DC    AL1(MIXIREP,MIXIOKS+MIXITXT)                                     
*&&US*&& DC    AL1(MIXIREP,MIXIOKS+MIXIOKO+MIXIOKN+MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'B515'                                                          
         DC    AL1(1,1,KEYPID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    CL8'CTSESY'                                                      
         DC    AL2(3097)                                                        
MSYSREPX EQU   *                                                                
*                                                                               
MSYSSEL  DC    AL1(MSYSSELX-MSYSSEL)                                            
         DC    AL1(RECSYS,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'F50A'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3098)                                                        
MSYSSELX EQU   *                                                                
                                                                                
*----------------------------------------                                       
* PASSWORD                                                                      
*----------------------------------------                                       
MPWDLST  DC    AL1(MPWDLSTX-MPWDLST)                                            
         DC    AL1(RECPWD,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'D30C'                                                          
         DC    AL1(1,1,KEYPWD,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3106)                                                        
MPWDLSTX EQU   *                                                                
*                                                                               
*PWDDIS  DC    AL1(MPWDDISX-MPWDDIS)                                            
*        DC    AL1(RECPWD,ACTDIS)                                               
*        DC    AL1(MIXILFM+MIXKREQ,MIXINOH)                                     
*        DC    AL1(0,0)                                                         
*        DC    X'E320'                                                          
*        DC    AL1(1,1,KEYPID,0,0,0,0,0,0,0)                                    
*        DC    AL4(0,0)                                                         
*        DC    XL8'00'                                                          
*        DC    AL2(3107)                                                        
*PWDDISX EQU   *                                                                
*                                                                               
*PWDCHA  DC    AL1(MPWDCHAX-MPWDCHA)                                            
*        DC    AL1(RECPWD,ACTCHA)                                               
*        DC    AL1(MIXILFM+MIXKREQ,MIXINOH)                                     
*        DC    AL1(0,0)                                                         
*        DC    X'E320'                                                          
*        DC    AL1(1,1,KEYPID,0,0,0,0,0,0,0)                                    
*        DC    AL4(0,0)                                                         
*        DC    XL8'00'                                                          
*        DC    AL2(3107)                                                        
*PWDCHAX EQU   *                                                                
*                                                                               
MPWDRST  DC    AL1(MPWDRSTX-MPWDRST)                                            
         DC    AL1(RECPWD,ACTRST)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXINOH)                                     
         DC    AL1(0,0)                                                         
         DC    X'E320'                                                          
         DC    AL1(1,1,KEYPID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3107)                                                        
MPWDRSTX EQU   *                                                                
                                                                                
*----------------------------------------                                       
* GROUP (ACCESS GROUP)                                                          
*----------------------------------------                                       
MAGRADD  DC    AL1(MAGRADDX-MAGRADD)                                            
         DC    AL1(RECAGR,ACTADD)                                               
         DC    AL1(MIXILFM+MIXDREQ+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'F40B'                                                          
         DC    AL1(1,1,KEYAGR,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3111)                                                        
MAGRADDX EQU   *                                                                
*                                                                               
MAGRDIS  DC    AL1(MAGRDISX-MAGRDIS)                                            
         DC    AL1(RECAGR,ACTDIS)                                               
         DC    AL1(MIXILFM,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'F40B'                                                          
         DC    AL1(1,1,KEYAGR,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3112)                                                        
MAGRDISX EQU   *                                                                
*                                                                               
MAGRCHA  DC    AL1(MAGRCHAX-MAGRCHA)                                            
         DC    AL1(RECAGR,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F40B'                                                          
         DC    AL1(1,1,KEYAGR,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3113)                                                        
MAGRCHAX EQU   *                                                                
*                                                                               
MAGRDEL  DC    AL1(MAGRDELX-MAGRDEL)                                            
         DC    AL1(RECAGR,ACTDEL)                                               
         DC    AL1(MIXILFM,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'F40B'                                                          
         DC    AL1(1,1,KEYAGR,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3114)                                                        
MAGRDELX EQU   *                                                                
*                                                                               
MAGRRES  DC    AL1(MAGRRESX-MAGRRES)                                            
         DC    AL1(RECAGR,ACTRES)                                               
         DC    AL1(MIXILFM,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'F40B'                                                          
         DC    AL1(1,1,KEYAGR,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3115)                                                        
MAGRRESX EQU   *                                                                
*                                                                               
MAGRLST  DC    AL1(MAGRLSTX-MAGRLST)                                            
         DC    AL1(RECAGR,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'D40B'                                                          
         DC    AL1(1,1,KEYAGR,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3116)                                                        
MAGRLSTX EQU   *                                                                
*                                                                               
MAGRREP  DC    AL1(MAGRREPX-MAGRREP)                                            
         DC    AL1(RECAGR,ACTREP)                                               
*&&UK*&& DC    AL1(MIXIREP,MIXIOKS+MIXITXT)                                     
*&&US*&& DC    AL1(MIXIREP,MIXIOKS+MIXIOKO+MIXIOKN+MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'B40B'                                                          
         DC    AL1(1,1,KEYAGR,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    CL8'CTSEGR'                                                      
         DC    AL2(3117)                                                        
MAGRREPX EQU   *                                                                
*                                                                               
MAGRSEL  DC    AL1(MAGRSELX-MAGRSEL)                                            
         DC    AL1(RECAGR,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'F40B'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3118)                                                        
MAGRSELX EQU   *                                                                
                                                                                
*----------------------------------------                                       
* FCONTROL (FIELD CONTROL)                                                      
*----------------------------------------                                       
MFCTADD  DC    AL1(MFCTADDX-MFCTADD)                                            
         DC    AL1(RECFCT,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F00F'                                                          
         DC    AL1(4,4,KEYSYS,KEYPGM,KEYUID,KEYAGR,0,0,0,0)                     
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3121)                                                        
MFCTADDX EQU   *                                                                
*                                                                               
MFCTDIS  DC    AL1(MFCTDISX-MFCTDIS)                                            
         DC    AL1(RECFCT,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F00F'                                                          
         DC    AL1(4,4,KEYSYS,KEYPGM,KEYUID,KEYAGR,0,0,0,0)                     
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3122)                                                        
MFCTDISX EQU   *                                                                
*                                                                               
MFCTCHA  DC    AL1(MFCTCHAX-MFCTCHA)                                            
         DC    AL1(RECFCT,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F00F'                                                          
         DC    AL1(4,4,KEYSYS,KEYPGM,KEYUID,KEYAGR,0,0,0,0)                     
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3123)                                                        
MFCTCHAX EQU   *                                                                
*                                                                               
MFCTDEL  DC    AL1(MFCTDELX-MFCTDEL)                                            
         DC    AL1(RECFCT,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F00F'                                                          
         DC    AL1(4,4,KEYSYS,KEYPGM,KEYUID,KEYAGR,0,0,0,0)                     
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3124)                                                        
MFCTDELX EQU   *                                                                
*                                                                               
MFCTRES  DC    AL1(MFCTRESX-MFCTRES)                                            
         DC    AL1(RECFCT,ACTRES)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F00F'                                                          
         DC    AL1(4,4,KEYSYS,KEYPGM,KEYUID,KEYAGR,0,0,0,0)                     
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3125)                                                        
MFCTRESX EQU   *                                                                
*                                                                               
MFCTLST  DC    AL1(MFCTLSTX-MFCTLST)                                            
         DC    AL1(RECFCT,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'D00F'                                                          
         DC    AL1(4,4,KEYSYS,KEYPGM,KEYUID,KEYAGR,0,0,0,0)                     
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3126)                                                        
MFCTLSTX EQU   *                                                                
*                                                                               
MFCTREP  DC    AL1(MFCTREPX-MFCTREP)                                            
         DC    AL1(RECFCT,ACTREP)                                               
*&&UK*&& DC    AL1(MIXIREP,MIXIOKS+MIXITXT)                                     
*&&US*&& DC    AL1(MIXIREP,MIXIOKS+MIXIOKO+MIXIOKN+MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'B00F'                                                          
         DC    AL1(4,4,KEYSYS,KEYPGM,KEYUID,KEYAGR,0,0,0,0)                     
         DC    AL4(0,0)                                                         
         DC    CL8'CTSEFC'                                                      
         DC    AL2(3127)                                                        
MFCTREPX EQU   *                                                                
*                                                                               
MFCTSEL  DC    AL1(MFCTSELX-MFCTSEL)                                            
         DC    AL1(RECFCT,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'F00F'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3128)                                                        
MFCTSELX EQU   *                                                                
                                                                                
*----------------------------------------                                       
* OCONTROL (OPTION CONTROL)                                                     
*----------------------------------------                                       
MOCTADD  DC    AL1(MOCTADDX-MOCTADD)                                            
         DC    AL1(RECOCT,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F10E'                                                          
         DC    AL1(4,4,KEYSYS,KEYPGM,KEYUID,KEYAGR,0,0,0,0)                     
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3131)                                                        
MOCTADDX EQU   *                                                                
*                                                                               
MOCTDIS  DC    AL1(MOCTDISX-MOCTDIS)                                            
         DC    AL1(RECOCT,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F10E'                                                          
         DC    AL1(4,4,KEYSYS,KEYPGM,KEYUID,KEYAGR,0,0,0,0)                     
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3132)                                                        
MOCTDISX EQU   *                                                                
*                                                                               
MOCTCHA  DC    AL1(MOCTCHAX-MOCTCHA)                                            
         DC    AL1(RECOCT,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F10E'                                                          
         DC    AL1(4,4,KEYSYS,KEYPGM,KEYUID,KEYAGR,0,0,0,0)                     
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3133)                                                        
MOCTCHAX EQU   *                                                                
*                                                                               
MOCTDEL  DC    AL1(MOCTDELX-MOCTDEL)                                            
         DC    AL1(RECOCT,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F10E'                                                          
         DC    AL1(4,4,KEYSYS,KEYPGM,KEYUID,KEYAGR,0,0,0,0)                     
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3134)                                                        
MOCTDELX EQU   *                                                                
*                                                                               
MOCTRES  DC    AL1(MOCTRESX-MOCTRES)                                            
         DC    AL1(RECOCT,ACTRES)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F10E'                                                          
         DC    AL1(4,4,KEYSYS,KEYPGM,KEYUID,KEYAGR,0,0,0,0)                     
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3135)                                                        
MOCTRESX EQU   *                                                                
*                                                                               
MOCTLST  DC    AL1(MOCTLSTX-MOCTLST)                                            
         DC    AL1(RECOCT,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'D10E'                                                          
         DC    AL1(4,4,KEYSYS,KEYPGM,KEYUID,KEYAGR,0,0,0,0)                     
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3136)                                                        
MOCTLSTX EQU   *                                                                
*                                                                               
MOCTREP  DC    AL1(MOCTREPX-MOCTREP)                                            
         DC    AL1(RECOCT,ACTREP)                                               
*&&UK*&& DC    AL1(MIXIREP,MIXIOKS+MIXITXT)                                     
*&&US*&& DC    AL1(MIXIREP,MIXIOKS+MIXIOKO+MIXIOKN+MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'B10E'                                                          
         DC    AL1(4,4,KEYSYS,KEYPGM,KEYUID,KEYAGR,0,0,0,0)                     
         DC    AL4(0,0)                                                         
         DC    CL8'CTSEOC'                                                      
         DC    AL2(3137)                                                        
MOCTREPX EQU   *                                                                
*                                                                               
MOCTSEL  DC    AL1(MOCTSELX-MOCTSEL)                                            
         DC    AL1(RECOCT,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'F10E'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3138)                                                        
MOCTSELX EQU   *                                                                
*----------------------------------------                                       
*                                                                               
*&&US                                                                           
MLAGADD  DC    AL1(MLAGADDX-MLAGADD)                                            
         DC    AL1(RECLAG,ACTADD)                                               
         DC    AL1(MIXILFM+MIXDREQ+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'EF10'                                                          
         DC    AL1(1,1,KEYLAG,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3141)                                                        
MLAGADDX EQU   *                                                                
*                                                                               
MLAGDIS  DC    AL1(MLAGDISX-MLAGDIS)                                            
         DC    AL1(RECLAG,ACTDIS)                                               
         DC    AL1(MIXILFM,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'EF10'                                                          
         DC    AL1(1,1,KEYLAG,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3142)                                                        
MLAGDISX EQU   *                                                                
*                                                                               
MLAGCHA  DC    AL1(MLAGCHAX-MLAGCHA)                                            
         DC    AL1(RECLAG,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'EF10'                                                          
         DC    AL1(1,1,KEYLAG,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3143)                                                        
MLAGCHAX EQU   *                                                                
*                                                                               
MLAGDEL  DC    AL1(MLAGDELX-MLAGDEL)                                            
         DC    AL1(RECLAG,ACTDEL)                                               
         DC    AL1(MIXILFM,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'EF10'                                                          
         DC    AL1(1,1,KEYLAG,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3144)                                                        
MLAGDELX EQU   *                                                                
*                                                                               
MLAGRES  DC    AL1(MLAGRESX-MLAGRES)                                            
         DC    AL1(RECLAG,ACTRES)                                               
         DC    AL1(MIXILFM,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'EF10'                                                          
         DC    AL1(1,1,KEYLAG,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3145)                                                        
MLAGRESX EQU   *                                                                
*                                                                               
MLAGLST  DC    AL1(MLAGLSTX-MLAGLST)                                            
         DC    AL1(RECLAG,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'CF10'                                                          
         DC    AL1(1,1,KEYLAG,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3146)                                                        
MLAGLSTX EQU   *                                                                
*                                                                               
MLAGREP  DC    AL1(MLAGREPX-MLAGREP)                                            
         DC    AL1(RECLAG,ACTREP)                                               
         DC    AL1(MIXIREP,MIXIOKS+MIXIOKO+MIXIOKN+MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'AF10'                                                          
         DC    AL1(1,1,KEYLAG,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    CL8'CTSELA'                                                      
         DC    AL2(3147)                                                        
MLAGREPX EQU   *                                                                
*                                                                               
MLAGSEL  DC    AL1(MLAGSELX-MLAGSEL)                                            
         DC    AL1(RECLAG,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'EF10'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3148)                                                        
MLAGSELX EQU   *                                                                
*&&                                                                             
*                                                                               
MAGECPY  DC    AL1(MAGECPYX-MAGECPY)                                            
         DC    AL1(RECAGE,ACTCPY)                                               
         DC    AL1(MIXIDDS+MIXILFM,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'EE12'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3150)                                                        
MAGECPYX EQU   *                                                                
*                                                                               
MLIMADD  DC    AL1(MLIMADDX-MLIMADD)                                            
         DC    AL1(RECLIM,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'ED13'                                                          
         DC    AL1(2,2,KEYSYS,KEYLIS,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3161)                                                        
MLIMADDX EQU   *                                                                
*                                                                               
MLIMDIS  DC    AL1(MLIMDISX-MLIMDIS)                                            
         DC    AL1(RECLIM,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT+MIXISET)                             
         DC    AL1(0,0)                                                         
         DC    X'ED13'                                                          
         DC    AL1(2,2,KEYSYS,KEYLIS,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3162)                                                        
MLIMDISX EQU   *                                                                
*                                                                               
MLIMCHA  DC    AL1(MLIMCHAX-MLIMCHA)                                            
         DC    AL1(RECLIM,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'ED13'                                                          
         DC    AL1(2,2,KEYSYS,KEYLIS,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3163)                                                        
MLIMCHAX EQU   *                                                                
*                                                                               
MLIMDEL  DC    AL1(MLIMDELX-MLIMDEL)                                            
         DC    AL1(RECLIM,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'ED13'                                                          
         DC    AL1(2,2,KEYSYS,KEYLIS,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3164)                                                        
MLIMDELX EQU   *                                                                
*                                                                               
MLIMRES  DC    AL1(MLIMRESX-MLIMRES)                                            
         DC    AL1(RECLIM,ACTRES)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'ED13'                                                          
         DC    AL1(2,2,KEYSYS,KEYLIS,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3165)                                                        
MLIMRESX EQU   *                                                                
*                                                                               
MLIMLST  DC    AL1(MLIMLSTX-MLIMLST)                                            
         DC    AL1(RECLIM,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'CD13'                                                          
         DC    AL1(2,2,KEYSYS,KEYLIS,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3166)                                                        
MLIMLSTX EQU   *                                                                
*                                                                               
MLIMREP  DC    AL1(MLIMREPX-MLIMREP)                                            
         DC    AL1(RECLIM,ACTREP)                                               
         DC    AL1(MIXIREP,MIXIOKS+MIXIOKO+MIXIOKN+MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'AD13'                                                          
         DC    AL1(2,2,KEYSYS,KEYLIS,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    CL8'CTSELI'                                                      
         DC    AL2(3167)                                                        
MLIMREPX EQU   *                                                                
*                                                                               
MLIMSEL  DC    AL1(MLIMSELX-MLIMSEL)                                            
         DC    AL1(RECLIM,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'ED13'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3168)                                                        
MLIMSELX EQU   *                                                                
*                                                                               
MDDSDIS  DC    AL1(MDDSDISX-MDDSDIS)                                            
         DC    AL1(RECDDS,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'AE14'                                                          
         DC    AL1(2,2,KEYPID,KEYPWD,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3082)                                                        
MDDSDISX EQU   *                                                                
*                                                                               
MDDSCHA  DC    AL1(MDDSCHAX-MDDSCHA)                                            
         DC    AL1(RECDDS,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'AE14'                                                          
         DC    AL1(2,2,KEYPID,KEYPWD,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3083)                                                        
MDDSCHAX EQU   *                                                                
*                                                                               
MDDSLST  DC    AL1(MDDSLSTX-MDDSLST)                                            
         DC    AL1(RECDDS,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'D708'                                                          
         DC    AL1(1,8,KEYPID,0,0,0,0,KEYAGR,KEYOID,KEYDID)                     
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3086)                                                        
MDDSLSTX EQU   *                                                                
*                                                                               
MDDSREP  DC    AL1(MDDSREPX-MDDSREP)                                            
         DC    AL1(RECDDS,ACTREP)                                               
*&&UK*&& DC    AL1(MIXIREP,MIXIOKS+MIXITXT)                                     
*&&US*&& DC    AL1(MIXIREP,MIXIOKS+MIXIOKO+MIXIOKN+MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'B711'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    CL8'CTSEPE'                                                      
         DC    AL2(3087)                                                        
MDDSREPX EQU   *                                                                
*                                                                               
MDDSSEL  DC    AL1(MDDSSELX-MDDSSEL)                                            
         DC    AL1(RECDDS,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'AE14'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MDDSSELX EQU   *                                                                
*                                                                               
MTSAADD  DC    AL1(MTSAADDX-MTSAADD)                                            
         DC    AL1(RECTSA,ACTADD)                                               
         DC    AL1(MIXILFM+MIXDREQ+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'CC16'                                                          
         DC    AL1(1,1,KEYTSA,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3180)                                                        
MTSAADDX EQU   *                                                                
*                                                                               
MTSADIS  DC    AL1(MTSADISX-MTSADIS)                                            
         DC    AL1(RECTSA,ACTDIS)                                               
         DC    AL1(MIXILFM,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'CC16'                                                          
         DC    AL1(1,1,KEYTSA,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3181)                                                        
MTSADISX EQU   *                                                                
*                                                                               
MTSACHA  DC    AL1(MTSACHAX-MTSACHA)                                            
         DC    AL1(RECTSA,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'CC16'                                                          
         DC    AL1(1,1,KEYTSA,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3182)                                                        
MTSACHAX EQU   *                                                                
*                                                                               
MTSADEL  DC    AL1(MTSADELX-MTSADEL)                                            
         DC    AL1(RECTSA,ACTDEL)                                               
         DC    AL1(MIXILFM,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'CC16'                                                          
         DC    AL1(1,1,KEYTSA,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3183)                                                        
MTSADELX EQU   *                                                                
*                                                                               
MTSARES  DC    AL1(MTSARESX-MTSARES)                                            
         DC    AL1(RECTSA,ACTRES)                                               
         DC    AL1(MIXILFM,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'CC16'                                                          
         DC    AL1(1,1,KEYTSA,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3184)                                                        
MTSARESX EQU   *                                                                
*                                                                               
MTSALST  DC    AL1(MTSALSTX-MTSALST)                                            
         DC    AL1(RECTSA,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'CB16'                                                          
         DC    AL1(1,1,KEYTSA,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3185)                                                        
MTSALSTX EQU   *                                                                
*                                                                               
MTSAREP  DC    AL1(MTSAREPX-MTSAREP)                                            
         DC    AL1(RECTSA,ACTREP)                                               
*&&UK*&& DC    AL1(MIXIREP,MIXIOKS+MIXITXT)                                     
*&&US*&& DC    AL1(MIXIREP,MIXIOKS+MIXIOKO+MIXIOKN+MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'CA16'                                                          
         DC    AL1(1,1,KEYTSA,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    CL8'CTSEAP'                                                      
         DC    AL2(3186)                                                        
MTSAREPX EQU   *                                                                
*                                                                               
MTSASEL  DC    AL1(MTSASELX-MTSASEL)                                            
         DC    AL1(RECTSA,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'CC16'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3187)                                                        
MTSASELX EQU   *                                                                
*                                                                               
MLALADD  DC    AL1(MLALADDX-MLALADD)                                            
         DC    AL1(RECLAL,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'EC17'                                                          
         DC    AL1(2,2,KEYSYS,KEYLIS,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3191)                                                        
MLALADDX EQU   *                                                                
*                                                                               
MLALDIS  DC    AL1(MLALDISX-MLALDIS)                                            
         DC    AL1(RECLAL,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT+MIXISET)                             
         DC    AL1(0,0)                                                         
         DC    X'EC17'                                                          
         DC    AL1(2,2,KEYSYS,KEYLIS,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3192)                                                        
MLALDISX EQU   *                                                                
*                                                                               
MLALCHA  DC    AL1(MLALCHAX-MLALCHA)                                            
         DC    AL1(RECLAL,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'EC17'                                                          
         DC    AL1(2,2,KEYSYS,KEYLIS,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3193)                                                        
MLALCHAX EQU   *                                                                
*                                                                               
MLALDEL  DC    AL1(MLALDELX-MLALDEL)                                            
         DC    AL1(RECLAL,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'EC17'                                                          
         DC    AL1(2,2,KEYSYS,KEYLIS,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3194)                                                        
MLALDELX EQU   *                                                                
*                                                                               
MLALRES  DC    AL1(MLALRESX-MLALRES)                                            
         DC    AL1(RECLAL,ACTRES)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'EC17'                                                          
         DC    AL1(2,2,KEYSYS,KEYLIS,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3195)                                                        
MLALRESX EQU   *                                                                
*                                                                               
MLALLST  DC    AL1(MLALLSTX-MLALLST)                                            
         DC    AL1(RECLAL,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'BC17'                                                          
         DC    AL1(2,2,KEYSYS,KEYLIS,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3196)                                                        
MLALLSTX EQU   *                                                                
*                                                                               
*&&DO                                                                           
MLALREP  DC    AL1(MLALREPX-MLALREP)                                            
         DC    AL1(RECLAL,ACTREP)                                               
         DC    AL1(MIXIREP,MIXIOKS+MIXIOKO+MIXIOKN+MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'AC17'                                                          
         DC    AL1(2,2,KEYSYS,KEYLIS,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    CL8'CTSETL'                                                      
         DC    AL2(3197)                                                        
MLALREPX EQU   *                                                                
*&&                                                                             
*                                                                               
MLALSEL  DC    AL1(MLALSELX-MLALSEL)                                            
         DC    AL1(RECLAL,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'EC17'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3198)                                                        
MLALSELX EQU   *                                                                
*                                                                               
MLALCPY  DC    AL1(MLALCPYX-MLALCPY)                                            
         DC    AL1(RECLAL,ACTCPY)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'EC17'                                                          
         DC    AL1(2,2,KEYSYS,KEYLIS,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3199)                                                        
MLALCPYX EQU   *                                                                
*                                                                               
*&&US                                                                           
*-----------------------------------------                                      
* MEDIA OFFICE  RECORDS                                                         
*-----------------------------------------                                      
MMOFADD  DC    AL1(MMOFADDX-MMOFADD)                                            
         DC    AL1(RECMOF,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'AA19'                                                          
         DC    AL1(3,3,KEYOID,KEYSYS,KEYOID,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3201)                                                        
MMOFADDX EQU   *                                                                
*                                                                               
MMOFDIS  DC    AL1(MMOFDISX-MMOFDIS)                                            
         DC    AL1(RECMOF,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT+MIXISET)                             
         DC    AL1(0,0)                                                         
         DC    X'AA19'                                                          
         DC    AL1(3,3,KEYOID,KEYSYS,KEYOID,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3202)                                                        
MMOFDISX EQU   *                                                                
*                                                                               
MMOFCHA  DC    AL1(MMOFCHAX-MMOFCHA)                                            
         DC    AL1(RECMOF,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'AA19'                                                          
         DC    AL1(3,3,KEYOID,KEYSYS,KEYOID,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3203)                                                        
MMOFCHAX EQU   *                                                                
*                                                                               
MMOFLST  DC    AL1(MMOFLSTX-MMOFLST)                                            
         DC    AL1(RECMOF,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'AB19'                                                          
         DC    AL1(1,1,KEYOID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3206)                                                        
MMOFLSTX EQU   *                                                                
*                                                                               
MMOFSEL  DC    AL1(MMOFSELX-MMOFSEL)                                            
         DC    AL1(RECMOF,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'AA19'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3208)                                                        
MMOFSELX EQU   *                                                                
*-----------------------------------------                                      
* MEDIA OFFICE LISTS                                                            
*-----------------------------------------                                      
MMOLADD  DC    AL1(MMOLADDX-MMOLADD)                                            
         DC    AL1(RECMOL,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'A029'                                                          
         DC    AL1(3,3,KEYOID,KEYSYS,KEYOID,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3201)                                                        
MMOLADDX EQU   *                                                                
*                                                                               
MMOLDIS  DC    AL1(MMOLDISX-MMOLDIS)                                            
         DC    AL1(RECMOL,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT+MIXISET)                             
         DC    AL1(0,0)                                                         
         DC    X'A029'                                                          
         DC    AL1(3,3,KEYOID,KEYSYS,KEYOID,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3202)                                                        
MMOLDISX EQU   *                                                                
*                                                                               
MMOLCHA  DC    AL1(MMOLCHAX-MMOLCHA)                                            
         DC    AL1(RECMOL,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'A029'                                                          
         DC    AL1(3,3,KEYOID,KEYSYS,KEYOID,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3203)                                                        
MMOLCHAX EQU   *                                                                
*                                                                               
MMOLDEL  DC    AL1(MMOLDELX-MMOLDEL)                                            
         DC    AL1(RECMOL,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'A029'                                                          
         DC    AL1(3,3,KEYOID,KEYSYS,KEYOID,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3194)                                                        
MMOLDELX EQU   *                                                                
*                                                                               
MMOLRES  DC    AL1(MMOLRESX-MMOLRES)                                            
         DC    AL1(RECMOL,ACTRES)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'A029'                                                          
         DC    AL1(3,3,KEYOID,KEYSYS,KEYOID,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3194)                                                        
MMOLRESX EQU   *                                                                
*                                                                               
MMOLLST  DC    AL1(MMOLLSTX-MMOLLST)                                            
         DC    AL1(RECMOL,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'A929'                                                          
         DC    AL1(1,1,KEYOID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3206)                                                        
MMOLLSTX EQU   *                                                                
*                                                                               
MMOLSEL  DC    AL1(MMOLSELX-MMOLSEL)                                            
         DC    AL1(RECMOL,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'A029'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3208)                                                        
MMOLSELX EQU   *                                                                
*                                                                               
MMOLREP  DC    AL1(MMOLREPX-MMOLREP)                                            
         DC    AL1(RECMOL,ACTREP)                                               
         DC    AL1(MIXIREP,MIXIOKS+MIXIOKO+MIXIOKN+MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'A129'                                                          
         DC    AL1(2,2,KEYSYS,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    CL8'CTSOSO'                                                      
         DC    AL2(3167)                                                        
MMOLREPX EQU   *                                                                
                                                                                
*-----------------------------------------                                      
* MONEYFLOW COMPANY RECORDS                                                     
*-----------------------------------------                                      
MMCYADD  DC    AL1(MMCYADDX-MMCYADD)                                            
         DC    AL1(RECMCY,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXDREQ,0)                                   
         DC    AL1(0,0)                                                         
         DC    X'A11A'                                                          
         DC    AL1(1,1,KEYOID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MMCYADDX EQU   *                                                                
*                                                                               
MMCYDIS  DC    AL1(MMCYDISX-MMCYDIS)                                            
         DC    AL1(RECMCY,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXISET)                                     
         DC    AL1(0,0)                                                         
         DC    X'A11A'                                                          
         DC    AL1(1,1,KEYOID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MMCYDISX EQU   *                                                                
*                                                                               
MMCYCHA  DC    AL1(MMCYCHAX-MMCYCHA)                                            
         DC    AL1(RECMCY,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    AL1(0,0)                                                         
         DC    X'A11A'                                                          
         DC    AL1(1,1,KEYOID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MMCYCHAX EQU   *                                                                
*                                                                               
MMCYDEL  DC    AL1(MMCYDELX-MMCYDEL)                                            
         DC    AL1(RECMCY,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    AL1(0,0)                                                         
         DC    X'A11A'                                                          
         DC    AL1(1,1,KEYOID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MMCYDELX EQU   *                                                                
*                                                                               
MMCYRES  DC    AL1(MMCYRESX-MMCYRES)                                            
         DC    AL1(RECMCY,ACTRES)                                               
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    AL1(0,0)                                                         
         DC    X'A11A'                                                          
         DC    AL1(1,1,KEYOID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MMCYRESX EQU   *                                                                
*                                                                               
MMCYLST  DC    AL1(MMCYLSTX-MMCYLST)                                            
         DC    AL1(RECMCY,ACTLST)                                               
         DC    AL1(MIXILST,0)                                                   
         DC    AL1(0,0)                                                         
         DC    X'A21A'                                                          
         DC    AL1(1,1,KEYOID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MMCYLSTX EQU   *                                                                
*                                                                               
MMCYSEL  DC    AL1(MMCYSELX-MMCYSEL)                                            
         DC    AL1(RECMCY,ACTSEL)                                               
         DC    AL1(MIXISEL,0)                                                   
         DC    AL1(0,0)                                                         
         DC    X'A11A'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MMCYSELX EQU   *                                                                
*-----------------------------------------                                      
* DOMAIN RECORDS                                                                
*-----------------------------------------                                      
MDOMADD  DC    AL1(MDOMADDX-MDOMADD)                                            
         DC    AL1(RECDOM,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXDREQ,0)                                   
         DC    AL1(0,0)                                                         
         DC    X'BE24'                                                          
         DC    AL1(1,1,KEYOID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MDOMADDX EQU   *                                                                
*                                                                               
MDOMDIS  DC    AL1(MDOMDISX-MDOMDIS)                                            
         DC    AL1(RECDOM,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXISET)                                     
         DC    AL1(0,0)                                                         
         DC    X'BE24'                                                          
         DC    AL1(1,1,KEYOID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MDOMDISX EQU   *                                                                
*                                                                               
MDOMCHA  DC    AL1(MDOMCHAX-MDOMCHA)                                            
         DC    AL1(RECDOM,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXISET)                                     
         DC    AL1(0,0)                                                         
         DC    X'BE24'                                                          
         DC    AL1(1,1,KEYOID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MDOMCHAX EQU   *                                                                
*                                                                               
MDOMDEL  DC    AL1(MDOMDELX-MDOMDEL)                                            
         DC    AL1(RECDOM,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXISET)                                     
         DC    AL1(0,0)                                                         
         DC    X'BE24'                                                          
         DC    AL1(1,1,KEYOID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MDOMDELX EQU   *                                                                
*                                                                               
MDOMRES  DC    AL1(MDOMRESX-MDOMRES)                                            
         DC    AL1(RECDOM,ACTRES)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXISET)                                     
         DC    AL1(0,0)                                                         
         DC    X'BE24'                                                          
         DC    AL1(1,1,KEYOID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MDOMRESX EQU   *                                                                
*                                                                               
MDOMLST  DC    AL1(MDOMLSTX-MDOMLST)                                            
         DC    AL1(RECDOM,ACTLST)                                               
         DC    AL1(MIXILST,0)                                                   
         DC    AL1(0,0)                                                         
         DC    X'BF24'                                                          
         DC    AL1(1,1,KEYOID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MDOMLSTX EQU   *                                                                
*                                                                               
MDOMSEL  DC    AL1(MDOMSELX-MDOMSEL)                                            
         DC    AL1(RECDOM,ACTSEL)                                               
         DC    AL1(MIXISEL,0)                                                   
         DC    AL1(0,0)                                                         
         DC    X'BE24'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MDOMSELX EQU   *                                                                
*-----------------------------------------                                      
* TOKEN RECORDS                                                                 
*-----------------------------------------                                      
MTOKADD  DC    AL1(MTOKADDX-MTOKADD)                                            
         DC    AL1(RECTOK,ACTADD)                                               
         DC    AL1(MIXILFM+MIXDREQ,0)                                           
         DC    AL1(0,0)                                                         
         DC    X'B228'                                                          
         DC    AL1(1,1,KEYOID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MTOKADDX EQU   *                                                                
*                                                                               
MTOKDIS  DC    AL1(MTOKDISX-MTOKDIS)                                            
         DC    AL1(RECTOK,ACTDIS)                                               
         DC    AL1(MIXILFM,MIXISET)                                             
         DC    AL1(0,0)                                                         
         DC    X'B228'                                                          
         DC    AL1(1,1,KEYOID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MTOKDISX EQU   *                                                                
*                                                                               
MTOKCHA  DC    AL1(MTOKCHAX-MTOKCHA)                                            
         DC    AL1(RECTOK,ACTCHA)                                               
         DC    AL1(MIXILFM,MIXISET)                                             
         DC    AL1(0,0)                                                         
         DC    X'B228'                                                          
         DC    AL1(1,1,KEYOID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MTOKCHAX EQU   *                                                                
*                                                                               
MTOKDEL  DC    AL1(MTOKDELX-MTOKDEL)                                            
         DC    AL1(RECTOK,ACTDEL)                                               
         DC    AL1(MIXILFM,MIXISET)                                             
         DC    AL1(0,0)                                                         
         DC    X'B228'                                                          
         DC    AL1(1,1,KEYOID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MTOKDELX EQU   *                                                                
*                                                                               
MTOKRES  DC    AL1(MTOKRESX-MTOKRES)                                            
         DC    AL1(RECTOK,ACTRES)                                               
         DC    AL1(MIXILFM,MIXISET)                                             
         DC    AL1(0,0)                                                         
         DC    X'B228'                                                          
         DC    AL1(1,1,KEYOID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MTOKRESX EQU   *                                                                
*                                                                               
MTOKLST  DC    AL1(MTOKLSTX-MTOKLST)                                            
         DC    AL1(RECTOK,ACTLST)                                               
         DC    AL1(MIXILST,0)                                                   
         DC    AL1(0,0)                                                         
         DC    X'B328'                                                          
         DC    AL1(1,1,KEYOID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MTOKLSTX EQU   *                                                                
*                                                                               
MTOKSEL  DC    AL1(MTOKSELX-MTOKSEL)                                            
         DC    AL1(RECTOK,ACTSEL)                                               
         DC    AL1(MIXISEL,0)                                                   
         DC    AL1(0,0)                                                         
         DC    X'B228'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MTOKSELX EQU   *                                                                
*                                                                               
*-----------------------------------------                                      
* COMPANY (USER) ID RECORDS                                                     
*-----------------------------------------                                      
MCIDDIS  DC    AL1(MCIDDISX-MCIDDIS)                                            
         DC    AL1(RECCID,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT+MIXISET)                             
         DC    AL1(0,0)                                                         
         DC    X'C925'                                                          
         DC    AL1(2,2,KEYUID,KEYSYS,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0402)                                                        
MCIDDISX EQU   *                                                                
*                                                                               
MCIDLST  DC    AL1(MCIDLSTX-MCIDLST)                                            
         DC    AL1(RECCID,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'C825'                                                          
         DC    AL1(1,1,KEYUID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0406)                                                        
MCIDLSTX EQU   *                                                                
*                                                                               
MCIDSEL  DC    AL1(MCIDSELX-MCIDSEL)                                            
         DC    AL1(RECCID,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'C925'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MCIDSELX EQU   *                                                                
*-----------------------------------------                                      
* BUY AGENCY RECORDS                                                            
*-----------------------------------------                                      
MBAGADD  DC    AL1(MBAGADDX-MBAGADD)                                            
         DC    AL1(RECBAG,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXDREQ,0)                                   
         DC    AL1(0,0)                                                         
         DC    X'9D30'                                                          
         DC    AL1(1,1,KEYOID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MBAGADDX EQU   *                                                                
*                                                                               
MBAGDIS  DC    AL1(MBAGDISX-MBAGDIS)                                            
         DC    AL1(RECBAG,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXISET)                                     
         DC    AL1(0,0)                                                         
         DC    X'9D30'                                                          
         DC    AL1(1,1,KEYOID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MBAGDISX EQU   *                                                                
*                                                                               
MBAGCHA  DC    AL1(MBAGCHAX-MBAGCHA)                                            
         DC    AL1(RECBAG,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    AL1(0,0)                                                         
         DC    X'9D30'                                                          
         DC    AL1(1,1,KEYOID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MBAGCHAX EQU   *                                                                
*                                                                               
MBAGDEL  DC    AL1(MBAGDELX-MBAGDEL)                                            
         DC    AL1(RECBAG,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    AL1(0,0)                                                         
         DC    X'9D30'                                                          
         DC    AL1(1,1,KEYOID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MBAGDELX EQU   *                                                                
*                                                                               
MBAGRES  DC    AL1(MBAGRESX-MBAGRES)                                            
         DC    AL1(RECBAG,ACTRES)                                               
         DC    AL1(MIXILFM+MIXKREQ,0)                                           
         DC    AL1(0,0)                                                         
         DC    X'9D30'                                                          
         DC    AL1(1,1,KEYOID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MBAGRESX EQU   *                                                                
*                                                                               
MBAGLST  DC    AL1(MBAGLSTX-MBAGLST)                                            
         DC    AL1(RECBAG,ACTLST)                                               
         DC    AL1(MIXILST,0)                                                   
         DC    AL1(0,0)                                                         
         DC    X'9F30'                                                          
         DC    AL1(1,1,KEYOID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MBAGLSTX EQU   *                                                                
*                                                                               
MBAGSEL  DC    AL1(MBAGSELX-MBAGSEL)                                            
         DC    AL1(RECBAG,ACTSEL)                                               
         DC    AL1(MIXISEL,0)                                                   
         DC    AL1(0,0)                                                         
         DC    X'9D30'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MBAGSELX EQU   *                                                                
*                                                                               
MIXTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
                                                                                
***********************************************************************         
* KEY COMPONENT TABLE (SEE KEYTABD)                                   *         
***********************************************************************         
KEYTAB   DS    0X                                                               
*                                                                               
         DC    C'SYSTEM ',AL1(KEYSYS)                                           
         DC    C'PROGRAM',AL1(KEYPGM)                                           
         DC    C'RECORD ',AL1(KEYREC)                                           
         DC    C'ACTION ',AL1(KEYACT)                                           
         DC    C'FIELD  ',AL1(KEYFLD)                                           
         DC    C'OPTIONS',AL1(KEYOPT)                                           
         DC    C'USERID ',AL1(KEYUID)                                           
         DC    C'PERSON ',AL1(KEYPID)                                           
         DC    C'DEPT   ',AL1(KEYDID)                                           
         DC    C'OFFICE ',AL1(KEYOID)                                           
         DC    C'PASSWD ',AL1(KEYPWD)                                           
         DC    C'GROUP  ',AL1(KEYAGR)                                           
         DC    C'LACCESS',AL1(KEYLAG)                                           
         DC    C'EFFDATE',AL1(KEYDEF)                                           
         DC    C'LIST ID',AL1(KEYLIS)                                           
         DC    C'APPROVE',AL1(KEYTSA)                                           
*                                                                               
KEYTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* OPTION TABLE (SEE OPTTABD)                                          *         
***********************************************************************         
         SPACE 1                                                                
OPTTAB   DS    0X                  ** OPTIONS TABLE **                          
*DISABLE AGY=/UID= OPTIONS, YYUN, 11/5/04                                       
*&&DO                                                                           
*                                  AGENCY FILTER                                
AGYOPT   DC    AL1(AGYOPTX-AGYOPT)                                              
         DCDD  CT#AGY,8                                                         
         DC    C'AGY',AL1(OPTIDDS+OPTNRTN+OPTITXT,0)                            
         DC    AL1(0,0)                                                         
         DC    AL1(0,0)                                                         
         DC    AL1(3,2,2,L'OPTAGY)                                              
         DC    AL4(OPTAGYB)                                                     
         DC    AL1(OPTAGYN)                                                     
         DC    AL2(OPTAGYR)                                                     
         DC    AL2(OPTAGY-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(3220,3220)                                                   
AGYOPTX  EQU   *                                                                
*                                  USER ID FILTER                               
*                                    RETURNS AGENCY ALPHA ID                    
UIDOPT   DC    AL1(UIDOPTX-UIDOPT)                                              
         DCDD  CT#USRID,8                                                       
         DC    C'UID',AL1(OPTIDDS+OPTNRTN+OPTITXT,0)                            
         DC    AL1(0,0)                                                         
         DC    AL1(0,0)                                                         
         DC    AL1(3,3,L'CTIKID,L'OPTAGY+L'OPTUSE)                              
         DC    AL4(OPTUIDB)                                                     
         DC    AL1(OPTUIDN)                                                     
         DC    AL2(OPTUIDR)                                                     
         DC    AL2(OPTAGY-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(3221,3221)                                                   
UIDOPTX  EQU   *                                                                
*&&                                                                             
*                                  PW= OPTION, ON SYSTEM/REP ACTION             
PWSOPT   DC    AL1(PWSOPTX-PWSOPT)                                              
         DCDD  CT#PSWD,8                                                        
         DC    C'PW ',AL1(OPTNRTN+OPTITXT,OPTNOSEC)                             
         DC    AL1(0,0)                                                         
         DC    AL1(RECSYS,ACTREP)                                               
         DC    AL1(2,1,1,L'OPTPWS)                                              
         DC    AL4(OPTPWSB)                                                     
         DC    AL1(OPTPWSN)                                                     
         DC    AL2(OPTPWSR)                                                     
         DC    AL2(OPTPWS-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(3230,3230)                                                   
PWSOPTX  EQU   *                                                                
*                                  PW= OPTION, ON PERSON RECORD                 
PWPOPT   DC    AL1(PWPOPTX-PWPOPT)                                              
         DCDD  CT#PSWD,8                                                        
         DC    C'PW ',AL1(OPTNRTN+OPTITXT,OPTNOSEC)                             
         DC    AL1(0,0)                                                         
         DC    AL1(RECPER,0)                                                    
         DC    AL1(2,1,1,L'OPTPWS)                                              
         DC    AL4(OPTPWSB)                                                     
         DC    AL1(OPTPWSN)                                                     
         DC    AL2(OPTPWSR)                                                     
         DC    AL2(OPTPWS-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(3230,3230)                                                   
PWPOPTX  EQU   *                                                                
*                                  TERM= OPTION                                 
TEROPT   DC    AL1(TEROPTX-TEROPT)                                              
*&&US*&& DC    C'TERM    '                                                      
*&&UK*&& DC    C'EXPIRED '                                                      
*&&US*&& DC    C'TER',AL1(OPTNRTN+OPTITXT,OPTNOSEC)                             
*&&UK*&& DC    C'EXP',AL1(OPTNRTN+OPTITXT,OPTNOSEC)                             
         DC    AL1(0,0)                                                         
         DC    AL1(0,0)                                                         
*        DC    AL1(RECSYS,ACTREP)                                               
         DC    AL1(3,1,1,L'OPTTER)                                              
         DC    AL4(OPTTERB)                                                     
         DC    AL1(OPTTERN)                                                     
         DC    AL2(OPTTERR)                                                     
         DC    AL2(OPTTER-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(3231,3231)                                                   
TEROPTX  EQU   *                                                                
*                                  PIN= OPTION, ON PERSON RECORD                
PINOPT   DC    AL1(PINOPTX-PINOPT)                                              
         DCDD  CT#PINN,8                                                        
         DC    C'PIN',AL1(OPTIDDS+OPTNRTN+OPTITXT,0)                            
         DC    AL1(0,0)                                                         
         DC    AL1(RECPER,0)                                                    
         DC    AL1(3,1,1,L'OPTPIN)                                              
         DC    AL4(OPTPINB)                                                     
         DC    AL1(OPTPINN)                                                     
         DC    AL2(OPTPINR)                                                     
         DC    AL2(OPTPIN-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(3232,3232)                                                   
PINOPTX  EQU   *                                                                
*                                                                               
*       ***********************************************                         
*        PWD OPTION ON PERSON REC IS NO LONGER ALLOWED                          
*       ***********************************************                         
*WDOPT   DC    AL1(PWDOPTX-PWDOPT)                                              
*        DCDD  CT#PWDO,8                                                        
*        DC    C'   ',AL1(OPTIDDS+OPTNRTN,OPTNOHLP)                             
*        DC    AL1(0,0)                                                         
*        DC    AL1(RECPER,0)                                                    
*        DC    AL1(8,1,1,L'OPTPWD)                                              
*        DC    AL4(OPTPWDB)                                                     
*        DC    AL1(OPTPWDN)                                                     
*        DC    AL2(OPTPWDR)                                                     
*        DC    AL2(OPTPWD-WORKD)                                                
*        DC    XL4'00'                                                          
*        DC    AL4(0)                                                           
*        DC    AL4(0)                                                           
*        DC    AL2(3233,3233)                                                   
*WDOPTX  EQU   *                                                                
*                                  SPECIAL CTL OPTION, ON PERSON RECORD         
CTLOPT   DC    AL1(CTLOPTX-CTLOPT)                                              
         DCDD  CT#CTLO,8                                                        
         DC    C'   ',AL1(OPTIDDS+OPTNRTN,OPTNOHLP)                             
         DC    AL1(0,0)                                                         
         DC    AL1(RECPER,0)                                                    
         DC    AL1(3,1,1,L'OPTCTL)                                              
         DC    AL4(OPTCTLB)                                                     
         DC    AL1(OPTCTLN)                                                     
         DC    AL2(OPTCTLR)                                                     
         DC    AL2(OPTCTL-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(3234,3234)                                                   
CTLOPTX  EQU   *                                                                
*                                  EXPAND OPTION FOR ACCESS GRID                
EXPOPT   DC    AL1(EXPOPTX-EXPOPT)                                              
         DC    CL8'EXPAND'                                                      
         DC    C'EX ',AL1(OPTNRTN,OPTNOSEC+OPTNOHLP)                            
         DC    AL1(0,0)                                                         
         DC    AL1(RECACS,ACTGRD)                                               
         DC    AL1(2,1,1,L'OPTEXP)                                              
         DC    AL4(OPTEXPB)                                                     
         DC    AL1(OPTEXPN)                                                     
         DC    AL2(OPTEXPR)                                                     
         DC    AL2(OPTEXP-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(0,0)                                                         
EXPOPTX  EQU   *                                                                
*&&US                              LAST LOGON DATE DISPLAY OPTION               
LOGOPT   DC    AL1(LOGOPTX-LOGOPT)                                              
         DC    CL8'LOGON'                                                       
         DC    C'LO ',AL1(OPTIDDS+OPTNRTN,OPTNOHLP)                             
         DC    AL1(0,0)                                                         
         DC    AL1(RECPER,0)                                                    
         DC    AL1(2,1,1,L'OPTLOG)                                              
         DC    AL4(OPTLOGB)                                                     
         DC    AL1(OPTLOGN)                                                     
         DC    AL2(OPTLOGR)                                                     
         DC    AL2(OPTLOG-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(0,0)                                                         
LOGOPTX  EQU   *                                                                
*                                  KEY=                                         
INFOPT   DC    AL1(INFOPTX-INFOPT)                                              
         DC    C'KEY     '                                                      
         DC    C'KEY',AL1(OPTIDDS+OPTNRTN,OPTNOHLP)                             
         DC    AL1(0,0)                                                         
         DC    AL1(RECDA2,0)                                                    
         DC    AL1(3,1,1,L'OPTPWD)                                              
         DC    AL4(OPTPWDB)                                                     
         DC    AL1(OPTPWDN)                                                     
         DC    AL2(OPTPWDR)                                                     
         DC    AL2(OPTPWD-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(0,0)                                                         
INFOPTX  EQU   *                                                                
*&&                                                                             
OPTTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
                                                                                
***********************************************************************         
* SELECTABLE ACTION TABLE (SEE SELTABD)                                         
***********************************************************************         
SELTAB   DS    0X                                                               
*                                                                               
         DC    C'S',AL1(RECPGM,ACTLST,RECPGM,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECPGM,ACTLST,RECPGM,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'D',AL1(RECPGM,ACTLST,RECPGM,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'A',AL1(RECPGM,ACTLST,RECACT,ACTLST)                            
         DC    AL1(KEYSYS,KEYPGM,0,0,0,0,0,0)                                   
         DC    AL1(0,0),AL1(0),AL1(0),XL7'00'                                   
         DC    C'R',AL1(RECPGM,ACTLST,RECREC,ACTLST)                            
         DC    AL1(KEYSYS,KEYPGM,0,0,0,0,0,0)                                   
         DC    AL1(0,0),AL1(0),AL1(0),XL7'00'                                   
*                                                                               
         DC    C'S',AL1(RECACT,ACTLST,RECACT,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECACT,ACTLST,RECACT,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'D',AL1(RECACT,ACTLST,RECACT,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*                                                                               
         DC    C'S',AL1(RECREC,ACTLST,RECREC,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECREC,ACTLST,RECREC,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'D',AL1(RECREC,ACTLST,RECREC,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*                                                                               
         DC    C'S',AL1(RECFLD,ACTLST,RECFLD,ACTDIS)                            
         DC    AL1(KEYSYS,KEYPGM,KEYFLD,0,0,0,0,0)                              
         DC    AL1(0,0),AL1(0),AL1(0),XL7'00'                                   
         DC    C'C',AL1(RECFLD,ACTLST,RECFLD,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'D',AL1(RECFLD,ACTLST,RECFLD,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*                                                                               
         DC    C'S',AL1(RECOPT,ACTLST,RECOPT,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECOPT,ACTLST,RECOPT,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'D',AL1(RECOPT,ACTLST,RECOPT,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*                                                                               
         DC    C'S',AL1(RECDPT,ACTLST,RECDPT,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'A',AL1(RECDPT,ACTLST,RECDPT,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    AL1(0,0),AL1(3),AL1(0),XL7'00'                                   
         DC    C'',AL1(RECDPT,ACTLST,RECDPT,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    AL1(0,0),AL1(3),AL1(0),XL7'00'                                   
         DC    C'L',AL1(RECDPT,ACTLST,RECDPT,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    AL1(0,0),AL1(3),AL1(0),XL7'00'                                   
         DC    C'C',AL1(RECDPT,ACTLST,RECDPT,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'D',AL1(RECDPT,ACTLST,RECDPT,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'1',AL1(RECDPT,ACTLST,RECPER,ACTLST)                            
         DC    AL1(0,0,0,0,KEYOID,KEYDID,0,0)                                   
         DC    AL1(0,0),AL1(0),AL1(0),XL7'00'                                   
*                                                                               
         DC    C'S',AL1(RECOFF,ACTLST,RECOFF,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'A',AL1(RECOFF,ACTLST,RECOFF,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    AL1(0,0),AL1(3),AL1(0),XL7'00'                                   
         DC    C'',AL1(RECOFF,ACTLST,RECOFF,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    AL1(0,0),AL1(3),AL1(0),XL7'00'                                   
         DC    C'L',AL1(RECOFF,ACTLST,RECOFF,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    AL1(0,0),AL1(3),AL1(0),XL7'00'                                   
         DC    C'C',AL1(RECOFF,ACTLST,RECOFF,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'D',AL1(RECOFF,ACTLST,RECOFF,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'1',AL1(RECOFF,ACTLST,RECPER,ACTLST)                            
         DC    AL1(0,0,0,0,KEYOID,0,0,0)                                        
         DC    AL1(0,0),AL1(0),AL1(0),XL7'00'                                   
         DC    C'2',AL1(RECOFF,ACTLST,RECDPT,ACTLST)                            
         DC    AL1(KEYOID,0,0,0,0,0,0,0)                                        
         DC    AL1(0,0),AL1(0),AL1(0),XL7'00'                                   
*                                                                               
         DC    C'S',AL1(RECPER,ACTLST,RECPER,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'A',AL1(RECPER,ACTLST,RECPER,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    AL1(0,0),AL1(3),AL1(0),XL7'00'                                   
         DC    C'',AL1(RECPER,ACTLST,RECPER,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    AL1(0,0),AL1(3),AL1(0),XL7'00'                                   
         DC    C'L',AL1(RECPER,ACTLST,RECPER,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    AL1(0,0),AL1(3),AL1(0),XL7'00'                                   
         DC    C'C',AL1(RECPER,ACTLST,RECPER,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'D',AL1(RECPER,ACTLST,RECPER,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'1',AL1(RECPER,ACTLST,RECSYS,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*                                                                               
         DC    C'S',AL1(RECACS,ACTLST,RECACS,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'A',AL1(RECACS,ACTLST,RECACS,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    AL1(0,0),AL1(3),AL1(0),XL7'00'                                   
         DC    C'',AL1(RECACS,ACTLST,RECACS,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    AL1(0,0),AL1(3),AL1(0),XL7'00'                                   
         DC    C'L',AL1(RECACS,ACTLST,RECACS,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    AL1(0,0),AL1(3),AL1(0),XL7'00'                                   
         DC    C'C',AL1(RECACS,ACTLST,RECACS,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'D',AL1(RECACS,ACTLST,RECACS,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*                                                                               
         DC    C'S',AL1(RECSYS,ACTLST,RECSYS,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'A',AL1(RECSYS,ACTLST,RECSYS,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    AL1(0,0),AL1(3),AL1(0),XL7'00'                                   
         DC    C'',AL1(RECSYS,ACTLST,RECSYS,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    AL1(0,0),AL1(3),AL1(0),XL7'00'                                   
         DC    C'L',AL1(RECSYS,ACTLST,RECSYS,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    AL1(0,0),AL1(3),AL1(0),XL7'00'                                   
         DC    C'C',AL1(RECSYS,ACTLST,RECSYS,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'1',AL1(RECSYS,ACTLST,RECPER,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*                                                                               
         DC    C'S',AL1(RECAGR,ACTLST,RECAGR,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'A',AL1(RECAGR,ACTLST,RECAGR,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    AL1(0,0),AL1(3),AL1(0),XL7'00'                                   
         DC    C'',AL1(RECAGR,ACTLST,RECAGR,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    AL1(0,0),AL1(3),AL1(0),XL7'00'                                   
         DC    C'L',AL1(RECAGR,ACTLST,RECAGR,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    AL1(0,0),AL1(3),AL1(0),XL7'00'                                   
         DC    C'C',AL1(RECAGR,ACTLST,RECAGR,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'D',AL1(RECAGR,ACTLST,RECAGR,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    AL1(0,0),AL1(0),AL1(SELIPROC),XL7'00'  PASS APMPROC MODE         
         DC    C'1',AL1(RECAGR,ACTLST,RECPER,ACTLST)                            
         DC    AL1(0,0,0,KEYAGR,0,0,0,0)                                        
         DC    AL1(0,0),AL1(0),AL1(0),XL7'00'                                   
         DC    C'2',AL1(RECAGR,ACTLST,RECACS,ACTLST)                            
         DC    AL1(0,0,0,KEYAGR,0,0,0,0)                                        
         DC    AL1(0,0),AL1(0),AL1(0),XL7'00'                                   
*                                                                               
         DC    C'1',AL1(RECPWD,ACTLST,RECPER,ACTLST)                            
         DC    AL1(KEYPID,0,0,0,0,0,0,0)                                        
         DC    AL1(0,0),AL1(0),AL1(0),XL7'00'                                   
         DC    C'2',AL1(RECPWD,ACTLST,RECSYS,ACTLST)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&US                                                                           
         DC    C'S',AL1(RECLAG,ACTLST,RECLAG,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECLAG,ACTLST,RECLAG,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'D',AL1(RECLAG,ACTLST,RECLAG,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&                                                                             
         DC    C'S',AL1(RECFCT,ACTLST,RECFCT,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'A',AL1(RECFCT,ACTLST,RECFCT,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    AL1(0,0),AL1(3),AL1(0),XL7'00'                                   
         DC    C'',AL1(RECFCT,ACTLST,RECFCT,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    AL1(0,0),AL1(3),AL1(0),XL7'00'                                   
         DC    C'L',AL1(RECFCT,ACTLST,RECFCT,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    AL1(0,0),AL1(3),AL1(0),XL7'00'                                   
         DC    C'C',AL1(RECFCT,ACTLST,RECFCT,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'D',AL1(RECFCT,ACTLST,RECFCT,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*                                                                               
         DC    C'S',AL1(RECOCT,ACTLST,RECOCT,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'A',AL1(RECOCT,ACTLST,RECOCT,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    AL1(0,0),AL1(3),AL1(0),XL7'00'                                   
         DC    C'',AL1(RECOCT,ACTLST,RECOCT,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    AL1(0,0),AL1(3),AL1(0),XL7'00'                                   
         DC    C'L',AL1(RECOCT,ACTLST,RECOCT,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    AL1(0,0),AL1(3),AL1(0),XL7'00'                                   
         DC    C'C',AL1(RECOCT,ACTLST,RECOCT,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'D',AL1(RECOCT,ACTLST,RECOCT,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*                                                                               
         DC    C'S',AL1(RECLIM,ACTLST,RECLIM,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECLIM,ACTLST,RECLIM,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'D',AL1(RECLIM,ACTLST,RECLIM,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*                                                                               
         DC    C'S',AL1(RECDDS,ACTLST,RECDDS,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECDDS,ACTLST,RECDDS,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*                                                                               
         DC    C'S',AL1(RECTSA,ACTLST,RECTSA,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECTSA,ACTLST,RECTSA,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'D',AL1(RECTSA,ACTLST,RECTSA,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    AL1(0,0),AL1(0),AL1(SELIPROC),XL7'00'  PASS APMPROC MODE         
         DC    C'1',AL1(RECTSA,ACTLST,RECPER,ACTLST)                            
         DC    AL1(0,0,KEYTSA,0,0,0,0,0)                                        
         DC    AL1(0,0),AL1(0),AL1(0),XL7'00'                                   
*                                                                               
         DC    C'S',AL1(RECLAL,ACTLST,RECLAL,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECLAL,ACTLST,RECLAL,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'D',AL1(RECLAL,ACTLST,RECLAL,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&US                                                                           
         DC    C'S',AL1(RECMOF,ACTLST,RECMOF,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECMOF,ACTLST,RECMOF,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*                                                                               
         DC    C'S',AL1(RECMOL,ACTLST,RECMOL,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECMOL,ACTLST,RECMOL,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*                                                                               
         DC    C'S',AL1(RECMCY,ACTLST,RECMCY,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECMCY,ACTLST,RECMCY,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*                                                                               
         DC    C'S',AL1(RECDOM,ACTLST,RECDOM,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECDOM,ACTLST,RECDOM,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*                                                                               
         DC    C'S',AL1(RECDAC,ACTLST,RECDAC,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECDAC,ACTLST,RECDAC,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'2',AL1(RECDAC,ACTLST,RECDA2,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'L',AL1(RECDAC,ACTLST,RECDA2,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'S',AL1(RECDA2,ACTLST,RECDA2,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*                                                                               
         DC    C'S',AL1(RECTOK,ACTLST,RECTOK,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECTOK,ACTLST,RECTOK,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*                                                                               
         DC    C'S',AL1(RECCID,ACTLST,RECCID,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*                                                                               
         DC    C'S',AL1(RECBAG,ACTLST,RECBAG,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECBAG,ACTLST,RECBAG,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*                                                                               
SELTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
                                                                                
***********************************************************************         
* PF KEY TABLE (SEE PFKTABD)                                                    
***********************************************************************         
PFKTAB   DS    0X                                                               
*                                                                               
         DC    X'A0',AL1(RECMOL,ANYACT,ANYTYPE)                                 
         DC    AL1(PFK07,RECMOL,ANYACT,PFKILFM,0)                               
         DCDD  CT#UP,9                                                          
         DC    X'A0',AL1(RECMOL,ANYACT,ANYTYPE)                                 
         DC    AL1(PFK08,RECMOL,ANYACT,PFKILFM,0)                               
         DCDD  CT#DOWN,9                                                        
         DC    X'A0',AL1(RECMOL,ACTCHA,ANYTYPE)                                 
         DC    AL1(PFK09,RECMOL,ACTCHA,PFKILFM,0)                               
         DCDD  CT#SORT,4                                                        
*&&US                                                                           
         DC    X'A6',AL1(RECDA2,ACTCHA,ANYTYPE)                                 
         DC    AL1(PFK02,RECDAC,ACTCHA,PFKILFM,0)                               
         DCDD  CT#DATAR,9                                                       
         DC    X'A6',AL1(RECDA2,ACTDIS,ANYTYPE)                                 
         DC    AL1(PFK02,RECDAC,ACTDIS,PFKILFM,0)                               
         DCDD  CT#DATAR,9                                                       
         DC    X'A6',AL1(RECDA2,ANYACT,ANYTYPE)                                 
         DC    AL1(PFK07,RECDA2,ANYACT,PFKILFM,0)                               
         DCDD  CT#UP,9                                                          
         DC    X'A6',AL1(RECDA2,ANYACT,ANYTYPE)                                 
         DC    AL1(PFK08,RECDA2,ANYACT,PFKILFM,0)                               
         DCDD  CT#DOWN,9                                                        
*                                                                               
         DC    X'A7',AL1(RECDAC,ACTDIS,ANYTYPE)                                 
         DC    AL1(PFK02,RECDA2,ACTDIS,PFKILFM,0)                               
         DCDD  CT#DATA2,9                                                       
         DC    X'A7',AL1(RECDAC,ACTCHA,ANYTYPE)                                 
         DC    AL1(PFK02,RECDA2,ACTCHA,PFKILFM,0)                               
         DCDD  CT#DATA2,9                                                       
         DC    X'A7',AL1(RECDAC,ACTADD,ANYTYPE)                                 
         DC    AL1(PFK02,RECDA2,ACTCHA,PFKILFM,0)                               
         DCDD  CT#DATA2,9                                                       
*                                                                               
         DC    X'BE',AL1(RECDOM,ANYACT,ANYTYPE)                                 
         DC    AL1(PFK07,RECDOM,ANYACT,PFKILFM,0)                               
         DCDD  CT#UP,9                                                          
         DC    X'BE',AL1(RECDOM,ANYACT,ANYTYPE)                                 
         DC    AL1(PFK08,RECDOM,ANYACT,PFKILFM,0)                               
         DCDD  CT#DOWN,9                                                        
         DC    X'BE',AL1(RECDOM,ACTCHA,ANYTYPE)                                 
         DC    AL1(PFK09,RECDOM,ACTCHA,PFKILFM,0)                               
         DCDD  CT#SORT,4                                                        
*&&                                                                             
PFKTABX  DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* RECORD SUB-TYPE TABLE (SEE TYPTABD)                                           
***********************************************************************         
TYPTAB   DS    0X                                                               
TYPTABX  DC    AL1(EOT)                                                         
                                                                                
* SEACSWRK                                                                      
       ++INCLUDE SEACSWRK                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'116SEACS01   12/06/19'                                      
         END                                                                    

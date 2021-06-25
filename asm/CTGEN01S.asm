*          DATA SET CTGEN01S   AT LEVEL 073 AS OF 02/27/96                      
*PHASE TA0B01A,*                                                                
*                                                                               
*                                                                               
         TITLE 'CTGEN01 -  FILE MAINTENANCE - CONTROLLER TABLES'                
GEN01    CSECT                                                                  
         SPACE 2                                                                
ARECTAB  DC    AL4(RECTAB-GEN01)   RECORD TABLE                                 
AACTTAB  DC    AL4(ACTTAB-GEN01)   ACTION TABLE                                 
AMIXTAB  DC    AL4(MIXTAB-GEN01)   REC/ACT MIX TABLE                            
AKEYTAB  DC    AL4(KEYTAB-GEN01)   KEY COMPONENT TABLE                          
AOPTTAB  DC    AL4(OPTTAB-GEN01)   OPTION TABLE                                 
ASELTAB  DC    AL4(SELTAB-GEN01)   SELECTABLE ACTION TABLE                      
         EJECT                                                                  
***********************************************************************         
* RECORD TYPE TABLE (SEE RECTABD)                                     *         
***********************************************************************         
         SPACE 1                                                                
RECTAB   DS    0X                                                               
*                                                                               
ROUT     DC    AL1(ROUTX-ROUT)                                                  
         DC    C'OUTPUT  ',C'   ',AL1(RECITXT+RECIDDS)                          
         DC    AL1(0,0,RECOUT)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(CTFILEQ),XL7'00'                                             
         DC    AL2(0100)                                                        
ROUTX    EQU   *                                                                
*                                                                               
RMSG     DC    AL1(RMSGX-RMSG)                                                  
         DC    C'MESSAGE ',C'MSG',AL1(RECITXT+RECIDDS)                          
         DC    AL1(0,0,RECMSG)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(GENFILQ),XL7'00'                                             
         DC    AL2(0200)                                                        
RMSGX    EQU   *                                                                
*                                                                               
RUID     DC    AL1(RUIDX-RUID)                                                  
         DC    C'ID      ',C'   ',AL1(RECITXT+RECIDDS+RECISET)                  
         DC    AL1(0,0,RECUID)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(CTFILEQ),XL7'00'                                             
         DC    AL2(0400)                                                        
RUIDX    EQU   *                                                                
*                                                                               
RIDI     DC    AL1(RIDIX-RIDI)                                                  
         DC    C'IDINFO  ',C'   ',AL1(RECITXT+RECIDDS+RECISET)                  
         DC    AL1(0,0,RECIDI)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(CTFILEQ),XL7'00'                                             
         DC    AL2(0500)                                                        
RIDIX    EQU   *                                                                
*                                                                               
RIDA     DC    AL1(RIDAX-RIDA)                                                  
         DC    C'IDATTN  ',C'   ',AL1(RECITXT+RECIDDS+RECISET)                  
         DC    AL1(0,0,RECIDA)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(CTFILEQ),XL7'00'                                             
         DC    AL2(0600)                                                        
RIDAX    EQU   *                                                                
*                                                                               
RPRT     DC    AL1(RPRTX-RPRT)                                                  
         DC    C'PRINTERQ',C'PRT',AL1(RECITXT+RECIDDS)                          
         DC    AL1(0,0,RECPRT)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(CTFILEQ),XL7'00'                                             
         DC    AL2(700)                                                         
RPRTX    EQU   *                                                                
*                                                                               
RTRM     DC    AL1(RTRMX-RTRM)                                                  
         DC    C'TERMINAL',C'TRM',AL1(RECITXT+RECIDDS)                          
         DC    AL1(0,0,RECTRM)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(CTFILEQ),XL7'00'                                             
         DC    AL2(1100)                                                        
RTRMX    EQU   *                                                                
*                                                                               
RNAR     DC    AL1(RNARX-RNAR)                                                  
         DC    C'NARRATIV',C'NAR',AL1(RECITXT)                                  
         DC    AL2(AUT1NAR)                                                     
         DC    AL1(RECNAR)                                                      
         DC    AL4(0,0)                                                         
         DC    AL1(GENFILQ),XL7'00'                                             
         DC    AL2(1200)                                                        
RNARX    EQU   *                                                                
*                                                                               
RCUR     DC    AL1(RCURX-RCUR)                                                  
         DC    C'CURRENCY',C'   ',AL1(RECITXT)                                  
         DC    AL2(AUT1CUR)                                                     
         DC    AL1(RECCUR)                                                      
         DC    AL4(0,0)                                                         
         DC    AL1(GENFILQ),XL7'00'                                             
         DC    AL2(1300)                                                        
RCURX    EQU   *                                                                
*                                                                               
REXC     DC    AL1(REXCX-REXC)                                                  
         DC    C'EXCHANGE',C'   ',AL1(RECITXT)                                  
         DC    AL2(AUT1CUR)                                                     
         DC    AL1(RECEXC)                                                      
         DC    AL4(0,0)                                                         
         DC    AL1(GENFILQ),XL7'00'                                             
         DC    AL2(1400)                                                        
REXCX    EQU   *                                                                
*                                                                               
RAUT     DC    AL1(RAUTX-RAUT)                                                  
         DC    C'AUTHORIZ',C'   ',AL1(RECITXT+RECIDDS)                          
         DC    AL1(0,0,RECAUT)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(CTFILEQ),XL7'00'                                             
         DC    AL2(1500)                                                        
RAUTX    EQU   *                                                                
*                                                                               
RFEE     DC    AL1(RFEEX-RFEE)                                                  
         DC    C'NICPARMS',C'   ',AL1(RECITXT)                                  
         DC    AL1(0,0,RECFEE)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(GENFILQ),XL7'00'                                             
         DC    AL2(1600)                                                        
RFEEX    EQU   *                                                                
*                                                                               
RCHN     DC    AL1(RCHNX-RCHN)                                                  
         DC    C'CHANNEL ',C'   ',AL1(RECITXT)                                  
         DC    AL1(0,0,RECCHN)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(GENFILQ),XL7'00'                                             
         DC    AL2(1700)                                                        
RCHNX    EQU   *                                                                
*                                                                               
RPAY     DC    AL1(RPAYX-RPAY)                                                  
         DC    C'PERCENT ',C'   ',AL1(RECITXT)                                  
         DC    AL1(0,0,RECPAY)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(GENFILQ),XL7'00'                                             
         DC    AL2(1800)                                                        
RPAYX    EQU   *                                                                
*                                                                               
RROL     DC    AL1(RROLX-RROL)                                                  
         DC    C'ROLES   ',C'   ',AL1(RECITXT)                                  
         DC    AL1(0,0,RECROL)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(GENFILQ),XL7'00'                                             
         DC    AL2(1900)                                                        
RROLX    EQU   *                                                                
*                                                                               
ROLDN    DC    AL1(ROLDNX-ROLDN)                                                
         DC    C'OLDNIC  ',C'   ',AL1(RECITXT)                                  
         DC    AL1(0,0,RECONIC)                                                 
         DC    AL4(0,0)                                                         
         DC    AL1(GENFILQ),XL7'00'                                             
         DC    AL2(2200)                                                        
ROLDNX   EQU   *                                                                
*                                                                               
RTVR     DC    AL1(RTVRX-RTVR)                                                  
         DC    C'TVRBANDS',C'   ',AL1(RECITXT)                                  
         DC    AL1(0,0,RECTVR)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(GENFILQ),XL7'00'                                             
         DC    AL2(2300)                                                        
RTVRX    EQU   *                                                                
*                                                                               
RDAT     DC    AL1(RDATX-RDAT)                                                  
         DC    C'DSCHEME ',C'   ',AL1(RECITXT+RECISET2)                         
         DC    AL2(AUT1SCH)                                                     
         DC    AL1(RECDAT)                                                      
         DC    AL4(0,0)                                                         
         DC    AL1(CTFILEQ),XL7'00'                                             
         DC    AL2(2000)                                                        
RDATX    EQU   *                                                                
*                                                                               
RACC     DC    AL1(RACCX-RACC)                                                  
         DC    C'ACCESS  ',C'   ',AL1(RECITXT+RECIDDS)                          
         DC    AL1(0,0,RECACC)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(CTFILEQ),XL7'00'                                             
         DC    AL2(2100)                                                        
RACCX    EQU   *                                                                
*                                                                               
RFIELD   DC    AL1(RACCX-RACC)                                                  
         DC    C'FIELD   ',C'FLD',AL1(RECITXT+RECIDDS)                          
         DC    AL1(0,0,RECFLD)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(CTFILEQ),XL7'00'                                             
         DC    AL2(2400)                                                        
RFIELDX  EQU   *                                                                
*                                                                               
RPRO     DC    AL1(RPROX-RPRO)                                                  
         DC    C'PROFILE ',C'   ',AL1(RECITXT+RECIDDS+RECISET)                  
         DC    AL1(0,0,RECPRO)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(CTFILEQ),XL7'00'                                             
         DC    AL2(2500)                                                        
RPROX    EQU   *                                                                
*                                                                               
RSYL     DC    AL1(RSYLX-RSYL)                                                  
         DC    C'SYSLIST ',C'   ',AL1(RECITXT)                                  
         DC    AL1(0,0,RECSYL)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(CTFILEQ),XL7'00'                                             
         DC    AL2(2600)                                                        
RSYLX    EQU   *                                                                
*                                                                               
RSEC     DC    AL1(RSECX-RSEC)                                                  
         DC    C'SECURITY',C'   ',AL1(RECITXT+RECIDDS)                          
         DC    AL1(0,0,RECSEC)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(CTFILEQ),XL7'00'                                             
         DC    AL2(2700)                                                        
RSECX    EQU   *                                                                
*                                                                               
RDCT     DC    AL1(RDCTX-RDCT)                                                  
         DC    C'DDICT   ',C'DD ',AL1(RECITXT+RECIDDS)                          
         DC    AL1(0,0,RECDCT)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(GENFILQ),XL7'00'                                             
         DC    AL2(0800)                                                        
RDCTX    EQU   *                                                                
*                                                                               
RSCR     DC    AL1(RSCRX-RSCR)                                                  
         DC    C'SCRIPT  ',C'SCR',AL1(RECITXT+RECIDDS)                          
         DC    AL1(0,0,RECSCR)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(CTFBIGQ),XL7'00'                                             
         DC    AL2(3600)                                                        
RSCRX    EQU   *                                                                
*                                                                               
RBOK     DC    AL1(RBOKX-RBOK)                                                  
         DC    C'BOOK    ',C'BK ',AL1(RECITXT+RECIDDS)                          
         DC    AL1(0,0,RECBOK)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(CTFILEQ),XL7'00'                                             
         DC    AL2(3600)                                                        
RBOKX    EQU   *                                                                
*                                                                               
RFAC     DC    AL1(RFACX-RFAC)                                                  
         DC    C'FAC     ',C'FA ',AL1(RECITXT+RECIDDS)                          
         DC    AL1(0,0,RECFAC)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(CTFILEQ),XL7'00'                                             
         DC    AL2(0801)                                                        
RFACX    EQU   *                                                                
*                                                                               
RPGM     DC    AL1(RPGMX-RPGM)                                                  
         DC    C'PROG    ',C'PGM',AL1(RECITXT+RECIDDS)                          
         DC    AL1(0,0,RECPGM)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(CTFILEQ),XL7'00'                                             
         DC    AL2(0802)                                                        
RPGMX    EQU   *                                                                
*                                                                               
REAS     DC    AL1(REASX-REAS)                                                  
         DC    C'EASI    ',C'EAS',AL1(RECITXT+RECIDDS)                          
         DC    AL1(0,0,RECEAS)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(CTFILEQ),XL7'00'                                             
         DC    AL2(0802)                                                        
REASX    EQU   *                                                                
*                                                                               
RDDS     DC    AL1(RDDSX-RDDS)                                                  
         DC    C'DDSDEPT ',C'DDS',AL1(RECITXT+RECIDDS)                          
         DC    AL1(0,0,RECDDS)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(CTFILEQ),XL7'00'                                             
         DC    AL2(0803)                                                        
RDDSX    EQU   *                                                                
*                                                                               
RPRV     DC    AL1(RPRVX-RPRV)                                                  
         DC    C'PROVER  ',C'PRV',AL1(RECITXT+RECIDDS)                          
         DC    AL1(0,0,RECPRV)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(CTFILEQ),XL7'00'                                             
         DC    AL2(0804)                                                        
RPRVX    EQU   *                                                                
*                                                                               
RECTABX  DC    AL1(EOT)                                                         
*                                                                               
         EJECT                                                                  
***********************************************************************         
* ACTION TABLE (SEE ACTTABD)                                          *         
***********************************************************************         
         SPACE 1                                                                
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
         DCDD  CT#ACCS,8                                                        
         DC    AL1(0,0,0,ACTACS)                                                
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
         DCDD  CT#UPDT,8                                                        
         DC    AL1(0,0,0,ACTUPD)                                                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DCDD  CT#REPL,8                                                        
         DC    AL1(0,0,0,ACTREPL)                                               
         DC    AL4(0,0)                                                         
         DC    AL1(ACTCHA),XL7'00'                                              
*                                                                               
         DCDD  CT#COPY,8                                                        
         DC    AL1(0,0,0,ACTCPY)                                                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
         DCDD  CT#MOVE,8                                                        
         DC    AL1(0,0,0,ACTMOVE)                                               
         DC    AL4(0,0)                                                         
         DC    AL1(ACTCHA),XL7'00'                                              
*                                                                               
ACTTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* RECORD/ACTION COMBO TABLE (SEE MIXTABD)                             *         
***********************************************************************         
         SPACE 1                                                                
MIXTAB   DS    0X                                                               
*                                                                               
MOUTADD  DC    AL1(MOUTADDX-MOUTADD)                                            
         DC    AL1(RECOUT,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXDREQ+MIXIDDS,MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'FD02'                                                          
         DC    AL1(1,1,KEYOUT,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(0101)                                                        
MOUTADDX EQU   *                                                                
*                                                                               
MOUTDIS  DC    AL1(MOUTDISX-MOUTDIS)                                            
         DC    AL1(RECOUT,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'FD02'                                                          
         DC    AL1(1,1,KEYOUT,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0102)                                                        
MOUTDISX EQU   *                                                                
*                                                                               
MOUTCHA  DC    AL1(MOUTCHAX-MOUTCHA)                                            
         DC    AL1(RECOUT,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'FD02'                                                          
         DC    AL1(1,1,KEYOUT,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(0103)                                                        
MOUTCHAX EQU   *                                                                
*                                                                               
MOUTDEL  DC    AL1(MOUTDELX-MOUTDEL)                                            
         DC    AL1(RECOUT,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'FD02'                                                          
         DC    AL1(1,1,KEYOUT,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(0104)                                                        
MOUTDELX EQU   *                                                                
*                                                                               
MOUTRES  DC    AL1(MOUTRESX-MOUTRES)                                            
         DC    AL1(RECOUT,ACTRES)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'FD02'                                                          
         DC    AL1(1,1,KEYOUT,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(0105)                                                        
MOUTRESX EQU   *                                                                
*                                                                               
MOUTLST  DC    AL1(MOUTLSTX-MOUTLST)                                            
         DC    AL1(RECOUT,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'DD02'                                                          
         DC    AL1(1,1,KEYOUT,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0106)                                                        
MOUTLSTX EQU   *                                                                
*                                                                               
MOUTREP  DC    AL1(MOUTREPX-MOUTREP)                                            
         DC    AL1(RECOUT,ACTREP)                                               
         DC    AL1(MIXIREP+MIXIDDS,MIXIOKN+MIXIOKS+MIXIOKO+MIXITXT)             
         DC    AL1(0,0)                                                         
         DC    X'BD02'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    CL8'CTOLOL'                                                      
         DC    AL2(0107)                                                        
MOUTREPX EQU   *                                                                
*                                                                               
MOUTSEL  DC    AL1(MOUTSELX-MOUTSEL)                                            
         DC    AL1(RECOUT,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'FD02'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0108)                                                        
MOUTSELX EQU   *                                                                
*                                                                               
MMSGADD  DC    AL1(MMSGADDX-MMSGADD)                                            
         DC    AL1(RECMSG,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXDREQ+MIXIDDS,MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'FC03'                                                          
         DC    AL1(4,4,KEYSYS,KEYMTYP,KEYMSG,KEYLANG,0,0,0,0)                   
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0201)                                                        
MMSGADDX EQU   *                                                                
*                                                                               
MMSGDIS  DC    AL1(MMSGDISX-MMSGDIS)                                            
         DC    AL1(RECMSG,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'FC03'                                                          
         DC    AL1(4,4,KEYSYS,KEYMTYP,KEYMSG,KEYLANG,0,0,0,0)                   
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0202)                                                        
MMSGDISX EQU   *                                                                
*                                                                               
MMSGCHA  DC    AL1(MMSGCHAX-MMSGCHA)                                            
         DC    AL1(RECMSG,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'FC03'                                                          
         DC    AL1(4,4,KEYSYS,KEYMTYP,KEYMSG,KEYLANG,0,0,0,0)                   
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0203)                                                        
MMSGCHAX EQU   *                                                                
*                                                                               
MMSGDEL  DC    AL1(MMSGDELX-MMSGDEL)                                            
         DC    AL1(RECMSG,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'FC03'                                                          
         DC    AL1(4,4,KEYSYS,KEYMTYP,KEYMSG,KEYLANG,0,0,0,0)                   
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0204)                                                        
MMSGDELX EQU   *                                                                
*                                                                               
MMSGRES  DC    AL1(MMSGRESX-MMSGRES)                                            
         DC    AL1(RECMSG,ACTRES)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'FC03'                                                          
         DC    AL1(4,4,KEYSYS,KEYMTYP,KEYMSG,KEYLANG,0,0,0,0)                   
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0205)                                                        
MMSGRESX EQU   *                                                                
*                                                                               
MMSGLST  DC    AL1(MMSGLSTX-MMSGLST)                                            
         DC    AL1(RECMSG,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'DC03'                                                          
         DC    AL1(4,4,KEYSYS,KEYMTYP,KEYLANG,KEYMSG,0,0,0,0)                   
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0206)                                                        
MMSGLSTX EQU   *                                                                
*                                                                               
MMSGREP  DC    AL1(MMSGREPX-MMSGREP)                                            
         DC    AL1(RECMSG,ACTREP)                                               
         DC    AL1(MIXIREP,MIXIOKN+MIXIOKS+MIXIOKO+MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'BC03'                                                          
         DC    AL1(3,3,KEYSYS,KEYMTYP,KEYMSG,0,0,0,0,0)                         
         DC    AL4(0,0)                                                         
         DC    CL8'CTMLML'                                                      
         DC    AL2(0207)                                                        
MMSGREPX EQU   *                                                                
*                                                                               
MMSGSEL  DC    AL1(MMSGSELX-MMSGSEL)                                            
         DC    AL1(RECMSG,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'FC03'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0208)                                                        
MMSGSELX EQU   *                                                                
*                                                                               
MUIDADD  DC    AL1(MUIDADDX-MUIDADD)                                            
         DC    AL1(RECUID,ACTADD)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ+MIXDREQ,MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'FA05'                                                          
         DC    AL1(2,2,KEYUID,KEYSYS,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(0401)                                                        
MUIDADDX EQU   *                                                                
*                                                                               
MUIDDIS  DC    AL1(MUIDDISX-MUIDDIS)                                            
         DC    AL1(RECUID,ACTDIS)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'FA05'                                                          
         DC    AL1(2,2,KEYUID,KEYSYS,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0402)                                                        
MUIDDISX EQU   *                                                                
*                                                                               
MUIDCHA  DC    AL1(MUIDCHAX-MUIDCHA)                                            
         DC    AL1(RECUID,ACTCHA)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'FA05'                                                          
         DC    AL1(2,2,KEYUID,KEYSYS,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(0403)                                                        
MUIDCHAX EQU   *                                                                
*                                                                               
MUIDDEL  DC    AL1(MUIDDELX-MUIDDEL)                                            
         DC    AL1(RECUID,ACTDEL)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'FA05'                                                          
         DC    AL1(2,2,KEYUID,KEYSYS,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(0404)                                                        
MUIDDELX EQU   *                                                                
*                                                                               
MUIDRES  DC    AL1(MUIDRESX-MUIDRES)                                            
         DC    AL1(RECUID,ACTRES)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'FA05'                                                          
         DC    AL1(2,2,KEYUID,KEYSYS,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(0405)                                                        
MUIDRESX EQU   *                                                                
*                                                                               
MUIDLST  DC    AL1(MUIDLSTX-MUIDLST)                                            
         DC    AL1(RECUID,ACTLST)                                               
         DC    AL1(MIXIDDS+MIXILST,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'DA05'                                                          
         DC    AL1(1,1,KEYUID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0406)                                                        
MUIDLSTX EQU   *                                                                
*                                                                               
MUIDREP  DC    AL1(MUIDREPX-MUIDREP)                                            
         DC    AL1(RECUID,ACTREP)                                               
         DC    AL1(MIXIDDS+MIXIREP,MIXIOKN+MIXIOKS+MIXIOKO+MIXITXT)             
         DC    AL1(0,0)                                                         
         DC    X'BA05'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    CL8'CTOLOL'                                                      
         DC    AL2(0407)                                                        
MUIDREPX EQU   *                                                                
*                                                                               
MUIDSEL  DC    AL1(MUIDSELX-MUIDSEL)                                            
         DC    AL1(RECUID,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'FA05'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MUIDSELX EQU   *                                                                
*                                                                               
MUIDCPY  DC    AL1(MUIDCPYX-MUIDCPY)                                            
         DC    AL1(RECUID,ACTCPY)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'FA05'                                                          
         DC    AL1(2,2,KEYUID,KEYSYS,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(0408)                                                        
MUIDCPYX EQU   *                                                                
*                                                                               
MIDIDIS  DC    AL1(MIDIDISX-MIDIDIS)                                            
         DC    AL1(RECIDI,ACTDIS)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'F906'                                                          
         DC    AL1(1,1,KEYUID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0502)                                                        
MIDIDISX EQU   *                                                                
*                                                                               
MIDICHA  DC    AL1(MIDICHAX-MIDICHA)                                            
         DC    AL1(RECIDI,ACTCHA)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'F906'                                                          
         DC    AL1(1,1,KEYUID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(0503)                                                        
MIDICHAX EQU   *                                                                
*                                                                               
MIDILST  DC    AL1(MIDILSTX-MIDILST)                                            
         DC    AL1(RECIDI,ACTLST)                                               
         DC    AL1(MIXIDDS+MIXILST,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'D906'                                                          
         DC    AL1(1,1,KEYUID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0506)                                                        
MIDILSTX EQU   *                                                                
*                                                                               
MIDIREP  DC    AL1(MIDIREPX-MIDIREP)                                            
         DC    AL1(RECIDI,ACTREP)                                               
         DC    AL1(MIXIDDS+MIXIREP,MIXIOKN+MIXIOKS+MIXIOKO+MIXITXT)             
         DC    AL1(0,0)                                                         
         DC    X'B906'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    CL8'CTOLOL'                                                      
         DC    AL2(0507)                                                        
MIDIREPX EQU   *                                                                
*                                                                               
MIDISEL  DC    AL1(MIDISELX-MIDISEL)                                            
         DC    AL1(RECIDI,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'F906'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MIDISELX EQU   *                                                                
*                                                                               
MIDADIS  DC    AL1(MIDADISX-MIDADIS)                                            
         DC    AL1(RECIDA,ACTDIS)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'F807'                                                          
         DC    AL1(1,1,KEYUID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0602)                                                        
MIDADISX EQU   *                                                                
*                                                                               
MIDACHA  DC    AL1(MIDACHAX-MIDACHA)                                            
         DC    AL1(RECIDA,ACTCHA)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'F807'                                                          
         DC    AL1(1,1,KEYUID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(0603)                                                        
MIDACHAX EQU   *                                                                
*                                                                               
MIDALST  DC    AL1(MIDALSTX-MIDALST)                                            
         DC    AL1(RECIDA,ACTLST)                                               
         DC    AL1(MIXIDDS+MIXILST,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'D807'                                                          
         DC    AL1(1,1,KEYUID,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0606)                                                        
MIDALSTX EQU   *                                                                
*                                                                               
MIDAREP  DC    AL1(MIDAREPX-MIDAREP)                                            
         DC    AL1(RECIDA,ACTREP)                                               
         DC    AL1(MIXIDDS+MIXIREP,MIXIOKN+MIXIOKS+MIXIOKO+MIXITXT)             
         DC    AL1(0,0)                                                         
         DC    X'B807'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    CL8'CTOLOL'                                                      
         DC    AL2(0607)                                                        
MIDAREPX EQU   *                                                                
*                                                                               
MIDASEL  DC    AL1(MIDASELX-MIDASEL)                                            
         DC    AL1(RECIDA,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'F807'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MIDASELX EQU   *                                                                
*                                                                               
MPRTDIS  DC    AL1(MPRTDISX-MPRTDIS)                                            
         DC    AL1(RECPRT,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F708'                                                          
         DC    AL1(1,1,KEYTRM,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(702)                                                         
MPRTDISX EQU   *                                                                
*                                                                               
MPRTCHA  DC    AL1(MPRTCHAX-MPRTCHA)                                            
         DC    AL1(RECPRT,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'F708'                                                          
         DC    AL1(1,1,KEYTRM,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(703)                                                         
MPRTCHAX EQU    *                                                               
*                                                                               
MPRTSEL  DC    AL1(MPRTSELX-MPRTSEL)                                            
         DC    AL1(RECPRT,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'F708'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MPRTSELX EQU   *                                                                
*                                                                               
MTRMADD  DC    AL1(MTRMADDX-MTRMADD)                                            
         DC    AL1(RECTRM,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXDREQ+MIXIDDS,MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'F20D'                                                          
         DC    AL1(3,3,KEYTRM,KEYSYS,KEYPWD,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(1101)                                                        
MTRMADDX EQU   *                                                                
*                                                                               
MTRMDIS  DC    AL1(MTRMDISX-MTRMDIS)                                            
         DC    AL1(RECTRM,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F20D'                                                          
         DC    AL1(3,3,KEYTRM,KEYSYS,KEYPWD,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1102)                                                        
MTRMDISX EQU   *                                                                
*                                                                               
MTRMCHA  DC    AL1(MTRMCHAX-MTRMCHA)                                            
         DC    AL1(RECTRM,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'F20D'                                                          
         DC    AL1(3,3,KEYTRM,KEYSYS,KEYPWD,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(1103)                                                        
MTRMCHAX EQU   *                                                                
*                                                                               
MTRMMOV  DC    AL1(MTRMMOVX-MTRMMOV)                                            
         DC    AL1(RECTRM,ACTMOVE)                                              
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'F20D'                                                          
         DC    AL1(3,3,KEYTRM,KEYSYS,KEYPWD,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(1103)                                                        
MTRMMOVX EQU   *                                                                
*                                                                               
MTRMDEL  DC    AL1(MTRMDELX-MTRMDEL)                                            
         DC    AL1(RECTRM,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'F20D'                                                          
         DC    AL1(3,3,KEYTRM,KEYSYS,KEYPWD,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(1104)                                                        
MTRMDELX EQU   *                                                                
*                                                                               
MTRMRES  DC    AL1(MTRMRESX-MTRMRES)                                            
         DC    AL1(RECTRM,ACTRES)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'F20D'                                                          
         DC    AL1(3,3,KEYTRM,KEYSYS,KEYPWD,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(1105)                                                        
MTRMRESX EQU   *                                                                
*                                                                               
MTRMLST  DC    AL1(MTRMLSTX-MTRMLST)                                            
         DC    AL1(RECTRM,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'D20D'                                                          
         DC    AL1(3,3,KEYTRM,KEYSYS,KEYPWD,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1106)                                                        
MTRMLSTX EQU   *                                                                
*                                                                               
MTRMSEL  DC    AL1(MTRMSELX-MTRMSEL)                                            
         DC    AL1(RECTRM,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'F20D'                                                          
         DC    AL1(3,3,KEYTRM,KEYSYS,KEYPWD,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MTRMSELX EQU   *                                                                
*                                                                               
MTRMREP  DC    AL1(MTRMREPX-MTRMREP)                                            
         DC    AL1(RECTRM,ACTREP)                                               
         DC    AL1(MIXIREP,MIXIOKN+MIXIOKS+MIXIOKO+MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'B20D'                                                          
         DC    AL1(3,3,KEYTRM,KEYSYS,KEYPWD,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    CL8'CTOLOL'                                                      
         DC    AL2(1107)                                                        
MTRMREPX EQU   *                                                                
*                                                                               
MTRMCPY  DC    AL1(MTRMCPYX-MTRMCPY)                                            
         DC    AL1(RECTRM,ACTCPY)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'F20D'                                                          
         DC    AL1(3,3,KEYTRM,KEYSYS,KEYPWD,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(1108)                                                        
MTRMCPYX EQU   *                                                                
*                                                                               
MNARADD  DC    AL1(MNARADDX-MNARADD)                                            
         DC    AL1(RECNAR,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXDREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'F10E'                                                          
         DC    AL1(5,5,KEYUID,KEYSYS,KEYTYP,KEYMED,KEYCLI,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1201)                                                        
MNARADDX EQU   *                                                                
*                                                                               
MNARDIS  DC    AL1(MNARDISX-MNARDIS)                                            
         DC    AL1(RECNAR,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F10E'                                                          
         DC    AL1(5,5,KEYUID,KEYSYS,KEYTYP,KEYMED,KEYCLI,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1202)                                                        
MNARDISX EQU   *                                                                
*                                                                               
MNARCHA  DC    AL1(MNARCHAX-MNARCHA)                                            
         DC    AL1(RECNAR,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F10E'                                                          
         DC    AL1(5,5,KEYUID,KEYSYS,KEYTYP,KEYMED,KEYCLI,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1203)                                                        
MNARCHAX EQU   *                                                                
*                                                                               
MNARDEL  DC    AL1(MNARDELX-MNARDEL)                                            
         DC    AL1(RECNAR,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F10E'                                                          
         DC    AL1(5,5,KEYUID,KEYSYS,KEYTYP,KEYMED,KEYCLI,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1204)                                                        
MNARDELX EQU   *                                                                
*                                                                               
MNARRES  DC    AL1(MNARRESX-MNARRES)                                            
         DC    AL1(RECNAR,ACTRES)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F10E'                                                          
         DC    AL1(5,5,KEYUID,KEYSYS,KEYTYP,KEYMED,KEYCLI,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1205)                                                        
MNARRESX EQU   *                                                                
*                                                                               
MNARLST  DC    AL1(MNARLSTX-MNARLST)                                            
         DC    AL1(RECNAR,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'D10E'                                                          
         DC    AL1(5,5,KEYUID,KEYSYS,KEYTYP,KEYMED,KEYCLI,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1206)                                                        
MNARLSTX EQU   *                                                                
*                                                                               
MNARREP  DC    AL1(MNARREPX-MNARREP)                                            
         DC    AL1(RECNAR,ACTREP)                                               
         DC    AL1(MIXIREP,MIXIOKN+MIXIOKS+MIXIOKO+MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'B10E'                                                          
         DC    AL1(5,5,KEYUID,KEYSYS,KEYTYP,KEYMED,KEYCLI,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    CL8'CTNRNR'                                                      
         DC    AL2(1207)                                                        
MNARREPX EQU   *                                                                
*                                                                               
MNARSEL  DC    AL1(MNARSELX-MNARSEL)                                            
         DC    AL1(RECNAR,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'F10E'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MNARSELX EQU   *                                                                
*                                                                               
*                                                                               
MCURADD  DC    AL1(MCURADDX-MCURADD)                                            
         DC    AL1(RECCUR,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXDREQ+MIXIDDS,MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'EF10'                                                          
         DC    AL1(1,1,KEYCUR,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1301)                                                        
MCURADDX EQU   *                                                                
*                                                                               
MCURDIS  DC    AL1(MCURDISX-MCURDIS)                                            
         DC    AL1(RECCUR,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'EF10'                                                          
         DC    AL1(1,1,KEYCUR,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1302)                                                        
MCURDISX EQU   *                                                                
*                                                                               
MCURCHA  DC    AL1(MCURCHAX-MCURCHA)                                            
         DC    AL1(RECCUR,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'EF10'                                                          
         DC    AL1(1,1,KEYCUR,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1303)                                                        
MCURCHAX EQU   *                                                                
*                                                                               
MCURDEL  DC    AL1(MCURDELX-MCURDEL)                                            
         DC    AL1(RECCUR,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'EF10'                                                          
         DC    AL1(1,1,KEYCUR,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1304)                                                        
MCURDELX EQU   *                                                                
*                                                                               
MCURRES  DC    AL1(MCURRESX-MCURRES)                                            
         DC    AL1(RECCUR,ACTRES)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'EF10'                                                          
         DC    AL1(1,1,KEYCUR,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1305)                                                        
MCURRESX EQU   *                                                                
*                                                                               
MCURLST  DC    AL1(MCURLSTX-MCURLST)                                            
         DC    AL1(RECCUR,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'CF10'                                                          
         DC    AL1(1,1,KEYCUR,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1306)                                                        
MCURLSTX EQU   *                                                                
*                                                                               
MCURREP  DC    AL1(MCURREPX-MCURREP)                                            
         DC    AL1(RECCUR,ACTREP)                                               
         DC    AL1(MIXIREP,MIXIOKN+MIXIOKS+MIXIOKO+MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'AF10'                                                          
         DC    AL1(1,1,KEYCUR,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    CL8'CTCLCL'                                                      
         DC    AL2(1307)                                                        
MCURREPX EQU   *                                                                
*                                                                               
MCURSEL  DC    AL1(MCURSELX-MCURSEL)                                            
         DC    AL1(RECCUR,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'EF10'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1308)                                                        
MCURSELX EQU   *                                                                
*                                                                               
*                                                                               
MEXCADD  DC    AL1(MEXCADDX-MEXCADD)                                            
         DC    AL1(RECEXC,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXDREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'F00F'                                                          
         DC    AL1(6,6,KEYSYS,KEYCRF,KEYTYP,KEYCRT,KEYCLI,KEYPER,0,0)           
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1401)                                                        
MEXCADDX EQU   *                                                                
*                                                                               
MEXCDIS  DC    AL1(MEXCDISX-MEXCDIS)                                            
         DC    AL1(RECEXC,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F00F'                                                          
         DC    AL1(6,6,KEYSYS,KEYCRF,KEYTYP,KEYCRT,KEYCLI,KEYPER,0,0)           
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1402)                                                        
MEXCDISX EQU   *                                                                
*                                                                               
MEXCCHA  DC    AL1(MEXCCHAX-MEXCCHA)                                            
         DC    AL1(RECEXC,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F00F'                                                          
         DC    AL1(6,6,KEYSYS,KEYCRF,KEYTYP,KEYCRT,KEYCLI,KEYPER,0,0)           
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1403)                                                        
MEXCCHAX EQU   *                                                                
*                                                                               
MEXCDEL  DC    AL1(MEXCDELX-MEXCDEL)                                            
         DC    AL1(RECEXC,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F00F'                                                          
         DC    AL1(6,6,KEYSYS,KEYCRF,KEYTYP,KEYCRT,KEYCLI,KEYPER,0,0)           
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1404)                                                        
MEXCDELX EQU   *                                                                
*                                                                               
MEXCRES  DC    AL1(MEXCRESX-MEXCRES)                                            
         DC    AL1(RECEXC,ACTRES)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F00F'                                                          
         DC    AL1(6,6,KEYSYS,KEYCRF,KEYTYP,KEYCRT,KEYCLI,KEYPER,0,0)           
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1405)                                                        
MEXCRESX EQU   *                                                                
*                                                                               
MEXCREP  DC    AL1(MEXCREPX-MEXCREP)                                            
         DC    AL1(RECEXC,ACTREP)                                               
         DC    AL1(MIXIREP,MIXIOKN+MIXIOKS+MIXIOKO+MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'B00F'                                                          
         DC    AL1(6,6,KEYSYS,KEYCRF,KEYTYP,KEYCRT,KEYCLI,KEYPER,0,0)           
         DC    AL4(0,0)                                                         
         DC    CL8'CTELEL'                                                      
         DC    AL2(1407)                                                        
MEXCREPX EQU   *                                                                
*                                                                               
MEXCSEL  DC    AL1(MEXCSELX-MEXCSEL)                                            
         DC    AL1(RECEXC,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'F00F'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1408)                                                        
MEXCSELX EQU   *                                                                
*                                                                               
MEXCUPD  DC    AL1(MEXCUPDX-MEXCUPD)                                            
         DC    AL1(RECEXC,ACTUPD)                                               
         DC    AL1(MIXIDDS+MIXIREP,MIXIOKS+MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'A916'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    CL8'CTFTFT'                                                      
         DC    AL2(1409)                                                        
MEXCUPDX EQU   *                                                                
*                                                                               
MAUTDIS  DC    AL1(MAUTDISX-MAUTDIS)                                            
         DC    AL1(RECAUT,ACTDIS)                                               
         DC    AL1(MIXILFM,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'EE11'                                                          
         DC    AL1(4,4,KEYAGY,KEYTYP,KEYNAM,KEYCODE,0,0,0,0)                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1502)                                                        
MAUTDISX EQU   *                                                                
*                                                                               
MAUTLST  DC    AL1(MAUTLSTX-MAUTLST)                                            
         DC    AL1(RECAUT,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'CE11'                                                          
         DC    AL1(4,4,KEYAGY,KEYTYP,KEYNAM,KEYCODE,0,0,0,0)                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1506)                                                        
MAUTLSTX EQU   *                                                                
*                                                                               
MAUTREP  DC    AL1(MAUTREPX-MAUTREP)                                            
         DC    AL1(RECAUT,ACTREP)                                               
         DC    AL1(MIXIREP,MIXIOKN+MIXIOKS+MIXIOKO+MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'AE11'                                                          
         DC    AL1(3,3,KEYAGY,KEYTYP,KEYNAM,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    CL8'CTOLOL'                                                      
         DC    AL2(1507)                                                        
MAUTREPX EQU   *                                                                
*                                                                               
MAUTSEL  DC    AL1(MAUTSELX-MAUTSEL)                                            
         DC    AL1(RECAUT,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'EE11'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MAUTSELX EQU   *                                                                
*                                                                               
MFEEADD  DC    AL1(MFEEADDX-MFEEADD)                                            
         DC    AL1(RECFEE,ACTADD)                                               
         DC    AL1(MIXILFM+MIXDREQ+MIXIDDS,MIXITXT)                             
         DC    AL2(0)                                                           
         DC    X'ED12'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1601)                                                        
MFEEADDX EQU   *                                                                
*                                                                               
MFEEDIS  DC    AL1(MFEEDISX-MFEEDIS)                                            
         DC    AL1(RECFEE,ACTDIS)                                               
         DC    AL1(MIXILFM,MIXITXT)                                             
         DC    AL2(AUT2FEE)                                                     
         DC    X'ED12'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1602)                                                        
MFEEDISX EQU   *                                                                
*                                                                               
MFEECHA  DC    AL1(MFEECHAX-MFEECHA)                                            
         DC    AL1(RECFEE,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXIDDS,MIXITXT)                                     
         DC    AL2(0)                                                           
         DC    X'ED12'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1603)                                                        
MFEECHAX EQU   *                                                                
*                                                                               
MOLDDIS  DC    AL1(MOLDDISX-MOLDDIS)                                            
         DC    AL1(RECONIC,ACTDIS)                                              
         DC    AL1(MIXILFM,MIXITXT)                                             
         DC    AL2(AUT2FEE)                                                     
         DC    X'ED12'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1605)                                                        
MOLDDISX EQU   *                                                                
*                                                                               
MOLDREP  DC    AL1(MOLDREPX-MOLDREP)                                            
         DC    AL1(RECONIC,ACTREPL)                                             
         DC    AL1(MIXILFM+MIXIDDS,MIXITXT)                                     
         DC    AL2(0)                                                           
         DC    X'ED12'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1605)                                                        
MOLDREPX EQU   *                                                                
*                                                                               
MCHNADD  DC    AL1(MCHNADDX-MCHNADD)                                            
         DC    AL1(RECCHN,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXDREQ+MIXIDDS,MIXITXT)                     
         DC    AL2(0)                                                           
         DC    X'EC13'                                                          
         DC    AL1(2,2,KEYCHAN,KEYCODE,0,0,0,0,0,0)                             
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1701)                                                        
MCHNADDX EQU   *                                                                
*                                                                               
MCHNDIS  DC    AL1(MCHNDISX-MCHNDIS)                                            
         DC    AL1(RECCHN,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL2(AUT2FEE)                                                     
         DC    X'EC13'                                                          
         DC    AL1(2,2,KEYCHAN,KEYCODE,0,0,0,0,0,0)                             
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1702)                                                        
MCHNDISX EQU   *                                                                
*                                                                               
MCHNCHA  DC    AL1(MCHNCHAX-MCHNCHA)                                            
         DC    AL1(RECCHN,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL2(0)                                                           
         DC    X'EC13'                                                          
         DC    AL1(2,2,KEYCHAN,KEYCODE,0,0,0,0,0,0)                             
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1703)                                                        
MCHNCHAX EQU   *                                                                
*                                                                               
MCHNDEL  DC    AL1(MCHNDELX-MCHNDEL)                                            
         DC    AL1(RECCHN,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'EC13'                                                          
         DC    AL1(2,2,KEYCHAN,KEYCODE,0,0,0,0,0,0)                             
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1704)                                                        
MCHNDELX EQU   *                                                                
*                                                                               
MCHNRES  DC    AL1(MCHNRESX-MCHNRES)                                            
         DC    AL1(RECCHN,ACTRES)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'EC13'                                                          
         DC    AL1(2,2,KEYCHAN,KEYCODE,0,0,0,0,0,0)                             
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1705)                                                        
MCHNRESX EQU   *                                                                
*                                                                               
MCHNLST  DC    AL1(MCHNLSTX-MCHNLST)                                            
         DC    AL1(RECCHN,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL2(AUT2FEE)                                                     
         DC    X'CC13'                                                          
         DC    AL1(2,2,KEYCHAN,KEYCODE,0,0,0,0,0,0)                             
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1706)                                                        
MCHNLSTX EQU   *                                                                
*                                                                               
MCHNSEL  DC    AL1(MCHNSELX-MCHNSEL)                                            
         DC    AL1(RECCHN,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'EC13'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1708)                                                        
MCHNSELX EQU   *                                                                
*                                                                               
MPAYADD  DC    AL1(MPAYADDX-MPAYADD)                                            
         DC    AL1(RECPAY,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXDREQ+MIXIDDS,MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'EB14'                                                          
         DC    AL1(2,2,KEYCHAN,KEYCODE,0,0,0,0,0,0)                             
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1801)                                                        
MPAYADDX EQU   *                                                                
*                                                                               
MPAYDIS  DC    AL1(MPAYDISX-MPAYDIS)                                            
         DC    AL1(RECPAY,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL2(AUT2FEE)                                                     
         DC    X'EB14'                                                          
         DC    AL1(2,2,KEYCHAN,KEYCODE,0,0,0,0,0,0)                             
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1802)                                                        
MPAYDISX EQU   *                                                                
*                                                                               
MPAYCHA  DC    AL1(MPAYCHAX-MPAYCHA)                                            
         DC    AL1(RECPAY,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'EB14'                                                          
         DC    AL1(2,2,KEYCHAN,KEYCODE,0,0,0,0,0,0)                             
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1803)                                                        
MPAYCHAX EQU   *                                                                
*                                                                               
MPAYDEL  DC    AL1(MPAYDELX-MPAYDEL)                                            
         DC    AL1(RECPAY,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'EB14'                                                          
         DC    AL1(2,2,KEYCHAN,KEYCODE,0,0,0,0,0,0)                             
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1804)                                                        
MPAYDELX EQU   *                                                                
*                                                                               
MPAYRES  DC    AL1(MPAYRESX-MPAYRES)                                            
         DC    AL1(RECPAY,ACTRES)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'EB14'                                                          
         DC    AL1(2,2,KEYCHAN,KEYCODE,0,0,0,0,0,0)                             
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1805)                                                        
MPAYRESX EQU   *                                                                
*                                                                               
MPAYLST  DC    AL1(MPAYLSTX-MPAYLST)                                            
         DC    AL1(RECPAY,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL2(AUT2FEE)                                                     
         DC    X'CB14'                                                          
         DC    AL1(2,2,KEYCHAN,KEYCODE,0,0,0,0,0,0)                             
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1806)                                                        
MPAYLSTX EQU   *                                                                
*                                                                               
MPAYSEL  DC    AL1(MPAYSELX-MPAYSEL)                                            
         DC    AL1(RECPAY,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'EB14'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1808)                                                        
*                                                                               
MPAYSELX EQU   *                                                                
*                                                                               
MROLADD  DC    AL1(MROLADDX-MROLADD)                                            
         DC    AL1(RECROL,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXDREQ+MIXIDDS,MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'EA15'                                                          
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1901)                                                        
MROLADDX EQU   *                                                                
*                                                                               
MROLDIS  DC    AL1(MROLDISX-MROLDIS)                                            
         DC    AL1(RECROL,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL2(AUT2FEE)                                                     
         DC    X'EA15'                                                          
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1902)                                                        
MROLDISX EQU   *                                                                
*                                                                               
MROLCHA  DC    AL1(MROLCHAX-MROLCHA)                                            
         DC    AL1(RECROL,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'EA15'                                                          
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(1903)                                                        
MROLCHAX EQU   *                                                                
*                                                                               
MDATADD  DC    AL1(MDATADDX-MDATADD)                                            
         DC    AL1(RECDAT,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXDREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'E917'                                                          
         DC    AL1(2,2,KEYAGY,KEYSCHM,0,0,0,0,0,0)                              
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(2001)                                                        
MDATADDX EQU   *                                                                
*                                                                               
MDATDIS  DC    AL1(MDATDISX-MDATDIS)                                            
         DC    AL1(RECDAT,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'E917'                                                          
         DC    AL1(2,2,KEYAGY,KEYSCHM,0,0,0,0,0,0)                              
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(2002)                                                        
MDATDISX EQU   *                                                                
*                                                                               
MDATCHA  DC    AL1(MDATCHAX-MDATCHA)                                            
         DC    AL1(RECDAT,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'E917'                                                          
         DC    AL1(2,2,KEYAGY,KEYSCHM,0,0,0,0,0,0)                              
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(2003)                                                        
MDATCHAX EQU   *                                                                
*                                                                               
MDATDEL  DC    AL1(MDATDELX-MDATDEL)                                            
         DC    AL1(RECDAT,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'E917'                                                          
         DC    AL1(2,2,KEYAGY,KEYSCHM,0,0,0,0,0,0)                              
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(2004)                                                        
MDATDELX EQU   *                                                                
*                                                                               
MDATRES  DC    AL1(MDATRESX-MDATRES)                                            
         DC    AL1(RECDAT,ACTRES)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'E917'                                                          
         DC    AL1(2,2,KEYAGY,KEYSCHM,0,0,0,0,0,0)                              
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(2005)                                                        
MDATRESX EQU   *                                                                
*                                                                               
MDATLST  DC    AL1(MDATLSTX-MDATLST)                                            
         DC    AL1(RECDAT,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'C917'                                                          
         DC    AL1(1,1,KEYAGY,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(2006)                                                        
MDATLSTX EQU   *                                                                
*                                                                               
MDATSEL  DC    AL1(MDATSELX-MDATSEL)                                            
         DC    AL1(RECDAT,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'E917'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(2008)                                                        
MDATSELX EQU   *                                                                
*                                                                               
MACCADD  DC    AL1(MACCADDX-MACCADD)                                            
         DC    AL1(RECACC,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXDREQ+MIXIDDS,MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'E818'                                                          
         DC    AL1(1,1,KEYACC,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(2101)                                                        
MACCADDX EQU   *                                                                
*                                                                               
MACCDIS  DC    AL1(MACCDISX-MACCDIS)                                            
         DC    AL1(RECACC,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'E818'                                                          
         DC    AL1(1,1,KEYACC,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(2102)                                                        
MACCDISX EQU   *                                                                
*                                                                               
MACCCHA  DC    AL1(MACCCHAX-MACCCHA)                                            
         DC    AL1(RECACC,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'E818'                                                          
         DC    AL1(1,1,KEYACC,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(2103)                                                        
MACCCHAX EQU   *                                                                
*                                                                               
MACCDEL  DC    AL1(MACCDELX-MACCDEL)                                            
         DC    AL1(RECACC,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'E818'                                                          
         DC    AL1(1,1,KEYACC,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(2104)                                                        
MACCDELX EQU   *                                                                
*                                                                               
MACCRES  DC    AL1(MACCRESX-MACCRES)                                            
         DC    AL1(RECACC,ACTRES)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'E818'                                                          
         DC    AL1(1,1,KEYACC,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(2105)                                                        
MACCRESX EQU   *                                                                
*                                                                               
MACCLST  DC    AL1(MACCLSTX-MACCLST)                                            
         DC    AL1(RECACC,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'C818'                                                          
         DC    AL1(1,1,KEYACC,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(2106)                                                        
MACCLSTX EQU   *                                                                
*                                                                               
MACCREP  DC    AL1(MACCREPX-MACCREP)                                            
         DC    AL1(RECACC,ACTREP)                                               
         DC    AL1(MIXIREP+MIXIDDS,MIXIOKS+MIXIOKO+MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'A818'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    CL8'CTOLOL'                                                      
         DC    AL2(2107)                                                        
MACCREPX EQU   *                                                                
*                                                                               
MACCSEL  DC    AL1(MACCSELX-MACCSEL)                                            
         DC    AL1(RECACC,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'E818'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(2108)                                                        
MACCSELX EQU   *                                                                
*                                                                               
MTVRADD  DC    AL1(MTVRADDX-MTVRADD)                                            
         DC    AL1(RECTVR,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXDREQ+MIXIDDS,MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'E51A'                                                          
         DC    AL1(2,2,KEYCODE,0,0,0,0,0,0,0)                                   
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(2201) GGGGGGG                                                
MTVRADDX EQU   *                                                                
*                                                                               
MTVRDIS  DC    AL1(MTVRDISX-MTVRDIS)                                            
         DC    AL1(RECTVR,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL2(AUT2FEE)                                                     
         DC    X'E51A'                                                          
         DC    AL1(2,2,KEYCODE,0,0,0,0,0,0,0)                                   
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(2202)                                                        
MTVRDISX EQU   *                                                                
*                                                                               
MTVRCHA  DC    AL1(MTVRCHAX-MTVRCHA)                                            
         DC    AL1(RECTVR,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'E51A'                                                          
         DC    AL1(2,2,KEYCODE,0,0,0,0,0,0,0)                                   
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(2203)                                                        
MTVRCHAX EQU   *                                                                
*                                                                               
MTVRDEL  DC    AL1(MTVRDELX-MTVRDEL)                                            
         DC    AL1(RECTVR,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'E51A'                                                          
         DC    AL1(2,2,KEYCODE,0,0,0,0,0,0,0)                                   
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(2204)                                                        
MTVRDELX EQU   *                                                                
*                                                                               
MTVRRES  DC    AL1(MTVRRESX-MTVRRES)                                            
         DC    AL1(RECTVR,ACTRES)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'E51A'                                                          
         DC    AL1(2,2,KEYCODE,0,0,0,0,0,0,0)                                   
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(2205)                                                        
MTVRRESX EQU   *                                                                
*                                                                               
MTVRLST  DC    AL1(MTVRLSTX-MTVRLST)                                            
         DC    AL1(RECTVR,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL2(AUT2FEE)                                                     
         DC    X'C51A'                                                          
         DC    AL1(2,2,KEYCODE,0,0,0,0,0,0,0)                                   
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(2206)                                                        
MTVRLSTX EQU   *                                                                
*                                                                               
MTVRSEL  DC    AL1(MTVRSELX-MTVRSEL)                                            
         DC    AL1(RECTVR,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'E51A'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(2208)                                                        
MTVRSELX EQU   *                                                                
*                                                                               
MFLDADD  DC    AL1(MFLDADDX-MFLDADD)                                            
         DC    AL1(RECFLD,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXDREQ+MIXIDDS,MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'E719'                                                          
         DC    AL1(3,3,KEYSYS,KEYPGM,KEYLANG,0,0,0,0,0)                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(2401)                                                        
MFLDADDX EQU   *                                                                
*                                                                               
MFLDDIS  DC    AL1(MFLDDISX-MFLDDIS)                                            
         DC    AL1(RECFLD,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'E719'                                                          
         DC    AL1(3,3,KEYSYS,KEYPGM,KEYLANG,0,0,0,0,0)                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(2402)                                                        
MFLDDISX EQU   *                                                                
*                                                                               
MFLDCHA  DC    AL1(MFLDCHAX-MFLDCHA)                                            
         DC    AL1(RECFLD,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'E719'                                                          
         DC    AL1(3,3,KEYSYS,KEYPGM,KEYLANG,0,0,0,0,0)                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(2403)                                                        
MFLDCHAX EQU   *                                                                
*                                                                               
MFLDDEL  DC    AL1(MFLDDELX-MFLDDEL)                                            
         DC    AL1(RECFLD,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'E719'                                                          
         DC    AL1(3,3,KEYSYS,KEYPGM,KEYLANG,0,0,0,0,0)                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(2404)                                                        
MFLDDELX EQU   *                                                                
*                                                                               
MFLDRES  DC    AL1(MFLDRESX-MFLDRES)                                            
         DC    AL1(RECFLD,ACTRES)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'E719'                                                          
         DC    AL1(3,3,KEYSYS,KEYPGM,KEYLANG,0,0,0,0,0)                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(2405)                                                        
MFLDRESX EQU   *                                                                
*                                                                               
MFLDLST  DC    AL1(MFLDLSTX-MFLDLST)                                            
         DC    AL1(RECFLD,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'C719'                                                          
         DC    AL1(3,3,KEYSYS,KEYPGM,KEYLANG,0,0,0,0,0)                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(2406)                                                        
MFLDLSTX EQU   *                                                                
*                                                                               
MFLDSEL  DC    AL1(MFLDSELX-MFLDSEL)                                            
         DC    AL1(RECFLD,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'E719'                                                          
         DC    AL1(3,3,KEYSYS,KEYPGM,KEYLANG,0,0,0,0,0)                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MFLDSELX EQU   *                                                                
*                                                                               
MFLDCPY  DC    AL1(MFLDCPYX-MFLDCPY)                                            
         DC    AL1(RECFLD,ACTCPY)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'E719'                                                          
         DC    AL1(3,3,KEYSYS,KEYPGM,KEYLANG,0,0,0,0,0)                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(2408)                                                        
MFLDCPYX EQU   *                                                                
*                                                                               
MPROADD  DC    AL1(MPROADDX-MPROADD)                                            
         DC    AL1(RECPRO,ACTADD)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ+MIXDREQ,MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'F609'                                                          
         DC    AL1(4,4,KEYSYS,KEYPGM,KEYUID,KEYPTYP,0,0,0,0)                    
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(2501)                                                        
MPROADDX EQU   *                                                                
*                                                                               
MPRODIS  DC    AL1(MPRODISX-MPRODIS)                                            
         DC    AL1(RECPRO,ACTDIS)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'F609'                                                          
         DC    AL1(4,4,KEYSYS,KEYPGM,KEYUID,KEYPTYP,0,0,0,0)                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(2502)                                                        
MPRODISX EQU   *                                                                
*                                                                               
MPROCHA  DC    AL1(MPROCHAX-MPROCHA)                                            
         DC    AL1(RECPRO,ACTCHA)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'F609'                                                          
         DC    AL1(4,4,KEYSYS,KEYPGM,KEYUID,KEYPTYP,0,0,0,0)                    
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(2503)                                                        
MPROCHAX EQU   *                                                                
*                                                                               
MPRODEL  DC    AL1(MPRODELX-MPRODEL)                                            
         DC    AL1(RECPRO,ACTDEL)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'F609'                                                          
         DC    AL1(4,4,KEYSYS,KEYPGM,KEYUID,KEYPTYP,0,0,0,0)                    
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(2504)                                                        
MPRODELX EQU   *                                                                
*                                                                               
MPRORES  DC    AL1(MPRORESX-MPRORES)                                            
         DC    AL1(RECPRO,ACTRES)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'F609'                                                          
         DC    AL1(4,4,KEYSYS,KEYPGM,KEYUID,KEYPTYP,0,0,0,0)                    
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(2505)                                                        
MPRORESX EQU   *                                                                
*                                                                               
MPROLST  DC    AL1(MPROLSTX-MPROLST)                                            
         DC    AL1(RECPRO,ACTLST)                                               
         DC    AL1(MIXIDDS+MIXILST,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'D609'                                                          
         DC    AL1(4,4,KEYSYS,KEYPGM,KEYUID,KEYPTYP,0,0,0,0)                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(2506)                                                        
MPROLSTX EQU   *                                                                
*                                                                               
MPROREP  DC    AL1(MPROREPX-MPROREP)                                            
         DC    AL1(RECPRO,ACTREP)                                               
         DC    AL1(MIXIDDS+MIXIREP,MIXIOKS+MIXIOKO+MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'B609'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    CL8'CTOLOL'                                                      
         DC    AL2(2507)                                                        
MPROREPX EQU   *                                                                
*                                                                               
MPROSEL  DC    AL1(MPROSELX-MPROSEL)                                            
         DC    AL1(RECPRO,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'F609'                                                          
         DC    AL1(4,4,KEYSYS,KEYPGM,KEYUID,KEYPTYP,0,0,0,0)                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MPROSELX EQU   *                                                                
*                                                                               
MPROCPY  DC    AL1(MPROCPYX-MPROCPY)                                            
         DC    AL1(RECPRO,ACTCPY)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'F609'                                                          
         DC    AL1(4,4,KEYSYS,KEYPGM,KEYUID,KEYPTYP,0,0,0,0)                    
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(2508)                                                        
MPROCPYX EQU   *                                                                
*                                                                               
MSYLADD  DC    AL1(MSYLADDX-MSYLADD)                                            
         DC    AL1(RECSYL,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXDREQ+MIXIDDS,MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'E41B'                                                          
         DC    AL1(2,2,KEYLTYP,KEYUID,0,0,0,0,0,0)                              
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(2601)                                                        
MSYLADDX EQU   *                                                                
*                                                                               
MSYLDIS  DC    AL1(MSYLDISX-MSYLDIS)                                            
         DC    AL1(RECSYL,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'E41B'                                                          
         DC    AL1(2,2,KEYLTYP,KEYUID,0,0,0,0,0,0)                              
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(2602)                                                        
MSYLDISX EQU   *                                                                
*                                                                               
MSYLCHA  DC    AL1(MSYLCHAX-MSYLCHA)                                            
         DC    AL1(RECSYL,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'E41B'                                                          
         DC    AL1(2,2,KEYLTYP,KEYUID,0,0,0,0,0,0)                              
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(2603)                                                        
MSYLCHAX EQU   *                                                                
*                                                                               
MSYLDEL  DC    AL1(MSYLDELX-MSYLDEL)                                            
         DC    AL1(RECSYL,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'E41B'                                                          
         DC    AL1(2,2,KEYLTYP,KEYUID,0,0,0,0,0,0)                              
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(2604)                                                        
MSYLDELX EQU   *                                                                
*                                                                               
MSYLRES  DC    AL1(MSYLRESX-MSYLRES)                                            
         DC    AL1(RECSYL,ACTRES)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'E41B'                                                          
         DC    AL1(2,2,KEYLTYP,KEYUID,0,0,0,0,0,0)                              
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(2605)                                                        
MSYLRESX EQU   *                                                                
*                                                                               
MSYLLST  DC    AL1(MSYLLSTX-MSYLLST)                                            
         DC    AL1(RECSYL,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'C41B'                                                          
         DC    AL1(2,2,KEYLTYP,KEYUID,0,0,0,0,0,0)                              
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(2606)                                                        
MSYLLSTX EQU   *                                                                
*                                                                               
MSYLREP  DC    AL1(MSYLREPX-MSYLREP)                                            
         DC    AL1(RECSYL,ACTREP)                                               
         DC    AL1(MIXIREP,MIXIOKS+MIXIOKO+MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'A41B'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    CL8'CTOLOL'                                                      
         DC    AL2(2607)                                                        
MSYLREPX EQU   *                                                                
*                                                                               
MSYLSEL  DC    AL1(MSYLSELX-MSYLSEL)                                            
         DC    AL1(RECSYL,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'E41B'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(2608)                                                        
MSYLSELX EQU   *                                                                
*                                                                               
MSECDIS  DC    AL1(MSECDISX-MSECDIS)                                            
         DC    AL1(RECSEC,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'E31C'                                                          
         DC    AL1(2,2,KEYACC,KEYSYS,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(2702)                                                        
MSECDISX EQU   *                                                                
*                                                                               
MSECCHA  DC    AL1(MSECCHAX-MSECCHA)                                            
         DC    AL1(RECSEC,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'E31C'                                                          
         DC    AL1(2,2,KEYACC,KEYSYS,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(2703)                                                        
MSECCHAX EQU   *                                                                
*                                                                               
MSECSEL  DC    AL1(MSECSELX-MSECSEL)                                            
         DC    AL1(RECSEC,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'E31C'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(2708)                                                        
MSECSELX EQU   *                                                                
*                                                                               
MDCTADD  DC    AL1(MDCTADDX-MDCTADD)                                            
         DC    AL1(RECDCT,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXDREQ+MIXIDDS,MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'E121'                                                          
         DC    AL1(3,3,KEYSYS,KEYMSG,KEYLANG,0,0,0,0,0)                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0801)                                                        
MDCTADDX EQU   *                                                                
*                                                                               
MDCTDIS  DC    AL1(MDCTDISX-MDCTDIS)                                            
         DC    AL1(RECDCT,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'E121'                                                          
         DC    AL1(3,3,KEYSYS,KEYMSG,KEYLANG,0,0,0,0,0)                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0802)                                                        
MDCTDISX EQU   *                                                                
*                                                                               
MDCTCHA  DC    AL1(MDCTCHAX-MDCTCHA)                                            
         DC    AL1(RECDCT,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'E121'                                                          
         DC    AL1(3,3,KEYSYS,KEYMSG,KEYLANG,0,0,0,0,0)                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0803)                                                        
MDCTCHAX EQU   *                                                                
*                                                                               
MDCTDEL  DC    AL1(MDCTDELX-MDCTDEL)                                            
         DC    AL1(RECDCT,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'E121'                                                          
         DC    AL1(3,3,KEYSYS,KEYMSG,KEYLANG,0,0,0,0,0)                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0804)                                                        
MDCTDELX EQU   *                                                                
*                                                                               
MDCTRES  DC    AL1(MDCTRESX-MDCTRES)                                            
         DC    AL1(RECDCT,ACTRES)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'E121'                                                          
         DC    AL1(3,3,KEYSYS,KEYMSG,KEYLANG,0,0,0,0,0)                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0805)                                                        
MDCTRESX EQU   *                                                                
*                                                                               
MDCTLST  DC    AL1(MDCTLSTX-MDCTLST)                                            
         DC    AL1(RECDCT,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'E221'                                                          
         DC    AL1(3,3,KEYSYS,KEYLANG,KEYMSG,0,0,0,0,0)                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0806)                                                        
MDCTLSTX EQU   *                                                                
*                                                                               
MDCTREP  DC    AL1(MDCTREPX-MDCTREP)                                            
         DC    AL1(RECDCT,ACTREP)                                               
         DC    AL1(MIXIREP,MIXIOKN+MIXIOKS+MIXIOKO+MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'E621'                                                          
         DC    AL1(2,2,KEYSYS,KEYLANG,0,0,0,0,0,0)                              
         DC    AL4(0,0)                                                         
         DC    CL8'CTMLML'                                                      
         DC    AL2(0807)                                                        
MDCTREPX EQU   *                                                                
*                                                                               
MDCTSEL  DC    AL1(MDCTSELX-MDCTSEL)                                            
         DC    AL1(RECDCT,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'E121'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0808)                                                        
MDCTSELX EQU   *                                                                
*                                                                               
MSCRADD  DC    AL1(MSCRADDX-MSCRADD)                                            
         DC    AL1(RECSCR,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXDREQ+MIXIDDS,MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'C624'                                                          
         DC    AL1(1,1,KEYSCR,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3601)                                                        
MSCRADDX EQU   *                                                                
*                                                                               
MSCRDIS  DC    AL1(MSCRDISX-MSCRDIS)                                            
         DC    AL1(RECSCR,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'C624'                                                          
         DC    AL1(1,1,KEYSCR,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3602)                                                        
MSCRDISX EQU   *                                                                
*                                                                               
MSCRCHA  DC    AL1(MSCRCHAX-MSCRCHA)                                            
         DC    AL1(RECSCR,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'C624'                                                          
         DC    AL1(1,1,KEYSCR,0,0,0,0,0,0)                                      
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3603)                                                        
MSCRCHAX EQU   *                                                                
*                                                                               
MSCRDEL  DC    AL1(MSCRDELX-MSCRDEL)                                            
         DC    AL1(RECSCR,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'C624'                                                          
         DC    AL1(1,1,KEYSCR,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3604)                                                        
MSCRDELX EQU   *                                                                
*                                                                               
MSCRRES  DC    AL1(MSCRRESX-MSCRRES)                                            
         DC    AL1(RECSCR,ACTRES)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'C624'                                                          
         DC    AL1(1,1,KEYSCR,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3605)                                                        
MSCRRESX EQU   *                                                                
*                                                                               
MSCRLST  DC    AL1(MSCRLSTX-MSCRLST)                                            
         DC    AL1(RECSCR,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'CD24'                                                          
         DC    AL1(1,1,KEYSCR,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3606)                                                        
MSCRLSTX EQU   *                                                                
*                                                                               
MSCRREP  DC    AL1(MSCRREPX-MSCRREP)                                            
         DC    AL1(RECSCR,ACTREP)                                               
         DC    AL1(MIXIREP,MIXIOKN+MIXIOKS+MIXIOKO+MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'AB24'                                                          
         DC    AL1(1,1,KEYSCR,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    CL8'CTMLML'                                                      
         DC    AL2(3607)                                                        
MSCRREPX EQU   *                                                                
*                                                                               
MSCRSEL  DC    AL1(MSCRSELX-MSCRSEL)                                            
         DC    AL1(RECSCR,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'C624'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3608)                                                        
MSCRSELX EQU   *                                                                
*                                                                               
MBOKADD  DC    AL1(MBOKADDX-MBOKADD)                                            
         DC    AL1(RECBOK,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXDREQ+MIXIDDS,MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'AD23'                                                          
         DC    AL1(2,2,KEYSYS,KEYBOK,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3701)                                                        
MBOKADDX EQU   *                                                                
*                                                                               
MBOKDIS  DC    AL1(MBOKDISX-MBOKDIS)                                            
         DC    AL1(RECBOK,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'AD23'                                                          
         DC    AL1(2,2,KEYSYS,KEYBOK,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3702)                                                        
MBOKDISX EQU   *                                                                
*                                                                               
MBOKCHA  DC    AL1(MBOKCHAX-MBOKCHA)                                            
         DC    AL1(RECBOK,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'AD23'                                                          
         DC    AL1(2,2,KEYSYS,KEYBOK,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3703)                                                        
MBOKCHAX EQU   *                                                                
*                                                                               
MBOKDEL  DC    AL1(MBOKDELX-MBOKDEL)                                            
         DC    AL1(RECBOK,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'AD23'                                                          
         DC    AL1(2,2,KEYSYS,KEYBOK,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3704)                                                        
MBOKDELX EQU   *                                                                
*                                                                               
MBOKRES  DC    AL1(MBOKRESX-MBOKRES)                                            
         DC    AL1(RECBOK,ACTRES)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'AD23'                                                          
         DC    AL1(2,2,KEYSYS,KEYBOK,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3705)                                                        
MBOKRESX EQU   *                                                                
*                                                                               
MBOKLST  DC    AL1(MBOKLSTX-MBOKLST)                                            
         DC    AL1(RECBOK,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'AC23'                                                          
         DC    AL1(2,2,KEYSYS,KEYBOK,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3706)                                                        
MBOKLSTX EQU   *                                                                
*                                                                               
MBOKREP  DC    AL1(MBOKREPX-MBOKREP)                                            
         DC    AL1(RECBOK,ACTREP)                                               
         DC    AL1(MIXIREP,MIXIOKN+MIXIOKS+MIXIOKO+MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'A523'                                                          
         DC    AL1(2,2,KEYSYS,KEYBOK,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    CL8'CTMLML'                                                      
         DC    AL2(3707)                                                        
MBOKREPX EQU   *                                                                
*                                                                               
MBOKCPY  DC    AL1(MBOKCPYX-MBOKCPY)                                            
         DC    AL1(RECBOK,ACTCPY)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'AD23'                                                          
         DC    AL1(2,2,KEYSYS,KEYBOK,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3710)                                                        
MBOKCPYX EQU   *                                                                
*                                                                               
MBOKSEL  DC    AL1(MBOKSELX-MBOKSEL)                                            
         DC    AL1(RECBOK,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'AD23'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3708)                                                        
MBOKSELX EQU   *                                                                
*                                                                               
MBOKUPD  DC    AL1(MBOKUPDX-MBOKUPD)                                            
         DC    AL1(RECBOK,ACTUPD)                                               
         DC    AL1(MIXIDDS+MIXIREP,MIXIOKN+MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'AD23'                                                          
         DC    AL1(1,1,KEYBOK,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3709)                                                        
MBOKUPDX EQU   *                                                                
*                                                                               
MFACADD  DC    AL1(MFACADDX-MFACADD)                                            
         DC    AL1(RECFAC,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXDREQ+MIXIDDS,MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'A720'                                                          
         DC    AL1(3,3,KEYTYP,KEYSYS,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0804)                                                        
MFACADDX EQU   *                                                                
*                                                                               
MFACDIS  DC    AL1(MFACDISX-MFACDIS)                                            
         DC    AL1(RECFAC,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'A720'                                                          
         DC    AL1(3,3,KEYTYP,KEYSYS,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0805)                                                        
MFACDISX EQU   *                                                                
*                                                                               
MFACCHA  DC    AL1(MFACCHAX-MFACCHA)                                            
         DC    AL1(RECFAC,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'A720'                                                          
         DC    AL1(3,3,KEYTYP,KEYSYS,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0806)                                                        
MFACCHAX EQU   *                                                                
*                                                                               
MFACDEL  DC    AL1(MFACDELX-MFACDEL)                                            
         DC    AL1(RECFAC,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'A720'                                                          
         DC    AL1(3,3,KEYTYP,KEYSYS,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0807)                                                        
MFACDELX EQU   *                                                                
*                                                                               
MFACRES  DC    AL1(MFACRESX-MFACRES)                                            
         DC    AL1(RECFAC,ACTRES)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'A720'                                                          
         DC    AL1(3,3,KEYTYP,KEYSYS,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0808)                                                        
MFACRESX EQU   *                                                                
*                                                                               
MFACLST  DC    AL1(MFACLSTX-MFACLST)                                            
         DC    AL1(RECFAC,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'B720'                                                          
         DC    AL1(3,3,KEYTYP,KEYSYS,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0809)                                                        
MFACLSTX EQU   *                                                                
*                                                                               
MFACREP  DC    AL1(MFACREPX-MFACREP)                                            
         DC    AL1(RECFAC,ACTREP)                                               
         DC    AL1(MIXIREP,MIXIOKN+MIXIOKS+MIXIOKO+MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'BB20'                                                          
         DC    AL1(3,3,KEYTYP,KEYSYS,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    CL8'CTFLFL'                                                      
         DC    AL2(0810)                                                        
MFACREPX EQU   *                                                                
*                                                                               
MFACSEL  DC    AL1(MFACSELX-MFACSEL)                                            
         DC    AL1(RECFAC,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'A720'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0811)                                                        
MFACSELX EQU   *                                                                
*                                                                               
MDDSDIS  DC    AL1(MDDSDISX-MDDSDIS)                                            
         DC    AL1(RECDDS,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'CA25'                                                          
         DC    AL1(2,2,KEYDEP,KEYLEV,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0812)                                                        
MDDSDISX EQU   *                                                                
*                                                                               
MDDSCHA  DC    AL1(MDDSCHAX-MDDSCHA)                                            
         DC    AL1(RECDDS,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'CA25'                                                          
         DC    AL1(2,2,KEYDEP,KEYLEV,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0812)                                                        
MDDSCHAX EQU   *                                                                
*                                                                               
MPGMADD  DC    AL1(MPGMADDX-MPGMADD)                                            
         DC    AL1(RECPGM,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXDREQ+MIXIDDS,MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'A720'                                                          
         DC    AL1(2,2,KEYSYS,KEYPGM,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0804)                                                        
MPGMADDX EQU   *                                                                
*                                                                               
MPGMDIS  DC    AL1(MPGMDISX-MPGMDIS)                                            
         DC    AL1(RECPGM,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'A720'                                                          
         DC    AL1(2,2,KEYSYS,KEYPGM,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0805)                                                        
MPGMDISX EQU   *                                                                
*                                                                               
MPGMCHA  DC    AL1(MPGMCHAX-MPGMCHA)                                            
         DC    AL1(RECPGM,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'A720'                                                          
         DC    AL1(2,2,KEYSYS,KEYPGM,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0806)                                                        
MPGMCHAX EQU   *                                                                
*                                                                               
MPGMDEL  DC    AL1(MPGMDELX-MPGMDEL)                                            
         DC    AL1(RECPGM,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'A720'                                                          
         DC    AL1(2,2,KEYSYS,KEYPGM,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0807)                                                        
MPGMDELX EQU   *                                                                
*                                                                               
MPGMRES  DC    AL1(MPGMRESX-MPGMRES)                                            
         DC    AL1(RECPGM,ACTRES)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'A720'                                                          
         DC    AL1(2,2,KEYSYS,KEYPGM,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0808)                                                        
MPGMRESX EQU   *                                                                
*                                                                               
MPGMLST  DC    AL1(MPGMLSTX-MPGMLST)                                            
         DC    AL1(RECPGM,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'B720'                                                          
         DC    AL1(2,2,KEYSYS,KEYPGM,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0809)                                                        
MPGMLSTX EQU   *                                                                
*                                                                               
MPGMREP  DC    AL1(MPGMREPX-MPGMREP)                                            
         DC    AL1(RECPGM,ACTREP)                                               
         DC    AL1(MIXIREP,MIXIOKN+MIXIOKS+MIXIOKO+MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'BB20'                                                          
         DC    AL1(2,2,KEYSYS,KEYPGM,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    CL8'CTFLFL'                                                      
         DC    AL2(0810)                                                        
MPGMREPX EQU   *                                                                
*                                                                               
MPGMSEL  DC    AL1(MPGMSELX-MPGMSEL)                                            
         DC    AL1(RECPGM,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'A720'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0811)                                                        
MPGMSELX EQU   *                                                                
*                                                                               
MEASADD  DC    AL1(MEASADDX-MEASADD)                                            
         DC    AL1(RECEAS,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXDREQ+MIXIDDS,MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'F304'                                                          
         DC    AL1(1,1,KEYEAS,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3804)                                                        
MEASADDX EQU   *                                                                
*                                                                               
MEASDIS  DC    AL1(MEASDISX-MEASDIS)                                            
         DC    AL1(RECEAS,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F304'                                                          
         DC    AL1(1,1,KEYEAS,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3805)                                                        
MEASDISX EQU   *                                                                
*                                                                               
MEASCHA  DC    AL1(MEASCHAX-MEASCHA)                                            
         DC    AL1(RECEAS,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'F304'                                                          
         DC    AL1(1,1,KEYEAS,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3806)                                                        
MEASCHAX EQU   *                                                                
*                                                                               
MEASDEL  DC    AL1(MEASDELX-MEASDEL)                                            
         DC    AL1(RECEAS,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'F304'                                                          
         DC    AL1(1,1,KEYEAS,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3807)                                                        
MEASDELX EQU   *                                                                
*                                                                               
MEASRES  DC    AL1(MEASRESX-MEASRES)                                            
         DC    AL1(RECEAS,ACTRES)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'F304'                                                          
         DC    AL1(1,1,KEYEAS,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3808)                                                        
MEASRESX EQU   *                                                                
*                                                                               
MEASLST  DC    AL1(MEASLSTX-MEASLST)                                            
         DC    AL1(RECEAS,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'D304'                                                          
         DC    AL1(1,1,KEYEAS,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3809)                                                        
MEASLSTX EQU   *                                                                
*                                                                               
MEASREP  DC    AL1(MEASREPX-MEASREP)                                            
         DC    AL1(RECEAS,ACTREP)                                               
         DC    AL1(MIXIREP,MIXIOKN+MIXIOKS+MIXIOKO+MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'B304'                                                          
         DC    AL1(1,1,KEYEAS,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    CL8'CTFLFL'                                                      
         DC    AL2(3810)                                                        
MEASREPX EQU   *                                                                
*                                                                               
MEASSEL  DC    AL1(MEASSELX-MEASSEL)                                            
         DC    AL1(RECEAS,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'F304'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3811)                                                        
MEASSELX EQU   *                                                                
*                                                                               
MPRVADD  DC    AL1(MPRVADDX-MPRVADD)                                            
         DC    AL1(RECPRV,ACTADD)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ+MIXDREQ,MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'AA0C'                                                          
         DC    AL1(1,1,KEYPTY,KEYPNM,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(3901)                                                        
MPRVADDX EQU   *                                                                
*                                                                               
MPRVDIS  DC    AL1(MPRVDISX-MPRVDIS)                                            
         DC    AL1(RECPRV,ACTDIS)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'AA0C'                                                          
         DC    AL1(1,1,KEYPTY,KEYPNM,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3902)                                                        
MPRVDISX EQU   *                                                                
*                                                                               
MPRVCHA  DC    AL1(MPRVCHAX-MPRVCHA)                                            
         DC    AL1(RECPRV,ACTCHA)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'AA0C'                                                          
         DC    AL1(1,1,KEYPTY,KEYPNM,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(3903)                                                        
MPRVCHAX EQU   *                                                                
*                                                                               
MPRVDEL  DC    AL1(MPRVDELX-MPRVDEL)                                            
         DC    AL1(RECPRV,ACTDEL)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'AA0C'                                                          
         DC    AL1(1,1,KEYPTY,KEYPNM,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(3904)                                                        
MPRVDELX EQU   *                                                                
*                                                                               
MPRVRES  DC    AL1(MPRVRESX-MPRVRES)                                            
         DC    AL1(RECPRV,ACTRES)                                               
         DC    AL1(MIXIDDS+MIXILFM+MIXKREQ,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'AA0C'                                                          
         DC    AL1(1,1,KEYPTY,KEYPNM,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    AL1(RESTRCTQ),XL7'00'                                            
         DC    AL2(3905)                                                        
MPRVRESX EQU   *                                                                
*                                                                               
MPRVLST  DC    AL1(MPRVLSTX-MPRVLST)                                            
         DC    AL1(RECPRV,ACTLST)                                               
         DC    AL1(MIXIDDS+MIXILST,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'A30C'                                                          
         DC    AL1(1,1,KEYPTY,KEYPNM,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(3906)                                                        
MPRVLSTX EQU   *                                                                
*                                                                               
MPRVREP  DC    AL1(MPRVREPX-MPRVREP)                                            
         DC    AL1(RECPRV,ACTREP)                                               
         DC    AL1(MIXIDDS+MIXIREP,MIXIOKS+MIXIOKO+MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'B40C'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    CL8'CTOLOL'                                                      
         DC    AL2(3907)                                                        
MPRVREPX EQU   *                                                                
*                                                                               
MPRVSEL  DC    AL1(MPRVSELX-MPRVSEL)                                            
         DC    AL1(RECPRV,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'AA0C'                                                          
         DC    AL1(1,1,KEYPTY,KEYPNM,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(0)                                                           
MPRVSELX EQU   *                                                                
*                                                                               
MIXTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* KEY COMPONENT TABLE (SEE KEYTABD)                                   *         
***********************************************************************         
         SPACE 1                                                                
KEYTAB   DS    0X                                                               
*                                                                               
         DC    C'OUTTYPE',AL1(KEYOUT)                                           
         DC    C'SYSTEM ',AL1(KEYSYS)                                           
         DC    C'MSGTYP ',AL1(KEYMTYP)                                          
         DC    C'MSGNUM ',AL1(KEYMSG)                                           
         DC    C'LANG   ',AL1(KEYLANG)                                          
         DC    C'COUNTRY',AL1(KEYCTRY)                                          
         DC    C'AGENCY ',AL1(KEYAGY)                                           
         DC    C'PROGRAM',AL1(KEYPGM)                                           
         DC    C'SUBSYS ',AL1(KEYSUB)                                           
         DC    C'ESSID  ',AL1(KEYESSID)                                         
         DC    C'USERID ',AL1(KEYUID)                                           
         DC    C'DATE   ',AL1(KEYDAT)                                           
         DC    C'TIME   ',AL1(KEYTIM)                                           
         DC    C'TYPE   ',AL1(KEYTYP)                                           
         DC    C'MEDIA  ',AL1(KEYMED)                                           
         DC    C'CLIENT ',AL1(KEYCLI)                                           
         DC    C'TERMINL',AL1(KEYTRM)                                           
         DC    C'PASSWRD',AL1(KEYPWD)                                           
         DC    C'CURRNCY',AL1(KEYCUR)                                           
         DC    C'FRM-CUR',AL1(KEYCRF)                                           
         DC    C'TO-CUR ',AL1(KEYCRT)                                           
         DC    C'PERIOD ',AL1(KEYPER)                                           
         DC    C'NAME   ',AL1(KEYNAM)                                           
         DC    C'CODE   ',AL1(KEYCODE)                                          
         DC    C'CHANNEL',AL1(KEYCHAN)                                          
         DC    C'DSCHEME',AL1(KEYSCHM)                                          
         DC    C'ALPHAID',AL1(KEYACC)                                           
         DC    C'LISTTYP',AL1(KEYLTYP)                                          
         DC    C'PROFTYP',AL1(KEYPTYP)                                          
         DC    C'SCRIPT ',AL1(KEYSCR)                                           
         DC    C'BOOK   ',AL1(KEYBOK)                                           
         DC    C'EASI   ',AL1(KEYEAS)                                           
         DC    C'DEPT   ',AL1(KEYDEP)                                           
         DC    C'LEVEL  ',AL1(KEYLEV)                                           
         DC    C'PROTYPE',AL1(KEYPTY)                                           
         DC    C'PRONAME',AL1(KEYPNM)                                           
*                                                                               
KEYTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* OPTION TABLE (SEE OPTTABD)                                          *         
***********************************************************************         
         SPACE 1                                                                
OPTTAB   DS    0X                  ** OPTIONS TABLE **                          
*                                                                               
SCANOPT  DC    AL1(SCANOPTX-SCANOPT)                                            
         DC    C'SCAN    ',C'SEL',AL1(OPTNRTN+OPTITXT,0)                        
         DC    AL1(0,0)                                                         
         DC    AL1(RECMSG,0)                                                    
         DC    AL1(2,1,L'OPTSCAN-2,L'OPTSCAN)                                   
         DC    AL4(OPTSCANB)                                                    
         DC    AL1(OPTSCANN)                                                    
         DC    AL2(OPTSCANR)                                                    
         DC    AL2(OPTSCAN-WORKD)                                               
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(1,2)                                                         
SCANOPTX EQU   *                                                                
*                                                                               
SCNOPT2  DC    AL1(SCNOPT2X-SCNOPT2)                                            
         DC    C'SCAN    ',C'SEL',AL1(OPTNRTN+OPTITXT,0)                        
         DC    AL1(0,0)                                                         
         DC    AL1(RECDCT,0)                                                    
         DC    AL1(2,1,L'OPTSCAN-2,L'OPTSCAN)                                   
         DC    AL4(OPTSCANB)                                                    
         DC    AL1(OPTSCANN)                                                    
         DC    AL2(OPTSCANR)                                                    
         DC    AL2(OPTSCAN-WORKD)                                               
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(1,2)                                                         
SCNOPT2X EQU   *                                                                
*                                                                               
PROGOPT  DC    AL1(PROGOPTX-PROGOPT)                                            
         DC    C'PROGRAM ',C'PGM',AL1(OPTNRTN+OPTITXT,0)                        
         DC    AL1(0,0)                                                         
         DC    AL1(RECMSG,0)                                                    
         DC    AL1(2,1,7,L'OPTPROG)                                             
         DC    AL4(OPTPROGB)                                                    
         DC    AL1(OPTPROGN)                                                    
         DC    AL2(OPTPROGR)                                                    
         DC    AL2(OPTPROG-WORKD)                                               
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(3,4)                                                         
PROGOPTX EQU   *                                                                
*                                                                               
SUBOPT   DC    AL1(SUBOPTX-SUBOPT)                                              
         DC    C'SUBREF  ',C'SUB',AL1(OPTNRTN+OPTITXT,0)                        
         DC    AL1(0,0)                                                         
         DC    AL1(RECMSG,0)                                                    
         DC    AL1(2,1,8,L'OPTSUB)                                              
         DC    AL4(OPTSUBB)                                                     
         DC    AL1(OPTSUBN)                                                     
         DC    AL2(OPTSUBR)                                                     
         DC    AL2(OPTSUB-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(5,6)                                                         
SUBOPTX  EQU   *                                                                
*                                                                               
USEROPT  DC    AL1(USEROPTX-USEROPT)                                            
         DC    C'USERID  ',C'U  ',AL1(OPTNRTN+OPTITXT,0)                        
         DC    AL1(0,0)                                                         
         DC    AL1(RECAUT,0)                                                    
         DC    AL1(2,1,8,L'OPTUSR)                                              
         DC    AL4(OPTUSRB)                                                     
         DC    AL1(OPTUSRN)                                                     
         DC    AL2(OPTUSRR)                                                     
         DC    AL2(OPTUSR-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(7,8)                                                         
USEROPTX EQU   *                                                                
*                                                                               
SYSOPT   DC    AL1(SYSOPTX-SYSOPT)                                              
         DC    C'SYSTEM  ',C'SYS',AL1(OPTNRTN+OPTITXT,0)                        
         DC    AL1(0,0)                                                         
         DC    AL1(RECAUT,0)                                                    
         DC    AL1(2,1,8,L'OPTSYS)                                              
         DC    AL4(OPTSYSB)                                                     
         DC    AL1(OPTSYSN)                                                     
         DC    AL2(OPTSYSR)                                                     
         DC    AL2(OPTSYS-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(9,10)                                                        
SYSOPTX  EQU   *                                                                
*                                                                               
RNGEOPT  DC    AL1(RNGEOPTX-RNGEOPT)                                            
         DC    C'RANGE   ',C'RNG',AL1(OPTNRTN+OPTITXT,0)                        
         DC    AL1(0,0)                                                         
         DC    AL1(RECAUT,0)                                                    
         DC    AL1(2,1,17,L'OPTRNG)                                             
         DC    AL4(OPTRNGB)                                                     
         DC    AL1(OPTRNGN)                                                     
         DC    AL2(OPTRNGR)                                                     
         DC    AL2(OPTRNG-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(11,12)                                                       
RNGEOPTX EQU   *                                                                
*                                                                               
OUTTOPT  DC    AL1(OUTTOPTX-OUTTOPT)                                            
         DC    C'OUTPUT  ',C'OUT',AL1(OPTNRTN+OPTITXT,0)                        
         DC    AL1(0,0)                                                         
         DC    AL1(RECIDI,0)                                                    
         DC    AL1(2,1,10,L'OPTOUT)                                             
         DC    AL4(OPTOUTB)                                                     
         DC    AL1(OPTOUTN)                                                     
         DC    AL2(OPTOUTR)                                                     
         DC    AL2(OPTOUT-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(13,14)                                                       
OUTTOPTX EQU   *                                                                
*                                                                               
OPTTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* SELECTABLE ACTION TABLE (SEE SELTABD)                               *         
***********************************************************************         
         SPACE 1                                                                
SELTAB   DS    0X                                                               
*                                                                               
         DC    C'S',AL1(RECOUT,ACTLST,RECOUT,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECOUT,ACTLST,RECOUT,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&UK                                                                           
         DC    C'D',AL1(RECOUT,ACTLST,RECOUT,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&                                                                             
*                                                                               
         DC    C'S',AL1(RECMSG,ACTLST,RECMSG,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECMSG,ACTLST,RECMSG,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&UK                                                                           
         DC    C'D',AL1(RECMSG,ACTLST,RECMSG,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&                                                                             
*                                                                               
         DC    C'S',AL1(RECUID,ACTLST,RECUID,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECUID,ACTLST,RECUID,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&UK                                                                           
         DC    C'D',AL1(RECUID,ACTLST,RECUID,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&                                                                             
         DC    C'I',AL1(RECUID,ACTLST,RECIDI,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'A',AL1(RECUID,ACTLST,RECIDA,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*                                                                               
         DC    C'S',AL1(RECIDI,ACTLST,RECIDI,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECIDI,ACTLST,RECIDI,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'U',AL1(RECIDI,ACTLST,RECUID,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'A',AL1(RECIDI,ACTLST,RECIDA,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*                                                                               
         DC    C'S',AL1(RECIDA,ACTLST,RECIDA,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECIDA,ACTLST,RECIDA,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'U',AL1(RECIDA,ACTLST,RECUID,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'I',AL1(RECIDA,ACTLST,RECIDI,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*                                                                               
         DC    C'S',AL1(RECTRM,ACTLST,RECTRM,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECTRM,ACTLST,RECTRM,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&UK                                                                           
         DC    C'D',AL1(RECTRM,ACTLST,RECTRM,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&                                                                             
         DC    C'P',AL1(RECTRM,ACTLST,RECPRT,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*                                                                               
         DC    C'S',AL1(RECNAR,ACTLST,RECNAR,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECNAR,ACTLST,RECNAR,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&UK                                                                           
         DC    C'D',AL1(RECNAR,ACTLST,RECNAR,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&                                                                             
*                                                                               
         DC    C'S',AL1(RECCUR,ACTLST,RECCUR,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECCUR,ACTLST,RECCUR,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&UK                                                                           
         DC    C'D',AL1(RECCUR,ACTLST,RECCUR,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&                                                                             
*                                                                               
         DC    C'S',AL1(RECEXC,ACTLST,RECEXC,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECEXC,ACTLST,RECEXC,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&UK                                                                           
         DC    C'D',AL1(RECEXC,ACTLST,RECEXC,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&                                                                             
*                                                                               
         DC    C'S',AL1(RECAUT,ACTLST,RECAUT,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*                                                                               
         DC    C'S',AL1(RECPAY,ACTLST,RECPAY,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECPAY,ACTLST,RECPAY,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*                                                                               
         DC    C'S',AL1(RECCHN,ACTLST,RECCHN,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECCHN,ACTLST,RECCHN,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*                                                                               
         DC    C'S',AL1(RECDAT,ACTLST,RECDAT,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECDAT,ACTLST,RECDAT,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&UK                                                                           
         DC    C'D',AL1(RECDAT,ACTLST,RECDAT,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&                                                                             
*                                                                               
         DC    C'S',AL1(RECACC,ACTLST,RECACC,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECACC,ACTLST,RECACC,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&UK                                                                           
         DC    C'D',AL1(RECACC,ACTLST,RECACC,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&                                                                             
         DC    C'1',AL1(RECACC,ACTLST,RECSEC,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*                                                                               
         DC    C'S',AL1(RECTVR,ACTLST,RECTVR,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECTVR,ACTLST,RECTVR,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*                                                                               
         DC    C'S',AL1(RECFLD,ACTLST,RECFLD,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECFLD,ACTLST,RECFLD,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&UK                                                                           
         DC    C'D',AL1(RECFLD,ACTLST,RECFLD,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&                                                                             
*                                                                               
         DC    C'S',AL1(RECPRO,ACTLST,RECPRO,ACTDIS)                            
         DC    AL1(KEYSYS,KEYPGM,KEYUID,KEYPTYP,0,0,0,0)                        
         DC    AL1(0,0),AL1(0),AL1(SELIPROC),XL7'00'                            
         DC    C'C',AL1(RECPRO,ACTLST,RECPRO,ACTCHA)                            
         DC    AL1(KEYSYS,KEYPGM,KEYUID,KEYPTYP,0,0,0,0)                        
         DC    AL1(0,0),AL1(0),AL1(SELIPROC),XL7'00'                            
*&&UK                                                                           
         DC    C'D',AL1(RECPRO,ACTLST,RECPRO,ACTDEL)                            
         DC    AL1(KEYSYS,KEYPGM,KEYUID,KEYPTYP,0,0,0,0)                        
         DC    AL1(0,0),AL1(0),AL1(SELIPROC),XL7'00'                            
*&&                                                                             
*                                                                               
         DC    C'S',AL1(RECSYL,ACTLST,RECSYL,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECSYL,ACTLST,RECSYL,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&UK                                                                           
         DC    C'D',AL1(RECSYL,ACTLST,RECSYL,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&                                                                             
*                                                                               
         DC    C'S',AL1(RECDCT,ACTLST,RECDCT,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECDCT,ACTLST,RECDCT,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&UK                                                                           
         DC    C'D',AL1(RECDCT,ACTLST,RECDCT,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&                                                                             
*                                                                               
         DC    C'S',AL1(RECSCR,ACTLST,RECSCR,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECSCR,ACTLST,RECSCR,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&UK                                                                           
         DC    C'D',AL1(RECSCR,ACTLST,RECSCR,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&                                                                             
*                                                                               
         DC    C'S',AL1(RECBOK,ACTLST,RECBOK,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECBOK,ACTLST,RECBOK,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&UK                                                                           
         DC    C'D',AL1(RECBOK,ACTLST,RECBOK,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&                                                                             
*                                                                               
         DC    C'S',AL1(RECFAC,ACTLST,RECFAC,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECFAC,ACTLST,RECFAC,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&UK                                                                           
         DC    C'D',AL1(RECFAC,ACTLST,RECFAC,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&                                                                             
*                                                                               
         DC    C'S',AL1(RECEAS,ACTLST,RECEAS,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECEAS,ACTLST,RECEAS,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&UK                                                                           
         DC    C'D',AL1(RECEAS,ACTLST,RECEAS,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&                                                                             
*                                                                               
         DC    C'S',AL1(RECPRV,ACTLST,RECPRV,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECPRV,ACTLST,RECPRV,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&UK                                                                           
         DC    C'D',AL1(RECPRV,ACTLST,RECPRV,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&                                                                             
*                                                                               
SELTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
* CTGENWRK                                                                      
         SPACE 1                                                                
       ++INCLUDE CTGENWRK                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'073CTGEN01S  02/27/96'                                      
         END                                                                    

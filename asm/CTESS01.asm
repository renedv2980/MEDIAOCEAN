*          DATA SET CTESS01    AT LEVEL 017 AS OF 11/04/08                      
*PHASE TA0E01A                                                                  
*                                                                               
*                                                                               
         TITLE 'CTESS01 -  ESS RECORD MAINTENANCE - CONTROLLER TABLES'          
ESS01    CSECT                                                                  
         SPACE 2                                                                
ARECTAB  DC    AL4(RECTAB-ESS01)   RECORD TABLE                                 
AACTTAB  DC    AL4(ACTTAB-ESS01)   ACTION TABLE                                 
AMIXTAB  DC    AL4(MIXTAB-ESS01)   REC/ACT MIX TABLE                            
AKEYTAB  DC    AL4(KEYTAB-ESS01)   KEY COMPONENT TABLE                          
AOPTTAB  DC    AL4(OPTTAB-ESS01)   OPTION TABLE                                 
ASELTAB  DC    AL4(SELTAB-ESS01)   SELECTABLE ACTION TABLE                      
         EJECT                                                                  
***********************************************************************         
* RECORD TYPE TABLE (SEE RECTABD)                                     *         
***********************************************************************         
         SPACE 1                                                                
RECTAB   DS    0X                                                               
*                                                                               
RESS     DC    AL1(RESSX-RESS)                                                  
         DC    C'ESSID   ',C'   ',AL1(RECITXT+RECIDDS)                          
         DC    AL1(0,0,RECESS)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(GENFILQ),XL7'00'                                             
         DC    AL2(5000)                                                        
RESSX    EQU   *                                                                
*                                                                               
RXAG     DC    AL1(RXAGX-RXAG)                                                  
         DC    C'XAGENCY ',C'   ',AL1(RECITXT+RECIDDS)                          
         DC    AL1(0,0,RECXAG)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(GENFILQ),XL7'00'                                             
         DC    AL2(5001)                                                        
RXAGX    EQU   *                                                                
*                                                                               
RXFL     DC    AL1(RXFLX-RXFL)                                                  
         DC    C'XFILE   ',C'   ',AL1(RECITXT+RECIDDS)                          
         DC    AL1(0,0,RECXFL)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(GENFILQ),XL7'00'                                             
         DC    AL2(5002)                                                        
RXFLX    EQU   *                                                                
*                                                                               
RXLG     DC    AL1(RXLGX-RXLG)                                                  
         DC    C'XLOG    ',C'   ',AL1(RECITXT+RECIDDS)                          
         DC    AL1(0,0,RECXLG)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(GENFILQ),XL7'00'                                             
         DC    AL2(5003)                                                        
RXLGX    EQU   *                                                                
*                                                                               
RXTR     DC    AL1(RXTRX-RXTR)                                                  
         DC    C'XTRANS  ',C'   ',AL1(RECITXT+RECIDDS)                          
         DC    AL1(0,0,RECXTR)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(GENFILQ),XL7'00'                                             
         DC    AL2(5004)                                                        
RXTRX    EQU   *                                                                
*                                                                               
RXAP     DC    AL1(RXAPX-RXAP)                                                  
         DC    C'XAPPLIC ',C'   ',AL1(RECITXT+RECIDDS)                          
         DC    AL1(0,0,RECXAP)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(GENFILQ),XL7'00'                                             
         DC    AL2(5005)                                                        
RXAPX    EQU   *                                                                
*                                                                               
RREF     DC    AL1(RREFX-RREF)                                                  
         DC    C'REFORM  ',C'   ',AL1(RECITXT+RECIDDS)                          
         DC    AL1(0,0,RECREF)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(GENFILQ),XL7'00'                                             
         DC    AL2(5006)                                                        
RREFX    EQU   *                                                                
*                                                                               
RBDE     DC    AL1(RBDEX-RBDE)                                                  
         DC    C'BDE     ',C'   ',AL1(RECITXT+RECIDDS)                          
         DC    AL1(0,0,RECBDE)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(GENFILQ),XL7'00'                                             
         DC    AL2(5007)                                                        
RBDEX    EQU   *                                                                
*                                                                               
RXLI     DC    AL1(RXLIX-RXLI)                                                  
         DC    C'XTLOGIN ',C'   ',AL1(RECITXT+RECIDDS)                          
         DC    AL1(0,0,RECXLI)                                                  
         DC    AL4(0,0)                                                         
         DC    AL1(GENFILQ),XL7'00'                                             
         DC    AL2(5008)                                                        
RXLIX    EQU   *                                                                
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
         DCDD  CT#TRN,8                                                         
         DC    AL1(0,0,0,ACTTRN)                                                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
*                                                                               
ACTTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* RECORD/ACTION COMBO TABLE (SEE MIXTABD)                             *         
***********************************************************************         
         SPACE 1                                                                
MIXTAB   DS    0X                                                               
*                                                                               
MESSADD  DC    AL1(MESSADDX-MESSADD)                                            
         DC    AL1(RECESS,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXDREQ+MIXIDDS,MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'FD02'                                                          
         DC    AL1(1,1,KEYESSID,0,0,0,0,0,0,0)                                  
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5101)                                                        
MESSADDX EQU   *                                                                
*                                                                               
MESSDIS  DC    AL1(MESSDISX-MESSDIS)                                            
         DC    AL1(RECESS,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'FD02'                                                          
         DC    AL1(1,1,KEYESSID,0,0,0,0,0,0,0)                                  
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5102)                                                        
MESSDISX EQU   *                                                                
*                                                                               
MESSCHA  DC    AL1(MESSCHAX-MESSCHA)                                            
         DC    AL1(RECESS,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'FD02'                                                          
         DC    AL1(1,1,KEYESSID,0,0,0,0,0,0,0)                                  
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5103)                                                        
MESSCHAX EQU   *                                                                
*                                                                               
MESSDEL  DC    AL1(MESSDELX-MESSDEL)                                            
         DC    AL1(RECESS,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'FD02'                                                          
         DC    AL1(1,1,KEYESSID,0,0,0,0,0,0,0)                                  
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5104)                                                        
MESSDELX EQU   *                                                                
*                                                                               
MESSRES  DC    AL1(MESSRESX-MESSRES)                                            
         DC    AL1(RECESS,ACTRES)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'FD02'                                                          
         DC    AL1(1,1,KEYESSID,0,0,0,0,0,0,0)                                  
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5105)                                                        
MESSRESX EQU   *                                                                
*                                                                               
MESSLST  DC    AL1(MESSLSTX-MESSLST)                                            
         DC    AL1(RECESS,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'DD02'                                                          
         DC    AL1(1,1,KEYESSID,0,0,0,0,0,0,0)                                  
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5106)                                                        
MESSLSTX EQU   *                                                                
*                                                                               
MESSREP  DC    AL1(MESSREPX-MESSREP)                                            
         DC    AL1(RECESS,ACTREP)                                               
         DC    AL1(MIXIREP,MIXIOKN+MIXIOKS+MIXIOKO+MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'BD02'                                                          
         DC    AL1(1,1,KEYESSID,0,0,0,0,0,0,0)                                  
         DC    AL4(0,0)                                                         
         DC    CL8'CTEEEE'                                                      
         DC    AL2(5107)                                                        
MESSREPX EQU   *                                                                
*                                                                               
MESSSEL  DC    AL1(MESSSELX-MESSSEL)                                            
         DC    AL1(RECESS,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'FD02'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5108)                                                        
MESSSELX EQU   *                                                                
*                                                                               
MXAGADD  DC    AL1(MXAGADDX-MXAGADD)                                            
         DC    AL1(RECXAG,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXDREQ+MIXIDDS,MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'FC03'                                                          
         DC    AL1(3,3,KEYAGY,KEYSYS,KEYSUB,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5111)                                                        
MXAGADDX EQU   *                                                                
*                                                                               
MXAGDIS  DC    AL1(MXAGDISX-MXAGDIS)                                            
         DC    AL1(RECXAG,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'FC03'                                                          
         DC    AL1(3,3,KEYAGY,KEYSYS,KEYSUB,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5112)                                                        
MXAGDISX EQU   *                                                                
*                                                                               
MXAGCHA  DC    AL1(MXAGCHAX-MXAGCHA)                                            
         DC    AL1(RECXAG,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'FC03'                                                          
         DC    AL1(3,3,KEYAGY,KEYSYS,KEYSUB,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5113)                                                        
MXAGCHAX EQU   *                                                                
*                                                                               
MXAGDEL  DC    AL1(MXAGDELX-MXAGDEL)                                            
         DC    AL1(RECXAG,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'FC03'                                                          
         DC    AL1(3,3,KEYAGY,KEYSYS,KEYSUB,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5114)                                                        
MXAGDELX EQU   *                                                                
*                                                                               
MXAGRES  DC    AL1(MXAGRESX-MXAGRES)                                            
         DC    AL1(RECXAG,ACTRES)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'FC03'                                                          
         DC    AL1(3,3,KEYAGY,KEYSYS,KEYSUB,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5115)                                                        
MXAGRESX EQU   *                                                                
*                                                                               
MXAGLST  DC    AL1(MXAGLSTX-MXAGLST)                                            
         DC    AL1(RECXAG,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'DC03'                                                          
         DC    AL1(3,3,KEYAGY,KEYSYS,KEYSUB,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5116)                                                        
MXAGLSTX EQU   *                                                                
*                                                                               
MXAGREP  DC    AL1(MXAGREPX-MXAGREP)                                            
         DC    AL1(RECXAG,ACTREP)                                               
         DC    AL1(MIXIREP,MIXIOKN+MIXIOKS+MIXIOKO+MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'BC03'                                                          
         DC    AL1(3,3,KEYAGY,KEYSYS,KEYSUB,0,0,0,0,0)                          
         DC    AL4(0,0)                                                         
         DC    CL8'CTEGEG'                                                      
         DC    AL2(5117)                                                        
MXAGREPX EQU   *                                                                
*                                                                               
MXAGSEL  DC    AL1(MXAGSELX-MXAGSEL)                                            
         DC    AL1(RECXAG,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'FC03'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5118)                                                        
MXAGSELX EQU   *                                                                
*                                                                               
MXFLDIS  DC    AL1(MXFLDISX-MXFLDIS)                                            
         DC    AL1(RECXFL,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'FB04'                                                          
         DC    AL1(5,5,KEYDAT,KEYTIM,KEYAGY,KEYSYS,KEYSUB,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5122)                                                        
MXFLDISX EQU   *                                                                
*                                                                               
MXFLRES  DC    AL1(MXFLRESX-MXFLRES)                                            
         DC    AL1(RECXFL,ACTRES)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'FB04'                                                          
         DC    AL1(5,5,KEYDAT,KEYTIM,KEYAGY,KEYSYS,KEYSUB,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5125)                                                        
MXFLRESX EQU   *                                                                
*                                                                               
MXFLLST  DC    AL1(MXFLLSTX-MXFLLST)                                            
         DC    AL1(RECXFL,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'DB04'                                                          
         DC    AL1(5,5,KEYDAT,KEYTIM,KEYAGY,KEYSYS,KEYSUB,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5126)                                                        
MXFLLSTX EQU   *                                                                
*                                                                               
MXFLTRN  DC    AL1(MXFLTRNX-MXFLTRN)                                            
         DC    AL1(RECXFL,ACTTRN)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'A004'                                                          
         DC    AL1(5,5,KEYDAT,KEYTIM,KEYAGY,KEYSYS,KEYSUB,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5126)                                                        
MXFLTRNX EQU   *                                                                
*                                                                               
MXFLREP  DC    AL1(MXFLREPX-MXFLREP)                                            
         DC    AL1(RECXFL,ACTREP)                                               
         DC    AL1(MIXIREP,MIXIOKN+MIXIOKS+MIXIOKO+MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'BB04'                                                          
         DC    AL1(5,5,KEYDAT,KEYTIM,KEYAGY,KEYSYS,KEYSUB,0,0,0)                
         DC    AL4(0,0)                                                         
         DC    CL8'CTEFEF'                                                      
         DC    AL2(5127)                                                        
MXFLREPX EQU   *                                                                
*                                                                               
MXFLSEL  DC    AL1(MXFLSELX-MXFLSEL)                                            
         DC    AL1(RECXFL,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'FB04'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5128)                                                        
MXFLSELX EQU   *                                                                
*                                                                               
MXLGDIS  DC    AL1(MXLGDISX-MXLGDIS)                                            
         DC    AL1(RECXLG,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'FA05'                                                          
         DC    AL1(1,1,KEYAGY,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5132)                                                        
MXLGDISX EQU   *                                                                
*                                                                               
MXLGLST  DC    AL1(MXLGLSTX-MXLGLST)                                            
         DC    AL1(RECXLG,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'DA05'                                                          
         DC    AL1(1,1,KEYAGY,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5136)                                                        
MXLGLSTX EQU   *                                                                
*                                                                               
MXLGREP  DC    AL1(MXLGREPX-MXLGREP)                                            
         DC    AL1(RECXLG,ACTREP)                                               
         DC    AL1(MIXIREP,MIXIOKN+MIXIOKS+MIXIOKO+MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'BA05'                                                          
         DC    AL1(1,1,KEYAGY,0,0,0,0,0,0,0)                                    
         DC    AL4(0,0)                                                         
         DC    CL8'CTELEL'                                                      
         DC    AL2(5137)                                                        
MXLGREPX EQU   *                                                                
*                                                                               
MXLGSEL  DC    AL1(MXLGSELX-MXLGSEL)                                            
         DC    AL1(RECXLG,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'FA05'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5138)                                                        
MXLGSELX EQU   *                                                                
*                                                                               
MXTRADD  DC    AL1(MXTRADDX-MXTRADD)                                            
         DC    AL1(RECXTR,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXDREQ+MIXIDDS,MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'F906'                                                          
         DC    AL1(4,4,KEYESSID,KEYAGY,KEYSYS,KEYSUB,0,0,0,0)                   
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5141)                                                        
MXTRADDX EQU   *                                                                
*                                                                               
MXTRDIS  DC    AL1(MXTRDISX-MXTRDIS)                                            
         DC    AL1(RECXTR,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F906'                                                          
         DC    AL1(4,4,KEYESSID,KEYAGY,KEYSYS,KEYSUB,0,0,0,0)                   
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5142)                                                        
MXTRDISX EQU   *                                                                
*                                                                               
MXTRCHA  DC    AL1(MXTRCHAX-MXTRCHA)                                            
         DC    AL1(RECXTR,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'F906'                                                          
         DC    AL1(4,4,KEYESSID,KEYAGY,KEYSYS,KEYSUB,0,0,0,0)                   
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5143)                                                        
MXTRCHAX EQU   *                                                                
*                                                                               
MXTRDEL  DC    AL1(MXTRDELX-MXTRDEL)                                            
         DC    AL1(RECXTR,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'F906'                                                          
         DC    AL1(4,4,KEYESSID,KEYAGY,KEYSYS,KEYSUB,0,0,0,0)                   
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5144)                                                        
MXTRDELX EQU   *                                                                
*                                                                               
MXTRRES  DC    AL1(MXTRRESX-MXTRRES)                                            
         DC    AL1(RECXTR,ACTRES)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'F906'                                                          
         DC    AL1(4,4,KEYESSID,KEYAGY,KEYSYS,KEYSUB,0,0,0,0)                   
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5145)                                                        
MXTRRESX EQU   *                                                                
*                                                                               
MXTRLST  DC    AL1(MXTRLSTX-MXTRLST)                                            
         DC    AL1(RECXTR,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'D906'                                                          
         DC    AL1(4,4,KEYESSID,KEYAGY,KEYSYS,KEYSUB,0,0,0,0)                   
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5146)                                                        
MXTRLSTX EQU   *                                                                
*                                                                               
MXTRREP  DC    AL1(MXTRREPX-MXTRREP)                                            
         DC    AL1(RECXTR,ACTREP)                                               
         DC    AL1(MIXIREP,MIXIOKN+MIXIOKS+MIXIOKO+MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'B906'                                                          
         DC    AL1(4,4,KEYESSID,KEYAGY,KEYSYS,KEYSUB,0,0,0,0)                   
         DC    AL4(0,0)                                                         
         DC    CL8'CTETET'                                                      
         DC    AL2(5147)                                                        
MXTRREPX EQU   *                                                                
*                                                                               
MXTRSEL  DC    AL1(MXTRSELX-MXTRSEL)                                            
         DC    AL1(RECXTR,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'F906'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5148)                                                        
MXTRSELX EQU   *                                                                
*                                                                               
MXAPADD  DC    AL1(MXAPADDX-MXAPADD)                                            
         DC    AL1(RECXAP,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXDREQ+MIXIDDS,MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'F807'                                                          
         DC    AL1(4,4,KEYESSID,KEYAGY,KEYSYS,KEYSUB,0,0,0,0)                   
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5151)                                                        
MXAPADDX EQU   *                                                                
*                                                                               
MXAPDIS  DC    AL1(MXAPDISX-MXAPDIS)                                            
         DC    AL1(RECXAP,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F807'                                                          
         DC    AL1(4,4,KEYESSID,KEYAGY,KEYSYS,KEYSUB,0,0,0,0)                   
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5152)                                                        
MXAPDISX EQU   *                                                                
*                                                                               
MXAPCHA  DC    AL1(MXAPCHAX-MXAPCHA)                                            
         DC    AL1(RECXAP,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'F807'                                                          
         DC    AL1(4,4,KEYESSID,KEYAGY,KEYSYS,KEYSUB,0,0,0,0)                   
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5153)                                                        
MXAPCHAX EQU   *                                                                
*                                                                               
MXAPDEL  DC    AL1(MXAPDELX-MXAPDEL)                                            
         DC    AL1(RECXAP,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'F807'                                                          
         DC    AL1(4,4,KEYESSID,KEYAGY,KEYSYS,KEYSUB,0,0,0,0)                   
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5154)                                                        
MXAPDELX EQU   *                                                                
*                                                                               
MXAPRES  DC    AL1(MXAPRESX-MXAPRES)                                            
         DC    AL1(RECXAP,ACTRES)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'F807'                                                          
         DC    AL1(4,4,KEYESSID,KEYAGY,KEYSYS,KEYSUB,0,0,0,0)                   
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5155)                                                        
MXAPRESX EQU   *                                                                
*                                                                               
MXAPLST  DC    AL1(MXAPLSTX-MXAPLST)                                            
         DC    AL1(RECXAP,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'D807'                                                          
         DC    AL1(4,4,KEYESSID,KEYAGY,KEYSYS,KEYSUB,0,0,0,0)                   
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5156)                                                        
MXAPLSTX EQU   *                                                                
*                                                                               
MXAPREP  DC    AL1(MXAPREPX-MXAPREP)                                            
         DC    AL1(RECXAP,ACTREP)                                               
         DC    AL1(MIXIREP,MIXIOKN+MIXIOKS+MIXIOKO+MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'B807'                                                          
         DC    AL1(4,4,KEYESSID,KEYAGY,KEYSYS,KEYSUB,0,0,0,0)                   
         DC    AL4(0,0)                                                         
         DC    CL8'CTEPEP'                                                      
         DC    AL2(5157)                                                        
MXAPREPX EQU   *                                                                
*                                                                               
MXAPSEL  DC    AL1(MXAPSELX-MXAPSEL)                                            
         DC    AL1(RECXAP,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'F807'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5158)                                                        
MXAPSELX EQU   *                                                                
*                                                                               
MREFADD  DC    AL1(MREFADDX-MREFADD)                                            
         DC    AL1(RECREF,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXDREQ+MIXIDDS,MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'F708'                                                          
         DC    AL1(2,2,KEYAGY,KEYREFID,0,0,0,0,0,0)                             
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5161)                                                        
MREFADDX EQU   *                                                                
*                                                                               
MREFDIS  DC    AL1(MREFDISX-MREFDIS)                                            
         DC    AL1(RECREF,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F708'                                                          
         DC    AL1(2,2,KEYAGY,KEYREFID,0,0,0,0,0,0)                             
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5162)                                                        
MREFDISX EQU   *                                                                
*                                                                               
MREFCHA  DC    AL1(MREFCHAX-MREFCHA)                                            
         DC    AL1(RECREF,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'F708'                                                          
         DC    AL1(2,2,KEYAGY,KEYREFID,0,0,0,0,0,0)                             
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5163)                                                        
MREFCHAX EQU   *                                                                
*                                                                               
MREFDEL  DC    AL1(MREFDELX-MREFDEL)                                            
         DC    AL1(RECREF,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'F708'                                                          
         DC    AL1(2,2,KEYAGY,KEYREFID,0,0,0,0,0,0)                             
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5164)                                                        
MREFDELX EQU   *                                                                
*                                                                               
MREFRES  DC    AL1(MREFRESX-MREFRES)                                            
         DC    AL1(RECREF,ACTRES)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'F708'                                                          
         DC    AL1(2,2,KEYAGY,KEYREFID,0,0,0,0,0,0)                             
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5165)                                                        
MREFRESX EQU   *                                                                
*                                                                               
MREFLST  DC    AL1(MREFLSTX-MREFLST)                                            
         DC    AL1(RECREF,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'D708'                                                          
         DC    AL1(2,2,KEYAGY,KEYREFID,0,0,0,0,0,0)                             
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5166)                                                        
MREFLSTX EQU   *                                                                
*                                                                               
MREFREP  DC    AL1(MREFREPX-MREFREP)                                            
         DC    AL1(RECREF,ACTREP)                                               
         DC    AL1(MIXIREP,MIXIOKN+MIXIOKS+MIXIOKO+MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'B708'                                                          
         DC    AL1(2,2,KEYAGY,KEYREFID,0,0,0,0,0,0)                             
         DC    AL4(0,0)                                                         
         DC    CL8'CTERER'                                                      
         DC    AL2(5167)                                                        
MREFREPX EQU   *                                                                
*                                                                               
MREFSEL  DC    AL1(MREFSELX-MREFSEL)                                            
         DC    AL1(RECREF,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'F708'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5168)                                                        
MREFSELX EQU   *                                                                
*                                                                               
MBDEADD  DC    AL1(MBDEADDX-MBDEADD)                                            
         DC    AL1(RECBDE,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXDREQ+MIXIDDS,MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'F609'                                                          
         DC    AL1(2,2,KEYAGY,KEYBDEID,0,0,0,0,0,0)                             
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5171)                                                        
MBDEADDX EQU   *                                                                
*                                                                               
MBDEDIS  DC    AL1(MBDEDISX-MBDEDIS)                                            
         DC    AL1(RECBDE,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F609'                                                          
         DC    AL1(2,2,KEYAGY,KEYBDEID,0,0,0,0,0,0)                             
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5172)                                                        
MBDEDISX EQU   *                                                                
*                                                                               
MBDECHA  DC    AL1(MBDECHAX-MBDECHA)                                            
         DC    AL1(RECBDE,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'F609'                                                          
         DC    AL1(2,2,KEYAGY,KEYBDEID,0,0,0,0,0,0)                             
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5173)                                                        
MBDECHAX EQU   *                                                                
*                                                                               
MBDEDEL  DC    AL1(MBDEDELX-MBDEDEL)                                            
         DC    AL1(RECBDE,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'F609'                                                          
         DC    AL1(2,2,KEYAGY,KEYBDEID,0,0,0,0,0,0)                             
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5174)                                                        
MBDEDELX EQU   *                                                                
*                                                                               
MBDERES  DC    AL1(MBDERESX-MBDERES)                                            
         DC    AL1(RECBDE,ACTRES)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'F609'                                                          
         DC    AL1(2,2,KEYAGY,KEYBDEID,0,0,0,0,0,0)                             
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5175)                                                        
MBDERESX EQU   *                                                                
*                                                                               
MBDELST  DC    AL1(MBDELSTX-MBDELST)                                            
         DC    AL1(RECBDE,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'D609'                                                          
         DC    AL1(2,2,KEYAGY,KEYBDEID,0,0,0,0,0,0)                             
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5176)                                                        
MBDELSTX EQU   *                                                                
*                                                                               
MBDESEL  DC    AL1(MBDESELX-MBDESEL)                                            
         DC    AL1(RECBDE,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'F609'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5178)                                                        
MBDESELX EQU   *                                                                
*                                                                               
MXLIADD  DC    AL1(MXLIADDX-MXLIADD)                                            
         DC    AL1(RECXLI,ACTADD)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXDREQ+MIXIDDS,MIXITXT)                     
         DC    AL1(0,0)                                                         
         DC    X'F50A'                                                          
         DC    AL1(2,2,KEYAGY,KEYSYS,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5181)                                                        
MXLIADDX EQU   *                                                                
*                                                                               
MXLIDIS  DC    AL1(MXLIDISX-MXLIDIS)                                            
         DC    AL1(RECXLI,ACTDIS)                                               
         DC    AL1(MIXILFM+MIXKREQ,MIXITXT)                                     
         DC    AL1(0,0)                                                         
         DC    X'F50A'                                                          
         DC    AL1(2,2,KEYAGY,KEYSYS,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5182)                                                        
MXLIDISX EQU   *                                                                
*                                                                               
MXLICHA  DC    AL1(MXLICHAX-MXLICHA)                                            
         DC    AL1(RECXLI,ACTCHA)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'F50A'                                                          
         DC    AL1(2,2,KEYAGY,KEYSYS,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5183)                                                        
MXLICHAX EQU   *                                                                
*                                                                               
MXLIDEL  DC    AL1(MXLIDELX-MXLIDEL)                                            
         DC    AL1(RECXLI,ACTDEL)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'F50A'                                                          
         DC    AL1(2,2,KEYAGY,KEYSYS,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5184)                                                        
MXLIDELX EQU   *                                                                
*                                                                               
MXLIRES  DC    AL1(MXLIRESX-MXLIRES)                                            
         DC    AL1(RECXLI,ACTRES)                                               
         DC    AL1(MIXILFM+MIXKREQ+MIXIDDS,MIXITXT)                             
         DC    AL1(0,0)                                                         
         DC    X'F50A'                                                          
         DC    AL1(2,2,KEYAGY,KEYSYS,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5185)                                                        
MXLIRESX EQU   *                                                                
*                                                                               
MXLILST  DC    AL1(MXLILSTX-MXLILST)                                            
         DC    AL1(RECXLI,ACTLST)                                               
         DC    AL1(MIXILST,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'D50A'                                                          
         DC    AL1(2,2,KEYAGY,KEYSYS,0,0,0,0,0,0)                               
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5186)                                                        
MXLILSTX EQU   *                                                                
*                                                                               
MXLISEL  DC    AL1(MXLISELX-MXLISEL)                                            
         DC    AL1(RECXLI,ACTSEL)                                               
         DC    AL1(MIXISEL,MIXITXT)                                             
         DC    AL1(0,0)                                                         
         DC    X'F50A'                                                          
         DC    AL1(0,0,0,0,0,0,0,0,0,0)                                         
         DC    AL4(0,0)                                                         
         DC    XL8'00'                                                          
         DC    AL2(5188)                                                        
MXLISELX EQU   *                                                                
*                                                                               
MIXTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* KEY COMPONENT TABLE (SEE KEYTABD)                                   *         
***********************************************************************         
         SPACE 1                                                                
KEYTAB   DS    0X                                                               
*                                                                               
         DC    C'SYSTEM ',AL1(KEYSYS)                                           
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
         DC    C'PERIOD ',AL1(KEYPER)                                           
         DC    C'NAME   ',AL1(KEYNAM)                                           
         DC    C'CODE   ',AL1(KEYCODE)                                          
         DC    C'ALPHAID',AL1(KEYACC)                                           
         DC    C'LISTTYP',AL1(KEYLTYP)                                          
         DC    C'APPLID ',AL1(KEYAPID)                                          
         DC    C'DEPT   ',AL1(KEYDEP)                                           
         DC    C'LEVEL  ',AL1(KEYLEV)                                           
         DC    C'REFORM ',AL1(KEYREFID)                                         
         DC    C'BDEID  ',AL1(KEYBDEID)                                         
*                                                                               
KEYTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* OPTION TABLE (SEE OPTTABD)                                          *         
***********************************************************************         
         SPACE 1                                                                
OPTTAB   DS    0X                  ** OPTIONS TABLE **                          
*                                                                               
*                                  RESET OPTION, ON XTRAN RECORD                
RSTOPT1  DC    AL1(RSTOPT1X-RSTOPT1)                                            
         DC    C'RESET   '                                                      
         DC    C'   ',AL1(OPTIDDS+OPTNRTN+OPTITXT,0)                            
         DC    AL1(0,0)                                                         
         DC    AL1(RECXTR,0)                                                    
         DC    AL1(3,1,1,L'OPTRST)                                              
         DC    AL4(OPTRSTB)                                                     
         DC    AL1(OPTRSTN)                                                     
         DC    AL2(OPTRSTR)                                                     
         DC    AL2(OPTRST-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(0,0)                                                         
RSTOPT1X EQU   *                                                                
*                                  RESET OPTION, ON XAGY RECORD                 
RSTOPT2  DC    AL1(RSTOPT2X-RSTOPT2)                                            
         DC    C'RESET   '                                                      
         DC    C'   ',AL1(OPTIDDS+OPTNRTN+OPTITXT,0)                            
         DC    AL1(0,0)                                                         
         DC    AL1(RECXAG,0)                                                    
         DC    AL1(3,1,1,L'OPTRST)                                              
         DC    AL4(OPTRSTB)                                                     
         DC    AL1(OPTRSTN)                                                     
         DC    AL2(OPTRSTR)                                                     
         DC    AL2(OPTRST-WORKD)                                                
         DC    XL4'00'                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL2(0,0)                                                         
RSTOPT2X EQU   *                                                                
*                                                                               
*                                                                               
OPTTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* SELECTABLE ACTION TABLE (SEE SELTABD)                               *         
***********************************************************************         
         SPACE 1                                                                
SELTAB   DS    0X                                                               
*                                                                               
         DC    C'S',AL1(RECESS,ACTLST,RECESS,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECESS,ACTLST,RECESS,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&UK                                                                           
         DC    C'D',AL1(RECESS,ACTLST,RECESS,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&                                                                             
*                                                                               
         DC    C'S',AL1(RECXAG,ACTLST,RECXAG,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECXAG,ACTLST,RECXAG,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&UK                                                                           
         DC    C'D',AL1(RECXAG,ACTLST,RECXAG,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&                                                                             
*                                                                               
         DC    C'S',AL1(RECXFL,ACTLST,RECXFL,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECXFL,ACTLST,RECXFL,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'S',AL1(RECXFL,ACTTRN,RECXFL,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'F',AL1(RECXFL,ACTTRN,RECXFL,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*                                                                               
         DC    C'S',AL1(RECXLG,ACTLST,RECXLG,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECXLG,ACTLST,RECXLG,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&UK                                                                           
         DC    C'D',AL1(RECXLG,ACTLST,RECXLG,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&                                                                             
*                                                                               
         DC    C'S',AL1(RECXTR,ACTLST,RECXTR,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECXTR,ACTLST,RECXTR,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&UK                                                                           
         DC    C'D',AL1(RECXTR,ACTLST,RECXTR,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&                                                                             
*                                                                               
         DC    C'S',AL1(RECXAP,ACTLST,RECXAP,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECXAP,ACTLST,RECXAP,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&UK                                                                           
         DC    C'D',AL1(RECXAP,ACTLST,RECXAP,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&                                                                             
*                                                                               
         DC    C'S',AL1(RECREF,ACTLST,RECREF,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECREF,ACTLST,RECREF,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&UK                                                                           
         DC    C'D',AL1(RECREF,ACTLST,RECREF,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&                                                                             
*                                                                               
         DC    C'S',AL1(RECBDE,ACTLST,RECBDE,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECBDE,ACTLST,RECBDE,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&UK                                                                           
         DC    C'D',AL1(RECBDE,ACTLST,RECBDE,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&                                                                             
*                                                                               
         DC    C'S',AL1(RECXLI,ACTLST,RECXLI,ACTDIS)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
         DC    C'C',AL1(RECXLI,ACTLST,RECXLI,ACTCHA)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&UK                                                                           
         DC    C'D',AL1(RECXLI,ACTLST,RECXLI,ACTDEL)                            
         DC    XL8'00'                                                          
         DC    XL11'00'                                                         
*&&                                                                             
*                                                                               
SELTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
* CTESSWRK                                                                      
         SPACE 1                                                                
       ++INCLUDE CTESSWRK                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017CTESS01   11/04/08'                                      
         END                                                                    

*          DATA SET REVIEW00   AT LEVEL 006 AS OF 08/31/00                      
*          DATA SET REVIEW00   AT LEVEL 005 AS OF 09/08/99                      
*PHASE T81700A                                                                  
*INCLUDE GEFILSET                                                               
*INCLUDE REVEWINI                                                               
REVIEW00 TITLE 'T81700 - REPORT VIEWER ROOT PHASE'                              
VEW00    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKX-WORKD,REVIEW00,CLEAR=YES,RR=R3                             
         LR    R9,RC                                                            
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
*                                                                               
INIT     L     RF,=V(GEFILSET)                                                  
         LA    RF,0(R3,RF)                                                      
         GOTO1 (RF),BCPARM                                                      
         L     R8,AGWORK                                                        
         USING GWORKD,R8           R8=A(PROGRAM W/S)                            
*                                                                               
         L     RF,=V(REVEWINI)                                                  
         LA    RF,0(R3,RF)                                                      
         GOTO1 (RF),BCPARM,ADDRS2,TWDICTL-TWDICT,C'FIL '                        
         L     RA,ATWA                                                          
         USING TWAD,RA             RA=A(TWA)                                    
         BL    INIT50                                                           
         BH    *+8                                                              
         OI    BCINDS1,BCIFRST     ON=NOT FIRST TIME                            
*                                                                               
         GOTO1 VCOLY,BCPARM,(X'01',0),0                                         
         CLI   4(R1),X'FF'         SET REVIEW01 ROUTINE ADDRESSES               
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         LA    R0,GLROUTSN                                                      
         XR    RE,RE                                                            
         LA    R1,GLROUTS                                                       
INIT04   ST    RF,0(R1)                                                         
         STC   RE,0(R1)                                                         
         LA    RE,1(RE)                                                         
         LA    R1,4(R1)                                                         
         BCT   R0,INIT04                                                        
*                                                                               
         MVC   GSDSPMAX,=Y(TWSAVE-TWAD)    END OF TWA SAVE AREA                 
         MVC   GSDSPREC,=Y(BASRECH-TWAD)   RECORD FIELD                         
         MVC   GSDSPSAV,=Y(BASOLY1H-TWAD)                                       
         MVC   GSDSPOVR,=Y(BASOLY1H-TWAD)                                       
         MVC   GSDSPKEY,=Y(BASKEYH-TWAD)                                        
         MVI   GSKEYSEP,C','                                                    
         MVI   GSKEYEQU,X'FE'                                                   
*                                                                               
         LA    RF,NTRRET           SET NTRSES RETURN POINTS                     
         ST    RF,GCANTR                                                        
         LA    RF,XITRET                                                        
         ST    RF,GCAXIT                                                        
         LA    RF,EXITRET                                                       
         ST    RF,GCAEXIT                                                       
*                                                                               
         TM    BCINDS1,BCIFRST     TEST FIRST TIME                              
         BZ    INIT06                                                           
         GOTOX ('RESVAL',AGROUTS)  RESTORE SAVED VALUES                         
         XC    NSSAV,NSSAV         ??                                           
*                                                                               
INIT06   MVI   GCSWSYSN,QSREP      DEFAULT IS REP SYSTEM                        
         MVI   GCSWSYSP,QSREP                                                   
*                                                                               
         OC    GSSYS,GSSYS         ALREADY GOT A SYSTEM SET UP?                 
         BNZ   *+8                                                              
         MVI   GSSYS,QSREP         SET REP AS DEFAULT                           
         MVC   GCFILNAM,REPVIEW    SET REPFILE AS DEFAULT FILE NAME             
*                                                                               
         MVI   GCOVSYS,QSREP       FASYSTAB ENTRY FOR LOGON SYSTEM              
         MVI   GCPRGNO,X'17'       FAPGMTAB ENTRY FOR THIS PROGRAM              
         GOTOX ('SWCHFC',AGROUTS),GSSYS MAKE SURE SWITCH SYSTEM                 
*                                                                               
         OC    CSOVER,CSOVER       LOAD IN PREV. OVERLAY (???????)              
         BZ    INIT08                                                           
         GOTOX ('OVRLAY',AGROUTS),CSOVER                                        
*                                                                               
INIT08   CLI   BASRECH+FHILD,0     RECORD SET?                                  
         BNE   INIT10              NO                                           
         TM    BCINDS1,BCIANYPF                                                 
         BNZ   INIT10                                                           
         MVI   CSREC,X'10'         SET DEFAULT MENU SCREEN                      
         GOTOX AGEN,BCPARM,ORTYPE,RTSET,(CSREC,0),0                             
         GOTOX AGEN,BCPARM,ORTYPE,RTDIS                                         
         BE    INIT30                                                           
         DC    H'0'                SERIOUS ERROR                                
*                                                                               
INIT10   GOTOX AGEN,BCPARM,OPFK,PFVAL                                           
         BNE   INIT50                                                           
*                                                                               
INIT20   GOTOX AGEN,BCPARM,ORTYPE,RTVAL                                         
         BE    INIT30                                                           
         MVC   FVMSGNO,=AL2(GE$INREC)                                           
         B     INIT50                                                           
*                                                                               
INIT30   MVI   CSACT,A#LST         ALL ACTIONS ARE LIST FOR VIEWER              
         GOTOX AGEN,BCPARM,OACT,ASET,(CSACT,0),0                                
         BE    *+6                                                              
         DC    H'0'                SERIOUS ERROR                                
*                                                                               
INIT40   GOTOX AGEN,BCPARM,OACT,APRC                                            
*                                                                               
INIT50   GOTOX ('SETMSG',AGROUTS)                                               
         GOTOX AGEN,BCPARM,OPFK,PFBLD                                           
         GOTOX RANKDIS                                                          
         GOTOX ('SAVVAL',AGROUTS)                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* NTRSES CALL RETURN POINT                                            *         
***********************************************************************         
         SPACE 1                                                                
NTRRET   B     INIT40                                                           
         SPACE 1                                                                
***********************************************************************         
* XITSES CALL RETURN POINT                                            *         
***********************************************************************         
         SPACE 1                                                                
XITRET   B     INIT20                                                           
         SPACE 1                                                                
***********************************************************************         
* EXIT CALL RETURN POINT                                              *         
***********************************************************************         
         SPACE 1                                                                
EXITRET  B     INIT50                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY 'RANKED ON' IN PFKEY FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
RANKDIS  NTR1  ,                                                                
         OC    CSREC,CSREC                                                      
         BZ    RNKDX                                                            
*                                                                               
         LA    R2,TWAPOLY          LOCATE PFKEY DISPLAY LINE IN TWA             
         USING FHD,R2                                                           
         LH    RF,GSDSPMAX                                                      
         A     RF,ATWA                                                          
         BCTR  RF,0                RF=A(LAST POSSIBLE BYTE IN TWA)              
         XR    RE,RE                                                            
*                                                                               
RNKD02   TM    FHAT,FHATXH                                                      
         BZ    RNKD04                                                           
         IC    RE,FHLN                                                          
         LA    R1,FHD(RE)                                                       
         SH    R1,=Y(FHDAD)        R1=A(EXTENDED FIELD HEADER)                  
         CLI   0(R1),FD#PFK                                                     
         BE    RNKD06                                                           
*                                                                               
RNKD04   ICM   RE,1,FHLN                                                        
         BZ    RNKDX                                                            
         BXLE  R2,RE,RNKD02                                                     
         B     RNKDX                                                            
*                                                                               
         USING PFKFLDD,R2                                                       
RNKD06   LA    R3,PFKFLD+L'PFKFLD-L'RANKED-9                                    
         LA    RF,RANKTAB                                                       
*                                                                               
RNKD08   CLI   0(RF),EOT                                                        
         BE    RNKDX                                                            
         CLC   CSREC,0(RF)                                                      
         BE    *+12                                                             
         LA    RF,9(RF)                                                         
         B     RNKD08                                                           
*                                                                               
         MVC   0(L'RANKED,R3),RANKED                                            
         LA    R3,L'RANKED(R3)                                                  
         MVC   0(8,R3),1(RF)                                                    
         B     RNKDX                                                            
*                                                                               
RNKDX    XIT1  ,                                                                
*                                                                               
RANKTAB  DC    X'13',CL8'CurrBlg'                                               
         DC    X'14',CL8'CurrBlg'                                               
*        DC    X'15',CL8'CurrBlg'                                               
         DC    X'16',CL8'CurrBlg'                                               
*        DC    X'17',CL8'Alpha'                                                 
*        DC    X'18',CL8'Alpha'                                                 
         DC    X'19',CL8'PriFin'                                                
         DC    X'1A',CL8'PriFin'                                                
         DC    X'1B',CL8'PriFin'                                                
         DC    X'1C',CL8'CurrBlg'                                               
         DC    X'1D',CL8'CurrBlg'                                               
         DC    X'20',CL8'PriFin'                                                
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
DD#DISK  EQU   32768                                                            
         SPACE 1                                                                
         LTORG                                                                  
REPVIEW  DC    CL8'REPVIEW'                                                     
RANKED   DC    C'Ranked On '                                                    
         SPACE 1                                                                
         EJECT                                                                  
DICUPR   DS    0XL4                ** UPPER CASE DICTIONARY LIST **             
DICUPRX  DC    AL1(EOT)            512 BYTES MAX                                
*                                                                               
DICMIX   DS    0XL4                ** MIXED CASE DICTIONARY LIST **             
DICMIXX  DC    AL1(EOT)            512 BYTES MAX                                
*                                                                               
         EJECT                                                                  
ADDRS2   DS    0F                  ** CONTROLLER ADDRESSES 2 **                 
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(DICUPR)                                                        
         DC    A(DICMIX)                                                        
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(0)                                                             
         EJECT                                                                  
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
         SPACE 1                                                                
* FATCB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATCB                                                          
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDGETHELPD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGETHELPD                                                     
         PRINT ON                                                               
         EJECT                                                                  
* REVIEWWRK                                                                     
       ++INCLUDE REVIEWWRK                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006REVIEW00  08/31/00'                                      
         END                                                                    

*          DATA SET RENFI00    AT LEVEL 015 AS OF 08/31/00                      
*          DATA SET RENFI00    AT LEVEL 014 AS OF 07/01/98                      
*&&      SET   NOP=N                                                            
*PHASE T83100A                                                                  
*INCLUDE RENFIINI                                                               
*INCLUDE GEFILSET                                                               
FIL00    TITLE '- NEW FILE PROGRAM ROOT (REP SYSTEM)'                           
FIL00    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKX-WORKD,RENFI00*,R6,CLEAR=YES,RR=R3                          
         LR    R9,RC                                                            
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
*                                                                               
INIT     L     RF,=V(GEFILSET)                                                  
         LA    RF,0(R3,RF)                                                      
         GOTO1 (RF),BCPARM,ADDRS2,TWDICTL-TWDICT,C'FIL '                        
         L     R8,AGWORK                                                        
         USING GWORKD,R8           R8=A(FILE PROGRAM W/S)                       
*                                                                               
         L     RF,=V(RENFIINI)                                                  
         LA    RF,0(R3,RF)                                                      
         GOTO1 (RF),BCPARM,ADDRS2,TWDICTL-TWDICT,C'FIL '                        
         L     RA,ATWA                                                          
         USING TWAD,RA             RA=A(TWA)                                    
         BL    INIT50                                                           
         BH    *+8                                                              
         OI    BCINDS1,BCIFRST     ON=NOT FIRST TIME                            
*                                                                               
         L     R1,AREP                                                          
         USING REPD,R1                                                          
         MVC   REPAPQB,ATIA        ??????                                       
         MVC   REPACOM,ACOM        INITIALIZE REPBLK VALUES                     
         LA    R0,REPHS                                                         
         ST    R0,REPABUF                                                       
         MVI   REPHEADN,REPHN                                                   
         MVI   REPMIDSN,REPMN                                                   
         MVI   REPPRNTN,REPPN                                                   
         MVI   REPFOOTN,REPFN                                                   
         OI    REPIND2,REPILOW                                                  
         OI    REPHEADI,REPHSPAC+REPHCLRA                                       
         OI    REPMIDSI,REPMSPAC+REPMCLRA                                       
         MVC   REPDATE,ASBDAT                                                   
         MVC   REPSYSID,=C'RE'                                                  
         MVC   REPPRGID,=C'NF'                                                  
         MVC   REPRLH,=Y(48)                                                    
         MVC   REPRDH,=Y(12)                                                    
*                                                                               
         ICM   R0,15,=X'D9000A6C'                                               
         LA    R1,BCPARM                                                        
         GOTO1 VCOLY,BCPARM,0,(R0)                                              
         CLI   4(R1),X'FF'         SET T00A6C ROUTINE ADDRESSES                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         LA    R0,AROUT6CN                                                      
         XR    RE,RE                                                            
         LA    R1,AROUT6C                                                       
INIT04   ST    RF,0(R1)                                                         
         STC   RE,0(R1)                                                         
         LA    RE,1(RE)                                                         
         LA    R1,4(R1)                                                         
         BCT   R0,INIT04                                                        
*                                                                               
         TM    BCINDS1,BCIFRST     TEST FIRST TIME                              
         BO    INIT05                                                           
*                                                                               
         MVC   GSDSPMAX,=Y(TWSAVE-TWAD)                                         
         MVC   GSDSPREC,=Y(BASRECH-TWAD)                                        
         MVC   GSDSPACT,=Y(BASACTH-TWAD)                                        
         MVC   GSDSPSCR,=Y(BASSCRH-TWAD)                                        
         MVC   GSDSPPAG,=Y(BASPAGH-TWAD)                                        
*        MVC   GSDSPREP,=Y(BASREPH-TWAD)                                        
         MVC   GSDSPSAV,=Y(BASOPTH-TWAD)                                        
         MVC   GSDSPOPT,=Y(BASOPTH-TWAD)                                        
         MVC   GSDSPOVR,=Y(BASOLY1H-TWAD)                                       
*                                                                               
INIT05   LA    RF,NTRRET           SET NTRSES RETURN POINTS                     
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
INIT06   MVI   GCSWSYSN,QSREP      DEFAULT IS SPT SYSTEM                        
         MVI   GCSWSYSC,QSREP                                                   
         MVI   GCSWSYSP,QSREP                                                   
*                                                                               
         OC    GSSYS,GSSYS         ALREADY GOT A SYSTEM SET UP?                 
         BNZ   *+8                                                              
         MVI   GSSYS,QSREP         SET SPT AS DEFAULT                           
         MVC   GCFILNAM,REPFILE    SET SPTFILE AS DEFAULT FILE NAME             
         MVI   GCOVSYS,QSREP       FASYSTAB ENTRY FOR LOGON SYSTEM              
         MVI   GCPRGNO,X'31'       FAPGMTAB ENTRY FOR THIS PROGRAM              
*                                                                               
         GOTOX ('SWCHFC',AGROUTS),=AL1(QSCON)  SWITCH TO CONTROL SYS            
         BE    INIT07                                                           
                                                                                
         MVC   BASMSG(28),=CL28'ERROR: NO CONTROL ACCESS YET'                   
         OI    BASMSGH+6,X'80'                                                  
         OI    BASRECH+1,X'20'     CHANGE TO PROTECTED                          
         OI    BASACTH+1,X'20'                                                  
         OI    BASRECH+6,X'80'                                                  
         OI    BASACTH+6,X'80'                                                  
         B     EXIT                                                             
*                                                                               
INIT07   GOTOX ('SWCHFC',AGROUTS),GSSYS MAKE SURE SWITCH SYSTEM BACK            
*                                                                               
         CLI   CSREC,0             LOAD IN PREV. OVERLAY (???????)              
         BE    INIT08                                                           
         GOTOX ('OVRLAY',AGROUTS),CSREC                                         
*                                                                               
INIT08   CLI   BASRECH+FHILD,0                                                  
         BNE   INIT10                                                           
         CLI   BASACTH+FHILD,0                                                  
         BNE   INIT10                                                           
         TM    BCINDS1,BCIANYPF                                                 
         BNZ   INIT10                                                           
         LA    R0,BASRECH          NO RECORD/ACTION OR PFKEY THIS TIME          
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(GI$EFAR)                                            
         MVI   FVOMTYP,GTMINF                                                   
         MVI   CSACT,0                                                          
         B     INIT50                                                           
*                                                                               
INIT10   DS    0H                                                               
         GOTOX AGEN2,BCPARM,OPFK,PFVAL                                          
         BNE   INIT50                                                           
*                                                                               
INIT20   GOTOX AGEN,BCPARM,ORTYPE,RTVAL                                         
         BNE   INIT50                                                           
*                                                                               
INIT30   GOTOX AGEN,BCPARM,OACT,AVAL                                            
         BNE   INIT50                                                           
*                                                                               
INIT40   GOTOX AGEN,BCPARM,OACT,APRC                                            
*                                                                               
INIT50   DS    0H                                                               
         GOTOX ('SETMSG',AGROUTS)                                               
         GOTOX AGEN,BCPARM,OPFK,PFBLD                                           
         GOTOX ('SAVVAL',AGROUTS)                                               
*                                                                               
         EJECT                                                                  
EXIT     CLI   ASONOFF,ASOFF     RUNNING OFFLINE?                               
         BNE   *+14              NO                                             
         L     RF,ASECBLK        SPOOF REDEFINES TWAUSER - SAVE IT              
         MVC   0(L'TWAUSER,RF),TWAUSER                                          
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* NTRSES CALL RETURN POINT                                            *         
***********************************************************************         
         SPACE 1                                                                
NTRRET   DS    0H                                                               
         B     INIT40                                                           
         SPACE 1                                                                
***********************************************************************         
* XITSES CALL RETURN POINT                                            *         
***********************************************************************         
         SPACE 1                                                                
XITRET   B     INIT20              ??                                           
         SPACE 1                                                                
***********************************************************************         
* EXIT CALL RETURN POINT                                              *         
***********************************************************************         
         SPACE 1                                                                
EXITRET  DS    0H                                                               
         B     INIT50                                                           
         EJECT                                                                  
FF       EQU   X'FF'                                                            
         SPACE 1                                                                
         LTORG                                                                  
REPFILE  DC    CL8'REPFILE'                                                     
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
         SPACE 1                                                                
FIL00    CSECT                                                                  
         DS    0D                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FASECRETD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASECRETD                                                      
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
* RENFIWORK                                                                     
       ++INCLUDE RENFIWORK                                                      
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015RENFI00   08/31/00'                                      
         END                                                                    

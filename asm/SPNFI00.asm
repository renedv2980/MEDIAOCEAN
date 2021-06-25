*          DATA SET SPNFI00    AT LEVEL 014 AS OF 04/10/01                      
*PHASE T22700A                                                                  
*INCLUDE SPNFIINI                                                               
*INCLUDE GEFILSET                                                               
         TITLE '- NEW FILE PROGRAM ROOT (SPOT SYSTEM)'                          
FIL00    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKX-WORKD,**FL00**,R6,CLEAR=YES,RR=R3                          
         LR    R9,RC                                                            
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
*                                                                               
         L     RF,=V(GEFILSET)                                                  
         AR    RF,R3                                                            
         GOTO1 (RF),BCPARM,ADDRS2,TWDICTL-TWDICT,C'FIL '                        
         L     R8,AGWORK                                                        
         USING GWORKD,R8           R8=A(FILE PROGRAM W/S)                       
*                                                                               
         L     RF,=V(SPNFIINI)                                                  
         AR    RF,R3                                                            
         GOTO1 (RF),BCPARM,ADDRS2,TWDICTL-TWDICT,C'FIL '                        
         L     RA,ATWA                                                          
         USING TWAD,RA             RA=A(TWA)                                    
         BL    INI20                                                            
         BH    *+8                                                              
         OI    BCINDS1,BCIFRST     ON=NOT FIRST TIME                            
*                                                                               
         BAS   RE,PRTINI           INITIALISE PRINT BLOCK                       
*                                                                               
         ICM   R0,15,=X'D9000A6C'                                               
         LA    R1,BCPARM                                                        
         GOTO1 VCOLY,BCPARM,0,(R0)                                              
         CLI   4(R1),X'FF'         SET T00A6C ROUTINE ADDRESSES                 
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,0(R1)                                                         
         LA    R0,AROUT6CN                                                      
         XR    RE,RE                                                            
         LA    R1,AROUT6C                                                       
*                                                                               
INI02    ST    RF,0(R1)                                                         
         STC   RE,0(R1)                                                         
         LA    RE,1(RE)                                                         
         LA    R1,4(R1)                                                         
         BCT   R0,INI02                                                         
*                                                                               
         TM    BCINDS1,BCIFRST     TEST FIRST TIME                              
         BO    INI04                                                            
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
INI04    LA    RF,NTRRET           SET NTRSES RETURN POINTS                     
         ST    RF,GCANTR                                                        
         LA    RF,XITRET                                                        
         ST    RF,GCAXIT                                                        
         LA    RF,EXITRET                                                       
         ST    RF,GCAEXIT                                                       
*                                                                               
         TM    BCINDS1,BCIFRST     TEST FIRST TIME                              
         BZ    INI06                                                            
         GOTOX ('RESVAL',AGROUTS)  RESTORE SAVED VALUES                         
         XC    NSSAV,NSSAV                                                      
*                                                                               
INI06    MVI   GCSWSYSN,QSSPT      DEFAULT IS SPT SYSTEM                        
         MVI   GCSWSYSC,QSSPT                                                   
         MVI   GCSWSYSP,QSSPT                                                   
*                                                                               
         OC    GSSYS,GSSYS         ALREADY GOT A SYSTEM SET UP?                 
         BNZ   *+8                                                              
         MVI   GSSYS,QSSPT         SET SPT AS DEFAULT                           
*                                                                               
         MVC   GCFILNAM,SPTFILE    SET SPTFILE AS DEFAULT FILE NAME             
         MVI   GCOVSYS,QSSPT       FASYSTAB ENTRY FOR LOGON SYSTEM              
         MVI   GCPRGNO,X'27'       FAPGMTAB ENTRY FOR THIS PROGRAM              
*                                                                               
         GOTOX ('SWCHFC',AGROUTS),=AL1(QSCON)                                   
         BE    INI08               CONTROL SYSTEM SWITCH IS OK                  
         GOTOX ('SWCHFC',AGROUTS),GSSYS                                         
         GOTOX ('SETMSG',AGROUTS)                                               
         B     EXIT                                                             
*                                                                               
INI08    GOTOX ('SWCHFC',AGROUTS),GSSYS MAKE SURE SWITCH SYSTEM BACK            
*                                                                               
         CLI   CSREC,0             LOAD IN PREVIOUS OVERLAY                     
         BE    INI10               IF ONE EXISTS                                
         GOTOX ('OVRLAY',AGROUTS),CSREC                                         
         BNE   INI20                                                            
*                                                                               
INI10    BAS   RE,GLOBALS          SEE IF GLOBALS PRESENT                       
         LH    R7,=Y(TWUSER-TWAD)                                               
         A     R7,ATWA                                                          
         USING SAVED,R7                                                         
         CLI   SPFMACT,0                                                        
         BE    INI11                                                            
         MVC   BASRECH+FHDAD(4),=CL4'FILE'                                      
         MVI   BASRECH+FHILD,4                                                  
         MVC   BASACTH+FHDAD(3),=CL3'DIS'                                       
         CLI   SPFMACT,C'C'                                                     
         BNE   *+10                                                             
         MVC   BASACTH+FHDAD(3),=CL3'CHA'                                       
         MVI   BASACTH+FHILD,3                                                  
*                                                                               
INI11    CLI   BASRECH+FHILD,0     MAKE SURE WE HAVE SOME INPUT                 
         BNE   INI12                                                            
         CLI   BASACTH+FHILD,0                                                  
         BNE   INI12                                                            
         TM    BCINDS1,BCIANYPF                                                 
         BNZ   INI12                                                            
         LA    R0,BASRECH          NO RECORD/ACTION OR PFKEY THIS TIME          
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(GI$EFAR)                                            
         MVI   FVOMTYP,GTMINF                                                   
         MVI   CSACT,0                                                          
         B     INI20                                                            
*                              *** VALIDATE PFKEY PRESS                         
INI12    GOTOX AGEN2,BCPARM,OPFK,PFVAL                                          
         BNE   INI20                                                            
*                              *** VALIDATE RECORD FIELD                        
INI14    GOTOX AGEN,BCPARM,ORTYPE,RTVAL                                         
         BNE   INI20                                                            
*                              *** VALIDATE ACTION FIELD                        
INI16    GOTOX AGEN,BCPARM,OACT,AVAL                                            
         BNE   INI20                                                            
         CLI   SPFMACT,0                                                        
         BE    INI18                                                            
         OI    GSINDSL1,GSIXKEY                                                 
*                              *** PROCESS ACTION                               
INI18    GOTOX AGEN,BCPARM,OACT,APRC                                            
*                                                                               
*                              *** SET MESSAGE/BUILD PFKEY LINE                 
INI20    GOTOX ('SETMSG',AGROUTS)                                               
         GOTOX AGEN,BCPARM,OPFK,PFBLD                                           
         GOTOX ('SAVVAL',AGROUTS)                                               
*                                                                               
EXIT     CLI   ASONOFF,ASOFF       RUNNING OFFLINE?                             
         BNE   *+14                NO                                           
         L     RF,ASECBLK          SPOOF REDEFINES TWAUSER - SAVE IT            
         MVC   0(L'TWAUSER,RF),TWAUSER                                          
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* NTRSES CALL RETURN POINT                                            *         
***********************************************************************         
         SPACE 1                                                                
NTRRET   B     INI18                                                            
         SPACE 1                                                                
***********************************************************************         
* XITSES CALL RETURN POINT                                            *         
***********************************************************************         
         SPACE 1                                                                
XITRET   B     INI14                                                            
         SPACE 1                                                                
***********************************************************************         
* EXIT CALL RETURN POINT                                              *         
***********************************************************************         
         SPACE 1                                                                
EXITRET  B     INI20                                                            
         EJECT                                                                  
***********************************************************************         
* INITIALISE PRINT BLOCK                                              *         
***********************************************************************         
         SPACE 1                                                                
PRTINI   L     R1,AREP                                                          
         USING REPD,R1                                                          
         MVC   REPAPQB,ATIA                                                     
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
         MVC   REPSYSID,=C'SP'                                                  
         MVC   REPPRGID,=C'NF'                                                  
         MVC   REPRLH,=Y(48)                                                    
         MVC   REPRDH,=Y(12)                                                    
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* SEE IF GLOBALS LOITERING AROUND                                     *         
***********************************************************************         
         SPACE 1                                                                
GLOBALS  NTR1  ,                                                                
         LH    R7,=Y(TWUSER-TWAD)                                               
         A     R7,ATWA                                                          
         USING SAVED,R7                                                         
         MVI   SPFMACT,0                                                        
*                                                                               
         L     RF,ACOM                                                          
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),BCPARM,=C'GETD',AIO1,54,8                                   
         TM    8(R1),X'10'                                                      
         BO    GLOBBERX            EXIT IF NO PFM GLOBAL PRESENT                
         GOTO1 (RF),(R1),=C'DELE'                                               
         GOTO1 (RF),(R1),=C'DELE',,,4                                           
         MVI   SPFMACT,C'D'        SET FLAG TO SHOW USED GLOBAL INFO            
         L     R4,AIO1                                                          
         USING GLPFMFIL,R4                                                      
*                                                                               
         MVC   SPFMFIL,GLPFMFIL    GET FILE NAME IN FIELD                       
         MVC   SPFMDA,GLPFMDA      GET DISK ADDRESS                             
         MVC   SPFMKEY,GLPFMKEY    GET KEY                                      
         CLI   GLPFMFLG,C'*'       TEST IF WANT TO SET ACTION                   
         BNE   GLOBBERX                                                         
         CLI   GLPFMACT,C'C'       TEST IF ACTION PASSED                        
         BNE   GLOBBERX                                                         
         MVI   SPFMACT,C'C'                                                     
*                                                                               
GLOBBERX XIT1                                                                   
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
SPTFILE  DC    CL8'SPTFILE'                                                     
*                                                                               
DICUPR   DS    0XL4                ** UPPER CASE DICTIONARY LIST **             
DICUPRX  DC    AL1(EOT)            512 BYTES MAX                                
*                                                                               
DICMIX   DS    0XL4                ** MIXED CASE DICTIONARY LIST **             
DICMIXX  DC    AL1(EOT)            512 BYTES MAX                                
*                                                                               
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
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED BOOKS                                                *         
***********************************************************************         
         SPACE 1                                                                
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
* SPNFIWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE SPNFIWORK                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014SPNFI00   04/10/01'                                      
         END                                                                    

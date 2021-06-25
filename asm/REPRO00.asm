*          DATA SET REPRO00    AT LEVEL 006 AS OF 04/20/99                      
*&&      SET   NOP=N                                                            
*PHASE T80A00C,*                                                                
*INCLUDE REPROINI                                                               
*INCLUDE GEFILSET                                                               
         TITLE 'T80A00 - REP PROPOSALS ROOT PHASE'                              
PRO00    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL LENTOTLQ,REPRO00*,R6,CLEAR=YES,RR=R3                             
         LR    R9,RC                                                            
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
*                                                                               
INIT     L     RF,=V(GEFILSET)                                                  
         LA    RF,0(R3,RF)                                                      
         GOTO1 (RF),BCPARM                                                      
         L     R8,AGWORK                                                        
         USING GWORKD,R8           R8=A(PROGRAM W/S)                            
*                                                                               
         L     RF,=V(REPROINI)                                                  
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
         MVC   REPPRGID,=C'SE'                                                  
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
         MVC   GSDSPMAX,=Y(TWSAVE-TWAD)                                         
         MVC   GSDSPREC,=Y(BASRECH-TWAD)                                        
         MVC   GSDSPACT,=Y(BASACTH-TWAD)                                        
*        MVC   GSDSPSCR,=Y(BASSCRH-TWAD)                                        
*        MVC   GSDSPPAG,=Y(BASPAGH-TWAD)   PAGE FIELD NOT USED                  
*        MVC   GSDSPREP,=Y(BASREPH-TWAD)                                        
**       MVC   GSDSPOPT,=Y(BASOPTH-TWAD)   OPTIONS FIELD NOT USED               
         MVC   GSDSPSAV,=Y(BASOPTH-TWAD)   FOR 2 LINE FF SCREEN                 
*JRD     MVC   GSDSPSAV,=Y(BASOLY1H-TWAD)  FOR 1 LINE FF SCREEN                 
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
INIT06   DS    0H                                                               
*                                                                               
*  PROGRAM AREA TOO SMALL FOR THIS & PRO/REPORT                                 
*                                                                               
*&&DO                                                                           
         CLI   ASONOFF,ASOFF       RUNNING OFFLINE?                             
         BE    *+8                 YES                                          
         BAS   RE,CHKGLOB          CHECK FOR BROWSE ELEMENT                     
*&&                                                                             
         MVI   GCSWSYSN,QSREP      DEFAULT IS REP SYSTEM                        
         MVI   GCSWSYSP,QSREP                                                   
*                                                                               
         GOTOX ('SWCHFC',AGROUTS),=AL1(QSCON)  SWITCH TO CONTROL SYS            
         BE    INIT07                                                           
*                                                                               
         MVC   BASMSG(28),=CL28'ERROR: NO CONTROL ACCESS YET'                   
         OI    BASMSGH+6,X'80'                                                  
         OI    BASRECH+1,X'20'     CHANGE TO PROTECTED                          
         OI    BASACTH+1,X'20'                                                  
         OI    BASRECH+6,X'80'                                                  
         OI    BASACTH+6,X'80'                                                  
         B     EXIT                                                             
*                                                                               
INIT07   OC    GSSYS,GSSYS         ALREADY GOT A SYSTEM SET UP?                 
         BNZ   *+8                                                              
         MVI   GSSYS,QSREP         SET REP AS DEFAULT                           
         MVC   GCFILNAM,REPFILE    SET REPFILE AS DEFAULT FILE NAME             
*                                                                               
         MVI   GCOVSYS,QSREP       FASYSTAB ENTRY FOR LOGON SYSTEM              
         MVI   GCPRGNO,X'0A'       FAPGMTAB ENTRY FOR THIS PROGRAM              
         GOTOX ('SWCHFC',AGROUTS),GSSYS MAKE SURE SWITCH SYSTEM                 
*                                                                               
         CLI   BASRECH+FHILD,0                                                  
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
         EJECT                                                                  
EXIT     CLI   ASONOFF,ASOFF     RUNNING OFFLINE?                               
         BNE   *+14              NO                                             
         L     RF,ASECBLK        SPOOF REDEFINES TWAUSER - SAVE IT              
         MVC   0(L'TWAUSER,RF),TWAUSER                                          
XIT      XIT1  ,                                                                
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
XITRET   DS    0H                                                               
         B     INIT20              ??                                           
         SPACE 1                                                                
***********************************************************************         
* EXIT CALL RETURN POINT                                              *         
***********************************************************************         
         SPACE 1                                                                
EXITRET  DS    0H                                                               
         B     INIT50                                                           
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
* CHECK GLOBER AREA AND DO BROWSE CLEAN UP                            *         
***********************************************************************         
CHKGLOB  NTR1                                                                   
         L     RF,ACOM                                                          
         USING COMFACSD,RF                                                      
         L     RF,CGLOBBER                                                      
         DROP  RF                                                               
         GOTO1 (RF),BODMCB,=C'GETD',BOWORK1,24,GLVXCTL                          
         TM    BODMCB+8,X'10'                                                   
         BNZ   CGLOB50             NO VARIABLES FOUND                           
*                                                                               
* DELETE ELEMENT AND =RE UNLESS FROM CONTRACT OR BROWSE                         
*                                                                               
         CLC   =C'CON',BOWORK1+(GLVXFRPR-GLVXFRSY)                              
         BE    CGLOBX              FROM CONTRACT                                
         CLC   =C'BRO',BOWORK1+(GLVXFRPR-GLVXFRSY)                              
         BE    CGLOBX              FROM BROWSE                                  
*                                                                               
         L     RF,ACOM                                                          
         USING COMFACSD,RF                                                      
         L     RF,CGLOBBER                                                      
         DROP  RF                                                               
         GOTO1 (RF),BODMCB,=C'DELE',,,GLVXCTL                                   
*                                                                               
         XC    TWASRV,TWASRV                                                    
         MVC   TWASRV(3),=C'=RE'                                                
         OI    TWASRVH+6,X'80'                                                  
         MVI   TWASRVH+5,3                                                      
*                                                                               
         B     CGLOBX                                                           
*                                                                               
* NO CONTROL ELEMENT, DELETE ANY BROWSE ELEMENTS                                
*                                                                               
CGLOB50  DS    0H                                                               
         L     RF,ACOM                                                          
         USING COMFACSD,RF                                                      
         L     RF,CGLOBBER                                                      
         DROP  RF                                                               
         GOTO1 (RF),BODMCB,=C'GETD',BOELEM,GLBRWLNQ,GLRBRWSE                    
         TM    BODMCB+8,X'10'      BROWSE ELEM?                                 
         BNZ   CGLOBX              NO                                           
*                                                                               
         L     RF,ACOM                                                          
         USING COMFACSD,RF                                                      
         L     RF,CGLOBBER                                                      
         DROP  RF                                                               
         GOTO1 (RF),BODMCB,=C'DELE',,,GLRBRWSE                                  
*                                                                               
CGLOBX   DS    0H                                                               
         B     XIT                                                              
*&&                                                                             
***********************************************************************         
* CONSTANTS AND LITERALS                                              *         
***********************************************************************         
FF       EQU   X'FF'                                                            
DD#DISK  EQU   32768                                                            
*                                                                               
REPFILE  DC    CL8'REPFILE'                                                     
*---------------------------------------------------------------------*         
         LTORG                                                                  
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
* REPROWORK                                                                     
       ++INCLUDE REPROWORK                                                      
         EJECT                                                                  
* DDGLVXCTLD + DDGLOBEQUS + REGLBRW                                             
         PRINT OFF                                                              
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE REGLBRW                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006REPRO00   04/20/99'                                      
         END                                                                    

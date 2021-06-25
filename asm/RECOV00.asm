*          DATA SET RECOV00    AT LEVEL 051 AS OF 10/25/11                      
*          DATA SET RECOV00    AT LEVEL 048 AS OF 05/14/98                      
*PHASE T82300C                                                                  
*INCLUDE RECOVINI                                                               
*INCLUDE GEFILSET                                                               
         TITLE 'T82300 - REP COVERSHEET ROOT PHASE'                             
T82300   CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL LENTOTLQ,RECOV00*,R6,CLEAR=YES,RR=R3                             
         LR    R9,RC                                                            
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
*                                                                               
INIT     L     RF,=V(GEFILSET)                                                  
         LA    RF,0(R3,RF)                                                      
         GOTO1 (RF),BCPARM                                                      
         L     R8,AGWORK                                                        
         USING GWORKD,R8           R8=A(PROGRAM W/S)                            
*                                                                               
         L     R7,ASVEBLK                                                       
         USING MYSAVED,R7          R7=A(TWA SAVED STORAGE)                      
*                                                                               
         L     RF,=V(RECOVINI)                                                  
         LA    RF,0(R3,RF)                                                      
         GOTO1 (RF),BCPARM,ADDRS2,TWDICTL-TWDICT,C'FIL '                        
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA             RA=A(TWA)                                    
         BL    INIT50                                                           
         BH    *+8                                                              
         OI    BCINDS1,BCIFRST     ON=NOT FIRST TIME                            
*&&DO                                                                           
         L     R1,AREP             THIS IS REPORTING CRAP                       
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
         MVC   REPPRGID,=C'CO'                                                  
         MVC   REPRLH,=Y(48)                                                    
         MVC   REPRDH,=Y(12)                                                    
*&&                                                                             
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
         MVC   GSDSPSCR,=Y(BASSCRH-TWAD)                                        
***      MVC   GSDSPPAG,=Y(BASPAGH-TWAD)   PAGE FIELD NOT USED                  
***      MVC   GSDSPREP,=Y(BASREPH-TWAD)                                        
***      MVC   GSDSPOPT,=Y(BASOPTH-TWAD)   OPTIONS FIELD NOT USED               
         MVC   GSDSPSAV,=Y(BASSCRH-TWAD)                                        
*JRD     MVC   GSDSPSAV,=Y(BASOLY1H-TWAD)  FOR 1 LINE FF SCREEN                 
         MVC   GSDSPOVR,=Y(BASOLY1H-TWAD)                                       
**       MVC   GSDSPKEY,=Y(BASKEYH-TWAD)                                        
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
         BZ    INIT05                                                           
         GOTOX ('RESVAL',AGROUTS)  RESTORE SAVED VALUES                         
         XC    NSSAV,NSSAV         ??                                           
*                                                                               
         TM    KFLAGS,KFFROMK                                                   
         BZ    INIT06                                                           
         OC    NAMFLD,NAMFLD                                                    
         BNZ   *+6                                                              
         DC    H'0'                SHOULD HAVE THIS IF NOT 1ST TIME             
         L     RE,NAMFLD                                                        
         AR    RE,RA                                                            
         NI    1(RE),X'FF'-X'20'   UNPROTECT COVERSHEET FLD                     
         B     INIT06                                                           
*                                                                               
INIT05   DS    0H                  FIRST TIME INITIALIZATION                    
         MVI   KFLAGS,0                                                         
         XC    NAMFLD,NAMFLD                                                    
*                                                                               
*  PROGRAM AREA TOO SMALL FOR THIS & PRO/REPORT                                 
*                                                                               
INIT06   DS    0H                                                               
         CLI   ASONOFF,ASOFF       RUNNING OFFLINE?                             
         BE    *+8                 YES                                          
         BAS   RE,CHKGLOB          CHECK FOR INCOMING GLOBBER                   
*                                                                               
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
         MVI   GCPRGNO,X'23'       FAPGMTAB ENTRY FOR THIS PROGRAM              
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
         CLI   BCPFKEY,12          QUIT?                                        
         BNE   INIT20                                                           
         CLI   TWASESNL,1          NESTED?                                      
         BE    INIT20                                                           
         GOTO1 AGEN,BODMCB,OSES,SXIT                                            
         DC    H'0'                                                             
*                                                                               
INIT20   GOTOX AGEN,BCPARM,ORTYPE,RTVAL                                         
         BNE   INIT50                                                           
         XC    GSRECMSK,GSRECMSK                                                
         OC    GSRECMSK,=AL4(MK#DIS)                                            
*                                                                               
INIT30   GOTOX AGEN,BCPARM,OACT,AVAL                                            
         BNE   INIT50                                                           
         TM    KFLAGS,KFFROMK      HERE FROM CONTRACT?                          
         BZ    *+8                                                              
         OI    GSINDSL1,GSIXKEY    FORCE PAST 1ST TRANSACTION                   
*                                                                               
INIT40   GOTOX AGEN,BCPARM,OACT,APRC                                            
*                                                                               
INIT50   GOTOX ('SETMSG',AGROUTS)                                               
         GOTOX AGEN,BCPARM,OPFK,PFBLD                                           
         GOTOX ('SAVVAL',AGROUTS)                                               
*                                                                               
         L     RF,AOLY             GUARANTEE LAST CALL                          
         CLI   CSOVER,O#FLTR                                                    
         BNE   *+8                                                              
         L     RF,APSOLY                                                        
         ICM   RF,8,=AL1(2)                                                     
         LA    R1,BOPARM                                                        
         BASR  RE,RF                                                            
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
***********************************************************************         
* CHECK GLOBER AREA AND DO BROWSE CLEAN UP                            *         
***********************************************************************         
CHKGLOB  NTR1                                                                   
         L     RF,ACOM             CHECK FOR GLOBBER CONTROL ELEM               
         USING COMFACSD,RF                                                      
         L     RF,CGLOBBER                                                      
         DROP  RF                                                               
         GOTO1 (RF),BODMCB,=C'GETD',SVGLCTEL,24,GLVXCTL                         
         TM    BODMCB+8,X'10'                                                   
         BNZ   CGLOBX              NO VARIABLES FOUND                           
         CLC   =C'CON',SVGLCTEL+(GLVXFRPR-GLVXFRSY)                             
         BE    *+6                 FROM CONTRACT                                
         DC    H'0'                NO OTHER POSSIBILITIES                       
*                                                                               
         GOTO1 (RF),BODMCB,=C'DELE',,,GLVXCTL                                   
         CLI   BODMCB+8,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 (RF),BODMCB,=C'GETD',SVCOVEL,GLCOVLNQ,GLRCOVER                   
         TM    BODMCB+8,X'10'      BROWSE ELEM?                                 
         BZ    *+6                                                              
         DC    H'0'                MUST HAVE COVERSHEET ELEMENT                 
*                                                                               
         GOTO1 (RF),BODMCB,=C'DELE',,,GLRCOVER                                  
*                                                                               
         OI    KFLAGS,KFFROMK      WE GOT HERE FROM CONTRACT                    
         OI    GSINDSL1,GSIXKEY    FORCE PAST 1ST TRANSACTION                   
*                                                                               
         LA    R2,BASRECH                                                       
         MVC   8(5,R2),=C'COVER'                                                
         MVI   5(R2),5                                                          
         OI    1(R2),X'20'+X'0C'   PROTECTED & HIDDEN                           
         OI    4(R2),X'80'         MODIFIED                                     
         OI    6(R2),X'80'         XMIT                                         
         OI    BASRELH+1,X'0C'     HIDE LABEL                                   
         OI    BASRELH+6,X'80'     XMIT LABEL                                   
*                                                                               
         LA    R3,SVCOVEL                                                       
         USING GLCOVNAM,R3                                                      
         TM    GLCOVFLG,X'40'      DELETING?                                    
         BO    CGLOB050            YES                                          
         TM    GLCOVFLG,X'80'      UPDATING?                                    
         BO    CGLOB100            YES                                          
         DROP  R3                                                               
*                                                                               
         LA    R2,BASACTH          DISPLAY                                      
         MVC   8(3,R2),=C'DIS'                                                  
         MVI   5(R2),3                                                          
         OI    1(R2),X'20'+X'0C'   PROTECTED & HIDDEN                           
         OI    4(R2),X'80'         MODIFIED                                     
         OI    6(R2),X'80'         XMIT                                         
         OI    BASACLH+1,X'0C'     HIDE LABEL                                   
         OI    BASACLH+6,X'80'     XMIT LABEL                                   
         B     CGLOBX                                                           
*                                                                               
CGLOB050 DS    0H                  UPDATING                                     
         LA    R2,BASACTH          DISPLAY                                      
         MVC   8(3,R2),=C'DEL'                                                  
         MVI   5(R2),3                                                          
         OI    1(R2),X'20'+X'0C'   PROTECTED & HIDDEN                           
         OI    4(R2),X'80'         MODIFIED                                     
         OI    6(R2),X'80'         XMIT                                         
         OI    BASACLH+1,X'0C'     HIDE LABEL                                   
         OI    BASACLH+6,X'80'     XMIT LABEL                                   
         B     CGLOBX                                                           
*                                                                               
CGLOB100 DS    0H                  UPDATING                                     
         LA    R2,BASACTH          DISPLAY                                      
         MVC   8(5,R2),=C'MAINT'                                                
         MVI   5(R2),5                                                          
         OI    1(R2),X'20'+X'0C'   PROTECTED & HIDDEN                           
         OI    4(R2),X'80'         MODIFIED                                     
         OI    6(R2),X'80'         XMIT                                         
         OI    BASACLH+1,X'0C'     HIDE LABEL                                   
         OI    BASACLH+6,X'80'     XMIT LABEL                                   
*                                                                               
CGLOBX   DS    0H                                                               
         B     XIT                                                              
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
* RECOVWORK                                                                     
       ++INCLUDE RECOVWORK                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'051RECOV00   10/25/11'                                      
         END                                                                    

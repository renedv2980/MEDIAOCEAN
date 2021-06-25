*          DATA SET SPTRA62    AT LEVEL 021 AS OF 11/09/20                      
*PHASE T21662B                                                                  
*INCLUDE GETBROAD                                                               
*INCLUDE XSORT                                                                  
*INCLUDE ENDOFMOD                                                               
                                                                                
***********************************************************************         
*                                                                     *         
*  TITLE: T21662 - TRAFFIC NETWORK COMMERCIAL SEED PROGRAM            *         
*  COMMENTS: THIS PROGRAM SEEDS NETWORK UNITS WITH COMMERCIALS        *         
*            USING NETWORK PATTERN RECORDS AS SOURCE.                 *         
*  OUTPUTS: UPDATED UNIT RECORDS AND REVISIONS RECORDS.               *         
*  LOCALS: REGISTER USAGE                                             *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR     *         
*          R4 - WORK REG & KEY DSECT POINTER                          *         
*          R5 - WORK REG & POINTER TO PROGRAM/UNIT TABLES             *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                  *         
*          R7 - SECOND BASE                                           *         
*          R8 - POINTER TO SPOOLD                                     *         
*          R9 - POINTER TO SYSD                                       *         
*          RA - POINTER TO ATWA                                       *         
*          RB - FIRST BASE                                            *         
*          RC - POINTER TO GEND                                       *         
*                                                                     *         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)         *         
*                  - IN AIO FROM HYPER CONTROLLER (T00A30)            *         
*                  - NETIO                                            *         
*             AIO2 - READ ESTIMATES IN BLUNT RTN                      *         
*                  - PATTERN TABLE 1ST 2000 BYTES           1-2000    *         
*                  - READ COMMERCIAL RECORD AT AIO2+2000    2-2940    *         
*                  - UNIT TABLE ENTRY AT AIO2+2938 (CL60)    -3000    *         
*                  - COMMERCIAL TABLE (LAST 1000 BYTES)     3-4000    *         
***********************************************************************         
*                                                                     *         
*             AIO3 -   0 - 1250 = EST/PROD/COPY CODE TABLE            *         
*                  - 1250 - 1475 = NON-STD PROG NAME LIST             *         
*                  - 1476 - 2500 = NETBLOCK                           *         
*                  - LAST 1500 USED AS SAVE AREA WHEN BRAND LEV SEC   *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
* LEV160 SMUR FEB28/05 ALLOW 100 PRDS PER PGROUP, BUG FIX IN VPER     *         
* LEV161 SMUR MAR01/05 MEDIA REQUIRED FOR MCNYB NET=ALL REQUEST       *         
* LEV162 SMUR MAR29/05 ADD PROD= TO OPTIONS                           *         
* LEV163 SMUR APR28/05 FIX PERIOD OVERRIDE FOR NET=ALL REQUEST        *         
* LEV168 BGRI SEP02/05 MORE PRODUCTS                                  *         
*        SMUR          USE DEFAULT TIME FOR PQ RETAINS                *         
*        SMUR DEC20/05 FIX ADID WHEN TN1 PR4 SET TO Y(BYPASS SEED PRT)*         
* LEV168 SMUR AUG17/06 MAKE PROGRAM TABLE BIGGER                      *         
*                      PUT BACK EST/PRD TABLE AT 249 ENTRIES NOT 140  *         
* LEV168 MNAS OCT02/06 FIX MORE BRANDS SEQUENTIAL READ BUG            *         
* LEV168 MNAS DEC18/06 FIX MORE BRANDS BUG: SVUNTCOM AREA NOT DEFINED *         
*                      AS THE SAME LENGTH AS UNTCOM: DATA WAS LOST    *         
* LEV168 SMUR FEB13/07 FIX NET REVISION WHEN TRAFFICKING BY PRD/PGROUP*         
* LEV168 SMUR FEB26/07 FORCE MSYRT TO ENTER MEDIA FOR NET=ALL REQUESTS*         
* LEV168 SMUR MAY14/07 FIX NET REV NUMBR WHEN NO INSTR RUN            *         
* LEV169 SMUR JUN06/07 BUILD C/S PROD LIST IN BLUNT                   *         
*        SMUR JUN06/07 MAKE ADID CODE SOFTER                          *         
* LEV170 MNAS JUL31/07 FIX OPTPGRL REFERENCES TO BE 3 CHAR ENTRIES :  *         
*                      ALSO INCREASE TABLE FROM 50 TO 329 ENTRIES     *         
* LEV172 SMUR OCT02/07 FIX PERIOD OVERRIDE FOR NET=ALL REQUEST        *         
* LEV173 SMUR OCT29/07 ALLOW SOON REQUESTS FOR AGY=M2 (MENYT)         *         
* LEV174 SMUR FEB28/08 ALLOW SWITCHING PERIODS BROA/CAL/WEEKLY        *         
*                      ALLOW SOON REQUESTS FOR AGY YN AND H7          *         
*                      MOVE PATTERN TABLE FROM BLOCK TO VADUMMY       *         
* LEV180 SMUR OCT13/08 500 PRDS PER PGROUP                            *         
* LEV187 SMUR JAN20/10 VALIDATE PRD= OPTION AGAINST TN2PR1 PROFILE    *         
* LEV007 SMUR   OCT/10 FIX WHEN NOPRT AND THERE IS ONLY 1 PROG FOR NET*         
* LEV008 SMUR   OCT/10 FIX SEEDING ADID AND NO ADID FLAG BUG          *         
* LEV012 SMUR APR27/11 ADD FEED ELEMS X'23' WITH BIGGER LENGTH        *         
* LEV016 SMUR FEB19/13 SET READ FOR UPDATE TO N BEFORE DMGR CALLS     *         
* LEV017 MNAS MAR19/13 PROILE LOGIC FOR SUBMEDIA V                    *         
* LEV018 SMUR SEP12/13 SKIP CHECK FOR X'99' ELEM FOR SKED UNITS       *         
* LEV019 SMUR NOV11/13 TREAT INVALID UNIT/CML AS UNASGND AND REASSIGN *         
*                      THEM EVEN WHEN BYPASS PREV ASGN CML IS Y       *         
* SPEC-46259  SMUR 05/14/20 STOP MAXIO DUMPS FOR OV REQUEST IN VK     *         
* SPEC-42505  SMUR FEB05/20 SUPPORT CONVERTED DATES ON REVISION RECS  *         
*                                                                     *         
*                    *** TERMINUS ***                                 *         
***********************************************************************         
         TITLE 'T21662 - NETWORK TRAFFIC COMMERCIAL SEED PROGRAM'               
T21662   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21662**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,SPTR62RR                                                      
         LH    R0,=AL2(ENDSYSD-SYSD)   SEE IF PAST END OF SYSD                  
         C     R0,LSYSD                                                         
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,INIT             GO SET UP ADCONS                             
*                                                                               
         TM    WHEN,X'40'          IS THIS RUNNING NOW                          
         BZ    INIT30                                                           
         CLI   TWAOFFC,C'*'        THIS A DDS TERM                              
         BNE   NOWERR                                                           
         EJECT                                                                  
INIT30   CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,PRINTREP       OFFLINE SEED                                 
         BE    LRR                                                              
         CLI   MODE,RUNFRST                                                     
         BE    GENAUTO                                                          
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     EXIT                                                             
*                                                                               
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
* SET UP FOR FILE OPENS OR RUNLAST AS NEEDED *                                  
*                                                                               
GENAUTO  CLI   OFFLINE,C'Y'                                                     
         BNE   EXIT                                                             
         TM    WHEN,X'C0'          TEST IMMED/NOW                               
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,TWAMASTC                                                      
         L     RF,MCSSB-MCBLOCK(RE)                                             
         OI    SSBSTAT2-SSBD(RF),SSBSROLC                                       
*                                                                               
         B     EXIT                                                             
*                                                                               
*     VALIDATE KEY ROUTINE                                                      
*                                                                               
* IF NO CHANGES TO KEY FIELDS, THEN CHECK FOR SELECTS *                         
*                                                                               
VK       MVI   PQSW,1              SUPPRESS AUTO PRTQUE OPEN                    
*                                                                               
         XC    SVNET,SVNET         INIT CURRENT NETWORK (FOR NET=ALL)           
*                                                                               
         TM    TRACLTH+4,X'20'     CLIENT CHANGED                               
         BZ    VK10                  YES REVALIDATE                             
         TM    TRANETH+4,X'20'     NETWORK CHANGED                              
         BZ    VK10                  YES REVALIDATE                             
         TM    TRAPROGH+4,X'20'    PROGRAM CHANGED                              
         BZ    VK10                  YES REVALIDATE                             
         TM    TRAPERH+4,X'20'     PERIOD CHANGED                               
         BZ    VK10                  YES REVALIDATE                             
         TM    TRAFLTRH+4,X'20'     OPTIONS CHANGED                             
         BZ    VK10                  YES REVALIDATE                             
         CLI   TABLESW,C'Y'        WAS TABLE BUILT                              
         BNE   VK10                NO                                           
         CLI   1(RA),C'*'          IS THIS A DDS TERMINAL                       
         BNE   VK10                NO                                           
         EJECT                                                                  
CSEL     BAS   RE,RDTWA            RESTORE UNIT TABLE, ETC                      
*                                                                               
* CHECK FOR UNPROTECTED SELECT FIELDS                                           
*                                                                               
         LA    R2,TRASEL1H                                                      
         L     R5,APRGTBLE                                                      
         USING PRGTABLD,R5                                                      
         NI    UPDSW,X'FF'-X'10'   SET OFF RETRANSMIT THIS SCREEN               
*                                                                               
CSEL10   TM    1(R2),X'20'         PROTECTED                                    
         BO    CSEL14               YES, DONE THIS BEFORE                       
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSELER                                                         
         CLI   8(R2),C'S'          SELECT (SEED UNITS)                          
         BNE   CSEL14               NO                                          
         OI    UPDSW,X'10'         SET TO RETRANSMIT THIS SCREEN                
         BAS   RE,GEN              GO SEED UNITS FOR PROGRAM                    
         B     DONEALL                                                          
*                                                                               
CSEL14   LA    R2,NEXTLINE(,R2)                                                 
         LLC   RE,0(R2)                                                         
         LA    RF,0(RE,R2)                                                      
         OC    8(L'TRADSP1,RF),8(RF) ANYTHING HERE                              
         BZ    CSEL16              NO, NO SEL CODE NEEDED                       
         CLI   0(R2),9                                                          
         BH    CSEL10                                                           
CSEL16   A     R5,ASVNEXT          GET STARTING POINT IN TABLE                  
         OC    0(4,R5),0(R5)       AT END OF TABLE                              
         BZ    DONEALL                                                          
         DROP  R5                                                               
*                                                                               
         TM    UPDSW,X'10'         SET TO RETRANSMIT THIS SCREEN                
         BZ    LRL                  NO - GO ON TO NEXT SCREEN, IF ANY           
         MVC   GERROR,=Y(SELMSG)                                                
         MVI   GMSGTYPE,C'I'       SET INFO MSG                                 
         OI    TRATAGH+1,X'01'     SET MODIFIED BIT                             
         OI    TRATAGH+6,X'80'     AND TRANSMIT                                 
         BAS   RE,SVTWA                                                         
         B     TRAPERR2                                                         
         EJECT                                                                  
* VALIDATE KEY FIELDS                                                           
*                                                                               
VK10     DS    0H                                                               
         LA    RE,STDATEB                                                       
         LA    RF,(WRK2-STDATEB)                                                
         XCEF                                                                   
*                                                                               
         LA    RE,SVOPTDTA                                                      
         LA    RF,(OPTEND-SVOPTSW)                                              
         XCEF                                                                   
*                                                                               
         LHI   RE,OPTPGRL-SYSD                                                  
         AR    RE,R9                                                            
         LA    RF,ENDPGTBL-OPTPGRL                                              
         XCEF                                                                   
*                                                                               
         L     R0,APRGTBLE                                                      
         L     R1,APRGTBLX                                                      
         SR    R1,R0                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R2,ELEM             FAKE VALIMED                                 
         XC    ELEM,ELEM                                                        
         MVC   ELEM,=X'0A01000184010001'                                        
         MVI   ELEM+8,C'N'                                                      
         GOTO1 VALIMED                                                          
*                                                                               
         LA    R2,TRACLTH          CLIENT                                       
*                                                                               
         GOTO1 VALICLT                                                          
*                                                                               
         OI    4(R2),X'20'         SET ON VALIDATED                             
*                                                                               
* READ ANY REVISION RECORD TO SEE IF THE FILE HAS BEEN CONVERTED.               
         NI    SVFLAG,X'FF'-CONVSW INIT CONVERTED RECORDS                       
         BRAS  RE,SETXSP           SET TO XSPOT                                 
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A1D'                                                  
         GOTO1 HIGH                                                             
         CLC   =X'0A1D',KEY        ANY REV REC?                                 
         BE    VK10C                                                            
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         MVC   0(L'NOTEMSG,R1),NOTEMSG                                          
         LA    R1,L'NOTEMSG(,R1)                                                
         MVC   0(2,R1),AGENCY                                                   
         MVC   3(3,R1),QCLT                                                     
         MVC   7(4,R1),TRANET                                                   
         MVC   12(8,R1),TRAPER                                                  
                                                                                
         OC    ELEM,SPACES                                                      
         GOTO1 DATAMGR,DMCB,=C'OPMSG',(L'NOTEMSG+72,ELEM)                       
         B     VK10D               NO REC FOUND, TREATE AS CONVERTED            
*                                                                               
VK10C    TM    KEY+32,X'03'        CONVERTED RECORDS?                           
         BZ    *+8                                                              
VK10D    OI    SVFLAG,CONVSW                                                    
*                                                                               
         BRAS  RE,SETSPOT         CHANGE TO SPOT                                
*                                                                               
* READ TN PROFILE *                                                             
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0TN'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVI   WORK+6,C'N'                                                      
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF CLIENT OFFICE                                
*                                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    VK10F                                                            
*                                                                               
         TM    SECFLAG,NEPRDOFF    PRDS OFFICES NOT EQ                          
         BO    VK10F                                                            
*                                                                               
         MVC   WORK+11(1),SVPRDOFF USE PROD OFFICE                              
*                                                                               
VK10F    MVC   WORK+16(16),WORK    SAVE PROFILE VALUES                          
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
*                                                                               
         MVC   MYTN2PR6,SVPROF+1                                                
*                                                                               
         MVC   WORK(16),WORK+16    RESTORE ARGS                                 
         MVC   WORK(4),=X'A2E3D5F1'  LOWERCASE S TN1                            
         GOTO1 (RF),(R1),,SVT1PROF                                              
*                                                                               
         LA    R2,TRANETH                                                       
         CLI   5(R2),3                                                          
         BNE   VK10H                                                            
         CLC   =C'ALL',8(R2)       ALL NET REQUEST                              
         BNE   VK10H                                                            
*                                                                               
         BAS   RE,VOPTP            CHK MEDIA, PERIOD                            
         MVC   WORK+6(1),SVOPTMED  BY MEDIA                                     
*                                                                               
VK10H    MVC   WORK+1(3),=C'TN2'     TN2 PROFILE                                
         GOTO1 GETPROF,DMCB,WORK,SVTN2PRO,DATAMGR                               
*                                                                               
         TM    SVOPTSW1,OPTPER    PERIOD OVERRIDE ENTERED                       
         BO    VK10K               YES, BYPASS PROFILE ENTRY                    
*                                                                               
         CLI   SVTN2PRO+5,0                                                     
         BE    *+18                                                             
         CLI   SVTN2PRO+5,C'0'                                                  
         BE    *+10                                                             
         MVC   MYTN2PR6,SVTN2PRO+5                                              
*                                                                               
VK10K    MVC   MYTN2PR8,SVTN2PRO+7                                              
*                                                                               
         MVC   WORK+1(3),=C'TN3'     TN3 PROFILE                                
         GOTO1 GETPROF,DMCB,WORK,SVTN3PRO,DATAMGR                               
*                                                                               
         LA    R2,TRANETH          NETWORK                                      
*                                                                               
         CLI   5(R2),3                                                          
         BNE   VK15                                                             
         CLC   =C'ALL',8(R2)                                                    
         BNE   VK15                                                             
         TM    WHEN,X'38'          TEST SOON/OVERNIGHT/DDS                      
         BZ    OFFLNERR             NO, ERROR                                   
*                                                                               
         BRAS  RE,VPER             VALIDATE PERIOD - MONTH/YEAR                 
*                                                                               
         MVI   BYTE1,X'FF'         PRE-SET NETWORK NOT FOUND                    
         B     VK13                                                             
*                                                                               
VK12     NTR1                                                                   
*                                                                               
VK13     DS    0H                  GET NETWORK (FILLS IN SVNET)                 
         MVI   ERROR,0                                                          
         OI    GENSTAT1,CATCHIOR   I WANT CONTROL BACK                          
         GOTO1 CATCHIOS            DON'T TASKNEXT OUT!                          
         CLI   ERROR,0             ANY ERROR?                                   
         BE    VK13C                                                            
         TM    WHEN,X'10'          TEST OVERNIGHT                               
         BO    EXIT                 YES, LET IT GO THROUGH                      
         B     MAXIOER1                                                         
                                                                                
VK13C    BRAS  RE,GNET                                                          
         CLC   SVNET,=X'FFFFFFFF'                                               
         BNE   VK15                                                             
         CLI   BYTE1,X'FF'                                                      
         BNE   EXIT                                                             
         MVI   SVNET,0             FUDGE ALL NETS REQUEST                       
         B     NOUNTER             IF NONE FOUND, ERROR                         
*                                                                               
VK15     LA    R2,TRANETH                                                       
         MVI   UPDSW,0                                                          
         BRAS  RE,VNET                                                          
         BNE   VK13                GET NEXT NETWORK                             
         CLC   SVNET,=X'FFFFFFFF'                                               
         BE    EXIT                                                             
*                                                                               
* MUST READ TN2 PROFILE AFTER VNET                                              
*                                                                               
         MVI   WORK,X'A2'                                                       
         MVC   WORK+1(3),=C'TN2'   READ TN2 PROFILE                             
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),SVMEDIA   BY MEDIA                                     
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF CLIENT OFFICE                                
*                                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    VK16                                                             
*                                                                               
         TM    SECFLAG,NEPRDOFF    PRDS OFFICES NOT EQ                          
         BO    VK16                                                             
*                                                                               
         MVC   WORK+11(1),SVPRDOFF USE PROD OFFICE                              
*                                                                               
VK16     GOTO1 GETPROF,DMCB,WORK,SVTN2PRO,DATAMGR                               
*MNV                                                                            
         CLI   SVTN2PRO+14,C'Y'                                                 
         BNE   VK16C                                                            
         TM    SVOPTSW1,OPTDIGI                                                 
         BO    VK16C                                                            
         CLI   SVMEDIA,C'V'                                                     
         BNE   VK16C                                                            
         MVC   GERROR,=Y(BDNETMED)                                              
         B     TRAPERR2                                                         
                                                                                
VK16C    DS    0H                                                               
*MNV                                                                            
*                                                                               
         TM    SVOPTSW1,OPTPER    PERIOD OVERRIDE ENTERED                       
         BO    VK16D               YES, BYPASS PROFILE ENTRY                    
*                                                                               
         MVC   MYTN2PR6,SVPROF+1                                                
*                                                                               
         CLI   SVTN2PRO+5,0                                                     
         BE    *+18                                                             
         CLI   SVTN2PRO+5,C'0'                                                  
         BE    *+10                                                             
         MVC   MYTN2PR6,SVTN2PRO+5                                              
*                                                                               
VK16D    MVC   MYTN2PR8,SVTN2PRO+7                                              
*                                                                               
         OC    SVNET,SVNET         ALL NETWORK REQUEST ?                        
         BZ    VK18                 NO                                          
*                                                                               
         TM    WHEN,X'20'          TEST SOON                                    
         BZ    VK18                                                             
         TM    SVOPTSW,OPTTEST                                                  
         BZ    OFFLNERR            ONLY TEST RUNS FOR SOON                      
*                                                                               
         EJECT                                                                  
* VALID OPTIONS = 'TEST'      OPTTEST  (X80)                                    
*                 'RERUN'     OPTRERUN (X40)                                    
*                                      (X20)                                    
*                                      (X10)                                    
*                                      (X08)                                    
*                                      (X04)                                    
*                                      (X02)                                    
*                 'PATTERN'   OPTPTTN  (X01) PRINT SOURCE PATTERN               
*                                                                               
VK18     LA    R2,TRAFLTRH                                                      
*                                                                               
         BRAS  RE,VOPT             GO VALIDATE OPTIONS                          
         OI    4(R2),X'20'         SET ON VALIDATED                             
*                                                                               
* VALIDATE PERIOD BEFORE PROGRAM *                                              
*                                                                               
VK20     BRAS  RE,VPER             VALIDATE PERIOD - MONTH/YEAR                 
*                                                                               
         OC    SVNET,SVNET         ALL NETWORK REQUEST ?                        
         BZ    VK22                 NO                                          
*                                                                               
         BRAS  RE,VMYNET           VALIDATE MY NETWORK                          
         BNE   VK13                GET NEXT NETWORK                             
*                                                                               
         LA    R2,TRAFLTRH                                                      
         BRAS  RE,VOPT             GO VALIDATE OPTIONS                          
         OI    4(R2),X'20'         SET ON VALIDATED                             
*                                                                               
VK22     TM    SVOPTSW,OPTTEST     IF TEST, FORCE REPORT TO PRINT               
         BZ    *+8                                                              
         MVI   SVTN1PR4,C'N'       FORCE REPORT TO PRINT FOR TEST               
*                                                                               
         LA    R2,TRAPROGH         PROGRAM                                      
*                                                                               
         BRAS  RE,VPROG                                                         
*                                                                               
         OI    4(R2),X'20'         SET ON VALIDATED                             
*                                                                               
         LA    R2,TRANETH          IF NET=ALL THEN BYPASS                       
         CLI   5(R2),3                                                          
         BNE   VK25                                                             
         CLC   =C'ALL',8(R2)                                                    
         BNE   VK25                                                             
         CLI   UPDSW,X'FF'         NO UNIT FOUND, BYPASS THIS NET               
         BE    VK13                GET NEXT NETWORK                             
*                                                                               
VK25     CLI   OFFLINE,C'Y'                                                     
         BNE   VK30                                                             
         L     R2,=A(WORKFIL)                                                   
*                                                                               
         TM    WHEN,X'20'          THIS RUNNING SOON                            
         BZ    *+10                                                             
         MVC   40(8,R2),=CL8'TALWRK'                                            
*                                                                               
         OPEN  ((R2),OUTPUT)                                                    
*                                                                               
         LTR   RF,RF                                                            
         BZ    VK30                                                             
         DC    H'0'                                                             
         EJECT                                                                  
* READ UNITS FOR THIS PERIOD AND PROGRAM, AND BUILD PROGRAM TABLE *             
*                                                                               
VK30     BRAS  RE,BLPRG                                                         
         BNE   NOUNTER             IF NONE FOUND, ERROR                         
*                                                                               
*                                                                               
VK32     OI    TRAPERH+4,X'20'     SET ON VALIDATED                             
         MVI   BYTE1,0             NETWORK W/UNITS FOUND                        
*                                                                               
* INITIALIZE PATTERN TABLE ADDRESSES                                            
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   VK34                                                             
*                                                                               
         L     R1,APRGTBLX          AT END OF TABLE                             
         MVC   0(8,R1),=C'*PATABLE'                                             
         LA    R1,8(R1)                                                         
         ST    R1,APATABLE         RESET ADDRESS OF START OF PATABL             
         LR    R0,R9                                                            
         A     R0,LSYSD                                                         
         SHI   R0,24                                                            
         ST    R0,MAXPATBL                                                      
*                                                                               
VK34     L     R5,APRGTBLE                                                      
*                                                                               
         MVI   TABLESW,C'Y'        SET TABLE BUILT, READY FOR SEED              
*                                                                               
         CLI   1(RA),C'*'          IS THIS A DDS TERMINAL                       
         BNE   *+12                 NOPE                                        
*                                                                               
         TM    WHEN,X'38'          TEST SOON/OVERNIGHT/DDS                      
         BZ    LRL                  NO, GO BUILD SCREEN                         
*                                                                               
         CLI   OFFLINE,C'Y'        OFFLINE?                                     
         BNE   VK40                 NO                                          
*                                                                               
         TM    SVOPTSW,OPTTEST     THIS A TEST RUN                              
         BO    VK40                 YES                                         
*                                                                               
         L     RE,TWAMASTC                                                      
         MVC   WORK+10(L'MCDATE),MCDATE-MCBLOCK(RE)                             
         GOTO1 DATCON,DMCB,(4,WORK+10),(0,WORK)                                 
*        GOTO1 DATCON,DMCB,(5,0),(0,WORK)                                       
         GOTO1 GETDAY,(R1),WORK,WORK+6                                          
         CLI   DMCB,6              IF SAT, DISALLOW                             
         BE    SATREQER                                                         
         CLC   WORK+6(3),SPACES                                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
VK40     BAS   RE,CLRSCR                                                        
         NI    TRACLTH+4,X'FF'-X'20' FORCE CLIENT CHANGED                       
*                                                                               
         TM    WHEN,X'20'          TEST SOON REQUEST                            
         BZ    EXIT                 NO                                          
*                                                                               
         CLI   OFFLINE,C'Y'        OFFLINE?                                     
         BE    VK50                                                             
*                                                                               
         CLI   TWAOFFC,C'*'        THIS A DDS TERM                              
         BE    VK50                                                             
*                                                                               
         CLC   =C'WD',AGENCY       IF AGENCY WEIDEN                             
         BE    VK50                                                             
*                                                                               
         CLC   =C'M2',AGENCY       IF AGENCY MEDIACOM                           
         BE    VK50                                                             
*                                                                               
         CLC   =C'YN',AGENCY       IF AGENCY YNRT                               
         BE    VK50                                                             
*                                                                               
         CLC   =C'H7',AGENCY       IF AGENCY MSYRT                              
         BE    VK50                                                             
*                                                                               
         CLC   =C'FM',AGENCY       IF AGENCY FMNES                              
         BNE   VK260                                                            
*                                                                               
VK50     TM    SVOPTSW,OPTTEST     AND IT IS NOT A TEST RUN ...                 
         BNZ   EXIT                                                             
*                                                                               
* SEE IF WE ARE LOCKED OUT BY UNIT ALLOCATION                                   
*                                                                               
         XC    DUB,DUB                                                          
         MVI   BYTE,0                                                           
         MVC   DUB(4),=C'TNET'     TEST                                         
         GOTO1 VALILOC,0                                                        
*                                                                               
* IF RUNNING FOR ALL PRODUCTS, SEE THAT NONE OF THE PRODS ARE LOCKED            
* NOTE - WE WILL NOW NOT CHECK FOR ALL PRODUCTS, BUT SEE IF A NULL              
* FAILS - MEANING THAT SOME PRODUCT IS LOCKED                                   
*                                                                               
         CLI   SVTN2PRO+00,C'0'    IF ZERO                                      
         BE    *+12                                                             
         CLI   SVTN2PRO+00,0       OR NULL, NOT RUNNING BY PROD/PRD GRP         
         BNE   VK130               NOT AN ALL PRODUCT RUN                       
*                                                                               
         BRAS  RE,SETSPOT                                                       
*                                                                               
         XC    SVCPROD,SVCPROD     INIT PROD                                    
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0DF1'      PROD REC                                    
         MVC   KEY+2(3),BAGYMD    & BCLT                                        
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
VK110    CLC   KEY(5),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   KEY+5,X'FF'         LAST REC FOR THIS CLT                        
         BNE   *+14                                                             
         XC    SVCPROD,SVCPROD     CLEAR PROD                                   
         B     VK120                YES, DONE                                   
*                                                                               
         MVC   SVCPROD,KEY+6       SAVE 3 CHAR PROD                             
*                                                                               
         XC    DUB,DUB                                                          
         MVI   DUB,C'T'            TEST                                         
         MVI   DUB+1,X'FF'         NET (TRAFFIC)                                
         MVC   DUB+2(4),TRANET     NETWORK                                      
*****    MVC   DUB+6(1),3(R4)      THIS PRD LOCKED?                             
         GOTO1 VALILOC,SVCPROD                                                  
         XC    DUB,DUB                                                          
         MVI   DUB,C'T'            TEST                                         
         MVI   DUB+1,X'FF'         NET (TRAFFIC)                                
         MVI   DUB+2,X'FF'         NETWORK=ALL                                  
         MVC   DUB+3(1),SVMEDIA    MEDIA                                        
******   MVC   DUB+6(1),3(R4)      THIS PRD LOCKED?                             
         GOTO1 VALILOC,SVCPROD                                                  
*                                                                               
         GOTO1 SEQ                 GET NEXT PROD REC                            
         B     VK110                                                            
*                                                                               
* LOCK OUT FOR SOON UPDATES (FOR ALL PRODUCTS)                                  
*                                                                               
VK120    XC    DUB,DUB                                                          
         MVI   DUB,C'L'            LOCK                                         
         MVI   DUB+1,X'FF'         NET (TRAFFIC)                                
         MVC   DUB+2(4),TRANET     NETWORK                                      
         CLI   TRANETH+5,3                                                      
         BNE   VK125                                                            
         CLC   =C'ALL',TRANETH+8   IF NET=ALL                                   
         BNE   VK125                                                            
         MVI   DUB+2,X'FF'                                                      
         MVC   DUB+3(1),SVMEDIA    THEN LOCK BY MEDIA                           
         XC    DUB+4(2),DUB+4                                                   
VK125    MVI   DUB+6,X'FF'         ALL PRODS                                    
         GOTO1 VALILOC,0                                                        
         B     VK200                                                            
*                                                                               
* IF RUNNING BY PRODUCT, SEE THAT THIS PRD IS NOT LOCKED                        
*                                                                               
VK130    CLI   SVTN2PRO+00,C'*'                                                 
         BNE   VK150                                                            
*                                                                               
         MVC   BYTE,SVTN2PRO+00                                                 
         XC    DUB,DUB                                                          
         MVI   DUB,C'T'            TEST                                         
         MVI   DUB+1,X'FF'         NET (TRAFFIC)                                
         MVC   DUB+2(4),TRANET     NETWORK                                      
****     MVC   DUB+6(1),OPTPRD     THIS PRD LOCKED?                             
         MVC   SVCPROD,OPTPROD                                                  
         GOTO1 VALILOC,SVCPROD                                                  
         XC    DUB,DUB                                                          
         MVI   DUB,C'T'            TEST                                         
         MVI   DUB+1,X'FF'         NET (TRAFFIC)                                
         MVI   DUB+2,X'FF'         NETWORK=ALL                                  
         MVC   DUB+3(1),SVMEDIA    MEDIA                                        
******   MVC   DUB+6(1),OPTPRD     THIS PRD LOCKED?                             
         GOTO1 VALILOC,SVCPROD                                                  
*                                                                               
         XC    DUB,DUB                                                          
         MVI   DUB,C'L'            LOCK                                         
         MVI   DUB+1,X'FF'         NET (TRAFFIC)                                
         MVC   DUB+2(4),TRANET     NETWORK                                      
         CLI   TRANETH+5,3                                                      
         BNE   VK135                                                            
         CLC   =C'ALL',TRANETH+8   IF NET=ALL                                   
         BNE   VK135                                                            
         MVI   DUB+2,X'FF'                                                      
         MVC   DUB+3(1),SVMEDIA    THEN LOCK BY MEDIA                           
         XC    DUB+4(2),DUB+4                                                   
*K135    MVC   DUB+6(1),OPTPRD     THIS PRD LOCKED?                             
VK135    GOTO1 VALILOC,SVCPROD                                                  
         B     VK200                                                            
*                                                                               
* IF RUNNING BY PGROUP, SEE THAT THIS PGROUP IS NOT LOCKED                      
*                                                                               
VK150    OC    OPTPRGR,OPTPRGR     PROD GROUP                                   
         BNZ   *+6                                                              
         DC    H'0'                BUG CATCHER                                  
*                                                                               
         XC    DUB,DUB                                                          
         MVC   BYTE,SVTN2PRO+00                                                 
         MVI   DUB,C'T'            TEST                                         
         MVI   DUB+1,X'FF'         NET (TRAFFIC)                                
         MVC   DUB+2(4),TRANET     NETWORK                                      
         MVC   DUB+6(2),OPTPRGR    THIS PGROUP LOCKED?                          
         GOTO1 VALILOC,0                                                        
         XC    DUB,DUB                                                          
         MVI   DUB,C'T'            TEST                                         
         MVI   DUB+1,X'FF'         NET (TRAFFIC)                                
         MVI   DUB+2,X'FF'         NETWORK=ALL                                  
         MVC   DUB+3(1),SVMEDIA    MEDIA                                        
         MVC   DUB+6(2),OPTPRGR    THIS PGROUP LOCKED?                          
         GOTO1 VALILOC,0                                                        
*                                                                               
         XC    DUB,DUB                                                          
         MVI   DUB,C'L'            LOCK                                         
         MVI   DUB+1,X'FF'         NET (TRAFFIC)                                
         MVC   DUB+2(4),TRANET     NETWORK                                      
         CLI   TRANETH+5,3                                                      
         BNE   VK155                                                            
         CLC   =C'ALL',TRANETH+8   IF NET=ALL                                   
         BNE   VK155                                                            
         MVI   DUB+2,X'FF'                                                      
         MVC   DUB+3(1),SVMEDIA    THEN LOCK BY MEDIA                           
         XC    DUB+4(2),DUB+4                                                   
VK155    MVC   DUB+6(2),OPTPRGR    PGROUP                                       
         GOTO1 VALILOC,0                                                        
*                                                                               
VK200    MVC   SVLOCK,DUB          SAVE LOCK INFO FOR UNLOCK                    
         MVC   SVSVPROD,SVCPROD    SAVE PROD LOCK INFO                          
*                                                                               
         MVI   TWAWHEN,5           SET UPDATIVE SOON FOR FRED                   
         B     EXIT                                                             
*                                                                               
VK260    TM    SVOPTSW,OPTTEST     THIS A TEST RUN                              
         BNZ   EXIT                 YES                                         
         MVC   GERROR,=Y(BDSOON)    NO - NOT SOONABLE                           
         LA    R2,CONWHENH                                                      
         B     FORCEVK                                                          
*                                                                               
MAXIOER1 XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'MAXIOMS1),MAXIOMS1                                     
         NI    TRACLTH+4,X'FF'-X'20' FORCE CLIENT CHANGED                       
         GOTO1 ERREX2                                                           
MAXIOMS1 DC    C'*TOO MANY I/OS, MUST SEED BY NETWORK OR OVERNIGHT*'            
*                                                                               
NOTEMSG  DC    C'AUTONOTE*SMUR:NO 0A1D REC FOUND IN 62 '                        
*                                                                               
         EJECT                                                                  
* SEE IF PERIOD OVERRIDE WAS ENTERED IN OPTION FIELD                            
*  (FOR NET=ALL REQUEST)                                                        
*                                                                               
VOPTP    NTR1                                                                   
         LA    R2,TRAFLTRH                                                      
*                                                                               
         CLC   =C'MSYRT ',AGYORIG  SIGN ON MSYRT (MEDIA REQ)                    
         BE    *+14                                                             
         CLC   =C'MCNYB ',AGYORIG  SIGN ON MCNYB (MEDIA REQ)                    
         BNE   VOPTP05                                                          
*                                                                               
         CLI   5(R2),0             ANY ENTRY                                    
         BE    NOMEDERR                                                         
         B     VOPTP06                                                          
*                                                                               
VOPTP05  CLI   5(R2),0             ANY ENTRY                                    
         BNE   VOPTP06                                                          
*                                                                               
         TM    WHEN,X'10'+X'08'    ALLOW FOR OV OR DDS                          
         BNZ   VOPTPX               DONE                                        
         B     NOMEDERR                                                         
*                                                                               
VOPTP06  MVI   SVOPTMED,0          INIT MEDIA                                   
*                                                                               
         LA    R4,BLOCK+240        ADDRESS OF FIRST BLOCK                       
         GOTO1 SCANNER,DMCB,TRAFLTRH,(7,(R4))                                   
         LLC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3               SEE IF SCANNER FOUND ANYTHING                
         BZ    MISSERR             NO                                           
*                                                                               
VOPTP10  LLC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
         EX    R1,TESTCLC                                                       
         BNE   *+12                                                             
         OI    SVOPTSW,OPTTEST     TEST RUN                                     
         B     VOPTP30                                                          
*                                                                               
         EX    R1,BROACLC          BROADCAST?                                   
         BNE   *+16                                                             
         MVI   MYTN2PR6,C'B'                                                    
         OI    SVOPTSW1,OPTPER    PERIOD OVERRIDE ENTERED                       
         B     VOPTP30                                                          
*MNV                                                                            
         EX    R1,DIGICLC                                                       
         BNE   *+12                                                             
         OI    SVOPTSW1,OPTDIGI     OVERRIDE SUPPRESS DIGITAL                   
         B     VOPTP30                                                          
*MNV                                                                            
*                                                                               
* SEED BY MEDIA (ALL=CAB/SYN/NET/OTHER)                                         
*                                                                               
         EX    R1,ALLCLC                                                        
         BE    VOPTP15                                                          
         EX    R1,NTPCLC                                                        
         BNE   VOPTP20                                                          
*                                                                               
VOPTP15  OI    SVOPTSW,OPTMED      ALL NETWORK REQUEST                          
         CLI   1(R4),0                                                          
         BE    NOMEDERR            MUST ENTER MEDIA                             
*                                                                               
         MVC   SVOPTMED,22(R4)     SAVE MEDIA                                   
*                                                                               
*MNV                                                                            
         CLI   SVOPTMED,C'V'                                                    
         BE    VOPTP30                                                          
*MNV                                                                            
         CLI   SVOPTMED,C'S'                                                    
         BE    VOPTP30                                                          
         CLI   SVOPTMED,C'C'                                                    
         BE    VOPTP30                                                          
         CLI   SVOPTMED,C'N'                                                    
         BE    VOPTP30                                                          
         CLI   SVOPTMED,C'O'                                                    
         BE    VOPTP30                                                          
         CLI   SVOPTMED,C'D'                                                    
         BE    VOPTP30                                                          
         CLI   SVOPTMED,C'H'                                                    
         BE    VOPTP30                                                          
         B     BDMEDERR                                                         
*                                                                               
VOPTP20  EX    R1,CALCLC           CALENDAR                                     
         BNE   *+16                                                             
         MVI   MYTN2PR6,C'C'                                                    
         OI    SVOPTSW1,OPTPER    PERIOD OVERRIDE ENTERED                       
         B     VOPTP30                                                          
*                                                                               
         CLI   12(R4),C'#'         THIS T/A REQUEST FROM PAT GEN                
         BNE   VOPTP26                                                          
         CLI   OFFLINE,C'Y'        ONLY GOOD OFFLINE                            
         BNE   VOPTP30                                                          
         TM    WHEN,X'20'          THIS SOON                                    
         BO    VOPTP30                                                          
*                                                                               
         OI    SVOPTSW,OPTPGEN                                                  
         B     VOPTP30                                                          
*                                                                               
VOPTP26  DS   0H                                                                
         EX    R1,WEEKCLC          WEEKLY?                                      
         BNE   VOPTP30                                                          
         MVI   MYTN2PR6,C'W'                                                    
         OI    SVOPTSW1,OPTPER    PERIOD OVERRIDE ENTERED                       
         B     VOPTP30                                                          
*                                                                               
VOPTP30  LA    R4,32(,R4)          POINT TO NEXT BLOCK                          
         BCT   R3,VOPTP10                                                       
*                                                                               
VOPTPX   DS   0H                                                                
         CLC   =C'MSYRT ',AGYORIG  SIGN ON MSYRT (MEDIA REQ)                    
         BE    *+14                                                             
         CLC   =C'MCNYB ',AGYORIG  SIGN ON MCNYB (MEDIA REQ)                    
         BNE   VOPTPX10                                                         
*                                                                               
         CLI   SVOPTMED,0          WAS MEDIA ENTERED                            
         BE    NOMEDERR                                                         
*                                                                               
VOPTPX10 DS    0H                                                               
         TM    WHEN,X'10'+X'08'       ALLOW FOR OV OR DDS                       
         BNZ   EXIT                                                             
*                                                                               
         CLI   SVOPTMED,0          WAS MEDIA ENTERED                            
         BNE   EXIT                 YES, DONE                                   
*                                                                               
NOMEDERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NETMSG),NETMSG                                         
         B     ERREXIT                                                          
*                                                                               
BDMEDERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BDMEDMSG),BDMEDMSG                                     
*                                                                               
ERREXIT  GOTO1 ERREX2                                                           
*                                                                               
NETMSG   DC    C'* ERROR * MISSING MEDIA EG. ALL=CABLE *'                       
BDMEDMSG DC    C'* ERROR * MEDIA = N/C/S/D/H *'                                 
*                                                                               
BROACLC  CLC   12(0,R4),=C'BROADCAST'                                           
CALCLC   CLC   12(0,R4),=C'CALENDAR'                                            
WEEKCLC  CLC   12(0,R4),=C'WEEKLY'                                              
TESTCLC  CLC   12(0,R4),=C'TEST'                                                
ALLCLC   CLC   12(0,R4),=C'ALL'                                                 
NTPCLC   CLC   12(0,R4),=C'NTYPE'                                               
*MNV                                                                            
DIGICLC  CLC   12(0,R4),=C'DIGITAL'        OVERRIDE SUPPRESS DIGITAL            
*MNV                                                                            
         EJECT                                                                  
OFFLNERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'OFFLINMS),OFFLINMS                                     
         GOTO1 ERREX2                                                           
         EJECT                                                                  
* ONLINE SEED ONLY SEED EACH PROGRAM SELECTED *                                 
*                                                                               
         USING PRGTABLD,R5                                                      
LRL      BAS   RE,CLRSCR                                                        
*                                                                               
* FORCE HEADINGS TO PRINT *                                                     
*                                                                               
         NI    TRAHDG1H+1,X'FF'-X'08'-X'04'                                     
         OI    TRAHDG1H+6,X'80'                                                 
         NI    TRAHDG2H+1,X'FF'-X'08'-X'04'                                     
         OI    TRAHDG2H+6,X'80'                                                 
         LA    R2,TRASEL1H         1ST DISPLAY LINE                             
*                                                                               
         L     R5,APRGTBLE                                                      
*                                                                               
* DISPLAY 1 LINE FOR EACH PROGRAM *                                             
*                                                                               
LRL10    NI    1(R2),X'FF'-X'20'   SET UNP                                      
         OI    6(R2),X'80'         SET XMT                                      
         LR    RF,R2                                                            
         SR    RF,RA               GET DISP TO SCREEN ENTRY                     
         STCM  RF,3,PRGDISP                                                     
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         USING LSTLINE,R2                                                       
*                                                                               
LRL11    MVC   LPROG,PRGPRG                                                     
*                                                                               
         LLC   R0,PRGREVN                                                       
         EDIT  (R0),(3,LREVNO),ZERO=NOBLANK,ALIGN=LEFT                          
*                                                                               
         CLI   PRGREVN,0          ANY PREVIOUS                                  
         BNE   LRL12               YES                                          
         MVC   LINSDTE(8),=CL8'ORIGINAL'                                        
         B     LRL14                                                            
*                                                                               
LRL12    OC    PRGINSDT,PRGINSDT INWERE INSTR RUN                               
         BNZ   *+14                                                             
         MVC   LINSDTE(7),=C'NOT RUN'                                           
         B     LRL14                                                            
         GOTO1 DATCON,DMCB,(3,PRGINSDT),(8,LINSDTE)                             
*                                                                               
LRL14    LH    R0,PRGUNITS                                                      
         EDIT  (R0),(4,LUNITS),ZERO=NOBLANK                                     
*                                                                               
         EDIT  (B2,PRGFDS),(4,LFEEDS)                                           
*                                                                               
         EDIT  (B2,PRGUNAS),(5,LUNASUNT)                                        
*                                                                               
         EDIT  (B2,PRGUNAL),(5,LUNALUNT)                                        
*                                                                               
         OI    6(R2),X'80'                                                      
*                                                                               
LRL20    LA    R5,PRGNEXT                                                       
         OC    0(4,R5),0(R5)       END OF TABLE                                 
         BZ    LRL50               YES                                          
*                                                                               
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9             END OF SCREEN                                
         BH    LRL10               YES                                          
LRL40    S     R5,APRGTBLE                                                      
         ST    R5,ASVNEXT          SAVE 1ST PROGRAM FOR NEXT SCREEN             
         MVC   GERROR,=Y(MOREMSG)                                               
         B     LRL60                                                            
LRL50    S     R5,APRGTBLE         SAVE                                         
         ST    R5,ASVNEXT          SO CSEL WILL END                             
         MVC   GERROR,=Y(DONEMSG)                                               
LRL60    MVI   GMSGTYPE,C'I'       SET INFO MSG                                 
         LA    R2,TRASEL1H                                                      
         BAS   RE,SVTWA                                                         
         B     TRAPERR2                                                         
*                                                                               
         DROP  R2,R5                                                            
*                                                                               
LRLEDITA EDIT  (R0),(3,FULL),ZERO=NOBLANK,ALIGN=LEFT                            
         BR    RE                                                               
         EJECT                                                                  
* OFFLINE SEED UNITS HERE *                                                     
*                                                                               
LRR      L     R5,APRGTBLE         START OF UNIT TABLE                          
         USING PRGTABLD,R5                                                      
*                                                                               
* GO SEED UNITS FOR ALL REQUESTED PROGRAM(S) *                                  
*                                                                               
         BAS   RE,SEED             SEED SELECTED PROGRAM                        
*                                                                               
* PRINT ANY ERROR REPORT FOR PROGRAMS WITH ERRORS *                             
*                                                                               
         BRAS  RE,PER              PRINT ERROR REPORT                           
*                                                                               
         CLI   SVNET,0             NETWORK = ALL ?                              
         BE    LRRXIT               NO, ONLY ONE NETWORK                        
         LA    R2,TRANETH          POINT TO NETWORK FIELD                       
         BAS   RE,VK12                                                          
         CLC   SVNET,=X'FFFFFFFF'                                               
         BE    LRRXIT                                                           
         B     LRR                                                              
                                                                                
LRRXIT   DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*******************************************************************             
* SEED UNITS FOR THIS NETWORK/PROGRAM, USER HAS SELECTED PROGRAM, *             
* NOW FIND FIRST TABLE ENTRY FOR THIS PROGRAM, THEN BUILD UNIT    *             
* TABLE, AND SEED ALL UNITS WITH PATTERNS FOR THIS PROGRAM        *             
*******************************************************************             
*                                                                               
         USING PRGTABLD,R5                                                      
GEN      NTR1                                                                   
*                                                                               
* INITIALIZE PATTERN TABLE ADDRESS FOR ONLINE                                   
*                                                                               
         CLI   OFFLINE,C'Y'        ONLY GOOD OFFLINE                            
         BE    GEN005                                                           
*                                                                               
         L     R1,AIO2                                                          
         ST    R1,APATABLE                                                      
         LA    R1,2000-L'PATENT(,R1)                                            
         ST    R1,MAXPATBL                                                      
*                                                                               
* SEED UNITS FOR PROGRAM(S) *                                                   
*                                                                               
GEN005   BAS   RE,SEED                                                          
*                                                                               
         TM    UPDSW,FTLCMLER     CML PRD ERR FOUND                             
         BO    PRDERR      PAT PRD CML ERR,RUN SOON FOR ERR REPORT              
*                                                                               
GEN00    LLC   R6,0(R2)                                                         
         AR    R6,R2                                                            
         USING LSTLINE,R6                                                       
GEN10    CLC   PRGPRG,LPROG                                                     
         BE    GEN20                                                            
         LA    R5,PRGNEXT                                                       
         CLI   0(R5),0             AT END OF TABLE                              
         BNE   GEN10                                                            
         DC    H'0'                                                             
*                                                                               
GEN20    DS    0H                                                               
         BAS   RE,SVTWA           SAVE UPDATED INFO                             
         OI    6(R6),X'80'         SET XMT                                      
*                                                                               
* EDIT UNASSIGNED UNITS FOR USER *                                              
*                                                                               
         LH    R0,PRGUNAS                                                       
         SH    R0,PRGNEW                                                        
         EDIT  (R0),(5,LUNASUNT)                                                
*                                                                               
* EDIT NEWLY ASSIGNED CMLS AND CHANGED CMLS *                                   
*                                                                               
         EDIT  (B2,PRGNEW),(3,LNEW)                                             
*                                                                               
         EDIT  (B2,PRGCHG),(3,LCHG)                                             
*                                                                               
* DISPLAY SPOOL ID FOR USER *                                                   
*                                                                               
*        MVC   LPRTID(3),=C'ID='                                                
         MVC   LPRTID+3(3),REMUSER                                              
         MVI   LPRTID+6,C','                                                    
         SR    R0,R0                                                            
         ICM   R0,3,SPOOLRPN                                                    
         EDIT  (R0),(5,LPRTID+7),ALIGN=LEFT                                     
         TM    UPDSW,X'80'         WERE ANY PATTERNS FOUND                      
         BZ    GEN40                YES                                         
         MVC   LPRTID+13(4),=C'NONE'                                            
         EJECT                                                                  
* MODIFY SELECT FIELD *                                                         
*                                                                               
GEN40    XC    8(3,R2),8(R2)       BLANK IT                                     
         MVI   8(R2),C'*'          MARK DONE                                    
         OI    6(R2),X'80'+X'20'   TRANSMIT AND CHANGE TO PROTECTED             
*                                                                               
GEN44    LA    R5,PRGNEXT                                                       
         CLI   0(R5),0             AT END OF TABLE                              
         BE    EXIT                                                             
*                                                                               
         LLC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         LLC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         TM    1(R2),X'20'         PROTECTED                                    
         BO    EXIT                                                             
         CLI   0(R2),9                                                          
         BH    GEN00                                                            
         B     EXIT                                                             
*                                                                               
         DROP  R6                                                               
*                                                                               
TSUPPERR SR    R2,R2                                                            
         MVC   GERROR,=Y(BDTSUPP)                                               
         GOTO1 VTRAERR                                                          
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* SUBROUTINE TO BUILD PATTERN TABLE, THEN SEED UNITS WITH COMMERCIALS *         
*                                                                     *         
***********************************************************************         
*                                                                               
SEED     NTR1                                                                   
*                                                                               
         MVC   NEXTADDR,APATABLE   PATTERN TABLE BUILT IN AIO2 ONLINE           
*                                                                               
         NI    UPDSW,X'FF'-X'08'   SET OFF BILLBOARD OR POS FOUND SW            
*                                                                               
* BUILD UNIT TABLE FOR PROGRAM(S) *                                             
*                                                                               
         GOTOR BLUNT,DMCB,(RC)                                                  
         BNE   EXIT                NO UNITS                                     
*                                                                               
* BUILD PATTERN TABLE FOR PROGRAM(S) AND SEED COMMLS *                          
*                                                                               
         BRAS  RE,BLPAT                                                         
         TM    UPDSW,FTLCMLER     CML PRD ERR FOUND                             
         BO    EXIT                                                             
*                                                                               
SEED05   OI    PRGFLG,X'40'       SET ON PROGRAM SEEDED FLAG                    
         DROP  R5                                                               
         EJECT                                                                  
* OPEN PRINT QUE *                                                              
*                                                                               
SEED10   CLI   PQSW,1              TEST PRTQUE OPEN                             
         BNE   SEED12              YES                                          
         LA    R1,ELEM                                                          
         ST    R1,SPOOLQLK                                                      
         XC    ELEM(128),ELEM                                                   
         USING PQPLD,R1                                                         
*NOP     MVC   QLRETNL,=H'36'                                                   
******   MVC   QLRETND,=H'12'                                                   
         MVI   QLEXTRA,X'FF'       NEW PARM LIST                                
         OI    SPOOLIND,X'40'      USER VALUES PRESENT                          
         GOTO1 OPENPQ                                                           
         DROP  R1                                                               
*                                                                               
* SET UP PRINTING ADDRESSABILITY (AFTER PRTQUE OPEN) *                          
*                                                                               
SEED12   L     R1,=A(HEADING)      HEADING LINE FOR REPORT                      
         L     RF,SPTR62RR                                                      
         AR    R1,RF                                                            
         ST    R1,SPECS            STORE FOR CONTROLLER                         
         L     R1,=A(HDHK)         HEADING ROUTINE FOR REPORT                   
         AR    R1,RF                                                            
         ST    R1,HEADHOOK         STORE FOR CONTROLLER                         
         MVI   MAXLINES,60         SET PAGE SIZE                                
*                                                                               
* UPDATE PROGRAM TABLE WITH PROGRAM REV #                                       
*                                                                               
SEED34   DS    0H                                                               
*                                                                               
         L     RE,APRGTBLE                                                      
         USING PRGTABLD,RE                                                      
*                                                                               
         MVI   SVHIREVP,0          INIT PRG HIGHEST REV #                       
*                                                                               
SEED34B  DS    0H                                                               
*NOP     TM    PRGFLG,X'10'        NEED TO ADD ORG REV REC                      
*        BO    SEED34D                                                          
*        TM    PRGFLG,PRGFLTRN    WERE INSTR RUN FOR THIS REVISION              
*        BZ    SEED34D             NO                                           
*        SPACE                                                                  
*        LLC   R1,PRGREVN                                                       
*        LA    R1,1(,R1)    <<<<--- MOVED TO FREV ROUTINE                       
*        STC   R1,PRGREVN                                                       
******** SPACE                                                                  
SEED34D  CLC   PRGREVN,SVHIREVP                                                 
         BNH   *+10                                                             
         MVC   SVHIREVP,PRGREVN     SAVE HIGHEST PROG REV #                     
*                                                                               
         LA    RE,PRGNEXT                                                       
         OC    PRGPRG,PRGPRG       SEE IF END OF TABLE                          
         BNZ   SEED34B                                                          
*        C     RE,APRGTBLX          AT END OF TABLE                             
*        BL    SEED34B                                                          
*                                                                               
         DROP  RE                                                               
*                                                                               
         L     R5,AUNTABLE                                                      
         USING UNTABLED,R5                                                      
*        CLI   SVTN1PR4,C'Y'       DON'T PRINT REPORT                           
*        BE    SEED40                                                           
*                                                                               
* PRINT ALL UNITS ON SEED REPORT *                                              
*                                                                               
         BRAS  RE,BLIN             GO BUILD PRINT LINE(S)                       
*                                                                               
* UPDATE REVISION RECORD AND ALL UPDATED UNITS *                                
*                                                                               
SEED40   DS   0H                                                                
         TM    UPDSW,X'80'         FIND ANY PATTERNS                            
         BZ    SEED50               YES                                         
         TM    SVFLAG,SVDELPAT     WERE THERE ANY DELETED PAT                   
         BO    SEED50                                                           
*                                                                               
         CLI   SVTN1PR9,C'Y'       BYPASS ASSIGNED CMLS                         
         BE    EXIT                 YES, DONE                                   
*                                                                               
         L     R5,AUNTABLE                                                      
*                                                                               
SEED45   TM    UNTFLG,UNTFLGCH     CHG FLAG FOR REV REC UPDATE                  
         BO    SEED50              YES, GO UPDATE RECORDS                       
*                                                                               
         LA    R5,UNTNEXT                                                       
         CLI   UNTADTEP,0          END OF UNITS                                 
         BE    EXIT                                                             
         B     SEED45                                                           
*                                                                               
SEED50   BRAS  RE,UPDR                                                          
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* RTN TO WRITE TWA2 WITH EXTENDED LIST BEFORE EXIT *                            
*                                                                               
SVTWA    NTR1                                                                   
         XC    DMCB(24),DMCB                                                    
         LA    R1,=C'DMWRT'                                                     
         B     COMTWA                                                           
*                                                                               
* RESTORE PRGTABLE AND UNIT TABLE *                                             
*                                                                               
RDTWA    NTR1                                                                   
         XC    DMCB(24),DMCB                                                    
         LA    R1,=C'DMREAD'                                                    
*                                                                               
COMTWA   ST    R1,DMCB                                                          
         L     RE,ATWA                                                          
         MVC   DMCB+10(2),2(RE)    MOVE TERMINAL NUMBER                         
         MVI   DMCB+8,2            SET PAGE 2                                   
         GOTO1 DATAMGR,DMCB,,=C'TEMPSTR',,ASVSTOR                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
*                                                                               
* CLEAR DISPLAY AREA OF SCREEN *                                                
*                                                                               
CLRSCR   LA    RF,TRASEL1H                                                      
*                                                                               
CLRSCR10 OC    8(L'TRASEL1,RF),8(RF)                                            
         BZ    CLRSCR20                                                         
         CLC   8(L'TRASEL1,RF),SPACES                                           
         BE    CLRSCR20                                                         
         XC    8(L'TRASEL1,RF),8(RF)                                            
         OI    6(RF),X'80'                                                      
         OI    1(RF),X'20'         SET PROTECT BIT                              
CLRSCR20 LLC   R0,0(RF)                                                         
         AR    RF,R0                                                            
*                                                                               
         OC    8(L'TRADSP1,RF),8(RF)                                            
         BZ    CLRSCR30                                                         
         CLC   8(L'TRADSP1,RF),SPACES                                           
         BE    CLRSCR30                                                         
         XC    8(L'TRADSP1,RF),8(RF)                                            
         OI    6(RF),X'80'                                                      
*                                                                               
CLRSCR30 IC    R0,0(RF)                                                         
         AR    RF,R0                                                            
         CLI   0(RF),9                                                          
         BH    CLRSCR10                                                         
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
         EJECT                                                                  
*        ERROR ROUTINES                                                         
*                                                                               
DONEALL  MVC   GERROR,=Y(ENDMSG)                                                
         MVI   GMSGTYPE,C'I'       SET INFO MSG                                 
         LA    R2,TRACLTH                                                       
         NI    TRACLTH+4,X'FF'-X'20' SET OFF VALIDATED                          
         B     TRAPERR2                                                         
*                                                                               
NOUNTER  DS    0H                                                               
         BRAS  RE,GOPER            GO PRINT ERROR REPORT                        
*                                                                               
         CLI   SVNET,0             ALL NETWORK REQUEST                          
         BNE   VK13                 YES GO GET NEXT NETWORK                     
*                                                                               
         MVC   GERROR,=Y(NOUNTSEL)                                              
         LA    R2,TRACLTH                                                       
         B     FORCEVK                                                          
*                                                                               
SATREQER XC    CONHEAD,CONHEAD                                                  
         L     R1,=A(SATREQMS)                                                  
         A     R1,SPTR62RR                                                      
         MVC   CONHEAD(L'SATREQMS),SATREQMS                                     
         GOTO1 ERREX2                                                           
BDCMLN   MVI   ERROR,INVCMMLN                                                   
         B     TRAPERR                                                          
NOCOMREC MVI   ERROR,NOCOMM                                                     
         B     TRAPERR                                                          
INVCMLER MVI   ERROR,INVCOMM                                                    
         B     TRAPERR                                                          
PRDINV   MVI   ERROR,INVPRDCD                                                   
         B     TRAPERR                                                          
*                                                                               
MISSELER TM    UPDSW,X'10'         ANY UPDATES DONE                             
         BZ    MISSERR             NO                                           
         BAS   RE,SVTWA            SAVE UPDATED UNIT TABLE                      
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
PRDERR   MVC   GERROR,=Y(PATPROER)                                              
         LA    R2,CONWHENH                                                      
         B     TRAPERR2                                                         
NOWERR   MVI   ERROR,INVPRINT                                                   
         LA    R2,CONWHENH                                                      
         B     TRAPERR                                                          
NUMERR   MVI   ERROR,NOTNUM                                                     
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
FORCEVK  NI    TRACLTH+4,X'FF'-X'20' FORCE CLIENT CHANGED                       
TRAPERR2 GOTO1 VTRAERR                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
OFFLINMS DC    C'* ERROR * MUST RUN OVERNIGHT *'                                
SATREQMS DC    C'* ERROR * NO REQUESTS ON SATURDAY *'                           
         EJECT                                                                  
HEADING  SSPEC H1,1,AGYNAME                                                     
         SSPEC H2,1,AGYADD                                                      
         SSPEC H4,1,C'CLIENT'                                                   
         SSPEC H5,1,C'NETWORK'                                                  
*                                                                               
         SSPEC H1,41,C'COMMERCIAL SEED REPORT'                                  
         SSPEC H2,41,C'----------------------'                                  
         SSPEC H3,40,C'PERIOD'                                                  
*                                                                               
         SSPEC H1,85,REPORT                                                     
         SSPEC H2,85,RUN                                                        
         SSPEC H3,85,PAGE                                                       
         SSPEC H3,93,REQUESTOR                                                  
         SSPEC H7,1,C'PROGRM NAME'                                              
         SSPEC H8,1,C'------ --------------'                                    
         SSPEC H7,23,C'D'                                                       
         SSPEC H8,23,C'P'                                                       
         SSPEC H7,26,C'DATE'                                                    
         SSPEC H8,26,C'-----------'                                             
         SSPEC H7,38,C'FEED'                                                    
         SSPEC H8,38,C'----'                                                    
         SSPEC H7,43,C'PRD NAME            '                                    
         SSPEC H8,43,C'--- ----------------'                                    
         SSPEC H7,64,C'COMMERCIAL  '                                            
         SSPEC H8,64,C'------------'                                            
         SSPEC H7,77,C'TITLE'                                                   
         SSPEC H8,77,C'------------------------'                                
         SSPEC H7,102,C'LEN'                                                    
         SSPEC H8,102,C'---'                                                    
*        SSPEC H7,103,C'TYPE'                                                   
*        SSPEC H8,103,C'----'                                                   
         DC    X'00'               END MARKER FOR SSPECS                        
*                                                                               
* MODEL DCB FOR ERROR LISTING-MOVED TO CORE RESIDENT AREA FOR USE               
         DS    0D                                                               
WORKFIL  DCB   BLKSIZE=4800,                                           X        
               DDNAME=WORKFIL,                                         X        
               DSORG=PS,                                               X        
               LRECL=48,                                               X        
               MACRF=PM,                                               X        
               RECFM=FB                                                         
         EJECT                                                                  
         DROP  R7                                                               
         EJECT                                                                  
*                                                                               
* RESET FILES TO SPOT *                                                         
*                                                                               
         USING GEND,RC                                                          
SETSPOT DS    0H                                                                
         MVI   SYSDIR,C'S'                                                      
         MVI   SYSDIR+1,C'P'                                                    
         MVI   SYSDIR+2,C'T'                                                    
         MVC   SYSFIL(3),SYSDIR                                                 
         MVI   DATADISP+1,24       SET FROM NET TO SPOT                         
         MVI   LKEY+1,13                                                        
         MVI   LSTATUS+1,1                                                      
         BR    RE                                                               
*                                                                               
* RESET FILES TO NET *                                                          
*                                                                               
SETNET   DS   0H                                                                
         MVI   SYSDIR,C'U'                                                      
         MVI   SYSDIR+1,C'N'                                                    
         MVI   SYSDIR+2,C'T'                                                    
         MVC   SYSFIL(3),SYSDIR                                                 
         MVI   DATADISP+1,27       SET FROM SPOT TO NET                         
         MVI   LKEY+1,20                                                        
         MVI   LSTATUS+1,1                                                      
         BR    RE                                                               
*                                                                               
* POINT TO EXTENDED SPOT FILE                                                   
*                                                                               
SETXSP   DS    0H                                                               
         MVI   SYSDIR,C'X'                                                      
         MVI   SYSDIR+1,C'S'                                                    
         MVI   SYSDIR+2,C'P'                                                    
         MVC   SYSFIL(3),SYSDIR                                                 
         MVI   LKEY+1,32          DETAILS OF DIRECTORY AND KEY                  
         MVI   DATADISP+1,42                                                    
         MVI   LSTATUS+1,4                                                      
         BR    RE                                                               
         EJECT                                                                  
*==============================================================                 
* ALLOCATE ADDITIONAL STORAGE IN T2168C                                         
* DO A MAINT CALL TO LOAD IT AT END OF THIS PHASE                               
* SINCE REAL NODE IS USED BY SPTRA04/07.                                        
*==============================================================                 
                                                                                
INIT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A50'  QSORT IS T00A50                          
         GOTO1 CALLOV,DMCB                                                      
         MVC   QSORT,0(R1)                                                      
*                                                                               
         MVC   DMCB+4(4),=X'D9000AFE'    GET ADDRESS OF TRPACK                  
         GOTO1 CALLOV,DMCB                                                      
         MVC   VTRPACK,0(R1)                                                    
                                                                                
*                                                                               
         LA    RE,ASVNEXT                                                       
         ST    RE,ASVSTOR                                                       
         AHI   RE,6144            LENGTH OF SAVED AREA                          
         LR    RF,R9                                                            
         A     RF,LSYSD            FIND END OF SYSD                             
         CR    RE,RF              ASSURE MORE THAN SYSD NOT RESTORED            
         BNH   *+6                                                              
         DC    H'0'                USED MORE THAN SYSD                          
         LR    RE,R9                                                            
         AHI   RE,ENDSYSD-SYSD                                                  
         CR    RE,RF              ASSURE NOT RUNNING PAST END SYSD              
         BNH   *+6                                                              
         DC    H'0'                END OF TABLES PAST END OF SYSD               
*                                                                               
         LHI   RE,PRGTABLE-SYSD                                                 
         AR    RE,R9                                                            
         ST    RE,APRGTBLE                                                      
         AHI   RE,PRGTBLX-PRGTABLE                                              
         ST    RE,APRGTBLX                                                      
*                                                                               
         LHI   RE,NETBLK-SYSD                                                   
         AR    RE,R9                                                            
         ST    RE,ANETBLK                                                       
*                                                                               
         LHI   R1,UNTABLE-T216FFD                                               
         AR    R1,RA                                                            
         SRL   R1,4                                                             
         SLL   R1,4                DOUBLE WORD BOUNDARY                         
         ST    R1,AUNTABLE                                                      
         AHI   R1,ENDUNTBL-UNTABLE UNIT TABLE LENGTH                            
         ST    R1,AUNTABLX                                                      
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    INIT05                                                           
*                                                                               
         XC    DMCB(24),DMCB       ONLINE STORAGE ALLOCATION                    
         L     RE,=V(ENDOFMOD)                                                  
         A     RE,SPTR62RR                                                      
         ST    RE,DMCB                                                          
         MVC   DMCB+4(4),=X'D902168C'  LOAD T2168C                              
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,0(R1)            GET PHASE ADDRESS                            
         LA    RE,16(RE)           CAN USE FROM +16                             
         MVC   0(8,RE),=C'*ADIDTAB'                                             
         LA    RE,8(RE)                                                         
*                                                                               
         ST    RE,AADIDTAB                                                      
         AHI   RE,1600             100X 16 BYTE ENTRIES                         
         ST    RE,AADIDTBX                                                      
         LA    RE,8(RE)                                                         
*                                                                               
         MVC   0(8,RE),=C'*PATROT*'                                             
         LA    RE,8(RE)                                                         
         ST    RE,APATROT          PATTERN ROTATION FOR ONLINE                  
         LA    RE,600(RE)          LEN OF BLOCK                                 
         ST    RE,APATROTX                                                      
         B     INITX                                                            
*MN                                                                             
INIT05   L     RE,ATWA                                                          
         L     RE,TWADCONS-T216FFD(,RE)                                         
         L     RE,TSPFUSER-TWADCOND(,RE) GET DCB ADDRESS                        
         OC    0(4,RE),0(R4)                                                    
         BNZ   INIT10                                                           
*                                                                               
         L     R0,=F'640000'                                                    
         ST    R0,GMBUFL                                                        
         GETMAIN R,LV=(0),LOC=(ANY,ANY)                                         
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,GMBUFF                                                        
*                                                                               
         L     RE,ATWA                                                          
         L     RE,TWADCONS-T216FFD(,RE)                                         
         L     RE,TSPFUSER-TWADCOND(,RE) GET DCB ADDRESS                        
         MVC   0(4,RE),GMBUFF    SAVE ADDRESS OF GETMN BUFFER                   
         B     INIT20                                                           
*                                                                               
INIT10   DS    0H                                                               
         L     RE,ATWA                                                          
         L     RE,TWADCONS-T216FFD(,RE)                                         
         L     RE,TSPFUSER-TWADCOND(,RE) GET DCB ADDRESS                        
         MVC   GMBUFF,0(RE)                                                     
*                                                                               
INIT20   DS    0H                                                               
         L     R1,GMBUFF                                                        
         ST    R1,AUNTABLE                                                      
         L     R0,=F'640000'                                                    
         AR    R0,R1                                                            
         ST    R0,AUNTABLX                                                      
*MN                                                                             
         L     R1,VADUMMY                                                       
         MVC   8(8,R1),=C'**ADID**'                                             
         LA    R1,16(R1)                                                        
         ST    R1,AADIDTAB         ADDRESS OF ADID TABLE                        
         A     R1,=F'28480'       1780X16                                       
         ST    R1,AADIDTBX                                                      
*                                                                               
         MVC   0(8,R1),=C'*PATROT*'                                             
         LA    R1,8(R1)                                                         
         ST    R1,APATROT          PATTERN ROTATION FOR ONLINE                  
         AHI   R1,960              60X16                                        
         ST    R1,APATROTX                                                      
*                                                                               
         LHI   RE,PRGTABLE-SYSD                                                 
         AR    RE,R9                                                            
         ST    RE,APRGTBLE                                                      
         AHI   RE,PRGTBLMO+PRGTBLX-PRGTABLE  INCREASE PRGTABLE                  
         ST    RE,APRGTBLX            TO FIT 160 MORE ENTRIES                   
*                                                                               
INITX    DS   0H                                                                
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* READ THROUGH BUYS AND BUILD UNIT TABLE FOR 1/ALL PROGRAMS FOR NET *           
*********************************************************************           
*                                                                               
BLUNT    NMOD1 0,**BLUN**,R7                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
*                                                                               
         L     R3,ANETBLK                                                       
         USING NETBLCKD,R3                                                      
*                                                                               
         L     R0,AIO3                                                          
         LA    R1,1250             CLEAR EST/PRD/COPY CODE TABLE AREA           
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R0,AADIDTAB         CLEAR ADID TABLE                             
         L     R1,AADIDTBX                                                      
         SR    R1,R0                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R1,PRGTU                                                         
         LA    R0,TOTCTRS                                                       
BLUNT00  DS    0H                                                               
         ZAP   0(4,R1),=P'0'                                                    
         LA    R1,4(,R1)                                                        
         BCT   R0,BLUNT00                                                       
*                                                                               
         L     R5,APRGTBLE                                                      
         USING PRGTABLD,R5                                                      
         MVC   SVPRGENT,PRGENT                                                  
         DROP  R5                                                               
*                                                                               
         ZAP   UNITCNT,=P'0'       CLEAR UNIT COUNTER                           
         XC    FULL,FULL                                                        
*                                                                               
         MVC   BASEPRG,PROGRAM                                                  
         MVC   BASEDTS,STDATEP                                                  
         OC    OPTDAT2,OPTDAT2                                                  
         BZ    *+10                                                             
         MVC   BASEEND,OPTDAT2                                                  
*                                                                               
         OC    OPTDATE,OPTDATE                                                  
         BZ    *+10                                                             
         MVC   BASESTR,OPTDATE                                                  
*                                                                               
         MVI   BASECT,0                                                         
*                                                                               
         BRAS  RE,NETI              INITIALIZE NETIO                            
*                                                                               
* CLEAR FIRST UNIT ACTIVITY LIST BUILD AREA *                                   
*                                                                               
         L     R5,AUNTABLE                                                      
         XC    0(256,R5),0(R5)                                                  
         USING UNTABLED,R5                                                      
*                                                                               
* FIRST READ - MUST IGNORE BAD NBSUBMSK SETTINGS *                              
*                                                                               
BLUNT02  DS    0H                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY CLIENT                  
         BZ    BLUNT03                                                          
*                                                                               
         CLI   T216FFD+6,C'$'      USER SIGN ON OFFICE LIST                     
         BNE   *+12                                                             
         OI    NBINDS5,NBI5PLST    SET OFFICE LIST FLAG                         
         B     *+12                                                             
         CLI   T216FFD+6,C'*'      USER SIGN ON BY OFFICE                       
         BNE   BLUNT03              NO                                          
*                                                                               
         MVC   NBTRAPSC,T216FFD+7  BRAND LEVEL OFFICE(LIST)                     
*                                                                               
BLUNT03  GOTO1 ANETIO,DMCB,(R3)                                                 
         CLI   NBERROR,NBGOOD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   NBMODE,NBREQLST                                                  
         BE    BLUNT70                                                          
         CLI   NBMODE,NBPROCUN                                                  
         BNE   BLUNT02                                                          
         EJECT                                                                  
* SEE IF EQV UNIT, IF SO FORCE TO BASE PROG CODE, UPDATE SVPRG *                
*                                                                               
BLUNT04  BAS   RE,CEQV                                                          
         BNE   BLUNT02                                                          
*                                                                               
         SR    R4,R4               SET NEED PROGRAM REC SW                      
*                                                                               
* PROCESS UNITS *                                                               
*                                                                               
BLUNT10  L     R6,NBAIO                                                         
         BRAS  RE,SETNET                                                        
*                                                                               
* SEE IF THIS UNIT HAS 5 SECONDS TAG                                            
*                                                                               
         MVI   ELCODE,X'60'                                                     
         LA    R6,27(R6)                                                        
BLUNTG10 BRAS  RE,NEXTEL                                                        
         BNE   BLUNT12                                                          
*                                                                               
         CLI   2(R6),C'Q'          IS THIS A TAG ELEMENT                        
         BNE   BLUNTG10                                                         
         CLI   3(R6),C'T'          T=TAG                                        
         BNE   BLUNTG10                                                         
*                                                                               
         PACK  DUB,4(1,R6)                                                      
         CVB   R0,DUB              TAGS IN BINARY                               
         MHI   R0,5                TIMES 5 (5 SECONDS PER TAG)                  
         LLC   R1,NBLEN            TOTAL UNIT LEN                               
         SR    R1,R0               MINUS TAG LEN                                
         STC   R1,NBLEN            = ACTUAL UNIT LENGTH                         
*                                                                               
BLUNT12  L     R6,NBAIO                                                         
*                                                                               
         CLC   STDATEP,NBACTDAT                                                 
         BH    BLUNT14                                                          
         CLC   ENDATEP,NBACTDAT                                                 
         BNL   *+6                                                              
BLUNT14  DC    H'0'                                                             
*                                                                               
* SEE IF MEDIA OR TRAFFIC SKED UNIT *                                           
*                                                                               
         CLI   SVTNPR9,C'B'        BOTH MEDIA AND SKED                          
         BE    BLUNT16                                                          
         CLI   SVTNPR9,C'S'        IF NOT SKED, MEDIA DEFAULT                   
         BNE   *+16                                                             
         CLI   NBACTSUB,C'A'       SKED ONLY                                    
         BL    BLUNT60                                                          
         B     BLUNT16                                                          
*                                                                               
         CLI   NBACTSUB,C'A'                                                    
         BNL   BLUNT60                                                          
*                                                                               
BLUNT16  XC    UNTENT(L'UNTENT+2),UNTENT                                        
*                                                                               
* BYPASS MISSED & DELETED UNITS *                                               
*                                                                               
         TM    NBUNITST,X'42'     PREEMPT OR MISSED                             
         BNZ   BLUNT60             BYPASS                                       
*                                                                               
         TM    22(R6),X'80'       THIS RECORD DELETED                           
         BO    BLUNT60             BYPASS                                       
*                                                                               
         CLI   NBACTSUB,C'A'       THIS A SKED UNIT                             
         BNL   BLUNT17              YES, NO EST CK                              
*                                                                               
         TM    NBUNST3,X'40'       IS THIS A COPY SPLIT                         
         BZ    *+8                                                              
         BAS   RE,BCSPRD            YES, BUILD C/S LIST                         
*                                                                               
         BRAS  RE,CKEST              GO CK EST FOR COPY CODE N                  
         BE    BLUNT60                BYPASS                                    
         EJECT                                                                  
* CK PRODUCT OR PRODUCT GROUP FILTER *                                          
*                                                                               
BLUNT17  DS   0H                                                                
         CLI   OPTPROD,0                                                        
         BE    BLUNT17F                                                         
         CLC   NBPR1CL3,OPTPROD                                                 
         BE    BLUNT17F                                                         
*                                                                               
         LA    RE,L'SVCSPROD                                                    
         LA    RF,SVCSPROD                                                      
BLUNT17C CLI   0(RF),0                                                          
         BE    BLUNT60                                                          
         CLC   OPTPROD,0(RF)                                                    
         BE    BLUNT17F                                                         
         LA    RF,3(,RF)                                                        
         BCT   RE,BLUNT17C                                                      
         B     BLUNT60                                                          
*                                                                               
BLUNT17F OC    OPTPRGR,OPTPRGR    ANY PRODUCT GROUP                             
         BZ    *+12                 NO                                          
         BAS   RE,FPGR             FILTER ON PRODUCT GROUP                      
         BNE   BLUNT60                                                          
*                                                                               
* CHECK BRAND LEVEL SECURITY                                                    
*                                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY CLIENT                  
         BZ    BLUNT17X                                                         
*                                                                               
         LA    R2,ELEM             FAKE VALIPRD                                 
         XC    ELEM,ELEM                                                        
         MVC   ELEM(8),=X'0A01000184030001'                                     
         MVC   ELEM+8(3),NBPR1CL3                                               
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
*                                                                               
         GOTO1 VALIPRD                                                          
         MVI   ERROPT,0                                                         
*                                                                               
         LA    R2,TRAFLTRH         CLIENT                                       
*                                                                               
         CLI   ERROR,0             ANY ERROR?                                   
         BNE   BLUNT60              BYPASS UNIT                                 
*                                                                               
BLUNT17X CLI   OPTLEN,0            ANY LENGTH FILTER?                           
         BE    *+14                 NO                                          
         CLC   NBLEN,OPTLEN        SAME LENGTH?                                 
         BNE   BLUNT60              NO                                          
*                                                                               
* SEE IF EQV UNIT, IF SO FORCE TO BASE PROG CODE, UPDATE SVPRG *                
*                                                                               
         BAS   RE,CEQV                                                          
         BNE   BLUNT60                                                          
*                                                                               
* READ PROGRAM RECORD FOR STANDARD PROGRAM INFORMATION ONE TIME ONLY *          
*                                                                               
         LTR   R4,R4               CT STILL ZERO                                
         BNZ   BLUNT20                                                          
*                                                                               
         BCTR  R4,0                SET SW TO READ PROG REC                      
*                                                                               
         BRAS  RE,SETSPOT          SET FROM NET TO SPOT                         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING NPGKEY,R1                                                        
         MVC   NPGKTYP,=X'0D20'                                                 
         MVC   NPGKAM,BAGYMD                                                    
         MVC   NPGKNET,NETMKT                                                   
         MVC   NPGKPROG,SVPRG                                                   
         MVC   NPGKEND,NBACTDAT                                                 
         DROP  R1                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE                                                  
         BE    BLUNT18                                                          
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         LA    R1,KEY                                                           
         USING NPGKEY,R1                                                        
         MVC   NPGKPROG,SVACTPRG                                                
         DROP  R1                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE                                                  
         BE    BLUNT18                                                          
         DC    H'0'                                                             
BLUNT18  L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         MVI   ELCODE,X'92'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NPGEL92,R6                                                       
         MVC   SVPRGDAY,NPGDAY                                                  
         MVC   SVPRGTIM,NPGTIME                                                 
         MVC   SVPRGNM,NPGNAME                                                  
*                                                                               
* BLANK FILL END OF PROGRAM NAME *                                              
*                                                                               
         LA    R0,16                                                            
         LA    R1,SVPRGNM+15                                                    
         CLI   0(R1),0                                                          
         BNE   *+16                                                             
         MVI   0(R1),C' '                                                       
         BCTR  R1,0                                                             
         BCT   R0,*-14                                                          
         DC    H'0'                                                             
*                                                                               
         CLI   NPGROT,0            NPGROT OVERRIDES DAY IF PRESENT              
         BE    *+10                                                             
         MVC   SVPRGDAY,NPGROT                                                  
*                                                                               
         MVI   ELCODE,X'E3'        TRAFFIC OVERRIDE ELEM                        
         BRAS  RE,NEXTEL                                                        
         BNE   BLUNT19                                                          
         USING NPGELE3,R6                                                       
         MVC   SVPRGDAY,NPGTDAY                                                 
BLUNT19  BRAS  RE,SETNET           SET FROM SPOT TO NET                         
         DROP  R6                                                               
         L     R6,NBAIO                                                         
*                                                                               
BLUNT20  MVC   UNTADTEP,NBACTDAT                                                
         MVC   UNTADTE2,NBACTDAT   ASSUME NO ROTATION                           
         MVC   UNTTIME,NBTIME                                                   
         MVC   UNTPROG,NBACTPRG                                                 
         MVC   UNTSUB,NBACTSUB                                                  
         MVC   UNTDSKAD,NBKEY+21                                                
*                                                                               
         CLI   SVMEDIA,C'N'        BUT ONLY APPLIES TO MEDIA N                  
         BNE   BLUNT22                                                          
         CLI   SVTN3PRO+4,C'Y'     TEST SEED/ASSIGN BY UNIT DAY                 
         BE    BLUNT22F            YES - SO IGNORE ROTATION                     
*                                                                               
         FIXDT02                                                                
BLUNT22  GOTO1 DATCON,DMCB,(2,NBACTDAT),(0,WORK)                                
         GOTO1 GETDAY,DMCB,WORK,WORK+6                                          
         LLC   RF,0(R1)            GET DAY OF WEEK                              
*                                                                               
         XC    WORK,WORK                                                        
         LLC   R0,NBSDROT                                                       
         SLL   R0,25                                                            
         SR    R1,R1               CLEAR COUNTER                                
*                                                                               
         LTR   RF,RF                                                            
         BZ    BLUNT22C                                                         
         SLL   R0,1                ONLY COUNT DAYS AFTER NBACTDAT               
         BCT   RF,*-4                                                           
*                                                                               
BLUNT22C LTR   R0,R0               COUNT UNTIL NO DAYS LEFT                     
         BZ    BLUNT22E                                                         
         SLL   R0,1                                                             
         BCT   R1,BLUNT22C                                                      
*                                                                               
BLUNT22E LPR   R0,R1               GET # OF DAYS IN ROT AFTER NBACTDAT          
*                                                                               
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(2,UNTADTEP),(0,WORK)                                
         GOTO1 ADDAY,(R1),WORK,WORK+6,(R0)                                      
         FIXDT02                                                                
         GOTO1 DATCON,(R1),(0,WORK+6),(2,UNTADTE2)                              
*                                                                               
         CLC   UNTADTE2,ENDATEP    CAN NOT RUN OVER ENDATE                      
         BNH   *+10                                                             
         MVC   UNTADTE2,ENDATEP    FORCE TO ENDATE                              
*                                                                               
BLUNT22F MVC   UNTPROD,NBPR1CL3                                                 
         OC    NBPR1CL3,NBPR1CL3   IS THERE A PRODUCT?                          
         BNZ   BLUNT22K                                                         
         CLI   NBPRD,0             IS THERE BINARY PRD?                         
         BE    BLUNT22K             GET ON WITH IT                              
*                                                                               
         LA    R0,NCLSTSIZ         MAX COUNT BUG CATCHER                        
         L     R1,ASVNCLST         TABLE OF CLIENT PROD CODES                   
BLUNT22G DS   0H                                                                
         CLC   NBPRD,3(R1)         THIS A VALID PROD CODE                       
         BE    BLUNT22I                                                         
         LA    R1,4(,R1)           BUMP PROD PTR                                
         CLI   0(R1),C' '          AT END OF TABLE?                             
         BNH   *+8                  YES, ERROR                                  
         BCT   R0,BLUNT22G                                                      
         DC    H'0'                                                             
BLUNT22I DS    0H                                                               
         MVC   UNTPROD,0(R1)                                                    
BLUNT22K DS    0H                                                               
         MVC   UNTSLN,NBLEN                                                     
         MVC   UNTDP,NBACTDP                                                    
         MVC   UNTEQVCT,BASECT                                                  
*                                                                               
         CLI   MYTN2PR8,C'Y'       SHOW UNIT COST?                              
         BNE   *+18                 NO                                          
         TM    NBUNITST,X'20'      ANY ACTUAL COST?                             
         BZ    *+10                                                             
         MVC   UNTACOST,NBACTUAL   SAVE COST IN TABLE                           
*                                                                               
         OC    NBPR1CL3,NBPR1CL3                                                
         BNZ   BLUNT23                                                          
*                                                                               
         CLI   NBPRD,0             UNALLOCATED                                  
         BNE   BLUNT23                                                          
         CLI   SVCSPROD,0          UNALLOCATED                                  
         BNE   BLUNT23                                                          
*                                                                               
         OI    UNTFLG,UNTFLGUL     SET ON UNALLOCATED                           
*                                                                               
BLUNT23  TM    NBUNST3,X'40'       COPY SPLIT                                   
         BZ    BLUNT24              NO                                          
         OI    UNTFLG,UNTFLGCS     SET COPY SPLIT FLAG                          
         XC    UNTPROD,UNTPROD                                                  
         B     BLUNT30                                                          
*                                                                               
BLUNT24  OC    NBPR2CL3,NBPR2CL3                                                
         BNZ   BLUNT28                                                          
         CLI   NBPRD2,0                                                         
         BE    BLUNT30                                                          
*                                                                               
         LA    R0,NCLSTSIZ         MAX COUNT BUG CATCHER                        
         L     R1,ASVNCLST         TABLE OF CLIENT PROD CODES                   
BLUNT26  DS   0H                                                                
         CLC   NBPRD2,3(R1)         THIS A VALID PROD CODE                      
         BE    BLUNT27                                                          
         LA    R1,4(,R1)           BUMP PROD PTR                                
         CLI   0(R1),C' '          AT END OF TABLE?                             
         BNH   *+8                  YES, ERROR                                  
         BCT   R0,BLUNT26                                                       
         DC    H'0'                                                             
BLUNT27  DS    0H                                                               
         MVC   UNTPROD2,0(R1)                                                   
         B     *+10                                                             
BLUNT28  MVC   UNTPROD2,NBPR2CL3                                                
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,3,NBP1SHR                                                     
         LLC   RF,UNTSLN                                                        
         MR    R0,RF                                                            
         AHI   R1,5000                                                          
         D     R0,=F'10000'                                                     
         STC   R1,UNTSLN                                                        
         SR    RF,R1                                                            
         STC   RF,UNTSLN2                                                       
*                                                                               
* GET TRAFFIC INFO *                                                            
*                                                                               
BLUNT30  MVI   ELCODE,X'21'                                                     
         BRAS  RE,GETEL                                                         
         BNE   BLUNT38                                                          
         USING NUCMLEL,R6                                                       
*                                                                               
         CLI   SVTNPR5,C'Y'       TRAFFIC DELETES ALLOWED                       
         BNE   BLUNT31            NO                                            
         TM    NUCMLFLG,X'08'                                                   
         BO    BLUNT60            BYPASS                                        
*                                                                               
* IF SVTN1PR9 THEN TURN ON UNTFLGAS (BYPASS PREVIOUSLY ASSIGNED CMLS)           
*                                                                               
BLUNT31  DS    0H                                                               
         TM    NUCMLFLG,X'E0'      NEED REASSIGN                                
         BNZ   BLUNT33              YES, TREAT AS UNASSIGNED                    
*                                                                               
         TM    NUCMLFL2,NUCMLFFD   FEED NO NATIONAL?                            
         BZ    *+12                                                             
         OI    UNTFLG1,UNTFL1FD    TURN ON FEED NO NATIONAL                     
         B     BLUNT34                                                          
*                                                                               
         CLC   NUCML1,=C'REASSIGN'                                              
         BE    BLUNT33                                                          
*                                                                               
         OC    NUCML1,NUCML1       CML ASSIGNED                                 
         BZ    BLUNT33              NO, GO BUILD TABLE                          
*                                                                               
         OC    NBPR2CL3,NBPR2CL3   PROD 2                                       
         BNZ   *+12                                                             
         CLI   NBPRD2,0            CHECK FOR PROD 2                             
         BE    BLUNT31A                                                         
*                                                                               
         TM    UNTFLG,UNTFLGCS     IS THIS A COPY SPLIT                         
         BO    BLUNT31A             YES, NOT A P/B                              
*                                                                               
         CLC   NUCML2,=C'REASSIGN'                                              
         BE    BLUNT33                                                          
*                                                                               
         OC    NUCML2,NUCML2       CML2 ASSIGNED                                
         BZ    BLUNT33                                                          
*                                                                               
BLUNT31A DS    0H                                                               
         CLI   SVTN1PR9,C'Y'       BYPASS ASSIGNED CMLS                         
         BNE   *+8                                                              
         OI    UNTFLG,UNTFLGAS     SET PREVIOUSLY ASSIGNED FLAG                 
*                                                                               
         L     RE,AIO3                                                          
         L     R4,AIO2                                                          
         LA    R4,3000(R4)         COMMERCIAL TABLE                             
         USING CMLTBLD,R4                                                       
*                                                                               
* SEE IF COMMERCIAL IN TABLE IF NOT THEN GO VALIDATE IT                         
*                                                                               
BLUNT31B CR    R4,RE               END OF TABLE                                 
         BNL   BLUNT31D             YES, CML NOT FOUND                          
*                                                                               
         CLC   CMLCML,NUCML1                                                    
         BNE   *+14                                                             
         MVC   SVNUCML1,CMLCML     8 CHAR CML                                   
         B     BLUNT31F            CML FOUND                                    
*                                                                               
         LA    R4,CMLENT(R4)                                                    
         OC    CMLCML,CMLCML                                                    
         BNZ   BLUNT31B                                                         
*                                                                               
* FIND CML                                                                      
*                                                                               
BLUNT31D MVC   SVCMLCOD,NUCML1                                                  
         XC    SVNUCML1,SVNUCML1                                                
         BRAS  RE,FCML              FIND CML                                    
         MVC   NUCML1,SVCMLCOD                                                  
         MVC   SVNUCML1,WORK       8 CHAR CML                                   
*                                                                               
         OC    NUCML1,NUCML1                                                    
         BZ    BLUNT31K                                                         
*                                                                               
         TM    UNTFLG,UNTFLGCS     IS THIS A COPY SPLIT                         
         BO    BLUNT31K             NOT A P/B                                   
*                                                                               
         OC    UNTPROD2,UNTPROD2                                                
         BZ    BLUNT31K                                                         
*                                                                               
         MVI   PRDMATSW,0                                                       
*                                                                               
         LA    R2,UNTPROD                                                       
         BAS   RE,VPRD                                                          
*                                                                               
         TM    PRDMATSW,X'04'      ALL PRDS COVERED ?                           
         BO    BLUNT31K                                                         
*                                                                               
         TM    PRDMATSW,X'20'                                                   
         BO    BLUNT31E             PRD1 IS NOT COVERED                         
*                                                                               
         OC    UNTPROD2,UNTPROD2                                                
         BZ    BLUNT31K                                                         
*                                                                               
         LA    R2,UNTPROD2         SEE IF PRD2 IS COVERED AS WELL               
         BAS   RE,VPRD                                                          
*                                                                               
         TM    PRDMATSW,X'20'                                                   
         BO    BLUNT31K            ONLY PRD 1 IS COVERED BY THIS CML            
*                                                                               
         OI    PRDMATSW,X'04'      TURN ON BOTH PRDS ARE COVERED                
         B     BLUNT31K                                                         
*                                                                               
BLUNT31E OC    UNTPROD2,UNTPROD2                                                
         BNZ   *+12                                                             
         MVI   CMLVLDSW,0                                                       
         B     BLUNT31F            PROD CHANGED SINCE LAST ASSIGNED             
*                                                                               
         NI    PRDMATSW,X'FF'-X'20'                                             
         LA    R2,UNTPROD2                                                      
         BAS   RE,VPRD             SEE IF PRD2 IS COVERED BY THIS CML           
         TM    PRDMATSW,X'20'                                                   
         BZ    *+12                                                             
         MVI   CMLVLDSW,0                                                       
         B     BLUNT31F            PROD CHANGED SINCE LAST ASSIGNED             
*                                                                               
         OI    PRDMATSW,X'10'      SWAP CMLS                                    
*                                                                               
         B     BLUNT31K                                                         
*                                                                               
BLUNT31F CLI   CMLVLDSW,1          VALID CML                                    
         BE    BLUNT31K                                                         
*                                                                               
         XC    NUCML1,NUCML1       ZERO OUT BAD CML                             
*                                                                               
BLUNT31K OC    NUCML1,NUCML1                                                    
         BZ    BLUNT33                                                          
*                                                                               
         OC    NBPR2CL3,NBPR2CL3   PROD 2                                       
         BNZ   *+12                                                             
         CLI   NBPRD2,0                                                         
         BE    BLUNT33D                                                         
*                                                                               
         TM    UNTFLG,UNTFLGCS     IS THIS A COPY SPLIT                         
         BO    BLUNT33              NOT A P/B                                   
*                                                                               
         OC    NUCML2,NUCML2       CML2 ASSIGNED                                
         BZ    BLUNT33             BUILD TABLE WITH PREV ASSIGN FLG             
*                                                                               
         L     RE,AIO3                                                          
         L     R4,AIO2                                                          
         LA    R4,3000(R4)         COMMERCIAL TABLE                             
*                                                                               
BLUNT32B CR    R4,RE               END OF TABLE                                 
         BNL   BLUNT32D             YES, CML NOT FOUND                          
*                                                                               
         CLC   CMLCML,NUCML2                                                    
         BNE   *+14                                                             
         MVC   SVNUCML2,CMLCML     8 CHAR CML                                   
         B     BLUNT32I            CML FOUND                                    
*                                                                               
         LA    R4,CMLENT(R4)                                                    
         OC    CMLCML,CMLCML                                                    
         BNZ   BLUNT32B                                                         
*                                                                               
* FIND CML2                                                                     
*                                                                               
BLUNT32D MVC   SVCMLCOD,NUCML2                                                  
         XC    SVNUCML2,SVNUCML2                                                
         BRAS  RE,FCML              FIND CML                                    
         MVC   NUCML2,SVCMLCOD                                                  
         MVC   SVNUCML2,WORK       8 CHAR CML                                   
*                                                                               
         OC    UNTPROD2,UNTPROD2                                                
         BZ    BLUNT32K                                                         
         TM    PRDMATSW,X'10'      SWAP CMLS ?                                  
         BZ    *+12                                                             
         LA    R2,UNTPROD          PRD2 WAS COVERED BY CML1                     
         B     *+8                                                              
         LA    R2,UNTPROD2                                                      
         NI    PRDMATSW,X'FF'-X'20'                                             
         BAS   RE,VPRD                                                          
         TM    PRDMATSW,X'20'      NO MATCH ?                                   
         BZ    BLUNT32H             MATCHED, OK TO GO ON                        
         TM    PRDMATSW,X'10'      SWAP CML                                     
         BZ    *+12                                                             
         MVI   CMLVLDSW,0          PROD CHANGED SINCE LAST ASSIGNED             
         B     BLUNT32I                                                         
         TM    PRDMATSW,X'04'      SEE IF CML1 COVERED BOTH PRDS                
         BO    *+12                                                             
         MVI   CMLVLDSW,0          PROD CHANGED SINCE LAST ASSIGNED             
         B     BLUNT32I                                                         
*                                                                               
         LA    R2,UNTPROD          MAYBE CML2 COVERS PRD1                       
         NI    PRDMATSW,X'FF'-X'20'                                             
         BAS   RE,VPRD                                                          
         TM    PRDMATSW,X'20'      NO MATCH ?                                   
         BZ    *+12                PROD CHANGED SINCE LAST ASSIGNED             
         MVI   CMLVLDSW,0                                                       
         B     BLUNT32I                                                         
         OI    PRDMATSW,X'10'      SWAP CML                                     
BLUNT32H DS    0H                                                               
         TM    PRDMATSW,X'10'      SWAP CML                                     
         BZ    *+26                                                             
         XC    NUCML1,NUCML2                                                    
         XC    NUCML2,NUCML1                                                    
         XC    NUCML1,NUCML2                                                    
         OI    UNTFLG1,UNTFL1SW    CMLS WERE SWAPPED                            
         B     BLUNT32K                                                         
*                                                                               
BLUNT32I CLI   CMLVLDSW,1          VALID CML                                    
         BE    BLUNT32K                                                         
*                                                                               
         XC    NUCML2,NUCML2       ZERO OUT BAD CML                             
*                                                                               
BLUNT32K OC    NUCML2,NUCML2       CML2 ASSIGNED                                
         BNZ   BLUNT33D            BUILD TABLE W/PREV ASS FLAG ON               
*                                                                               
         OC    NBPR2CL3,NBPR2CL3   PROD 2                                       
         BNZ   BLUNT33                                                          
*                                                                               
         CLI   NBPRD2,0                                                         
         BE    BLUNT33D                                                         
*                                                                               
BLUNT33  DS    0H                                                               
         TM    NUCMLFL2,NUCMLFFD   FEED NO NATIONAL?                            
         BZ    *+12                                                             
         OI    UNTFLG1,UNTFL1FD    TURN ON FEED NO NATIONAL                     
         NI    UNTFLG,X'FF'-UNTFLGAS SET OFF PREVIOUSLY ASSIGNED                
*                                                                               
BLUNT33D TM    NUCMLFLG,X'E0'      NEED REASSIGN                                
         BNZ   BLUNT34             YES, TREAT AS UNASSIGNED                     
*                                                                               
         OC    SVCMLTBA,SVCMLTBA   REPLACE CML W/TBA?                           
         BZ    BLUNT33F             NO                                          
*                                                                               
         CLC   =C'ALL',SVCMLTBA    ALL=TBA                                      
         BE    *+14                                                             
         CLC   SVCMLTBA,NUCML1     IS THIS IT                                   
         BNE   *+14                                                             
         XC    NUCML1,NUCML1                                                    
         NI    UNTFLG,X'FF'-UNTFLGAS                                            
*                                                                               
         CLC   =C'ALL',SVCMLTBA    ALL=TBA                                      
         BE    *+14                                                             
         CLC   SVCMLTBA,NUCML2                                                  
         BNE   BLUNT33F                                                         
         XC    NUCML2,NUCML2                                                    
*                                                                               
BLUNT33F DS    0H                                                               
         MVC   UNTCML1(16),NUCML1                                               
*                                                                               
         OC    NUCML1,NUCML1                                                    
         BZ    *+10                                                             
         MVC   UNTCML1,SVNUCML1    8 CHAR CML                                   
*                                                                               
         OC    NUCML2,NUCML2                                                    
         BZ    *+10                                                             
         MVC   UNTCML2,SVNUCML2    8 CHAR CML                                   
*                                                                               
         MVC   UNTREF,NUCMLR3F                                                  
         MVC   UNTPKEY,NUCMLKEY                                                 
*                                                                               
         OC    UNTREF,UNTREF                                                    
         BZ    *+10                                                             
         AP    PRGNOR,=P'1'                                                     
*                                                                               
         MVC   UNTPKEY,NUCMLKEY                                                 
*                                                                               
BLUNT34  DS    0H                                                               
         MVC   SVUNTCOM,UNTCOM     SAVE COMMON STUFF                            
         TM    NBUNST3,X'40'       COPY SPLIT                                   
         BZ    BLUNT35              NO                                          
         OC    NUCMPROD,NUCMPROD                                                
         BZ    BLUNT34C                                                         
         MVC   UNTPROD,NUCMPROD                                                 
         B     BLUNT35                                                          
BLUNT34C DS    0H                                                               
         CLI   NUCMLPRD,0          UNALLOCATED?                                 
         BE    BLUNT35                                                          
*                                                                               
         LA    R0,NCLSTSIZ         MAX COUNT BUG CATCHER                        
         L     R1,ASVNCLST         TABLE OF CLIENT PROD CODES                   
BLUNT34F CLI   0(R1),C' '          AT END OF TABLE?                             
         BNH   INVPRERB             YES, ERROR                                  
         CLC   NUCMLPRD,3(R1)      THIS A VALID PROD CODE                       
         BE    BLUNT34H                                                         
         LA    R1,4(,R1)           BUMP PROD PTR                                
         BCT   R0,BLUNT34F                                                      
         B     INVPRERB                                                         
BLUNT34H DS    0H                                                               
         MVC   UNTPROD,0(R1)                                                    
*                                                                               
BLUNT35  DS    0H                                                               
         TM    NUCMLFLG,X'E0'      NEED REASSIGN                                
         BZ    *+8                                                              
         OI    UNTFLG,UNTFLGRE                                                  
*                                                                               
         MVC   UNTREVN,NUCMLREV                                                 
*                                                                               
         DROP  R6                                                               
*                                                                               
* IS THIS A FEED ? *                                                            
*                                                                               
BLUNT38  MVI   ELCODE,X'22'                                                     
         L     R6,NBAIO                                                         
         BRAS  RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   UNTFEED,2(R6)                                                    
*                                                                               
* BLANK FILL END OF PROGRAM NAME *                                              
*                                                                               
         LA    R0,16                                                            
         LA    R1,NBPROGNM+15                                                   
         CLI   0(R1),0                                                          
         BNE   *+16                                                             
         MVI   0(R1),C' '                                                       
         BCTR  R1,0                                                             
         BCT   R0,*-14                                                          
         DC    H'0'                                                             
*                                                                               
* TEST FOR STANDARD PROGRAM NAME *                                              
*                                                                               
         CLC   SVPRGNM,NBPROGNM   SAME AS PROGRAM REC NAME                      
         BE    BLUNT48                                                          
         LA    R0,14              MAX NON-STD PROG NAMES                        
         L     R1,AIO3                                                          
         LA    R1,1250(,R1)                                                     
         LA    RF,1               POINTER CT TO ENTRY                           
BLUNT40  OC    0(16,R1),0(R1)     EMPTY SLOT                                    
         BZ    BLUNT42                                                          
         CLC   NBPROGNM,0(R1)                                                   
         BE    BLUNT46                                                          
         LA    R1,16(,R1)                                                       
         LA    RF,1(,RF)                                                        
         BCT   R0,BLUNT40                                                       
         MVI   UNTEXNAM,X'FF'     MORE THAN 14 NON-STD PROG NAMES!              
         B     BLUNT48                                                          
BLUNT42  MVC   0(16,R1),NBPROGNM                                                
*                                                                               
BLUNT46  STC   RF,UNTEXNAM                                                      
*                                                                               
BLUNT48  TM    UNTFLG1,UNTFL1FD    FEED W/O NATIONAL                            
         BO    BLUNT49              YES                                         
*                                                                               
         AP    UNITCNT,=P'1'       ADD TO UNIT CT                               
*                                                                               
* SEE IF THIS UNIT HAS A 5 SECOND TAG                                           
*                                                                               
         MVI   ELCODE,X'60'                                                     
         L     R6,NBAIO                                                         
         LA    R6,27(R6)                                                        
BLUTAG10 BRAS  RE,NEXTEL                                                        
         BNE   BLUTAGX                                                          
*                                                                               
         CLI   2(R6),C'Q'          IS THIS A TAG ELEMENT                        
         BNE   BLUTAG10                                                         
         CLI   3(R6),C'T'          T=TAG                                        
         BNE   BLUTAG10                                                         
         OC    UNTFEED,UNTFEED     SHOULD BE ZEROS                              
         BZ    *+6                                                              
         DC    H'0'                TAG AND FEED ARE NO NO                       
*                                                                               
         MVI   UNTFEED+1,X'FF'                                                  
         MVC   UNTFEED+2(2),3(R6)  MOVE T(N) 00FFE3F1 (00FFT3)                  
*                                                                               
BLUTAGX  LA    R5,UNTNEXT                                                       
         C     R5,AUNTABLX         AT END OF TABLE                              
         BNL   UNTSIZER                                                         
         XC    0(L'UNTENT+2,R5),0(R5)                                           
         EJECT                                                                  
* GET ANY POSSIBLE FEEDS *                                                      
*                                                                               
BLUNT49  L     R6,NBAIO                                                         
         MVI   ELCODE,X'23'                                                     
         BRAS  RE,GETEL                                                         
         BE    BLUNT50                                                          
         TM    NBUNST3,X'40'       COPY SPLIT                                   
         BZ    BLUNT60              NO                                          
*                                                                               
         TM    UNTFLG1,UNTFL1FD    FEED NO NATIONAL                             
         BZ    *+6                                                              
         DC    H'0'                YES, 23 ELEM BETTER BE THERE                 
*                                                                               
         MVC   UNTCOM,SVUNTCOM     GET COMMON STUFF                             
*==>     LR    R1,R5                                                            
*        SH    R1,=AL2(UNTNEXT-UNTENT)                                          
*==>     MVC   UNTCOM,0(R1)        GET COMMON STUFF                             
         NI    UNTFLG,0+UNTFLGRE+UNTFLGCS                                       
*        NI    UNTFLG,X'80'                                                     
         XC    UNTPROD,UNTPROD                                                  
         OC    UNTFEED,UNTFEED                                                  
         BZ    *+16                                                             
         CLC   =X'00FFE3',UNTFEED  CHK FOR TAG                                  
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   UNTFEED(2),=C'??'                                                
         AP    UNITCNT,=P'1'       ADD TO UNIT CT                               
*                                                                               
         LLC   R1,UNTELSEQ                                                      
         LA    R1,1(,R1)                                                        
         STC   R1,UNTELSEQ                                                      
*                                                                               
         LA    R5,UNTNEXT                                                       
         C     R5,AUNTABLX         AT END OF TABLE                              
         BNL   UNTSIZER                                                         
         B     BLUNT60                                                          
*                                                                               
         USING NUFDCEL,R6                                                       
BLUNT50  DS    0H                                                               
         TM    NUFDCFL2,X'80'      MEDIA FEED DELETED                           
         BO    BLUNT58              YES, BYPASS                                 
*                                                                               
         MVC   UNTCOM,SVUNTCOM     GET COMMON STUFF                             
*==>     LR    R1,R5                                                            
*        SH    R1,=AL2(UNTNEXT-UNTENT)                                          
*==>     MVC   UNTCOM,0(R1)        GET COMMON STUFF                             
         NI    UNTFLG,0+UNTFLGRE+UNTFLGCS                                       
*        NI    UNTFLG,X'80'                                                     
*                                                                               
         LLC   R1,UNTELSEQ                                                      
         LA    R1,1(,R1)                                                        
         STC   R1,UNTELSEQ                                                      
*                                                                               
         TM    NBUNST3,X'40'       COPY SPLIT                                   
         BZ    BLUN50G              NO                                          
*                                                                               
         OI    UNTFLG,UNTFLGCS     SET COPY SPLIT FLAG                          
*                                                                               
         OC    NUFDPROD,NUFDPROD                                                
         BZ    BLUN50A                                                          
         MVC   UNTPROD,NUFDPROD                                                 
         B     BLUN50G                                                          
BLUN50A  DS    0H                                                               
         CLI   NUFDCPRD,0          UNALLOCATED?                                 
         BE    BLUNT35                                                          
*                                                                               
         LA    R0,NCLSTSIZ         MAX COUNT BUG CATCHER                        
         L     R1,ASVNCLST         TABLE OF CLIENT PROD CODES                   
BLUN50C  CLI   0(R1),C' '          AT END OF TABLE?                             
         BNH   INVPRERB             YES, ERROR                                  
         CLC   NUFDCPRD,3(R1)      THIS A VALID PROD CODE                       
         BE    BLUN50E                                                          
         LA    R1,4(,R1)           BUMP PROD PTR                                
         BCT   R0,BLUN50C                                                       
         B     INVPRERB                                                         
BLUN50E  DS    0H                                                               
         MVC   UNTPROD,0(R1)                                                    
*                                                                               
BLUN50G  DS    0H                                                               
         TM    UNTFLG,UNTFLGRE     NEED REASSIGN                                
         BO    BLUNT53             YES, TREAT AS UNASSIGNED                     
*                                                                               
* IF SVTN1PR9 THEN TURN ON UNTFLGAS (BYPASS PREVIOUSLY ASSIGNED CMLS)           
*                                                                               
         CLC   NUFDCML2,=C'REASSIGN'                                            
         BE    BLUNT50X             NO, GO BUILD TABLE                          
*                                                                               
         OC    NUFDCML1,NUFDCML1   CML ASSIGNED                                 
         BZ    BLUNT50X             NO, GO BUILD TABLE                          
*                                                                               
         TM    UNTFLG,UNTFLGCS     IS THIS A COPY SPLIT                         
         BO    BLUNT50A             YES, NOT A P/B                              
*                                                                               
         OC    NBPR2CL3,NBPR2CL3   PROD 2                                       
         BNZ   *+12                                                             
         CLI   NBPRD2,0            CHECK FOR PROD 2                             
         BE    BLUNT50A                                                         
*                                                                               
         CLC   NUFDCML2,=C'REASSIGN'                                            
         BE    BLUNT50X                                                         
*                                                                               
         OC    NUFDCML2,NUFDCML2   CML2 ASSIGNED                                
         BZ    BLUNT50X                                                         
*                                                                               
BLUNT50A DS   0H                                                                
         CLI   SVTN1PR9,C'Y'       BYPASS ASSIGNED CMLS                         
         BNE   *+8                                                              
*                                                                               
         OI    UNTFLG,UNTFLGAS     SET PREVIOUSLY ASSIGNED FLAG                 
*                                                                               
         L     RE,AIO3                                                          
         L     R4,AIO2                                                          
         LA    R4,3000(R4)         COMMERCIAL TABLE                             
*                                                                               
* SEE IF COMMERCIAL IN TABLE IF NOT THEN GO VALIDATE IT                         
*                                                                               
BLUNT50B CR    R4,RE               END OF TABLE                                 
         BNL   BLUNT50D             YES, CML NOT FOUND                          
*                                                                               
         CLC   CMLCML,NUFDCML1                                                  
         BE    BLUNT50F            CML FOUND                                    
         LA    R4,CMLENT(R4)                                                    
         OC    CMLCML,CMLCML                                                    
         BNZ   BLUNT50B                                                         
*                                                                               
* FIND CML                                                                      
*                                                                               
BLUNT50D MVC   SVCMLCOD,NUFDCML1                                                
         XC    SVNUCML1,SVNUCML1                                                
         BRAS  RE,FCML              FIND CML                                    
         MVC   NUFDCML1,SVCMLCOD                                                
         MVC   SVNUCML1,WORK       8 CHAR CML                                   
*                                                                               
         OC    NUFDCML1,NUFDCML1                                                
         BZ    BLUNT50K                                                         
*                                                                               
         TM    UNTFLG,UNTFLGCS     IS THIS A COPY SPLIT                         
         BO    BLUNT50K             YES, NOT A P/B                              
*                                                                               
         OC    UNTPROD2,UNTPROD2                                                
         BZ    BLUNT50K                                                         
         MVI   PRDMATSW,0                                                       
*                                                                               
         LA    R2,UNTPROD                                                       
         BAS   RE,VPRD                                                          
         TM    PRDMATSW,X'04'      ALL PRDS COVERED ?                           
         BO    BLUNT50K                                                         
         TM    PRDMATSW,X'20'                                                   
         BO    BLUNT50E             PRD1 IS NOT COVERED                         
         OC    UNTPROD2,UNTPROD2                                                
         BZ    BLUNT50K                                                         
         LA    R2,UNTPROD2         SEE IF PRD2 IS COVERED AS WELL               
         BAS   RE,VPRD                                                          
         TM    PRDMATSW,X'20'                                                   
         BO    BLUNT50K            ONLY PRD 1 IS COVERED BY THIS CML            
         OI    PRDMATSW,X'04'      TURN ON BOTH PRDS ARE COVERED                
         B     BLUNT50K                                                         
BLUNT50E OC    UNTPROD2,UNTPROD2                                                
         BNZ   *+12                                                             
         MVI   CMLVLDSW,0                                                       
         B     BLUNT50F                                                         
*                                                                               
         NI    PRDMATSW,X'FF'-X'20'                                             
         LA    R2,UNTPROD2                                                      
         BAS   RE,VPRD             SEE IF PRD2 IS COVERED BY THIS CML           
         TM    PRDMATSW,X'20'                                                   
         BZ    *+12                                                             
         MVI   CMLVLDSW,0                                                       
         B     BLUNT50F                                                         
         OI    PRDMATSW,X'10'      SWAP CMLS                                    
         B     BLUNT50K                                                         
*                                                                               
BLUNT50F CLI   CMLVLDSW,1          VALID CML                                    
         BE    BLUNT50K                                                         
*                                                                               
         XC    NUFDCML1,NUFDCML1   ZERO OUT BAD CML                             
*                                                                               
BLUNT50K OC    NUFDCML1,NUFDCML1                                                
         BZ    BLUNT50X                                                         
*                                                                               
         OC    NBPR2CL3,NBPR2CL3   PROD 2                                       
         BNZ   *+12                                                             
         CLI   NBPRD2,0                                                         
         BE    BLUNT50Y                                                         
*                                                                               
         OC    NUFDCML2,NUFDCML2   CML2 ASSIGNED                                
         BZ    BLUNT50X            BIULD TABLE WITH PREV ASSIGN FLG             
*                                                                               
         L     RE,AIO3                                                          
         L     R4,AIO2                                                          
         LA    R4,3000(R4)         COMMERCIAL TABLE                             
*                                                                               
BLUNT50L CR    R4,RE               END OF TABLE                                 
         BNL   BLUNT50M             YES, CML NOT FOUND                          
*                                                                               
         CLC   CMLCML,NUFDCML2                                                  
         BE    BLUNT50N            CML FOUND                                    
         LA    R4,CMLENT(R4)                                                    
         OC    CMLCML,CMLCML                                                    
         BNZ   BLUNT50L                                                         
*                                                                               
* FIND CML2                                                                     
*                                                                               
BLUNT50M MVC   SVCMLCOD,NUFDCML2                                                
         XC    SVNUCML2,SVNUCML2                                                
         BRAS  RE,FCML              FIND CML                                    
         MVC   NUFDCML2,SVCMLCOD                                                
         MVC   SVNUCML2,WORK       8 CHAR CML                                   
*                                                                               
         OC    UNTPROD2,UNTPROD2                                                
         BZ    BLUNT50P                                                         
*                                                                               
         TM    PRDMATSW,X'10'      SWAP CMLS ?                                  
         BZ    *+12                                                             
         LA    R2,UNTPROD          PRD2 WAS COVERED BY CML1                     
         B     *+8                                                              
         LA    R2,UNTPROD2                                                      
         NI    PRDMATSW,X'FF'-X'20'                                             
         BAS   RE,VPRD                                                          
         TM    PRDMATSW,X'20'      NO MATCH ?                                   
         BZ    BLUNT5AN             YES MATCH, OK TO GO ON                      
         TM    PRDMATSW,X'10'      SWAP CML                                     
         BZ    *+12                                                             
         MVI   CMLVLDSW,0                                                       
         B     BLUNT50N                                                         
*                                                                               
         TM    PRDMATSW,X'04'      SEE IF CML1 COVERED BOTH PRDS                
         BO    *+12                                                             
         MVI   CMLVLDSW,0                                                       
         B     BLUNT50N                                                         
*                                                                               
         LA    R2,UNTPROD          MAYBE CML2 COVERS PRD1                       
         NI    PRDMATSW,X'FF'-X'20'                                             
         BAS   RE,VPRD                                                          
         TM    PRDMATSW,X'20'      NO MATCH ?                                   
         BZ    *+12                                                             
         MVI   CMLVLDSW,0                                                       
         B     BLUNT50N                                                         
*                                                                               
         OI    PRDMATSW,X'10'      SWAP CML                                     
BLUNT5AN DS    0H                                                               
         TM    PRDMATSW,X'10'      SWAP CML                                     
         BZ    *+26                                                             
         XC    NUFDCML1,NUFDCML2                                                
         XC    NUFDCML2,NUFDCML1                                                
         XC    NUFDCML1,NUFDCML2                                                
         OI    UNTFLG1,UNTFL1SW    CMLS WERE SWAPPED                            
         B     BLUNT50P                                                         
*                                                                               
BLUNT50N CLI   CMLVLDSW,1          VALID CML                                    
         BE    BLUNT50P                                                         
*                                                                               
         XC    NUFDCML2,NUFDCML2   ZERO OUT BAD CML                             
*                                                                               
BLUNT50P OC    NUFDCML2,NUFDCML2   CML2 ASSIGNED                                
         BNZ   BLUNT50Y            BUILD TABLE W/PREV ASS FLAG ON               
*                                                                               
         OC    NBPR2CL3,NBPR2CL3   PROD 2                                       
         BNZ   *+12                                                             
         CLI   NBPRD2,0                                                         
         BE    BLUNT50Y                                                         
*                                                                               
BLUNT50X NI    UNTFLG,X'FF'-UNTFLGAS  SET OFF PREV ASSIGNED                     
*                                                                               
BLUNT50Y OC    SVCMLTBA,SVCMLTBA   REPLACE CML W/TBA?                           
         BZ    BLUNT52              NO                                          
*                                                                               
         CLC   =C'ALL',SVCMLTBA    ALL=TBA                                      
         BE    *+14                                                             
         CLC   SVCMLTBA,NUFDCML1   IS THIS IT                                   
         BNE   *+14                                                             
         XC    NUFDCML1,NUFDCML1                                                
         NI    UNTFLG,X'FF'-UNTFLGAS                                            
*                                                                               
BLUNT51  CLC   =C'ALL',SVCMLTBA    ALL=TBA                                      
         BE    *+14                                                             
         CLC   SVCMLTBA,NUFDCML2                                                
         BNE   BLUNT52                                                          
         XC    NUFDCML2,NUFDCML2                                                
*                                                                               
BLUNT52  DS   0H                                                                
         TM    SVOPTSW,OPTCLR      CLEAR ALL COMMLS?                            
         BO    BLUNT52C                                                         
*                                                                               
         MVC   UNTCML1(16),NUFDCML1                                             
         B     BLUNT53                                                          
*                                                                               
BLUNT52C DS   0H                                                                
*                                                                               
         OC    NUFDCML1(16),NUFDCML1   WAS THERE A COMML?                       
         BZ    BLUNT53                  NO                                      
         OI    UNTFLG,UNTFLGCH     SET UNIT CHANGED                             
*                                                                               
BLUNT53  OC    NBPR1CL3,NBPR1CL3   UNALLOCATED                                  
         BNZ   BLUNT54                                                          
*                                                                               
         CLI   SVCSPROD,0          UNALLOCATED                                  
         BNE   BLUNT54                                                          
         OI    UNTFLG,UNTFLGUL     SET ON UNALLOCATED                           
*                                                                               
BLUNT54  MVC   UNTFEED,NUFDCFED                                                 
*                                                                               
         TM    SVOPTSW,OPTCLR      CLEAR ALL COMMLS?                            
         BO    BLUNT56                                                          
*                                                                               
         MVC   UNTREF,NUFDCR3F                                                  
*                                                                               
BLUNT56  AP    UNITCNT,=P'1'       BUMP COUNT                                   
         LA    R5,UNTNEXT                                                       
         C     R5,AUNTABLX         AT END OF TABLE                              
         BNL   UNTSIZER                                                         
BLUNT58  XC    0(L'UNTENT+2,R5),0(R5)                                           
         BRAS  RE,NEXTEL                                                        
         BE    BLUNT50                                                          
         EJECT                                                                  
BLUNT60  DS   0H                                                                
         XC    UNTENT(L'UNTENT+2),UNTENT                                        
*                                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY CLIENT                  
         BZ    BLUNT62                                                          
*                                                                               
         CLI   T216FFD+6,C'$'      USER SIGN ON OFFICE LIST                     
         BNE   *+12                                                             
         OI    NBINDS5,NBI5PLST    SET OFFICE LIST FLAG                         
         B     *+12                                                             
         CLI   T216FFD+6,C'*'      USER SIGN ON BY OFFICE                       
         BNE   BLUNT62              NO                                          
*                                                                               
         MVC   NBTRAPSC,T216FFD+7  BRAND LEVEL OFFICE(LIST)                     
*                                                                               
BLUNT62  GOTO1 ANETIO,DMCB,(R3)                                                 
         CLI   NBERROR,NBGOOD                                                   
         BE    BLUNT64                                                          
         DC    H'0'                                                             
*                                                                               
BLUNT64  TM    NBSUBMSK,NBSBMCLI+NBSBMNET                                       
         BNZ   BLUNT70                                                          
         CLI   NBMODE,NBREQLST                                                  
         BE    BLUNT70                                                          
*                                                                               
         OC    PROGRAM,PROGRAM     DOING ALL PROGRAMS                           
         BZ    BLUNT66              YES                                         
*                                                                               
         TM    NBSUBMSK,NBSBMPRG                                                
         BNZ   BLUNT70                                                          
*                                                                               
BLUNT66  CLI   NBMODE,NBPROCUN                                                  
         BNE   BLUNT60                                                          
*                                                                               
         MVC   BASEPRG,NBACTPRG                                                 
         LA    RE,EQVPTBL                                                       
         LA    RF,L'EQVPTBL/(EQVNEXT-EQVENT)                                    
*                                                                               
         USING EQVPTBLD,RE                                                      
TPRGC10  OC    EQVEPRG,EQVEPRG     END OF ENTRIES                               
         BZ    TPRGCX               YES                                         
*                                                                               
         CLC   NBACTPRG,EQVEPRG                                                 
         BNE   TPRGC14                                                          
*                                                                               
         CLC   NBACTDAT,EQVSDT     WITHIN DATES                                 
         BL    TPRGC14                                                          
         CLC   NBACTDAT,EQVEDT                                                  
         BNH   TPRGC20                                                          
*                                                                               
TPRGC14  LA    RE,EQVNEXT                                                       
         BCT   RF,TPRGC10                                                       
*                                                                               
         B     TPRGCX                                                           
*                                                                               
TPRGC20  MVC   BASEPRG,EQVBPRG     SET FOR USE                                  
         DROP  RE                                                               
TPRGCX   DS    0H                                                               
*                                                                               
         L     RF,APRGTBLE                                                      
         USING PRGTABLD,RF                                                      
*                                                                               
         CLC   BASEPRG,PRGPRG                                                   
         BE    BLUNT68                                                          
         LA    RF,PRGNEXT                                                       
         OC    PRGPRG,PRGPRG                                                    
         BNZ   *-20                                                             
         DC    H'0'                BUG, PROGRAM S/B BE IN PROG TABLE            
         DROP  RF                                                               
*                                                                               
BLUNT68  TM    NBSUBMSK,NBSBMPRG                                                
         BZ    BLUNT10                                                          
*                                   IF NEW PROG, READ NEW PROG REC              
*                                   AND CK IF EQUIVALENT PROGRAM                
         SR    R4,R4                                                            
         B     BLUNT10                                                          
*                                                                               
BLUNT70  DS   0H                                                                
         LA    RE,EQVPTBL                                                       
         LA    RF,L'EQVPTBL/(EQVNEXT-EQVENT)                                    
         USING EQVPTBLD,RE                                                      
BLUNT72  OC    EQVENT,EQVENT       EMPTY ENTRY IS END                           
         BZ    BLUNT80                                                          
         CLC   EQVBPRG,SVPRG       THIS SAME AS CURRENT                         
         BNE   BLUNT74                                                          
BLUNT73  CLI   EQVUSED,C'Y'        ENTRY HAVE UNITS & NOT BEEN USED             
         BE    BLUNT76                                                          
BLUNT74  LA    RE,EQVNEXT                                                       
         BCT   RF,BLUNT72                                                       
*                                                                               
         B     BLUNT80                                                          
*                                                                               
BLUNT76  MVI   EQVUSED,C'U'        SET TO USED                                  
         MVC   BASEPRG,EQVEPRG                                                  
         MVC   BASEDTS,EQVSDT                                                   
*                                                                               
         CLC   BASESTR,STDATEP     BEFORE START DATE                            
         BNL   *+10                 NO                                          
         MVC   BASESTR,STDATEP     DON'T RUN BEFORE START DATE                  
*                                                                               
         CLC   BASEEND,ENDATEP     PAST END DATE                                
         BNH   *+10                 NO                                          
         MVC   BASEEND,ENDATEP     DON'T RUN PAST END DATE                      
*                                                                               
         OC    OPTDATE,OPTDATE                                                  
         BZ    BLUNT77                                                          
         CLC   BASESTR,OPTDATE                                                  
         BNL   *+10                                                             
         MVC   BASESTR,OPTDATE                                                  
*                                                                               
         OC    OPTDAT2,OPTDAT2                                                  
         BZ    BLUNT77                                                          
         CLC   BASEEND,OPTDAT2                                                  
         BNH   BLUNT77                                                          
         MVC   BASEEND,OPTDAT2                                                  
*                                                                               
BLUNT77  MVC   BASECT,EQVCT                                                     
         DROP  RE                                                               
         BRAS  RE,NETI                                                          
*                                                                               
BLUNT78  TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY CLIENT                  
         BZ    BLUNT79                                                          
*                                                                               
         CLI   T216FFD+6,C'$'      USER SIGN ON OFFICE LIST                     
         BNE   *+12                                                             
         OI    NBINDS5,NBI5PLST    SET OFFICE LIST FLAG                         
         B     *+12                                                             
         CLI   T216FFD+6,C'*'      USER SIGN ON BY OFFICE                       
         BNE   BLUNT79              NO                                          
*                                                                               
         MVC   NBTRAPSC,T216FFD+7  BRAND LEVEL OFFICE(LIST)                     
*                                                                               
BLUNT79  GOTO1 ANETIO,DMCB,(R3)                                                 
         MVI   NBUSER+13,C'N'      DO NOT SUPPRESS PRE-EMPTS                    
         CLI   NBERROR,NBGOOD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   NBMODE,NBREQLST                                                  
         BE    BLUNT72                                                          
         CLI   NBMODE,NBPROCUN                                                  
         BNE   BLUNT78                                                          
         CLC   NBACTPRG,NBSELPRG                                                
         BNE   BLUNT72                                                          
         B     BLUNT10                                                          
*                                                                               
BLUNT80  BRAS  RE,SETSPOT          SET FROM NET TO SPOT                         
*                                                                               
         CP    UNITCNT,=P'0'       ANY UNITS FOUND                              
         BZ    BLXBAD               NO                                          
         ZAP   DUB,UNITCNT                                                      
         CVB   R0,DUB                                                           
*                                                                               
         GOTO1 QSORT,DMCB,AUNTABLE,(R0),L'UNTENT,L'UNTSRT,0                     
         B     BLXOK                                                            
*                                                                               
BLXBAD   CR    RB,RC                                                            
         B     BLUNTX                                                           
BLXOK    CR    RB,RB                                                            
*                                                                               
BLUNTX   XIT1                                                                   
         EJECT                                                                  
* BUILD PRODUCT LIST FOR COPY SPLIT                                             
*                                                                               
BCSPRD   NTR1                                                                   
*                                                                               
         XC    SVCSPROD,SVCSPROD                                                
*                                                                               
         L     R6,NBAIO                                                         
         LA    R6,27(,R6)                                                       
         MVI   ELCODE,X'19'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   BCSPR20                                                          
         USING NUPDED,R6                                                        
         SR    R0,R0                                                            
         LLC   R1,1(R6)                                                         
         D     R0,=F'7'                                                         
         CHI   R1,2                MUST BE MORE THAN 2 PRODUCTS ELSE            
         BNH   BCSPRX              DON'T BUILD SVCSPROD TABLE                   
*                                                                               
         LR    R0,R1                                                            
         LA    RE,NUPDEPR                                                       
         LA    RF,SVCSPROD                                                      
BCSPR10  MVC   0(3,RF),0(RE)                                                    
         LA    RE,7(,RE)                                                        
         LA    RF,3(,RF)                                                        
         BCT   R0,BCSPR10                                                       
         B     BCSPRX                                                           
*                                                                               
BCSPR20  DS    0H                                                               
         L     R6,NBAIO                                                         
*                                                                               
         LA    R6,27(,R6)                                                       
         MVI   ELCODE,X'14'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   BCSPRX                                                           
*                                                                               
         USING NUPRDD,R6                                                        
         SR    R0,R0                                                            
         LLC   R1,1(R6)                                                         
         D     R0,=F'6'                                                         
         LR    R0,R1               GET NUMBER OF PRDS IN R0                     
         CHI   R0,1                MUST BE MORE THAN 1 PRODUCT                  
         BH    *+6                                                              
         DC    H'0'                                                             
         LA    RE,NUPRDPR                                                       
         LA    RF,SVCSPROD                                                      
*                                                                               
BCSPR30  DS   0H                                                                
         LA    R2,NCLSTSIZ         MAX COUNT BUG CATCHER                        
         L     R1,ASVNCLST         TABLE OF CLIENT PROD CODES                   
BCSPR35  DS   0H                                                                
         CLC   3(1,R1),0(RE)       THIS A VALID PROD CODE                       
         BE    BCSPR40                                                          
         LA    R1,4(,R1)           BUMP PROD PTR                                
         CLI   0(R1),C' '          AT END OF TABLE?                             
         BNH   *+8                  YES, ERROR                                  
         BCT   R2,BCSPR35                                                       
         DC    H'0'                                                             
BCSPR40  MVC   0(3,RF),0(R1)                                                    
         LA    R1,6(,R1)                                                        
         LA    RF,3(,RF)                                                        
         BCT   R0,BCSPR30                                                       
*                                                                               
         CLI   SVCSPROD,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
BCSPRX   XIT1                                                                   
         EJECT                                                                  
* SEE THAT CML PRD MATCH TO UNIT PRD                                            
*                                                                               
VPRD     NTR1                                                                   
*                                                                               
         USING UNTABLED,R5                                                      
*                                                                               
         L     R6,AIO2                                                          
         LA    R6,2000(R6)         COMMERCIAL RECORD IS READ IN HERE            
         BRAS  RE,SETSPOT          SET TO SPOT                                  
*                                                                               
         MVI   ELCODE,X'29'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLMPREL,R6                                                      
         OI    PRDMATSW,X'04'      COVERS ALL PRDS                              
         CLI   CMLMPRS,X'FF'       IS THIS COMML PRD=ALL                        
         BE    VPRDX               YES, COVERS ALL PRODUCTS                     
         NI    PRDMATSW,X'FF'-X'04'                                             
*                                                                               
         LLC   R0,CMLMPRLN                                                      
         SHI   R0,2                                                             
*                                                                               
         LA    R1,CMLMPRS          START OF PROD LIST                           
VPRD30   CLC   0(3,R2),0(R1)       UNTPROD TO PROD                              
         BE    VPRDX               YES                                          
         LA    R1,3(,R1)                                                        
         SHI   R0,3                                                             
         LTR   R0,R0                                                            
         BNZ   VPRD30                                                           
         OI    PRDMATSW,X'20'                                                   
VPRDX    XIT1                                                                   
*                                                                               
         DROP  R5,R4                                                            
         EJECT                                                                  
* FILTER ON PRODUCT GROUP *                                                     
*                                                                               
FPGR     NTR1                                                                   
         LA    R0,L'OPTPGRL/3                                                   
*NOP     LA    R1,OPTPGRL                                                       
         LHI   R1,OPTPGRL-SYSD                                                  
         AR    R1,R9                                                            
FPGR10   CLC   NBPR1CL3,0(R1)                                                   
         BE    FPGREQ                                                           
*                                                                               
         LA    RE,L'SVCSPROD                                                    
         LA    RF,SVCSPROD                                                      
FPGR20   CLI   0(RF),0                                                          
         BE    FPGR30                                                           
         CLC   0(3,R1),0(RF)                                                    
         BE    FPGREQ                                                           
         LA    RF,3(,RF)                                                        
         BCT   RE,FPGR20                                                        
*                                                                               
FPGR30   LA    R1,3(,R1)                                                        
         CLI   0(R1),0                                                          
         BNH   *+8                                                              
         BCT   R0,FPGR10                                                        
         CR    RD,RB                                                            
         XIT1                                                                   
FPGREQ   CR    RB,RB                                                            
         XIT1                                                                   
         EJECT                                                                  
* SEE IF EQUIVALENT UNIT, AND IF SO CK DATES *                                  
*                                                                               
CEQV     LA    R1,EQVPTBL                                                       
         LA    R0,L'EQVPTBL/(EQVNEXT-EQVENT)                                    
         MVI   BYTE,0                                                           
         XC    SVACTPRG,SVACTPRG                                                
         USING EQVPTBLD,R1                                                      
CEQV10   OC    EQVENT,EQVENT       END OF ENTRIES                               
         BZ    CEQV30                                                           
         CLC   EQVEPRG,NBACTPRG    THIS AN EQUIVALENT PROG                      
         BNE   CEQV20                                                           
         MVI   BYTE,1                                                           
         CLC   NBACTDAT,EQVSDT     EQUIV FOR THIS DATE                          
         BL    CEQV20                                                           
         CLC   NBACTDAT,EQVEDT                                                  
         BH    CEQV20                                                           
         MVI   EQVUSED,C'U'        SET ENTRY USED                               
*                                                                               
         MVC   SVACTPRG,NBACTPRG   SAVE MEDIA BUY PROG NAME                     
*                                                                               
         MVC   NBACTPRG,EQVBPRG    SUBSTITUTE BASE CODE                         
         MVC   BASECT,EQVCT                                                     
         MVI   BYTE,0                                                           
*                                                                               
         B     CEQV30                                                           
*                                                                               
CEQV20   LA    R1,EQVNEXT                                                       
         BCT   R0,CEQV10                                                        
         DROP  R1                                                               
*                                                                               
* FIND PROGRAM INFO IN PROGRAM TABLE AND SAVE IT *                              
*                                                                               
CEQV30   CLC   NBACTPRG,SVPRG                                                   
         BE    CEQV38                                                           
         L     RF,APRGTBLE                                                      
         USING PRGTABLD,RF                                                      
*                                                                               
CEQV34   CLC   NBACTPRG,PRGPRG                                                  
         BE    CEQV36                                                           
         LA    RF,PRGNEXT                                                       
         OC    PRGPRG,PRGPRG                                                    
         BNZ   CEQV34                                                           
         DC    H'0'                                                             
*                                                                               
CEQV36   MVC   SVPRGENT,PRGENT                                                  
*                                                                               
CEQV38   CLI   BYTE,1              WAS PROGRAM CODE FOUND                       
         BE    CEQV40               YES-NOT FOR THIS DATE (PARTIAL)             
*                                                                               
         CR    RB,RB               SET EQUAL CC                                 
         BR    RE                                                               
         DROP  RF                                                               
*                                                                               
* PROGRAM CODE WAS EQUIVALENT FOR PART OF PERIOD, ERROR, BYPASS *               
*                                                                               
CEQV40   LTR   RB,RB                                                            
         BR    RE                                                               
*                                                                               
INVPRERB MVI   ERROR,INVPROD                                                    
         GOTO1 ERREX                                                            
*                                                                               
UNTSIZER CLI   OFFLINE,C'Y'        IF OFFLINE, DUMP                             
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   GERROR,=Y(MANYUNT)                                               
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         STCM  R1,7,GASUBST        A(SUBST TEXT)                                
         MVI   ELEM,3              L'SUBST TEXT + 1                             
         MVC   ELEM+1(2),=C'92'                                                 
         LA    R2,TRACLTH                                                       
         NI    TRACLTH+4,X'FF'-X'20' FORCE CLIENT CHANGED                       
         GOTO1 VTRAERR                                                          
         DROP  R7                                                               
         EJECT                                                                  
         LTORG                                                                  
* GET NETWORK                                                                   
*                                                                               
GNET     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,SETNET           SET TO NET                                   
*                                                                               
         XC    KEY,KEY                                                          
         XC    KEYSAVE,KEYSAVE                                                  
*                                                                               
         LA    R4,KEY                                                           
         USING NURECD,R4                                                        
*                                                                               
         MVI   NUKPTYPE,X'84'      UNIT PASSIVE KEY                             
         MVC   NUKPAM,BAGYMD       AGENCY                                       
         MVC   NUKPCLT,BCLT        CLIENT                                       
         MVC   NUKPNET,SVNET       CURRENT NETWORK                              
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
GNET10   CLC   KEY(4),KEYSAVE      SAME AGY/CLT                                 
         BNE   GNETDONE                                                         
*                                                                               
         CLI   SVNET,0             FIRST NETWORK                                
         BE    *+14                                                             
         CLC   SVNET,NUKPNET                                                    
         BNL   GNET20              DONE THIS NETWORK BEFORE                     
         MVC   SVNET,NUKPNET                                                    
         B     GNETX                                                            
*                                                                               
* FORCE NEXT NETWORK                                                            
*                                                                               
GNET20   MVC   NUKPPROG,=X'FFFFFFFFFFFF'  FORCE NEXT NET                        
         XC    NUKPDATE(6),NUKPDATE       CLEAR THE REST OF KEY                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     GNET10                                                           
*                                                                               
GNETDONE MVC   SVNET,=X'FFFFFFFF'                                               
*                                                                               
GNETX    DS    0H                                                               
         BRAS  RE,SETSPOT          SET TO SPOT                                  
*                                                                               
         XIT1                                                                   
         DROP  R3,R4                                                            
         LTORG                                                                  
         EJECT                                                                  
*============================================================                   
* READ THROUGH UNITS AND BUILD PROGRAM TABLE FOR SELECTS                        
*============================================================                   
                                                                                
BLPRG    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LHI   RE,PRGTBLX-PRGTABLE                                              
         CLI   OFFLINE,C'Y'                                                     
         BNE   *+8                                                              
         LHI   RE,PRGTBLMO+PRGTBLX-PRGTABLE                                     
         A     RE,APRGTBLE                                                      
         ST    RE,APRGTBLX         RESET PRGTABLE END ADDRESS                   
*                                                                               
         L     R0,APRGTBLE                                                      
         L     R1,APRGTBLX                                                      
         SR    R1,R0                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R3,ANETBLK                                                       
         USING NETBLCKD,R3                                                      
*                                                                               
         L     RE,AIO3                                                          
         LA    RF,1250             CLEAR EST/PRD/COPY CODE TABLE AREA           
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         NI    UPDSW,X'FF'-X'40'-X'02'  SET OFF END UNITS & FOUND SW            
*                                                                               
BLPRG04  L     R4,APRGTBLE                                                      
         USING PRGTABLD,R4                                                      
         LR    RE,R4                                                            
         L     RF,APRGTBLX                                                      
         SR    RF,RE                                                            
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         SR    R1,R1                                                            
         ST    R1,ASVNEXT                                                       
*                                                                               
         MVC   BASEPRG,PROGRAM                                                  
         MVC   BASEDTS,STDATEP                                                  
         OC    OPTDAT2,OPTDAT2                                                  
         BZ    *+10                                                             
         MVC   BASEEND,OPTDAT2                                                  
*                                                                               
         OC    OPTDATE,OPTDATE                                                  
         BZ    *+10                                                             
         MVC   BASESTR,OPTDATE                                                  
*                                                                               
         MVI   BASECT,0                                                         
*                                                                               
         BRAS  RE,NETI                  INITIALIZE NETIO                        
*                                                                               
         L     RE,AIO2                                                          
         LA    RE,3000(RE)         AREA TO BUILD CML TABLE                      
         LA    RF,1000                                                          
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
BLPRG10  TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY CLIENT                  
         BZ    BLPRG11                                                          
*                                                                               
         CLI   T216FFD+6,C'$'      USER SIGN ON OFFICE LIST                     
         BNE   *+12                                                             
         OI    NBINDS5,NBI5PLST    SET OFFICE LIST FLAG                         
         B     *+12                                                             
         CLI   T216FFD+6,C'*'      USER SIGN ON BY OFFICE                       
         BNE   BLPRG11              NO                                          
*                                                                               
         MVC   NBTRAPSC,T216FFD+7  BRAND LEVEL OFFICE(LIST)                     
*                                                                               
BLPRG11  GOTO1 ANETIO,DMCB,(R3)                                                 
*                                                                               
         CLI   NBERROR,NBGOOD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBREQLST                                                  
         BNE   BLPRG12                                                          
*                                                                               
         OC    PROGRAM,PROGRAM                                                  
         BZ    BLPRG90                                                          
         MVC   NBACTPRG,PROGRAM                                                 
         B     BLPRG14                                                          
*                                                                               
BLPRG12  CLI   NBMODE,NBPROCUN                                                  
         BNE   BLPRG10                                                          
         EJECT                                                                  
* GET PROGRAM INFO                                                              
*                                                                               
BLPRG14  DS    0H                                                               
         NI    SVFLAG,X'FF'-SVDELPAT  INIT DEL PATTERN FND                      
*                                                                               
         BRAS  RE,SETSPOT          SET TO SPOT                                  
*                                                                               
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING NPGKEY,R1                                                        
         MVC   NPGKTYP,=X'0D20'                                                 
         MVC   NPGKAM,BAGYMD                                                    
         MVC   NPGKNET,NETMKT                                                   
         MVC   NPGKPROG,NBACTPRG                                                
         MVC   NPGKEND,NBACTDAT                                                 
         DROP  R1                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE                                                  
         BNE   PROGTER1                                                         
*                                                                               
         CLI   NBMODE,NBREQLST     IF NO UNITS, GET OUT                         
         BE    BLPRG60                                                          
*                                                                               
         MVC   KEYSAVE,NBKEY                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'UNTDIR',KEYSAVE,KEY           
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         EJECT                                                                  
BLPRG20  L     R6,NBAIO                                                         
         BRAS  RE,FPRGC              FIND BASE IF EQUIV PROG                    
         BE    *+16                                                             
         CLI   BYTE,7              TRI-BACK UNITS FOUND?                        
         BNE   BLPRG50             NO, EQUIV BUT NO BASE                        
         B     NOUNTER1                                                         
*                                                                               
         OC    BASEDTS,BASEDTS                                                  
         BZ    BLPRG22                                                          
         CLC   NBACTDAT,BASESTR                                                 
         BL    BLPRG21                                                          
         CLC   NBACTDAT,BASEEND                                                 
         BNH   BLPRG22                                                          
BLPRG21  DC    H'0'                                                             
*                                                                               
BLPRG22  CLI   SVTNPR9,C'B'        BOTH MEDIA AND SKED                          
         BE    BLPRG26                                                          
         CLI   SVTNPR9,C'S'        MEDIA ONLY (DEFAULT)                         
         BNE   BLPRG24                                                          
         CLI   NBACTSUB,C'A'       SKED ONLY                                    
         BL    BLPRG50                                                          
         B     BLPRG26                                                          
*                                                                               
BLPRG24  CLI   NBACTSUB,C'A'                                                    
         BNL   BLPRG50                                                          
*                                                                               
BLPRG26  CLI   NBACTSUB,C'A'       THIS A SKED UNIT                             
         BNL   BLPRG27              YES, NO EST CK                              
*                                                                               
         BRAS  RE,CKEST              GO CK EST FOR COPY CODE N                  
         BE    BLPRG50                YES, BYPASS                               
*                                                                               
* CK PRODUCT OR PRODUCT GROUP FILTER *                                          
*                                                                               
BLPRG27  CLI   OPTPROD,0                                                        
         BE    BLPRG28                                                          
         CLC   NBPR1CL3,OPTPROD                                                 
         BE    BLPRG28                                                          
*                                                                               
         LA    RE,L'SVCSPROD                                                    
         LA    RF,SVCSPROD                                                      
BLPRG27E CLI   0(RF),0                                                          
         BE    BLPRG50                                                          
         CLC   OPTPROD,0(RF)                                                    
         BE    BLPRG28                                                          
         LA    RF,3(,RF)                                                        
         BCT   RE,BLPRG27E                                                      
         B     BLPRG50                                                          
*                                                                               
BLPRG28  DS   0H                                                                
         OC    OPTPRGR,OPTPRGR    ANY PRODUCT GROUP                             
         BZ    BLPRG28A             NO                                          
         BRAS  RE,FPG              FILTER ON PRODUCT GROUP                      
         BNE   BLPRG50                                                          
*                                                                               
BLPRG28A TM    NBUNITST,X'42'     PREEMPTED OR MISSED                           
         BNZ   BLPRG50             IGNORE                                       
         TM    22(R6),X'80'       DELETED UNIT                                  
         BO    BLPRG50              IGNORE                                      
*                                                                               
         LA    R6,27(R6)                                                        
         MVI   ELCODE,X'21'                                                     
         MVI   BYTE,0                                                           
         BRAS  RE,NEXTEL                                                        
         BNE   BLPRG29                                                          
         EJECT                                                                  
         USING NUCMLEL,R6                                                       
         MVC   BYTE,NUCMLFLG                                                    
         CLI   SVTNPR5,C'Y'       TRAFFIC DELETES ALLOWED                       
         BNE   BLPRG29            NO                                            
         TM    NUCMLFLG,X'08'                                                   
         BO    BLPRG50                                                          
*                                                                               
*                                                                               
BLPRG29  L     R6,NBAIO                                                         
*                                                                               
         L     R5,AIO2                                                          
         LA    R5,2940(R5)         UNIT ENTRY IS SAVE HERE ???                  
         USING UNTABLED,R5                                                      
*                                                                               
         XC    0(60,R5),0(R5)      WILL SAVE UNTABLE INFO HERE                  
*                                                                               
         MVC   UNTADTEP,NBACTDAT                                                
         MVC   UNTTIME,NBTIME                                                   
         MVC   UNTPROG,NBACTPRG                                                 
         MVC   UNTSUB,NBACTSUB                                                  
         MVC   UNTDSKAD,NBKEY+21                                                
*                                                                               
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(0,WORK)                                
         GOTO1 GETDAY,DMCB,WORK,WORK+6                                          
         LLC   RF,0(R1)            GET DAY OF WEEK                              
*                                                                               
         XC    WORK,WORK                                                        
         LLC   R0,NBSDROT                                                       
         SLL   R0,25                                                            
         SR    R1,R1               CLEAR COUNTER                                
*                                                                               
         LTR   RF,RF                                                            
         BZ    BLPRG29C                                                         
         SLL   R0,1                ONLY COUNT DAYS AFTER NBACTDAT               
         BCT   RF,*-4                                                           
*                                                                               
BLPRG29C LTR   R0,R0               COUNT UNTIL NO DAYS LEFT                     
         BZ    BLPRG29E                                                         
         SLL   R0,1                                                             
         BCT   R1,BLPRG29C                                                      
*                                                                               
BLPRG29E LPR   R0,R1               GET # OF DAYS IN ROT AFTER NBACTDAT          
*                                                                               
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(2,UNTADTEP),(0,WORK)                                
         GOTO1 ADDAY,(R1),WORK,WORK+6,(R0)                                      
         FIXDT02                                                                
         GOTO1 DATCON,(R1),(0,WORK+6),(2,UNTADTE2)                              
*                                                                               
         CLC   UNTADTE2,ENDATEP    CAN NOT RUN OVER ENDATE                      
         BNH   *+10                                                             
         MVC   UNTADTE2,ENDATEP    FORCE TO ENDATE                              
*                                                                               
         DROP  R5                                                               
*                                                                               
BLPRG30  DS    0H                                                               
         OC    NBPR1CL3,NBPR1CL3   UNALLOCATED                                  
         BNZ   BLPRG32                                                          
*                                                                               
         CLI   NBPRD,0             UNALLOCATED                                  
         BNE   BLPRG32                                                          
*                                                                               
         CLI   SVCSPROD,0          MULTI COPY SPLITS                            
         BNE   BLPRG32                                                          
         LH    R1,PRGUNAL                                                       
         LA    R1,1(,R1)                                                        
         STH   R1,PRGUNAL                                                       
         CLI   SVTNPR3,C'Y'        ALLOW UNALLOCATED UNITS                      
         BNE   BLPRG40              NO                                          
         OI    UPDSW,X'02'         SET ON UNITS FOUND SW                        
         B     BLPRG40                                                          
*                                                                               
BLPRG32  OI    UPDSW,X'02'         SET ON UNITS FOUND SW                        
*                                                                               
         LA    R6,27(R6)                                                        
         MVI   ELCODE,X'22'                                                     
         BRAS  RE,NEXTEL                                                        
         BE    *+20                                                             
         LH    R1,PRGUNITS                                                      
         LA    R1,1(,R1)                                                        
         STH   R1,PRGUNITS                                                      
         B     *+16                                                             
*                                                                               
         LH    R1,PRGFDS                                                        
         LA    R1,1(,R1)                                                        
         STH   R1,PRGFDS                                                        
*                                                                               
         L     R6,NBAIO                                                         
         LA    R6,27(R6)                                                        
         MVI   ELCODE,X'21'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   BLPRG36                                                          
         TM    NUCMLFLG,X'E0'      ANY CHANGES (MADE CML INVALID)               
         BNZ   BLPRG36                                                          
*                                                                               
         TM    NBUNST3,X'40'       COPY SPLIT                                   
         BZ    BLPRG32A                                                         
         CLC   NUCMPROD,SPACES                                                  
         BH    BLPRG32A                                                         
         CLI   NUCMLPRD,0                                                       
         BE    BLPRG38                                                          
*                                                                               
BLPRG32A TM    NUCMLFL2,NUCMLFFD   FEED NO NATIONAL                             
         BO    BLPRG40                                                          
*                                                                               
         CLC   NUCML1,=C'REASSIGN'                                              
         BE    BLPRG36                                                          
*                                                                               
         OC    NUCML1,NUCML1       CML ASSIGNED                                 
         BZ    BLPRG36                                                          
*                                                                               
         TM    NBUNST3,X'40'       COPY SPLIT                                   
         BO    BLPRG32C             YES, NOT A P/B                              
*                                                                               
         OC    NBPR2CL3,NBPR2CL3   PROD 2                                       
         BNZ   *+12                                                             
         CLI   NBPRD2,0                                                         
         BE    BLPRG32C                                                         
*                                                                               
         CLC   NUCML2,=C'REASSIGN'                                              
         BE    BLPRG36                                                          
*                                                                               
         OC    NUCML2,NUCML2       CML ASSIGNED                                 
         BZ    BLPRG36                                                          
*                                                                               
BLPRG32C DS    0H                                                               
         MVC   SVCMLCOD,NUCML1                                                  
*                                                                               
         L     R5,AIO2                                                          
         LA    R5,2940(R5)         UNIT ENTRY IS HERE?                          
*                                                                               
* BUILD CML TABLE                                                               
*                                                                               
         BRAS  RE,BCML              BUILD CML TABLE                             
         MVC   NUCML1,SVCMLCOD                                                  
*                                                                               
         OC    NUCML1,NUCML1       VALID CML                                    
         BZ    BLPRG36              NO                                          
*                                                                               
         CLC   =C'ALL',SVCMLTBA    ALL=TBA                                      
         BE    *+14                                                             
         CLC   SVCMLTBA,NUCML1     IS THIS IT                                   
         BNE   BLPRG33                                                          
         LH    R1,PRGTBA           COUNT FORCED TBA                             
         LA    R1,1(,R1)                                                        
         STH   R1,PRGTBA                                                        
*                                                                               
BLPRG33  DS   0H                                                                
         OC    NBPR2CL3,NBPR2CL3   PROD 2                                       
         BNZ   *+12                                                             
         CLI   NBPRD2,0                                                         
         BE    BLPRG35                                                          
*                                                                               
         TM    NBUNST3,X'40'       COPY SPLIT                                   
         BO    BLPRG35              YES, NOT A P/B                              
*                                                                               
         OC    NUCML2,NUCML2       CML2 ASSIGNED                                
         BZ    BLPRG36                                                          
*                                                                               
         MVC   SVCMLCOD,NUCML2                                                  
*                                                                               
* BUILD CML TABLE                                                               
*                                                                               
         BRAS  RE,BCML              BUILD CML TABLE                             
         MVC   NUCML2,SVCMLCOD                                                  
*                                                                               
         OC    NUCML2,NUCML2                                                    
         BZ    BLPRG36                                                          
*                                                                               
         CLC   =C'ALL',SVCMLTBA    ALL=TBA                                      
         BE    *+14                                                             
         CLC   SVCMLTBA,NUCML2                                                  
         BNE   BLPRG35                                                          
         LH    R1,PRGTBA           COUNT FORCED TBA                             
         LA    R1,1(,R1)                                                        
         STH   R1,PRGTBA                                                        
*                                                                               
BLPRG35  DS    0H                                                               
         CLI   SVTN1PR9,C'Y'       BYPASS PREVIOUSLY ASSIGNED CMLS              
         BNE   BLPRG40                                                          
         LH    R1,PRGASS           COUNT PREVIOUSLY ASSIGNED CMLS               
         LA    R1,1(,R1)                                                        
         STH   R1,PRGASS                                                        
         B     BLPRG40                                                          
*                                                                               
BLPRG36  LH    R1,PRGUNAS                                                       
         LA    R1,1(,R1)                                                        
         STH   R1,PRGUNAS                                                       
         B     BLPRG40                                                          
*                                                                               
BLPRG38  LH    R1,PRGUNAL                                                       
         LA    R1,1(,R1)                                                        
         STH   R1,PRGUNAL                                                       
*                                                                               
BLPRG40  MVI   ELCODE,X'23'                                                     
         L     R6,NBAIO                                                         
         LA    R6,27(R6)                                                        
         BRAS  RE,NEXTEL                                                        
         BE    BLPRG44                                                          
         B     BLPRG50                                                          
*                                                                               
BLPRG42  BRAS  RE,NEXTEL                                                        
         BNE   BLPRG50                                                          
         USING NUFDCEL,R6                                                       
BLPRG44  TM    NUFDCFL2,X'80'      DELETED FEED                                 
         BO    BLPRG42                                                          
*                                                                               
         LH    R1,PRGFDS                                                        
         LA    R1,1(,R1)                                                        
         STH   R1,PRGFDS                                                        
*                                                                               
         TM    NBUNST3,X'40'       COPY SPLIT                                   
         BZ    BLPRG46                                                          
*                                                                               
         OC    NUFDPROD,NUFDPROD   UNALLOCATED                                  
         BNZ   BLPRG46                                                          
         CLI   NUFDCPRD,0          UNALLOCATED                                  
         BNE   BLPRG46                                                          
         LH    R1,PRGUNAL                                                       
         LA    R1,1(,R1)                                                        
         STH   R1,PRGUNAL                                                       
         B     BLPRG42                                                          
*                                                                               
BLPRG46  TM    BYTE,X'E0'          ANY CHANGES (MADE CML INVALID)               
         BNZ   BLPRG48                                                          
*                                                                               
         CLC   NUFDCML1,=C'REASSIGN'                                            
         BE    BLPRG48                                                          
*                                                                               
         OC    NUFDCML1,NUFDCML1   CML ASSIGNED                                 
         BZ    BLPRG48                                                          
*                                                                               
         TM    NBUNST3,X'40'       COPY SPLIT                                   
         BZ    BLPRG46C                                                         
*                                                                               
         OC    NBPR2CL3,NBPR2CL3   PROD 2                                       
         BNZ   *+12                                                             
         CLI   NBPRD2,0                                                         
         BE    BLPRG46C                                                         
*                                                                               
         CLC   NUFDCML2,=C'REASSIGN'                                            
         BE    BLPRG48                                                          
*                                                                               
         OC    NUFDCML2,NUFDCML2   CML ASSIGNED                                 
         BZ    BLPRG48                                                          
*                                                                               
BLPRG46C DS    0H                                                               
         MVC   SVCMLCOD,NUFDCML1                                                
*                                                                               
         L     R5,AIO2                                                          
         LA    R5,2940(R5)         UNIT ENTRY IS HERE???                        
*                                                                               
* BUILD CML TABLE                                                               
*                                                                               
         BRAS  RE,BCML              BUILD CML TABLE                             
         MVC   NUFDCML1,SVCMLCOD                                                
*                                                                               
         OC    NUFDCML1,NUFDCML1   VALID CML                                    
         BZ    BLPRG48              NO                                          
*                                                                               
         CLC   =C'ALL',SVCMLTBA    ALL=TBA                                      
         BE    *+14                                                             
         CLC   SVCMLTBA,NUFDCML1                                                
         BNE   BLPRG47                                                          
         LH    R1,PRGTBA           COUNT FORCED TBA                             
         LA    R1,1(,R1)                                                        
         STH   R1,PRGTBA                                                        
*                                                                               
BLPRG47  DS    0H                                                               
         OC    NBPR2CL3,NBPR2CL3   PROD 2                                       
         BNZ   *+12                                                             
         CLI   NBPRD2,0                                                         
         BE    BLPRG49                                                          
*                                                                               
         TM    NBUNST3,X'40'       COPY SPLIT                                   
         BO    BLPRG49                                                          
*                                                                               
         OC    NUFDCML2,NUFDCML2   CML ASSIGNED                                 
         BZ    BLPRG48                                                          
*                                                                               
         MVC   SVCMLCOD,NUFDCML2                                                
*                                                                               
         L     R5,AIO2                                                          
         LA    R5,2940(R5)         UNIT ENTRY IS HERE                           
*                                                                               
* BUILD CML TABLE                                                               
*                                                                               
         BRAS  RE,BCML              BUILD CML TABLE                             
         MVC   NUFDCML2,SVCMLCOD                                                
*                                                                               
         OC    NUFDCML2,NUFDCML2   VALID CML                                    
         BZ    BLPRG48              NO                                          
*                                                                               
         CLC   =C'ALL',SVCMLTBA    ALL=TBA                                      
         BE    *+14                                                             
         CLC   SVCMLTBA,NUFDCML2                                                
         BNE   BLPRG49                                                          
         LH    R1,PRGTBA           COUNT FORCED TBA                             
         LA    R1,1(,R1)                                                        
         STH   R1,PRGTBA                                                        
         B     BLPRG49                                                          
*                                                                               
BLPRG48  LH    R1,PRGUNAS                                                       
         LA    R1,1(,R1)                                                        
         STH   R1,PRGUNAS                                                       
         B     BLPRG42                                                          
BLPRG49  DS    0H                                                               
         CLI   SVTN1PR9,C'Y'       BYPASS PREVIOUSLY ASSIGNED CMLS              
         BNE   BLPRG42                                                          
         LH    R1,PRGASS           COUNT PREV ASSIGNED                          
         LA    R1,1(,R1)                                                        
         STH   R1,PRGASS                                                        
         B     BLPRG42                                                          
*                                                                               
BLPRG50  DS    0H                                                               
         MVI   ERROR,0                                                          
         OI    GENSTAT1,CATCHIOR   I WANT CONTROL BACK                          
         GOTO1 CATCHIOS            DON'T TASKNEXT OUT!                          
         CLI   ERROR,0             ANY ERROR?                                   
         BE    BLPRG55                                                          
         TM    WHEN,X'10'          TEST OVERNIGHT                               
         BO    BLPRG94              YES, LET IT GO THROUGH                      
         B     MAXIOERR                                                         
*                                                                               
BLPRG55  TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY CLIENT                  
         BZ    BLPRG58                                                          
*                                                                               
         CLI   T216FFD+6,C'$'      USER SIGN ON OFFICE LIST                     
         BNE   *+12                                                             
         OI    NBINDS5,NBI5PLST    SET OFFICE LIST FLAG                         
         B     *+12                                                             
         CLI   T216FFD+6,C'*'      USER SIGN ON BY OFFICE                       
         BNE   BLPRG58              NO                                          
*                                                                               
         MVC   NBTRAPSC,T216FFD+7  BRAND LEVEL OFFICE(LIST)                     
*                                                                               
BLPRG58  GOTO1 ANETIO,DMCB,(R3)                                                 
*                                                                               
         CLI   NBERROR,NBGOOD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBREQLST                                                  
         BE    BLPRG60                                                          
         CLI   NBMODE,NBPROCUN                                                  
         BNE   BLPRG50                                                          
         TM    NBSUBMSK,NBSBMCLI+NBSBMNET                                       
         BNZ   BLPRG60                                                          
         TM    NBSUBMSK,NBSBMPRG                                                
         BZ    BLPRG20                                                          
         OC    PROGRAM,PROGRAM     WAS PROGRAM = ALL                            
         BZ    BLPRG64                                                          
*                                                                               
BLPRG60  OC    PROGRAM,PROGRAM     WAS PROGRAM = ALL                            
         BZ    BLPRG62                                                          
*                                                                               
         BRAS  RE,NXTEQV           GET NEXT EQUIV PROGRAM IF ANY                
         BNE   BLPRG20              ROLL IT INTO THIS                           
*                                                                               
BLPRG62  OI    UPDSW,X'40'         END OF UNITS SWITCH                          
*                                                                               
BLPRG64  CLI   OFFLINE,C'Y'                                                     
         BNE   BLPRG70                                                          
*                                                                               
*NOP     TM    SVFLAG,SVTSERR      ANY TRAFFIC SUPPLIER ERRORS?                 
******   BO    NOTSUPER                                                         
*                                                                               
         CLI   SVTNPR3,C'Y'        ALLOW UNALLOCATED UNITS                      
         BE    BLPRG66                                                          
         OC    PRGUNAL,PRGUNAL     ANY UNALLOCATED UNITS                        
         BNZ   PUNALOC                                                          
*                                                                               
BLPRG66  DS    0H                                                               
         B     BLPRG80                                                          
*                                                                               
* THIS IS FOR OVERNITE SEED, SEE IF ANY ERRORS *                                
* IF AT LEAST 1 GOOD PROGRAM, SUBMIT REQUEST   *                                
*                                                                               
BLPRG70  TM    WHEN,X'40'          IF NOW, GO AHEAD                             
         BO    BLPRG80              YES                                         
*                                                                               
*NOP     TM    SVFLAG,SVTSERR      ANY TRAFFIC SUPPLIER ERRORS?                 
*****    BO    BLPRG86              YES                                         
*                                                                               
         CLI   SVTNPR3,C'Y'        ALLOW UNALLOCATED UNITS                      
         BE    BLPRG76                                                          
         CLC   PRGUNITS,PRGUNAL    ALL ALLOCATED UNITS                          
         BNH   BLPRG86                                                          
*                                                                               
BLPRG76  TM    UPDSW,X'02'         ANY UNITS FOUND SW                           
         BO    BLPRG94              ONLY NEED 1 PROG FOR OFFLINE                
         EJECT                                                                  
* CHECK FOR NEW/REV ONLY OR ALL *                                               
*                                                                               
BLPRG80  CLI   OFFLINE,C'Y'        IF OFFLINE TABLE BUILD                       
         BE    BLPRG84                                                          
         TM    WHEN,X'40'          IF NOW, ALSO BUILD TABLE                     
         BZ    BLPRG86              IF ONLINE VALIDATE, NO TABLE                
*                                                                               
BLPRG84  CLC   PRGPRG,NBACTPRG     FOR EQV PROG, CK IF THERE                    
         BE    BLPRG88                                                          
*                                                                               
         LA    R4,PRGNEXT                                                       
         C     R4,APRGTBLX          AT END OF TABLE                             
         BL    BLPRG85                                                          
         MVC   GERROR,=Y(MANYPGM)                                               
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         STCM  R1,7,GASUBST        A(SUBST TEXT)                                
         MVI   ELEM,4              L'SUBST TEXT + 1                             
         MVC   ELEM+1(3),=C'270'                                                
*                                                                               
         L     R2,=A(WORKFIL)      CLOSE WORKFIL ON ERROR                       
         CLOSE ((R2),)                                                          
         FREEPOOL (R2)                                                          
*                                                                               
         LA    R2,TRACLTH                                                       
         B     FORCEVK1                                                         
*                                                                               
BLPRG85  OC    PRGPRG,PRGPRG       EMPTY ENTRY                                  
         BNZ   BLPRG84                                                          
*                                                                               
BLPRG86  XC    PRGENT(L'PRGENT+2),PRGENT                                        
*                                                                               
BLPRG88  TM    UPDSW,X'40'         END OF UNITS                                 
         BZ    BLPRG14                                                          
*                                                                               
BLPRG90  TM    UPDSW,X'02'         ANY UNITS FOUND SW                           
         BZ    NOUNTER1             NO                                          
         C     R4,APRGTBLE         ANY PROGRAMS FOUND                           
         BH    BLPRG94              YES                                         
         OC    0(6,R4),0(R4)                                                    
         BZ    BLPRG96                                                          
BLPRG94  CR    RB,RB                                                            
         B     BLPRGX                                                           
BLPRG96  TM    WHEN,X'40'          IF NOT NOW, ERROR                            
         BZ    PRTERR                                                           
         CR    RB,RC                                                            
BLPRGX   LA    R4,L'PRGENT(R4)                                                  
         LA    R4,L'PRGENT(R4)                                                  
         ST    R4,APRGTBLX         RESET END OF TBL                             
BLPRGXIT XIT1                                                                   
*                                                                               
PRTERR   DS    0H                                                               
         BRAS  RE,GOPER                                                         
         MVC   GERROR,=Y(NOGDPRG)                                               
         B     FORCEVK1                                                         
*                                                                               
MAXIOERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'MAXIOMSG),MAXIOMSG                                     
         NI    TRACLTH+4,X'FF'-X'20' FORCE CLIENT CHANGED                       
         GOTO1 ERREX2                                                           
MAXIOMSG DC    C'*TOO MANY I/OS, MUST SEED BY NETWORK OR OVERNIGHT*'            
         EJECT                                                                  
*                                                                               
* GENERATE COMML SEED ERROR REPORT *                                            
*                                                                               
PUNALOC  MVI   BYTE,2             SOME UNALLOCATED UNITS                        
         CLI   OFFLINE,C'Y'                                                     
         BNE   BLPRG80                                                          
         MVC   ELEM,SPACES                                                      
         LA    R2,ELEM                                                          
         USING XRECD,R2                                                         
         MVC   XBAGYMD,BAGYMD                                                   
         MVC   XCLT,QCLT                                                        
         MVC   XNET,NETWORK                                                     
         MVC   XNETMKT,NETMKT                                                   
         MVC   XPROG,PRGPRG                                                     
         MVC   XTUNT,PRGUNITS                                                   
         MVC   XUNAS,PRGUNAS                                                    
         MVC   XUNAL,PRGUNAL                                                    
         MVC   XPERST,STDATEP                                                   
         MVC   XPERND,ENDATEP                                                   
         MVC   XTYPE,BYTE                                                       
         CLI   SVTN1PR9,C'Y'       BYPASS ASSIGNED CMLS                         
         BNE   *+10                                                             
         MVC   XCMLAS,PRGASS                                                    
*                                                                               
         L     R1,=A(WORKFIL)                                                   
         LA    R0,ELEM                                                          
         PUT   (1),(0)                                                          
         B     BLPRG80                                                          
         DROP  R4                                                               
         EJECT                                                                  
PROGTER1 DS    0H                                                               
         BRAS  RE,GOPER                                                         
         MVC   GERROR,=Y(BDPGMDAT)                                              
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         STCM  R4,7,GASUBST        A(SUBST TEXT)                                
         MVI   0(R4),7             L'SUBST TEXT + 1                             
         MVC   1(6,R4),NBACTPRG                                                 
         MVI   7(R4),8             L'SUBST TEXT + 1                             
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(2,KEYSAVE+11),(8,8(R4))                             
         B     FORCEVK1                                                         
*                                                                               
NOUNTER1 DS    0H                                                               
         CLI   SVNET,0             ALL NETWORK REQUEST                          
         BNE   BLPRGXIT             YES GO GET NEXT NETWORK                     
*                                                                               
         BRAS  RE,GOPER                                                         
*                                                                               
         MVC   GERROR,=Y(NOUNTSEL)                                              
         LA    R2,TRACLTH                                                       
*                                                                               
FORCEVK1 NI    TRACLTH+4,X'FF'-X'20' FORCE CLIENT CHANGED                       
         GOTO1 VTRAERR                                                          
         DROP  R3,R6                                                            
         LTORG                                                                  
         EJECT                                                                  
* GET BASE PROGRAM CODE FOR EQUIVALENT PROGRAM CODE *                           
*                                                                               
         DS   0H                                                                
FPRGC    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R3,ANETBLK                                                       
         USING NETBLCKD,R3                                                      
         USING PRGTABLD,R5                                                      
*                                                                               
         MVI   BYTE,0              INIT FOR ERROR                               
*                                                                               
         XC    SVCSPROD,SVCSPROD                                                
         MVC   BASEPRG,NBACTPRG                                                 
         XC    BASEDTS,BASEDTS                                                  
         MVI   BASECT,0                                                         
*                                                                               
         L     R6,NBAIO                                                         
         LA    R6,27(,R6)                                                       
         MVI   ELCODE,X'19'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   FPRGC0Z                                                          
         USING NUPDED,R6                                                        
         SR    R0,R0                                                            
         LLC   R1,1(R6)                                                         
         D     R0,=F'7'                                                         
         CH    R1,=H'2'            MUST BE MORE THAN 2 PRODUCTS ELSE            
         BNH   FPRGC06             DON'T BUILD SVCSPROD TABLE                   
                                                                                
         LR    R0,R1                                                            
         LA    RE,NUPDEPR                                                       
         LA    RF,SVCSPROD                                                      
FPRGC0A  MVC   0(3,RF),0(RE)                                                    
         LA    RE,7(,RE)                                                        
         LA    RF,3(,RF)                                                        
         BCT   R0,FPRGC0A                                                       
         B     FPRGC06                                                          
                                                                                
FPRGC0Z  DS    0H                                                               
         L     R6,NBAIO                                                         
         LA    R6,27(,R6)                                                       
         MVI   ELCODE,X'14'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   FPRGC06                                                          
         USING NUPRDD,R6                                                        
         SR    R0,R0                                                            
         LLC   R1,1(R6)                                                         
         D     R0,=F'6'                                                         
         CH    R0,=H'3'            MUST BE A REMAINDER OF 3                     
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R0,R1                                                            
         CH    R0,=H'1'            MUST BE MORE THAN 1 PRODUCT                  
         BH    *+6                                                              
         DC    H'0'                                                             
         LA    RE,NUPRDPR                                                       
         LA    RF,SVCSPROD                                                      
*                                                                               
FPRGC01  DS   0H                                                                
         LA    R2,NCLSTSIZ         MAX COUNT BUG CATCHER                        
         L     R1,ASVNCLST         TABLE OF CLIENT PROD CODES                   
FPRGC02  DS   0H                                                                
         CLC   3(1,R1),0(RE)       THIS A VALID PROD CODE                       
         BE    FPRGC04                                                          
         LA    R1,4(,R1)           BUMP PROD PTR                                
         CLI   0(R1),C' '          AT END OF TABLE?                             
         BNH   *+8                  YES, ERROR                                  
         BCT   R2,FPRGC02                                                       
         DC    H'0'                                                             
FPRGC04  MVC   0(3,RF),0(R1)                                                    
         LA    R1,6(,R1)                                                        
         LA    RF,3(,RF)                                                        
         BCT   R0,FPRGC01                                                       
*                                                                               
         CLI   SVCSPROD,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         TM    NUPRDIND,X'80'      IS THIS TRI-BACK                             
         BZ    *+12                                                             
         MVI   BYTE,7              YES, ERROR TYPE 7 (NO TRI-BACKS)             
         B     FPRGC60                                                          
*                                                                               
         TM    NBUNST3,X'40'       IS THIS A COPY SPLIT                         
         BO    *+6                  YES                                         
         DC    H'0'                                                             
*                                                                               
FPRGC06  LA    R2,EQVPTBL                                                       
         LA    R4,L'EQVPTBL/(EQVNEXT-EQVENT)                                    
         SR    R6,R6                                                            
*                                                                               
         USING EQVPTBLD,R2                                                      
FPRGC10  OC    EQVEPRG,EQVEPRG     END OF ENTRIES                               
         BZ    FPRGC16              YES                                         
*                                                                               
         CLC   NBACTPRG,EQVEPRG                                                 
         BNE   FPRGC14                                                          
*                                                                               
         BCTR  R6,0                SET PROG CODE FOUND                          
*                                                                               
         CLC   NBACTDAT,EQVSDT     WITHIN DATES                                 
         BL    FPRGC14                                                          
         CLC   NBACTDAT,EQVEDT                                                  
         BNH   FPRGC20                                                          
*                                                                               
FPRGC14  LA    R2,EQVNEXT                                                       
         BCT   R4,FPRGC10                                                       
*                                                                               
FPRGC16  LTR   R6,R6               WAS PROGRAM CODE FOUND                       
         BZ    FPRGC26                                                          
         MVI   BYTE,4                                                           
         B     FPRGC60              YES, PARTIAL PERIOD                         
*                                                                               
FPRGC20  MVC   BASEPRG,EQVBPRG     SET FOR USE                                  
*                                                                               
         MVC   BASEDTS,EQVSDT                                                   
*                                                                               
         CLC   BASEEND,ENDATEP     PAST END DATE                                
         BNH   *+10                 NO                                          
         MVC   BASEEND,ENDATEP     DON'T RUN PAST END DATE                      
*                                                                               
         MVC   BASECT,EQVCT                                                     
         MVI   EQVUSED,C'Y'                                                     
*                                                                               
FPRGC26  CLC   PRGPRG,BASEPRG                                                   
         BE    FPRGC50                                                          
*                                                                               
         L     R5,APRGTBLE                                                      
*                                                                               
         USING EQVPTBLD,R2                                                      
FPRGC30  OC    PRGENT,PRGENT       END OF ENTRIES                               
         BZ    FPRGC40              YES                                         
         CLC   PRGPRG,BASEPRG                                                   
         BE    FPRGC50                                                          
*                                                                               
         LA    R5,PRGNEXT                                                       
         C     R5,APRGTBLX                                                      
         BL    FPRGC30                                                          
         DC    H'0'                                                             
*                                                                               
FPRGC40  MVC   PRGPRG,BASEPRG                                                   
*                                                                               
FPRGC50  OC    PRGINSDT,PRGINSDT   GOT REVISION INFO YET                        
         BNZ   FPRGC54                                                          
*                                                                               
* GET REVISION INFO *                                                           
*                                                                               
         BAS   RE,FREV                                                          
*                                                                               
FPRGC54  CR    RB,RB               SET CONDITION CODE TO OK                     
*                                                                               
FPRGCX   XIT1  REGS=(R5)                                                        
*                                                                               
* PROGRAM CODE WAS EQUIVALENT FOR PART OF PERIOD, ERROR, BYPASS *               
*                                                                               
FPRGC60  CLI   OFFLINE,C'Y'                                                     
         BNE   FPRGC70                                                          
         MVC   ELEM,SPACES                                                      
         LA    R2,ELEM                                                          
         USING XRECD,R2                                                         
         MVC   XBAGYMD,BAGYMD                                                   
         MVC   XCLT,QCLT                                                        
         MVC   XNET,NETWORK                                                     
         MVC   XNETMKT,NETMKT                                                   
         MVC   XPROG,NBACTPRG                                                   
         XC    XTUNT,XTUNT                                                      
         XC    XUNAS,XUNAS                                                      
         XC    XUNAL,XUNAL                                                      
         MVC   XPERST,NBACTDAT                                                  
         MVC   XTYPE,BYTE                                                       
*                                                                               
         CLI   XTYPE,7                                                          
         BNE   FPRGC62                                                          
*                                                                               
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(0,WORK)                                
         GOTO1 GETDAY,DMCB,WORK,WORK+6                                          
*                                                                               
         XC    WORK,WORK                                                        
*                                                                               
         LLC   RF,0(R1)            GET DAY OF WEEK                              
         LLC   R0,NBSDROT                                                       
         SLL   R0,25                                                            
         SR    R1,R1               CLEAR COUNTER                                
*                                                                               
         LTR   RF,RF                                                            
         BZ    FPRGC61                                                          
         SLL   R0,1                ONLY COUNT DAYS AFTER NBACTDAT               
         BCT   RF,*-4                                                           
*                                                                               
FPRGC61  LTR   R0,R0               COUNT UNTIL NO DAYS LEFT                     
         BZ    FPRGC61E                                                         
         SLL   R0,1                                                             
         BCT   R1,FPRGC61                                                       
*                                                                               
FPRGC61E LPR   R0,R1               GET # OF DAYS IN ROT AFTER NBACTDAT          
*                                                                               
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(2,XPERST),(0,WORK)                                  
         GOTO1 ADDAY,(R1),WORK,WORK+6,(R0)                                      
         FIXDT02                                                                
         GOTO1 DATCON,(R1),(0,WORK+6),(2,XPERND)                                
*                                                                               
         CLC   XPERND,ENDATEP      CAN NOT RUN OVER ENDATE                      
         BNH   *+10                                                             
         MVC   XPERND,ENDATEP      FORCE TO ENDATE                              
*                                                                               
FPRGC62  L     R1,=A(WORKFIL)                                                   
         LA    R0,ELEM                                                          
         PUT   (1),(0)                                                          
         B     FPRGC70                                                          
*                                                                               
FPRGC70  LTR   RB,RB               SET CONDITION CODE TO NG                     
         B     FPRGCX                                                           
         DROP  R2,R5                                                            
         EJECT                                                                  
* GET REVISION INFO *                                                           
*                                                                               
         USING PRGTABLD,R5                                                      
FREV     NTR1                                                                   
         BRAS  RE,SETXSP           SET TO XSPOT                                 
*                                                                               
         MVI   OREVNN,0            INIT PROG REV NUM FOR OTHER RECS             
         MVI   OREVNUM,0                AND OTHER NET REV NUM                   
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING REVXKEY,R4                                                       
KS       USING REVXKEY,KEYSAVE                                                  
*                                                                               
         MVC   REVXKID,=X'0A1D'                                                 
         MVC   REVXKAM(3),BAGYMD AND BCLT                                       
         MVC   REVXKNET,NETWORK                                                 
         MVC   REVXKPRG,PRGPRG                                                  
         MVC   REVXKPER,PERIOD                                                  
*                                                                               
         CLI   MYTN2PR6,C'W'       USING WEEKLY PERIOD                          
         BNE   *+18                                                             
         TM    SVFLAG,CONVSW       CONVERTED RECORDS?                           
         BNZ   *+10                YES, ALREADY SET                             
         MVC   REVXKPER,STDATEP                                                 
*                                                                               
         CLI   SVTN2PRO+00,C'*'    TRAFFIC BY PRODUCT                           
         BNE   FREV06                                                           
         CLI   OPTPROD,0           RUNNING BY PRODUCT                           
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   REVXKPRD,OPTPROD                                                 
*                                                                               
FREV06   DS   0H                                                                
         OC    OPTPRGR,OPTPRGR    ANY PRODUCT GROUP                             
         BZ    *+10                                                             
         MVC   REVXKPGR(2),OPTPRGR                                              
*                                                                               
         MVC   KEYSAVE,KEY                                                      
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'REVXKEY),KEYSAVE                                           
         BE    FREV10                                                           
*                                                                               
FREV07B  CLC   KEY(REVXKNUM-REVXKEY),KEYSAVE                                    
         BNE   FREV30                                                           
*                                                                               
         OC    OPTPROD,OPTPROD     RUNNING BY PRODUCT                           
         BZ    FREV07C                                                          
         CLC   REVXKPRD,OPTPROD                                                 
         BNE   FREV08                                                           
         BE    FREV10                                                           
*                                                                               
FREV07C  OC    OPTPRGR,OPTPRGR     BY PRODUCT GROUP                             
         BZ    FREV07F                                                          
         CLC   REVXKPGR,OPTPRGR                                                 
         BNE   FREV08                                                           
         BE    FREV10                                                           
*                                                                               
FREV07F  OC    REVXKPRD(L'REVXKPRD+L'REVXKPGR),REVXKPRD PRD&PG                  
         BZ    FREV10                                                           
*                                                                               
FREV08   MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
         B     FREV07B                                                          
*                                                                               
* TEST REVISED/ORIGINAL HERE *                                                  
*                                                                               
FREV10   L     R6,AIO2                                                          
         ST    R6,AIO                                                           
                                                                                
*NOP     GOTO1 (RF),(R1),=C'GETREC',=C'UNTFIL',KEY+21,(R6),DMWORK               
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         CLC   KEY(L'REVXKEY),0(R6)                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PRGREVN,REVXKNUM-REVXKEY(R6)                                     
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING REVREVEL,R6                                                      
*                                                                               
         CLI   MYTN2PR6,C'B'                                                    
         BNE   *+12                                                             
         OI    REVFLAG,REVBRD      TURN ON PERIOD IS BROAD MONTH                
         B     *+16                                                             
         CLI   MYTN2PR6,C'C'                                                    
         BNE   *+8                                                              
         OI    REVFLAG,REVCAL      TURN ON PERIOD IS CALENDAR MONTH             
*                                                                               
         NI    PRGFLG,X'FF'-X'20' SET OFF INSTR RUN THIS REV                    
*                                                                               
         MVC   PRGINSDT,REVIDATE                                                
*                                                                               
         TM    REVFLAG,X'80'      INSTR RUN FOR THIS REV                        
         BZ    FREV20                                                           
*                                                                               
         OI    PRGFLG,PRGFLTRN    SET ON INSTR RUN                              
*                                                                               
         LLC   R1,PRGREVN                                                       
         LA    R1,1(,R1)                                                        
         STC   R1,PRGREVN                                                       
*                                                                               
FREV20   DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
*                                                                               
         CLC   KEY(REVXKNUM-REVXKEY),KEYSAVE                                    
         BNE   FREVX                                                            
*                                                                               
         CLI   SVTN2PRO+00,C'*'                                                 
         BNE   FREV22                                                           
*                                                                               
         OC    OPTPROD,OPTPROD                                                  
         BZ    FREV22                                                           
         CLC   KEY+REVXKPRD-REVXKEY(3),OPTPROD                                  
         BNE   FREV20                                                           
         BE    FREV10                                                           
*                                                                               
FREV22   OC    OPTPRGR,OPTPRGR                                                  
         BZ    FREV22C                                                          
         CLC   KEY+REVXKPGR-REVXKEY(2),OPTPRGR                                  
         BNE   FREV20                                                           
         BE    FREV10                                                           
FREV22C  OC    REVXKPRD(L'REVXKPRD+L'REVXKPGR),REVXKPRD PRD&PG                  
         BZ    FREV10                                                           
         B     FREV20                                                           
         EJECT                                                                  
* NO REV REC FOUND, CK FOR WKLY IF RUNNING MONTH OR MONTHLY IF WEEKLY *         
*                                                                               
FREV30   OI    PRGFLG,X'10'        NEED TO ADD ORG REV REC                      
         MVC   KEY,KEYSAVE                                                      
         MVI   REVXKNUM,0          SET REVISION TO ZERO                         
         CLI   OPTPROD,0           RUNNING BY PRODUCT                           
         BE    *+10                                                             
         MVC   REVXKPRD,OPTPROD                                                 
*                                                                               
         OC    OPTPRGR,OPTPRGR     RUNNING BY PRODUCT GROUP                     
         BZ    *+10                                                             
         MVC   REVXKPGR(2),OPTPRGR                                              
*                                                                               
         CLI   MYTN2PR6,C'W'                                                    
         BE    FREV40                                                           
         MVC   WORK(6),STDATE                                                   
         GOTO1 GETDAY,DMCB,(0,STDATE),WORK+6                                    
         CLC   WORK+6(3),SPACES                                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   0(R1),1             IF MONDAY, DONE                              
         BE    FREV34                                                           
         LLC   R6,DMCB                                                          
         BCTR  R6,0                                                             
         LNR   R6,R6                                                            
         GOTO1 ADDAY,(R1),STDATE,WORK,(R6)                                      
         FIXDT02                                                                
FREV34   GOTO1 DATCON,(R1),(0,WORK),(2,WORK+6)                                  
                                                                                
         TM    SVFLAG,CONVSW       CONVERTED RECORDS?                           
         BZ    *+8                                                              
         BRAS  RE,CONVPER                                                       
                                                                                
         MVC   REVXKPER,WORK+6                                                  
         XC    SVLGKEY,SVLGKEY     SAVE LAST GOOD KEY                           
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
FREV34B  CLC   KEY(REVXKNUM-REVXKEY),KEYSAVE                                    
         BNE   FREV38               LOOK UP NEXT WEEK                           
*                                                                               
         OC    OPTPROD,OPTPROD     RUNNING BY PRODUCT                           
         BZ    FREV34C                                                          
         CLC   REVXKPRD,OPTPROD                                                 
         BNE   FREV34G                                                          
         BE    FREV35                                                           
*                                                                               
FREV34C  OC    OPTPRGR,OPTPRGR     BY PRODUCT GROUP                             
         BZ    FREV34F                                                          
         CLC   REVXKPGR,OPTPRGR                                                 
         BNE   FREV34G                                                          
         BE    FREV35                                                           
*                                                                               
FREV34F  OC    REVXKPRD(L'REVXKPRD+L'REVXKPGR),REVXKPRD PRD&PG                  
         BZ    FREV35                                                           
*                                                                               
FREV34G  MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
         B     FREV34B                                                          
*-------------------------------------------                                    
* READ ALL REVISION RECORDS FOR THIS PERIOD                                     
* AND GET HIGHEST REVISION NUMBER USED                                          
*-------------------------------------------                                    
FREV35   CLC   OREVNUM,REVXKNUM    COMPARE OTHER REV # TO THIS                  
         BNL   *+10                                                             
         MVC   OREVNUM,REVXKNUM    SAVE THIS REV#                               
*                                                                               
         MVC   SVLGKEY,KEY            SAVE LAST GOOD KEY                        
*                                                                               
FREV35F  MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
         CLC   KEY(REVXKNUM-REVXKEY),KEYSAVE                                    
         BNE   FREV37               LOOK UP NEXT WEEK                           
*                                                                               
FREV36   CLI   OPTPROD,0           RUNNING BY PRODUCT                           
         BE    FREV36B                                                          
         CLC   REVXKPRD,OPTPROD                                                 
         BE    FREV35                                                           
         BNE   FREV35F             GET NEXT RECORD                              
FREV36B  OC    OPTPRGR,OPTPRGR     RUNNING BY PRODUCT GROUP                     
         BZ    FREV36C                                                          
         CLC   REVXKPGR(2),OPTPRGR                                              
         BNE   FREV35F             GET NEXT RECORD                              
         BE    FREV35                                                           
FREV36C  OC    REVXKPRD(L'REVXKPRD+L'REVXKPGR),REVXKPRD PRD&PGR                 
         BZ    FREV35                                                           
         B     FREV35F             GET NEXT RECORD                              
*                                                                               
* RE-READ LAST GOOD RECORD                                                      
*                                                                               
FREV37   OC    SVLGKEY,SVLGKEY     ANY GOOD RECORDS                             
         BZ    FREV38                                                           
         MVC   KEY,SVLGKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(L'REVXKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                 MUST BE THERE                               
*                                                                               
         CLC   OREVNUM,REVXKNUM    IS OTHER REV# HIGHER                         
         BH    FREV38               YES, DONE                                   
         BE    *+6                                                              
         DC    H'0'                OTHER REV# IS LOWER ?  BUG???                
*                                                                               
         L     R6,AIO2             WAS AIO3                                     
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING REVREVEL,R6                                                      
*                                                                               
         CLI   REVDTALN,12         OLD REV REC                                  
         BE    *+20                                                             
         CLC   OREVNN,REVNNUM                                                   
         BNL   *+10                                                             
         MVC   OREVNN,REVNNUM      SAVE NET REV NUM FOR OTHER PERIOD            
*                                                                               
         TM    REVFLAG,X'80'       HAVE INSTRUCTIONS BEEN RUN                   
         BZ    FREV38              NO,LOOK UP NEXT WEEK                         
*                                                                               
         LLC   RF,OREVNUM          YES, UP REVISION NUMBER                      
         LA    RF,1(,RF)                                                        
         STC   RF,OREVNUM                                                       
*                                                                               
* LOOK UP NEXT WEEK IF NOT DONE WITH THIS MONTH                                 
*                                                                               
FREV38   MVC   KEY,KEYSAVE                                                      
         MVI   REVXKNUM,0          SET REVISION TO ZERO                         
         CLI   OPTPROD,0           RUNNING BY PRODUCT                           
         BE    *+10                                                             
         MVC   REVXKPRD,OPTPROD                                                 
*                                                                               
         OC    OPTPRGR,OPTPRGR     RUNNING BY PRODUCT GROUP                     
         BZ    *+10                                                             
         MVC   REVXKPGR(2),OPTPRGR                                              
*                                                                               
         GOTO1 ADDAY,(R1),WORK,WORK,F'7' GET MONDAY OF NEXT WEEK                
*                                                                               
         CLC   STDATE+2(2),WORK+2  SAME MONTH?                                  
         BE    FREV34               YES                                         
*                                                                               
         CLI   MYTN2PR6,C'B'                                                    
         BNE   FREVX                                                            
*                                                                               
         CLC   ENDATE+2(2),=C'12'                                               
         BNE   *+14                                                             
         CLC   WORK+2(2),=C'01'                                                 
         BE    FREVX                                                            
*                                                                               
         CLC   ENDATE+2(2),WORK+2  SAME MONTH?                                  
         BNL   FREV34               YES                                         
         B     FREVX                                                            
*                                                                               
* CK FOR MONTHLY REV REC *                                                      
*                                                                               
FREV40   DS    0H                                                               
         GOTO1 DATCON,DMCB,(0,STDATE),(3,WORK)                                  
         MVC   REVXKPER,WORK                                                    
         XC    SVLGKEY,SVLGKEY     INIT LAST GOOD KEY                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'REVXKEY),KEYSAVE                                           
         BE    FREV42                                                           
*                                                                               
FREV40B  CLC   KEY(REVXKNUM-REVXKEY),KEYSAVE                                    
         BNE   FREV50                                                           
*                                                                               
         OC    OPTPROD,OPTPROD     RUNNING BY PRODUCT                           
         BZ    FREV40C                                                          
         CLC   REVXKPRD,OPTPROD                                                 
         BNE   FREV40G                                                          
         BE    FREV42                                                           
*                                                                               
FREV40C  OC    OPTPRGR,OPTPRGR     BY PRODUCT GROUP                             
         BZ    FREV40F                                                          
         CLC   REVXKPGR,OPTPRGR                                                 
         BNE   FREV40G                                                          
         BE    FREV42                                                           
*                                                                               
FREV40F  OC    REVXKPRD(L'REVXKPRD+L'REVXKPGR),REVXKPRD PRD&PG                  
         BZ    FREV42                                                           
*                                                                               
FREV40G  MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
         B     FREV40B                                                          
*----------------------------------------------                                 
*                                                                               
* READ ALL REVISION RECORDS FOR THIS PERIOD                                     
* AND GET HIGHEST REVISION NUMBER USED                                          
*----------------------------------------------                                 
FREV42   CLC   OREVNUM,REVXKNUM    COMPARE OTHER REV # TO THIS                  
         BNL   *+10                                                             
         MVC   OREVNUM,REVXKNUM    SAVE THIS REV#                               
*                                                                               
         MVC   SVLGKEY,KEY         LAST GOOD KEY                                
*                                                                               
FREV42F  MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
*                                                                               
         CLC   KEY(REVXKNUM-REVXKEY),KEYSAVE                                    
         BNE   FREV45               LOOK UP NEXT WEEK                           
*                                                                               
FREV43   CLI   OPTPROD,0           RUNNING BY PRODUCT                           
         BE    FREV43B                                                          
         CLC   REVXKPRD,OPTPROD                                                 
         BE    FREV42                                                           
         BNE   FREV42F             GET NEXT RECORD                              
FREV43B  OC    OPTPRGR,OPTPRGR     RUNNING BY PRODUCT GROUP                     
         BZ    FREV43C                                                          
         CLC   REVXKPGR(2),OPTPRGR                                              
         BNE   FREV42F             GET NEXT RECORD                              
         BE    FREV42                                                           
FREV43C  OC    REVXKPRD(L'REVXKPRD+L'REVXKPGR),REVXKPRD PRD&PGR                 
         BZ    FREV42                                                           
         B     FREV42F             GET NEXT RECORD                              
*                                                                               
* RE-READ LAST GOOD RECORD                                                      
*                                                                               
FREV45   OC    SVLGKEY,SVLGKEY     ANY GOOD RECORD                              
         BZ    FREV50                                                           
         MVC   KEY,SVLGKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(L'REVXKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                 MUST BE THERE                               
*                                                                               
         L     R6,AIO2             WAS AIO3                                     
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING REVREVEL,R6                                                      
*                                                                               
         CLI   REVDTALN,12         OLD REV REC                                  
         BE    *+20                                                             
         CLC   OREVNN,REVNNUM                                                   
         BNL   *+10                                                             
         MVC   OREVNN,REVNNUM      SAVE NET REV NUM FOR OTHER PERIOD            
*                                                                               
         TM    REVFLAG,X'80'       HAVE INSTRUCTIONS BEEN RUN                   
         BZ    FREV50                                                           
*                                                                               
         LLC   RF,OREVNUM          YES, UP REVISION NUMBER                      
         LA    RF,1(,RF)                                                        
         STC   RF,OREVNUM                                                       
*                                                                               
FREV50   DS    0H                                                               
         MVC   KEY,KEYSAVE                                                      
         MVI   REVXKNUM,0          SET REVISION TO ZERO                         
         CLI   OPTPROD,0           RUNNING BY PRODUCT                           
         BE    *+10                                                             
         MVC   REVXKPRD,OPTPROD                                                 
*                                                                               
         OC    OPTPRGR,OPTPRGR     RUNNING BY PRODUCT GROUP                     
         BZ    *+10                                                             
         MVC   REVXKPGR(2),OPTPRGR                                              
*                                                                               
         MVC   WORK(6),STDATE                                                   
*                                                                               
         GOTO1 =V(GETBROAD),DMCB,(1,WORK),WORK+6,GETDAY,ADDAY,         C        
               RR=SPTR62RR                                                      
*                                                                               
         CLC   WORK+8(2),WORK+14   START AND END MONTH SAME                     
         BE    FREV55                                                           
*                                                                               
* ONLY INCREMENT MONTH IF DAY IS OVER 21 *                                      
*                                                                               
         CLC   =C'20',WORK+2                                                    
         BL    FREV55                                                           
         GOTO1 ADDAY,(R1),WORK+6,WORK+6,F'15'                                   
FREV55   GOTO1 DATCON,(R1),(0,WORK+6),(3,WORK)                                  
*                                                                               
         CLC   REVXKPER,WORK                                                    
         BE    FREVX               DONE THIS PERIOD BEFORE                      
*                                                                               
         MVC   REVXKPER,WORK                                                    
         B     FREV60                                                           
*                                                                               
* CODE IS BYPASSED TO SEE WHAT IS THERE, PROD & PGRP CKD BEFORE                 
*                                                                               
*        MVC   REVXKPER,PERIOD                                                  
*        SPACE                                                                  
*        CLI   SVTN2PRO+00,C'*'    TRAFFIC BY PRODUCT                           
*        BNE   FREV58                                                           
*        CLI   OPTPROD,0           RUNNING BY PRODUCT                           
*        BE    *+10                                                             
*        MVC   REVXKPRD,OPTPROD                                                 
*                                                                               
*REV58   DS   0H                                                                
*        OC    OPTPRGR,OPTPRGR     RUNNING BY PRODUCT GROUP                     
*        BZ    *+10                                                             
*        MVC   REVXKPRG,OPTPRGR                                                 
*                                                                               
FREV60   MVI   RDUPDATE,C'N'                                                    
         BRAS  RE,SETXSP           SET TO XSPOT                                 
         XC    SVLGKEY,SVLGKEY                                                  
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'REVXKEY),KEYSAVE                                           
         BE    FREV62                                                           
*                                                                               
         CLC   KEY(REVXKNUM-REVXKEY),KEYSAVE                                    
         BNE   FREVX                                                            
*                                                                               
FREV61   CLI   OPTPROD,0           RUNNING BY PRODUCT                           
         BE    FREV61B                                                          
         CLC   REVXKPRD,OPTPROD                                                 
         BE    FREV62                                                           
         BNE   FREV61F                                                          
*                                                                               
FREV61B  OC    OPTPRGR,OPTPRGR     RUNNING BY PRODUCT GROUP                     
         BZ    FREV61C                                                          
         CLC   REVXKPGR(2),OPTPRGR                                              
         BNE   FREV61F                                                          
         BE    FREV62                                                           
FREV61C  OC    REVXKPRD(L'REVXKPRD+L'REVXKPGR),REVXKPRD PRD&PGR                 
         BZ    FREV62                                                           
*                                                                               
FREV61F  MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
*                                                                               
         CLC   KEY(REVXKNUM-REVXKEY),KEYSAVE                                    
         BNE   FREV65                                                           
         B     FREV61                                                           
*                                                                               
FREV62   CLC   OREVNUM,REVXKNUM    COMPARE OTHER REV # TO THIS                  
         BNL   *+10                                                             
         MVC   OREVNUM,REVXKNUM    SAVE THIS REV#                               
*                                                                               
         MVC   SVLGKEY,KEY            SAVE LAST GOOD KEY                        
         MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
*                                                                               
         CLC   KEY(REVXKNUM-REVXKEY),KEYSAVE                                    
         BE    FREV61                                                           
*                                                                               
*                                                                               
* RE-READ LAST GOOD RECORD                                                      
*                                                                               
FREV65   OC    SVLGKEY,SVLGKEY     WERE THERE ANY GOOD RECORDS                  
         BZ    FREVX                NO, DONE                                    
         MVC   KEY,SVLGKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(L'REVXKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                 MUST BE THERE                               
*                                                                               
         CLC   OREVNUM,REVXKNUM    COMPARE OTHER REV # TO THIS                  
         BH    FREVX                                                            
         BE    *+6                                                              
         DC    H'0'                OTHER REV IS LOWER ? BUG??                   
*                                                                               
         L     R6,AIO2             WAS AIO3                                     
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING REVREVEL,R6                                                      
*                                                                               
         TM    REVFLAG,X'80'       HAVE INSTRUCTIONS BEEN RUN                   
         BZ    FREVX                                                            
*                                                                               
         LLC   RF,OREVNUM          YES, UP REVISION NUMBER                      
         LA    RF,1(,RF)                                                        
         STC   RF,OREVNUM                                                       
*                                                                               
FREVX    CLC   OREVNUM,PRGREVN     COMPARE OTHER  REVISION #                    
         BNH   *+10                TO THIS REVISION                             
         MVC   PRGREVN,OREVNUM     USE THE HIGHER NUMBER OF THE TWO             
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'UNTDIR',NBKEY,KEY                     
*                                                                               
         CLC   KEY(20),NBKEY                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,SETSPOT          SET TO SPOT                                  
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO              RESET BACK TO AIO1                           
         XIT1                                                                   
*                                                                               
         DROP  KS                                                               
         DROP  R3,R6                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* GET BASE PROGRAM CODE FOR EQUIVALENT PROGRAM CODE *                           
*                                                                               
         DS   0H                                                                
NXTEQV   NTR1  BASE=*,LABEL=*                                                   
         L     R3,ANETBLK                                                       
         USING NETBLCKD,R3                                                      
*                                                                               
         MVC   BASEPRG,NBACTPRG                                                 
         XC    BASEDTS,BASEDTS                                                  
*                                                                               
         LA    R2,EQVPTBL                                                       
         LA    R4,L'EQVPTBL/(EQVNEXT-EQVENT)                                    
         SR    R5,R5                                                            
*                                                                               
         USING EQVPTBLD,R2                                                      
NXTEQV10 OC    EQVEPRG,EQVEPRG     END OF ENTRIES                               
         BZ    NXTEQVX              YES                                         
         CLI   EQVUSED,C'Y'                                                     
         BNE   NXTEQV20                                                         
*                                                                               
         LA    R2,EQVNEXT                                                       
         BCT   R4,NXTEQV10                                                      
         B     NXTEQVX                                                          
*                                                                               
NXTEQV20 MVC   BASEPRG,EQVEPRG     SET FOR USE                                  
*                                                                               
         MVC   BASEDTS,EQVSDT                                                   
*                                                                               
         CLC   BASESTR,STDATEP     BEFORE START DATE                            
         BNL   *+10                 NO                                          
         MVC   BASESTR,STDATEP     DON'T RUN BEFORE START DATE                  
*                                                                               
         CLC   BASEEND,ENDATEP     PAST END DATE                                
         BNH   *+10                 NO                                          
         MVC   BASEEND,ENDATEP     DON'T RUN PAST END DATE                      
*                                                                               
         OC    OPTDAT2,OPTDAT2                                                  
         BZ    NXTEQV26                                                         
         CLC   BASEEND,OPTDAT2                                                  
         BNH   NXTEQV26                                                         
         MVC   BASEEND,OPTDAT2                                                  
*                                                                               
         OC    OPTDATE,OPTDATE                                                  
         BZ    NXTEQV26                                                         
         CLC   BASESTR,OPTDATE                                                  
         BNL   *+10                                                             
         MVC   BASESTR,OPTDATE                                                  
*                                                                               
         OC    OPTDAT2,OPTDAT2                                                  
         BZ    NXTEQV26                                                         
         CLC   BASEEND,OPTDAT2                                                  
         BNH   NXTEQV26                                                         
         MVC   BASEEND,OPTDAT2                                                  
*                                                                               
NXTEQV26 MVC   BASECT,EQVCT                                                     
*                                                                               
         MVI   EQVUSED,C'Y'                                                     
*                                                                               
         BRAS  RE,NETI                  INITIALIZE NETIO                        
*                                                                               
NXTEQV30 DS    0H                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY CLIENT                  
         BZ    NXTEQV32                                                         
*                                                                               
         CLI   T216FFD+6,C'$'      USER SIGN ON OFFICE LIST                     
         BNE   *+12                                                             
         OI    NBINDS5,NBI5PLST    SET OFFICE LIST FLAG                         
         B     *+12                                                             
         CLI   T216FFD+6,C'*'      USER SIGN ON BY OFFICE                       
         BNE   NXTEQV32             NO                                          
*                                                                               
         MVC   NBTRAPSC,T216FFD+7  BRAND LEVEL OFFICE(LIST)                     
*                                                                               
NXTEQV32 GOTO1 ANETIO,DMCB,(R3)                                                 
         MVI   NBUSER+13,C'N'      DO NOT SUPPRESS PRE-EMPTS                    
         CLI   NBERROR,NBGOOD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   NBMODE,NBREQLST                                                  
         BE    NXTEQV34                                                         
         CLI   NBMODE,NBPROCUN                                                  
         BNE   NXTEQV30                                                         
*                                                                               
NXTEQV34 BCTR  R5,0                                                             
*                                                                               
NXTEQVX  LTR   R5,R5                                                            
         XIT1                                                                   
*                                                                               
         DROP  R2,R3                                                            
         LTORG                                                                  
         EJECT                                                                  
* BUILD COMMERCIAL TABLE *                                                      
*                                                                               
         DS   0H                                                                
BCML     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,AIO2                                                          
         LA    R4,3000(R4)         CML TABLE                                    
         USING CMLTBLD,R4                                                       
*                                                                               
         L     RE,AIO3                                                          
BCML10   OC    0(CMLENT,R4),0(R4) EMPTY ENTRY                                   
         BZ    BCML20                                                           
         CLC   CMLCML,SVCMLCOD     SAME CML                                     
         BE    BCML60              YES, JUST INCREMENT CML COUNTER              
         LA    R4,CMLENT(R4)                                                    
         B     BCML10                                                           
*                                                                               
BCML20   LR    R1,R4                                                            
         LA    R1,CMLENT(R1)                                                    
         CR    RE,R1               END OF AIO2                                  
         BH    BCML50               NO                                          
*                                                                               
* AIO2 IS FULL, LOOK FOR LEAST USED CML AND REPLACE IT WITH THIS ONE            
*                                                                               
         L     R4,AIO2                                                          
         LA    R4,3000(R4)         CML TABLE                                    
         LR    R1,R4                                                            
COMPCNT  USING CMLTBLD,R1                                                       
*                                                                               
         L     RE,AIO3                                                          
BCML30   CR    R1,RE               END OF AIO2                                  
         BNL   BCML40               YES                                         
         LA    R1,CMLENT(R1)                                                    
         CLC   CMLCT,COMPCNT.CMLCT     COMPARE 2 CML COUNTS                     
         BL    BCML30                                                           
         LR    R4,R1               R4 POINTS TO THE CML TO BE REPLACED          
         B     BCML30                                                           
*                                                                               
         DROP  COMPCNT                                                          
*                                                                               
BCML40   MVI   CMLCT,0             RE-INIT COUNT                                
BCML50   MVC   CMLCML,SVCMLCOD     SAVE CML IN TABLE                            
*                                                                               
BCML60   LA    R1,CMLCT                                                         
         CLI   0(R1),255                                                        
         BE    BCML70                                                           
*                                                                               
         LLC   R1,CMLCT            INCREMENT CML USAGE COUNT                    
         LA    R1,1(R1)                                                         
         STC   R1,CMLCT                                                         
*                                                                               
BCML70   DS    0H                                                               
         BRAS  RE,FCML              VALIDATE CML                                
         OC    SVCMLCOD,SVCMLCOD                                                
         BZ    BCMLX                                                            
         MVI   CMLVLDSW,1          TURN ON CML VALID SWITCH                     
BCMLX    XIT1                                                                   
*                                                                               
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
* FILTER ON PRODUCT GROUP *                                                     
*                                                                               
         DS    0H                                                               
FPG      NTR1  BASE=*,LABEL=*                                                   
         L     R3,ANETBLK                                                       
         USING NETBLCKD,R3                                                      
*                                                                               
         LA    R0,L'OPTPGRL/3                                                   
         LHI   R1,OPTPGRL-SYSD                                                  
         AR    R1,R9                                                            
FPG10    CLC   NBPR1CL3,0(R1)                                                   
         BE    FPGEQ                                                            
*                                                                               
         LA    RE,L'SVCSPROD                                                    
         LA    RF,SVCSPROD                                                      
FPG20    CLI   0(RF),0                                                          
         BE    FPG30                                                            
         CLC   0(3,R1),0(RF)                                                    
         BE    FPGEQ                                                            
         LA    RF,3(,RF)                                                        
         BCT   RE,FPG20                                                         
*                                                                               
FPG30    LA    R1,3(,R1)                                                        
         CLI   0(R1),0                                                          
         BNH   *+8                                                              
         BCT   R0,FPG10                                                         
         CR    RD,RB                                                            
         XIT1                                                                   
FPGEQ    CR    RB,RB                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* VALIDATE MY NETWORK (FOR NET=ALL REQUEST)                                     
*                                                                               
         DS    0H                                                               
VMYNET   NTR1  BASE=*,LABEL=*                                                   
         L     R3,ANETBLK                                                       
         USING NETBLCKD,R3                                                      
*                                                                               
         BRAS  RE,SETNET           SET TO NET                                   
*                                                                               
         XC    KEY,KEY                                                          
         XC    KEYSAVE,KEYSAVE                                                  
*                                                                               
         LA    R4,KEY                                                           
         USING NURECD,R4                                                        
*                                                                               
         MVI   NUKPTYPE,X'84'      UNIT PASSIVE KEY                             
         MVC   NUKPAM,BAGYMD       AGENCY                                       
         MVC   NUKPCLT,BCLT        CLIENT                                       
         MVC   NUKPNET,SVNET       CURRENT NETWORK                              
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
VMYNET10 CLC   KEY(8),KEYSAVE      SAME AGY/CLT/NET                             
         BNE   VMYNETX              NO, DONE                                    
*                                                                               
         CLC   ENDATEP,NUKPDATE    IF AIR DATE                                  
         BL    VMYNET20                                                         
         CLC   STDATEP,NUKPDATE     IS WITHIN PERIOD DATES                      
         BNH   VMYNETX                THEN PROCESS THIS NETWORK                 
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         B     VMYNET10                                                         
*                                                                               
VMYNET20 MVC   NUKPDATE,=X'FFFF'   ELSE, FORCE NEXT PROG                        
         XC    NUKPEST(4),NUKPEST        CLEAR THE REST OF KEY                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     VMYNET10                                                         
*                                                                               
VMYNETX  BRAS  RE,SETSPOT          SET TO SPOT                                  
*                                                                               
         CLC   SVNET,NUKPNET       SET CC CODE                                  
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE NETWORK                                                              
*                                                                               
         DS    0H                                                               
VNET     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   5(R2),3                                                          
         BNE   VNET01                                                           
         CLC   =C'ALL',8(R2)                                                    
         BNE   VNET01                                                           
*                                                                               
         MVC   WORK(L'SVNET),SVNET                                              
         B     VNET02                                                           
*                                                                               
VNET01   GOTO1 ANY                                                              
VNET02   MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY       PRE-FILL THE KEY WITH ZEROES                 
         LA    R4,KEY                                                           
         USING STARECD,R4                                                       
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'N'                                                     
         MVC   STAKCALL(4),WORK                                                 
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,AGENCY                                                   
         MVC   STAKCLT,BCLT                                                     
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO                      
         CLI   8(R1),0                                                          
         BNE   NETERR                                                           
         MVC   NETWORK,WORK                                                     
         L     R4,AIO                                                           
         PACK  DUB,SMKT                                                         
         CVB   R0,DUB                                                           
         STH   R0,NETMKT                                                        
         MVC   SVMEDIA,STRTYPE     (N,C,S,O,D,H)                                
*                                                                               
         TM    SVOPTSW,OPTMED      SEED BY MEDIA?                               
         BZ    VNET30               NO                                          
         CLI   SVOPTMED,0          WAS SPECIFIC MEDIA ENTERED?                  
         BE    VNET30               NO                                          
         CLC   SVMEDIA,SVOPTMED    SAME MEDIA?                                  
         BE    VNET30                                                           
         CR    RB,RC                                                            
         B     VNETX                                                            
*                                                                               
VNET30   OI    4(R2),X'20'         SET ON VALIDATED                             
         CR    RB,RB                                                            
VNETX    XIT1                                                                   
*                                                                               
NETERR   MVC   GERROR,=Y(NONET)                                                 
         GOTO1 VTRAERR                                                          
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE PROGRAM *                                                            
*                                                                               
         DS    0H                                                               
VPROG    NTR1  BASE=*,LABEL=*                                                   
         L     R3,ANETBLK                                                       
         USING NETBLCKD,R3                                                      
*                                                                               
         GOTO1 ANY                                                              
         CLC   =CL6'ALL   ',WORK                                                
         BE    VPROG06                                                          
         CLI   1(RA),C'*'          IS THIS A DDS TERMINAL                       
         BE    *+12                                                             
         TM    SVOPTSW,OPTTEST     TEST IS OK TOO                               
         BZ    ALLPRGER                                                         
*                                                                               
         MVC   PROGRAM,WORK                                                     
*                                                                               
VPROG06  BAS   RE,GTEQV            GO GET ANY EQUIVALENT PROGRAMS               
*                                                                               
         MVC   BASEPRG,PROGRAM                                                  
         MVC   BASEDTS,STDATEP                                                  
         OC    OPTDAT2,OPTDAT2                                                  
         BZ    *+10                                                             
         MVC   BASEEND,OPTDAT2                                                  
*                                                                               
         OC    OPTDATE,OPTDATE                                                  
         BZ    *+10                                                             
         MVC   BASESTR,OPTDATE                                                  
*                                                                               
VPROG10  BRAS  RE,NETI              INITIALIZE NETIO                            
*                                                                               
VPROG20  DS    0H                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY CLIENT                  
         BZ    VPROG30                                                          
*                                                                               
         CLI   T216FFD+6,C'$'      USER SIGN ON OFFICE LIST                     
         BNE   *+12                                                             
         OI    NBINDS5,NBI5PLST    SET OFFICE LIST FLAG                         
         B     *+12                                                             
         CLI   T216FFD+6,C'*'      USER SIGN ON BY OFFICE                       
         BNE   VPROG30              NO                                          
*                                                                               
         MVC   NBTRAPSC,T216FFD+7  BRAND LEVEL OFFICE(LIST)                     
*                                                                               
VPROG30  GOTO1 ANETIO,DMCB,(R3)                                                 
         CLI   NBERROR,NBGOOD                                                   
         BE    VPROG40                                                          
         DC    H'0'                                                             
*                                                                               
VPROG40  CLI   NBMODE,NBREQLST                                                  
         BE    VPROG50             NO UNITS                                     
         CLI   NBMODE,NBPROCUN                                                  
         BNE   VPROG20                                                          
*                                                                               
         CLI   SVTNPR9,C'B'        BOTH MEDIA AND SKED                          
         BE    VPROG44                                                          
         CLI   SVTNPR9,C'S'        IF NOT SKED, MUST BE MEDIA                   
         BNE   VPROG42                                                          
         CLI   NBACTSUB,C'A'       SKED ONLY                                    
         BL    VPROG20                                                          
         B     VPROG44                                                          
VPROG42  CLI   NBACTSUB,C'A'                                                    
         BNL   VPROG20                                                          
VPROG44  OC    PROGRAM,PROGRAM     SPECIFIC PROGRAM REUEST                      
         BZ    VPROG46                                                          
         CLC   NBACTPRG,NBSELPRG                                                
         BE    VPROG46                                                          
         MVC   GERROR,=Y(NOPROG)                                                
         B     VPROGERX                                                         
*                                                                               
* SET OFF ALL EQVUSED                                                           
*                                                                               
VPROG46  LA    RE,EQVPTBL                                                       
         LA    RF,L'EQVPTBL/(EQVNEXT-EQVENT)                                    
         USING EQVPTBLD,RE                                                      
VPROG48  OC    EQVENT,EQVENT       EMPTY ENTRY                                  
         BZ    VPROGX              NO UNITS                                     
         MVI   EQVUSED,0           SET OFF ENTRY USED ALREADY                   
         LA    RE,EQVNEXT                                                       
         BCT   RF,VPROG48                                                       
VPROGX   XIT1                                                                   
         EJECT                                                                  
* HAVE NOT FOUND GOOD UNIT ON REQUESTED CODE, SEE IF EQUIVALENTS *              
*                                                                               
VPROG50  DS    0H                  NOW LOOK FOR EQUIVALENT PROGRAMS             
         LA    RE,EQVPTBL                                                       
         LA    RF,L'EQVPTBL/(EQVNEXT-EQVENT)                                    
         USING EQVPTBLD,RE                                                      
VPROG54  OC    EQVENT,EQVENT       EMPTY ENTRY                                  
         BNZ   VPROG55             NO UNITS                                     
*                                                                               
         LA    R2,TRANETH          IF NET=ALL THEN BYPASS                       
         CLI   5(R2),3                                                          
         BNE   *+22                                                             
         CLC   =C'ALL',8(R2)                                                    
         BNE   *+12                                                             
         MVI   UPDSW,X'FF'         NO UNIT FOUND, BYPASS THIS NET               
         B     VPROGX                                                           
*                                                                               
         MVC   GERROR,=Y(NOUNTPER)                                              
         B     VPROGERX                                                         
VPROG55  CLI   EQVUSED,C'Y'        ENTRY USED ALREADY                           
         BNE   VPROG60                                                          
         LA    RE,EQVNEXT                                                       
         BCT   RF,VPROG54                                                       
*                                                                               
         LA    R2,TRANETH          IF NET=ALL THEN BYPASS                       
         CLI   5(R2),3                                                          
         BNE   *+22                                                             
         CLC   =C'ALL',8(R2)                                                    
         BNE   *+12                                                             
         MVI   UPDSW,X'FF'         NO UNIT FOUND, BYPASS THIS NET               
         B     VPROGX                                                           
*                                                                               
         MVC   GERROR,=Y(NOUNTPER)                                              
         B     VPROGERX                                                         
*                                                                               
VPROG60  MVI   EQVUSED,C'Y'                                                     
         MVC   BASEDTS,EQVSDT                                                   
*                                                                               
         CLC   BASESTR,STDATEP     BEFORE START DATE                            
         BNL   *+10                 NO                                          
         MVC   BASESTR,STDATEP     DON'T RUN BEFORE START DATE                  
*                                                                               
         CLC   BASEEND,ENDATEP     PAST END DATE                                
         BNH   *+10                 NO                                          
         MVC   BASEEND,ENDATEP     DON'T RUN PAST END DATE                      
*                                                                               
         OC    OPTDATE,OPTDATE                                                  
         BZ    VPROG64                                                          
         CLC   BASESTR,OPTDATE                                                  
         BNL   *+10                                                             
         MVC   BASESTR,OPTDATE                                                  
*                                                                               
         OC    OPTDAT2,OPTDAT2                                                  
         BZ    VPROG64                                                          
         CLC   BASEEND,OPTDAT2                                                  
         BNH   VPROG64                                                          
         MVC   BASEEND,OPTDAT2                                                  
*                                                                               
VPROG64  MVC   BASEPRG,EQVEPRG                                                  
*                                                                               
         B     VPROG10                                                          
         DROP  RE                                                               
VPROGERX GOTO1 VTRAERR                                                          
*                                                                               
ALLPRGER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'ALLPRGMS),ALLPRGMS                                     
         GOTO1 ERREX2                                                           
         EJECT                                                                  
* GET EQUIVALENT PROGRAM RECORDS FOR CLIENT *                                   
*                                                                               
GTEQV    NTR1                                                                   
         BRAS  RE,SETNET           SET TO NET                                   
*                                                                               
         LA    RE,EQVPTBL                                                       
         LA    RF,L'EQVPTBL                                                     
         XCEFL                                                                  
*                                                                               
         LA    R2,EQVPTBL                                                       
         LA    R3,L'EQVPTBL/(EQVNEXT-EQVENT)                                    
         LA    R5,1                INITIALIZE COUNTER                           
         USING EQVPTBLD,R2                                                      
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PGEKEY,R4                                                        
         MVI   PGEKID,X'24'                                                     
         MVC   PGEKAM(3),BAGYMD    & BCLT                                       
         MVC   PGEKNET,NETWORK                                                  
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
GTEQV10  CLC   KEY(8),KEYSAVE                                                   
         BNE   GTEQVX                                                           
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    GTEQV24                                                          
         DC    H'0'                                                             
         USING PGEDTAEL,R6                                                      
*                                                                               
GTEQV20  BRAS  RE,NEXTEL                                                        
         BNE   GTEQV60                                                          
*                                                                               
         FIXDT02                                                                
GTEQV24  GOTO1 DATCON,DMCB,(3,PGESTR),(2,WORK)                                  
         FIXDT02                                                                
         GOTO1 (RF),(R1),(3,PGEEND),(2,WORK+2)                                  
         CLC   WORK(2),ENDATEP     SEE IF WITHIN DATES                          
         BH    GTEQV20                                                          
         CLC   WORK+2(2),STDATEP                                                
         BL    GTEQV20                                                          
*                                                                               
         OC    PROGRAM,PROGRAM                                                  
         BZ    GTEQV26                                                          
         CLI   1(RA),C'*'          THIS A DDS TERMINAL                          
         BNE   *+12                                                             
         TM    SVOPTSW,OPTTEST     AND IN TEST MODE                             
         BO    GTEQV26                                                          
*                                                                               
         CLC   PROGRAM,PGEKPRG     THIS AN EQV PROG                             
         BNE   GTEQV26                                                          
         MVC   GERROR,=Y(NOEQUIV)                                               
         LA    R2,TRAPROGH         PROGRAM                                      
         B     VPROGERX                                                         
*                                                                               
GTEQV26  OC    PROGRAM,PROGRAM                                                  
         BZ    GTEQV28                                                          
         CLC   PROGRAM,PGEPROG     SAME BASE PROG                               
         BNE   GTEQV20                                                          
*                                                                               
GTEQV28  CLC   WORK(2),STDATEP                                                  
         BNL   *+10                                                             
         MVC   WORK(2),STDATEP     FORCE DATES TO BE WITHIN PERIOD              
*                                                                               
         CLC   WORK+2(2),ENDATEP                                                
         BNH   *+10                                                             
         MVC   WORK+2(2),ENDATEP                                                
*                                                                               
         MVC   EQVSDT(4),WORK                                                   
*                                                                               
         MVC   EQVEPRG,PGEKPRG                                                  
         MVC   EQVBPRG,PGEPROG                                                  
         STC   R5,EQVCT                                                         
         LA    R5,1(,R5)           ADD 1 TO ENTRY COUNT                         
*                                                                               
         GOTO1 (RF),(R1),(3,PGESTR),(0,WORK)                                    
         CLI   MYTN2PR6,C'B'       TRAFFIC OVERRIDE BROADCAST MONTH             
         BE    GTEQV34                                                          
         CLI   MYTN2PR6,C'C'       TRAFFIC OVERRIDE CALENDAR MONTH              
         BE    GTEQV30                                                          
         CLI   NBUSER+3,C'B'       BROADCAST MONTH                              
         BE    GTEQV34                                                          
*                                                                               
GTEQV30  MVC   WORK+4(2),=C'01'                                                 
         B     GTEQV36                                                          
*                                                                               
GTEQV34  GOTO1 =V(GETBROAD),(R1),(1,WORK),WORK+6,GETDAY,ADDAY,         C        
               RR=SPTR62RR                                                      
         MVC   WORK(6),WORK+6                                                   
         FIXDT02                                                                
GTEQV36  GOTO1 DATCON,(R1),(0,WORK),(2,EQVSMODT)                                
*                                                                               
GTEQV40  MVC   EQVEDT,=F'-1'       ONLY USES 1ST 2                              
         MVC   EQVEMODT,=F'-1'     SAME                                         
         CLC   PGEEND,=F'-1'                                                    
         BE    GTEQV50                                                          
         FIXDT02                                                                
         GOTO1 (RF),(R1),(3,PGEEND),(2,EQVEDT)                                  
         GOTO1 (RF),(R1),,(0,WORK)                                              
         CLI   MYTN2PR6,C'B'       TRAFFIC OVERRIDE BROADCAST MONTH             
         BE    GTEQV46                                                          
         CLI   MYTN2PR6,C'C'       TRAFFIC OVERRIDE CALENDAR MONTH              
         BE    GTEQV42                                                          
         CLI   NBUSER+3,C'B'       BROADCAST MONTH                              
         BE    GTEQV46                                                          
*                                                                               
GTEQV42  MVC   WORK+6(4),WORK                                                   
         GOTO1 ADDAY,(R1),WORK,WORK,F'31'                                       
GTEQV44  GOTO1 (RF),(R1),,,F'-1'                                                
         CLC   WORK(4),WORK+6                                                   
         BNE   GTEQV44                                                          
         B     GTEQV48                                                          
*                                                                               
GTEQV46  GOTO1 =V(GETBROAD),(R1),(1,WORK),WORK+6,GETDAY,ADDAY,         C        
               RR=SPTR62RR                                                      
         MVC   WORK(6),WORK+12                                                  
         FIXDT02                                                                
GTEQV48  GOTO1 DATCON,(R1),(0,WORK),(2,EQVEMODT)                                
*                                                                               
GTEQV50  LA    R2,EQVNEXT                                                       
         BCT   R3,GTEQV20                                                       
         DC    H'0'                                                             
*                                                                               
GTEQV60  MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         B     GTEQV10                                                          
*                                                                               
GTEQVX   DS    0H                                                               
         CLI   KEYSAVE+2,0         DID WE LOOK ACROSS ALL CLIENT                
         BE    GTEQVX10             YES WE ARE DONE                             
         XC    KEY,KEY                                                          
         MVC   KEY(2),KEYSAVE     ID/A/M                                        
         MVC   KEY+4(4),NETWORK     NETWORK                                     
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         B     GTEQV10                                                          
*                                                                               
GTEQVX10 DS   0H                                                                
         BRAS  RE,SETSPOT          SET TO SPOT                                  
         B     VPROGX                                                           
*                                                                               
ALLPRGMS DC    C'* ERROR * MUST BE ALL UNLESS OPTION TEST *'                    
         DROP  R2,R3,R4,R6                                                      
         EJECT                                                                  
*                                                                               
* PRINT ANY ERROR REPORT FOR PROGRAMS WITH ERRORS *                             
*                                                                               
         DS    0H                                                               
GOPER    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   XGOPER                                                           
         BRAS  RE,PER               PRINT ERROR REPORT                          
XGOPER   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***************************************************************                 
* SUBROUTINE VALIDATES PERIOD AND CALCULATES START/END DATES  *                 
***************************************************************                 
*                                                                               
VPER     NTR1  BASE=*,LABEL=*                                                   
         L     R3,ANETBLK                                                       
         USING NETBLCKD,R3                                                      
*                                                                               
         LA    R2,TRAPERH          PERIOD                                       
         MVI   TABLESW,0           RESET TABLE BUILT                            
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERRA            NO, ERROR                                    
*                                                                               
         XC    KEY,KEY             GET USER PROFILE INTO NBUSER                 
         MVC   KEY(4),=C'S0N0'                                                  
         MVC   KEY+4(2),AGENCY                                                  
         MVI   KEY+6,C'N'                                                       
         MVC   KEY+7(3),QCLT                                                    
         MVI   KEY+10,C'*'                                                      
         MVC   KEY+11(1),SVCLTOFF  CLIENT OFFICE                                
*                                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    VPER01                                                           
*                                                                               
         TM    SECFLAG,NEPRDOFF    PRDS OFFICES NOT EQ                          
         BO    VPER01                                                           
*                                                                               
         MVC   KEY+11(1),SVPRDOFF  USE PROD OFFICE                              
*                                                                               
VPER01   GOTO1 GETPROF,DMCB,KEY,NBUSER,DATAMGR GET PROFILE IN NBUSER            
*                                                                               
VPER02   LA    R4,TRAPER                                                        
         CLI   0(R4),C'?'          QUESTION MARK HELP                           
         BNE   VPER04                                                           
         LA    R4,1(,R4)                                                        
*                                                                               
VPER04   GOTO1 DATVAL,DMCB,(0,0(R4)),STDATE                                     
         OC    DMCB(4),DMCB                                                     
         BZ    VPER06                                                           
         CLI   MYTN2PR6,C'W'       INSTR PERIOD = WEEKLY                        
         BNE   PERDATER                                                         
*                                                                               
         GOTO1 GETDAY,(R1),(0,STDATE),WORK                                      
         CLI   0(R1),1             MUST BE MONDAY                               
         BNE   DAYDATER                                                         
         GOTO1 ADDAY,(R1),STDATE,ENDATE,F'6'                                    
         GOTO1 DATCON,(R1),(0,STDATE),(3,PERIOD)                                
                                                                                
         TM    SVFLAG,CONVSW       CONVERTED RECORDS?                           
         BZ    VPER30               NO                                          
         MVC   WORK(6),STDATE                                                   
         BRAS  RE,CONVPER                                                       
         MVC   PERIOD,WORK+6                                                    
         B     VPER30                                                           
*                                                                               
VPER06   GOTO1 DATVAL,DMCB,(2,(R4)),STDATE                                      
         OC    DMCB(4),DMCB                                                     
         BZ    PERDATER                                                         
         CLI   MYTN2PR6,C'W'       INSTR PERIOD = WEEKLY                        
         BE    WKDATER                                                          
*                                                                               
         GOTO1 DATCON,(R1),(0,STDATE),(3,PERIOD)                                
         CLI   MYTN2PR6,C'B'       TRAFFIC BROADCAST MONTH                      
         BE    VPER20                                                           
         CLI   MYTN2PR6,C'C'       TRAFFIC CALENDAR MONTH                       
         BE    VPER10                                                           
         CLI   NBUSER+3,C'B'       BROADCAST MONTH                              
         BE    VPER20                                                           
         MVI   MYTN2PR6,C'C'       CALENDAR MONTH                               
*                                                                               
* GET START AND END OF CALENDAR MONTH *                                         
*                                                                               
VPER10   MVC   STDATE+4(2),=C'01'                                               
         GOTO1 ADDAY,(R1),STDATE,ENDATE,F'31'                                   
*                                                                               
VPER14   GOTO1 (RF),(R1),ENDATE,ENDATE,F'-1'                                    
         CLC   STDATE(4),ENDATE                                                 
         BNE   VPER14                                                           
         B     VPER30                                                           
*                                                                               
* GET BROADCAST MONTH *                                                         
*                                                                               
VPER20   DS    0H                                                               
         MVI   MYTN2PR6,C'B'       BROADCAST MONTH                              
         MVC   STDATE+4(2),=C'15'                                               
         MVC   WORK(6),STDATE                                                   
         GOTO1 =V(GETBROAD),DMCB,(1,WORK),STDATE,GETDAY,ADDAY,         C        
               RR=SPTR62RR                                                      
*                                                                               
         FIXDT02                                                                
VPER30   GOTO1 DATCON,(R1),(0,STDATE),(2,STDATEP)                               
         FIXDT02                                                                
         GOTO1 (RF),(R1),(0,ENDATE),(2,ENDATEP)                                 
         GOTO1 (RF),(R1),(0,STDATE),(3,STDATEB)                                 
         GOTO1 (RF),(R1),(0,ENDATE),(3,ENDATEB)                                 
*NOP     CLI   WORK+50,C' '        WAS OPTION DATE= USED                        
*        BNH   VPER30C              NO                                          
*        CLC   STDATE,WORK+50      MUST BE W/N PERIOD                           
*        BH    DTPERERR                                                         
*        CLC   ENDATE,WORK+53                                                   
******   BL    DTPERERR                                                         
*                                                                               
VPER30C  OC    OPTDATE,OPTDATE     WAS PARTIAL PERIOD DATE ENTERED              
         BZ    VPER32                                                           
*                                                                               
         CLC   STDATEP,OPTDATE     MUST BE WITHIN PERIOD DATES                  
         BH    OPTPERER                                                         
         CLC   ENDATEP,OPTDATE     MUST BE WITHIN PERIOD DATES                  
         BL    OPTPERER                                                         
         OC    OPTDAT2,OPTDAT2     WAS PARTIAL PERIOD DATE ENTERED              
         BNZ   VPER32                                                           
         MVC   OPTDAT2,ENDATEP                                                  
         B     VPER34                                                           
*                                                                               
VPER32   OC    OPTDAT2,OPTDAT2     WAS PARTIAL PERIOD DATE ENTERED              
         BZ    VPER34                                                           
*                                                                               
         CLC   STDATEP,OPTDAT2     MUST BE WITHIN PERIOD DATES                  
         BH    OPTPERER                                                         
         CLC   ENDATEP,OPTDAT2     MUST BE WITHIN PERIOD DATES                  
         BL    OPTPERER                                                         
*                                                                               
VPER34   CLI   TRAPER,C'?'         QUESTION MARK HELP                           
         BNE   VPERX                                                            
*                                                                               
         MVC   GERROR,=Y(PERHLP)                                                
         MVI   GMSGTYPE,C'I'                                                    
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         STCM  R4,7,GASUBST        A(SUBST TEXT)                                
         MVI   0(R4),15            L'SUBST TEXT + 1                             
         MVC   1(14,R4),=C'CALENDAR MONTH'                                      
*                                                                               
         CLI   MYTN2PR6,C'W'       TRAFFIC WEEKLY                               
         BE    VPER44                                                           
         CLI   MYTN2PR6,C'B'       TRAFFIC BROADCAST MONTH                      
         BE    VPER42                                                           
         CLI   MYTN2PR6,C'C'       TRAFFIC CALENDAR MONTH                       
         BE    VPER46                                                           
         CLI   NBUSER+3,C'B'       BROADCAST MONTH                              
         BNE   VPER46                                                           
*                                                                               
VPER42   MVI   0(R4),16            L'SUSBST TEXT + 1                            
         MVC   1(15,R4),=C'BROADCAST MONTH'                                     
         B     VPER46                                                           
VPER44   MVI   0(R4),14            L'SUBST TEXT + 1                             
         MVC   1(13,R4),=C'CALENDAR WEEK'                                       
*                                                                               
VPER46   LLC   R0,0(R4)                                                         
         AR    R4,R0                                                            
         MVI   0(R4),9             L'SUBST TEXT + 1                             
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(2,STDATEP),(5,1(R4))                                
         LA    R4,9(R4)                                                         
         MVI   0(R4),9             L'SUBST TEXT + 1                             
         FIXDT02                                                                
         GOTO1 (RF),(R1),(2,ENDATEP),(5,1(R4))                                  
         B     VPERERX                                                          
VPERX    XIT1                                                                   
         EJECT                                                                  
MISSERRA MVI   ERROR,MISSING                                                    
         GOTO1 ERREX                                                            
PERDATER MVC   GERROR,=Y(ONLYMMYY)                                              
         CLI   MYTN2PR6,C'W'       INSTR PERIOD = WEEKLY                        
         BE    WKDATER                                                          
         B     VPERERX                                                          
DTPERERR MVC   GERROR,=Y(DTNINPER)                                              
         B     VPERERX                                                          
DAYDATER MVC   GERROR,=Y(MONSTDT)                                               
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         STCM  R4,7,GASUBST        A(SUBST TEXT)                                
         MVI   0(R4),4             L'SUBST TEXT + 1                             
         MVC   1(3,R4),WORK                                                     
         B     VPERERX                                                          
OPTPERER MVC   GERROR,=Y(BDPARPER)                                              
         NI    TRAPERH+4,X'FF'-X'20'   SET OFF VALIDATED                        
         LA    R2,TRAFLTRH                                                      
         B     VPERERX                                                          
WKDATER  MVC   GERROR,=Y(WKDTER)                                                
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         STCM  R4,7,GASUBST        A(SUBST TEXT)                                
         MVI   0(R4),7             L'SUBST TEXT + 1                             
         MVI   1(R4),C'('                                                       
         MVI   6(R4),C')'                                                       
         MVC   2(4,R4),TRANET                                                   
         OC    SVNET,SVNET         ALL NETWORK REQUEST ?                        
         BZ    VPERERX              NO                                          
         MVC   2(4,R4),SVNET                                                    
*                                                                               
VPERERX  GOTO1 VTRAERR                                                          
         DROP  R3                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
****************************************************************                
* ON ENTRY WORK CONTAINS PERIOD (YYMMDD)                                        
* CONVERT PERIOD INTO WORK+6:                                                   
* 1ST BYTE = YEAR                                                               
* 2ND BYTE = HOB MONTH AND LOB WEEK NUMBER FOR THAT MONTH                       
* EG. 1/27/20 CONVERT X'7814' YEAR= X'78' MONTH=1 WEEK=4                        
****************************************************************                
CONVPER  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 DATCON,(R1),(0,WORK),(3,WORK+6) YYMMDD TO YMD                    
*                                                                               
* CALC WEEK NUMBER FOR THIS MONTH                                               
         SR    R5,R5               INIT WEEK NUMBER                             
         MVC   DUB(6),WORK         YYMMDD                                       
CONV02   AHI   R5,1                INCR WEEK NUMBER                             
         GOTO1 ADDAY,(R1),DUB,DUB,F'-7'                                         
         CLC   WORK+2(2),DUB+2     SAME MONTH?                                  
         BE    CONV02                                                           
*                                                                               
         SLL   R5,28               WEEK NUMBER IN HOB                           
         LLC   R4,WORK+7                                                        
         SLDL  R4,28               HOB MONTH/LOB WEEK NUMBER                    
         STCM  R4,8,WORK+7         MONTH/WEEK NO (X'C4'= DEC/WEEK 4)            
         XIT1                                                                   
*                                                                               
*                                                                               
         EJECT                                                                  
* VALIDATE OPTIONS                                                              
*                                                                               
* VALID OPTIONS = 'TEST'      OPTTEST  (X80)                                    
*                 'RERUN'     OPTRERUN (X40)                                    
*                                      (X20)                                    
*                                      (X10)                                    
*                                      (X08)                                    
*                                      (X04)                                    
*                                      (X02)                                    
*                 'PATTERN'   OPTPTTN  (X01) PRINT SOURCE PATTERN               
*                                                                               
         DS    0H                                                               
VOPT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VOPT92               NO                                          
         CLI   8(R2),C'?'          HELP                                         
         BE    VOPTHLP              YES                                         
         CLC   8(0,R2),=C'HELP'    HELP?                                        
         BE    VOPTHLP              YES                                         
*                                                                               
         LA    R4,BLOCK+240        ADDRESS OF FIRST BLOCK                       
         GOTO1 SCANNER,DMCB,(20,TRAFLTRH),(7,(R4))                              
         LLC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3               SEE IF SCANNER FOUND ANYTHING                
         BZ    MISSERRB            NO                                           
VOPT10   LLC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
*                                                                               
* GET ADDRESS OF OPTION VALIDATION RTN                                          
*                                                                               
         CLC   12(3,R4),=C'#  '                                                 
         BE    VOPT15                                                           
         CLC   12(3,R4),=C'TS '                                                 
         BE    VOPT15                                                           
         CH    R1,=H'1'                                                         
         BH    VOPT15                                                           
         B     VOPTERR                                                          
VOPT15   DS    0H                                                               
         LA    RF,OPTTABLE                                                      
         EX    R1,OPTCLC                                                        
         BE    OPTGO                                                            
         LA    RF,L'OPTTABLE(RF)                                                
         CLI   0(RF),X'FF'                                                      
         BNE   *-16                                                             
         CLC   =C'TBA',22(R4)                                                   
         BNE   VOPTHLP                                                          
         B     VOPTTBA                                                          
VOPTERR  DS    0H                                                               
         MVI   ERROR,128               (INVALID FILTER OPTION)                  
         B     VOPTERX                                                          
*                                                                               
OPTCLC   CLC   12(0,R4),0(RF)                                                   
OPTGO    L     RE,10(RF)                                                        
         A     RE,SPTR62RR                                                      
         BR    RE                                                               
         EJECT                                                                  
VOPTTEST OI    SVOPTSW,OPTTEST                                                  
         MVI   SVTN1PR4,C'N'       FORCE REPORT TO PRINT FOR TEST               
         B     VOPT90                                                           
*                                                                               
VOPTRACE OI    SVOPTSW,OPTRACE                                                  
         MVI   SVTN1PR4,C'N'       FORCE REPORT TO PRINT FOR TRACE              
         B     VOPT90                                                           
*                                                                               
VOPTPRD  CLI   SVTN2PRO+00,C'*'    TRAFFIC BY PRODUCT                           
         BNE   VOPTHLP              NO, PRD= IS INVALID                         
*                                                                               
         CLI   SVTN2PRO+00,C'A'    TRAFFIC BY PRODUCT GROUP                     
         BL    *+12                                                             
         CLI   SVTN2PRO+00,C'Z'    TRAFFIC BY PRODUCT GROUP                     
         BNH   VOPTPRER                                                         
*                                                                               
         CLC   =C'POL',22(R4)      IS PRD=POL                                   
         BE    INVPRER              YES, ERROR                                  
         CLC   =C'ALL',22(R4)      IS PRD=ALL                                   
         BE    INVPRER              YES, ERROR                                  
         CLI   1(R4),2             MIN 2 CHAR                                   
         BL    PRLENERR                                                         
         CLI   1(R4),3             MAX 3 CHAR                                   
         BH    PRLENERR                                                         
         BE    *+8                                                              
         MVI   24(R4),C' '                                                      
*                                                                               
         LA    R2,ELEM             FAKE VALIPRD                                 
         XC    ELEM,ELEM                                                        
         MVC   ELEM(8),=X'0A01000184030001'                                     
         MVC   ELEM+8(3),22(R4)                                                 
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
*                                                                               
         GOTO1 VALIPRD                                                          
         MVI   ERROPT,0                                                         
*                                                                               
         LA    R2,TRAFLTRH         CLIENT                                       
*                                                                               
         CLI   ERROR,0             ANY ERROR?                                   
         BNE   VOPTERX             VOPTERX                                      
*                                                                               
         MVC   OPTPROD,WORK        PROD                                         
         B     VOPT90                                                           
         EJECT                                                                  
VOPTPGRP CLI   SVTN2PRO+00,C'A'    TRAFFIC BY PRODUCT GROUP                     
         BL    VOPTHLP                                                          
         CLI   SVTN2PRO+00,C'Z'    TRAFFIC BY PRODUCT GROUP                     
         BH    VOPTHLP                                                          
*                                                                               
* VALIDATE PRODUCT GROUP *                                                      
*                                                                               
VOPT34   BAS   RE,VPGR                                                          
         B     VOPT90                                                           
*                                                                               
VOPTTBA  DS    0H                                                               
         CLC   =C'ALL',12(R4)      CLEAR ALL CMLS (ALL=TBA)                     
         BNE   *+14                                                             
         MVC   SVCMLTBA(3),=C'ALL'                                              
         B     VOPT90                                                           
*                                                                               
         CLI   0(R4),8             MUST BE 8 CHAR                               
         BNE   CMLENER                                                          
         GOTO1 ANY                                                              
         XC    KEY(13),KEY                                                      
         LA    R1,KEY                                                           
         USING CMLKEY,R1                                                        
         MVC   CMLKID,=XL2'0A21'                                                
         MVC   CMLKAM(3),BAGYMD  & BCLT                                         
         MVC   CMLKCML,12(R4)                                                   
         DROP  R1                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   VCMLERR                                                          
         MVC   SVCMLTBA,12(R4)                                                  
         B     VOPT90                                                           
*                                                                               
VOPTDATE DS    0H                                                               
         LA    R5,22(,R4)                                                       
         GOTO1 DATVAL,DMCB,(0,(R5)),WORK+50 SAVE THE DATE FOR LATER             
         L     R6,DMCB                                                          
         LTR   R6,R6               WAS DATE VALID                               
         BNZ   *+18                                                             
VOPTDERR MVC   GERROR,=Y(BDPARPER)                                              
         NI    TRAFLTRH+4,X'FF'-X'20'   SET OFF VALIDATED                       
         B     VOPTTRAP                                                         
*                                                                               
         FIXDT02                                                                
         GOTO1 DATCON,(R1),(0,WORK+50),(2,OPTDATE)                              
         CLM   R6,1,1(R4)          WAS THERE ONLY 1 DATE                        
         BE    VOPTD10                                                          
*        BE    VOPT90               YES                                         
*                                                                               
         LA    R5,1(R6,R5)                                                      
         GOTO1 DATVAL,(R1),(0,(R5)),WORK+53                                     
         L     R6,DMCB                                                          
         LTR   R6,R6               WAS DATE VALID                               
         BZ    VOPTDERR            BAD DATE                                     
*                                                                               
         FIXDT02                                                                
         GOTO1 DATCON,(R1),(0,WORK+53),(2,OPTDAT2)                              
         CLC   OPTDATE,OPTDAT2                                                  
         BNH   VOPTD10                                                          
*        BNH   VOPT90                                                           
         MVC   GERROR,=Y(DATSEQER)                                              
         B     VOPTTRAP                                                         
*                                                                               
VOPTD10  DS    0H                                                               
*                                                                               
         CLI   SVNET,0             NETWORK=ALL REQUEST                          
         BE    VOPT90               NO                                          
*                                                                               
         OC    STDATEP,STDATEP     MAKE SURE THAT PERIOD IS SET                 
         BZ    VOPT90                                                           
         OC    ENDATEP,ENDATEP                                                  
         BZ    VOPT90                                                           
*                                                                               
         CLC   STDATEP,OPTDATE     MUST BE WITHIN PERIOD DATES                  
         BH    VOPTDERR                                                         
         CLC   ENDATEP,OPTDATE     MUST BE WITHIN PERIOD DATES                  
         BL    VOPTDERR                                                         
         B     VOPT90                                                           
*                                                                               
VOPTPGEN DS   0H                                                                
         OI    SVOPTSW,OPTPGEN     REQUEST IS FROM PATTERN GEN                  
         B     VOPT90                                                           
*                                                                               
VOPTPATT OI    SVOPTSW,OPTPTTN     PRINT PATTERN INFO                           
         B     VOPT90                                                           
*                                                                               
VOPTSKED MVI   SVTNPR9,C'S'                                                     
         B     VOPT90                                                           
*                                                                               
VOPTMED  MVI   SVTNPR9,C'M'                                                     
         B     VOPT90                                                           
*                                                                               
VOPTBOTH MVI   SVTNPR9,C'B'                                                     
         B     VOPT90                                                           
*                                                                               
VOPTNOPR MVI   SVTN1PR4,C'Y'                                                    
         B     VOPT90                                                           
*                                                                               
VOPTPRT  MVI   SVTN1PR4,C'N'                                                    
         B     VOPT90                                                           
*                                                                               
VOPTCAL  MVI   MYTN2PR6,C'C'                                                    
         B     VOPT90                                                           
*                                                                               
VOPTBRD  MVI   MYTN2PR6,C'B'                                                    
         B     VOPT90                                                           
*                                                                               
VOPTWEEK MVI   MYTN2PR6,C'W'                                                    
         B     VOPT90                                                           
*                                                                               
VOPTASS  MVI   SVTN1PR9,C'N'                                                    
         OI    SVOPTSW,OPTCLR                                                   
         B     VOPT90                                                           
*                                                                               
VOPTLEN  DS    0H                                                               
*                                                                               
* BUILD DUMMY FIELD HEADER TO FAKE OUT VALISLN                                  
         XC    WRK2,WRK2                                                        
         LLC   RF,1(R4)                                                         
         STC   RF,WRK2+5           LENGTH OF FLD                                
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WRK2+8(0),22(R4)    DATA                                         
         MVI   ERROPT,C'Y'                                                      
         LA    R2,WRK2                                                          
         GOTO1 VALISLN                                                          
         MVI   ERROPT,C'N'                                                      
         MVC   OPTLEN,WORK+4       SAVE BINARY LENGTH                           
         LA    R2,TRAFLTRH         RESTORE R2                                   
         CLI   ERROR,0             ANY ERROR?                                   
         BE    VOPT90                                                           
         B     VOPTERX                                                          
VOPT90   LA    R4,42(,R4)          POINT TO NEXT BLOCK                          
         BCT   R3,VOPT10           FOR NUMBER OF BLOCKS FOUND                   
*                                                                               
VOPT92   CLI   SVTN2PRO+00,0       ANY PROD OR PROD GROUP EXPECTED              
         BE    VOPT98               NO                                          
         CLI   SVTN2PRO+00,C'0'    ANY PROD OR PROD GROUP EXPECTED              
         BE    VOPT98               NO                                          
         CLI   SVTN2PRO+00,C'*'    EXPECTING PRODUCT                            
         BNE   VOPT96                                                           
         OC    OPTPROD,OPTPROD     ANY PROD ENTERED                             
         BNZ   VOPT98                                                           
         MVC   GERROR,=Y(PRDREQD)                                               
         B     VOPTTRAP                                                         
*                                                                               
VOPT96   DS   0H                                                                
         OC    OPTPRGR,OPTPRGR    ANY PRODUCT GROUP                             
         BNZ   VOPT98                                                           
         MVC   GERROR,=Y(PGRPREQD)                                              
         B     VOPTTRAP                                                         
*                                                                               
VOPT98   OI    4(R2),X'20'         SET VALIDATED                                
*                                                                               
VOPTX    DS    0H                                                               
         TM    SECFLAG,NECONPRD+NEMORPRD FULLY CONV OR DO BOTH                  
         BNZ   VOPTXX                     ALREADY CHECKED IT                    
*                                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    VOPTXX                                                           
*                                                                               
         OC    OPTPROD,OPTPROD     WAS PRODUCT OPTION ENTERED                   
         BZ    VOPTXX                                                           
*                                                                               
         LA    R0,NCLSTSIZ         FIND PROD                                    
         L     R1,ASVNCLST                                                      
*                                                                               
VOPT95   CLC   OPTPROD,0(R1)                                                    
         BE    VOPTXX                                                           
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '                                                       
         BNH   *+8                                                              
         BCT   R0,VOPT95                                                        
*                                                                               
         LA    R2,TRAFLTRH                                                      
         MVI   ERROR,SECLOCK       SECURITY LOCK-OUT                            
         B     VOPTERX                                                          
*                                                                               
VOPTXX   XIT1                                                                   
         EJECT                                                                  
* VALIDATE PRODUCT GROUP *                                                      
*                                                                               
VPGR     NTR1                                                                   
         MVI   ERROR,0                                                          
         OI    GENSTAT1,CATCHIOR   I WANT CONTROL BACK                          
         GOTO1 CATCHIOS            DON'T TASKNEXT OUT!                          
         CLI   ERROR,0             ANY ERROR?                                   
         BE    VPGR10               NO-STILL HAVE AT LEAST 800 IOS              
         TM    WHEN,X'10'          TEST OVERNIGHT                               
         BO    VOPTX                YES, LET IT GO THROUGH                      
         B     MAXIOER2                                                         
*                                                                               
VPGR10   GOTO1 VALIPGR                                                          
         MVC   OPTPRGR,SVPGRP     MOVE PRDGRP TO LOCAL STORAGE                  
*                                                                               
         LA   R0,L'OPTPGRL/3                                                    
         LHI  R5,OPTPGRL-SYSD                                                   
         AR   R5,R9                                                             
*                                                                               
VPGR30   MVC   SVKEY,KEY           SAVE                                         
*                                                                               
         LA    R2,ELEM             FAKE VALIPRD                                 
         XC    ELEM,ELEM                                                        
         MVC   ELEM(8),=X'0A01000184030001'                                     
         MVC   ELEM+8(3),SVKEY+8                                                
         MVI   ERROPT,C'Y'                                                      
         MVI   ERROR,0                                                          
*                                                                               
         GOTO1 VALIPRD                                                          
         MVI   ERROPT,0                                                         
*                                                                               
         CLI   ERROR,0             ANY ERROR?                                   
         BNE   VPGR50               YES                                         
*                                                                               
         MVC   0(3,R5),SVKEY+8                                                  
         LA    R5,3(,R5)                                                        
         BCT   R0,*+6                                                           
         DC    H'0'                MORE PRODUCTS THAN TABLE SIZE                
*                                                                               
VPGR50   MVC   KEY,SVKEY                                                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         CLC   KEY(8),KEYSAVE                                                   
         BE    VPGR30                                                           
         B     VOPTX                                                            
         EJECT                                                                  
* ERROR RTNS FOR VOPT                                                           
*                                                                               
VCMLERR  MVI   ERROR,INVCOMM       NO SUCH COMMERCIAL FOR CLT                   
         B     VOPTERX                                                          
CMLENER  MVI   ERROR,INVCMMLN      COMMERCIAL MUST BE 8 CHAR                    
         B     VOPTERX                                                          
PRLENERR MVC   GERROR,=Y(PRDLENER)                                              
         B     VOPTTRAP                                                         
VOPTPRER MVC   GERROR,=Y(PRODNA)                                                
         B     VOPTTRAP                                                         
*                                                                               
MAXIOER2 XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'MAXIOMS2),MAXIOMS2                                     
         NI    TRACLTH+4,X'FF'-X'20' FORCE CLIENT CHANGED                       
         GOTO1 ERREX2                                                           
MAXIOMS2 DC    C'*TOO MANY I/OS, MUST SEED BY NETWORK OR OVERNIGHT*'            
*                                                                               
*                                                                               
VOPTHLP  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'OPTHLPMS),OPTHLPMS                                     
         LA    R1,CONHEAD+L'OPTHLPMS-2                                          
         CLI   SVTN1PR4,C'Y'                                                    
         BNE   VOPTHLPA                                                         
         MVC   0(7,R1),=C'/PRT= *'                                              
         LA    R1,4(,R1)                                                        
         B     VOPTHLPC                                                         
VOPTHLPA MVC   0(9,R1),=C'/NOPRT= *'                                            
         LA    R1,6(,R1)                                                        
VOPTHLPC DS    0H                                                               
         LA    R2,TRANETH          NETWORK                                      
         CLI   5(R2),3                                                          
         BNE   VOPTHLPD                                                         
         CLC   =C'ALL',8(R2)                                                    
         BNE   VOPTHLPD                                                         
*                                                                               
         MVC   0(7,R1),=C'/ALL= *'                                              
         LA    R1,5(,R1)                                                        
*                                                                               
VOPTHLPD CLI   SVTN2PRO+00,0       ANY PROD OR PROD GROUP EXPECTED              
         BE    VOPTERX2             NO                                          
         CLI   SVTN2PRO+00,C'0'    ANY PROD OR PROD GROUP EXPECTED              
         BE    VOPTERX2             NO                                          
         MVC   0(7,R1),=C'/PRD= *'                                              
         CLI   SVTN2PRO+00,C'*'    EXPECTING PRODUCT                            
         BE    VOPTERX2                                                         
         MVC   1(6,R1),=C'PGR= *'                                               
         LA    R1,4(,R1)                                                        
*                                                                               
VOPTERX2 DS    0H                                                               
         LA    R2,TRAFLTRH                                                      
         GOTO1 ERREX2                                                           
*                                                                               
INVPRER  MVI   ERROR,INVPROD                                                    
         B     VOPTERX                                                          
MISSERRB MVI   ERROR,MISSING                                                    
VOPTERX  GOTO1 ERREX                                                            
VOPTTRAP GOTO1 VTRAERR                                                          
*                                                                               
OPTTABLE DS    0CL14                                                            
         DC    CL10'#         ',AL4(VOPTPGEN) REQUEST FROM PATTERN GEN          
         DC    CL10'ALL       ',AL4(VOPT90)   MEDIA FOR ALL NETS REQ            
         DC    CL10'NTYPE     ',AL4(VOPT90)                                     
         DC    CL10'BOTH      ',AL4(VOPTBOTH)                                   
         DC    CL10'BROADCAST ',AL4(VOPTBRD)                                    
         DC    CL10'CALENDAR  ',AL4(VOPTCAL)                                    
         DC    CL10'DATE      ',AL4(VOPTDATE)                                   
         DC    CL10'LENGTH    ',AL4(VOPTLEN)                                    
         DC    CL10'MEDIA     ',AL4(VOPTMED)                                    
         DC    CL10'NOPRT     ',AL4(VOPTNOPR)                                   
         DC    CL10'PATTERN   ',AL4(VOPTPATT)                                   
         DC    CL10'PGRP      ',AL4(VOPTPGRP)                                   
         DC    CL10'PRD       ',AL4(VOPTPRD)                                    
         DC    CL10'PROD      ',AL4(VOPTPRD)                                    
         DC    CL10'PRT       ',AL4(VOPTPRT)                                    
         DC    CL10'SKED      ',AL4(VOPTSKED)                                   
         DC    CL10'TEST      ',AL4(VOPTTEST)                                   
         DC    CL10'TRACE     ',AL4(VOPTRACE)                                   
         DC    CL10'WEEKLY    ',AL4(VOPTWEEK)                                   
         DC    CL10'CLEAR     ',AL4(VOPTASS)                                    
         DC    X'FF'                                                            
*                                                                               
OPTHLPMS DC    C'* OPTIONS=TEST/DATE/CLEAR/TS/-TS *'                            
TSMSG    DC    C'* ERROR * INVALID TRAFFIC SUPPLIER ENTERED *'                  
         LTORG                                                                  
         EJECT                                                                  
*=========================================================                      
* CHECK ESTIMATES, BYPASS THOSE WITH COPY CODE N                                
* ESTIMATE TABLE ENTRY = 5 BYTES,                                               
*                        1 EST, 3 PROD, 1 COPY CD                               
*=========================================================                      
                                                                                
CKEST    NTR1  BASE=*,LABEL=*                                                   
         L     R3,ANETBLK                                                       
         USING NETBLCKD,R3                                                      
*                                                                               
         OC    NBPR1CL3,NBPR1CL3   IS THERE ANY PROD?                           
         BNZ   CKEST04                                                          
         LTR   RB,RB               SET NE CC                                    
         B     CKESTX                                                           
*                                                                               
CKEST04  DS    0H                                                               
         LA    R0,249                                                           
         L     R1,AIO3                                                          
CKEST10  CLI   0(R1),0             EMPTY ENTRY                                  
         BE    CKEST30                                                          
         CLC   NBACTEST,0(R1)      SAME EST                                     
         BNE   CKEST14                                                          
         CLC   NBPR1CL3,1(R1)      SAME PRODUCT                                 
         BE    CKEST20                                                          
*                                                                               
         TM    NBUNST3,X'40'       COPY SPLIT                                   
         BZ    CKEST14                                                          
*                                                                               
         LA    RE,L'SVCSPROD/3                                                  
         LA    RF,SVCSPROD                                                      
CKEST12  CLI   0(RF),0                                                          
         BE    CKEST14                                                          
         CLC   NBPR1CL3,0(RF)                                                   
         BE    CKEST20                                                          
         LA    RF,3(,RF)                                                        
         BCT   RE,CKEST12                                                       
*                                                                               
CKEST14  LA    R1,5(,R1)                                                        
         BCT   R0,CKEST10                                                       
         DC    H'0'                                                             
CKEST20  CLI   4(R1),C'N'          BYPASS EST                                   
         XIT1                                                                   
*                                                                               
* RESET FILES TO SPOT *                                                         
*                                                                               
CKEST30  DS    0H                                                               
         BRAS  RE,SETSPOT          SWITCH TO SPOT                               
*                                                                               
CKEST32  XC    KEY,KEY                                                          
         MVC   KEY+1(3),BAGYMD                                                  
         MVC   KEY+4(3),NBPR1CL3                                                
*                                                                               
*KEST33  DS   0H                                                                
*        TM    SECFLAG,RD+NEMORPRD FULLY CONV OR DO BOTH                        
*        BNZ   CKEST35                    YES, CHECK IT                         
*                                                                               
*        LA    RE,NCLSTSIZ                                                      
*        L     RF,ASVNCLST                                                      
*KEST34  CLC   BYTE,3(RF)                                                       
*        BE    CKEST38                                                          
*        LA    RF,4(,RF)                                                        
*        CLI   0(RF),C' '                                                       
*        BNH   *+8                                                              
*        BCT   RE,CKEST34                                                       
*                                                                               
*        TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
*        BO    *+6                                                              
*        DC    H'0'                                                             
*        CR    RB,RB               SET CC EQ (BYPASS)                           
*        B     CKESTX                                                           
*                                                                               
*KEST35  DS    0H                                                               
         OC    NBPR1CL3,NBPR1CL3                                                
         BZ    CKEST36                                                          
*                                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    CKEST36                                                          
         LA    R2,ELEM             FAKE VALIPRD                                 
         XC    ELEM,ELEM                                                        
         MVC   ELEM(8),=X'0A01000184030001'                                     
*        MVC   ELEM+8(3),KEY+8                                                  
         MVC   ELEM+8(3),NBPR1CL3                                               
         MVI   ERROPT,C'Y'                                                      
*                                                                               
         LR    R4,R1               SAVE R1                                      
*                                                                               
         L     R0,AIO3             RESTORE                                      
         AHI   R0,2500             GO PAST OTHER USES                           
         LA    R1,1500                                                          
         L     RE,AIO1                                                          
         LR    RF,R1                                                            
         MVCL  (R0),(RE)                                                        
*                                                                               
         GOTO1 VALIPRD                                                          
         MVI   ERROPT,0                                                         
*                                                                               
         CLI   ERROR,0             ANY ERROR?                                   
         BNE   CKESERX              YES                                         
*                                                                               
         L     R0,AIO3                                                          
         AHI   R0,2500             GO PAST OTHER USES                           
         LA    R1,1500                                                          
         L     RE,AIO1                                                          
         LR    RF,R1                                                            
         MVCL  (RE),(R0)                                                        
*                                                                               
         LR    R1,R4               RESTORE                                      
*                                                                               
         LA    R2,TRAFLTRH         CLIENT                                       
*                                                                               
         LA    RF,WORK             PROD AND BPRD                                
         B     CKEST38                                                          
*                                                                               
CKEST36  MVC   KEY+4(3),=C'POL'                                                 
*        MVI   WORK+3,X'FF'                                                     
*        LA    RF,WORK             PROD AND BPRD                                
         B     *+10                                                             
*                                                                               
CKEST38  MVC   KEY+4(3),NBPR1CL3                                                
*        MVC   BYTE,3(RF)                                                       
         MVC   KEY+7(1),NBACTEST                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    CKEST40                                                          
         OC    NBPR1CL3,NBPR1CL3   IS THERE ANY PROD?                           
         BZ    CKESTX                                                           
         DC    H'0'                                                             
CKEST40  L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         USING ESTHDRD,R6                                                       
         MVC   0(1,R1),NBACTEST                                                 
         MVC   1(3,R1),NBPR1CL3                                                 
         MVC   4(1,R1),ECOPY                                                    
*                                                                               
         BRAS  RE,SETNET           SWITCH TO NET                                
         B     CKEST20                                                          
*                                                                               
*KEST50  MVC   0(1,R1),NBACTEST    IF NO POL EST, SHOW                          
*        MVI   1(R1),0                                                          
*        MVI   2(R1),0                                                          
*        B     CKEST20                                                          
*                                                                               
CKESTX   XIT1                                                                   
*                                                                               
CKESERX  MVI   ERROR,INVPROD                                                    
         GOTO1 ERREX                                                            
         DROP  R6                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* BUILD PRINT LINE(S) FOR ALL UNITS FOR 1 SEEDED PROGRAM *                      
*                                                                               
BLIN     NTR1  BASE=*,LABEL=*                                                   
         USING UNTABLED,R5                                                      
*                                                                               
         SR    RE,RE                                                            
         LR    RF,R5                                                            
         CLI   UNTADTEP-UNTENT(RF),0  ANY UNITS?                                
         BE    *+12                                                             
         LA    RF,L'UNTENT(,RF)                                                 
         BCT   RE,*-12                                                          
         LPR   R2,RE                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
* DO NOT USE QSORT, AS WE HAVE TO PRESERVE PREVIOUS SORT ORDER *                
*                                                                               
         ZAP   DUB,UNITCNT                                                      
         CVB   R0,DUB                                                           
         GOTO1 =V(XSORT),DMCB,AUNTABLE,(R0),L'UNTENT,L'UNTFEED,        C        
               UNTFEED-UNTENT,RR=SPTR62RR                                       
         GOTO1 (RF),(R1),,,,5,UNTDSKAD-UNTENT                                   
         GOTO1 (RF),(R1),,,,L'UNTADTEP,UNTADTEP-UNTENT                          
         GOTO1 (RF),(R1),,,,L'UNTPROG,UNTPROG-UNTENT                            
*                                                                               
         CLI   SVTN1PR4,C'Y'       DON'T PRINT REPORT                           
         BE    BLIN11                                                           
*                                                                               
         XC    SVUNTDTE,SVUNTDTE                                                
         OI    UPDSW,X'20'         SET ON PRINTING UNITS SW                     
*                                                                               
         CLI   UNTADTEP,0          ANY UNITS?                                   
         BNE   BLIN10                                                           
         DC    H'0'                                                             
*                                                                               
BLIN10   DS    0H                  MINIMUM LINES TO PRINT                       
         LA    R1,2                MINIMUM LINES TO PRINT                       
         LA    R2,P                                                             
         USING PRTLINE,R2                                                       
         XC    SVCMLDS2,SVCMLDS2   CLEAR SECOND TITLE                           
         XC    SVCMLDS3,SVCMLDS3   CLEAR THIRD TITLE                            
         CLI   UNTPROD2,0          P/B PROD                                     
         BE    *+8                                                              
         LA    R1,2(,R1)                                                        
*                                                                               
         CLC   UNTIME,SVPRGTIM     THIS SAME PROG TIME                          
         BE    *+8                                                              
         LA    R1,1(,R1)                                                        
         TM    UNTFLG,UNTFLGNW+UNTFLGCH+UNTFLGAS  NEW OR CHANGE                 
         BZ    *+14                         OR PREVIOUSLY ASSIGNED              
         LA    R1,1(,R1)                                                        
         AP    PRGCU,=P'1'                                                      
*                                                                               
         STC   R1,ALLOWLIN                                                      
*                                                                               
BLIN11   DS    0H                                                               
         MVC   SVCMLCOD,UNTCML1    PRESET                                       
         OC    UNTCML1,UNTCML1                                                  
         BZ    *+8                                                              
BLIN11A  BRAS  RE,FCML              VALIDATE CML (ADD ADID TO TABLE)            
         CLC   SVCMLCOD,UNTCML2                                                 
         BE    BLIN11B             DONE                                         
         MVC   SVCMLCOD,UNTCML2                                                 
         OC    UNTCML2,UNTCML2                                                  
         BNZ   BLIN11A                                                          
*                                                                               
BLIN11B  L     RF,APRGTBLE                                                      
         USING PRGTABLD,RF                                                      
*                                                                               
BLIN11C  CLC   SVPRG,PRGPRG                                                     
         BE    BLIN11D                                                          
         LA    RF,PRGNEXT                                                       
         OC    PRGPRG,PRGPRG                                                    
         BNZ   BLIN11C                                                          
         DC    H'0'                                                             
*                                                                               
BLIN11D  MVC   SVPRGRVN,PRGREVN                                                 
*                                                                               
         OC    SVPRGCTS,SVPRGCTS                                                
         BZ    *+10                                                             
         MVC   PRGCTS,SVPRGCTS                                                  
*                                                                               
         CLC   UNTPROG,SVPRG                                                    
         BE    BLIN14                                                           
*                                                                               
         L     RF,APRGTBLE                                                      
         USING PRGTABLD,RF                                                      
         CLC   UNTPROG,PRGPRG                                                   
         BE    *+20                                                             
         LA    RF,PRGNEXT                                                       
         OC    PRGPRG,PRGPRG                                                    
         BNZ   *-20                                                             
         DC    H'0'                                                             
*                                                                               
         MVC   SVPRGENT,PRGENT                                                  
         DROP  RF                                                               
*                                                                               
         BRAS  RE,SETSPOT          SET TO SPOT                                  
*                                                                               
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING NPGKEY,R1                                                        
         MVC   NPGKTYP,=X'0D20'                                                 
         MVC   NPGKAM,BAGYMD                                                    
         MVC   NPGKNET,NETMKT                                                   
         MVC   NPGKPROG,SVPRG                                                   
         MVC   NPGKEND,UNTADTE2                                                 
         DROP  R1                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE                                                  
         BE    BLIN13F                                                          
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         LA    RE,EQVPTBL                                                       
         LA    RF,L'EQVPTBL/(EQVNEXT-EQVENT)                                    
         USING EQVPTBLD,RE                                                      
*                                                                               
BLIN13   OC    EQVENT,EQVENT       EMPTY ENTRY IS END                           
         BZ    XPROGPER            PROGRAM PERIOD ERROR                         
*                                                                               
         CLC   EQVBPRG,SVPRG       THIS SAME AS CURRENT                         
         BNE   BLIN13C                                                          
*                                                                               
         CLC   UNTADTEP,EQVSDT     WITHIN DATES                                 
         BL    BLIN13C                                                          
         CLC   UNTADTE2,EQVEDT                                                  
         BNH   BLIN13E                                                          
*                                                                               
BLIN13C  LA    RE,EQVNEXT                                                       
         BCT   RF,BLIN13                                                        
         DC    H'0'                                                             
BLIN13E  LA    R1,KEY                                                           
         USING NPGKEY,R1                                                        
         MVC   NPGKPROG,EQVEPRG                                                 
         DROP  R1,RE                                                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
BLIN13F  L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'92'                                                     
*                                                                               
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NPGEL92,R6                                                       
         MVC   SVPRGDAY,NPGDAY                                                  
         MVC   SVPRGTIM,NPGTIME                                                 
         MVC   SVPRGNM,NPGNAME                                                  
*                                                                               
         CLI   NPGROT,0            NPGROT OVERRIDES DAY IF PRESENT              
         BE    *+10                                                             
         MVC   SVPRGDAY,NPGROT                                                  
         DROP  R6                                                               
*                                                                               
         CLI   SVTN1PR4,C'Y'       DON'T PRINT REPORT                           
         BE    BLIN25C                                                          
*                                                                               
         MVC   PPROG,SVPRG                                                      
         MVC   PPROGNM,SVPRGNM                                                  
*                                                                               
BLIN14   CLI   SVTN1PR4,C'Y'       DON'T PRINT REPORT                           
         BE    BLIN25C                                                          
*                                                                               
         CLI   UNTDP,0                                                          
         BE    BLIN16                                                           
*                                                                               
         XC    GERROR,GERROR                                                    
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
*                                                                               
         GOTO1 VALIDPT,DMCB,(X'01',UNTDP)  GET 2 CHAR DAYPART                   
*                                                                               
         MVI   ERROPT,0                                                         
         OC    GERROR,GERROR                                                    
         BZ    BLIN15                                                           
*                                                                               
         MVC   PDAYPT,=C'??'       BAD DAYPART CODE PRINT '??'                  
         B     *+10                                                             
BLIN15   MVC   PDAYPT,QDPT2                                                     
*                                                                               
BLIN16   MVC   FULL(2),UNTADTEP                                                 
         MVC   FULL+2(2),UNTADTE2                                               
         CLC   FULL,SVUNTDTE                                                    
         BE    BLIN20                                                           
*                                                                               
         MVC   SVUNTDTE,FULL                                                    
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(2,UNTADTEP),(4,PAIRDT)                              
*                                                                               
         CLC   UNTADTEP,UNTADTE2   BOTH DATES EQUAL                             
         BE    BLIN20                                                           
         MVI   PAIRDT+5,C'-'                                                    
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(2,UNTADTE2),(4,PAIRDT2)                             
*                                                                               
BLIN20   TM    UNTFLG,UNTFLGCS     THIS A COPY SPLIT                            
         BZ    BLIN24                                                           
         OC    UNTFEED,UNTFEED     THIS A FEED                                  
         BNZ   BLIN24               YES                                         
         MVC   PFEED+1(3),=C'C/S'                                               
         B     BLIN24C                                                          
*                                                                               
BLIN24   DS    0H                                                               
         CLC   =X'00FFE3',UNTFEED  CHK FOR TAG                                  
         BNE   BLIN24C                                                          
         MVC   PFEED+131(1),UNTFEED+3                                           
         MVI   PFEED+132,C'-'                                                   
         MVC   PFEED+133(6),=C'TAG(S)'                                          
         B     *+10                                                             
BLIN24C  MVC   PFEED,UNTFEED       THIS A FEED                                  
         TM    UNTFLG1,UNTFL1FD    FEED NO NATIONAL                             
         BZ    BLIN25                                                           
         LR    RF,R5               YES                                          
         SH    RF,=AL2(UNTNEXT-UNTENT)                                          
         CLC   UNTDSKAD,UNTDSKAD-UNTENT(RF) PREV UNIT SAME REC                  
         BE    BLIN25                        YES                                
         MVC   PFEED+132(19),=C'*** NO NATIONAL ***'                            
*                                                                               
BLIN25   MVC   QPRD,UNTPROD                                                     
*        MVC   BPRD,UNTPRD                                                      
         BAS   RE,FPRD                                                          
         MVC   PPROD,QPRD                                                       
         MVC   PPRDNM,PRDNM                                                     
*                                                                               
BLIN25C  OC    UNTPSEQ,UNTPSEQ     ANY PATTERN FOR THIS UNIT THIS TIME          
         BZ    BLIN26               NO                                          
*                                                                               
         OC    UNTCML1,UNTCML1                                                  
         BNZ   BLIN30                                                           
*                                                                               
BLIN26   DS    0H                                                               
         CLI   SVTN1PR9,C'Y'       BYPASS ASSIGNED CMLS                         
         BNE   BLIN27               NO                                          
         TM    UNTFLG,UNTFLGAS     PREVIOUSLY ASSIGNED                          
         BO    BLIN30                                                           
*                                                                               
BLIN27   DS    0H                                                               
         CLI   SVTN1PR4,C'Y'       DON'T PRINT REPORT                           
         BE    *+10                                                             
         MVC   PCML+2(15),=CL15'TO BE ANNOUNCED'                                
         XC    UNTCML1(16),UNTCML1                                              
         XC    UNTREF,UNTREF                                                    
         OI    UNTFLG,UNTFLGCH                                                  
*                                                                               
         CLI   SVTN1PR4,C'Y'       DON'T PRINT REPORT                           
         BE    BLIN96                                                           
         B     BLIN34                                                           
*                                                                               
* MARK ANY REVISIONS FROM THIS RUN *                                            
*                                                                               
BLIN30   DS    0H                                                               
         CLI   SVTN1PR4,C'Y'       DON'T PRINT REPORT                           
         BE    BLIN96                                                           
*                                                                               
         CLI   SVPRGRVN,0          IF ORIG, NO NEW                              
         BE    BLIN32                                                           
         CLC   SVPRGRVN,UNTREVN    THIS CHANGED THIS REVSION                    
         BNE   BLIN32                                                           
         MVI   PAIRDT-1,C'*'       MARK THIS REVISION CHANGE                    
         MVI   PCML-1,C'*'                                                      
BLIN32   DS    0H                                                               
         TM    UNTFLG,UNTFLGSW     WERE PRDS SWAPPED                            
         BZ    BLIN32C              NO                                          
         TM    UNTFLG,UNTFLGNW     IS IT NEW ASSIGN                             
         BZ    BLIN32C                                                          
         XC    UNTCML1,UNTCML2      YES SWAP CMLS                               
         XC    UNTCML2,UNTCML1                                                  
         XC    UNTCML1,UNTCML2                                                  
         NI    UNTFLG,X'FF'-UNTFLGSW                                            
*                                                                               
BLIN32C  MVC   SVCMLCOD,UNTCML1                                                 
*                                                                               
         BRAS  RE,FCML              VALIDATE CML                                
         MVC   PCML,WORK+8                                                      
*                                                                               
         MVC   WORK(8),WORK+8       MOVE FOR RADID                              
         XC    WORK+8(12),WORK+8                                                
         BRAS  RE,RADID            GET ADID FROM TABLE                          
         CLI   WORK+8,0                                                         
         BE    BLIN32D                                                          
         GOTO1 VTRPACK,DMCB,(C'U',WORK+8),PCML  PRINT ADID                      
*                                                                               
BLIN32D  MVC   PCMLTL,SVCMLDS                                                   
*                                                                               
         CLC   UNTSLN,SVCMLLEN     IF UNIT LEN NOT = CML LEN                    
         BE    BLIN34                                                           
         OC    UNTPROD2,UNTPROD2   AND THERE IS A P/B PRD                       
         BZ    BLIN34                                                           
         CLI   UNTSLN2,0           AND UNIT LEN 2 ZERO                          
         BNE   BLIN34                                                           
         LLC   RE,UNTSLN           ADJUST LENGTHS                               
         LLC   RF,SVCMLLEN                                                      
         SR    RE,RF                                                            
         STC   RF,UNTSLN                                                        
         STC   RE,UNTSLN2                                                       
*                                                                               
BLIN34   LLC   R0,UNTSLN                                                        
*                                                                               
         OC    UNTCML1,UNTCML1                                                  
         BZ    BLIN38                                                           
         CLC   UNTCML1,UNTCML2     1 COMML FOR BOTH PRODS                       
         BNE   BLIN38                                                           
BLIN36   LLC   R1,UNTSLN2                                                       
         AR    R0,R1                                                            
BLIN38   DS    0H                                                               
         OC    SVCMLOVR,SVCMLOVR   ANY OVERRIDE?                                
         BZ    BLIN39                                                           
         LA    RF,SVCMLOVR                                                      
         EDIT  (1,(RF)),(3,PUNTLEN),ALIGN=LEFT,ZERO=NOBLANK                     
         LA    R1,PUNTLEN                                                       
         AR    R1,R0                                                            
         MVI   0(R1),C'/'                                                       
         EDIT  (1,1(RF)),(3,1(R1)),ALIGN=LEFT,ZERO=NOBLANK                      
         B     BLIN39C                                                          
BLIN39   EDIT  (R0),(3,PUNTLEN)                                                 
*                                                                               
BLIN39C  OC    UNTACOST,UNTACOST   ANY COST                                     
         BZ    BLIN39F              NO                                          
         BAS   RE,ACOST                                                         
         MVC   PPROG+2+132(12),=C'UNIT COST= $'                                 
         MVC   PPROG+14+132(11),WORK                                            
*                                                                               
BLIN39F  OC    UNTPROD2,UNTPROD2                                                
         BZ    BLIN40                                                           
*                                                                               
         OC    UNTCML1,UNTCML1     WAS THERE A CML                              
         BZ    BLIN40                                                           
*                                                                               
         CLI   SVTN1PR9,C'Y'       BYPASS ASSIGNED CMLS                         
         BNE   *+12                                                             
         TM    UNTFLG,UNTFLGAS     PREV ASSIGN                                  
         BO    *+14                                                             
         CLC   PCML+2(15),=CL15'TO BE ANNOUNCED'                                
         BE    BLIN40                                                           
*                                                                               
         CLC   UNTCML1,UNTCML2    1 CMML FOR BOTH PRODS                         
         BE    BLIN40                                                           
         LA    R0,23                                                            
         LA    R6,PCMLTL+23        SAVE FOR POSSIBLE PIGGYBACK                  
         CLI   0(R6),C' '                                                       
         BH    *+12                                                             
         BCTR  R6,0                                                             
         BCT   R0,*-10                                                          
         DC    H'0'                                                             
*                                                                               
         MVI   1(R6),C'/'                                                       
         LA    RE,PCML+11                                                       
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         MVI   1(RE),C'-'                                                       
*                                                                               
         LA    RF,PUNTLEN          GET FIRST BLANK SPACE                        
         CLI   0(RF),C' '                                                       
         BE    *+12                                                             
         LA    RF,1(RF)                                                         
         B     *-12                                                             
         MVI   0(RF),C'-'                                                       
*                                                                               
BLIN40   OC    SVCMLDS2,SVCMLDS2                                                
         BZ    BLIN42                                                           
         BAS   RE,NXL              GO TO NEXT PRINT LINE OR PRINT               
         MVC   PCMLTL,SVCMLDS2                                                  
         BAS   RE,NXL                                                           
         OC    SVCMLDS3,SVCMLDS3                                                
         BZ    *+14                                                             
         MVC   PCMLTL,SVCMLDS3                                                  
BLIN42   BAS   RE,NXL              GO TO NEXT PRINT LINE OR PRINT               
         EJECT                                                                  
* PRINT PARTNER IF ANY *                                                        
*                                                                               
         OC    UNTPROD2,UNTPROD2                                                
         BZ    BLIN60                                                           
*                                                                               
         MVC   QPRD,UNTPROD2                                                    
*        MVC   BPRD,UNTPRD2                                                     
         BAS   RE,FPRD                                                          
         MVC   PPROD,QPRD                                                       
         MVC   PPRDNM,PRDNM                                                     
*                                                                               
         EDIT  (B1,UNTSLN2),(3,PUNTLEN)                                         
*                                                                               
         OC    UNTCML2,UNTCML2     ANY COMML ASSIGNED                           
         BZ    BLIN48                                                           
*                                                                               
         CLC   UNTCML1,UNTCML2                                                  
         BNE   BLIN46                                                           
         MVC   PAIRDT+2(3),=C'P/B'                                              
         MVC   PCML+3(3),=C'P/B'                                                
*                                                                               
         MVI   PUNTLEN,C'('                                                     
         LA    R1,PUNTLEN+1                                                     
         EDIT  (B1,UNTSLN),(3,0(R1)),ALIGN=LEFT                                 
         CLI   0(R1),C' '                                                       
         BNH   *+12                                                             
         LA    R1,1(,R1)                                                        
         B     *-12                                                             
         MVI   0(R1),C','                                                       
         EDIT  (B1,UNTSLN2),(3,1(R1)),ALIGN=LEFT                                
         CLI   0(R1),C' '                                                       
         BNH   *+12                                                             
         LA    R1,1(,R1)                                                        
         B     *-12                                                             
         MVI   0(R1),C')'                                                       
         B     BLIN48                                                           
*                                                                               
BLIN46   MVC   SVCMLCOD,UNTCML2                                                 
*                                                                               
         BRAS  RE,FCML              VALIDATE CML                                
         MVC   PCML,WORK+8          ISCI OR ADID (12)                           
*                                                                               
         CLC   WORK(8),WORK+8      TEST ISCI CMML                               
         BNE   BLIN46A             NO                                           
*                                                                               
         MVC   WORK(8),WORK+8       MOVE FOR RADID                              
         XC    WORK+8(12),WORK+8                                                
         BRAS  RE,RADID            GET ADID FROM TABLE                          
         CLI   WORK+8,0                                                         
         BE    BLIN46A                                                          
         GOTO1 VTRPACK,DMCB,(C'U',WORK+8),PCML  PRINT ADID                      
*                                                                               
BLIN46A  MVC   PCMLTL,SVCMLDS                                                   
*                                                                               
         OC    SVCMLOVR,SVCMLOVR   ANY OVERRIDE?                                
         BZ    BLIN47                                                           
         LA    RF,SVCMLOVR                                                      
         EDIT  (1,(RF)),(3,PUNTLEN),ALIGN=LEFT,ZERO=NOBLANK                     
         LA    R1,PUNTLEN                                                       
         AR    R1,R0                                                            
         MVI   0(R1),C'/'                                                       
         EDIT  (1,1(RF)),(3,1(R1)),ALIGN=LEFT,ZERO=NOBLANK                      
*                                                                               
BLIN47   OC    SVCMLDS2,SVCMLDS2                                                
         BZ    BLIN48                                                           
         BAS   RE,NXL              GO TO NEXT PRINT LINE OR PRINT               
         MVC   PCMLTL,SVCMLDS2                                                  
         BAS   RE,NXL              GO TO NEXT PRINT LINE OR PRINT               
         OC    SVCMLDS3,SVCMLDS3                                                
         BZ    *+14                                                             
         MVC   PCMLTL,SVCMLDS3                                                  
BLIN48   BAS   RE,NXL              GO TO NEXT PRINT LINE OR PRINT               
*                                                                               
BLIN60   CLI   SVTN1PR4,C'Y'       DON'T PRINT REPORT                           
         BE    BLIN96                                                           
*                                                                               
         SR    RE,RE                                                            
         TM    UNTFLG,UNTFLGNW     NEW ASSIGN                                   
         BZ    *+14                                                             
         MVC   PCML+2(5),=C'*NEW*'                                              
         LA    RE,SVPRGNEW                                                      
         TM    UNTFLG,UNTFLGCH     CHANGE ASSIGN                                
         BZ    *+14                                                             
         MVC   PCML+2(5),=C'*CHG*'                                              
         LA    RE,SVPRGCHG                                                      
         TM    UNTFLG,UNTFLGAS     PREVIOUSLY ASSIGNED                          
         BZ    *+14                                                             
         MVC   PCML+2(15),=C'*PREV ASSIGNED*'                                   
         B     BLIN62                                                           
         LTR   RE,RE                                                            
         BZ    BLIN64                                                           
         LH    RF,0(RE)                                                         
         LA    RF,1(,RF)                                                        
         STH   RF,0(RE)                                                         
*                                                                               
BLIN62   CLI   SVTN1PR4,C'Y'       DON'T PRINT REPORT                           
         BE    BLIN96                                                           
*                                                                               
         BAS   RE,NXL                                                           
*                                                                               
* PRINT DAY/TIME/PROGRAM NAME IF DIFFERENT FROM PROGRAM HEADING DATA *          
*                                                                               
BLIN64   MVC   WORK(1),SVPRGDAY                                                 
         CLI   UNTEXNAM,0         STD PROGRAM NAME                              
         BNE   BLIN70                                                           
         CLC   SVPRGTIM,UNTTIME                                                 
         BE    BLIN90                                                           
BLIN70   MVC   PPRDNM(9),=CL9'DIFFERENT'                                        
         LA    R6,PPRDNM+10                                                     
         CLI   UNTEXNAM,0         STD PROGRAM NAME                              
         BE    BLIN80                                                           
         MVC   0(13,R6),=CL13'PROGRAM NAME='                                    
         CLI   UNTEXNAM,X'FF'      NOT IN TABLE, MORE THAN 14                   
         BNE   BLIN74                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'UNTFIL',UNTDSKAD,AIO1,DMWORK          
         L     RF,AIO1                                                          
         LA    RF,NUPROGNM-NURECD(RF)                                           
         B     BLIN76                                                           
BLIN74   LLC   RE,UNTEXNAM                                                      
         BCTR  RE,0                                                             
         SLL   RE,4               TIMES 16                                      
         L     RF,AIO3                                                          
         LA    RF,1250(RE,RF)                                                   
BLIN76   MVC   14(16,R6),0(RF)                                                  
         LA    R6,40(,R6)                                                       
*                                                                               
BLIN80   CLC   SVPRGTIM,UNTTIME                                                 
         BE    BLIN90                                                           
         MVC   0(4,R6),=CL4'TIME'                                               
         GOTO1 UNTIME,DMCB,UNTTIME,5(R6)                                        
BLIN90   TM    SVOPTSW,OPTPTTN     PRINT PATTERN INFO                           
         BZ    BLIN94                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R2,P1                                                            
         OC    UNTPSEQ,UNTPSEQ     ANY PATTERN ASSIGNED                         
         BNZ   BLIN92                                                           
         CLI   SVTN1PR9,C'Y'       BYPASS ASSIGNED CMLS                         
         BNE   *+12                                                             
         TM    UNTFLG,UNTFLGAS     PREV ASSIGN                                  
         BO    BLIN94                                                           
         MVC   PPROD(7),=C'NO PTTN'                                             
         B     BLIN94                                                           
*                                                                               
BLIN92   BRAS  RE,PTN                   PRINT PATTERN INFO                      
*                                                                               
BLIN94   MVI   SPACING,2                                                        
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
BLIN96   LA    R5,UNTNEXT          NEXT UNIT                                    
         CLC   UNTPROG,SVPRG        SAME PROGRAM                                
         BE    BLIN10                                                           
         NI    UPDSW,X'FF'-X'20'   SET OFF PRINTING UNITS SW                    
*                                                                               
         CLI   SVTN1PR4,C'Y'       DON'T PRINT REPORT                           
         BE    BLIN100                                                          
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   P+3(8),=C'ORIGINAL'                                              
         CLI   SVPRGRVN,0                                                       
         BE    BLIN98                                                           
         MVC   P+3(8),=CL8'REV'                                                 
         EDIT  (B1,SVPRGRVN),(3,P+7)                                            
*                                                                               
BLIN98   LH    R0,SVPRGUNT                                                      
         EDIT  (R0),(4,P+13)                                                    
         MVC   P+18(5),=C'UNITS'                                                
*                                                                               
         LH    R0,SVPRGFDS                                                      
         EDIT  (R0),(4,P+25),ZERO=NOBLANK                                       
         MVC   P+30(5),=C'FEEDS'                                                
*                                                                               
         LH    R0,SVPRGUNA                                                      
         SH    R0,SVPRGNEW                                                      
         EDIT  (R0),(4,P+37),ZERO=NOBLANK                                       
         MVC   P+42(10),=C'UNASSIGNED'                                          
*                                                                               
         LH    R0,SVPRGUAL                                                      
         EDIT  (R0),(4,P+54),ZERO=NOBLANK                                       
         MVC   P+59(11),=C'UNALLOCATED'                                         
*                                                                               
         EDIT  (B2,SVPRGNEW),(4,P+71),ZERO=NOBLANK                              
         MVC   P+76(11),=C'NEW ASSIGNS'                                         
*                                                                               
         EDIT  (B2,SVPRGCHG),(4,P+88),ZERO=NOBLANK                              
         MVC   P+93(15),=C'CHANGED ASSIGNS'                                     
         GOTO1 SPOOL,(R1),(R8)                                                  
*                                                                               
         EDIT  (B2,SVPRGTBA),(4,P+71),ZERO=NOBLANK                              
         MVC   P+76(10),=C'FORCED TBA'                                          
*                                                                               
         CLI   SVTN1PR9,C'Y'       BYPASS ASSIGNED CMLS                         
         BNE   BLIN99                                                           
         EDIT  (B2,SVPRGASS),(4,P+88),ZERO=NOBLANK                              
         MVC   P+93(13),=C'PREV ASSIGNED'                                       
BLIN99   MVI   SPACING,2                                                        
         GOTO1 SPOOL,(R1),(R8)                                                  
*                                                                               
BLIN100  CLI   UNTADTEP,0          ANY MORE                                     
         BNE   BLIN10               YES                                         
*                                                                               
         OC    SVPRGCTS,SVPRGCTS                                                
         BZ    BLINX                                                            
         L     RF,APRGTBLE                                                      
         USING PRGTABLD,RF                                                      
         CLC   SVPRG,PRGPRG                                                     
         BE    *+20                                                             
         LA    RF,PRGNEXT                                                       
         OC    PRGPRG,PRGPRG                                                    
         BNZ   *-20                                                             
         DC    H'0'                                                             
         MVC   PRGCTS,SVPRGCTS                                                  
         DROP  RF                                                               
*                                                                               
BLINX    XIT1                                                                   
*                                                                               
XPROGPER L     R2,=A(WORKFIL)      CLOSE WORKFIL ON ERROR                       
         CLOSE ((R2),)                                                          
         FREEPOOL (R2)                                                          
         MVC   GERROR,=Y(PROGCOVR)  PROG RECORD DOESN'T COVER PER+ROT           
         LA    R2,CONWHENH                                                      
         GOTO1 VTRAERR                                                          
         EJECT                                                                  
CPRDA    DS   0H                                                                
         CLC   DUB(3),DUB+3                                                     
         BLR   RE                                                               
         XC    UNTCML1,UNTCML2     SWAP COMMERCIALS                             
         XC    UNTCML2,UNTCML1                                                  
         XC    UNTCML1,UNTCML2                                                  
         BR    RE                                                               
         EJECT                                                                  
* GET ACTUAL COST                                                               
*                                                                               
ACOST    NTR1                                                                   
*                                                                               
         ICM   R2,15,UNTACOST                                                   
         SRDA  R2,32                                                            
         D     R2,=F'100'                                                       
         EDIT  (R3),(11,WORK),ALIGN=LEFT,MINUS=YES,COMMAS=YES                   
         XIT1                                                                   
*                                                                               
* FIND PRODUCT NAME *                                                           
*                                                                               
FPRD     NTR1                                                                   
*                                                                               
*        CLI   BPRD,0              UNALLOCATED                                  
*        BE    FPRD30                                                           
*                                                                               
*        LA    R0,NCLSTSIZ         MAX COUNT BUG CATCHER                        
*        L     R1,ASVNCLST         TABLE OF CLIENT PROD CODES                   
*PRD02   CLC   BPRD,3(R1)          THIS A VALID PROD CODE                       
*        BE    FPRD06                                                           
*        LA    R1,4(,R1)           BUMP PROD PTR                                
*        CLI   0(R1),C' '          AT END OF TABLE?                             
*        BNH   FPRD04              YES                                          
*        BCT   R0,FPRD02                                                        
*PRD04   DC    H'0'                SEE IF BRAND LEVEL SECURITY PROBLEM          
*PRD06   MVC   QPRD,0(R1)                                                       
*                                                                               
         OC    QPRD,QPRD                                                        
         BZ    FPRD30                                                           
*                                                                               
         BRAS  RE,SETSPOT          SET TO SPOT                                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),QPRD                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         USING PRDHDRD,R6                                                       
*                                                                               
         BRAS  RE,SETNET           SET TO NET                                   
*                                                                               
         MVC   PRDNM(20),PNAME       AND PRODUCT NAME                           
         B     BLINX                                                            
*                                                                               
FPRD30   MVC   QPRD,=C'---'                                                     
         MVC   PRDNM,=CL20'TO BE ALLOCATED'                                     
         B     BLINX                                                            
         DROP  R6                                                               
         EJECT                                                                  
* INCREMENT TO NEXT LINE AND PRINT IF ALL USED *                                
*                                                                               
NXL      NTR1                                                                   
         MVI   131(R2),0                                                        
         LA    R2,132(,R2)                                                      
         LA    R0,P4                                                            
         CR    R2,R0                                                            
         BNH   NXLX                                                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   ALLOWLIN,0                                                       
         LA    R2,P                                                             
NXLX     XIT1  REGS=(R2)                                                        
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* PRINT PATTERN KEY AND REF *                                                   
*                                                                               
PTN      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   PPROD(11),=C'PATTERN KEY'                                        
         SR    R6,R6                                                            
         ICM   R6,3,UNTPSEQ                                                     
         BCTR  R6,0                                                             
         MH    R6,=AL2(PATNEXT-PATENT)                                          
         A     R6,APATABLE                                                      
         USING PATABLED,R6                                                      
*                                                                               
         CLI   PATYPE,PATQDPMD     MEDIA DAYPART SPECIFIC                       
         BE    PTN04                                                            
*                                                                               
         CLI   PATYPE,PATQMED      MEDIA SPECIFIC                               
         BNE   PTN06                                                            
PTN04    MVC   PPROD+12(6),=C'MEDIA='                                           
         MVC   PPROD+18(1),SVMEDIA                                              
         LA    R2,PPROD+20                                                      
*                                                                               
         CLI   PATYPE,PATQDPMD     MEDIA DAYPART SPECIFIC                       
         BE    PTN24                                                            
*                                                                               
         B     PTN50                                                            
*                                                                               
PTN06    MVC   PPROD+12(4),=C'NET='                                             
*                                                                               
         CLI   PATYPE,PATQALL      ALL NET                                      
         BNE   PTN10                                                            
         MVC   PPROD+16(5),=C'(ALL)'                                            
         LA    R2,PPROD+22                                                      
         B     PTN50                                                            
*                                                                               
PTN10    MVC   PPROD+16(4),NETWORK                                              
         LA    R2,PPROD+22                                                      
         CLI   PATYPE,PATQNET      BY NET                                       
         BE    PTN50                                                            
         CLI   PATYPE,PATQPROG     BY PROG                                      
         BNE   PTN20                                                            
         OC    PATNONFD,PATNONFD                                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLI   PATDP,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(5,R2),=C'PROG='                                                
         MVC   5(6,R2),SVPRG                                                    
         LA    R2,13(,R2)                                                       
         B     PTN50                                                            
PTN20    CLI   PATYPE,PATQDPT      BY DAYPART                                   
         BNE   PTN30                                                            
*                                                                               
PTN24    OC    PATFEED,PATFEED                                                  
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    GERROR,GERROR                                                    
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
*                                                                               
         GOTO1 VALIDPT,DMCB,(X'01',PATDP)  GET 2 CHAR DAYPART                   
*                                                                               
         MVI   ERROPT,0                                                         
         OC    GERROR,GERROR                                                    
         BZ    PTN24F                                                           
*                                                                               
         MVC   4(2,R2),=C'??'      BAD DAYPART CODE PRINT '??'                  
         B     *+10                                                             
PTN24F   MVC   4(2,R2),QDPT2                                                    
         MVC   0(4,R2),=C'D/P='                                                 
         LA    R2,8(,R2)                                                        
         B     PTN50                                                            
*                                                                               
PTN30    CLI   PATYPE,PATQFEED     BY FEED                                      
         BNE   PTN40                                                            
         CLI   PATDP,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(5,R2),=C'FEED='                                                
         MVC   5(4,R2),PATFEED                                                  
         LA    R2,10(,R2)                                                       
         B     PTN50                                                            
PTN40    CLI   PATYPE,PATQDPFD     BY FEED AND DAYPART                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   PATDP,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         OC    PATFEED,PATFEED                                                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   0(5,R2),=C'FEED='                                                
         MVC   5(4,R2),PATFEED                                                  
         MVC   10(4,R2),=C'D/P='                                                
         MVC   14(1,R2),PATDP                                                   
         LA    R2,17(,R2)                                                       
PTN50    MVC   0(4,R2),=C'REF='                                                 
         SR    R0,R0                                                            
         ICM   R0,7,PATREF                                                      
         X     R0,=X'00FFFFFF'                                                  
         EDIT  (R0),(5,4(R2)),ALIGN=LEFT                                        
*                                                                               
         LA    R2,12(,R2)                                                       
         MVC   0(4,R2),=C'SEQ='                                                 
         SR    R0,R0                                                            
         ICM   R0,7,PATSEQNO                                                    
         EDIT  (R0),(5,4(R2)),ALIGN=LEFT                                        
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         DROP  R2,R5,R6                                                         
         EJECT                                                                  
* FIND COMMERCIAL TITLE *                                                       
*                                                                               
         DS    0H                                                               
FCML     NTR1  BASE=*,LABEL=*                                                   
         USING UNTABLED,R5                                                      
*                                                                               
         LA    R3,SVCMLCOD                                                      
         XC    SVCMLDS2,SVCMLDS2                                                
         XC    SVCMLDS3,SVCMLDS3                                                
*                                                                               
         BRAS  RE,SETSPOT          SET TO SPOT                                  
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(3),BAGYMD AND BCLT                                        
         MVC   CMLKCML,SVCMLCOD                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    FCML15                                                           
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         MVI   KEY+1,X'C1'         SEE IF AD-ID                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
FCML15   L     R6,AIO2                                                          
         LA    R6,2000(R6)         CML RECORD IS READ HERE                      
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVC   WORK(8),5(R6)       SAVE ISCI OR PACKED ADID                     
         MVC   WORK+8(8),WORK      SAVE AT WORK+8 ALSO                          
         MVC   WORK+16(4),SPACES                                                
         TM    15(R6),CMLKSTA_PCKD   TEST ADID                                  
         BZ    FCML20                                                           
         GOTO1 VTRPACK,DMCB,(C'U',WORK),WORK+8  DECODE ADID TO WORK+8           
*                                                                               
FCML20   MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
*                                                                               
         MVC   SVCMLLEN,CMLSLN                                                  
         MVC   SVCMLOVR,CMLOVRD1   SAVE PRINT OVERRIDE 1 & 2                    
         TM    CMLSTAT,X'80'       TEST DELETED COMMERCIAL                      
         BO    FCML30                                                           
*                                                                               
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(2,UNTADTEP),(3,DUB)                                 
         FIXDT02                                                                
         GOTO1 (RF),(R1),(2,UNTADTE2),(3,DUB+3)                                 
         CLC   CMLRLSE,DUB+3       SEE IF AIR DATE FALLS IN CML PERIOD          
         BH    FCML30                                                           
         CLC   CMLRCL,DUB                                                       
         BL    FCML30                                                           
         MVC   SVCMLDS,SPACES                                                   
         MVC   SVCMLDS(15),CMLTITLE                                             
*                                                                               
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   FCML24                                                           
         MVC   SVCMLDS,3(R6)                                                    
         BRAS  RE,NEXTEL                                                        
         BNE   FCML24                                                           
         MVC   SVCMLDS2,3(R6)                                                   
         BRAS  RE,NEXTEL                                                        
         BNE   FCML24                                                           
         MVC   SVCMLDS3,3(R6)                                                   
FCML24   B     FCMLX                                                            
*                                                                               
FCML30   MVC   SVCMLDS(14),=CL14'NONE SCHEDULED'                                
         MVC   SVCMLDS+14(10),SPACES                                            
         XC    0(8,R3),0(R3)       ZERO OUT BAD COMML                           
FCMLX    DS    0H                                                               
         BRAS  RE,ADIDLIST         ADD TO ADID LIST                             
*                                                                               
         BRAS  RE,SETNET           SET TO NET                                   
         XIT1                                                                   
         EJECT                                                                  
*===========================================================                    
* ADD ISCI/ADID TO ADID LIST (IF ANY)                                           
* AND ADID/ADID SO CAN FIND IT LATER                                            
* SINCE PATTERN RECORDS CAN CONTAIN PACKED ADIDS FOR COMMERCIALS                
* THAT HAVE NO ADIDS, ADD AN ENTRY OF ADID/ADID FOR THEM TOO!                   
*===========================================================                    
                                                                                
ADIDLIST NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'A0'                                                     
         BRAS  RE,GETEL                                                         
         BE    ADIDL10                                                          
*                                                                               
         CLI   KEY+1,X'C1'         DID THIS COME FROM AN ADID POINTER           
         BNE   ADIDLX                                                           
         MVC   WORK(8),KEY+5       CREATE AN ADID/ADID ENTRY                    
         B     ADIDL12                                                          
                                                                                
* DO NOT PUT ADID IN FOR ISCI CMML UNLESS IT'S A REAL ADID                      
                                                                                
         USING CMLADIEL,R6                                                      
ADIDL10  GOTO1 VTRPACK,DMCB,(C'U',CMLADIDP),WORK+20                             
         CLI   WORK+20+8,C' '      REAL ADIDS ARE >8 CHARS                      
         BNH   ADIDLX              SO EXIT IF NOT                               
*                                                                               
ADIDL12  L     R4,AADIDTAB                                                      
         L     R0,AADIDTBX                                                      
         SR    R0,R4                                                            
         SRL   R0,4                / 16 GIVES ENTRY COUNT                       
*                                                                               
ADIDL20  DS   0H                                                                
         OC    0(8,R4),0(R4)       EMPTY                                        
         BZ    ADIDL30                                                          
         CLC   0(8,R4),WORK        SAME COMML?                                  
         BE    ADIDLX                                                           
*                                                                               
         LA    R4,16(R4)           BUMP TO NEXT ENTRY                           
         BCT   R0,ADIDL20                                                       
         DC    H'0'                MAKE TABLE BIGGER                            
*                                                                               
ADIDL30  MVC   0(8,R4),WORK        SAVE 8 CHAR CML                              
         MVC   8(8,R4),CMLADIDP    SAVE PACKED ADID                             
         CLI   0(R6),X'A0'         TEST FOUND AN ADID ELEMENT                   
         BE    *+10                                                             
         MVC   8(8,R4),WORK        IF NO ADID ELEM, REPEAT ADID                 
*                                                                               
ADIDLX   XIT1                                                                   
*                                                                               
         LTORG                                                                  
         DROP  R4,R5,R6                                                         
         EJECT                                                                  
*=========================================================                      
* INITIALIZE NETIO                                                              
*=========================================================                      
                                                                                
NETI     NTR1  BASE=*,LABEL=*                                                   
         L     R3,ANETBLK                                                       
         USING NETBLCKD,R3                                                      
*                                                                               
         LR    R0,R3               CLEAR NETBLOCK                               
         LHI   R1,NETBLKX-NETBLK                                                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    KEY,KEY             GET USER PROFILE INTO NBUSER                 
         MVC   KEY(4),=C'S0N0'                                                  
         MVC   KEY+4(2),AGENCY                                                  
         MVI   KEY+6,C'N'                                                       
         MVC   KEY+7(3),QCLT                                                    
         MVI   KEY+10,C'*'                                                      
         MVC   KEY+11(1),SVCLTOFF  CLIENT OFFICE                                
*                                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    NETI02                                                           
*                                                                               
         TM    SECFLAG,NEPRDOFF    PRDS OFFICES NOT EQ                          
         BO    NETI02                                                           
*                                                                               
         MVC   KEY+11(1),SVPRDOFF  USE PROD OFFICE                              
*                                                                               
NETI02   GOTO1 GETPROF,DMCB,KEY,NBUSER,DATAMGR GET PROFILE IN NBUSER            
*                                                                               
         MVC   NBSELAGY,AGENCY                                                  
         MVC   NBEFFAGY,AGENCY                                                  
         MVC   NBSELMED,QMED                                                    
         MVC   NBSELAM,BAGYMD                                                   
         MVC   NBSELCLI,QCLT                                                    
         MVC   NBSELCL2,BCLT                                                    
         MVC   NBSELPRD,=C'ALL'                                                 
         MVC   NBSELNET,NETWORK                                                 
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(2,BASESTR),NBSELSTR                                 
         FIXDT02                                                                
         GOTO1 (RF),(R1),(2,BASEEND),NBSELEND                                   
*                                                                               
         MVC   NBSELPRG,BASEPRG                                                 
         MVI   NBSELPST,0                                                       
         MVI   NBSELUOP,C'A'      GET ACTUALS ONLY                              
         MVI   NBDATA,C'U'         UNITS ONLY (NO PACKAGES)                     
         MVI   NBSEQ,C'Q'          GET UNITS IN PROGRAM SEQ                     
         MVI   NBMODE,NBPROCUN                                                  
         MVI   NBSELTRF,C'B'       READ BOTH NETWORK AND TRAFFIC UNITS          
         MVC   NBAIO,AIO1                                                       
*        OI    NBDMGOPT,X'08'     AND DELETED                                   
         L     R1,SYSPARMS                                                      
         L     RF,16(,R1)           COMFACS ADDRESS                             
         ST    RF,NBACOM                                                        
*                                                                               
         GOTO1 CALLOV,DMCB,0,(C'R',X'00000A27')                                 
         L     RF,0(R1)                                                         
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RF,ANETIO                                                        
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   NETI10                                                           
         TM    SVOPTSW,OPTRACE                                                  
         BZ    NETI10                                                           
         MVI   NBTRCOPT,C'Y'       TO TRACE NETIO (OFFLINE ONLY)                
         MVC   NBPRINT,VPRINT       ALSO NEEDS PRINT ADDRESS                    
*                                                                               
NETI10   DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
         DROP  R3                                                               
         EJECT                                                                  
* UPDATE REVISION RECORDS AND UPDATED UNITS *                                   
*                                                                               
UPDR     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,SETNET          SET TO NET                                    
*                                                                               
         BRAS  RE,FNREVN           FIND NETWORK REVISION NUMBER                 
*                                                                               
         CLI   UPDREVSW,C'Y'       REV REC FOUND                                
         BNE   UPDR06               NO,MUST BE ORIGINAL                         
         LLC   R1,HIREVNN                                                       
         LA    R1,1(R1)            INCREMENT NETWORK REVISION NUMBER            
         STC   R1,HIREVNN                                                       
*                                                                               
UPDR06   MVI   UPDREVSW,C'N'                                                    
         CLC   SVHIREVP,HIREVNN    PROG REV # MAY NOT BE > NET REV #            
         BNH   *+10                                                             
         MVC   HIREVNN,SVHIREVP    USE PROG REV #                               
*                                                                               
* UNLESS SOME COMMLS CHANGED OR NEWLY ASSIGNED, NO UPDATES AT ALL *             
*                                                                               
         NI    UPDSW,X'FF'-X'04'   SET NO UNITS TO UPDATE                       
         L     R5,APRGTBLE                                                      
         USING PRGTABLD,R5                                                      
         B     *+8                                                              
UPDR10   LA    R5,PRGNEXT                                                       
*                                                                               
         OC    PRGPRG,PRGPRG                                                    
         BNZ   UPDR14                                                           
         TM    UPDSW,X'04'         FIND ANY UPDATES HERE                        
         BZ    UPDRX                NO, ALL DONE                                
         B     UPDR40                                                           
*                                                                               
UPDR14   L     RF,AUNTABLE                                                      
         USING UNTABLED,RF                                                      
         TM    PRGFLG,PRGFLGNW     IS NEW REV REC NEEDED                        
         BO    UPDR20                                                           
UPDR16   CLI   SVTN1PR9,C'Y'       BYPASS ASSIGNED CMLS                         
         BNE   UPDR17                                                           
         TM    UNTFLG,UNTFLGAS     PREV ASSIGNED                                
         BO    UPDR18              BYPASS, ONLY IF CMLS WEREN'T SWAPED          
*                                                                               
UPDR17   DS    0H                                                               
         CLC   UNTPROG,PRGPRG                                                   
         BNE   UPDR19                                                           
         TM    UNTFLG,UNTFLGNW+UNTFLGCH NEW OR CHANGE                           
         BNZ   UPDR20                                                           
*                                                                               
UPDR18   DS    0H                                                               
         TM    UNTFLG1,UNTFL1DL    DELETED PATTERN ?                            
         BZ    *+8                                                              
         OI    UPDSW,X'04'         UPDATE REQUIRED                              
*                                                                               
UPDR19   TM    UNTFLG1,UNTFL1SW    CMLS WERE SWAPPED                            
         BO    UPDR20                                                           
         LA    RF,UNTNEXT          NEXT UNIT                                    
         CLI   UNTADTEP,0           ANY MORE                                    
         BNE   UPDR16                                                           
         B     UPDR10                                                           
         DROP  RF                                                               
*                                                                               
UPDR20   OI    UPDSW,X'04'         SET ON UPDATE REQUIRED                       
         MVC   SVPRGENT,PRGENT                                                  
         DROP  R5                                                               
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
*                                                                               
         BRAS  RE,SETXSP           SET TO XSPOT                                 
*                                                                               
         USING REVXKEY,R4                                                       
         MVC   REVXKID,=X'0A1D'                                                 
         MVC   REVXKAM(3),BAGYMD AND BCLT                                       
         MVC   REVXKNET,NETWORK                                                 
         MVC   REVXKPRG,SVPRG                                                   
         MVC   REVXKPER,PERIOD                                                  
         CLI   MYTN2PR6,C'W'       TRAFFIC WEEKLY                               
         BNE   *+18                                                             
         TM    SVFLAG,CONVSW       CONVERTED RECORDS?                           
         BNZ   *+10                YES, ALREADY SET                             
         MVC   REVXKPER,STDATEP                                                 
         MVC   REVXKNUM,SVPRGRVN                                                
*                                                                               
         CLC   REVXKNUM,HIREVNN                                                 
         BNH   *+6                 PRG NUM BETTER NOT BE > NET #                
         DC    H'0'                BUG CATCHER                                  
*                                                                               
         CLI   SVTN2PRO+00,C'*'    TRAFFIC BY PRODUCT                           
         BNE   UPDR22                                                           
         CLI   OPTPROD,0           RUNNING BY PRODUCT                           
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   REVXKPRD,OPTPROD                                                 
*                                                                               
UPDR22   DS   0H                                                                
         OC    OPTPRGR,OPTPRGR    ANY PRODUCT GROUP                             
         BZ    *+10                                                             
         MVC   REVXKPGR,OPTPRGR                                                 
*                                                                               
* X'10' = ADD NEW REV FOR ORIG, X'20' = INST RUN FOR LAST REV                   
*                                                                               
         TM    SVPRGFLG,X'30'                                                   
         BNZ   UPDR30                                                           
*                                                                               
         DROP  R4                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(32),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         CLC   KEY(32),0(R6)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING REVREVEL,R6                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(3,REVADATE)                                   
*                                                                               
         OC    OPTPRGR,OPTPRGR     RUNNING BY PRODUCT GROUP                     
         BZ    *+8                                                              
         OI    REVFLAG,REVPRGRP                                                 
*                                                                               
         CLI   REVDTALN,12         OLD REVISON RECORD                           
         BE    UPDR23                                                           
         MVC   REVNNUM,HIREVNN                                                  
         OI    REVFLAG,X'40'                                                    
*                                                                               
UPDR23   DS    0H                                                               
         TM    SVOPTSW,OPTPGEN     PATTERN GEN INSTR REQ THIS RUN?              
         BZ    UPDR24               NOA                                         
*                                                                               
         MVC   REVIDATE,REVADATE   SET INSTR DATE TO TODAY ALSO                 
*                                                                               
         OI    REVFLAG,REVINS+REVPAT                                            
         DROP  R6                                                               
*                                                                               
* GO BUILD NEW AUTO REVISION COMMENT ELEMENT *                                  
*                                                                               
UPDR24   DS   0H                                                                
         BRAS  RE,BACMT            BUILD AUTO SEED COMMENT                      
*                                                                               
*MNREVT                                                                         
         BRAS  RE,BTIME                                                         
*MNREVT                                                                         
*                                                                               
         MVI   ELCODE,X'50'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   *+14                                                             
         MVC   0(63,R6),ELEM                                                    
         B     UPDR28                                                           
         GOTO1 ADDELEM                                                          
*                                                                               
UPDR28   BRAS  RE,BSCMT            GO BUILD OPTIONAL SEED COMMENTS              
*                                                                               
         TM    SVOPTSW,OPTTEST     THIS A TEST RUN                              
         BO    UPDR10               NO UPDATE                                   
*                                                                               
         CLI   OFFLINE,C'Y'        IF OFFLINE                                   
         BNE   *+12                 CK UPDATE                                   
         CLI   TWAWRITE,C'Y'       WRITE OK                                     
         BNE   UPDR10               NO UPDATE                                   
*                                                                               
         GOTO1 PUTREC                                                           
         B     UPDR10                                                           
         EJECT                                                                  
* ADD NEW REVISION REC FOR NEW/CHA RECS THIS SEED *                             
*                                                                               
UPDR30   GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
* BUILD NEW REVISION RECORD *                                                   
*                                                                               
         L     R6,AIO1                                                          
         XC    0(256,R6),0(R6)                                                  
         MVC   0(32,R6),KEYSAVE                                                 
         MVC   KEY(32),KEYSAVE                                                  
         MVI   33(R6),42                                                        
*                                                                               
* BUILD NEW REVISION ELEMENT *                                                  
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING REVREVEL,R6                                                      
         MVI   REVREVEL,X'10'                                                   
         MVI   REVDTALN,(REVREVEQ-REVREVEL)                                     
         GOTO1 DATCON,DMCB,(5,0),(3,REVADATE)                                   
         MVC   REVNNUM,HIREVNN     NETWORK REVISION NUMBER                      
         OI    REVFLAG,X'40'                                                    
*                                                                               
         OC    OPTPRGR,OPTPRGR    ANY PRODUCT GROUP                             
         BZ    *+8                                                              
         OI    REVFLAG,REVPRGRP    TURN ON RAN BY PGROUP                        
*                                                                               
         CLI   MYTN2PR6,C'B'                                                    
         BNE   UPDR32                                                           
         OI    REVFLAG,REVBRD      TURN ON PERIOD IS BROAD MONTH                
         B     UPDR34                                                           
*                                                                               
UPDR32   DS    0H                                                               
         CLI   MYTN2PR6,C'C'                                                    
         BNE   UPDR34                                                           
         OI    REVFLAG,REVCAL      TURN ON PERIOD IS CALENDAR MONTH             
*                                                                               
UPDR34   DS    0H                                                               
         TM    SVOPTSW,OPTPGEN     PATTERN GEN INSTR REQ THIS RUN?              
         BZ    UPDR36               NOA                                         
*                                                                               
         MVC   REVIDATE,REVADATE   SET INSTR DATE TO TODAY ALSO                 
*                                                                               
         OI    REVFLAG,REVINS+REVPAT                                            
         DROP  R6                                                               
UPDR36   DS    0H                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
* GO BUILD NEW AUTO REVISION COMMENT ELEMENT *                                  
*                                                                               
         BRAS  RE,BACMT            BUILD AUTO SEED COMMENT                      
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
         BRAS  RE,BSCMT            GO BUILD OPTIONAL SEED COMMENTS              
*                                                                               
         TM    SVOPTSW,OPTTEST     THIS A TEST RUN                              
         BO    UPDR10              NO UPDATE                                    
*                                                                               
*MNREVT                                                                         
         BRAS  RE,BTIME                                                         
*MNREVT                                                                         
*                                                                               
         GOTO1 ADDREC                                                           
*                                                                               
         B     UPDR10                                                           
*                                                                               
         EJECT                                                                  
* GO THRU UNIT TABLE FOR UPDATED UNITS, AND UPDATE AS NEEDED *                  
*                                                                               
UPDR40   NI    UPDSW,X'FF'-X'04'   TURN OFF UNIT UPDATED                        
*                                                                               
         BRAS  RE,SETNET           SET TO UNIT FILE                             
*                                                                               
         L     R5,AUNTABLE                                                      
         USING UNTABLED,R5                                                      
*                                                                               
UPDR50   TM    UNTFLG1,UNTFL1SW    WERE CMLS SWAPPED                            
         BO    UPDR52C              YES                                         
         CLI   SVTN1PR9,C'Y'       BYPASS ASSIGNED CMLS                         
         BNE   *+12                                                             
         TM    UNTFLG,UNTFLGAS     PREV ASSIGNED                                
         BO    UPDR68               BYPASS                                      
         OC    UNTPSEQ,UNTPSEQ     PATTERN FOUND FOR THIS UNIT                  
         BNZ   UPDR52               YES                                         
*                                                                               
         OC    UNTCML1(16),UNTCML1 COMML ASSIGNED                               
         BZ    UPDR52               NO                                          
*                                                                               
         XC    UNTCML1(16),UNTCML1                                              
         XC    UNTREF,UNTREF                                                    
         OI    UNTFLG,UNTFLGCH                                                  
         B     UPDR54                                                           
*                                                                               
UPDR52   TM    UNTFLG1,UNTFL1DL    DELETED PATTERN ?                            
         BO    UPDR52C              YES                                         
         TM    UNTFLG,UNTFLGNW+UNTFLGCH NEW OR CHANGED COMMLS                   
         BZ    UPDR68                                                           
*                                                                               
UPDR52C  L     RF,AUNTABLE                                                      
         CR    RF,R5               AT START OF TABLE                            
         BE    UPDR54               YES                                         
*                                                                               
* BACK UP TO FIRST ENTRY FOR THIS UNIT *                                        
*                                                                               
UPDR53   LR    RF,R5                                                            
         SH    RF,=AL2(UNTNEXT-UNTENT)                                          
         CLC   UNTDSKAD,UNTDSKAD-UNTENT(RF) PREV UNIT SAME REC                  
         BNE   UPDR54                        NO                                 
         SH    R5,=AL2(UNTNEXT-UNTENT)                                          
         B     UPDR53                                                           
*                                                                               
UPDR54   L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 DATAMGR,DMCB,(X'88',=C'GETREC'),=C'UNTFIL',             C        
               UNTDSKAD,(R6),DMWORK                                             
*                                                                               
         TM    UNTFLG1,UNTFL1SW    WERE CMLS SWAPPED                            
         BO    *+20                 YES                                         
         CLI   SVTN1PR9,C'Y'       BYPASS ASSIGNED CMLS                         
         BNE   *+12                                                             
         TM    UNTFLG,UNTFLGAS     PREV ASSIGNED                                
         BO    UPDR61               BYPASS                                      
*                                                                               
         L     R6,AIO1                                                          
         CLI   18(R6),C'A'         SKED?                                        
         BNL   TEMP01              NO X'99' ELEM FOR SKED                       
*                                                                               
*** SEE THAT THERE IS X'99' ELEM                                                
*                                                                               
         MVI   TESTBYTE,0                                                       
         MVI   ELCODE,X'99'                                                     
         LA    R6,27(,R6)                                                       
         BRAS  RE,NEXTEL                                                        
         BE    TEMP01                                                           
         MVI   TESTBYTE,1          NO X'99' ELEM                                
TEMP01   DS    0H                                                               
*                                                                               
         L     R6,AIO1                                                          
*                                                                               
         MVI   ELCODE,X'21'                                                     
         USING NUCMLEL,R6                                                       
*                                                                               
         TM    UNTFLG1,UNTFL1DL    DELETED PATTERN ?                            
         BZ    UPDR54B              NO                                          
*                                                                               
         LA    R6,27(,R6)                                                       
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                SHOULD NOT BE                                
*                                                                               
         XC    NUCML1(16),NUCML1   CLEAR COMMERCIALS                            
         NI    NUCMADFL,X'FF'-NUCMADF1-NUCMADF2 INIT AD-ID FLAG                 
         XC    NUCMLR3F,NUCMLR3F        PAT REF NUMBER                          
         MVI   NUCMLKEY,0               AND PAT KEY                             
         OI    UPDSW,X'04'         NEED TO UPDATE                               
         B     UPDR61                                                           
*                                                                               
UPDR54B  LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         GOTO1 REMELEM                                                          
*                                                                               
         MVI   NUCMLEL,X'21'                                                    
         MVI   NUCMLELN,NUCMLEND-NUCMLEL                                        
         MVC   NUCMLREV,SVPRGRVN                                                
*                                                                               
* CHK FOR NEWLY ASSIGNED CMLS                                                   
* SPECIAL CASE : IF CML HAS BEEN DELETED - FLAG IS ON                           
*                                                                               
         MVC   HALF(1),NUCMLFLG                                                 
         NI    HALF,X'E0'                                                       
         LLC   R0,HALF                                                          
         SRL   R0,4                                                             
         STC   R0,HALF+1                                                        
*                                                                               
         TM    UNTFLG1,UNTFL1FD    FEED NO NATIONAL?                            
         BZ    *+12                                                             
         OI    NUCMLFL2,NUCMLFFD    YES                                         
         B     UPDR56                                                           
*                                                                               
         OC    NUCML1(16),NUCML1   IF THERE WAS NO CML                          
         BNZ   UPDR54D                                                          
         OC    UNTCML1(16),UNTCML1 BUT THERE IS ONE NOW                         
         BZ    UPDR54F                                                          
         TM    NUCMLFL2,NUCMLNEW   AND THE FLAG WAS OFF                         
         BO    *+12                                                             
         OI    NUCMLFL2,NUCMLNEW   THEN ITS A NEW CML                           
         B     UPDR54F                                                          
         NI    NUCMLFL2,X'FF'-NUCMLNEW                                          
         B     UPDR54F                                                          
*                                                                               
*                                  THERE WAS A CML                              
UPDR54D  OC    UNTCML1(16),UNTCML1 AND THERE IS ONE NOW                         
         BZ    UPDR54E                                                          
         TM    NUCMLFL2,NUCMLNEW   AND NEW FLAG WAS TURNED ON                   
         BZ    UPDR54F                                                          
         CLC   UNTREVN,SVPRGRVN    AT DIFFERENT REVISION                        
         BE    UPDR54F                                                          
         NI    NUCMLFL2,X'FF'-NUCMLNEW THEN TURN OFF THE FLAG                   
         B     UPDR54F                                                          
*                                                                               
* THERE WAS A CML BUT NOW ITS DELETED AND IF REVISION # CHANGED                 
* THEN TURN ON NEWLY ASGND FLAG                                                 
*                                                                               
UPDR54E  CLC   UNTREVN,SVPRGRVN    IF REV IS SAME                               
         BNE   *+12                                                             
         TM    NUCMLFL2,NUCMLNEW   AND NEW FLAG IS OFF                          
         BO    *+12                                                             
         OI    NUCMLFL2,NUCMLNEW  THEN TURN ON NEW FLAG(FAKE NEW ASGN)          
         B     UPDR54F                                                          
         NI    NUCMLFL2,X'FF'-NUCMLNEW                                          
*                                                                               
UPDR54F  OC    UNTCML1(16),UNTCML1 ANY CMLS                                     
         BNZ   UPDR54H                                                          
*                                                                               
         CLI   SVTN1PR9,C'Y'       BYPASS PREV ASSIGNED CMLS                    
         BNE   UPDR54G                                                          
*                                                                               
         CLC   =C'H7',AGENCY       IF NOT H7                                    
         BNE   UPDR56              HONOR BYPASS PREV ASGN CML                   
*                                                                               
         TM    UNTFLG,UNTFLGRE     NEED REASSIGN                                
         BZ    UPDR56               NO                                          
*                                                                               
UPDR54G  XC    NUCML1(16),NUCML1   CLEAR COMMERCIALS                            
         NI    NUCMADFL,X'FF'-NUCMADF1-NUCMADF2 INIT AD-ID FLAG                 
         XC    NUCMLR3F,NUCMLR3F        PAT REF NUMBER                          
         MVI   NUCMLKEY,0               AND PAT KEY                             
         OI    UPDSW,X'04'         NEED TO UPDATE                               
         B     UPDR56                                                           
*                                                                               
UPDR54H  NI    NUCMADFL,X'FF'-NUCMADF1-NUCMADF2 INIT AD-ID FLAG                 
*                                                                               
         XC    WORK(16),WORK                                                    
         MVC   WORK(8),UNTCML1                                                  
         BRAS  RE,RADID            REPLACE ISCI WITH AD-ID                      
*                                                                               
         MVC   NUCML1,WORK                                                      
         OC    WORK+8(8),WORK+8                                                 
         BZ    UPDR54J                                                          
         MVC   NUCML1,WORK+8       MOVE IN ADID                                 
         OI    NUCMADFL,NUCMADF1   TURN ON FLAG AD-ID CML1                      
*                                                                               
UPDR54J  OC    UNTCML2,UNTCML2                                                  
         BZ    UPDR56                                                           
*                                                                               
         XC    WORK(16),WORK                                                    
         MVC   WORK(8),UNTCML2                                                  
         BRAS  RE,RADID            REPLACE ISCII WITH AD-ID                     
*                                                                               
         MVC   NUCML2,WORK                                                      
         OC    WORK+8(8),WORK+8                                                 
         BZ    UPDR56                                                           
         MVC   NUCML2,WORK+8       MOVE IN ADID                                 
         OI    NUCMADFL,NUCMADF2   TURN ON FLAG AD-ID CML2                      
*                                                                               
UPDR56   CLC   =C'H7',AGENCY                                                    
         BNE   UPDR56C                                                          
*                                                                               
         CLI   SVTN1PR9,C'Y'       BYPASS PREV ASSIGNED CMLS                    
         BNE   *+12                                                             
         TM    UNTFLG,UNTFLGRE     NEED REASSIGN                                
         BZ    UPDR60               NO                                          
*                                                                               
UPDR56C  TM    UNTFLG,UNTFLGCS                                                  
         BO    UPDR56E                                                          
         MVI   NUCMLPRD,0          UNIT IS NOT C/S, CLEAR C/S PRD               
         XC    NUCMPROD,NUCMPROD   UNIT IS NOT C/S, CLEAR C/S PRD               
*                                                                               
* RESET PRD/DATE/SPOT LENGTH FLAGS *                                            
*                                                                               
UPDR56E  DS    0H                                                               
*                                                                               
         NI    NUCMLFLG,X'FF'-X'80'-X'40'-X'20'                                 
         MVI   NUCMLKEY,0                                                       
         XC    NUCMLR3F,NUCMLR3F                                                
*                                                                               
         OC    UNTPSEQ,UNTPSEQ     NO PATTERN ASSIGNED                          
         BZ    UPDR60                                                           
         TM    UNTFLG1,UNTFL1FD    FEED NO NATIONAL?                            
         BO    UPDR60                                                           
         SR    RE,RE                                                            
         ICM   RE,3,UNTPSEQ        GET PATTERN SEQ #                            
         BCTR  RE,0                                                             
         MH    RE,=AL2(PATNEXT-PATENT)                                          
         A     RE,APATABLE                                                      
         MVC   NUCMLR3F,PATSEQNO-PATENT(RE)                                     
         CLI   PATYPE-PATENT(RE),PATQNET   NET PAT                              
         BE    UPDR60                                                           
         CLI   PATYPE-PATENT(RE),0         NETWORK SPECIFIC PATTERN             
         BE    UPDR60                                                           
         CLI   PATYPE-PATENT(RE),PATQALL   ALL NET PAT                          
         BNE   *+12                                                             
         OI    NUCMLKEY,X'10'      SET TO ALL NETS                              
         B     UPDR60                                                           
*                                                                               
         CLI   PATYPE-PATENT(RE),PATQMED   MEDIA SPECIFIC                       
         BNE   *+12                                                             
         OI    NUCMLKEY,X'08'      SET TO MEDIA                                 
         B     UPDR60                                                           
*                                                                               
         CLI   PATYPE-PATENT(RE),PATQDPMD  DAYPART & MEDIA SPECIFIC             
         BNE   *+12                                                             
         OI    NUCMLKEY,X'28'      SET TO MEDIA & DAYPART                       
         B     UPDR60                                                           
*                                                                               
         CLI   PATYPE-PATENT(RE),PATQDPT   DAYPART SPECIFIC                     
         BNE   *+12                                                             
         OI    NUCMLKEY,X'20'                                                   
         B     UPDR60                                                           
*                                                                               
         CLI   PATYPE-PATENT(RE),PATQPROG  PROG SPECIFIC                        
         BNE   *+12                                                             
         OI    NUCMLKEY,X'80'                                                   
         B     UPDR60                                                           
*                                                                               
         CLI   PATYPE-PATENT(RE),PATQFEED  FEED ONLY                            
         BNE   *+12                                                             
         OI    NUCMLKEY,X'40'                                                   
         B     UPDR60                                                           
*                                                                               
         CLI   PATYPE-PATENT(RE),PATQDPFD  FEED AND DAYPART                     
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    NUCMLKEY,X'60'                                                   
*                                                                               
UPDR60   GOTO1 ADDELEM                                                          
         OI    UPDSW,X'04'         TURN ON UNIT UPDATED                         
*                                                                               
UPDR61   L     R6,AIO1                                                          
         LA    R6,27(,R6)                                                       
         MVI   ELCODE,X'23'                                                     
         TM    UNTFLG1,UNTFL1FD    FEED NO NATIONAL?                            
         BO    *+18                 YES                                         
UPDR64   CLC   UNTDSKAD,UNTDSKAD+L'UNTENT SAME UNIT                             
         BNE   UPDR70                                                           
         LA    R5,UNTNEXT          NEXT UNIT                                    
         OC    UNTFEED,UNTFEED     MUST BE FEED                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   =C'??',UNTFEED      THIS JUST FOR INFO                           
         BNE   UPDR66                                                           
         TM    UNTFLG,UNTFLGCS     MUST BE COPY SPLIT                           
         BO    UPDR64                                                           
         DC    H'0'                                                             
*                                                                               
UPDR66   BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NUFDCEL,R6                                                       
         TM    NUFDCFL2,X'80'      DELETED FEED                                 
         BO    UPDR66                                                           
         CLC   UNTFEED,NUFDCFED                                                 
         BH    UPDR66                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   1(R6),NUFDCELN      NEW ELEM LEN=X'53'                           
         BE    UPDR66B                                                          
*                                                                               
         LLC   RE,1(R6)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8              SAVE ELEMENT                                 
         B     *+10                                                             
         MVC   ELEM(0),0(R6)                                                    
*                                                                               
         XC    DMCB(24),DMCB                                                    
         GOTO1 VRECUP,DMCB,(C'U',AIO),(R6) DELETE OLD ELEM                      
*                                                                               
         LA    R6,ELEM                                                          
         USING NUFDCEL,R6                                                       
         MVI   NUFDCEL,X'23'                                                    
         MVI   NUFDCLEN,NUFDCELN                                                
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
*NOP     XC    DMCB(24),DMCB                                                    
******   GOTO1 VRECUP,DMCB,(C'U',AIO),(R6) ADD BIGGER ELEMENT                   
*                                                                               
UPDR66B  CLI   SVTN1PR9,C'Y'       BYPASS ASSIGNED CMLS                         
         BE    UPDR66C              YES                                         
         TM    UNTFLG1,UNTFL1DL    DELETED PATTERN ?                            
         BZ    UPDR66C              NO                                          
*                                                                               
         XC    NUFDCML1(16),NUFDCML1              CLEAR COMMERCIALS             
         NI    NUFDADFL,X'FF'-NUFDADF1-NUFDADF2   CLEAR ADID FLAGS              
         XC    NUFDCR3F,NUFDCR3F        PAT REF NUMBER                          
         MVI   NUFDCKEY,0               AND PAT KEY                             
*                                                                               
         OI    UPDSW,X'04'         NEED TO UPDATE                               
         B     UPDR64                                                           
*                                                                               
* CHK FOR NEWLY ASSIGNED CMLS                                                   
* SPECIAL CASE : IF CML HAS BEEN DELETED - FLAG IS ON                           
*                                                                               
UPDR66C  DS    0H                                                               
         OC    NUFDCML1(16),NUFDCML1 IF THERE WAS NO CML                        
         BNZ   UPDR66D                                                          
         OC    UNTCML1(16),UNTCML1 BUT THERE IS ONE NOW                         
         BZ    UPDR66G                                                          
         TM    NUFDCFL3,NUFDCNEW   AND THE FLAG IS OFF                          
         BO    *+12                                                             
         OI    NUFDCFL3,NUFDCNEW   THEN ITS NEW CML                             
         B     UPDR66F                                                          
         NI    NUFDCFL3,X'FF'-NUFDCNEW                                          
         B     UPDR66F                                                          
*                                                                               
*                                  THERE WAS A CML                              
UPDR66D  OC    UNTCML1(16),UNTCML1 AND THERE IS ONE NOW                         
         BZ    UPDR66E                                                          
         TM    NUFDCFL3,NUFDCNEW                                                
         BZ    UPDR66G                                                          
         CLC   UNTREVN,SVPRGRVN    AT DIFFERENT REVISION                        
         BE    UPDR66G                                                          
         NI    NUFDCFL3,X'FF'-NUFDCNEW THEN TURN OFF THE FLAG                   
         B     UPDR66F                                                          
*                                                                               
* THERE WAS A CML BUT NOW ITS DELETED AND IF REVISION # CHANGED                 
* THEN TURN ON NEWLY ASGND FLAG                                                 
*                                                                               
UPDR66E  CLC   UNTREVN,SVPRGRVN    IF REV IS SAME                               
         BNE   *+12                                                             
         TM    NUFDCFL3,NUFDCNEW   AND THE FLAG IS OFF                          
         BO    *+12                                                             
         OI    NUFDCFL3,NUCMLNEW  THEN TURN ON NEW FLAG(FAKE NEW ASGN)          
         B     UPDR66F                                                          
         NI    NUFDCFL3,X'FF'-NUCMLNEW                                          
UPDR66F  OI    UPDSW,X'04'                                                      
*                                                                               
UPDR66G  TM    UNTFLG,UNTFLGCS                                                  
         BO    UPDR66K                                                          
*                                                                               
         OC    NUFDPROD,NUFDPROD                                                
         BNZ   UPDR66J                                                          
*                                                                               
         CLI   NUFDCPRD,0                                                       
         BE    UPDR66K                                                          
*                                                                               
UPDR66J  MVI   NUFDCPRD,0          UNIT IS NOT C/S, CLEAR C/S                   
         XC    NUFDPROD,NUFDPROD   UNIT IS NOT C/S, CLEAR C/S                   
         OI    UPDSW,X'04'         TURN ON UNIT UPDATED                         
*                                                                               
UPDR66K  TM    UNTFLG1,UNTFL1SW    WERE CMLS SWAPPED                            
         BO    UPDR67               YES                                         
         CLI   SVTN1PR9,C'Y'       BYPASS ASSIGNED CMLS                         
         BNE   *+12                                                             
         TM    UNTFLG,UNTFLGAS     PREV ASSIGNED                                
         BO    UPDR64               BYPASS                                      
*                                                                               
         TM    UNTFLG,UNTFLGNW+UNTFLGCH NEW OR CHANGED COMMLS                   
         BZ    UPDR64                                                           
*                                                                               
UPDR67   DS   0H                                                                
         OC    UNTCML1(16),UNTCML1 ANY CMLS                                     
         BZ    UPDR67C                                                          
*                                                                               
         NI    NUFDADFL,X'FF'-NUFDADF1-NUFDADF2   CLEAR ADID FLAGS              
         XC    WORK(16),WORK                                                    
*                                                                               
         OC    UNTCML1,UNTCML1                                                  
         BZ    UPDR67B                                                          
*                                                                               
         MVC   WORK(8),UNTCML1                                                  
         BRAS  RE,RADID            REPLACE ISCI WITH AD-ID                      
*                                                                               
         OC    WORK+8(8),WORK+8                                                 
         BZ    UPDR67B                                                          
         MVC   UNTCML1,WORK+8      MOVE IN ADID                                 
         OI    NUFDADFL,NUFDADF1   TURN ON FLAG AD-ID CML1                      
*                                                                               
         XC    WORK(16),WORK                                                    
*                                                                               
UPDR67B  OC    UNTCML2,UNTCML2                                                  
         BZ    UPDR67C                                                          
*                                                                               
         MVC   WORK(8),UNTCML2                                                  
         BRAS  RE,RADID            REPLACE ISCI WITH AD-ID                      
*                                                                               
         OC    WORK+8(8),WORK+8                                                 
         BZ    UPDR67C                                                          
         MVC   UNTCML2,WORK+8      MOVE IN ADID                                 
         OI    NUFDADFL,NUFDADF2   TURN ON FLAG AD-ID CML2                      
*                                                                               
         XC    WORK(16),WORK                                                    
*                                                                               
UPDR67C  CLC   NUFDCML1,UNTCML1                                                 
         BE    UPDR67D                                                          
         TM    UNTFLG1,UNTFL1SW    WERE CMLS SWAPPED                            
         BZ    *+14                                                             
         CLC   NUFDCML1,UNTCML2                                                 
         BE    UPDR67D                                                          
         OC    NUFDCTFL,HALF                                                    
         OI    NUFDCTFL,NUFDCTF1                                                
*                                                                               
UPDR67D  CLC   NUFDCML2,UNTCML2                                                 
         BE    UPDR67F                                                          
         TM    UNTFLG1,UNTFL1SW    WERE CMLS SWAPPED                            
         BZ    *+14                                                             
         CLC   NUFDCML2,UNTCML1                                                 
         BE    UPDR67F                                                          
         OC    NUFDCTFL,HALF+1                                                  
         OI    NUFDCTFL,NUCMLTF2                                                
*                                                                               
UPDR67F  OI    UPDSW,X'04'         TURN ON UNIT UPDATED                         
*                                                                               
         MVC   NUFDCML1(16),UNTCML1                                             
         MVC   NUFDCREV,SVPRGRVN                                                
         MVI   NUFDCKEY,0                                                       
*                                                                               
         OC    UNTPSEQ,UNTPSEQ     PATTERN ASSIGNED?                            
         BZ    UPDR64               NO, GO ON TO NEXT                           
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,UNTPSEQ        GET PATTERN SEQ #                            
         BCTR  RE,0                                                             
         MHI   RE,PATNEXT-PATENT                                                
         A     RE,APATABLE                                                      
         MVC   NUFDCR3F,PATSEQNO-PATENT(RE)                                     
*                                                                               
         CLI   PATYPE-PATENT(RE),PATQNET   NET PAT                              
         BE    UPDR64                                                           
         CLI   PATYPE-PATENT(RE),PATQDPT   DAYPART SPECIFIC                     
         BNE   *+12                                                             
         OI    NUFDCKEY,X'20'                                                   
         B     UPDR64                                                           
*                                                                               
         CLI   PATYPE-PATENT(RE),PATQPROG  PROG SPECIFIC                        
         BNE   *+12                                                             
         OI    NUFDCKEY,X'80'                                                   
         B     UPDR64                                                           
*                                                                               
         CLI   PATYPE-PATENT(RE),PATQFEED  FEED ONLY                            
         BNE   *+12                                                             
         OI    NUFDCKEY,X'40'                                                   
         B     UPDR64                                                           
*                                                                               
         CLI   PATYPE-PATENT(RE),PATQDPFD  FEED AND DAYPART                     
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    NUFDCKEY,X'60'                                                   
         B     UPDR64                                                           
*                                                                               
UPDR68   L     RF,AUNTABLE                                                      
         CR    RF,R5               AT START OF TABLE                            
         BE    UPDR68C              YES                                         
*                                                                               
* BACK UP TO FIRST ENTRY FOR THIS UNIT *                                        
*                                                                               
UPDR68B  LR    RF,R5                                                            
         SH    RF,=AL2(UNTNEXT-UNTENT)                                          
         CLC   UNTDSKAD,UNTDSKAD-UNTENT(RF) PREV UNIT SAME REC                  
         BNE   UPDR68C                       NO                                 
         SH    R5,=AL2(UNTNEXT-UNTENT)                                          
         B     UPDR68B                                                          
*                                                                               
* CLEAR COPY SPLIT PRD AS NEEDED                                                
*                                                                               
UPDR68C  L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 DATAMGR,DMCB,(X'88',=C'GETREC'),=C'UNTFIL',             C        
               UNTDSKAD,(R6),DMWORK                                             
*                                                                               
         LA    R6,27(R6)                                                        
*                                                                               
         TM    UNTFLG,UNTFLGCS     IS THE UNIT COPY SPLIT                       
         BO    UPDR61               YES, BYPASS                                 
*                                                                               
         MVI   ELCODE,X'21'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   UPDR61                                                           
*                                                                               
         TM    UNTFLG1,UNTFL1FD    FEED NO NATIONAL?                            
         BO    UPDR68E                                                          
*                                                                               
         USING NUCMLEL,R6                                                       
*                                                                               
* CHK FOR NEWLY ASSIGNED CMLS                                                   
* SPECIAL CASE : IF CML HAS BEEN DELETED - FLAG IS ON                           
*                                                                               
         DS    0H                                                               
         OC    NUCML1(16),NUCML1   IF THERE WAS NO CML                          
         BNZ   UPDR68D                                                          
         OC    UNTCML1(16),UNTCML1 BUT THERE IS ONE NOW                         
         BZ    UPDR68G                                                          
         TM    NUCMLFL2,NUCMLNEW   AND THE FLAG WAS OFF                         
         BO    *+12                                                             
         OI    NUCMLFL2,NUCMLNEW   THEN ITS A NEW CML                           
         B     UPDR68F                                                          
         NI    NUCMLFL2,X'FF'-NUCMLNEW                                          
         B     UPDR68F                                                          
*                                                                               
*                                  THERE WAS A CML                              
UPDR68D  OC    UNTCML1(16),UNTCML1 AND THERE IS ONE NOW                         
         BZ    UPDR68E                                                          
         TM    NUCMLFL2,NUCMLNEW   AND NEW FLAG WAS TURNED ON                   
         BZ    UPDR68G                                                          
         CLC   UNTREVN,SVPRGRVN    AT DIFFERENT REVISION                        
         BE    UPDR68G                                                          
         NI    NUCMLFL2,X'FF'-NUCMLNEW THEN TURN OFF THE FLAG                   
         B     UPDR68F                                                          
*                                                                               
* THERE WAS A CML BUT NOW ITS DELETED AND IF REVISION # CHANGED                 
* THEN TURN ON NEWLY ASGND FLAG                                                 
*                                                                               
UPDR68E  CLC   UNTREVN,SVPRGRVN    IF REV IS SAME                               
         BNE   *+12                                                             
         TM    NUCMLFL2,NUCMLNEW   AND NEW FLAG IS OFF                          
         BO    *+12                                                             
         OI    NUCMLFL2,NUCMLNEW  THEN TURN ON NEW FLAG(FAKE NEW ASGN)          
         B     UPDR68F                                                          
         NI    NUCMLFL2,X'FF'-NUCMLNEW                                          
UPDR68F  OI    UPDSW,X'04'                                                      
*                                                                               
UPDR68G  DS    0H                                                               
         OC    NUCMPROD,NUCMPROD                                                
         BNZ   UPDR68K                                                          
         CLI   NUCMLPRD,0                                                       
         BE    UPDR61                                                           
UPDR68K  MVI   NUCMLPRD,0                                                       
         XC    NUCMPROD,NUCMPROD                                                
         OI    UPDSW,X'04'         TURN ON UNIT UPDATED                         
         B     UPDR61                                                           
*                                                                               
         DROP  R6                                                               
*                                                                               
UPDR70   TM    SVOPTSW,OPTTEST     THIS A TEST RUN                              
         BO    UPDR80               YES, NO UPDATE                              
*                                                                               
         TM    UPDSW,X'04'         WAS UNIT UPDATED                             
         BZ    UPDR80               NO, NO NEED TO PUTREC                       
         CLI   OFFLINE,C'Y'        IF OFFLINE                                   
         BNE   *+12                 CK UPDATE                                   
         CLI   TWAWRITE,C'Y'       WRITE OK                                     
         BNE   UPDR80               NO, NO UPDATE                               
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING ACTVD,R6                                                         
         MVI   ELCODE,X'F1'        ACTIVITY ELEMENT                             
         GOTO1 REMELEM                                                          
*                                                                               
         GOTO1 DATCON,(R1),(5,0),(3,ACTVCHDT) RECORD CHANGED DATE               
*                                                                               
         MVI   ACTVEL,X'F1'                                                     
         MVI   ACTVLEN,20                                                       
         OC    ACTVADDT,ACTVADDT   DATE ADDED THE RECORD ?                      
         BNZ   *+10                 YES, DO NOT OVERWRITE IT                    
         MVC   ACTVADDT,ACTVCHDT   TODAY'S DATE (DATE ADDED)                    
*                                                                               
         OC    ACTVADID,ACTVADID   ANY ID THAT ADDED THE RECORD                 
         BNZ   *+10                 YES,DO NOT OVERWRITE                        
         MVC   ACTVADID,TWAORIG    USER ID THAT ADDED RECORD                    
         MVC   ACTVCHID,TWAORIG    USER ID THAT CHANGED REC                     
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
         L     R6,AIO1                                                          
         CLI   18(R6),C'A'         SKED?                                        
         BNL   TEMP02              NO X'99' ELEM ON SKEDS                       
*                                                                               
*                                                                               
*** SEE IF X'99' ELEM IS STILL THERE                                            
*                                                                               
         MVI   ELCODE,X'99'                                                     
         LA    R6,27(,R6)                                                       
         BRAS  RE,NEXTEL                                                        
         BE    TEMP02                                                           
         CLI   TESTBYTE,1          THERE WAS NO X'99' ELEM BEFORE               
         BE    *+6                                                              
         DC    H'0'                WE CREATED A BAD RECORD                      
TEMP02   DS    0H                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'08',=C'PUTREC'),=C'UNTFIL',             C        
               UNTDSKAD,AIO1,DMWORK                                             
*                                                                               
UPDR80   LA    R5,UNTNEXT          NEXT UNIT                                    
         CLI   UNTADTEP,0           ANY MORE                                    
         BE    UPDR90                                                           
         NI    UPDSW,X'FF'-X'04'   TURN OFF UNIT UPDATED                        
         CLC   UNTPROG,SVPRG                                                    
         BE    UPDR50                                                           
         L     RF,APRGTBLE                                                      
         USING PRGTABLD,RF                                                      
UPDR84   CLC   UNTPROG,PRGPRG                                                   
         BE    UPDR86                                                           
         LA    RF,PRGNEXT                                                       
         OC    PRGPRG,PRGPRG                                                    
         BNZ   UPDR84                                                           
         DC    H'0'                                                             
*                                                                               
UPDR86   MVC   SVPRGENT,PRGENT                                                  
         B     UPDR50                                                           
         DROP  RF                                                               
*                                                                               
UPDR90   DS    0H                                                               
*                                                                               
UPDR95   BRAS  RE,SETSPOT          SET TO SPOT FILES                            
UPDRX    XIT1                                                                   
         LTORG                                                                  
         DROP  R5                                                               
         EJECT                                                                  
* FIND HIGHEST NETWORK REVISION NUMBER *                                        
*                                                                               
FNREVN   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   HIREVNN,0                                                        
         BRAS  RE,SETXSP           SET TO XSPOT                                 
*                                                                               
         L     R5,APRGTBLE                                                      
         USING PRGTABLD,R5                                                      
         B     *+8                                                              
FNREVN05 LA    R5,PRGNEXT                                                       
*                                                                               
         OC    PRGPRG,PRGPRG                                                    
         BZ    XFNREVN                                                          
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              WAS KEYSAVE                                  
         USING REVXKEY,R4                                                       
*                                                                               
         MVC   REVXKID,=X'0A1D'                                                 
         MVC   REVXKAM(3),BAGYMD AND BCLT                                       
         MVC   REVXKNET(4),NETWORK                                              
         MVC   REVXKPRG(6),PRGPRG                                               
         MVC   REVXKPER,PERIOD                                                  
*                                                                               
         CLI   MYTN2PR6,C'W'       USING WEEKLY PERIOD                          
         BNE   *+18                                                             
         TM    SVFLAG,CONVSW       CONVERTED RECORDS?                           
         BNZ   *+10                YES, ALREADY SET                             
         MVC   REVXKPER,STDATEP                                                 
*                                                                               
         CLI   SVTN2PRO+00,C'*'     TRAFFIC BY PRODUCT                          
         BNE   FNREV08                                                          
         CLI   OPTPROD,0            RUNNING BY PRODUCT                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   REVXKPRD,OPTPROD                                                 
*                                                                               
FNREV08  OC    OPTPRGR,OPTPRGR      RUNNING BY PRODUCT GROUP    GROUP           
         BZ    *+10                                                             
         MVC   REVXKPGR(2),OPTPRGR                                              
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(REVXKNUM-REVXKEY),KEYSAVE                                    
         BNE   FNREVN05                                                         
*                                                                               
         CLI   SVTN2PRO+00,C'*'    TRAFFIC BY PRODUCT                           
         BNE   FNREVN10                                                         
*                                                                               
         CLI   OPTPROD,0           RUNNING BY PRODUCT                           
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   REVXKPRD,OPTPROD                                                 
         BNE   FNREVN05                                                         
*                                                                               
FNREVN10 OC    OPTPRGR,OPTPRGR    RUNNING BY PRODUCT GROUP                      
         BZ    FNREVN12                                                         
         CLC   REVXKPGR(2),OPTPRGR                                              
         BNE   FNREVN05                                                         
*                                                                               
FNREVN12 L     R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         CLC   KEY(32),0(R6)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING REVREVEL,R6                                                      
         CLI   REVDTALN,12         OLD REV REC                                  
         BE    FNREVN14                                                         
*                                                                               
         MVI   UPDREVSW,C'N'       RESET INSTR RUN FOR THIS                     
*                                                                               
         TM    REVFLAG,REVINS+REVCAB INST RUN FOR THIS                          
         BZ    *+8                  NO                                          
         MVI   UPDREVSW,C'Y'                                                    
*                                                                               
         CLC   HIREVNN,REVNNUM                                                  
         BNL   FNREVN14                                                         
         MVC   HIREVNN,REVNNUM     SAVE REV NET NUM                             
*                                                                               
FNREVN14 DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
         CLC   KEY(REVXKNUM-REVXKEY),KEYSAVE                                    
         BNE   FNREVN05                                                         
*                                                                               
         CLI   SVTN2PRO+00,C'*'    TRAFFIC BY PRODUCT                           
         BNE   FNREVN20                                                         
*                                                                               
         CLI   OPTPROD,0           RUNNING BY PRODUCT                           
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   REVXKPRD,OPTPROD                                                 
         BNE   FNREVN05                                                         
*                                                                               
FNREVN20 OC    OPTPRGR,OPTPRGR    RUNNING BY PRODUCT GROUP                      
         BZ    FNREVN12                                                         
         CLC   REVXKPGR(2),OPTPRGR                                              
         BE    FNREVN12                                                         
         B     FNREVN05                                                         
*                                                                               
XFNREVN  DS    0H                                                               
         CLC   HIREVNN,OREVNN                                                   
         BNL   *+10                                                             
         MVC   HIREVNN,OREVNN      SAVE HIGHEST NET REV #                       
         XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
*MNREVT                                                                         
* BUILD/UPDATE TIME ELEMENT                                                     
*                                                                               
BTIME    NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,SETXSP           SET TO XSPOT                                 
         L     R6,AIO                                                           
         MVI   ELCODE,X'70'                                                     
         BRAS  RE,GETEL                                                         
         BE    BTIM20                                                           
                                                                                
         XC    ELEM,ELEM                                                        
         USING REVTMEL,R6                                                       
         LA    R6,ELEM                                                          
         USING REVTMEL,R6                                                       
         MVI   REVTMEL,X'70'                                                    
         MVI   REVTMLN,REVTMEQ                                                  
         EDIT  (TIME,NOW),(8,WORK)                                              
         MVC   REVTIME(2),WORK                                                  
         MVC   REVTIME+2(2),WORK+3                                              
         MVC   REVTIME+4(2),WORK+6                                              
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(5,0),(2,REVDATE)                                    
         MVI   REVTPG,REVNSED                                                   
         OC    OPTDATE,OPTDATE                                                  
         BZ    BTIM10                                                           
         MVC   REVDTOV,OPTDATE                                                  
         MVC   REVDTOVR,REVDATE                                                 
                                                                                
BTIM10   GOTO1 ADDELEM                                                          
         B     BTIMXIT                                                          
                                                                                
BTIM20   DS    0H                                                               
         MVC   REVTIME2,REVTIME1                                                
         MVC   REVDATE2,REVDATE1                                                
         MVC   REVTPG2,REVTPG1                                                  
         MVC   REVTIME1,REVTIME                                                 
         MVC   REVDATE1,REVDATE                                                 
         MVC   REVTPG1,REVTPG                                                   
         EDIT  (TIME,NOW),(8,WORK)                                              
         MVC   REVTIME(2),WORK                                                  
         MVC   REVTIME+2(2),WORK+3                                              
         MVC   REVTIME+4(2),WORK+6                                              
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(5,0),(2,REVDATE)                                    
         MVI   REVTPG,REVNSED                                                   
         OC    OPTDATE,OPTDATE                                                  
         BZ    BTIM30                                                           
         MVC   REVDTOV,OPTDATE                                                  
         MVC   REVDTOVR,REVDATE                                                 
                                                                                
BTIM30   DS    0H                                                               
                                                                                
BTIMXIT  XIT1                                                                   
*MNREVT                                                                         
* BUILD NEW REVISION COMMENT ELEMENT *                                          
*                                                                               
BACMT    NTR1  BASE=*,LABEL=*                                                   
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING REVCMTEL,R6                                                      
         MVI   REVCMTEL,X'50'                                                   
         MVI   REVCMTLN,63                                                      
         MVI   REVNUMB,0                                                        
         MVC   REVCMT,SPACES                                                    
*        MVC   REVCMT(40),=CL40'SEED ASSIGNED XXX NEW, XXX CHGD COMMLS'         
         MVC   REVCMT(37),=CL37'SEED ASGND XXX NEW, XXX CHGD, XXX PRE'          
         MVC   REVCMT+37(15),=CL13' ASGND COMMLS'                               
*                                                                               
* EDIT NEWLY ASSIGNED CMLS AND CHANGED CMLS *                                   
*                                                                               
         LA    R1,REVCMT+11                                                     
         EDIT  (B2,SVPRGNEW),(3,(R1)),ZERO=NOBLANK                              
*                                                                               
         LA    R1,REVCMT+20                                                     
         EDIT  (B2,SVPRGCHG),(3,(R1)),ZERO=NOBLANK                              
*                                                                               
         LA    R1,REVCMT+30                                                     
         EDIT  (B2,SVPRGASS),(3,(R1)),ZERO=NOBLANK                              
         GOTO1 DATCON,DMCB,(5,0),(5,REVCMT+51)                                  
         J     UPDRX                                                            
         DROP  R6                                                               
* BUILD SEED REVISION COMMENTS ELEMENTS *                                       
*                                                                               
BSCMT    NTR1  BASE=*,LABEL=*                                                   
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         MVI   ELCODE,X'60'                                                     
         GOTO1 REMELEM                                                          
         USING REVCMTEL,R6                                                      
         SR    R5,R5                                                            
         LA    R2,TRACMT1H                                                      
*                                                                               
BSCMT10  CLI   5(R2),0             TEST FOR A COMMENT IN THIS FIELD             
         BE    BSCMT20                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R5,1(,R5)                                                        
         MVI   REVCMTEL,X'60'      ELEMENT IDENTIFIER                           
         MVI   REVCMTLN,63         ELEMENT LENGTH                               
         STC   R5,REVNUMB          ELEMENT SEQUENCE NUMBER                      
*                                                                               
         GOTO1 ANY                 MOVES DATA LEFT JUSTIFIED INTO WORK          
         MVC   REVCMT,WORK                                                      
         GOTO1 ADDELEM                                                          
*                                                                               
BSCMT20  LLC   R0,0(R2)            NEXT FIELD                                   
         AR    R2,R0                                                            
         LA    RF,TRALCOMH                                                      
         CR    R2,RF               TEST END OF SCREEN                           
         JNL   UPDRX                                                            
         LLC   R0,0(R2)            NEXT FIELD                                   
         AR    R2,R0                                                            
         B     BSCMT10                                                          
*                                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
* REPLACE ISCI W/ADID IN UNIT TABLE (IF ANY)                                    
*                                                                               
RADID    NTR1  BASE=*,LABEL=*                                                   
         L     R1,AADIDTAB                                                      
         L     R0,AADIDTBX                                                      
         SR    R0,R1                                                            
         SRL   R0,4                GIVES ENTRIES COUNT                          
*                                                                               
RADID20  DS   0H                                                                
         OC    0(8,R1),0(R1)       EMPTY                                        
         BZ    RADIDX                                                           
         CLC   0(8,R1),WORK        SAME COMML?                                  
         BE    RADID30                                                          
         CLC   8(8,R1),WORK        TEST MATCHES ADID                            
         BE    RADID30             YES - TREAT AS MATCH                         
*                                                                               
         LA    R1,16(R1)           BUMP TO NEXT ENTRY                           
         BCT   R0,RADID20                                                       
         B     RADIDX                                                           
*                                                                               
RADID30  MVC   WORK+8(8),8(R1)     MOVE IN ADID                                 
*                                                                               
RADIDX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* HEAD HOOK ROUTINE                                                             
*                                                                               
HDHK     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OC    OPTDATE,OPTDATE                                                  
         BZ    HDHK02                                                           
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(2,OPTDATE),(5,H3+47)                                
         B     HDHK03                                                           
         FIXDT02                                                                
HDHK02   GOTO1 DATCON,DMCB,(2,STDATEP),(5,H3+47)                                
*                                                                               
HDHK03   MVI   H3+55,C'-'                                                       
         OC    OPTDAT2,OPTDAT2                                                  
         BZ    HDHK05                                                           
         FIXDT02                                                                
         GOTO1 (RF),(R1),(2,OPTDAT2),(5,H3+56)                                  
         B     HDHK06                                                           
         FIXDT02                                                                
HDHK05   GOTO1 (RF),(R1),(2,ENDATEP),(5,H3+56)                                  
*                                                                               
HDHK06   TM    SVOPTSW,OPTTEST     TEST OPTION TEST                             
         BZ    *+10                                                             
         MVC   H4+85(16),=C'*** TEST RUN ***'                                   
*                                                                               
         MVC   H4+8(3),QCLT                                                     
         MVC   H4+13(20),CLTNM                                                  
         MVC   H5+8(4),NETWORK                                                  
*                                                                               
         CLI   SVTNPR9,C'M'        MEDIA ONLY                                   
         BE    HDHK10                                                           
         MVC   H1+101(3),=C'(S)'                                                
         CLI   SVTNPR9,C'S'        SKED ONLY                                    
         BE    HDHK10                                                           
         MVI   H1+102,C'B'                                                      
*                                                                               
HDHK10   CLI   OPTPROD,0                                                        
         BE    HDHK20                                                           
         MVC   H4+50(9),=C'PRODUCT ='                                           
         MVC   H4+60(3),OPTPROD                                                 
*                                                                               
HDHK20   DS   0H                                                                
         OC    OPTPRGR,OPTPRGR    ANY PRODUCT GROUP                             
         BZ    HDHK50               NO                                          
         MVC   H4+45(13),=C'PRODUCT GROUP'                                      
         MVC   H4+59(1),SVTN2PRO+00                                             
         MVC   DUB(2),OPTPRGR                                                   
         UNPK  DUB+3(5),DUB(3)                                                  
         LLC   RE,PGRLEN           GET NUMBER OF PRDGRP DIGITS                  
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   H4+60(0),DUB+3                                                   
*                                                                               
HDHK50   LA    R2,P                                                             
         USING PRTLINE,R2                                                       
         OC    PAIRDT,PAIRDT       IS AIR DATE ALREADY PRINTED                  
         BZ    HDHK54               NO                                          
         CLC   PAIRDT,SPACES       IS AIR DATE ALREADY PRINTED                  
         BNE   HDHK60               YES, MAY ALSO BE BILLBOARD                  
*                                                                               
         FIXDT02                                                                
HDHK54   GOTO1 DATCON,DMCB,(2,SVUNTDTE),(4,PAIRDT)                              
*                                                                               
         CLC   SVUNTDTE(2),SVUNTDTE+2  BOTH DATES EQUAL                         
         BE    HDHK60                   YES, DO NOT PRINT                       
*                                                                               
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(2,SVUNTDTE+2),(4,PAIRDT2)                           
*                                                                               
HDHK60   OC    PPROG(2),PPROG      IS PROG ALREADY PRINTED                      
         BZ    HDHK64               NO                                          
         CLC   PPROG,SPACES        IS PROG ALREADY PRINTED                      
         BNE   HDHKX                YES                                         
HDHK64   MVC   PPROG,SVPRG                                                      
         MVC   PPROGNM,SVPRGNM                                                  
*                                                                               
HDHKX    XIT1                                                                   
*                                                                               
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************                      
* SUBROUTINE TO READ PATTERN RECORDS FOR UNTABLE ENTRIES *                      
**********************************************************                      
*                                                                               
BLPAT    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,APATABLE                                                      
         LR    R0,R2                                                            
         L     R1,MAXPATBL                                                      
         SR    R1,R0                                                            
         LR    RE,R1                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    0(L'PATENT+1,R2),0(R2) CLEAR ONE ENTRY                           
         ST    R2,NEXTADDR                                                      
*                                                                               
         BRAS  RE,SETXSP           SWITCH TO XSPOT                              
*                                                                               
* FIRST READ ANY ALL NETWORK PATTERN RECORDS *                                  
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NPTXKEY,R4                                                       
         MVC   NPTXKID,=X'0A61'                                                 
         MVC   NPTXAM(3),BAGYMD    A-M/CLT                                      
         MVI   NPTXR3F,X'A0'       BYPASS ANY SAVED PATTERNS                    
*                                                                               
* FIRST READ ANY ALL NETWORK PATTERN RECORDS *                                  
*                                                                               
BLP06    GOTO1 HIGH                                                             
*                                                                               
BLP08    DS    0H                                                               
         CLC   KEY(5),KEYSAVE      SAME A-M/CLT                                 
         BNE   BLP30                                                            
*                                                                               
         CLC   KEY(9),KEYSAVE      SAME A-M/CLT/NETWORK                         
         BNE   BLP20F                                                           
*                                                                               
         OC    NPTXNET(20),NPTXNET   THIS A PTN SEQ REC                         
         BZ    BLP10                                                            
*                                                                               
         CLI   NPTXR3F,X'A0'         THIS SAVED PTN                             
         BL    BLP10                                                            
*                                                                               
         B     BLP20                                                            
*                                                                               
BLP10    MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
         B     BLP08                                                            
*                                                                               
BLP20    CLC   KEY(9),KEYSAVE      SAME A-M/CLT/NETWORK                         
         BE    BLP24                                                            
         CLC   KEY(5),KEYSAVE      SAME A-M/CLT                                 
         BNE   BLP30                                                            
BLP20F   CLI   NPTXNET,X'00'       MEDIA SPECIFIC?                              
         BNE   BLP21               NO                                           
         CLI   NPTXNET+2,X'FF'     MEDIA SPECIFIC                               
         BNE   BLP21                NO                                          
         CLC   NPTXNET+1(1),SVMEDIA   THIS MEDIA?                               
         BNE   BLP20H                                                           
         CLI   NPTXR3F,X'A0'       THIS SAVED PTN                               
         BL    BLP10               YES, DO SEQ                                  
         B     BLP24                                                            
BLP20H   MVI   NPTXNET+3,X'FF'     NO, SKIP THIS MEDIA                          
         B     BLP06                                                            
*                                                                               
BLP21    CLC   NPTXNET,NETWORK                                                  
         BH    BLP30                                                            
         BNE   BLP21F                                                           
         CLI   NPTXR3F,X'A0'       THIS SAVED PTN                               
         BL    BLP10               YES, DO SEQ                                  
         B     BLP22                                                            
*                                                                               
* NOW READ NETWORK SPECIFIC PATTERN RECORDS *                                   
*                                                                               
BLP21F   XC    KEY,KEY                                                          
         MVC   NPTXKID,=X'0A61'                                                 
         MVC   NPTXAM(3),BAGYMD    A-M/CLT                                      
         MVC   NPTXNET(4),NETWORK    NETWORK                                    
         MVI   NPTXR3F,X'A0'         BYPASS ANY SAVED PTNS                      
         B     BLP06                                                            
*                                                                               
BLP22    MVC   KEYSAVE,KEY                                                      
*                                                                               
BLP24    L     R6,AIO1             SET I/O AREA ADDRESS                         
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         CLI   NPTXNET-NPTXKEY(R6),C'$' TEST NETWORK LIST PATTERN               
         BNE   *+12                                                             
         BRAS  RE,CKPATLST              CHECK IF NETWORK IS DELETED             
         BNE   BLP10                                                            
*                                                                               
         BRAS  RE,BLENT              BUILD PATTERN TABLE ENTRY                  
         B     BLP10                                                            
*                                                                               
BLP30    DS    0H                                                               
         BAS   RE,BLSORT           SORT PATTERN LIST                            
         CLI   ERROR,0                                                          
*                                                                               
BLPX     BRAS  RE,SETSPOT          SET TO SPOT                                  
         XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*================================================================               
* SEE IF NETWORK IN KEY+6 IS DELETED IN A 5B PATTERN LIST ELEM                  
*================================================================               
                                                                                
CKPATLST NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'5B'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
CKPAT2   BRAS  RE,NEXTEL                                                        
         JNE   NEQXIT                                                           
*                                                                               
         CLC   NPTXNET-NPTXKEY+KEY(4),2(R6)   RIGHT NETWORK ELEM                
         BNE   CKPAT2                                                           
*                                                                               
         TM    6(R6),X'80'         TEST NETWORK DELETED                         
         JO    NEQXIT                                                           
         J     EQXIT                                                            
         LTORG                                                                  
         EJECT                                                                  
* CHECK THAT PRD AND PRD LEN IS SAME IN COMMERCIAL                              
*                                                                               
VCMLPRD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,AIO               PAT REC IN AIO1                             
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   PRDMATSW,0                                                       
*                                                                               
         USING NPTCMLEL,R6                                                      
*                                                                               
         LLC   R0,NPTCMLLN         GET ELEM LEN                                 
         SRL   R0,4                DIV BY 16=NO OF CMML PRS                     
         LA    R5,NPTCML           1ST CMML                                     
VCMLPR10 CLI   0(R5),C'*'          IS IT DELETED                                
         BE    VCMLPR90                                                         
*                                                                               
         MVC   WORK(8),0(R5)                                                    
*                                                                               
VCMLPR15 DS    0H                                                               
         XC    DUB(4),DUB                                                       
         BRAS  RE,FCMLB              FIND CML RECORD                            
         BNE   BDCMLERR            CML NOT FOUND                                
*                                                                               
         L     R4,AIO1             PATTERN RECORD                               
         USING NPTXKEY,R4                                                       
*                                                                               
         L     R6,AIO3             COMMERCIAL RECORD                            
*                                                                               
         MVI   ELCODE,X'29'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLMPREL,R6                                                      
         CLC   =XL3'2903FF',CMLMPREL IS THIS COMML PRD=ALL                      
         BE    VCMLPR30            YES, COVERS ALL PRODUCTS                     
         LLC   RF,CMLMPRLN                                                      
         SHI   RF,2                                                             
         LR    RE,RF                                                            
         LA    R2,CMLMPRS          START OF PROD LIST                           
VCMLPR20 CLC   NPTXPRD,0(R2)       MATCH PAT PROD TO CML PRD                    
         BE    VCMLPR30            YES                                          
         LA    R2,3(,R2)                                                        
         SHI   RF,2                                                             
         BCT   RF,VCMLPR20                                                      
         OC    NPTXPRD2,NPTXPRD2   IS THERE A PARTNER PROD                      
         BZ    BDCMLERR            NO, MUST MATCH PROD 1                        
         B     VCMLPR50            SEE IF 2 MATCHES                             
*                                                                               
VCMLPR30 CLC   NPTXSLN,DUB         IS THIS CML SAME SPOT LENGTH                 
         BE    VCMLPR40            YES                                          
         CLI   NPTXPRD2,0          IS THERE A PARTNER PROD                      
         BE    BDCMLERR            NO, MUST MATCH PROD 1                        
         LLC   RE,NPTXSLN                                                       
         LLC   RF,NPTXSLN2                                                      
         AR    RE,RF                                                            
         CLM   RE,1,DUB                                                         
         BNE   BDCMLERR            NO, MUST MATCH PROD 1                        
VCMLPR40 CLI   NPTXPRD2,0          IS THERE A PARTNER PROD                      
         BE    VCMLPR90            NO, JUST GET OUT                             
         OI    PRDMATSW,X'80'                                                   
VCMLPR50 CLC   =XL3'2903FF',CMLMPREL IS THIS COMML PRD=ALL                      
         BE    VCMLPR70            YES, COVERS ALL PRODUCTS                     
         LA    R2,CMLMPRS          START OF PROD LIST                           
VCMLPR60 CLC   NPTXPRD2,0(R2)      IS THIS PRD 2                                
         BE    VCMLPR70                                                         
         LA    R2,3(,R2)                                                        
         SHI   RE,2                                                             
         BCT   RE,VCMLPR60                                                      
         TM    PRDMATSW,X'80'      WAS OTHER PROD FOUND                         
         BO    VCMLPR80            YES                                          
         B     BDCMLERR                                                         
VCMLPR70 OI    PRDMATSW,X'40'                                                   
         CLC   NPTXSLN2,DUB        IS THIS CML SAME SPOT LENGTH                 
         BE    VCMLPR80            YES                                          
         TM    PRDMATSW,X'C0'      WERE BOTH PRODS FOUND                        
         BNO   BDCMLERR            NO                                           
         LLC   RE,NPTXSLN                                                       
         LLC   RF,NPTXSLN2                                                      
         AR    RE,RF                                                            
         CLM   RE,1,DUB                                                         
         BNE   BDCMLERR                                                         
         OI    PRDMATSW,X'08'      SET ON BOTH PROD COVERED                     
         B     VCMLPR90                                                         
*                                                                               
VCMLPR80 DS    0H                                                               
         TM    PRDMATSW,X'C0'      WERE BOTH PRODUCTS FOUND                     
         BO    VCMLPR90             YES                                         
         TM    PRDMATSW,X'08'      BOTH PRODUCTS COVERED                        
         BO    VCMLPR90             YES                                         
         TM    PRDMATSW,X'02'      DID WE LOOK FOR PRD/PTR                      
         BZ    *+8                                                              
         B     BDCMLERR            YES, AND DIDN'T FIND IT                      
         OI    PRDMATSW,X'02'      LOOK FOR PRD/PTR                             
         MVC   WORK(8),8(R5)                                                    
         B     VCMLPR15                                                         
*                                                                               
VCMLPR90 LA    R5,16(R5)                                                        
         BCT   R0,VCMLPR10         VLD NEXT CML                                 
*                                                                               
         BRAS  RE,SETNET           SET TO NET                                   
*                                                                               
         B     VCMLPRX                                                          
*                                                                               
* GENERATE COMML SEED ERROR REPORT *                                            
*                                                                               
BDCMLERR DS    0H                                                               
         OI    UPDSW,FTLCMLER     CML PRD ERR FOUND                             
         MVI   BYTE,5             CML PRD ERROR                                 
         CLI   OFFLINE,C'Y'                                                     
         BNE   VCMLPR90                                                         
*                                                                               
         MVC   ELEM,SPACES                                                      
         LA    R4,ELEM                                                          
         USING XRECD,R4                                                         
         MVC   XBAGYMD,BAGYMD                                                   
         MVC   XCLT,QCLT                                                        
         MVC   XCML,WORK                                                        
         MVC   XCML+8(4),SPACES                                                 
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   BDCML02                                                          
         GOTO1 VTRPACK,DMCB,(C'U',WORK),XCML                                    
*                                                                               
BDCML02  MVC   XTYPE,BYTE                                                       
         MVI   XSUBTYPE,0                                                       
*                                                                               
         CLC   DUB(7),=C'DELETED'                                               
         BNE   *+12                                                             
         MVI   XSUBTYPE,1                                                       
         B     BDCML10                                                          
         CLC   DUB(7),=C'REL/RCL'                                               
         BNE   *+12                                                             
         MVI   XSUBTYPE,2                                                       
         B     BDCML10                                                          
         CLC   DUB(7),=C'NOT FND'                                               
         BNE   *+8                                                              
         MVI   XSUBTYPE,3                                                       
*                                                                               
* SAVE SOME PATTERN INFORMATION                                                 
*                                                                               
BDCML10  DS   0H                                                                
*                                                                               
         L     R6,AIO1             PAT REC                                      
         USING NPTXKEY,R6                                                       
*                                                                               
         MVC   XNET,NPTXNET                                                     
         MVC   XPROG,NPTXPROG                                                   
         MVC   XPROD1,NPTXPRD      PRD AND LEN                                  
         MVC   XPROD2,NPTXPRD2     PARTNER AND LEN                              
         MVC   XREF,NPTXR3F        REFERENCE                                    
         B     BDCML50                                                          
*                                                                               
BDCML50  DS    0H                                                               
*                                                                               
         DROP  R6                                                               
*                                                                               
         BRAS  RE,SETXSP           SET TO XSPOT                                 
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING NPTDTAEL,R6                                                      
*                                                                               
         MVC   XDATEST,NPTSTART    PATTERN START DATE                           
         MVC   XDATEND,NPTEND       END DATE                                    
*                                                                               
         DROP  R6,R4                                                            
*                                                                               
         STC   R0,BYTE1            SAVE COUNTER IN BYTE                         
         L     R1,=A(WORKFIL)                                                   
         LA    R0,ELEM                                                          
         PUT   (1),(0)                                                          
         LLC   R0,BYTE1                                                         
         BRAS  RE,SETSPOT          SET TO SPOT                                  
         B     VCMLPR90                                                         
*                                                                               
VCMLPRX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*************************************************************                   
* SUBROUTINE TO PROCESS PATTERN LIST DATA AND BUILD A TABLE *                   
* OF PATTERNS THAT APPLY TO EACH DAY IN THE TELECAST PERIOD *                   
*************************************************************                   
*                                                                               
BLSORT   NTR1  BASE=*,LABEL=*                                                   
         L     R2,APATABLE                                                      
         SR    R0,R0                                                            
         LR    R1,R2                                                            
*                                                                               
BLS10    CLI   0(R1),0                                                          
         BE    BLS12                                                            
         LA    R1,L'PATENT(,R1)                                                 
         BCTR  R0,0                                                             
         B     BLS10                                                            
*                                                                               
BLS12    DS    0H                                                               
         LPR   R0,R0                                                            
         BZ    PATERR              NO PATTERNS                                  
*                                                                               
         GOTO1 QSORT,DMCB,(R2),(R0),L'PATENT,L'PATSRT,0                         
*                                                                               
*                                                                               
* NOW REDO PAT SEQNUMS                                                          
*                                                                               
         L     R2,APATABLE                                                      
         USING PATABLED,R2                                                      
         LA    R4,1                                                             
*                                                                               
BLS15    STH   R4,PATSEQ                                                        
*                                                                               
         LA    R4,1(R4)                                                         
         LA    R2,PATNEXT                                                       
         BCT   R0,BLS15                                                         
*                                                                               
         L     R5,AUNTABLE                                                      
         USING UNTABLED,R5                                                      
         OI    UPDSW,X'80'         SET ON NO PATTERNS FOUND                     
*                                                                               
BLS16    DS    0H                                                               
         OC    UNTPROD2,UNTPROD2   CK P/B PROD                                  
         BZ    BLS17                                                            
*                                                                               
         CLC   UNTPROD,UNTPROD2    TEST PRDS IN ALPHA SEQ                       
         BL    BLS17                                                            
         XC    UNTPROD(4),UNTPROD2 SWAP PRDS AND SLNS                           
         XC    UNTPROD2(4),UNTPROD                                              
         XC    UNTPROD(4),UNTPROD2                                              
         OI    UNTFLG,UNTFLGSW     SET ON PRDS SWAPPED                          
*                                                                               
BLS17    LA    R5,UNTNEXT                                                       
         CLI   0(R5),0             END OF UNITS                                 
         BNE   BLS16                                                            
                                                                                
*                                                                               
* GO THRU UNIT TABLE, ASSIGNING AND MARKING PATTERNS USED *                     
*                                                                               
         L     R5,AUNTABLE                                                      
*                                                                               
BLS20    L     R2,APATABLE         PATTERN TABLE                                
         SR    R6,R6               CLEAR SAVED PATTERN POINTER                  
*                                                                               
BLS22    DS    0H                                                               
         OC    UNTFEED,UNTFEED     THIS A FEED UNIT                             
         BZ    BLS24                NO                                          
         CLC   =X'00FFE3',UNTFEED  CHK FOR TAG                                  
         BE    BLS24               ITS A TAG NOT A FEED                         
*                                                                               
         CLI   PATYPE,PATQFEED     THIS A FEED OR FEED/DP PATTN                 
         BH    BLS40                                                            
         CLC   PATFEED,UNTFEED                                                  
         BNE   BLS40                                                            
*                                                                               
         CLI   PATDP,0             DAYPART CODE ALSO                            
         BE    BLS30                NO                                          
         CLC   PATDP,UNTDP                                                      
         BE    BLS30                                                            
         B     BLS40                                                            
                                                                                
BLS24    CLI   PATYPE,PATQPROG     THIS A PROG SPEC PATTERN                     
         BNE   BLS26                                                            
         CLC   UNTPROG,PATPROG                                                  
         BE    BLS30                                                            
         B     BLS40                                                            
*                                                                               
BLS26    OC    PATFEED,PATFEED     THIS A FEED PATTERN                          
         BNZ   BLS40                YES                                         
         CLI   PATDP,0                                                          
         BE    BLS30                                                            
         CLC   PATDP,UNTDP                                                      
         BNE   BLS40                                                            
                                                                                
*=============================================================                  
* FIND LEAST USED PATTERN ENTRY FOR THIS UNIT                                   
*=============================================================                  
                                                                                
BLS30    CLC   UNTPROD(8),PATPROD1   PATTERN FOR RIGHT PRDS                     
         BNE   BLS40                 NO - NEXT PATTERN                          
*                                                                               
         CLC   UNTADTEP,PATEND     UNIT START DATE AFTER PATTERN END            
         BH    BLS40                                                            
         CLC   UNTADTE2,PATSTR     UNIT END BEFORE PATTERN START                
         BL    BLS40                                                            
*                                                                               
         OC    PATSTIM,PATSTIM      TEST PATTERN HAS TIME                       
         BZ    BLS36                NO - USE IT                                 
*                                                                               
         CLC   UNTADTEP,UNTADTE2     TEST ONE DAY UNIT                          
         BNE   BLS36                 NO - USE IT                                
*                                                                               
         MVC   MYPSTIM(4),PATSTIM  MOVE PATTERN TIMES                           
         CLC   MYPSTIM,=H'2400'                                                 
         BNE   *+10                                                             
         XC    MYPSTIM,MYPSTIM     MAKE 12M 0                                   
*                                                                               
         CLC   MYPSTIM,=H'600'     TEST PAT STARTS AFTER 6A                     
         BL    BLS31A                                                           
         LH    RE,MYPETIM                                                       
         CLC   MYPETIM,=H'600'     AND PAT ENDS BEFORE 6A                       
         BH    *+8                                                              
         LA    RE,2400(RE)                                                      
         STH   RE,MYPETIM                                                       
*                                                                               
BLS31A   MVC   MYUSTIM(4),UNTTIME  MOVE UNIT TIMES                              
         CLC   MYPSTIM,=H'600'     TEST PAT STARTS AFTER 6A                     
         BL    BLS31B                                                           
*                                                                               
         LH    RE,MYUSTIM                                                       
         CLC   MYUSTIM,=H'600'     TEST UNIT STARTS AFTER 6A                    
         BNL   *+8                                                              
         LA    RE,2400(RE)                                                      
         STH   RE,MYUSTIM                                                       
*                                                                               
         LH    RE,MYUETIM                                                       
         CLC   MYUETIM,=H'600'                                                  
         BH    *+8                                                              
         LA    RE,2400(RE)                                                      
         STH   RE,MYUETIM                                                       
*                                                                               
BLS31B   CLC   UNTADTEP,PATSTR       UNIT DATE MATCH PATT START                 
         BNE   BLS32                                                            
         CLC   PATSTR,PATEND         TEST ONE DAY PATTERN                       
         BE    BLS31C                                                           
         CLC   MYUETIM,MYPSTIM       TEST UNIT ENDS BEFORE PATT START           
         BL    BLS40                 YES - SKIP                                 
         B     BLS36                 ELSE USE IT                                
*                                                                               
* ONE DAY UNIT AND ONE DAY PATTERN                                              
*                                                                               
BLS31C   CLC   MYUSTIM,MYPETIM       UNIT START TIME AFTER PATT END             
         BH    BLS40                 YES - SKIP                                 
         CLC   MYUETIM,MYPSTIM       UNIT END TIME BEFORE PATT START            
         BL    BLS40                                                            
         B     BLS36                 USE THIS PATTERN                           
*                                                                               
* ONE DAY UNIT NOT ON PATTERN START DATE                                        
*                                                                               
BLS32    CLC   UNTADTEP,PATEND       UNIT DATE MATCH PATTERN END                
         BL    BLS36                 BEFORE END DATE - USE IT                   
         CLC   MYUSTIM,MYPETIM       UNIT START TIME AFTER PATTERN END          
         BH    BLS40                 YES - SKIP                                 
*                                                                               
BLS36    MVC   FULL(2),UNTADTEP    MOVE UNIT START/END                          
         MVC   FULL+2(2),UNTADTE2                                               
*                                                                               
         CLC   FULL(2),PATSTR      UNIT START BEFORE PATTERN START              
         BNL   *+10                NO                                           
         MVC   FULL(2),PATSTR      YES - SAVE PATTERN START                     
*                                                                               
         CLC   FULL+2(2),PATEND    UNIT END BEFORE PATTERN END                  
         BL    *+10                NO                                           
         MVC   FULL+2(2),PATEND    YES - SAVE PATTERN END                       
*                                                                               
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(2,FULL),WORK                                        
         FIXDT02                                                                
         GOTO1 (RF),(R1),(2,FULL+2),WORK+6                                      
         GOTO1 PERVERT,DMCB,WORK,WORK+6                                         
         LH    R0,DMCB+8                                                        
         STC   R0,PATCOVER         AND SAVE IT                                  
*                                                                               
         LTR   R6,R6               TEST FOUND AN ENTRY YET                      
         BNZ   *+6                 YES                                          
         LR    R6,R2               SAVE PATTERN TABLE ENTRY ADDRESS             
*                                                                               
         CLC   PATYPE,PATYPE-PATENT(R6) TEST SAME LEVEL PATTERNS                
         BNE   BLS37                    NO                                      
*                                                                               
         LLC   RE,PATCNT                GET NEW USE COUNT                       
         LA    RF,1000                                                          
         MR    RE,RE                    SCALE UP                                
         LLC   R0,PATCOVER              GET NEW DAY COUNT                       
         DR    RE,R0                    CALCULATE COUNT/DAYS                    
         ST    RF,DUB                   AND SAVE IT                             
*                                                                               
         LLC   RE,PATCNT-PATENT(R6)     GET OLD USE COUNT                       
         LA    RF,1000                  SCALE UP                                
         MR    RE,RE                                                            
         LLC   R0,PATCOVER-PATENT(R6)   GET OLD DAY COUNT                       
         DR    RE,R0                                                            
*                                                                               
         C     RF,DUB                   COMPARE RATIOS                          
         BH    BLS38                    OLD HIGHER - USE NEW                    
         BL    BLS40                    OLD LOWER - SKIP IT                     
*                                                                               
BLS37    CLC   PATCOVER,PATCOVER-PATENT(R6)  NEW PATT COVER MORE DAYS?          
         BL    BLS40                    NO SKIP IT                              
         BH    BLS38                    IF HIGH - USE IT                        
*                                                                               
         LTR   R6,R6                 TEST FOUND AN ENTRY YET                    
         BNZ   *+6                   YES                                        
         LR    R6,R2                 SAVE PATTERN TABLE ENTRY ADDRESS           
*                                                                               
         CLC   PATYPE,PATYPE-PATENT(R6)   COMPARE PATTERN TYPES                 
         BH    BLS40                      HI IS LESS SPECIFIC - SKIP            
         BL    BLS38                      LO IS MORE SPECIFIC - USE IT          
         CLC   PATCNT,PATCNT-PATENT(R6)   COMPARE COUNTERS                      
         BH    BLS40                                                            
         BL    BLS38                                                            
* IF EQUAL USAGE, USE EARLIER START DATE                                        
         CLC   PATSTR,PATSTR-PATENT(R6)   IS NEW STDATE BEFORE OLD?             
         BH    BLS40                      NO - KEEP OLD                         
         BL    BLS38                                                            
* IF SAME START DATE, USE EARLIER START TIME                                    
         CLC   PATSTIM,PATSTIM-PATENT(R6) IS NEW STTIME BEFORE OLD?             
         BH    BLS40                                                            
*                                                                               
BLS38    LR    R6,R2               SAVE ENTRY WITH LOWEST COUNT                 
*                                                                               
BLS40    LA    R2,PATNEXT                                                       
         CLI   0(R2),0                                                          
         BNE   BLS22                                                            
*                                                                               
         LTR   R6,R6               TEST FOUND ANY ENTRY                         
         BZ    BLS42               NO                                           
         LR    R2,R6                                                            
         LLC   RE,PATCNT                                                        
         LA    RE,1(RE)                                                         
         STC   RE,PATCNT                                                        
         OI    PATFLG,PATFLUSD     MARK PATTERN USED                            
         MVC   UNTPSEQ,PATSEQ                                                   
*                                                                               
BLS42    TM    UNTFLG,UNTFLGSW     WERE PRDS SWAPPED                            
         BZ    BLS44                                                            
         XC    UNTPROD(4),UNTPROD2                                              
         XC    UNTPROD2(4),UNTPROD                                              
         XC    UNTPROD(4),UNTPROD2                                              
*                                                                               
BLS44    LA    R5,UNTNEXT                                                       
         CLI   UNTADTEP,0          END OF UNITS                                 
         BNE   BLS20                                                            
*                                                                               
* NOW GO THRU PATTERNS AND SEED ALL UNITS *                                     
*                                                                               
         L     R2,APATABLE         START OF PATTERN TABLE                       
*                                                                               
BLS48    L     R5,AUNTABLE                                                      
*                                                                               
         TM    PATFLG,PATFLUSD     WAS PATTERN USED                             
         BZ    BLS74                NO                                          
*                                                                               
         MVI   BYTE,0                                                           
         CLI   PATYPE,PATQNET              NET PAT                              
         BE    BLS50                                                            
         CLI   PATYPE,PATQALL              ALL NET PAT                          
         BNE   *+12                                                             
         OI    BYTE,X'10'          SET TO ALL NETS                              
         B     BLS50                                                            
*                                                                               
         CLI   PATYPE,PATQMED              MEDIA SPECIFIC                       
         BNE   *+12                                                             
         OI    BYTE,X'08'          SET TO MEDIA                                 
         B     BLS50                                                            
*                                                                               
         CLI   PATYPE,PATQDPMD             DAYPART & MEDIA SPECIFIC             
         BNE   *+12                                                             
         OI    BYTE,X'28'          SET TO MEDIA & DAYPART                       
         B     BLS50                                                            
*                                                                               
         CLI   PATYPE,PATQDPT              DAYPART SPECIFIC                     
         BNE   *+12                                                             
         OI    BYTE,X'20'                                                       
         B     BLS50                                                            
*                                                                               
         CLI   PATYPE,PATQPROG             PROG SPECIFIC                        
         BNE   *+12                                                             
         OI    BYTE,X'80'                                                       
         B     BLS50                                                            
*                                                                               
         CLI   PATYPE,PATQFEED             FEED ONLY                            
         BNE   *+12                                                             
         OI    BYTE,X'40'                                                       
         B     BLS50                                                            
*                                                                               
         CLI   PATYPE,PATQDPFD             FEED AND DAYPART                     
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    BYTE,X'60'                                                       
*                                                                               
BLS50    DS   0H                                                                
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A61'                                                  
         MVC   KEY+36(4),PATDSKA                                                
         BRAS  RE,SETXSP           SET TO XSPOT                                 
*                                                                               
BLS52    DS   0H                                                                
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING NPTDTAEL,R6                                                      
         MVI   ADIDFLAG,C'Y'                                                    
         TM    NPTSTAT,NPTS_ADID                                                
         BO    *+8                                                              
         MVI   ADIDFLAG,C'N'                                                    
*                                                                               
         MVI   ELCODE,X'30'                                                     
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R4,R6               SAVE CMML ELEM ADDRESS                       
                                                                                
*=================================================================              
* ISCI CODES ARE SAVED IN THE UNITS IN EBCDIC, BUT IF ADIDFLAG=Y                
* THEY ARE IN THE PATTERN AS PACKED ADIDS                                       
* SO BUILD ANOTHER ELEMENT IN ELEM WITH 8-BYTE ISCI CODES                       
*=================================================================              
                                                                                
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   BLS52X                                                           
*                                                                               
         XC    ELEM,ELEM                                                        
         LLC   RE,1(R6)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R6)                                                    
*                                                                               
         LLC   R0,1(R6)                                                         
         SRL   R0,4                                                             
         LA    R4,2(R6)                                                         
         LA    R5,ELEM+2                                                        
*                                                                               
BLS52A   GOTO1 VTRPACK,DMCB,(C'U',(R4)),WORK                                    
         CLI   WORK+8,C' '         TEST 8 CHAR                                  
         BH    *+10                NO                                           
         MVC   0(8,R5),WORK        MOVE ISCI CMML                               
*                                                                               
         OC    8(8,R4),8(R4)       TEST P/B CMML                                
         BZ    BLS52B                                                           
         GOTO1 (RF),(R1),(C'U',8(R4)),WORK                                      
         CLI   WORK+8,C' '                                                      
         BH    *+10                                                             
         MVC   8(8,R5),WORK                                                     
*                                                                               
BLS52B   LA    R4,16(R4)                                                        
         LA    R5,16(R5)                                                        
         BCT   R0,BLS52A                                                        
*                                                                               
BLS52X   LR    R4,R6               SAVE 30 ELEMENT ADDRESS                      
*                                                                               
         MVI   ELCODE,X'32'        NOW FIND ROTATION ELEMENT                    
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,VCMLPRD                                                       
                                                                                
*=====================================================================          
* BUILD A TABLE IN AIO1+3000 OF CMML NUMBERS WHERE A=1,B=2,...J=10...           
* THEN REPLICATE IT AS MANY TIMES AS IT WILL FIT IN 3000 BYTES                  
* THEN GO THROUGH UNIT TABLE AND REMOVE ENTRIES FOR PREVIOUSLY                  
* ASSIGNED COMMERCIALS                                                          
*======================================================================         
                                                                                
         L     RE,AIO1                                                          
         LA    RE,3000(RE)                                                      
         LR    R3,RE               SAVE START OF TABLE ADDRESS                  
         LA    RF,3000                                                          
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LLC   R0,1(R6)            ELEMENT LENGTH                               
         LA    R1,2(R6)            FIRST ENTRY                                  
         BCTR  R0,0                                                             
         BCTR  R0,0                NUMBER OF ENTRIES                            
         STH   R0,0(R3)            SET LENGTH OF PATTERN LIST                   
         LA    RE,2(R3)            POINT TO FIRST ENTRY                         
*                                                                               
BLS54    ICM   RF,1,0(R1)                                                       
         N     RF,=X'0000000F'                                                  
         CLI   0(R1),C'J'                                                       
         BL    *+8                                                              
         LA    RF,9(RF)            J=10, K=11                                   
         STC   RF,0(RE)                                                         
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,BLS54                                                         
*                                                                               
         LH    RE,0(R3)            GET NUMBER OF TABLE ENTRIES                  
         LA    RF,2(RE,R3)         POINT BEYOND LAST ENTRY                      
         BCTR  RE,0                SET FOR EX                                   
*                                                                               
BLS56    EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),2(R3)       COPY THE TABLE                               
*                                                                               
         LA    RF,1(RE,RF)         NEXT MOVE TO ADDRESS                         
         LA    R1,0(RE,RF)         GIVES END OF NEXT MOVE                       
         LA    R0,2999(R3)                                                      
         CR    R1,R0                                                            
         BL    BLS56                                                            
         SR    RF,R3               GIVES COUNT TO RF                            
         AHI   RF,-2               NOW ADJUST TO NUMBER OF ENTRIES              
         STH   RF,0(R3)            SET NUMBER OF ENTRIES IN TABLE NOW!          
*                                                                               
         CLI   SVTN1PR9,C'Y'       BYPASS ASSIGNED CMLS                         
         BNE   BLS60               NO                                           
*                                                                               
* LOOK FOR PREVIOUSLY ASSIGNED COMMERCIALS AND REMOVE                           
* THE FIRST ENTRY FOR IT IN THE PATTERN LIST                                    
*                                                                               
         L     R5,AUNTABLE                                                      
*                                                                               
BLS58A   TM    UNTFLG,UNTFLGAS     PREV ASSIGNED                                
         BZ    BLS58X                                                           
         CLC   PATSEQ,UNTPSEQ      THIS UNIT FOR THIS PAT                       
         BNE   BLS58X                                                           
*                                                                               
         LLC   R0,1(R4)            GET CMML ELEM LENGTH                         
         SRL   R0,4                DIV BY 16 GIVES NUMBER OF ENTRIES            
         LA    RE,2(R4)            POINT TO REAL ELEMENT                        
         LA    RF,ELEM+2           POINT TO ISCI MODIFIED ELEMENT               
*                                                                               
BLS58B   CLC   UNTCML1(16),0(RE)                                                
         BE    BLS58C                                                           
         CLC   UNTCML1(16),0(RF)                                                
         BE    BLS58C                                                           
         LA    RE,16(RE)                                                        
         LA    RF,16(RF)                                                        
         BCT   R0,BLS58B                                                        
         B     BLS58X              CAN'T FIND ENTRY                             
*                                                                               
BLS58C   SR    RE,R4               DSPL FROM START OF ELEMENT                   
         SRL   RE,4                DIV BY 16 GIVES ENTRY NUMBER-1               
         LA    RE,1(RE)            AND FINALLY THE ENTRY NUMBER                 
*                                                                               
         LH    R0,0(R3)                                                         
         LA    R1,2(R3)                                                         
*                                                                               
BLS58D   CLM   RE,1,0(R1)          MATCH ENTRY NUMBER                           
         BE    BLS58E                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,BLS58D                                                        
         B     BLS58X              ALL OCCURRENCES ARE GONE!                    
*                                                                               
BLS58E   MVI   0(R1),X'FF'         SET DO NOT USE THIS ENTRY!                   
*                                                                               
BLS58X   LA    R5,UNTNEXT                                                       
         CLI   UNTADTEP,0          END OF UNITS                                 
         BNE   BLS58A              NO - CONTINUE                                
*                                                                               
BLS60    L     R5,AUNTABLE                                                      
         LH    R0,0(R3)            NUMBER OF TABLE ENTRIES                      
         LA    R1,2(R3)            POINT TO FIRST ENTRY                         
*                                                                               
BLS62    CLC   PATSEQ,UNTPSEQ      THIS UNIT FOR THIS PAT                       
         BNE   BLS70                                                            
         CLI   SVTN1PR9,C'Y'       BYPASS ASSIGNED CMLS                         
         BNE   *+12                NO                                           
         TM    UNTFLG,UNTFLGAS     PREV ASSIGNED                                
         BO    BLS70               YES -SKIP                                    
*                                                                               
         CLI   0(R1),X'FF'         TEST DELETED ENTRY                           
         BNE   BLS63                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,*-12                                                          
         DC    H'0'                3000 ENTRIES SHOULD BE ENOUGH!               
*                                                                               
BLS63    LLC   RE,0(R1)            CMML ENTRY NUMBERR                           
         BCTR  RE,0                                                             
         SLL   RE,4                TIMES 16                                     
         LA    RF,2(RE,R4)         THIS COMML                                   
*                                                                               
         CLI   PATPROD2,0          PIGGYBACK PROD                               
         BE    *+20                 NO                                          
         OC    8(8,RF),8(RF)       IS THERE A SECOND CML                        
         BNZ   *+10                 YES                                         
         MVC   8(8,RF),0(RF)       SET UP FOR BOTH PRODS                        
*                                                                               
BLS64    DS    0H                                                               
         OC    UNTCML1(16),UNTCML1 ANY COMML ASSIGNED                           
         BNZ   BLS64C               YES                                         
         OI    UNTFLG,UNTFLGNW                                                  
         B     BLS65C                                                           
BLS64C   CLC   UNTCML1(16),0(RF)   THESE DIFFERENT COMMLS                       
         BE    BLS64F               NO                                          
*                                                                               
* SEE IF CMLS WERE SWAPPED                                                      
*                                                                               
         CLC   UNTCML1,8(RF)                                                    
         BNE   BLS65                YES, CHANGE, NEW REVISION                   
         CLC   UNTCML2,0(RF)                                                    
         BNE   BLS65                YES, CHANGE, NEW REVISION                   
*                                                                               
BLS64F   CLC   UNTPKEY,BYTE        THIS DIFFERENT PATTERN TYPE                  
         BNE   BLS65                YES, CHANGE, NEW REVISION                   
*                                                                               
         CLC   UNTREF,PATSEQNO     THIS NEW PAT REF?                            
         BE    BLS68                NO, NO CHANGE, NO NEW REVISION              
*                                                                               
BLS65    OI    UNTFLG,UNTFLGCH                                                  
*                                                                               
BLS65C   DS    0H                                                               
*                                                                               
BLS66    MVC   UNTREVN,SVPRGRVN                                                 
         NI    UPDSW,X'FF'-X'80'     SET OFF NO PATTERNS SW                     
*                                                                               
         CLC   =C'H7',AGENCY                                                    
         BNE   BLS66C                                                           
*                                                                               
*===> ASSIGN COMMERCIAL HERE                                                    
         CLI   SVTN1PR9,C'Y'       BYPASS ASSIGNED CMLS                         
         BE    *+8                 YES, DO NOT RESET REASGN SWITCH YET          
BLS66C   NI    UNTFLG,X'FF'-UNTFLGRE SET OFF NEED REASSIGN SWITCH               
                                                                                
         MVC   UNTCML1(16),0(RF)     ASSIGN COMMERCIAL TO UNIT!                 
*===> ASSIGN COMMERCIAL HERE                                                    
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   BLS68                                                            
         BAS   RE,GETISCI          GET ISCI FOR CMML IF NEEDED                  
*                                                                               
BLS68    LA    R1,1(,R1)                                                        
         BCTR  R0,0                                                             
*                                                                               
BLS70    LA    R5,UNTNEXT                                                       
         CLI   UNTADTEP,0          END OF UNITS                                 
         BE    BLS74                YES                                         
         LTR   R0,R0                                                            
         BNZ   BLS62                                                            
         B     BLS60                                                            
*                                                                               
BLS74    LA    R2,PATNEXT                                                       
         CLI   0(R2),0             END OF PATTERN TABLE                         
         BNE   BLS48                NO                                          
         B     BLS90                                                            
                                                                                
*==========================================================                     
* USE ISCI FOR PACKED ISCI CMMLS (CMML LESS THAN 9 CHARS)                       
*==========================================================                     
                                                                                
GETISCI  NTR1                                                                   
         GOTO1 VTRPACK,DMCB,(C'U',UNTCML1),WORK                                 
         CLI   WORK+8,C' '                                                      
         BH    *+10                                                             
         MVC   UNTCML1,WORK                                                     
*                                                                               
         OC    UNTCML2,UNTCML2                                                  
         BZ    GETISCIX                                                         
         GOTO1 VTRPACK,DMCB,(C'U',UNTCML2),WORK                                 
         CLI   WORK+8,C' '                                                      
         BH    *+10                                                             
         MVC   UNTCML2,WORK                                                     
*                                                                               
GETISCIX XIT1                                                                   
                                                                                
*================================================================               
* GO THROUGH UNIT TABLE AND IF THERE WAS A PREVIOUSLY ASSIGNED                  
* COMMERCIAL AND NOW THERE IS NO PATTERN FOR IT THEN TURN ON                    
* FLAG TO UPDATE REVISION RECORD                                                
*================================================================               
                                                                                
BLS90    CLI   SVTN1PR9,C'Y'       BYPASS ASSIGNED CMLS                         
         BE    BLSX                 YES, DONE                                   
*                                                                               
         L     R5,AUNTABLE                                                      
*                                                                               
BLS92    OC    UNTPSEQ,UNTPSEQ     IF PATTERN SEQUENCE IS ZERO                  
         BNZ   BLS95                                                            
         OC    UNTCML1(16),UNTCML1 AND THERE WAS CML ASSIGNED BEFORE            
         BZ    BLS95                                                            
         OI    UNTFLG,UNTFLGCH     TURN ON CHG FLAG FOR REV REC UPDATE          
*                                                                               
         XC    UNTCML1(16),UNTCML1 CLEAR COMML(S)                               
         XC    UNTREF,UNTREF                                                    
         XC    UNTPSEQ,UNTPSEQ     CLEAR PATTERN POINTER                        
*                                                                               
BLS95    LA    R5,UNTNEXT                                                       
         CLI   UNTADTEP,0          END OF UNITS                                 
         BNE   BLS92                                                            
*                                                                               
BLSX     XIT1                                                                   
*                                                                               
PATERR   CLI   OFFLINE,C'Y'        IF OFFLINE DO ERROR MSG                      
         BE    PATERR10                                                         
         SR    R2,R2                                                            
         ICM   R2,3,SVPRGDSP                                                    
         AR    R2,RA                                                            
         MVC   GERROR,=Y(NOPATT)                                                
         GOTO1 VTRAERR                                                          
PATERR10 MVI   ERROR,NOPATERR                                                   
         J     BLPX                                                             
NOPATERR EQU   X'01'                                                            
         LTORG                                                                  
         EJECT                                                                  
****************************************************************                
* SUBROUTINE TO BUILD A PATTERN LIST ENTRY FROM PATTERN RECORD *                
****************************************************************                
*                                                                               
BLENT    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    NXPATSEQ,NXPATSEQ                                                
         L     R2,NEXTADDR                                                      
         XC    0(L'PATENT+1,R2),0(R2)    CLEAR 1 ENTRY + 1 BYTE                 
*                                                                               
         L     R5,AUNTABLE                                                      
         USING UNTABLED,R5                                                      
*                                                                               
         LR    R4,R6                                                            
*                                                                               
*NOP     TM    SECFLAG,NEMORPRD+NECONPRD MORE PRD USER                          
****     BZ    BLE10                                                            
*                                                                               
         USING NPTXKEY,R4                                                       
         CLI   NPTXPROG,X'FF'      THIS A FEED OR DAYPART                       
         BE    BLE16                YES                                         
         OC    NPTXPROG,NPTXPROG   PATTERN PROGRAM SPECIFIC                     
         BZ    BLE16                NO                                          
         OC    PROGRAM,PROGRAM     IS REQUEST PROGRAM SPECIFIC                  
         BZ    BLE15                NO                                          
         CLC   PROGRAM,NPTXPROG    THIS PROGRAM                                 
         BE    BLE16                YES                                         
         B     BLEX                                                             
*                                                                               
* CHECK IF PROGAM SPECIFIC PATTERN NEEDED FOR THESE UNITS *                     
*                                                                               
BLE15    CLC   UNTPROG,NPTXPROG    THIS PROGRAM                                 
         BE    BLE16                YES                                         
         LA    R5,UNTNEXT                                                       
         CLI   UNTENT,0                                                         
         BNE   BLE15                                                            
         B     BLEX                                                             
*                                                                               
*                                                                               
*        USING NPTKEY,R4                                                        
*LE10    DS    0H                                                               
*        CLI   NPTKPROG,X'FF'      THIS A FEED OR DAYPART                       
*        BE    BLE16                YES                                         
*        OC    NPTKPROG,NPTKPROG   PATTERN PROGRAM SPECIFIC                     
*        BZ    BLE16                NO                                          
*        OC    PROGRAM,PROGRAM     IS REQUEST PROGRAM SPECIFIC                  
*        BZ    BLE15                NO                                          
*        CLC   PROGRAM,NPTKPROG    THIS PROGRAM                                 
*        BE    BLE16                YES                                         
*        B     BLEX                                                             
*        SPACE                                                                  
* CHECK IF PROGAM SPECIFIC PATTERN NEEDED FOR THESE UNITS *                     
*        SPACE                                                                  
*LE15    CLC   UNTPROG,NPTKPROG    THIS PROGRAM                                 
*        BE    BLE16                YES                                         
*        LA    R5,UNTNEXT                                                       
*        CLI   UNTENT,0                                                         
*        BNE   BLE15                                                            
******   B     BLEX                                                             
*                                                                               
*LE16    DS    0H                                                               
*NOP     BRAS  RE,SETNET           SET TO NET                                   
*        SPACE                                                                  
*        TM    SECFLAG,NECONPRD+NEMORPRD FULLY CONV OR DO BOTH                  
*******  BZ    *+8                                                              
*                                                                               
BLE16    BRAS  RE,SETXSP           SET TO XSPOT                                 
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING NPTDTAEL,R6                                                      
*                                                                               
         CLC   ENDATEB,NPTSTART    PERIOD END BEFORE PATTERN START              
         BL    BLEX                                                             
         CLC   STDATEB,NPTEND      PERIOD START AFTER PATTERN END               
         BH    BLEX                                                             
         MVI   ELEM+6,0                                                         
         TM    NPTSTAT,X'80'       TEST STATUS = DELETED                        
         BZ    BLE19                NO                                          
         B     BLEX                                                             
*                                                                               
BLE19    L     R2,NEXTADDR                                                      
         LA    R2,0(,R2)            CLEAR HOB                                   
         USING PATABLED,R2                                                      
*                                                                               
* ADD ENTRY TO PATTERN LIST IF SPACE *                                          
*                                                                               
         C     R2,MAXPATBL         TEST ROOM IN LIST                            
         BH    NOPTNRM                                                          
*                                                                               
         XC    0(L'PATENT+1,R2),0(R2)    CLEAR 1 ENTRY + 1 BYTE                 
         LH    RE,NXPATSEQ         BUMP SEQUENCE NUMBER                         
         LA    RE,1(RE)                                                         
         STH   RE,NXPATSEQ         AND SAVE                                     
*                                                                               
         OC    NPTXNET,NPTXNET     THIS ALL NETWORKS PATTERN                    
         BNZ   BLE20                NO                                          
         MVI   ELCODE,PATQALL      SET ALL NETS IND                             
         B     BLE50                                                            
*                                                                               
BLE20    CLI   NPTXNET,X'00'       IS THIS MEDIA SPECIFIC PATTERN               
         BNE   BLE22               NO                                           
         CLI   NPTXNET+2,X'FF'                                                  
         BNE   BLE22                                                            
         MVI   ELCODE,PATQMED      SET BY MEDIA                                 
         CLI   NPTXPROG,X'FF'      NOW CHECK IF ALSO DAYPART                    
         BNE   BLE50                                                            
         MVI   ELCODE,PATQDPMD     SET BY MEDIA & DAYPART                       
         MVC   PATDP,NPTXPROG+1                                                 
         B     BLE50                                                            
*                                                                               
BLE22    OC    NPTXPROG,NPTXPROG   THIS ALL PROGRAMS PATTERN                    
         BNZ   BLE24                NO                                          
         MVI   ELCODE,PATQNET      SET ALL PGMS IND                             
         B     BLE50                                                            
*                                                                               
BLE24    CLI   NPTXPROG,X'FF'      THIS FEED AND/OR DAYPART CD                  
         BE    BLE30                YES                                         
         MVI   ELCODE,PATQPROG     SET AS PROGRAM PATTERN                       
         MVC   PATPROG,NPTXPROG                                                 
         B     BLE50                                                            
*                                                                               
BLE30    OC    NPTXPROG+2(4),NPTXPROG+2 THIS A FEED PATTERN                     
         BNZ   BLE40                     YES                                    
         MVI   ELCODE,PATQDPT      SET AS DAYPART PATTERN                       
         B     BLE46                                                            
*                                                                               
BLE40    MVC   PATFEED,NPTXPROG+2                                               
         CLI   NPTXPROG+1,0        THIS A FEED DAYPART PATTERN                  
         BNE   BLE44                     YES                                    
         MVI   ELCODE,PATQFEED     SET AS FEED ONLY                             
         B     BLE50                                                            
*                                                                               
BLE44    MVI   ELCODE,PATQDPFD     SET AS FEED AND DAYPART                      
*                                                                               
BLE46    MVC   PATDP,NPTXPROG+1                                                 
*                                                                               
BLE50    MVC   PATSEQ,NXPATSEQ         MOVE SEQUENCE NUMBER                     
         MVC   PATREF,NPTXR3F          REF                                      
         MVC   PATPROD1,NPTXPRD                                                 
         MVC   PATULN,NPTXSLN                                                   
         MVC   PATPROD2,NPTXPRD2                                                
         MVC   PATULN2,NPTXSLN2                                                 
         MVC   PATYPE,ELCODE           SET ENTRY TYPE                           
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(3,NPTSTART),(2,PATSTR)                              
         CLC   NPTEND,=X'FFFFFF'                                                
         BNE   BLE56                                                            
         MVC   PATEND,ENDATEP          MOVE END PERIOD DATE                     
         OI    PATFLG,PATFLUFN     SET FLAG                                     
         B     BLE60                                                            
         FIXDT02                                                                
BLE56    GOTO1 (RF),(R1),(3,NPTEND),(2,PATEND)                                  
*                                                                               
BLE60    DS    0H                                                               
         MVC   PATDSKA,KEY+36          AND DISK ADDRESS                         
*                                                                               
         TM    NPTSTAT,NPTS_TIME       ELEM BIG ENOUGH TO HAVE TIME             
         BZ    *+10                    NO                                       
         MVC   PATSTIM(4),NPTSTIM      MOVE START/END TIMES                     
*                                                                               
         MVC   PATSEQNO,NPTS3QNO                                                
*                                                                               
BLE90    DS    0H                                                               
         LA    R2,L'PATENT(,R2)        NEXT PATTERN LIST ENTRY                  
         STCM  R2,7,NEXTADDR+1                                                  
BLEX     XIT1                                                                   
*                                                                               
NOPTNRM  DC    H'0'                                                             
         EJECT                                                                  
* SET PRD AND PRD2 IN ALPHA SEQ                                                 
*                                                                               
* SET KEY TYPE FOR MOR PRODS                                                    
*                                                                               
SETKEY   NTR1                                                                   
*                                                                               
*                                                                               
SET16F   CLI   SVTN1PR9,C'Y'       BYPASS ASSIGNED CMLS                         
         BNE   *+12                 NO                                          
         TM    UNTFLG,UNTFLGAS     PREV ASSIGNED                                
         BO    SET16X               YES                                         
*                                                                               
SET16G   DS    0H                                                               
         MVI   ELCODE,0                                                         
         USING NPTXKEY,R4                                                       
         OC    NPTXNET,NPTXNET     THIS ALL NETWORKS PATTERN                    
         BNZ   SET16H               NO                                          
         MVI   ELCODE,X'10'        SET ALL NETS IND                             
         B     SET16N                                                           
*                                                                               
SET16H   CLI   NPTXNET,X'00'       IS THIS MEDIA SPECIFIC PATTERN               
         BNE   SET16J              NO                                           
         CLI   NPTXNET+2,X'FF'                                                  
         BNE   SET16J                                                           
         MVI   ELCODE,X'08'        SET BY MEDIA                                 
         CLI   NPTXPROG,X'FF'      NOW CHECK IF ALSO DAYPART                    
         BNE   SET16N                                                           
         MVI   ELCODE,X'28'        SET BY MEDIA & DAYPART                       
         B     SET16N                                                           
*                                                                               
SET16J   OC    NPTXPROG,NPTXPROG   THIS ALL PROGRAMS PATTERN                    
         BZ    SET16N               YES                                         
*                                                                               
SET16K   CLI   NPTXPROG,X'FF'      THIS FEED AND/OR DAYPART                     
         BE    SET16L               YES                                         
         MVI   ELCODE,X'80'        SET AS PROGRAM PATTERN                       
         B     SET16N                                                           
*                                                                               
SET16L   OC    NPTXPROG+2(4),NPTXPROG+2 THIS A FEED PATTERN                     
         BNZ   SET16M                    YES                                    
         MVI   ELCODE,X'20'        SET AS DAYPART PATTERN                       
         B     SET16N                                                           
*                                                                               
SET16M   MVC   PATFEED,NPTXPROG+2                                               
         CLI   NPTXPROG+1,0        THIS A FEED DAYPART PATTERN                  
         BNE   *+12                      YES                                    
         MVI   ELCODE,X'40'        SET AS FEED ONLY                             
         B     SET16N                                                           
         MVI   ELCODE,X'60'        SET AS FEED AND DAYPART                      
*                                                                               
SET16N   CLC   UNTPKEY,ELCODE                                                   
         BNE   SET16X                                                           
*                                                                               
         CLC   UNTPROD,NPTXPRD                                                  
         BNE   SET16X                                                           
         CLC   UNTSLN,NPTXSLN                                                   
         BNE   SET16X                                                           
         CLC   UNTPROD2,NPTXPRD2                                                
         BNE   SET16X                                                           
         CLC   UNTSLN2,NPTXSLN2                                                 
         BNE   SET16X                                                           
*                                                                               
         CLC   UNTREF,NPTXR3F      SAME PAT REF?                                
         BNE   SET16X                                                           
*                                                                               
* CLEAR PAT INFO  (THIS PAT WAS DELETED)                                        
*                                                                               
         XC    UNTPSEQ,UNTPSEQ                                                  
         MVI   UNTPKEY,0                                                        
         XC    UNTREF,UNTREF       CLEAR PAT REF NUMBER                         
         XC    UNTCML1(16),UNTCML1 AND COMMERCIALS                              
         OI    UNTFLG1,UNTFL1DL    TURN ON DELETED PATTERN FOUND                
         OI    SVFLAG,SVDELPAT     DELETED PATTERN FOUND, UPDT UNIT             
*                                                                               
         L     RF,APRGTBLE                                                      
         USING PRGTABLD,RF                                                      
         CLC   UNTPROG,PRGPRG                                                   
         BE    *+20                                                             
         LA    RF,PRGNEXT                                                       
         OC    PRGPRG,PRGPRG                                                    
         BNZ   *-20                                                             
         DC    H'0'                                                             
*                                                                               
         LH    R1,PRGUNAS                                                       
         LA    R1,1(,R1)                                                        
         STH   R1,PRGUNAS                                                       
*                                                                               
*                                                                               
SET16X   LA    R5,UNTNEXT                                                       
         CLI   UNTADTEP,0          END OF UNITS                                 
         BNE   SET16F                                                           
         XIT1                                                                   
*                                                                               
         DROP  R2,R4,R5,R6                                                      
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* FIND CML AND CK FOR RELEASE/RECALL DATES AND DELETED STATUS *                 
*                                                                               
FCMLB    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,SETSPOT          SET TO SPOT                                  
*                                                                               
* BUILD CMML KEY *                                                              
*                                                                               
         XC    KEY(13),KEY                                                      
         LA    R1,KEY                                                           
         USING CMLKEY,R1                                                        
         MVC   CMLKID,=XL2'0A21'                                                
         MVC   CMLKAM,BAGYMD                                                    
         MVC   CMLKCLT,BCLT                                                     
         MVC   CMLKCML,WORK                                                     
         DROP  R1                                                               
         L     R6,AIO3                                                          
         ST    R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    FCMLB15                                                          
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         MVI   KEY+1,X'C1'         SEE IF AD-ID                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   FCMLBERC                                                         
*                                                                               
FCMLB15  MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R4,R6                                                            
         USING CMLDTAEL,R4                                                      
         TM    CMLSTAT,X'80'       CMML DELETED                                 
         BO    FCMLBERA                                                         
*                                                                               
         L     R6,AIO1                                                          
*                                                                               
         BRAS  RE,SETXSP           SET TO XSPDIR/FIL                            
*                                                                               
*NOP     TM    SECFLAG,NECONPRD+NEMORPRD FULLY CONV OR DO BOTH                  
*        BNZ   *+8                                                              
*        SPACE                                                                  
******   BRAS  RE,SETNET           SET TO NET                                   
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,SETSPOT          SET TO SPOT                                  
*                                                                               
         USING NPTDTAEL,R6                                                      
*                                                                               
         CLC   CMLRCL,NPTSTART     CML END BEFORE PATTERN START                 
         BL    FCMLBERB                                                         
         CLC   CMLRLSE,NPTEND      CML START AFTER PATTERN END                  
         BH    FCMLBERB                                                         
         MVC   DUB(1),CMLSLN       SAVE CML LEN                                 
         CR    RB,RB                                                            
         B     FCMLBX                                                           
FCMLBERA MVC   DUB(7),=C'DELETED'                                               
         B     FCMLBER                                                          
FCMLBERB MVC   DUB(7),=CL7'REL/RCL'                                             
         B     FCMLBER                                                          
FCMLBERC MVC   DUB(7),=CL7'NOT FND'                                             
FCMLBER  LTR   RB,RB                                                            
*                                                                               
FCMLBX   XIT1                                                                   
*                                                                               
         DROP  R4,R6                                                            
         LTORG                                                                  
         EJECT                                                                  
* PRINT ERROR REPORT *                                                          
*                                                                               
         DS    0H                                                               
PER      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    WHEN,X'20'          THIS RUNNING SOON                            
         BZ    PER02                NO                                          
*                                                                               
         TM    SVOPTSW,OPTTEST     THIS A TEST RUN                              
         BO    PER02                YES                                         
*                                                                               
         CLI   OFFLINE,C'Y'        OFFLINE?                                     
         BE    PER01                                                            
*                                                                               
         CLC   =C'WD',AGENCY       IF AGENCY WEIDEN                             
         BE    PER01                                                            
*                                                                               
         CLC   =C'M2',AGENCY       IF AGENCY MEDIACOM                           
         BE    PER01                                                            
*                                                                               
         CLC   =C'YN',AGENCY       IF AGENCY YNRT                               
         BE    PER01                                                            
*                                                                               
         CLC   =C'H7',AGENCY       IF AGENCY MSYRT                              
         BE    PER01                                                            
*                                                                               
         CLC   =C'FM',AGENCY       AND AGENCY FMNES                             
         BNE   PER02                                                            
*                                                                               
* UNLOCK FOR SOON UPDATES                                                       
*                                                                               
PER01    DS    0H                                                               
*                                                                               
* IF RUNNING FOR ALL PRODUCTS, UNLOCK ALL PRODS                                 
*                                                                               
         CLI   SVTN2PRO+00,C'0'                                                 
         BE    *+12                                                             
         CLI   SVTN2PRO+00,0                                                    
         BNE   ULOCK30             NOT AN ALL PRODUCT RUN                       
*                                                                               
         MVI   BYTE,0                                                           
         B     ULOCK100                                                         
*                                                                               
* IF RUNNING BY PRODUCT, UNLOCK THIS PRD                                        
*                                                                               
ULOCK30  CLI   SVTN2PRO+00,C'*'                                                 
         BNE   ULOCK50                                                          
*                                                                               
         MVC   BYTE,SVTN2PRO+00                                                 
         B     ULOCK100                                                         
*                                                                               
* IF RUNNING BY PGROUP, UNLOCK THIS PGROUP                                      
*                                                                               
ULOCK50  OC    OPTPRGR,OPTPRGR     PRGOUP                                       
         BNZ   *+6                                                              
         DC    H'0'                BUG CATCHER                                  
         MVC   BYTE,SVTN2PRO+00                                                 
*                                                                               
ULOCK100 MVC   DUB,SVLOCK          MOVE LOCK INFO                               
         MVC   SVCPROD,SVSVPROD    MOVE PROD LOCK INFO                          
         MVI   DUB,C'U'                                                         
         GOTO1 VALILOC,SVCPROD                                                  
*                                                                               
PER02    L     R2,=A(WORKFIL)                                                   
         CLOSE ((R2),)                                                          
         FREEPOOL (R2)                                                          
         LA    R2,ERREPT                                                        
*                                                                               
         TM    WHEN,X'20'          THIS RUNNING SOON                            
         BZ    *+10                                                             
         MVC   40(8,R2),=CL8'TALWRK'                                            
*                                                                               
         OPEN  ((R2),INPUT)                                                     
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,PEREOF                                                        
         STCM  RE,7,ERREPT+33                                                   
*                                                                               
         CLI   PQSW,1                                                           
         BNE   PER04                                                            
         GOTO1 OPENPQ                                                           
*                                                                               
* CLEAR HEAD AND MIDLINES FROM SEED *                                           
*                                                                               
PER04    LA    R0,14                                                            
         LA    R1,H1                                                            
         MVC   0(132,R1),SPACES                                                 
         LA    R1,132(R1)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
*                                                                               
         XC    SPECS,SPECS                                                      
         XC    HEADHOOK,HEADHOOK                                                
         XC    FOOTHOOK,FOOTHOOK                                                
*                                                                               
         CLI   SVTN1PR4,C'Y'       DON'T PRINT REPORT                           
         BNE   PER06                                                            
         MVC   P+20(35),=C'** NET SEED REPORT WAS PROCESSED **'                 
         OC    SVNET,SVNET                                                      
         BZ    *+22                                                             
         MVC   P+53(12),=C'FOR NETWORK '                                        
         MVC   P+65(4),SVNET                                                    
         MVC   P+70(2),=C'**'                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PER06    LA    RE,PHDG                                                          
         ST    RE,SPECS                                                         
         LA    RE,PHDHK                                                         
         ST    RE,HEADHOOK                                                      
         MVI   FORCEHED,C'Y'                                                    
*        MVC   PAGE,=H'1'                                                       
         EJECT                                                                  
PER10    LA    R1,ERREPT                                                        
         LA    R0,ELEMENT                                                       
         LR    R3,R0                                                            
         USING XRECD,R3                                                         
         GET   (1),(0)                                                          
*                                                                               
         LA    R7,P                                                             
         USING XPRINT,R7                                                        
         MVC   P,SPACES                                                         
*                                                                               
         CLI   XTYPE,5                                                          
         BE    PER35                                                            
*                                                                               
         MVC   XPPROG,XPROG                                                     
*                                                                               
* READ PROGRAM RECORD FOR STANDARD PROGRAM INFORMATION *                        
*                                                                               
         BRAS  RE,SETSPOT          SET TO SPOT                                  
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NPGKEY,R4                                                        
         MVC   NPGKTYP,=X'0D20'                                                 
         MVC   NPGKAM,XBAGYMD                                                   
         MVC   NPGKNET,XNETMKT                                                  
         MVC   NPGKPROG,XPROG                                                   
         MVC   NPGKEND,XPERND                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE                                                  
         BE    PER12                                                            
         MVC   KEY,KEYSAVE                                                      
         MVC   NPGKEND,XPERST                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE                                                  
         BE    PER12                                                            
         MVC   KEY,KEYSAVE                                                      
         XC    NPGKEND,NPGKEND                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
PER12    L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         AH    R6,DATADISP                                                      
PER14    CLI   0(R6),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R6),X'92'                                                      
         BE    PER16                                                            
         SR    RF,RF                                                            
         ICM   RF,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,RF                                                            
         B     PER14                                                            
         USING NPGEL92,R6                                                       
PER16    MVC   XPPROGNM,NPGNAME                                                 
*                                                                               
         MVC   BYTE,NPGDAY                                                      
         CLI   NPGROT,0            NPGROT OVERRIDES DAY IF PRESENT              
         BE    *+10                                                             
         MVC   BYTE,NPGROT                                                      
*                                                                               
         GOTO1 UNDAY,(R1),BYTE,XPPROGDA                                         
*                                                                               
         GOTO1 UNTIME,(R1),NPGTIME,XPPROGTM                                     
         FIXDT02                                                                
         GOTO1 DATCON,(R1),(2,XPERST),(5,XPPERIOD)                              
*                                                                               
         CLI   XTYPE,4             EQUIV PROG CODE WITH PARTIAL PERIOD          
         BE    PER18                                                            
         MVI   XPPERIOD+8,C'-'                                                  
         FIXDT02                                                                
         GOTO1 (RF),(R1),(2,XPERND),(5,XPPERIOD+9)                              
         CLI   XTYPE,1             SOME UNASSIGNED UNITS                        
         BE    PER20                                                            
         CLI   XTYPE,2             UNALLOCATED UNITS                            
         BE    PER30                                                            
         CLI   XTYPE,6             TRAFFIC SUPPLIER ERROR                       
         BE    PER50                                                            
         CLI   XTYPE,7             TRI-BACK UNITS FOUND                         
         BE    PER60                                                            
         CLI   XTYPE,3             NO ASSIGNED UNITS                            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   XPEXPLAN(36),=C'NO COMMERCIALS ASSIGNED TO ANY UNITS'            
         B     PER100                                                           
PER18    MVC   XPEXPLAN(37),=C'EQUIV PROG - NO BASE COVERS THIS UNIT'           
         B     PER100                                                           
*                                                                               
PER20    EDIT  XUNAS,(4,XPEXPLAN),ALIGN=LEFT                                    
         LR    RF,R0                                                            
         LA    R1,XPEXPLAN+1(RF)                                                
         MVC   0(16,R1),=CL16'UNASSIGNED UNITS'                                 
         OC    XUNAL,XUNAL         ANY UNALLOCATED                              
         BZ    PER100                                                           
         LA    R7,132(,R7)                                                      
         EDIT  XUNAL,(4,XPEXPLAN),ALIGN=LEFT                                    
         LR    RF,R0                                                            
         LA    R1,XPEXPLAN+1(RF)                                                
         MVC   0(17,R1),=CL17'UNALLOCATED UNITS'                                
         B     PER100                                                           
*                                                                               
PER30    EDIT  XUNAL,(4,XPEXPLAN),ALIGN=LEFT                                    
         LR    RF,R0                                                            
         LA    R1,XPEXPLAN+1(RF)                                                
         MVC   0(17,R1),=CL17'UNALLOCATED UNITS'                                
         B     PER100                                                           
*                                                                               
PER35    DS    0H                                                               
         LA    R6,XPPROG                                                        
         MVC   2(11,R6),=C'PATTERN KEY'                                         
         LA    R6,14(R6)                                                        
         CLI   XPROD1,0            ANY PRD ?                                    
         BE    PER36                NO                                          
*        LA    R1,XPRD1                                                         
*        BAS   RE,PPRD                                                          
*        CLI   WORK,C' '                                                        
         CLI   XPROD1,C' '                                                      
         BE    PER36                                                            
         MVC   0(4,R6),=C'PRD='                                                 
*        MVC   4(3,R6),WORK                                                     
         MVC   4(3,R6),XPROD1                                                   
         LA    R6,8(R6)                                                         
         CLI   XPROD2,0                                                         
         BE    PER36                                                            
*        LA    R1,XPRD2                                                         
*        BAS   RE,PPRD                                                          
*        CLI   WORK,C' '                                                        
         CLI   XPROD1,C' '                                                      
         BE    PER36                                                            
         MVC   0(4,R6),=C'PTR='                                                 
*        MVC   4(3,R6),WORK                                                     
         MVC   4(3,R6),XPROD2                                                   
         LA    R6,8(R6)                                                         
*                                                                               
PER36    DS    0H                                                               
         MVC   0(7,R6),=C'PERIOD='                                              
*                                                                               
         GOTO1 DATCON,DMCB,(3,XDATEST),(5,7(R6))                                
         MVI   15(R6),C'-'                                                      
         CLC   XDATEND,=XL3'FFFFFF'                                             
         BNE   PER36C                                                           
         MVC   16(3,R6),=CL3'UFN'                                               
         LA    R6,20(R6)                                                        
         B     PER37                                                            
*                                                                               
PER36C   DS    0H                                                               
         GOTO1 DATCON,DMCB,(3,XDATEND),(5,16(R6))                               
         LA    R6,24(R6)                                                        
*                                                                               
PER37    DS    0H                                                               
         MVC   1(4,R6),=C'REF='                                                 
         LA    R6,5(R6)                                                         
         SR    R5,R5                                                            
         ICM   R5,3,XREF                                                        
         X     R5,=XL4'0000FFFF'                                                
         EDIT  (R5),(4,0(R6)),ALIGN=LEFT                                        
         CLI   0(R6),C' '                                                       
         BNH   *+12                                                             
         LA    R6,1(R6)                                                         
         B     *-12                                                             
         LA    R6,1(R6)                                                         
*                                                                               
         MVC   0(4,R6),=C'CML='                                                 
         MVC   4(8,R6),XCML                                                     
         CLI   XSUBTYPE,0                                                       
         BNE   *+14                                                             
         MVC   15(18,R6),=C'*PAT PROD CML ERR*'                                 
         B     PER37C                                                           
         CLI   XSUBTYPE,1                                                       
         BNE   *+14                                                             
         MVC   15(20,R6),=C'*DELETED COMMERCIAL*'                               
         B     PER37C                                                           
         CLI   XSUBTYPE,2                                                       
         BNE   *+14                                                             
         MVC   15(19,R6),=C'*REL/RCL DATES ERR*'                                
         B     PER37C                                                           
         CLI   XSUBTYPE,3                                                       
         BNE   *+10                                                             
         MVC   15(20,R6),=C'*COMMERCIAL NOT FND*'                               
*                                                                               
PER37C   GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         LA    R6,XPPROGNM                                                      
         CLI   XNET,X'00'           TEST FOR PATTERN BY MEDIA                   
         BNE   PER38                                                            
         CLI   XNET+2,X'FF'                                                     
         BNE   PER38                                                            
         MVC   0(2,R6),=C'M='                                                   
         MVC   2(1,R6),XNET+1                                                   
         LA    R6,4(R6)                                                         
         B     PER38C                                                           
*                                                                               
PER38    DS    0H                                                               
         CLI   XNET,X'00'           TEST FOR NETWORKPATTERN                     
         BE    PER38C                                                           
         MVC   0(4,R6),=C'NET='                                                 
         MVC   4(4,R6),XNET                                                     
         LA    R6,9(R6)                                                         
PER38C   CLI   XPROG,X'FF'                                                      
         BE    PER38F                                                           
         CLI   XPROG,C' '                                                       
         BNH   PER100                                                           
         MVC   0(5,R6),=C'PROG='                                                
         MVC   5(6,R6),XPROG                                                    
         B     PER100                                                           
*                                                                               
PER38F   DS    0H                                                               
         CLI   XPROG+1,C' '                                                     
         BNH   *+20                                                             
         MVC   0(5,R6),=C'CODE='                                                
         MVC   5(1,R6),XPROG+1                                                  
         LA    R6,11(R6)                                                        
         CLI   XPROG+2,C' '                                                     
         BNH   *+16                                                             
         MVC   0(5,R6),=C'FEED='                                                
         MVC   5(4,R6),XPROG+2                                                  
         B     PER100                                                           
*                                                                               
PER50    MVC   XPEXPLAN(30),=C'ALL UNITS MUST HAVE SAME TSUPP'                  
         B     PER100                                                           
*                                                                               
PER60    MVC   XPEXPLAN(20),=C'TRI-BACK UNITS FOUND'                            
*                                                                               
PER100   GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PER10                                                            
PEREOF   CLOSE ((2),)                                                           
         FREEPOOL (R2)                                                          
*                                                                               
         TM    UPDSW,X'01'        CML PRD ERR FOUND                             
         BZ    PERX                                                             
         MVC   P+20(37),=C'** FATAL ERROR ** NO SEED OCCURRED **'               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PERX     MVI   FORCEHED,C'Y'                                                    
         XIT1                                                                   
         EJECT                                                                  
* PRINT PRODUCT CODE AND SPOT LENGTH                                            
* THIS ROUTINE IS EFFECTIVELY DEFUNCT - THERE IS NO REFERENCE TO IT             
PPRD     NTR1                                                                   
         LA    R5,WORK             ADDRESS OF OUTPUT AREA                       
         MVC   WORK,SPACES                                                      
         LA    RE,NCLSTSIZ                                                      
         L     RF,ASVNCLST         ADDRESS OF SAVED C LIST (VALICLT)            
PPRD10   DS   0H                                                                
         CLC   0(1,R1),3(RF)                                                    
         BE    PPRD12                                                           
         LA    RF,4(RF)                                                         
         CLI   0(RF),C' '                                                       
         BNH   PPRD15X                                                          
         BCT   RE,PPRD10                                                        
         B     PPRD15X                                                          
*                                                                               
PPRD12   MVC   0(3,R5),0(RF)                                                    
PPRD15X  XIT1                                                                   
         EJECT                                                                  
* HEAD HOOK ROUTINE                                                             
*                                                                               
PHDHK    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    SVOPTSW,OPTTEST     TEST OPTION TEST                             
         BZ    *+10                                                             
         MVC   H4+85(16),=C'*** TEST RUN ***'                                   
*                                                                               
         MVC   H3+10(3),QCLT                                                    
         MVC   H3+15(20),CLTNM                                                  
*                                                                               
         MVC   H4+10(4),NETWORK                                                 
*                                                                               
PHDHKX    XIT1                                                                  
*                                                                               
PHDG     SSPEC H1,3,AGYNAME                                                     
         SSPEC H2,3,AGYADD                                                      
         SSPEC H3,3,C'CLIENT'                                                   
         SSPEC H4,3,C'NETWORK'                                                  
         SSPEC H1,46,C'COMMERCIAL SEED ERROR LISTING'                           
         SSPEC H2,46,C'-----------------------------'                           
         SSPEC H1,85,REPORT                                                     
         SSPEC H2,85,RUN                                                        
         SSPEC H3,85,PAGE                                                       
         SSPEC H6,03,C'PROGRAM-NAME'                                            
         SSPEC H7,03,C'-------------'                                           
         SSPEC H6,32,C'DAY         TIME'                                        
         SSPEC H7,32,C'----        -----'                                       
         SSPEC H6,56,C'PERIOD              REASON'                              
         SSPEC H7,56,C'-----------------   ------------------'                  
         DC    X'00'                                                            
*                                                                               
ERREPT   DCB   BLKSIZE=4800,                                           X        
               DDNAME=WORKFIL,                                         X        
               DSORG=PS,                                               X        
               EODAD=PEREOF,                                           X        
               LRECL=48,                                               X        
               MACRF=GM,                                               X        
               RECFM=FB                                                         
         LTORG                                                                  
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
         EJECT                                                                  
       ++INCLUDE SPTRNREV                                                       
         EJECT                                                                  
       ++INCLUDE SPTRNEQPRG                                                     
         EJECT                                                                  
       ++INCLUDE DDACTIVD                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
       ++INCLUDE SPTRNPAT                                                       
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
       ++INCLUDE SPGENPROG                                                      
         EJECT                                                                  
       ++INCLUDE SPTRCMML                                                       
         TITLE 'T21662 - NETWORK COMMERCIAL SEED - DSECTS'                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
NETBLCKD DSECT                                                                  
       ++INCLUDE NETBLOCKN                                                      
       ++INCLUDE SPTRAFFD                                                       
         ORG   CONTAGH                                                          
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPTRA72D                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE FASSB                                                          
       ++INCLUDE SPTRAWORKD                                                     
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
         PRINT ON                                                               
         EJECT                                                                  
NEXTLINE EQU   TRASEL2H-TRASEL1H                                                
*                                                                               
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
SPTR62RR DS    A                                                                
QSORT    DS    A                                                                
VTRPACK  DS    A                                                                
ASVSTOR  DS    A                                                                
GMBUFF   DS    A                                                                
GMBUFL   DS    A                                                                
AUNTABLE DS    A                                                                
AUNTABLX DS    A                                                                
APATABLE DS    A                   ADDRESS OF PATTERN TABLE                     
APATABLX DS    A                                                                
MAXPATBL DS    A                                                                
APATROT  DS    A                   ADDRESS OF PATTERN ROTATION TABLE            
APATROTX DS    A                                                                
APRGTBLE DS    A                   ADDRESS OF PROGRAM TABLE                     
APRGTBLX DS    A                                                                
AADIDTAB DS    A                   ADDRESS OF ADID TABLE                        
AADIDTBX DS    A                                                                
ANETIO   DS    A                                                                
AWORKFIL DS    A                                                                
ANETBLK  DS    A                                                                
NEXTADDR DS    A                                                                
ASVNEXT  DS    A                   1ST TABLE ENTRY ON NEXT SCREEN               
*                                                                               
BASEPRG  DS    CL6                                                              
BASEDTS  DS   0XL4                                                              
BASESTR  DS    XL2                                                              
BASEEND  DS    XL2                                                              
*                                                                               
BASECT   DS    XL1                                                              
*                                                                               
SVNET    DS    CL4                 SAVE CURRENT NETWORK FOR NET=ALL             
SVLOCK   DS    CL8                 SAVE LOCK INFO FOR UNLOCK                    
SVSVPROD DS    CL3                 SAVE PROD LOCK INFO FOR UNLOCK               
SVCPROD  DS    CL3                 3 CHAR PROD FOR LOCKING                      
SVQPRD   DS    CL3                 SAVED PROD FROM QPRD FOR VALILOC TST         
*                                                                               
         DS    0H                                                               
HOLDDIR  DS    CL8                                                              
HOLDFIL  DS    CL8                                                              
*                                                                               
MYPSTIM  DS    H                                                                
MYPETIM  DS    H                                                                
MYUSTIM  DS    H                                                                
MYUETIM  DS    H                                                                
*                                                                               
UPDREVSW DS    XL1                                                              
HIREVNN  DS    C                   NETWORK REV NUM                              
OREVNN   DS    X                   OTHER NETWORK REV NUM                        
OREVNUM  DS    X                   OTHER PROGRAM REV NUM                        
SVMEDIA  DS    C                   SAVE MEDIA                                   
SVCMLCOD DS    CL8                                                              
SVCMLLEN DS    XL1                                                              
SVCMLOVR DS    XL2                 PRINT OVERRIDE 1 & 2                         
SVCMLDS  DS    CL24                COMMERCIAL DESC                              
SVCMLDS2 DS    CL24                COMMERCIAL EXT DESC 2                        
SVCMLDS3 DS    CL24                COMMERCIAL EXT DESC 3                        
SVUNTDTE DS    XL4                                                              
SVACTPRG DS    CL6                                                              
SVHIREVP DS    CL1                                                              
* SVUNTCOM DS    CL32  *CAUSED BUG* SAVE COMMON STUFF FROM UNIT TABLE           
SVUNTCOM DS    CL(L'UNTCOM)        SAVE COMMON STUFF FROM UNIT TABLE            
*                                                                               
MYTN2PR6 DS    C                                                                
MYTN2PR8 DS    C                                                                
*                                                                               
*SVTS     DS    CL5                 TRAFFIC SUPPLIER                            
*SVTSLEN  DS    CL1                 (TS) ENTRY LENGTH                           
SVOPTMED DS    CL1                 MEDIA (FOR ALL NETWORK REQUEST)              
         DS    CL2                 WAS LEN 3 FOR NO REASON (I HOPE-BG)          
TESTBYTE DS    XL1                                                              
UNITCNT  DS    PL3                                                              
*                                                                               
         DS    0H                                                               
SVPRGENT DS    0CL30               FROM PRGTABLD                                
SVPRG    DS    CL6                                                              
SVPRGUNT DS    H                   TOTAL UNITS                                  
SVPRGUNA DS    H                   UNASSIGNED UNITS                             
SVPRGUAL DS    H                   UNALLOCATED UNITS                            
SVPRGFDS DS    H                   TOTAL FEEDS                                  
SVPRGDSP DS    XL2                                                              
SVPRGRVN DS    XL1                                                              
SVPRGIDT DS    XL3                                                              
SVPRGFLG DS    XL1                                                              
SVTSPRT  DS    CL1                 TRAFFIC SUPPLIER POINTER                     
*                                  FROM SEED PROCESS                            
SVPRGCTS DS    0XL8                                                             
SVPRGNEW DS    H                   NEWLY ASSIGNED UNITS                         
SVPRGCHG DS    H                   CHANGE TO ASSIGNED UNITS                     
SVPRGASS DS    H                   PREVIOSLY ASSIGNED CML                       
SVPRGTBA DS    H                   CMLNAME=TBA                                  
*                                  FROM PROGRAM RECORD                          
SVPRGDAY DS    CL1                                                              
SVPRGTIM DS    CL4                                                              
SVPRGNM  DS    CL16                                                             
SVCSPROD DS    XL18                SAVE MULTI-PROD COPY SPLIT PRODS             
*                                                                               
STDATE   DS    CL6                                                              
ENDATE   DS    CL6                                                              
         EJECT                                                                  
* FROM VPER RTN - PERIOD FIELD IN HEADING ON SCREEN                             
*                                                                               
STDATEB  DS    XL3                                                              
ENDATEB  DS    XL3                                                              
STDATEP  DS    XL2                                                              
ENDATEP  DS    XL2                                                              
PERIOD   DS    XL3                                                              
*                                                                               
         DS    0H                                                               
CVRDAYS  DS    H                                                                
NXPATSEQ DS    H                                                                
SVPATSEQ DS    CL16                2 BYTE ENTRIES                               
SVUNTDTS DS   0XL4                                                              
SVUNTSTR DS    XL2                                                              
SVUNTEND DS    XL2                                                              
*                                                                               
SVNUCML1 DS    CL8                                                              
SVNUCML2 DS    CL8                                                              
*                                                                               
NETMKT   DS    H                                                                
NETWORK  DS    CL4                 KEEP NETWORK/PROGRAM TOGETHER                
PROGRAM  DS    CL6                     AND IN ORDER                             
TABLESW  DS    XL1                 Y=TABLE BUILT                                
UPDSW    DS    CL1               80 NO PATTERNS FOUND FOR THIS PROG             
*                                40 END OF UNITS IN BLPRG                       
*                                20 PRINTING UNITS, PRT MIDLNS IN HDHK          
*                                10 UNITS SEEDED FOR PROGRAM SEND DIP           
*                                08 UNITS HAVE BILLBOARD OR POS (BLUNT)         
*                                04 UNITS TO UPDATE (IN UPDR RTN)               
*                                02 BLPRG RTN - UNITS FOUND                     
FTLCMLER EQU  X'01'              01 VCMLPRD - CML PRD ERR FOUND                 
*                    X'FF' NO UNTS FOUND BYPAASS THIS NET (FOR NET=ALL)         
*                                                                               
PRDMATSW DS    XL1                80 - PRODUCT FOUND IN COMMERCIAL              
*                                 40 - PARTNER FOUND IN CML                     
*                                 08 - PRD/PTR SPOT LENS=CML LEN                
*                                    -CML IS FOR PIGGYBACK PAIR                 
*                                 02 - LOOKING FOR PTR                          
*                                 01 - CML ERR FOUND                            
BYTE1    DS    X                                                                
*                                                                               
SVFLAG   DS    XL1                                                              
*                                                                               
*        EQU   X'80'                                                            
*        EQU   X'40'                                                            
CONVSW   EQU   X'20'               CONVERTED RECORDS                            
SVDELPAT EQU   X'10'               DELETED PATTERN FOUND, UPDT UNIT             
*                                                                               
*                                                                               
SVOPTDTA DS    0CL(OPTEND-SVOPTSW)                                              
*                                                                               
*SVOPTTS  DS    CL5                 SAVE TSUPP ENTERED IN OPTION FIELD          
*SVOTSLEN DS    CL1                 AND ITS LENGTH                              
*                                                                               
SVCMLTBA DS    CL8                                                              
*                                                                               
SVOPTSW1 DS    XL1                                                              
OPTPER   EQU   X'80'              PERIOD OVERRIDE                               
*MNV                                                                            
OPTDIGI  EQU   X'40'              OVERRIDE SUPPRESS DIGITAL                     
*MNV                                                                            
*                                                                               
SVOPTSW  DS    XL1                                                              
OPTTEST  EQU   X'80'              NO UPDATES TO DISK FILES                      
OPTRACE  EQU   X'40'                                                            
OPTMED   EQU   X'20'              SEED BY MEDIA                                 
OPTPGEN  EQU   X'10'              REQUEST WAS FROM PATTERN GEN                  
*OPTNTS   EQU   X'08'              NON-TRAFFIC SUPPLIER REQUEST                 
*OPTTS    EQU   X'04'              FILTER ON TRAFFIC SUPPLIER                   
OPTCLR   EQU   X'02'               CLEAR ALL COMMLS BEFORE SEEDING              
OPTPTTN  EQU   X'01'                                                            
*                                                                               
*                                                                               
OPTPRGR  DS    XL2                 SCHEME IS IN SVTN2PRO+00                     
OPTPROD  DS    CL3                 KEEP OPTPROD & OPTPRD TOGETHER               
OPTDATE  DS    XL2                 PARTIAL PERIOD DATE                          
OPTDAT2  DS    XL2                 END DATE OF THE DATE RANGE                   
OPTLEN   DS    X                   BINARY SPOT LENGTH                           
OPTEND   EQU   *                                                                
*                                                                               
PRGTU    DS    PL4                 TOT UNITS FOR PROG                           
PRGCU    DS    PL4                 PROG - CHANGED ASSIGNS                       
PRGNOR   DS    PL4                 PROG - NO PAT REF - PRIOR                    
PRGSA    DS    PL4                 PROG - SAME ASSIGN                           
*                                                                               
NETTU    DS    PL4                 TOT UNITS FOR PROG                           
NETCU    DS    PL4                 PROG - CHANGED ASSIGNS                       
NETNOR   DS    PL4                 PROG - NO PAT REF - PRIOR                    
NETSA    DS    PL4                 PROG - SAME ASSIGN                           
*                                                                               
ALLTU    DS    PL4                 TOT UNITS FOR PROG                           
ALLCU    DS    PL4                 PROG - CHANGED ASSIGNS                       
ALLNOR   DS    PL4                 PROG - NO PAT REF - PRIOR                    
ALLSA    DS    PL4                 PROG - SAME ASSIGN                           
TOTCTRS  EQU   (*-PRGTU)/4                                                      
*                                                                               
FLDH     DS    CL8                                                              
FLD      DS    CL40                                                             
*                                                                               
WRK2     DS    CL20                USED IN VOPT TO BUILD DUMMY FIELD            
SVLGKEY  DS    CL(L'KEYSAVE)                                                    
*                                                                               
* EQUIVALENT PROGRAM TABLE                                                      
*                                                                               
EQVPTBL  DS    CL530                                                            
*                                                                               
* TRAFFIC SUPPLIER TABLE                                                        
*                                                                               
*TSUPTBL  DS     XL(15*TSUPLEN)     TRAFFIC SUPPLIER TABLE                      
*TSUPLEN  EQU    5                          AND ITS FIELD LENGTH                
*                                                                               
         DS    0D                                                               
NETBLK   DS    1024C                                                            
NETBLKX  EQU   *                                                                
         ORG   NETBLK                                                           
TIMECNT  DS    F                                                                
TIMELIST DS    1020C                                                            
TIMELSTX EQU   *                                                                
         ORG                                                                    
*                                                                               
* PRODUCT GROUP TABLE                                                           
*                                                                               
OPTPGRL  DS    XL1500              HOLDS 500 PRODUCTS (3 CHAR EACH)             
ENDPGTBL EQU   *                                                                
*                                                                               
         DS    0D                                                               
PRGTABLE DS     XL(90*L'PRGENT)    PROGRAM TABLE                                
PRGTBLX  EQU   *                                                                
PRGTBLMO EQU    400*L'PRGENT       160 MORE PROG ENTRIES (OFFLINE)              
*                                                                               
         DS    XL2                                                              
ENDSYSD  EQU   *       IF THIS ADDRESS MORE THAN 2F08, PAST END OF SYSD         
         EJECT                                                                  
* DSECT FOR PROGRAM TABLE LIST ENTRIES *                                        
*                                                                               
PRGTABLD DSECT                                                                  
PRGENT   DS    0CL30                                                            
PRGPRG   DS    CL6                PROGRAM                                       
PRGUNITS DS    XL2                TOTAL UNITS                                   
PRGUNAS  DS    XL2                TOTAL UNITS UNASSIGNED (NO CMLS)              
PRGUNAL  DS    XL2                TOTAL UNITS UNALLOCATED (NO PRD)              
PRGFDS   DS    XL2                TOTAL FEEDS                                   
PRGDISP  DS    XL2                DISP TO SCREEN ENTRY                          
PRGREVN  DS    XL1                REVISION NUMBER                               
PRGINSDT DS    XL3                LAST INSTR DATE                               
PRGFLG   DS    XL1                                                              
*PRGXCLUD EQU   X'80' NOT USED     80 - EXCLUDE THIS PROGRAM                    
*                                 40 - UNITS SEEDED FOR THIS PROGRAM            
PRGFLTRN EQU   X'20'              20 - INSTR WERE RUN FOR THIS                  
*                                       REVISION NUMBER                         
PRGFLGNW EQU   X'10'              10 - NEED TO ADD ORIG REV REC                 
*                                    (CREATE ORG REV REV FOR THIS PROG)         
PRGTSPTR DS    XL1   *** NOT USED*** TRAFFIC SUPPLIER POINTER                   
PRGCTS   DS    0XL8                                                             
PRGNEW   DS    XL2                                                              
PRGCHG   DS    XL2                                                              
PRGASS   DS    XL2                                                              
PRGTBA   DS    XL2                                                              
PRGNEXT  EQU   *                                                                
         EJECT                                                                  
* DSECT FOR UNIT ACTIVITY LIST ENTRIES *                                        
*                                                                               
UNTABLED DSECT                                                                  
UNTENT   DS    0CL64               IF THIS IS >60 ADJUST IN AIO2                
UNTCOM   DS    0CL36                                                            
UNTSRT   DS    0CL27                                                            
UNTPROD  DS    CL3                                                              
UNTSLN   DS    XL1                UNIT LENGTH                                   
UNTPROD2 DS    CL3                                                              
UNTSLN2  DS    XL1                UNIT LENGTH                                   
UNTADTEP DS    XL2                AIR DATE                                      
UNTADTE2 DS    XL2                AIR DATE PLUS ROTATION DAYS                   
UNTTIME  DS    CL4                TIME                                          
UNTPROG  DS    CL6                PROGRAM CODE                                  
UNTSUB   DS    XL1                SUBLINE                                       
UNTFEED  DS    CL4                FEED                                          
*                   END OF UNTSRT                                               
UNTDP    DS    CL1                DAY PART CODE                                 
UNTDSKAD DS    XL4                DISK ADDRESS                                  
UNTELSEQ DS    XL1                 SEQ OF ELEMENT IN RECORD                     
UNTEQVCT DS    XL1                ENTRY CT FOR EQUIV PROG CODE                  
UNTFLG   DS    XL1                                                              
UNTFLGRE EQU   X'80'               80/40/20 ON IN NUCMLFLG (REASSIGN)           
UNTFLGNW EQU   X'40'               COMMLS NEW (NONE ASSIGNED BEFORE)            
UNTFLGCH EQU   X'20'               COMMLS CHANGED                               
UNTFLGAS EQU   X'10'               PREVIOUSLY ASSIGNED CML                      
UNTFLGSW EQU   X'08'               PRODS SWAPPED IN UNIT FOR PTTN MATCH         
UNTFLGCS EQU   X'04'               COPY SPLIT                                   
UNTFLGUL EQU   X'02'               UNALLOCATED                                  
UNTFLGPD EQU   X'01'               PARTIAL DATES                                
*                                                                               
UNTFLG1  DS    XL1                                                              
UNTFL1SW EQU   X'80'               CMLS WERE SWAPPED , UPDATE THIS UNIT         
UNTFL1FD EQU   X'40'               FEED NO NATIONAL                             
UNTFL1DL EQU   X'20'               DELETED PAT FOUND, UPDATE THIS UNIT          
*                   END OF UNTCOM                                               
*                                                                               
UNTCML1  DS    CL8                 ASSIGNED COMMERCIAL                          
UNTCML2  DS    CL8                 ASSIGNED COMMERCIAL                          
UNTREF   DS    XL3                 PAT REF                                      
UNTREVN  DS    XL1                                                              
UNTEXNAM DS    XL1                 PTR-UNIT EXCEPTION NAME (0 IF SAME)          
UNTPSEQ  DS    XL2                 PATTERN SEQ #                                
UNTPKEY  DS    XL1                 PATTERN KEY STUCTURE                         
UNTACOST DS    XL4                 ACTUAL COST                                  
UNTNEXT  EQU   *                                                                
         EJECT                                                                  
* DSECT FOR PATTERN LIST ENTRIES *                                              
*                                                                               
PATABLED DSECT                                                                  
PATENT   DS    0CL(PATNEXT-PATSRT)                                              
PATSRT   DS    0CL11                                                            
PATYPE   DS    XL1                                                              
*                                                                               
PATQALL  EQU   64                  ALL NETWORKS                                 
PATQMED  EQU   32                  MEDIA                                        
PATQDPMD EQU   24                  DAYPART & MEDIA                              
PATQNET  EQU   16                  NETWORK                                      
PATQDPT  EQU   08                  DAYPART                                      
PATQPROG EQU   04                  PROGRAM                                      
PATQFEED EQU   02                  FEED                                         
PATQDPFD EQU   01                  DAYPART & FEED                               
*                                                                               
PATPROG  DS   0CL6                PROG CODE                                     
PATFEED  DS    CL4                FEED                                          
PATNONFD DS    CL2                                                              
PATSTR   DS    XL2                START DATE                                    
PATEND   DS    XL2                END DATE                                      
PATSTIM  DS    XL2                START TIME                                    
PATETIM  DS    XL2                END TIME                                      
PATDP    DS    CL1                DAYPART                                       
PATSEQ   DS    XL2                USED INTERNALLY ONLY                          
PATREF   DS    XL3                REF NO                                        
PATPROD1 DS    CL3                PROD                                          
PATULN   DS    XL1                UNIT LEN                                      
PATPROD2 DS    CL3                PROD 2                                        
PATULN2  DS    XL1                UNIT LEN                                      
PATDSKA  DS    XL4                                                              
PATFLG   DS    XL1                                                              
PATFLUSD EQU   X'80'              PATTERN USED                                  
*        EQU   X'40'              COMMLS 1ST ASSIGNED (NO PREV)                 
*        EQU   X'20'              COMMLS CHANGED (DIFF FROM PREV)               
PATFLUFN EQU   X'10'              PATTERN END DATE UFN                          
*        EQU   X'08'                                                            
*        EQU   X'04'                                                            
*        EQU   X'02'                                                            
*        EQU   X'01'                                                            
PATSEQNO DS    XL3                 NEW PATTERN SEQ NO                           
         DS    XL1                 SPARE                                        
PATCNT   DS    XL1                                                              
PATCOVER DS    XL1                                                              
PATNEXT  EQU   *                                                                
*                                                                               
* DSECT FOR EQUIVALENT PROGRAM CODE ENTRIES                                     
*                                                                               
EQVPTBLD DSECT                                                                  
EQVENT   DS   0CL(EQVNEXT-EQVEPRG)                                              
EQVEPRG  DS    CL6                EQUIVALENT                                    
EQVBPRG  DS    CL6                BASE PROGRAM CODE                             
EQVSDT   DS    XL2                START DATE                                    
EQVEDT   DS    XL2                END                                           
EQVSMODT DS    XL2                START PERIOD DATE                             
EQVEMODT DS    XL2                END                                           
EQVUSED  DS    XL1                                                              
EQVCT    DS    XL1                ENTRY COUNTER                                 
EQVNEXT  EQU   *                                                                
         EJECT                                                                  
* DSECT FOR COMMERCIAL TABLE * (BUILT IN AIO2)                                  
*                                                                               
CMLTBLD  DSECT                                                                  
CMLCML   DS    CL8                                                              
CMLCT    DS    C                                                                
CMLVLDSW DS    C                                                                
CMLENT   EQU   *-CMLTBLD                                                        
         EJECT                                                                  
* DSECT FOR NETWORK COMMERCIAL SEED ERROR LIST *                                
*                                                                               
XPRINT   DSECT                                                                  
*                                                                               
         DS    CL2                                                              
XPPROG   DS    CL6                 PROGRAM                                      
         DS    CL2                                                              
XPPROGNM DS    CL16                PROGRAM NAME                                 
         DS    CL5                                                              
XPPROGDA DS    CL8                 PROGRAM DAY                                  
         DS    CL4                                                              
XPPROGTM DS    CL11                PROGRAM TIME                                 
         DS    CL1                                                              
XPPERIOD DS    CL17                PRINT PERIOD                                 
         DS    CL3                                                              
XPEXPLAN DS    CL1                 PRINT REASON                                 
*                                                                               
* DSECT FOR NETWORK COMMERCIAL SEED ERROR LIST *                                
*                                                                               
XRECD    DSECT                                                                  
XREC     DS    0XL48                                                            
*                                                                               
XBAGYMD  DS    XL1                                                              
XCLT     DS    CL3                 CLIENT                                       
XNET     DS    CL4                 NETWORK                                      
*                                                                               
XPROD1   DS   0XL4                 PAT PRD AND LEN                              
XNETMKT  DS    XL2                 NETWORK MARKET (00 FOR XTYPE 5)              
         DS    XL2                                                              
*                                                                               
XPROG    DS    CL6                 PROGRAM                                      
*                                                                               
XPROD2   DS   0XL4                 PRD PARTNER AND LEN                          
XTUNT    DS    XL2                                                              
         DS    XL2                                                              
*                                                                               
XUNAS    DS    XL2                 UNASSIGNED UNITS                             
XUNAL    DS    XL2                 UNALLOCATED UNITS                            
XPERST   DS    XL2                                                              
XPERND   DS    XL2                                                              
         ORG   XUNAS                                                            
XCML     DS    CL12                CML WITH BAD PRODS                           
*                                                                               
XTYPE    DS    CL1                 1=NO COMMERCIALS ASSIGNED                    
*                                  2=UNALLOCATED UNITS                          
*                                  3=UNASSIGNED UNITS                           
*                                  3=EQUIV PROG CODE WITH PARTIAL BASE          
*                                     FOR PERIOD                                
*                                  5=CML ERROR                                  
*                                  6=TSUPP                                      
*                                  7=TRI-BACK                                   
XREF     DS   0XL3                 REFERENCE NUMBER                             
XCMLAS   DS    XL2                 PREVIOUSLY ASSIGNED CMLS                     
         DS    XL1                                                              
*                                                                               
XDATEST  DS    XL3                 PATTERN START DATE                           
XDATEND  DS    XL3                 PATTERN END DATE                             
XSUBTYPE DS    XL1              FOR XTYPE=5 SUBTYPE=0 PAT PRD CML ERR           
*                                                  =1 DELETED CML               
*                                                  =2 REL/RCL                   
*                                                  =3 CML NOT FOUND             
         DS    XL1                 SPARE                                        
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
* ONLINE LIST                                                                   
*                                                                               
LSTLINE  DSECT                                                                  
         DS    CL8                 FIELD HEADER                                 
LPROG    DS    CL6                                                              
         DS    CL2                                                              
LINSDTE  DS    CL8                                                              
         DS    CL2                                                              
LREVNO   DS    CL3                                                              
         DS    CL1                                                              
LUNITS   DS    CL5                                                              
         DS    CL1                                                              
LFEEDS   DS    CL5                                                              
         DS    CL2                                                              
LUNASUNT DS    CL5                                                              
         DS    CL1                                                              
LUNALUNT DS    CL5                                                              
         DS    CL1                                                              
LNEW     DS    CL3                                                              
         DS    CL1                                                              
LCHG     DS    CL3                                                              
         DS    CL2                                                              
LPRTID   DS    CL20                                                             
*                                                                               
* DSECT FOR PRINT LINE DATA *                                                   
*                                                                               
PRTLINE  DSECT                 COLS                                             
PPROG    DS    CL6              01                                              
         DS    CL1                                                              
PPROGNM  DS    CL14             08                                              
         DS    CL1                                                              
PDAYPT   DS    CL2              23                                              
         DS    CL1                                                              
PAIRDT   DS    CL5              26                                              
         DS    CL1                                                              
PAIRDT2  DS    CL5              32                                              
         DS    CL1                                                              
PFEED    DS    CL4              38                                              
         DS    CL1                                                              
PPROD    DS    CL3              43                                              
         DS    CL1                                                              
PPRDNM   DS    CL16             47                                              
         DS    CL1                                                              
PCML     DS    CL12             64                                              
         DS    CL1                                                              
PCMLTL   DS    CL24             80                                              
         DS    CL1                                                              
PUNTLEN  DS    CL7              105                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021SPTRA62   11/09/20'                                      
         END                                                                    

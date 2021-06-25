*          DATA SET ACCAP41    AT LEVEL 023 AS OF 12/11/09                      
*PHASE T61D41A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        ACCAP41 -- TEMPO MAINTENANCE OVERLAY                 *         
*                                                                     *         
*  COMMENTS:     MAINTAINS TEMPO X-REF RECORDS                        *         
*                                                                     *         
*  CALLED FROM:  CAP CONTROLLER (T61D00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS ACCAPC7 (OVERHEAD)                           *         
*                        ACCAPCA (LINE DETAIL)                        *         
*                        ACCAPCB (COMMUTER CODE)                      *         
*                        ACCAPCC (LINE INFO)                          *         
*                                                                     *         
*  OUTPUTS:      UPDATED TEMPO X-REF RECORDS                          *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- SYSSPARE                                       *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOLD                                         *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'T61D41 - TEMPO X-REF RECORD MAINTENANCE'                        
T61D41   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1D41**,R7,RR=R3                                              
         USING TIMEGWSD,R9         R9=A(TIME GLOBAL WORKING STORAGE)            
         L     RC,BCSVRC                                                        
         USING GEND,RC             RC=A(ADDRS-GENCON STUFF)                     
         L     RA,ATWA                                                          
         USING T61DFFD,RA          RA=A(TWA)                                    
         L     R8,ASYSD                                                         
         USING SYSD,R8             R8=A(BASE SAVED STORAGE)                     
         LA    R4,SYSSPARE                                                      
         USING STORED,R4           R4=A(APPLICATION SAVED STORAGE)              
         ST    R3,RELO                                                          
*                                                                               
         BAS   RE,SETUP            ANY INITIALIZING                             
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* VALIDATE KEY                                                       *          
**********************************************************************          
         SPACE 1                                                                
VK       DS    0H                                                               
         LA    RE,SVVKBLCK                                                      
         LA    RF,SVVKBLKQ                                                      
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         TM    GFACTST6,X'40'      RUNNING UNDER SCRIPT UPLOAD                  
         BNO   VKPRSN              THEN  DONT CHECK RECORD CHANGE               
         TM    TRANSTAT,RCHANG     RECORD CHANGE                                
         BZ    *+10                                                             
         XC    SVPERDTE,SVPERDTE   CLEAR PERIOD EACH REC CHANGE                 
*                                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE PERSON CODE                                                *         
***********************************************************************         
         SPACE 1                                                                
VKPRSN   DS    0H                                                               
         LA    R2,THDPERH          PERIOD IS FIRST FIELD ON SCREEN              
         CLI   5(R2),0             BUT CAN'T VALIDATE WITHOUT PERSON            
         BH    VKPRSN10                                                         
         OC    SVPERDTE,SVPERDTE   DO WE HAVE A PERIOD SAVED AROUND?            
         BNZ   VKPRSN10                                                         
         MVC   GERROR,=Y(ACIPLSE)  PLEASE INPUT REQUIRED FIELDS                 
         B     VKGERRX                                                          
*                                                                               
VKPRSN10 LA    R2,THDCODEH                                                      
         MVI   BCIFMIN,1           MINIMUN LENGTH - PERSON REQUIRED             
         MVC   BCIFMAX,BC1RLEV4    MAXIMUM LENGTH                               
         GOTO1 AFVAL,THDCODEH                                                   
         BH    VKAERRX                                                          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 AVALPRSN,THDCODEH   VALIDATE PERSON                              
         BE    *+14                                                             
         MVC   GERROR,=AL2(ACEIVPER)    INVALID PERSON CODE                     
         B     VKAERRX                                                          
*                                                                               
         MVC   SVPIDNO,BCPIDNO     SAVE PERSON ID #                             
         MVC   SV1RPRSN,BCIFLD     SAVE PERSON CODE                             
         MVC   THDNAME,BCWORK      DISPLAY PERSON NAME                          
         OI    THDNAMEH+6,X'80'                                                 
         EJECT                                                                  
***********************************************************************         
* VALIDATE PERIOD                                                     *         
***********************************************************************         
         SPACE 1                                                                
VKPERD   DS    0H                                                               
         LA    R2,THDPERH          ANYTHING IN PERIOD FIELD                     
         CLI   5(R2),0                                                          
         BNE   VKPERD10                                                         
         OC    SVPERDTE,SVPERDTE   DO WE HAVE A PERIOD SAVED AROUND?            
         BNZ   *+14                                                             
         MVC   GERROR,=Y(ACIPLSE) PLEASE INPUT REQUIRED FIELDS                  
         B     VKGERRX                                                          
         GOTO1 DATCON,BCDMCB,(2,SVPERDTE),(5,8(R2))                             
         MVI   5(R2),8             SET APPROPRIATE LENGTH                       
VKPERD10 XC    BCFLAG4,BCFLAG4                                                  
*                                                                               
         USING SCANBLKD,R3                                                      
         LA    R3,BLOCK+L'PVALOUTB                                              
         XC    BLOCK(250),BLOCK                                                 
         GOTO1 SCANNER,BCDMCB,(R2),(2,BLOCK+L'PVALOUTB),C',=-='                 
         CLI   BCDMCB+4,1                                                       
         BE    *+14                                                             
         MVC   GERROR,=AL2(ACEINV) INVALID PERIOD                               
         B     VKAERRX                                                          
         CLI   5(R2),3             L'INPUT>3 MEANS DATE  EX/'051594'            
         BH    *+12                                                             
         TM    SC1STVAL,SCNUMQ     DID THEY ENTER A PERIOD NUMBER               
         BO    VKPERD20                                                         
*                                                                               
* USER ENTERED A DATE                                                           
*                                                                               
         OI    BCFLAG4,BCFL4DTE    FLAG THAT USER ENTERED A DATE                
         USING PERVALD,R6                                                       
         LA    R6,BLOCK                                                         
         MVC   BYTE,LANGCODE                                                    
         TM    GFACTST6,X'40'      RUNNING UNDER SCRIPT UPLOAD                  
         BNO   *+8                 THEN FORCE DATE VALIDATION TO                
         MVI   BYTE,LANGEUS        USA FORMAT (EX MM/DD/YY)                     
         OI    BYTE,X'60'                                                       
         GOTO1 PERVAL,BCDMCB,(SC1STLEN,SC1STFLD),(BYTE,BLOCK)                   
         CLI   BCDMCB+4,PVRCMISS                                                
         BNE   *+14                                                             
         MVC   GERROR,=AL2(ACEINV) INVALID PERIOD                               
         B     VKAERRX                                                          
         CLI   BCDMCB+4,PVRCINV1                                                
         BNE   *+14                                                             
         MVC   GERROR,=AL2(ACEINV) INVALID PERIOD                               
         B     VKAERRX                                                          
         MVC   YYMMDD,PVALPSTA                                                  
         BAS   RE,GETLOC           GET OFFICE FOR THIS DATE                     
         BE    *+14                NO LOCATION FOR THIS PERIOD                  
         MVC   GERROR,=AL2(ACEINV)                                              
         B     VKAERRX                                                          
*                                                                               
         USING CALD,R1                                                          
         LA    R1,WORK2            FILL IN CALENDAR BLOCK                       
         XC    WORK2(CALDQ),WORK2                                               
         MVC   CALPYMD,YYMMDD                                                   
         OI    CALSTAT,CALYMDQ                                                  
         MVC   CALOFF,SV1ROFFC                                                  
         B     VKPERD30                                                         
*                                                                               
* USER ENTERED A PERIOD NUMBER                                                  
*                                                                               
VKPERD20 OI    BCFLAG4,BCFL4PER    FLAG THAT USER ENTERED A PERIOD#             
         MVC   YYMMDD,BCTODAY3                                                  
         BAS   RE,GETLOC           GET LOCATION FOR TODAY                       
         USING CALD,R1                                                          
         LA    R1,WORK2            FILL IN CALENDAR BLOCK                       
         XC    WORK2(CALDQ),WORK2                                               
         MVC   CALPNUM,SC1STNUM+3                                               
         OI    CALSTAT,CALNUMQ                                                  
         MVC   CALOFF,SV1ROFFC                                                  
*                                                                               
* CALL GETCAL TO GET PERIOD INFO                                                
*                                                                               
VKPERD30 MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 AGETCAL,WORK2                                                    
         BNE   VKAERRX                                                          
         MVC   AIO,AIO1                                                         
*                                                                               
         USING CALD,R1                                                          
         LA    R1,WORK2            FINAL CHECK TO MAKE SURE OFFICE IS           
         MVC   PRDSTDTE,CALRSTRT   PERIOD START DATE                            
         MVC   PRDENDTE,CALREND    CORRECT FOR THAT DATE                        
         MVC   PRDNUM,CALRNUM                                                   
         MVC   PRDMON,CALRMTH                                                   
         MVC   SVYYMMDD,YYMMDD                                                  
         TM    BCFLAG4,BCFL4DTE    USER ENTERED A DATE?                         
         BO    VKPERD40                                                         
         MVC   YYMMDD,PRDENDTE                                                  
         MVC   SVYYMMDD,PRDENDTE                                                
         BAS   RE,GETLOC           DO OFFICE AND DATE MATCH                     
         BE    *+14                                                             
         MVC   GERROR,=AL2(ACEENTDT)                                            
         B     VKAERRX             MUST ENTER A DATE                            
*                                                                               
VKPERD40 MVC   YYMMDD,SVYYMMDD                                                  
         GOTO1 DATCON,BCDMCB,(1,PRDENDTE),(2,SVPERDTE)   PERIOD END DTE         
         BAS   RE,GETLOC           REREAD TO GET ACTUAL OFFC/DPT/SUBD           
         DROP  R1,R3,R6                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY PERIOD NUMBER - DATE - MONTH                                *         
***********************************************************************         
         SPACE 1                                                                
         MVC   BCWORK,BCSPACES                                                  
         OI    THDPDESH+6,X'80'                                                 
         LA    R3,BCWORK                                                        
         MVI   0(R3),C'#'                                                       
         LA    R3,1(R3)                                                         
         EDIT  (B1,PRDNUM),(2,(R3)),0                                           
         OC    0(2,R3),=X'F0F0'    PAD WITH ZERO                                
         LA    R3,3(R3)                                                         
         GOTO1 DATCON,BCDMCB,(1,PRDENDTE),(17,(R3))                             
         LA    R3,9(R3)                                                         
         MVC   YYMMDD,PRDMON                                                    
         MVI   YYMMDD+2,X'01'                                                   
         GOTO1 DATCON,BCDMCB,(1,YYMMDD),(9,(R3))                                
         GOTO1 SQUASHER,DMCB,BCWORK,L'BCWORK                                    
         MVC   THDPDES,BCWORK                                                   
         OI    THDPDESH+6,X'80'                                                 
         EJECT                                                                  
***********************************************************************         
* VALIDATE 1R OFFICE CODE                                             *         
***********************************************************************         
         SPACE 1                                                                
VKOFFC   DS    0H                                                               
         MVC   AIO,AIO2                                                         
         MVC   SV1RACT,BCSPACES                                                 
         MVC   SV1RCPY,CMPY                                                     
         MVC   SV1RUL,=C'1R'                                                    
         LA    R3,SV1RCODE                                                      
         SR    R1,R1                                                            
         IC    R1,BC1RLEV1         LENGTH OF LEVEL A                            
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R3),SV1ROFFC                                                 
         LA    R3,1(R1,R3)                                                      
         BAS   RE,VAL1R            VALIDATE OFFICE CODE                         
         EJECT                                                                  
***********************************************************************         
* VALIDATE 1R DEPARTMENT CODE                                         *         
***********************************************************************         
         SPACE 1                                                                
VKDEPT   DS    0H                                                               
         SR    R1,R1                                                            
         IC    R1,BC1RLEV2         LENGTH OF LEVEL B                            
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R3),SV1RDPT                                                  
         LA    R3,1(R1,R3)                                                      
         BAS   RE,VAL1R                                                         
         EJECT                                                                  
***********************************************************************         
* VALIDATE SUBDEPARTMENT                                              *         
***********************************************************************         
         SPACE 1                                                                
VKSUBD   DS    0H                                                               
         SR    R1,R1                                                            
         IC    R1,BC1RLEV3         LENGTH OF LEVEL C                            
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R3),SV1RSDPT                                                 
         LA    R3,1(R1,R3)                                                      
         BAS   RE,VAL1R                                                         
         EJECT                                                                  
***********************************************************************         
* VALIDATE PERSON CODE ON 1R LEDGER                                   *         
***********************************************************************         
         SPACE 1                                                                
VKPERS   DS    0H                                                               
         SR    R1,R1                                                            
         IC    R1,BC1RLEV4         LENGTH OF LEVEL D                            
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R3),SV1RPRSN                                                 
         LA    R2,THDCODEH         ERROR AT PERSON CODE                         
         BAS   RE,VAL1R            VALIDATE 1R ACCOUNT AND GET INFO             
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET PERSON'S LOCATION WITH DATE IN YYMMDD                           *         
***********************************************************************         
         SPACE 1                                                                
GETLOC   NTR1                                                                   
         L     R6,AIO                                                           
         AH    R6,=Y(ACTRFST-ACTRECD)                                           
*                                                                               
         USING LOCELD,R6                                                        
GETLOC10 SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         CLI   0(R6),0                                                          
         BE    VKROUTH                                                          
         CLI   0(R6),LOCELQ        LOCATION ELEMENT X'83'                       
         BNE   GETLOC10                                                         
         CLC   YYMMDD,LOCSTART                                                  
         BL    GETLOC10                                                         
         OC    LOCEND,LOCEND       ANY END DATE                                 
         BZ    *+14                                                             
         CLC   YYMMDD,LOCEND                                                    
         BH    GETLOC10                                                         
*                                                                               
         MVC   SV1ROFFC,LOCOFF                                                  
         MVC   SV1RDPT,LOCDEPT                                                  
         MVC   SV1RSDPT,LOCSUB                                                  
         MVC   SV1RSTDT,LOCSTART                                                
         B     VKROUTE                                                          
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE 1R ACCOUNT - CALL GETACT WITH EACH LEVEL OF 1R             *         
***********************************************************************         
         SPACE 1                                                                
VAL1R    NTR1                                                                   
         USING ACTRECD,R6                                                       
         LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY(L'SV1RACT),SV1RACT                                       
         GOTO1 AGETACT,0                                                        
         BE    *+14                                                             
         MVC   GERROR,=AL2(ACEACCT)                                             
         B     VKIERRX                                                          
*                                                                               
         CLI   ACTEQU,ACTDIS                                                    
         BE    VAL1R10                                                          
         TM    BCASTAT1,RSTSACIL   IS ACCOUNT LOCKED                            
         BNO   VAL1R10                                                          
         MVC   GERROR,=AL2(ACEACTLK)                                            
         B     VKAERRX                                                          
*                                                                               
VAL1R10  CLC   BCSPINC,BCSPACES                                                 
         BNH   *+10                                                             
         MVC   SVSPINC,BCSPINC     SPECIAL INCOME ACCOUNT                       
*                                                                               
         CLC   BCDFTASK,BCSPACES   DEFAULT TASK CODE                            
         BNH   *+10                                                             
         MVC   SVDFTASK,BCDFTASK                                                
*                                                                               
VAL1RX   B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* VALKEY EXIT POINTS                                                  *         
***********************************************************************         
         SPACE 1                                                                
VKROUTL  MVI   BCDUB,0             SET CC LOW                                   
         B     VKROUTCC                                                         
VKROUTH  MVI   BCDUB,2             SET CC HIGH                                  
         B     VKROUTCC                                                         
VKROUTE  MVI   BCDUB,1             SET CC EQUAL                                 
VKROUTCC CLI   BCDUB,1                                                          
VKEXIT   XIT1                                                                   
*                                                                               
VKIERRX  MVI   GLTXT,L'BCACCODE-1                                               
         LA    R1,BCACKUL                                                       
         STCM  R1,7,GATXT          A(INSERTION TEST)                            
*                                                                               
VKAERRX  MVI   GMSYS,6             DEFAULT IS ACCOUNTING MSG SYSTEM             
         B     *+12                                                             
VKGERRX  MVI   GMSYS,X'FF'         GENERAL MESSAGE SYSTEM                       
         MVI   GMSGTYPE,C'I'                                                    
         GOTO1 MYERR                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD                                                     *         
***********************************************************************         
         SPACE 1                                                                
VR       DS    0H                                                               
         MVC   AIO,AIO1            SET IO AREA                                  
         TM    TRANSTAT,RCHANG+ACHANG   RECORD OR ACTION CHANGE                 
         BZ    *+8                                                              
         OI    FLAG,FLGKEY         MARK KEY AS CHANGED                          
*        BRAS  RE,CLRSCR           CLEAR SCR FOR REC/ACT CHANGE                 
*                                                                               
*                                                                               
*        CLC   SVYYMMDD,SAVPERDT   OR PERIOD                                    
*        BNE   VR10                                                             
*        CLC   SV1RPRSN,SAVPERSN   OR PERSON                                    
*        BNE   VR10                                                             
*        TM    FLAG,FLGKEY         DID RECORD/ACTION CHANGE?                    
*        BNO   *+8                                                              
*R10     BAS   RE,DELETE           DELETE ALL SEQUENTIAL RECORDS                
*                                                                               
         CLI   RECNUM,RTTHD        HEADER SCREEN?                               
         BNE   VR10                                                             
         CLI   ACTEQU,ACTCHA       IF ACTION=CHANGE-DEL ALL PREV INFO           
         BNE   VR20                                                             
         NI    FLAG,X'FF'-FLGREC   TURN OFF REC VAL BIT                         
         BAS   RE,DELETE           DELETE ALL SEQUENTIAL RECORDS                
         BRAS  RE,CLRSCR           FOR COM CODE/TEMPO LINE CLEAR SCR            
         TM    FLAG,FLGREC         WAS ANYTHING DELETE                          
         BO    ERRDEL                                                           
         LA    R2,THDPERH           POSITION CURSOR                             
         MVC   GERROR,=Y(ACERECNF)  RECORD NOT FOUND                            
         B     ERECNF                                                           
VR10     CLI   RECNUM,RTTCM        COMMUTER CODE                                
         BNE   VR60                                                             
*                                                                               
         USING TSXRECD,R6                                                       
VR20     XC    BIGKEY,BIGKEY       READ FOR TEMPO X-REF RECORD                  
         LA    R6,BIGKEY                                                        
         MVI   TSXKTYP,TSXKTYPQ    X'3E' - TIMESHEET TEMPO RECORD TYPE          
         MVI   TSXKSUB,TSXKSUBQ    X'13' - TIMESHEET TEMPO SUB RECORD           
         MVC   TSXKCPY,CMPY        COMPANY                                      
         LA    RF,SV1RCODE                                                      
         SR    R1,R1                                                            
         IC    R1,BC1RLNQ3                                                      
         SH    R1,=H'1'                                                         
         EX    R1,*+4                                                           
         MVC   TSXKODS(0),0(RF)    ISOLATE OFF/DPT/SUB                          
         OC    TSXKODS,BCSPACES                                                 
         LA    RF,1(R1,RF)                                                      
         IC    R1,BC1RLEV4                                                      
         SH    R1,=H'1'                                                         
         EX    R1,*+4                                                           
         MVC   TSXKPER(0),0(RF)    ISOLATE PERSON CODE                          
         OC    TSXKPER,BCSPACES                                                 
         MVC   TSXKEND,PRDENDTE    PERIOD END DATE                              
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         B     VR40                                                             
*                                                                               
VR30     MVI   RDUPDATE,C'Y'                                                    
         GOTO1 SEQ                                                              
VR40     CLC   BIGKEY(TSXKSBR-TSXKEY),KEYSAVE                                   
         BE    VR50                                                             
         LA    R2,THDPERH           POSITION CURSOR                             
         MVC   GERROR,=Y(ACERECNF)  RECORD NOT FOUND                            
         B     ERECNF                                                           
*                                                                               
VR50     TM    TSXKSTA1,X'10'      ARE THERE OTHER SEQ RECORDS?                 
         BO    VR30                YES - READ UNTIL YOU HAVE THE LAST           
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         CLI   ACTEQU,ACTDIS       IF ACTION=DISPLAY - DISPLAY                  
         BE    DR                                                               
*        TM    FLAG,FLGKEY         WAS KEY CHANGED?                             
*        BO    DR                                                               
         NI    FLAG,X'FF'-FLGREC   TURN OFF REC VALID BIT                       
         NI    FLAG,X'FF'-FLGADD   TURN OFF ADDREC FLAG                         
         BAS   RE,ELEMBD                                                        
         BAS   RE,ELEMBE                                                        
         B     VR100                                                            
*                                                                               
VR60     CLI   RECNUM,RTTDT        LINE DETAIL                                  
         BE    *+12                                                             
         CLI   RECNUM,RTTLN        LINE INFO                                    
         BNE   VRX                                                              
*                                                                               
         MVC   AIO,AIO2                                                         
*                                                                               
* GET LINE NUMBER AND READ T/S DETAIL RECORD X'3E14'                            
*                                                                               
         XC    TMSLIN#,TMSLIN#     TMS LINE NUMBER                              
         XC    TMPLIN#,TMPLIN#     TEMPO LINE NUMBER                            
*                                                                               
         LA    R2,TDTLN#H          LINE NUM FLD HEADER/TEMPO DETAIL             
         CLI   RECNUM,RTTDT        TEMPO DETAIL                                 
         BE    *+8                                                              
         LA    R2,TLNLN#H          LINE NUM FLD HEADER/TEMPO LINE INFO          
         CLI   5(R2),0             LINE NUMBER MUST BE ENTERED                  
         BE    ERRMISS             NOTHING ENTERED - ERROR                      
*                                                                               
         XC    BLOCK,BLOCK                                                      
         GOTO1 SCANNER,BCDMCB,(0,(R2)),(0,BLOCK)                                
         CLI   BCDMCB+4,1                                                       
         BNE   ERRINV                                                           
         ICM   R1,15,BLOCK+4                                                    
*        C     R1,=F'65000'        CAN'T BE HIGHER THAN 65000                   
*        BH    ERRINV                                                           
         STCM  R1,3,TMPLIN#        SAVE TEMPO INTERNAL KEY NUMBER               
*                                                                               
         BAS   RE,GETTIME          READ TIME RECORD AND GET INFO                
*                                                                               
         NI    FLAG,X'FF'-FLGREC   TURN OFF REC VALID BIT                       
         NI    FLAG,X'FF'-FLGADD   TURN OFF ADDREC FLAG                         
*                                                                               
* READ FOR TIMESHEET DETAIL RECORD                                              
*                                                                               
         NI    FLAG,X'FF'-FLGDEL   TURN OFF REC/KEY WAS DELETED                 
         USING TSIRECD,R6                                                       
         LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY       READ FOR T/S DETAIL RECORD                   
         MVI   TSIKTYP,TSIKTYPQ    X'3E' - TIMESHEET DETAIL RECORD TYPE         
         MVI   TSIKSUB,TSIKSUBQ    X'14' - TIMESHEET DETAIL SUB RECORD          
         MVC   TSIKCPY,CMPY        COMPANY                                      
         MVC   TSIKUNT(2),=C'1R'   ALWAYS 1R                                    
         MVC   TSIKACT,SV1RCODE    PUT IN ACCOUNT CODE                          
         MVC   TSIKOFF,SVOFF       PUT IN OFFICE CODE                           
         MVC   TSIKCCPY,CMPY                                                    
         MVC   TSIKULC,SVCACT      CONTRA ACCOUNT                               
         SR    R1,R1                                                            
         ICM   R1,7,PRDENDTE                                                    
         LNR   R1,R1                                                            
         STCM  R1,7,TSIKPEDT                                                    
         OI    DMINBTS,X'08'       READ FOR DELETED RECORDS                     
         GOTO1 HIGH                                                             
         B     VR80                                                             
*                                                                               
VR70     GOTO1 SEQ                                                              
VR80     CLC   BIGKEY(TSIKPEDT-TSIKEY+L'TSIKPEDT),KEYSAVE                       
         BE    *+16                                                             
         BAS   RE,NEWTSI           CREATE NEW TSI RECORD                        
         OI    FLAG,FLGADD          IF RECORD NOT FOUND-ADDIT                   
         B     VR90                                                             
*                                                                               
         TM    TSIKSTA,X'80'       IS THIS RECORD MARKED DELETED?               
         BNO   *+20                                                             
         OI    FLAG,FLGDEL         TURN ON REC/KEY WAS DELETED                  
         NI    TSIKSTA,X'FF'-X'80'    MARK ACTIVE KEY UNDELETED                 
         NI    TSIKSTA1,X'FF'-X'10'   TURNOFF SEQ BIT                           
         B     *+12                                                             
*                                                                               
         TM    TSIKSTA1,TSISEQL    ARE THERE OTHER SEQ RECORDS?                 
         BO    VR70                YES - READ UNTIL YOU HAVE THE LAST           
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
VR90     CLI   ACTEQU,ACTDIS       IF ACTION=DISPLAY - DISPLAY                  
         BE    DR                                                               
*        TM    FLAG,FLGKEY         WAS KEY CHANGED?                             
*        BO    DR                                                               
         L     R6,AIO              POINT TO RECORD AND UNDELETE                 
         NI    TSIRSTA,X'FF'-X'80'                                              
         NI    FLAG,X'FF'-FLGELM   CLEAR FLAG TO SHOW NO ELMS YET               
         BAS   RE,ELEMBF                                                        
         TM    FLAG,FLGELM         DO WE HAVE AN ELEMENT?                       
         BNO   VRX                 NO - SKIP ADDING RECORD                      
*                                                                               
VR100    L     RF,PUTREC                                                        
         TM    FLAG,FLGADD         ARE WE ADDING A RECORD?                      
         BNO   *+8                                                              
         L     RF,ADDREC                                                        
         BASR  RE,RF                                                            
         TM    FLAG,FLGDEL         WAS REC/KEY MARKED DEL?                      
         BNO   VRX                                                              
         GOTO1 WRITE                                                            
*                                                                               
VRX      DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         OI    FLAG,FLGREC         TURN ON BIT TO SHOW RECORD VALID             
         MVC   SAVPERDT,SVYYMMDD   SAVE PERIOD AND                              
         MVC   SAVPERSN,SV1RPRSN   PERSON                                       
         MVC   SAVLIN#,TMPLIN#     SAVE OFF TEMPO LINE#                         
*                                                                               
* ONLY DISPLAY RECORD FOR HEADER AND DETAIL                                     
*                                                                               
         CLI   RECNUM,RTTHD        HEADER SCREEN?                               
         BE    DR                                                               
         CLI   RECNUM,RTTDT        LINE DETAIL                                  
         BE    DR                                                               
         BRAS  RE,CLRSCR           FOR COM CODE/TEMPO LINE CLEAR SCR            
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
* UPDATE X'BD' TEMPO HEADER ELEMENT                                  *          
**********************************************************************          
         SPACE 1                                                                
         USING XTHELD,R6                                                        
ELEMBD   NTR1                                                                   
         CLI   RECNUM,RTTHD        TEMPO HEADER                                 
         BNE   ELBDX                                                            
         MVI   ELCODE,XTHELQ       X'BD'                                        
         GOTO1 REMELEM             REMOVE FOR REC/ACT OR KEY CHANGE             
*                                                                               
ELBD10   LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   XTHEL,XTHELQ        X'BD' - TEMPO HEADER ELEMENT                 
         MVI   XTHLN,XTHLNQ        ELEMENT LENGTH                               
*                                                                               
         LA    R2,THDPPIDH                                                      
         CLI   5(R2),0                                                          
         BE    ERRMISS             NOTHING ENTERED - ERROR                      
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,THDPPID(0)      CONVERT PID NUM TO 2 BYTE HEX                
         CVB   R1,DUB                                                           
         STCM  R1,3,XTHPPID                                                     
*                                                                               
         LA    R2,THDLCNTH                                                      
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,THDLCNT(0)      CONVERT LINE COUNT TO 2 BYTE HEX             
         CVB   R1,DUB                                                           
         STCM  R1,3,XTHLCNT                                                     
*                                                                               
         LA    R2,THDLDCTH                                                      
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,THDLDCT(0)      CONVERT LINE/DAY CNT TO 2 BYTE HEX           
         CVB   R1,DUB                                                           
         STCM  R1,3,XTHLDCNT                                                    
*                                                                               
         LA    R2,THDSCNTH                                                      
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,THDSCNT(0)      CONVERT SWIPES COUNT TO 2 BYTE HEX           
         CVB   R1,DUB                                                           
         STCM  R1,3,XTHSCNT                                                     
*                                                                               
         LA    R2,THDBHRSH         B-TIME HOURS                                 
         CLI   5(R2),0                                                          
         BE    ELBD20              NOTHING ENTERED - GO ONTO NEXT               
         BRAS  RE,VALHRS                                                        
         BNE   ACCERRX                                                          
         ZAP   XTHBHRS,BCHOURS                                                  
*                                                                               
ELBD20   LA    R2,THDRHRSH         R-TIME HOURS                                 
         CLI   5(R2),0                                                          
         BE    ELBD30              NOTHING ENTERED - GO ONTO NEXT               
         BRAS  RE,VALHRS                                                        
         BNE   ACCERRX                                                          
         ZAP   XTHRHRS,BCHOURS                                                  
*                                                                               
ELBD30   LA    R2,THDNHRSH         N-TIME HOURS                                 
         CLI   5(R2),0                                                          
         BE    ELBD40              NOTHING ENTERED - GO ONTO NEXT               
         BRAS  RE,VALHRS                                                        
         BNE   ACCERRX                                                          
         ZAP   XTHNHRS,BCHOURS                                                  
*                                                                               
ELBD40   LA    R2,THDNCHRH         NON-CLIENT HOURS                             
         CLI   5(R2),0                                                          
         BE    ELBD50              NOTHING ENTERED - GO ONTO NEXT               
         BRAS  RE,VALHRS                                                        
         BNE   ACCERRX                                                          
         ZAP   XTHNCHRS,BCHOURS                                                 
*                                                                               
ELBD50   LA    R0,STATABQ          # OF ENTRIES IN TABLE                        
         LA    R1,STATAB           STATUS TABLE                                 
ELBD60   CLC   THDTSST,0(R1)       TIMESHEET STATUS                             
         BE    ELBD70                                                           
         LA    R1,L'STATAB(R1)                                                  
         BCT   R0,ELBD60                                                        
*                                                                               
         MVI   XTHSTAT,X'04'       FORCE STATUS TO 4                            
         B     *+10                                                             
ELBD70   MVC   XTHSTAT,1(R1)       MOVE STATUS INTO ELEMENT                     
         MVC   XTHERR,THDUPER      UPLOAD ERROR                                 
         MVC   XTHPERD,PRDNUM      PERIOD NUMBER                                
*                                                                               
         GOTO1 GETDTE,DMCB,THDSTRH,XTHSTDTE   START    DATE                     
         GOTO1 GETDTE,DMCB,THDENDH,XTHEND     END      DATE                     
         GOTO1 GETDTE,DMCB,THDSDTEH,XTHSDTE   SUBMIT   DATE                     
         LA    R2,THDSPIDH                                                      
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,THDSPID(0)      CONVERT SUBMITTER PID TO 2 BYTE HEX          
         CVB   R1,DUB                                                           
         STCM  R1,3,XTHSPID                                                     
*                                                                               
         GOTO1 GETDTE,DMCB,THDADTEH,XTHADTE   APPROVED DATE                     
         LA    R2,THDAPIDH                                                      
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,THDAPID(0)      CONVERT APPROVER PID TO 2 BYTE HEX           
         CVB   R1,DUB                                                           
         STCM  R1,3,XTHAPID                                                     
*                                                                               
         GOTO1 GETDTE,DMCB,THDUDTEH,XTHUDTE   UPLOAD   DATE                     
         LA    R2,THDUPIDH                                                      
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,THDUPID(0)      CONVERT UPLOAD PID TO 2 BYTE HEX             
         CVB   R1,DUB                                                           
         STCM  R1,3,XTHUPID                                                     
*                                                                               
         BAS   RE,ADDIT            ADD ELEMENT TO TEMPO X-REF RECORD            
ELBDX    B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
* UPDATE X'BE' TEMPO COMMUTER CODE ELEMENT                           *          
**********************************************************************          
         SPACE 1                                                                
ELEMBE   NTR1                                                                   
         CLI   RECNUM,RTTCM        TEMPO COMMUTER CODE                          
         BNE   ELBEX                                                            
*        CLC   SVYYMMDD,SAVPERDT   OR PERIOD                                    
*        BNE   ELBE10                                                           
*        CLC   SV1RPRSN,SAVPERSN   OR PERSON                                    
*        BNE   ELBE10                                                           
*        TM    FLAG,FLGKEY         HAS KEY CHANGED?                             
*        BNO   ELBE20                                                           
*LBE10   MVI   ELCODE,XTCELQ       X'BE'                                        
*        GOTO1 REMELEM             REMOVE FOR REC/ACT OR KEY CHANGE             
*                                                                               
ELBE20   LA    R2,TCMDAYH          ADDRESS OF FIRST FIELD                       
*                                                                               
         USING XTCELD,R6                                                        
ELBE30   LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   XTCEL,XTCELQ        X'BE' - TEMPO COMMUTER CODE ELEMENT          
         MVI   XTCLN,XTCLN1Q       PUT IN LENGTH SO FAR                         
*                                                                               
         NI    FLAG,X'FF'-(FLGELM+FLGCOM)  CLEAR DAY/CODE COMBO/ELEM            
ELBE40   DS    0H                                                               
         LA    RF,TCMNARLH         SET RF TO POINT TO LAST SCREEN FLD           
         CR    R2,RF               ARE WE PAST THE LAST FIELD?                  
         BH    ELBE110                                                          
         CLI   5(R2),0             ANYTHING IN FIELD?                           
         BE    ELBE60              NO SKIP BOTH DAY AND CODE                    
         TM    4(R2),X'80'         WAS FIELD INPUTTED THIS TIME?                
         BNO   ELBE60              IF NOT SKIP IT                               
*                                                                               
         TM    FLAG,FLGELM         DO WE HAVE A BE ELEMENT?                     
         BNO   ELBE50                                                           
         BAS   RE,ADDIT                                                         
         B     ELBE30                                                           
*                                                                               
ELBE50   OI    FLAG,FLGELM+FLGCOM  SHOW THAT WE HAVE A 'BE'AND COMBO            
         LA    RF,SVDAY                                                         
         LA    R1,L'SVDAY                                                       
         CLI   9(R2),X'40'         IF ONLY THE 1ST BYTE IS FILLED IN            
         BH    *+12                MOVE IT TO THE 2ND BYTE OF FIELD             
         LA    RF,SVDAY+1                                                       
         LA    R1,L'SVDAY-1                                                     
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,RF),8(R2)       SAVE DAY FIELD                               
*                                                                               
         GOTO1 HEXIN,DMCB,SVDAY,XTCDAY,L'SVDAY,L'XTCDAY                         
*                                                                               
ELBE60   BAS   RE,BMPFLD                                                        
         CLI   5(R2),0             ANYTHING IN CODE FIELD?                      
         BNE   ELBE70                                                           
         TM    FLAG,FLGCOM         DO WE HAVE A COMBO?                          
         BNO   ELBE80                                                           
         MVC   XTCCOM,BCSPACES                                                  
         B     ELBE80                                                           
ELBE70   TM    4(R2),X'80'         WAS FIELD INPUTTED THIS TIME?                
         BNO   ELBE80              IF NOT SKIP IT                               
         MVC   XTCCOM,8(R2)        MOVE IN CODE TO ELEMENT                      
*                                                                               
ELBE80   NI    FLAG,X'FF'-FLGCOM   TURN OFF DAY/CODE COMBO                      
         BAS   RE,BMPFLD                                                        
         CLI   5(R2),0             ANYTHING IN NARRATIVE FIELD?                 
         BE    ELBE100             BUMP ELEMENT AND CLEAR LENGTH                
         TM    4(R2),X'80'         WAS FIELD INPUTTED THIS TIME?                
         BNO   ELBE100             IF NOT SKIP IT                               
*                                                                               
         SR    R1,R1                                                            
         IC    R1,5(R2)            GET LENGTH OF INPUT FIELD                    
         SR    RF,RF                                                            
         IC    RF,XTCLN                                                         
         AR    RF,R1               INCREASE LENGTH OF ELEM ACCORDINGLY          
         CH    RF,=Y(ELMAX)        MAKE SURE IT IS NOT TOO BIG                  
         BH    ELBE90              NARRATIVE IS TOO BIG TO FIT ON EL            
*                                                                               
         LR    R3,R6               START AT THE BEGINNING OF THE ELEM           
         SR    RE,RE                                                            
         IC    RE,XTCLN            GET CURRENT LENGTH OF ELEMENT                
         AR    R3,RE               BUMP TO CURRENT POSITION IN ELEMENT          
*        CLI   RE,XTCLN1Q          DO WE JUST HAVE THE OVERHEAD?                
*        BE    *+8                                                              
*        MVI   0(R3),X'40'         INSERT A SPACE                               
*        LA    R3,1(R3)                                                         
*        LA    RF,1(RF)            BUMP UP CURRENT EL LEN TO FIT SPACE          
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R3),8(R2)                                                    
         STC   RF,XTCLN            SAVE OFF LEN SO FAR                          
         B     ELBE100             DONT BUMP R5 BUT BUMP FLDS AND CONT          
*                                                                               
ELBE90   LR    R3,R6                                                            
         LHI   R1,ELMAX                                                         
         SR    RE,RE                                                            
         IC    RE,XTCLN                                                         
         AR    R3,RE               BUMP TO CURRENT POSITION IN ELEMENT          
         LA    R3,1(R3)            ADD A SPACE                                  
         MVI   0(R3),X'40'                                                      
         LA    R3,1(R3)            MAKE ROOM FOR THE SPACE                      
*                                                                               
         SR    R1,RE                                                            
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R3),8(R2)                                                    
         MVI   XTCLN,ELMAX         MAX OUT THE ELEMENT LENGTH                   
         BAS   RE,ADDIT            ADD ELEMENT                                  
         B     ELBE30                                                           
*                                                                               
ELBE100  BAS   RE,BMPFLD                                                        
         B     ELBE40                                                           
*                                                                               
ELBE110  DS    0H                                                               
         TM    FLAG,FLGELM         DO WE HAVE A BE ELEMENT?                     
         BNO   ELBEX                                                            
         BAS   RE,ADDIT            ADD ELEMENT TO TEMPO X-REF RECORD            
*                                                                               
ELBEX    B     EXIT                                                             
         EJECT                                                                  
         DROP  R6                                                               
**********************************************************************          
* CREATE NEW TSI RECORD                                              *          
**********************************************************************          
         SPACE 1                                                                
NEWTSI   NTR1                                                                   
         USING TSIRECD,R6                                                       
         L     R6,AIO                                                           
         GOTO1 AXAIO,AIO           CLEAR IO                                     
         MVI   TSIKTYP,TSIKTYPQ    X'3E' - TIMESHEET DETAIL RECORD TYPE         
         MVI   TSIKSUB,TSIKSUBQ    X'14' - TIMESHEET DETAIL SUB RECORD          
         MVC   TSIKCPY,CMPY        COMPANY                                      
         MVC   TSIKUNT(2),=C'1R'   ALWAYS 1R                                    
         MVC   TSIKACT,SV1RCODE    PUT IN ACCOUNT CODE                          
         MVC   TSIKOFF,SVOFF       PUT IN OFFICE CODE                           
         MVC   TSIKCCPY,CMPY                                                    
         MVC   TSIKULC,SVCACT      CONTRA ACCOUNT                               
         SR    R1,R1                                                            
         ICM   R1,7,PRDENDTE                                                    
         LNR   R1,R1                                                            
         STCM  R1,7,TSIKPEDT                                                    
         MVI   TSIRRI1,TSIKRI1Q                                                 
         MVI   TSIRRI2,TIMKRI2Q                                                 
         MVI   TSIRRI3,TIMKRI3Q                                                 
         MVI   TSIRRI4,TIMKRI4Q                                                 
         TM    GFACTST6,X'40'      SCRIPT UPLOAD                                
         BNO   *+8                                                              
         OI    TSIRSTA1,TSITEMPO   RECORD CREATED DURING SCRIPT UPLOAD          
         LA    R1,L'TSIKEY+L'TSIKSTA+L'TSIKDA   SET TEMPORARY LENGTH            
         STCM  R1,3,TSIRLEN                                                     
         MVC   BIGKEY,0(R6)                                                     
*                                                                               
NEWTSIX  B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
* UPDATE X'BF' TEMPO DETAIL LINE ELEMENT                             *          
**********************************************************************          
         SPACE 1                                                                
ELEMBF   NTR1                                                                   
         CLI   RECNUM,RTTDT        TEMPO DETAIL                                 
         BNE   ELBFL10             NO-SEE IF ITS LINE INFO                      
*        MVI   ELCODE,XTMELQ       X'BF'                                        
*        GOTO1 REMELEM             REMOVE FOR REC/ACT OR KEY CHANGE             
*                                                                               
         CLC   SAVLIN#,TMPLIN#     DO WE HAVE THE SAME LINE NUMBER?             
         BE    *+8                                                              
         BRAS  RE,CLRSCR           CLEAR ENTIRE SCREEN                          
*                                                                               
         BAS   RE,DELBF            DELETE ALL LINE INFO/NAR (IF ANY)            
*                                                                               
         USING XTMELD,R6                                                        
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   XTMEL,XTMELQ        X'BF'                                        
         MVI   XTMTYP,XTMTLIN      ALWAYS PUT OUT A DETAIL ONLY ELM             
         MVI   XTMLN,XTMLN2Q                                                    
*                                                                               
         OC    TMPLIN#,TMPLIN#     ANY NUMBER?                                  
         BZ    ELBFD40             NO SKIP ADDING ANY DETAIL INFO               
*                                                                               
         MVC   XTMTLIN#,TMPLIN#    PUT LINE NUMBER TO ELEMENT                   
         MVC   XTMLIN#,TMSLIN#     TMS LINE                                     
*                                                                               
         LA    R2,TDTTYPH          TYPE OF TIME HEADER                          
         CLI   5(R2),0                                                          
         BE    ELBFD20                                                          
         OI    FLAG,FLGELM         MARK THAT YOU WANT ELEMENT                   
         USING TIMETABD,RE                                                      
         LA    RE,TIMETAB                                                       
ELBFD10  CLI   0(RE),X'FF'                                                      
         BE    ERRINV              INVALID TYPE OF TIME                         
         CLC   TTFIELD,8(R2)                                                    
         BE    *+12                                                             
         LA    RE,TTLENQ(RE)                                                    
         B     ELBFD10                                                          
         MVC   XTMTTYP,TTTYPE      TYPE EQUIV                                   
*                                                                               
         CLI   TTTYPE,TIMTCN       FOR 'N' TIME CHECK IF 'N/C' TIME             
         BNE   ELBFD20                                                          
         CLC   SVCACT(2),=C'1N'    IF CONTRA IS 1N-'N/C' TIME NOT 'N'           
         BNE   ELBFD20                                                          
         MVI   XTMTTYP,TIMTNC      NON-CLIENT TIME                              
         DROP  RE                                                               
*                                                                               
ELBFD20  LA    R2,TDTHRSH          STORE HOURS                                  
         CLI   5(R2),0                                                          
         BE    ELBFD30             NOTHING ENTERED - ERROR                      
         OI    FLAG,FLGELM         MARK THAT YOU WANT ELEMENT                   
         BRAS  RE,VALHRS                                                        
         BNE   ACCERRX                                                          
         ZAP   XTMHRS,BCHOURS                                                   
*                                                                               
ELBFD30  LA    R2,TDTFLGH                                                       
         CLI   5(R2),0                                                          
         BE    ELBFD35             NOTHING ENTERED                              
         GOTO1 HEXIN,BCDMCB,TDTFLG,BCWORK,L'TDTFLG                              
         MVC   XTMDFLG,BCWORK      DEFAULT FLAGS                                
         OI    FLAG,FLGELM         SET FLAG TO SHOW WE HAVE AN ELEM             
*                                                                               
ELBFD35  DS    0H                                                               
         TM    FLAG,FLGELM         DO YOU WANT TO ADD ELM?                      
         BNO   ELBFD40             NO - SKIP IT                                 
         BAS   RE,ADDIT            ADD ELEMENT TO TEMPO X-REF RECORD            
*                                                                               
* BUILD NARRATIVE ELEMENT                                                       
*                                                                               
ELBFD40  LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   XTMEL,XTMELQ        X'BF'                                        
         MVI   XTMTYP,XTMTNAR      NARRATIVE ELEMENT                            
*                                                                               
         LA    R2,TDTNRTH          1ST NARRATIVE FIELD HEADER                   
         CLI   5(R2),0                                                          
         BE    ELBFX               NOTHING ENTERED                              
*                                                                               
         MVC   XTMTLIN#,TMPLIN#    PUT LINE NUMBER TO ELEMENT                   
         MVI   SVLN,XTMLN1Q        SET DEFAULT LENGTH AS OVERHEAD               
         LA    R5,XTMNAR           NARRATIVE IN ELEMENT                         
         LA    R0,4                MAXIMUM OF 4 NARRATIVE FLDS/SCREEN           
ELBFD50  CLI   5(R2),0                                                          
         BE    ELBFD60                                                          
         SR    RE,RE                                                            
         IC    RE,SVLN                                                          
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         AR    RE,R1                                                            
         STC   RE,SVLN             UPDATE LENGTH                                
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R5),8(R2)     NARRATIVE                                      
         LA    R5,1(R1,R5)                                                      
         BAS   RE,BMPFLD                                                        
         BCT   R0,ELBFD50                                                       
*                                                                               
ELBFD60  MVC   XTMLN,SVLN          LENGTH                                       
         OI    FLAG,FLGELM         SET FLAG TO SHOW WE HAVE AN ELEM             
         BAS   RE,ADDIT            ADD ELEMENT                                  
*                                                                               
ELBFL10  CLI   RECNUM,RTTLN        TEMPO LINE INFO                              
         BNE   ELBFX                                                            
*                                                                               
         CLC   SAVLIN#,TMPLIN#     DO WE HAVE THE SAME LINE NUMBER?             
         BE    *+8                                                              
         BRAS  RE,CLRSCR           CLEAR ENTIRE SCREEN                          
*                                                                               
* BUILD DAY/SWIP ELEMENT                                                        
*                                                                               
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   XTMEL,XTMELQ        X'BF'                                        
         MVI   XTMTYP,XTMTALL      LINE ALLOCATION - DAY/SWIPE                  
*                                                                               
         LA    R2,TLNLN#H          LINE #                                       
         XC    BLOCK,BLOCK                                                      
         GOTO1 SCANNER,BCDMCB,(0,(R2)),(0,BLOCK)                                
         CLI   BCDMCB+4,1                                                       
         BNE   ERRINV                                                           
         ICM   R1,15,BLOCK+4                                                    
         STCM  R1,3,XTMTLIN#       SAVE TEMPO LINE NUMBER                       
*                                                                               
         XC    FLDCNT,FLDCNT       CLEAR FIELD COUNT                            
         LA    R2,TLNDAYH          ADDRESS OF FIRST FIELD                       
         LA    R3,TLNSWPLH         ADDRESS OF LAST FIELD ON SCREEN              
         LA    R5,XTMDATA          BEGIN OF MINI ELEMENTS                       
         USING XTMDATA,R5                                                       
*                                                                               
ELBFL20  CR    R2,R3               ARE WE PAST THE LAST FIELD?                  
         BH    ELBFL110                                                         
         CLI   5(R2),0             ANYTHING IN FIELD?                           
         BE    ELBFL30             NO SKIP BOTH DAY AND CODE                    
*                                                                               
* SAVE OFF DAY IN SVDAY FIELD BUT DONT ADD TO ELEMENT UNTIL AFTER               
*      WE VALIDATE THAT THERE IS A SWIPE                                        
*                                                                               
         LA    RF,SVDAY                                                         
         LA    R1,L'SVDAY                                                       
         CLI   9(R2),X'40'         IF ONLY THE 1ST BYTE IS FILLED IN            
         BH    *+12                MOVE IT TO THE 2ND BYTE OF FIELD             
         LA    RF,SVDAY+1                                                       
         LA    R1,L'SVDAY-1                                                     
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,RF),8(R2)       SAVE DAY FIELD                               
*                                                                               
ELBFL30  BAS   RE,BMPFLD                                                        
         CLI   5(R2),0             ANYTHING IN FIELD?                           
         BE    ELBFL100            NO SKIP BOTH DAY AND CODE                    
         TM    4(R2),X'80'         WAS FIELD INPUTTED THIS TIME?                
         BNO   ELBFL100            IF NOT SKIP IT                               
*                                                                               
* ADD DAY TO ELEMENT AND UPDATE COUNTER                                         
*                                                                               
         GOTO1 HEXIN,DMCB,SVDAY,XTMDAY,L'SVDAY,L'XTMDAY                         
*                                                                               
         SR    R1,R1               MINI COUNTER                                 
         IC    R1,FLDCNT           INCREMENT FIELD COUNT                        
         LA    R1,1(R1)            BUMP NUMBER OF MINI                          
         STC   R1,FLDCNT           SAVE FIELD COUNT                             
*                                                                               
         SR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         NI    FLAG,X'FF'-FLGMID   TURN OFF MIDNIGHT INDICATOR                  
*                                                                               
         USING MIDNTBD,RE                                                       
         LA    RE,MIDNTAB          CHECK FOR MIDNIGHT                           
         LA    RF,MIDNTABQ         SPECIAL CODE FOR MIDNIGHT ONLY               
ELBFL40  SR    R1,R1                                                            
         IC    R1,MIDTLN           LENGTH OF COMPARE                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),MIDTHRS                                                  
         BNE   ELBFL60                                                          
         OI    FLAG,FLGMID         SET INDICATOR                                
         TM    MIDTSTAT,X'80'      DO WE HAVE TO ADD FIELDS?                    
         BNO   ELBFL50                                                          
         LR    RF,R0                                                            
         SHI   RF,3                REMOVE LENGTH FOR 12A OR 12M                 
         MVC   BCWORK,BCSPACES                                                  
         MVC   BCWORK(5),=C'1201A'                                              
         SHI   RF,1                                                             
         EX    RF,*+4                                                           
         MVC   BCWORK+5(0),11(R2)  MOVE IN FROM - ON                            
         AHI   R0,2                                                             
         STC   R0,5(R2)            UPDATE LENGTH OF FIELD                       
         LR    RF,R0                                                            
         SHI   RF,1                                                             
         EX    RF,*+4                                                           
         MVC   8(0,R2),BCWORK                                                   
         B     ELBFL70                                                          
*                                                                               
ELBFL50  MVC   8(5,R2),=C'1201A'   CHANGE 1200A OR 1200M TO 1201A               
         B     ELBFL70                                                          
*                                                                               
ELBFL60  LA    RE,MIDTBLNQ(RE)                                                  
         BCT   RF,ELBFL40                                                       
         DROP  RE                                                               
*                                                                               
ELBFL70  GOTO1 TIMVAL,DMCB,((R0),8(R2)),XTMSTR                                  
         CLI   DMCB,X'FF'                                                       
         BE    ELBFL80             IF NOT A VALID SWIPE TRY HOURS               
         OI    XTMMSTAT,XTMMSWPQ   FIELD IS A SWIPE                             
         TM    FLAG,FLGMID         WAS THE SWIPE ALTERED?                       
         BNO   ELBFL90                                                          
         MVC   XTMSTR,=X'0960'     YES - SET IT TO MIDNIGHT HEX'2400'           
         B     ELBFL90                                                          
*                                                                               
ELBFL80  BRAS  RE,VALHRS                                                        
         BNE   ACCERRX             ERROR IF NOT VALID SWIPE OR HOURS            
         ZAP   XTMBLK,BCHOURS      IF NOT SWIPE SAVE AS HOURS                   
*                                                                               
ELBFL90  LA    R5,XTMLN4Q(R5)      BUMP TO NEXT MINI                            
ELBFL100 BAS   RE,BMPFLD                                                        
         B     ELBFL20                                                          
*                                                                               
ELBFL110 SR    R1,R1                                                            
         IC    R1,FLDCNT           NUMBER OF FIELDS FILLED IN                   
         CH    R1,=H'0'            DID WE ADD ANYTHING?                         
         BE    ELBFX               NO - EXIT                                    
         STC   R1,XTMMINI          NUMBER OF MINI ELEMENTS                      
         SR    R5,R6                                                            
         STC   R5,XTMLN            ELEMENT LENGTH                               
         OI    FLAG,FLGELM         SET FLAG TO SHOW WE HAVE AN ELEM             
         BAS   RE,ADDIT            ADD ELEMENT TO TEMPO X-REF RECORD            
*                                                                               
ELBFX    B     EXIT                                                             
         EJECT                                                                  
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
* LOOP THROUGH THE RECORD IN AIO AND DELETE ALL BF01 AND BF02        *          
**********************************************************************          
         SPACE 1                                                                
DELBF    NTR1                                                                   
         L     R1,AIO                                                           
         LA    R2,TSXRFST-TSXKEY(R1)      POINT TO FIRST ELEMENT                
         NI    FLAG,X'FF'-FLGBF    CLEAR FLAG                                   
*                                                                               
DELBF10  CLI   0(R2),0             AT THE END OF THE RECORD?                    
         BE    DELBFX                                                           
         CLI   0(R2),X'BF'         DELETE ALL BF ELEMENTS                       
         BNE   DELBF20                                                          
*                                                                               
         USING XTMELD,R2                                                        
         CLI   XTMTYP,XTMTLIN      IS THIS A LINE INFO ELEMENT?                 
         BE    *+12                                                             
         CLI   XTMTYP,XTMTNAR      IS THIS A NARRATIVE?                         
         BNE   DELBF20                                                          
         CLC   XTMTLIN#,TMPLIN#    SAME TEMPO LINE NUMBER                       
         BNE   DELBF20                                                          
         MVI   0(R2),DELELQ        MARK AS DELETED                              
         OI    FLAG,FLGBF          SET FLAG FOR DELETING                        
*                                                                               
DELBF20  SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     DELBF10                                                          
*                                                                               
DELBFX   TM    FLAG,FLGBF          ANYTHING TO DELETE?                          
         BNO   EXIT                                                             
         GOTO1 DELIT,0             DELETE ONLY THESE ELEMENTS                   
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* GET DATE FROM SCREEN AND MAKE SURE IT IS VAILID                    *          
**********************************************************************          
         SPACE 1                                                                
GETDTE   NTR1                                                                   
         L     R2,0(R1)            HEADER FIELD OF DATE TO BE TESTED            
         CLI   5(R2),0                                                          
         BE    GETDTEX                                                          
         L     R5,4(R1)            DATE FIELD IN ELEMENT                        
         XC    0(3,R5),0(R5)                                                    
*                                                                               
         LA    R1,8(R2)            DATE FIELD                                   
         ST    R1,DMCB                                                          
         ZIC   R1,5(R2)                                                         
         STC   R1,DMCB                                                          
         MVC   BYTE,LANGCODE                                                    
         OI    BYTE,X'60'                                                       
         GOTO1 PERVAL,DMCB,,(BYTE,BLOCK)                                        
         CLI   DMCB+4,X'04'                                                     
         BNE   EINVDATE                                                         
         LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
         MVC   0(3,R5),PVALPSTA     PWOS                                        
GETDTEX  B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
**********************************************************************          
* ADD ELEMENT TO TEMPO X-REF RECORD                                  *          
**********************************************************************          
         SPACE 1                                                                
         USING ACTRECD,R6                                                       
ADDIT    NTR1                                                                   
         L     R6,AIO                                                           
*                                                                               
* BUMP UP SEQUENCE NUMBER IN BF03(LINE)ELEMENTS.                                
*                                                                               
         USING XTMELD,RE                                                        
         LA    RE,ELEM                                                          
         CLI   0(RE),XTMELQ        ONLY CHECK FOR BF ELEMENTS                   
         BNE   ADDIT99                                                          
         CLI   XTMTYP,XTMTALL      ONLY CARE ABOUT ALLOCATION LINES             
         BNE   ADDIT99                                                          
         XC    SVSEQ,SVSEQ                                                      
*                                                                               
         LA    RF,ACTRFST                                                       
ADDIT10  CLI   0(RF),0                                                          
         BE    ADDIT90                                                          
         CLI   0(RF),XTMELQ                                                     
         BNE   ADDIT20                                                          
         CLI   XTMTYP-XTMELD(RF),XTMTALL                                        
         BNE   ADDIT20                                                          
         SR    R1,R1                                                            
         IC    R1,SVSEQ                                                         
         LA    R1,1(R1)                                                         
         STC   R1,SVSEQ                                                         
*                                                                               
ADDIT20  SR    R1,R1                                                            
         IC    R1,1(RF)                                                         
         AR    RF,R1                                                            
         B     ADDIT10                                                          
*                                                                               
ADDIT90  MVC   XTMSEQ,SVSEQ                                                     
*                                                                               
ADDIT99  LH    R1,ACTRLEN                                                       
         SR    R0,R0                                                            
         IC    R0,ELEM+1                                                        
         AR    R1,R0                                                            
         CH    R1,=H'1950'                                                      
         BNH   *+8                                                              
         BAS   RE,NEWREC           ADD NEW REC TO FILE                          
         LA    RF,=C'ADD=END'                                                   
         CLI   RECNUM,RTTDT        TEMPO DETAIL                                 
         BE    *+12                                                             
         CLI   RECNUM,RTTLN        TEMPO LINE INFO                              
         BNE   *+8                                                              
         LA    RF,0                                                             
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',=C'ACCMST'),(R6),ELEM,(RF)                      
         CLI   DMCB+12,0                                                        
         BE    EXIT                                                             
         DC    H'0'                                                             
         DROP  R6,RE                                                            
         EJECT                                                                  
**********************************************************************          
* DELETE ELEMENT FROM RECORD                                         *          
*        R6 - TSI OR TSX RECORD IN AIO                               *          
*        TWO TYPES OF CALLS                                          *          
*            R1 - 0 DELETE ANY ELEMENT PASSED TO ROUTINE W/CODE FF   *          
*            R1 - 1 DELETE ALL ELEMENTS OFF RECORD(S)                *          
*                 ROUTINE ALSO SETS FLGREC ON IN FLAG                *          
**********************************************************************          
         SPACE 1                                                                
DELIT    NTR1                                                                   
         CHI   R1,1                ARE WE DELETING ALL ELEMENTS?                
         BNE   DELIT20             NO JUST THOSE ELEMENTS DESIRED               
*                                                                               
         LA    R2,TSXRFST-TSXKEY(R6)      POINT TO FIRST ELEMENT                
DELIT10  CLI   0(R2),0             AT THE END OF THE RECORD?                    
         BE    DELIT20                                                          
         CLI   0(R2),X'BD'         DELETE ALL BD ELEMENTS                       
         BE    *+20                                                             
         CLI   0(R2),X'BE'         DELETE ALL BE ELEMENTS                       
         BE    *+12                                                             
         CLI   0(R2),X'BF'         DELETE ALL BF ELEMENTS                       
         BNE   *+12                                                             
         OI    FLAG,FLGREC         MARK REC AS VALIDATED                        
         MVI   0(R2),DELELQ        MARK AS DELETED                              
         SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     DELIT10                                                          
*                                                                               
DELIT20  DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'D',=C'ACCMST'),('DELELQ',(R6)),0,0                 
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* ADD NEW RECORD TO THE FILE                                         *          
**********************************************************************          
         SPACE 1                                                                
         USING TSXRECD,R6                                                       
NEWREC   NTR1                                                                   
         L     R6,AIO                                                           
         OI    TSXRSTA1,X'10'      MARK RECORD TO SHOW SEQ RECS FOLLOW          
         L     RF,PUTREC                                                        
         TM    FLAG,FLGADD         ARE WE ADDING A RECORD?                      
         BNO   *+8                                                              
         L     RF,ADDREC                                                        
         BASR  RE,RF                                                            
*                                                                               
         LA    R6,BIGKEY                                                        
         OI    TSXKSTA1,X'10'      MARK RECORD TO SHOW SEQ RECS FOLLOW          
         GOTO1 WRITE               WRITE OLD DIR BACK ON FILE                   
*                                                                               
         NI    TSXKSTA1,X'FF'-X'10'   TURNOFF SEQ BIT                           
         LA    R6,BIGKEY                                                        
         SR    R1,R1                                                            
         IC    R1,TSXKSBR                                                       
         LA    R1,1(R1)            BUMP SEQUENCE NUMBER                         
         STC   R1,TSXKSBR                                                       
         MVC   SVBIGKEY,BIGKEY     SAVE OFF BIGKEY IF REC NOT FOUND             
         OI    DMINBTS,X'08'       READ FOR DELETED RECORDS                     
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BNE   NEWR10              CREATE NEW RECORD TO ADD                     
         NI    TSXKSTA,X'FF'-X'80'    MARK ACTIVE KEY UNDELETED                 
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         NI    TSXRSTA,X'FF'-X'80'    MARK ACTIVE REC UNDELETED                 
         B     NEWRX               EXIT AND US THIS RECORD TO ADD ELEMS         
*                                                                               
NEWR10   OI    FLAG,FLGADD                                                      
         L     R6,AIO                                                           
         GOTO1 AXAIO,AIO                 CLEAR IO                               
*                                                                               
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'KEYSAVE),KEYSAVE     RESET BIGKEY                       
         MVC   0(L'SVBIGKEY,R6),SVBIGKEY                                        
         LA    R1,L'TSXKEY+L'TSXKSTA+L'TSXKDA                                   
         STCM  R1,3,TSXRLEN        SETUP LENGTH FOR ADDELEM                     
*                                                                               
         MVI   TSXRRI1,TSXKRI1Q                                                 
         MVI   TSXRRI2,TIMKRI2Q                                                 
         MVI   TSXRRI3,TIMKRI3Q                                                 
         MVI   TSXRRI4,TIMKRI4Q                                                 
         TM    GFACTST6,X'40'      RUNNING UNDER SCRIPT UPLOAD                  
         BNO   *+8                                                              
         OI    TSXRSTA1,TSXTEMPO   RECORD CREATED DURING SCRIPT UPLOAD          
*                                                                               
NEWRX    MVI   RDUPDATE,C'N'                                                    
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
* DELETE ALL ADDITIONAL CROSS X-REF RECORDS                          *          
**********************************************************************          
         SPACE 1                                                                
DELETE   NTR1                                                                   
*                                                                               
         USING TSXRECD,R6                                                       
         XC    BIGKEY,BIGKEY       READ FOR TEMPO X-REF RECORD                  
         LA    R6,BIGKEY                                                        
         MVI   TSXKTYP,TSXKTYPQ    X'3E' - TIMESHEET TEMPO RECORD TYPE          
         MVI   TSXKSUB,TSXKSUBQ    X'13' - TIMESHEET TEMPO SUB RECORD           
         MVC   TSXKCPY,CMPY        COMPANY                                      
         LA    RF,SV1RCODE                                                      
         SR    R1,R1                                                            
         IC    R1,BC1RLNQ3                                                      
         SH    R1,=H'1'                                                         
         EX    R1,*+4                                                           
         MVC   TSXKODS(0),0(RF)    ISOLATE OFF/DPT/SUB                          
         OC    TSXKODS,BCSPACES                                                 
         LA    RF,1(R1,RF)                                                      
         IC    R1,BC1RLEV4                                                      
         SH    R1,=H'1'                                                         
         EX    R1,*+4                                                           
         MVC   TSXKPER(0),0(RF)    ISOLATE PERSON CODE                          
         OC    TSXKPER,BCSPACES                                                 
         MVC   TSXKEND,PRDENDTE    PERIOD END DATE                              
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         B     DEL20                                                            
*                                                                               
DEL10    MVI   RDUPDATE,C'Y'                                                    
         GOTO1 SEQ                                                              
DEL20    CLC   BIGKEY(TSXKSBR-TSXKEY),KEYSAVE                                   
         BNE   DEL30                                                            
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         CLI   TSXKSBR,0           ARE WE AT THE FIRST RECORD?                  
         BE    *+8                                                              
         OI    TSXRSTA,X'80'       MARK ACCMST RECORD DELETED                   
         NI    TSXKSTA1,X'FF'-X'10'   TURN OFF FLAG                             
         GOTO1 DELIT,1             DELETE ELEMENTS OFF RECORD                   
         GOTO1 PUTREC              WRITE ACCMST RECORD BACK                     
         LA    R6,BIGKEY           RESET POINTER                                
         CLI   TSXKSBR,0           ARE WE AT THE FIRST RECORD?                  
         BE    DEL10                                                            
         OI    TSXKSTA,X'80'       MARK ACTIVE DELETED                          
         GOTO1 WRITE               WRITE ACCDIR ACTIVE POINTER                  
         B     DEL10                                                            
*                                                                               
DEL30    DS    0H                                                               
         SR    R1,R1                                                            
         ICM   R1,7,PRDENDTE                                                    
         LNR   R1,R1                                                            
         STCM  R1,7,SVDTE                                                       
*                                                                               
         USING TSIRECD,R6                                                       
         LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY       READ FOR T/S DETAIL RECORD                   
         MVI   TSIKTYP,TSIKTYPQ    X'3E' - TIMESHEET DETAIL RECORD TYPE         
         MVI   TSIKSUB,TSIKSUBQ    X'14' - TIMESHEET DETAIL SUB RECORD          
         MVC   TSIKCPY,CMPY        COMPANY                                      
         MVC   TSIKUNT(2),=C'1R'   ALWAYS 1R                                    
         MVC   TSIKACT,SV1RCODE    PUT IN ACCOUNT CODE                          
         MVC   TSIKOFF,SVOFF       PUT IN OFFICE CODE                           
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         B     DEL50                                                            
*                                                                               
DEL40    MVI   RDUPDATE,C'Y'                                                    
         GOTO1 SEQ                                                              
DEL50    CLC   BIGKEY(TSIKOFF-TSIKEY),KEYSAVE                                   
         BNE   DELX                                                             
*                                                                               
         LA    R6,BIGKEY                                                        
         CLC   TSIKPEDT,SVDTE      ARE WE DOIN THE SAME DATE?                   
         BNE   DEL40                                                            
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         OI    TSIRSTA,X'80'       MARK ACCMST RECORD DELETED                   
         NI    TSIKSTA1,X'FF'-X'10'   TURN OFF FLAG                             
         GOTO1 DELIT,1             DELETE ELEMENTS OFF RECORD                   
         GOTO1 PUTREC              WRITE ACCMST RECORD BACK                     
         LA    R6,BIGKEY           RESET POINTER                                
         OI    TSIKSTA,X'80'       MARK ACTIVE DELETED                          
         GOTO1 WRITE               WRITE ACCDIR ACTIVE POINTER                  
         B     DEL40               GET NEXT REC                                 
*                                                                               
DELX     DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
* GET TIME RECORDS                                                   *          
**********************************************************************          
         SPACE 1                                                                
         USING TSWRECD,R6                                                       
GETTIME  NTR1                                                                   
         MVC   FULL,AIO            SAVE IO                                      
         MVC   AIO,AIO3            SET IO TO READ TMS PASSIVE POINTER           
         XC    BIGKEY,BIGKEY       READ FOR TIME RECORD                         
         LA    R6,BIGKEY                                                        
         MVI   TSWKTYP,TSWKTYPQ    X'3E' - TIMESHEET WEEKLY PASSIVE             
         MVI   TSWKSUB,TSWKSUBQ    X'0F'                                        
         MVC   TSWKCPY,CMPY        COMPANY                                      
         LA    RF,SV1RCODE                                                      
         SR    R1,R1                                                            
         IC    R1,BC1RLNQ3                                                      
         SH    R1,=H'1'                                                         
         EX    R1,*+4                                                           
         MVC   TSWKODS(0),0(RF)    ISOLATE OFF/DPT/SUB                          
         OC    TSWKODS,BCSPACES                                                 
         LA    RF,1(R1,RF)                                                      
         IC    R1,BC1RLEV4                                                      
         SH    R1,=H'1'                                                         
         EX    R1,*+4                                                           
         MVC   TSWKPER(0),0(RF)    ISOLATE PERSON CODE                          
         OC    TSWKPER,BCSPACES                                                 
         SR    R1,R1                                                            
         ICM   R1,7,PRDENDTE                                                    
         LNR   R1,R1                                                            
         STCM  R1,7,TSWKEND        PERIOD END DATE                              
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     GETT20                                                           
*                                                                               
GETT10   MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
GETT20   CLC   BIGKEY(TSWKULC-TSWKEY),KEYSAVE                                   
         BE    GETT30                                                           
         LA    R2,TLNPERH           POSITION CURSOR                             
         MVC   GERROR,=Y(ACERECNF)  RECORD NOT FOUND                            
         B     ERECNF                                                           
*                                                                               
GETT30   MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         XC    SVCACT,SVCACT       CONTRA ACCOUNT                               
         XC    SVOFF,SVOFF         OFFICE CODE                                  
*                                                                               
         USING TIMRECD,R6                                                       
         L     R6,AIO                                                           
*                                                                               
         USING TIMELD,R2                                                        
         LA    R2,TIMRFST                                                       
GETT40   CLI   0(R2),0             END OF RECORD?                               
         BE    GETT10                                                           
         CLI   TIMEL,TIMELQ        SAVE OFF 1C ACCOUNT AND TMS LINE #           
         BNE   GETT50                                                           
         CLI   TIMETYP,TIMEINP                                                  
         BE    GETT60                                                           
         CLI   TIMETYP,TIMEXTRA                                                 
         BE    GETT70                                                           
*                                                                               
GETT50   SR    R1,R1                                                            
         IC    R1,TIMLN            BUMP TO NEXT TIMELD                          
         AR    R2,R1                                                            
         B     GETT40                                                           
*                                                                               
GETT60   MVC   SVCACT,TIMKULC      CONTRA ACCOUNT                               
         MVC   SVOFF,TIMKOFF       OFFICE CODE                                  
         MVC   TMSLIN#,TIMLINE#    LINE NUMBER                                  
         B     GETT50                                                           
*                                                                               
GETT70   CLC   TMPLIN#,TIMXTLN#    MATCH ON TEMPO LINE NUMBER                   
         BNE   GETT50                                                           
*                                                                               
GETX     DS    0H                                                               
         MVC   AIO,FULL            RESET IO                                     
         B     EXIT                                                             
         DROP  R2,R6                                                            
         EJECT                                                                  
**********************************************************************          
* DISPLAY RECORD                                                     *          
**********************************************************************          
         SPACE 1                                                                
DR       DS    0H                                                               
         BRAS  RE,DISPLAY                                                       
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* BUMP TO NEXT FIELD                                                 *          
**********************************************************************          
         SPACE 1                                                                
BMPFLD   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)            IC IN LENGTH TO BUMP                         
         AR    R2,R0               BUMP TO NEXT FIELD                           
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
* SETUP                                                              *          
**********************************************************************          
         SPACE 1                                                                
SETUP    NTR1                                                                   
         MVI   FLAG,0              INITIALIZE FLAG                              
*                                                                               
         SR    R2,R2                                                            
         SR    R3,R3                                                            
         LA    R2,MPFTABLE                                                      
         LA    R3,THDPFKYH                                                      
         CLI   RECNUM,RTTHD        TEMPO HEADER                                 
         BE    SETUP10                                                          
         LA    R3,TCMPFKYH                                                      
         CLI   RECNUM,RTTCM        TEMPO COMMUTER CODE                          
         BE    SETUP10                                                          
         LA    R3,TDTPFKYH                                                      
         CLI   RECNUM,RTTDT        TEMPO DETAIL LINE SCREEN                     
         BE    SETUP10                                                          
         LA    R3,TLNPFKYH         MUST BE TEMPO LINE INFO SCREEN               
*                                                                               
SETUP10  GOTO1 INITIAL,DMCB,(X'40',(R2)),(R3)   INITIALIZE THE PFKEYS           
*                                                                               
SETX     B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* RANDOM STUFF                                                       *          
*        GENERAL MESSAGES                                            *          
**********************************************************************          
         SPACE 1                                                                
ERRMISS  MVI   GERROR1,MISSING                                                  
         B     ERRX                                                             
ERRINV   MVI   GERROR1,INVALID                                                  
         B     ERRX                                                             
ERRDEL   MVI   GERROR1,ACIRDEL     SHOW RECORD WAS DELETED                      
         B     ERRIX               SET INFO TYPE AND EXIT                       
ERRPLS   MVI   GERROR1,2                                                        
ERRIX    MVI   GMSGTYPE,C'I'                                                    
ERRX     MVI   GMSYS,X'FF'         GENERAL MESSAGE SYSTEM                       
         GOTO1 MYERR                                                            
*                                                                               
ERECNF   MVC   GERROR,=AL2(ACERECNF)   RECORD NOT FOUND                         
         B     ACCERRX                                                          
EONLYLOW MVC   GERROR,=AL2(ACENOLOW)   SAL TO LOW LEVEL ONLY                    
         B     ACCERRX                                                          
EENDREQD MVC   GERROR,=AL2(ACEENDRQ)   END REQD WITH YTD                        
         B     ACCERRX                                                          
ERATE99  MVC   GERROR,=AL2(ACERT99)    RATE NOT > 99%                           
         B     ACCERRX                                                          
EINVBASE MVC   GERROR,=AL2(ACEINVBA)   INVALID BASIS FOR TYPE                   
         B     ACCERRX                                                          
EDATECON MVC   GERROR,=AL2(ACEDCONF)   CONFLICTING DATES                        
         B     ACCERRX                                                          
EINVACC  MVC   GERROR,=AL2(ACEACCT)                                             
         B     ACCERRX                                                          
ETOOSHRT MVC   GERROR,=AL2(ACELSHO)                                             
         B     ACCERRX                                                          
ETOOLONG MVC   GERROR,=AL2(ACELLONG)                                            
         B     ACCERRX                                                          
EMISHIGH MVC   GERROR,=AL2(ACEHIGH)    MISSING HIGHER LEVELS                    
         B     ACCERRX                                                          
EINVDATE MVC   GERROR,=AL2(ACEIVDTE)                                            
         B     ACCERRX                                                          
EINVTYPE MVC   GERROR,=AL2(ACEIVTYP)                                            
         B     ACCERRX                                                          
EINVOPT  MVC   GERROR,=AL2(ACEINVOP)                                            
         B     ACCERRX                                                          
ACCERRX  MVI   GMSYS,6             ACC MESSAGE SYSTEM                           
         GOTO1 MYERR                                                            
*                                                                               
XYES     SR    RC,RC                                                            
XNO      LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* TABLES/CONSTANTS                                                    *         
***********************************************************************         
         SPACE 1                                                                
STATAB   DS    0CL2                TIMESHEET STATUS TABLE                       
         DC    C'1',X'01'                                                       
         DC    C'2',X'02'                                                       
         DC    C'3',X'03'                                                       
         DC    C'4',X'04'                                                       
         DC    C'5',X'05'                                                       
STATABQ  EQU   (*-STATAB)/L'STATAB                                              
*                                                                               
MIDNTAB  DS    0CL2                TIMESHEET STATUS TABLE                       
         DC    X'02',C'12A',X'0000',X'80'                                       
         DC    X'02',C'12M',X'0000',X'80'                                       
         DC    X'04',C'1200A',X'00'                                             
         DC    X'04',C'1200M',X'00'                                             
MIDNTABQ EQU   (*-MIDNTAB)/MIDTBLNQ                                             
*                                                                               
DELELQ   EQU   X'FF'               ELEMENT CODE TO DELETE                       
         EJECT                                                                  
**********************************************************************          
* MAINTENANCE SCREEN PFKEY TABLE DEFINITIONS                         *          
**********************************************************************          
         SPACE 1                                                                
MPFTABLE DS    0C                                                               
*                                                                               
*        HEADER SCREEN                                                          
*                                                                               
         DC    AL1(MPF01X-*,01,PFTCPROG,(MPF01X-MPF01)/KEYLNQ,0)                
         DC    CL3' '                                                           
         DCDD  AC#TMPHD,8                                                       
         DC    CL8' '                                                           
MPF01    DC    AL1(KEYTYTWA,L'THDPER-1),AL2(THDPER-T61DFFD)                     
MPF01X   EQU   *                                                                
*                                                                               
*        COMMUTER CODE                                                          
*                                                                               
         DC    AL1(MPF02X-*,02,PFTCPROG,(MPF02X-MPF02)/KEYLNQ,0)                
         DC    CL3' '                                                           
         DCDD  AC#TMPCM,8                                                       
         DC    CL8' '                                                           
MPF02    DC    AL1(KEYTYTWA,L'TCMPER-1),AL2(TCMPER-T61DFFD)                     
MPF02X   EQU   *                                                                
*                                                                               
*        LINE DETAIL SCREEN                                                     
*                                                                               
         DC    AL1(MPF03X-*,03,PFTCPROG,(MPF03X-MPF03)/KEYLNQ,0)                
         DC    CL3' '                                                           
         DCDD  AC#TMPDT,8                                                       
         DC    CL8' '                                                           
MPF03    DC    AL1(KEYTYTWA,L'TDTPER-1),AL2(TDTPER-T61DFFD)                     
MPF03X   EQU   *                                                                
*                                                                               
*        LINE DETAIL SCREEN                                                     
*                                                                               
         DC    AL1(MPF04X-*,04,PFTCPROG,(MPF04X-MPF04)/KEYLNQ,0)                
         DC    CL3' '                                                           
         DCDD  AC#TMPLN,8                                                       
         DC    CL8' '                                                           
MPF04    DC    AL1(KEYTYTWA,L'TLNPER-1),AL2(TLNPER-T61DFFD)                     
MPF04X   EQU   *                                                                
*                                                                               
*        RETURN TO CALLER                                                       
*                                                                               
         DC    AL1(MPF12X-*,12,PFTRPROG,0,0)                                    
         DC    CL3' ',CL8' ',CL8' '                                             
MPF12X   EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* TYPE OF TIME TABLE                                                  *         
***********************************************************************         
         SPACE 1                                                                
TIMETAB  DS    0C                                                               
         DC    C'  ',AL1(TIMTCN)                                                
         DC    C'N ',AL1(TIMTCN)                                                
         DC    C'R ',AL1(TIMTCR)                                                
         DC    C'B ',AL1(TIMTCB)                                                
         DC    C'16',AL1(TIMTNC)                                                
         DC    C'03',AL1(TIMTCN)                                                
         DC    C'02',AL1(TIMTCR)                                                
         DC    C'01',AL1(TIMTCB)                                                
         DC    C'3 ',AL1(TIMTCN)                                                
         DC    C'2 ',AL1(TIMTCR)                                                
         DC    C'1 ',AL1(TIMTCB)                                                
         DC    XL2'00',AL1(TIMTCN)                                              
         DC    C'N',X'00',AL1(TIMTCN)                                           
         DC    C'R',X'00',AL1(TIMTCR)                                           
         DC    C'B',X'00',AL1(TIMTCB)                                           
         DC    C'3',X'00',AL1(TIMTCN)                                           
         DC    C'2',X'00',AL1(TIMTCR)                                           
         DC    C'1',X'00',AL1(TIMTCB)                                           
         DC    XL2'00',AL1(TIMTCN)                                              
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* GETEL                                                               *         
***********************************************************************         
         SPACE 1                                                                
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
* GETEL#2                                                             *         
***********************************************************************         
         SPACE 1                                                                
         GETEL2 R2,DATADISP,ELCODE                                              
         EJECT                                                                  
***********************************************************************         
* LTORG                                                               *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DISPLAY RECORD                                                     *          
**********************************************************************          
         SPACE 1                                                                
DISPLAY  NTR1  BASE=*,LABEL=*                                                   
*        NI    FLAG,X'FF'-FLGKEY   RESET KEY CHANGE FLAG                        
         BRAS  RE,CLRSCR           CLEAR SCREEN                                 
         BAS   RE,DISHD                                                         
         BAS   RE,DISCM                                                         
         BAS   RE,DISDT                                                         
         BAS   RE,DISLN                                                         
*                                                                               
         CLI   ACTEQU,ACTDIS       IF ACTION=DISPLAY - DISPLAY                  
         BE    *+12                                                             
         TM    FLAG,FLGREC         WAS RECORD VALIDATED?                        
         BNO   DRX                                                              
         LA    R1,THDPERH          PERIOD IS FIRST FIELD ON SCREEN              
         ST    R1,AEDTFLD                                                       
         MVC   GERROR,=Y(ACIACTN) ACTION COMPLETED                              
         MVI   SVTMSYS,X'00'       USE ACCOUNTING SYSTEM                        
         B     DRGERRX                                                          
DRX      MVC   GERROR,=Y(ACICHNG)  ENTER CHANGES                                
         MVI   SVTMSYS,X'FF'       USE GENERAL SYSTEM                           
         B     DRGERRX                                                          
         EJECT                                                                  
**********************************************************************          
* DISPLAY TEMPO HEADER SCREEN                                        *          
**********************************************************************          
         SPACE 1                                                                
DISHD    NTR1                                                                   
         CLI   RECNUM,RTTHD        TEMPO HEADER                                 
         BNE   DISHDX                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         LA    R1,THDPPIDH         SAVED 1ST FIELD FOR EDIT                     
         ST    R1,AEDTFLD                                                       
*                                                                               
         BAS   RE,SREC             FIND CORRECT RECORD                          
*                                                                               
         USING XTHELD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,XTHELQ       X'BD' - TEMPO HEADER ELEMENT                 
         BAS   RE,GETEL                                                         
         BNE   DISHDX                                                           
*                                                                               
* DISPLAY B HOURS                                                               
*                                                                               
         LA    R2,THDBHRSH         HOURS FIELD HEADER                           
         MVC   BCWORK,BCSPACES                                                  
         CURED XTHBHRS,(L'THDBHRS,BCWORK),2,ALIGN=LEFT                          
         MVC   8(L'THDBHRS,R2),BCWORK                                           
         OI    6(R2),X'80'                                                      
*                                                                               
* DISPLAY R HOURS                                                               
*                                                                               
         LA    R2,THDRHRSH         HOURS FIELD HEADER                           
         MVC   BCWORK,BCSPACES                                                  
         CURED XTHRHRS,(L'THDRHRS,BCWORK),2,ALIGN=LEFT                          
         MVC   8(L'THDRHRS,R2),BCWORK                                           
         OI    6(R2),X'80'                                                      
*                                                                               
* DISPLAY N HOURS                                                               
*                                                                               
         LA    R2,THDNHRSH         HOURS FIELD HEADER                           
         MVC   BCWORK,BCSPACES                                                  
         CURED XTHNHRS,(L'THDNHRS,BCWORK),2,ALIGN=LEFT                          
         MVC   8(L'THDNHRS,R2),BCWORK                                           
         OI    6(R2),X'80'                                                      
*                                                                               
* DISPLAY N/C HOURS                                                             
*                                                                               
         LA    R2,THDNCHRH         HOURS FIELD HEADER                           
         MVC   BCWORK,BCSPACES                                                  
         CURED XTHNCHRS,(L'THDNCHR,BCWORK),2,ALIGN=LEFT                         
         MVC   8(L'THDNCHR,R2),BCWORK                                           
         OI    6(R2),X'80'                                                      
*                                                                               
* DISPLAY STATUS                                                                
*                                                                               
         GOTO1 HEXOUT,DMCB,XTHSTAT,BCWORK,L'XTHSTAT                             
         MVC   THDTSST,BCWORK+1    TIMESHEET STATUS                             
         OI    THDTSSTH+6,X'80'    TRANSMIT                                     
*                                                                               
* DISPLAY COUNTS LINE/LINE-DAY/SWIPES                                           
*                                                                               
         EDIT  XTHLCNT,THDLCNT,ZERO=NOBLANK    LINE COUNT                       
         OI    THDLCNTH+6,X'80'                TRANSMIT                         
         EDIT  XTHLDCNT,THDLDCT,ZERO=NOBLANK   HOURS/DAY COUNT                  
         OI    THDLDCTH+6,X'80'                TRANSMIT                         
         EDIT  XTHSCNT,THDSCNT,ZERO=NOBLANK    SWIPE COUNT                      
         OI    THDSCNTH+6,X'80'                TRANSMIT                         
*                                                                               
* DISPLAY PIDS - PERSONAL/SUBMIT/APPROVE/UPLOAD                                 
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,XTHPPID                                                     
         EDIT  (RE),THDPPID        PERSONAL PID                                 
         OI    THDPPIDH+6,X'80'    TRANSMIT                                     
         ICM   RE,3,XTHSPID                                                     
         EDIT  (RE),THDSPID        SUBMIT   PID                                 
         OI    THDSPIDH+6,X'80'    TRANSMIT                                     
         ICM   RE,3,XTHAPID                                                     
         EDIT  (RE),THDAPID        APPROVED PID                                 
         OI    THDAPIDH+6,X'80'    TRANSMIT                                     
         ICM   RE,3,XTHUPID                                                     
         EDIT  (RE),THDUPID        UPLOAD   PID                                 
         OI    THDUPIDH+6,X'80'    TRANSMIT                                     
*                                                                               
* DISPLAY ERROR                                                                 
*                                                                               
         MVC   THDUPER,XTHERR      UPLOAD ERROR                                 
         OI    THDUPERH+6,X'80'    TRANSMIT                                     
*                                                                               
* DISPLAY DATES - START/END/SUBMIT/APPROVE/UPLOAD                               
*                                                                               
         CLC   XTHSTDTE,BCSPACES                                                
         BNH   DISHD10                                                          
         GOTO1 DATCON,BCDMCB,(1,XTHSTDTE),(17,THDSTR)                           
         OI    THDSTRH+6,X'80'     TRANSMIT                                     
*                                                                               
DISHD10  CLC   XTHEND,BCSPACES                                                  
         BNH   DISHD20                                                          
         GOTO1 DATCON,BCDMCB,(1,XTHEND),(17,THDEND)                             
         OI    THDENDH+6,X'80'     TRANSMIT                                     
*                                                                               
DISHD20  CLC   XTHSDTE,BCSPACES                                                 
         BNH   DISHD30                                                          
         GOTO1 DATCON,BCDMCB,(1,XTHSDTE),(17,THDSDTE)                           
         OI    THDSDTEH+6,X'80'    TRANSMIT                                     
*                                                                               
DISHD30  CLC   XTHADTE,BCSPACES                                                 
         BNH   DISHD40                                                          
         GOTO1 DATCON,BCDMCB,(1,XTHADTE),(17,THDADTE)                           
         OI    THDADTEH+6,X'80'    TRANSMIT                                     
*                                                                               
DISHD40  CLC   XTHUDTE,BCSPACES                                                 
         BNH   DISHDX                                                           
         GOTO1 DATCON,BCDMCB,(1,XTHUDTE),(17,THDUDTE)                           
         OI    THDUDTEH+6,X'80'    TRANSMIT                                     
*                                                                               
DISHDX   B     DRROUTE                                                          
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
* DISPLAY TEMPO COMMUTER CODE SCREEN                                 *          
**********************************************************************          
         SPACE 1                                                                
DISCM    NTR1                                                                   
         CLI   RECNUM,RTTCM        TEMPO COMMUTER CODE                          
         BNE   DISCMX                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         LA    R1,TCMDAYH          SAVED 1ST FIELD FOR EDIT                     
         ST    R1,AEDTFLD                                                       
*                                                                               
         LA    R2,TCMDAYH          FIRST FIELD ON SCREEN                        
         ST    R2,ACURFLD                                                       
*                                                                               
         BAS   RE,SREC                                                          
*                                                                               
         USING XTCELD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,XTCELQ       X'BE'-TEMPO COMMUTER CODE ELEMENT            
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DISCM05  BAS   RE,NEXTEL                                                        
         BNE   DISCMX                                                           
*                                                                               
         L     R2,ACURFLD          CURRENT FIELD                                
DISCM10  OC    XTCDAY,XTCDAY       ANY DAY NUMBER???                            
         BZ    DISCM30                                                          
         GOTO1 HEXOUT,DMCB,XTCDAY,BCWORK,L'XTCDAY                               
         MVC   8(L'TCMDAY,R2),BCWORK    DAY FIELD                               
         OI    6(R2),X'80'                                                      
*                                                                               
DISCM30  BAS   RE,BMPFLD2          BUMP TO NEXT FIELD                           
         OC    XTCCOM,XTCCOM       ANY COMMUTER CODE?????                       
         BZ    *+14                NO - SKIP IT                                 
         MVC   8(L'XTCCOM,R2),XTCCOM    COMMUTER CODE                           
         OI    6(R2),X'80'                                                      
*                                                                               
         BAS   RE,BMPFLD2          BUMP TO NAR FIELD                            
         SR    R1,R1                                                            
         IC    R1,XTCLN            MINI ELEMENT LENGTH                          
         SH    R1,=Y(XTCLN1Q)      SUBTRACT OVRHD TO GET NAR LEN                
         BZ    DISCM60                                                          
*                                                                               
         LA    RF,XTCNAR           ELEMENT NARRATIVE FIELD                      
DISCM40  CH    R1,=Y(L'TCMNAR)     IS IT MORE THAN ONE NARRATIVE FIELD          
         BNH   DISCM50             NO - DO EX MOVE AND CONTINUE                 
         MVC   8(L'TCMNAR,R2),0(RF)   NARRATIVE FIELD                           
         OI    6(R2),X'80'            TRANSMIT                                  
         LA    RE,L'TCMNAR         LENGTH OF SCREEN FIELD                       
         AR    RF,RE               BUMP TO 1 POS IN EL NAR FLD FOR EX           
         SR    R1,RE               SUBTRACT THAT AMOUNT FROM NAR LEN            
         BAS   RE,BMPFLD2          BUMP TO DAY FIELD                            
         BAS   RE,BMPFLD2          BUMP TO COM CODE FIELD                       
         BAS   RE,BMPFLD2          BUMP TO NEXT NAR FIELD                       
         B     DISCM40                                                          
*                                                                               
DISCM50  BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   8(0,R2),0(RF)       NARRATIVE FIELD                              
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
DISCM60  BAS   RE,BMPFLD2          BUMP TO NEXT FIELD                           
         ST    R2,ACURFLD                                                       
         B     DISCM05                                                          
*                                                                               
DISCMX   B     DRROUTE                                                          
         DROP  R5,R6                                                            
         EJECT                                                                  
**********************************************************************          
* DISPLAY TEMPO DETAIL LINE SCREEN                                   *          
**********************************************************************          
         SPACE 1                                                                
DISDT    NTR1                                                                   
         CLI   RECNUM,RTTDT        TEMPO DETAIL                                 
         BNE   DISDTX                                                           
         MVC   AIO,AIO2                                                         
*                                                                               
         LA    R1,TDTLN#H          SAVED 1ST FIELD FOR EDIT                     
         ST    R1,AEDTFLD                                                       
*                                                                               
         BAS   RE,FREC             FIND CORRECT RECORD                          
         BNE   DISDTX                                                           
*                                                                               
         USING XTMELD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,XTMELQ       X'BF'-TEMPO DETAIL LINE ELEMENT              
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DISDT10  BAS   RE,NEXTEL                                                        
         BNE   DISDTX                                                           
*                                                                               
         CLC   XTMTLIN#,TMPLIN#    SAME LINE NUMBER AS REQUESTED?               
         BNE   DISDT10                                                          
*                                                                               
         CLI   XTMTYP,XTMTNAR      NARRATIVE ELEMENT                            
         BE    DISDT30                                                          
         CLI   XTMTYP,XTMTLIN      DETAIL LINE ONLY                             
         BNE   DISDT10                                                          
*                                                                               
* DETAIL LINE INFORMATION                                                       
*                                                                               
         LA    R2,TDTTYPH          TYPE OF TIME HEADER                          
         CLI   XTMTTYP,TIMTNC      IS THIS 'N/C' TIME?                          
         BNE   *+12                                                             
         MVI   8(R2),C'N'          TYPE FIELD                                   
         B     DISDT25                                                          
*                                                                               
         USING TIMETABD,RE                                                      
         LA    RE,TIMETAB                                                       
DISDT20  CLI   0(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   TTTYPE,XTMTTYP                                                   
         BNE   *+14                                                             
         CLC   TTFIELD,BCSPACES                                                 
         BNE   *+12                                                             
         LA    RE,TTLENQ(RE)                                                    
         B     DISDT20                                                          
         MVC   8(L'TTFIELD,R2),TTFIELD    TYPE FIELD                            
         DROP  RE                                                               
DISDT25  OI    6(R2),X'80'                                                      
*                                                                               
         MVC   BCWORK,BCSPACES     DISPLAY LINE NUMBER                          
         SR    R0,R0                                                            
         ICM   R0,3,XTMTLIN#                                                    
         LA    R2,TDTLN#H          LINE NUMBER HEADED                           
         CURED (R0),(L'TDTLN#,BCWORK),0,ALIGN=LEFT                              
         MVC   8(L'TDTLN#,R2),BCWORK                                            
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,TDTFLGH          DISPLAY DEFAULT FLAG                         
         GOTO1 HEXOUT,BCDMCB,XTMDFLG,BCWORK,L'XTMDFLG                           
         MVC   8(L'TDTFLG,R2),BCWORK                                            
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,TDTHRSH          DISPLAY HOURS                                
         MVC   BCWORK,BCSPACES     HOURS FIELD HEADER                           
         CURED XTMHRS,(L'TDTHRS,BCWORK),2,ALIGN=LEFT                            
         MVC   8(L'TDTHRS,R2),BCWORK                                            
         OI    6(R2),X'80'                                                      
         B     DISDT10                                                          
*                                                                               
* DISPLAY NARRATIVE INFORMATION                                                 
*                                                                               
DISDT30  DS    0H                                                               
         SR    R1,R1                                                            
         IC    R1,XTMLN                                                         
         SH    R1,=Y(XTMLN1Q+1)    SUBTRACT OVERHEAD + 1 FOR EX                 
         STC   R1,SVLN             SAVE OFF LENGTH FOR DISPLAYING               
         LA    R2,TDTNRTH          FIRST LINE OF NARRATIVE HEADER               
         LA    R3,XTMNAR           RE=A(NARRATIVE IN ELEMENT)                   
*                                                                               
DISDT40  SR    R1,R1                                                            
         IC    R1,SVLN                                                          
         SR    RE,RE                                                            
         IC    RE,0(R2)            RE=LENGTH OF FIELD+HEADER                    
         SHI   RE,8                SUBTRACT HEADER                              
         CR    R1,RE               IS THE NARRATIVE MORE THAN THE FLD           
         BNH   DISDT50                                                          
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   8(0,R2),0(R3)                                                    
         OI    6(R2),X'80'                                                      
         LA    RE,1(RE)            ADD BACK THE EX DECREMENT                    
         AR    R3,RE               BUMP TO NEXT POSITION IN NARRATIVE           
         SR    R1,RE               SUBTRACT LAST LEN FROM TOTAL LEN             
         STC   R1,SVLN                                                          
         BAS   RE,BMPFLD2          BUMP TO NEXT NARRATIVE FIELD                 
         B     DISDT40                                                          
*                                                                               
DISDT50  EX    R1,*+4                                                           
         MVC   8(0,R2),0(R3)        NARRATIVE                                   
         OI    6(R2),X'80'                                                      
         B     DISDT10                                                          
*                                                                               
DISDTX   B     DRROUTE                                                          
         EJECT                                                                  
         DROP  R6                                                               
**********************************************************************          
* DISPLAY TEMPO LINE INFO SCREN                                      *          
**********************************************************************          
         SPACE 1                                                                
DISLN    NTR1                                                                   
         CLI   RECNUM,RTTLN        TEMPO LINE INFO                              
         BNE   DISLNX                                                           
         MVC   AIO,AIO2                                                         
*                                                                               
         LA    R1,TLNLN#H          SAVED 1ST FIELD FOR EDIT                     
         ST    R1,AEDTFLD                                                       
*                                                                               
         LA    R2,TLNDAYH          FIRST FIELD ON SCREEN                        
         ST    R2,ACURFLD                                                       
*                                                                               
         BAS   RE,FREC             FIND CORRECT RECORD                          
         BNE   DISLNX                                                           
*                                                                               
         USING XTMELD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,XTMELQ       X'D4'-TEMPO DETAIL LINE ELEMENT              
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DISLN10  BAS   RE,NEXTEL                                                        
         BNE   DISLNX                                                           
*                                                                               
         CLC   XTMTLIN#,TMPLIN#    SAME LINE NUMBER AS REQUESTED?               
         BNE   DISLN10                                                          
         CLI   XTMTYP,XTMTALL      LINE ALLOCTION DAY/SWIPE                     
         BNE   DISLN10                                                          
*                                                                               
* DISPLAY LINE NUMBER                                                           
*                                                                               
         LA    R2,TLNLN#H          LINE NUMBER HEADER                           
         ICM   R0,3,XTMTLIN#                                                    
         CURED (R0),(L'TLNLN#,BCWORK),0,ALIGN=LEFT                              
         MVC   8(L'TLNLN#,R2),BCWORK                                            
         OI    6(R2),X'80'                                                      
*                                                                               
* DISPLAY LINE HOUR INFORMATION - DAYS OR SWIPES                                
*                                                                               
         LA    R5,XTMDATA                                                       
         USING XTMDATA,R5                                                       
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,1,XTMMINI        NUMBER OF MINI ELEMENTS                      
         BZ    DISLNX                                                           
*                                                                               
         L     R2,ACURFLD          FIRST FIELD ON SCREEN                        
         LA    RE,TLNSWPLH         ADDRESS OF LAST FIELD ON SCREEN              
         CR    R2,RE                                                            
         BH    DISLNX                                                           
*                                                                               
DISLN20  GOTO1 HEXOUT,DMCB,XTMDAY,BCWORK,L'XTMDAY                               
         MVC   8(L'TLNDAY,R2),BCWORK                                            
         OI    6(R2),X'80'                                                      
         BAS   RE,BMPFLD2          BUMP TO NEXT FIELD                           
*                                                                               
         TM    XTMMSTAT,XTMMSWPQ   IS IT A SWIPE?                               
         BO    DISLN30                                                          
         MVC   BCWORK,BCSPACES                                                  
         CURED XTMBLK,(L'TLNSWP,BCWORK),2,ALIGN=LEFT                            
         MVC   8(L'TLNSWP,R2),BCWORK                                            
         OI    6(R2),X'80'                                                      
         B     DISLN40                                                          
*                                                                               
DISLN30  MVC   BCWORK,BCSPACES                                                  
         GOTO1 UNTIME,DMCB,(0,XTMSTR),BCWORK                                    
         MVC   8(L'TLNSWP,R2),BCWORK    DAY/SWIPE BLOCK                         
         OI    6(R2),X'80'                                                      
*                                                                               
DISLN40  BAS   RE,BMPFLD2          BUMP TO NEXT FIELD                           
         LA    R5,XTMLN4Q(R5)                                                   
         BCT   R3,DISLN20                                                       
         ST    R2,ACURFLD          SET CURRENT SCREEN FIELD                     
         B     DISLN10                                                          
*                                                                               
DISLNX   B     DRROUTE                                                          
         EJECT                                                                  
         DROP  R5,R6                                                            
***********************************************************************         
* DISPLAY RECORD EXIT POINTS                                          *         
***********************************************************************         
         SPACE 1                                                                
DRROUTL  MVI   BCDUB,0             SET CC LOW                                   
         B     DRROUTCC                                                         
DRROUTH  MVI   BCDUB,2             SET CC HIGH                                  
         B     DRROUTCC                                                         
DRROUTE  MVI   BCDUB,1             SET CC EQUAL                                 
DRROUTCC CLI   BCDUB,1                                                          
DREXIT   XIT1                                                                   
*                                                                               
DRAERRX  MVI   GMSYS,6             DEFAULT IS ACCOUNTING MSG SYSTEM             
         GOTO1 MYERR                                                            
*                                                                               
DRGERRX  MVC   ACURFORC,AEDTFLD    FORCE CURSOR TO EDIT FIELD                   
         OI    GENSTAT2,USGETTXT+USMYOK                                         
         USING GETTXTD,RF                                                       
         LA    RF,GETTXTCB                                                      
         MVC   GTINDX,GINDEX       MESSAGE INDEX                                
         MVC   GTMSGNO,GERROR      MESSAGE NUMBER                               
         MVI   GTMTYP,C'I'         MESSAGE TYPE                                 
         MVC   GTLTXT,GLTXT        LENGTH OF INSERTION TEXT                     
         MVC   GTATXT,GATXT        A(INSERTION TEXT)                            
         CLI   SVTMSYS,X'00'                                                    
         BE    *+10                                                             
         MVC   GTMSYS,SVTMSYS      EITHER ACC OR GENERAL                        
         XIT1                                                                   
         DROP  RF                                                               
         EJECT                                                                  
**********************************************************************          
* BUMP TO NEXT FIELD                                                 *          
**********************************************************************          
         SPACE 1                                                                
BMPFLD2  DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)            IC IN LENGTH TO BUMP                         
         AR    R2,R0               BUMP TO NEXT FIELD                           
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* FREC - GOES THROUGH SEQUENTIAL RECORDS AND FINDS THE CORRECT ONE.   *         
*        USED BY TDETAIL AND TLINE.                                   *         
***********************************************************************         
         SPACE 1                                                                
FREC     NTR1                                                                   
*                                                                               
         USING TSIRECD,R5                                                       
         LA    R5,BIGKEY                                                        
         XC    BIGKEY,BIGKEY       READ FOR T/S DETAIL RECORD                   
         MVI   TSIKTYP,TSIKTYPQ    X'3E' - TIMESHEET DETAIL RECORD TYPE         
         MVI   TSIKSUB,TSIKSUBQ    X'14' - TIMESHEET DETAIL SUB RECORD          
         MVC   TSIKCPY,CMPY        COMPANY                                      
         MVC   TSIKUNT(2),=C'1R'   ALWAYS 1R                                    
         MVC   TSIKACT,SV1RCODE    PUT IN ACCOUNT CODE                          
         MVC   TSIKOFF,SVOFF       PUT IN OFFICE CODE                           
         MVC   TSIKCCPY,CMPY                                                    
         MVC   TSIKULC,SVCACT      CONTRA ACCOUNT                               
         SR    R1,R1                                                            
         ICM   R1,7,PRDENDTE                                                    
         LNR   R1,R1                                                            
         STCM  R1,7,TSIKPEDT                                                    
         OI    DMINBTS,X'08'       READ FOR DELETED RECORDS                     
         GOTO1 HIGH                                                             
         B     FREC20                                                           
*                                                                               
FREC10   GOTO1 SEQ                                                              
FREC20   CLC   BIGKEY(TSIKSBR-TSIKEY),KEYSAVE                                   
         BNE   XNO                                                              
*                                                                               
FREC30   GOTO1 GETREC                                                           
*                                                                               
         USING XTMELD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,XTMELQ       X'BF'-TEMPO DETAIL LINE ELEM                 
         BAS   RE,GETEL                                                         
         B     *+8                                                              
FREC40   BAS   RE,NEXTEL                                                        
         BNE   FREC50                                                           
*                                                                               
         CLI   RECNUM,RTTLN        IS IT TLINE?                                 
         BNE   *+12                NO                                           
         CLI   XTMTYP,XTMTALL      IS IT OF THIS TYPE                           
         BNE   FREC40              NO,LOOK FOR MORE ELEMENTS                    
*                                                                               
         CLI   RECNUM,RTTDT        IS IT TDETAIL?                               
         BNE   *+12                NO                                           
         CLI   XTMTYP,XTMTLIN                                                   
         BNE   FREC40                                                           
*                                                                               
         CLC   XTMTLIN#,TMPLIN#    SAME LINE NUMBER AS REQUESTED?               
         BNE   FREC40                                                           
         B     FRECX               DONE, CORRECT RECORD IN AIO                  
*                                                                               
FREC50   L     R5,AIO                                                           
         TM    TSIRSTA1,X'10'      ARE THERE OTHER SEQ RECORDS?                 
         BO    FREC10              GET NEXT RECORD                              
*                                  NO MORE, SO RECORD NOT FOUND                 
FRECX    B     XYES                SETS CC TO TRUE                              
         DROP  R5,R6                                                            
         EJECT                                                                  
***********************************************************************         
* SREC - SEARCHES THROUGH SEQUENTIAL RECORDS AND FINDS THE CORRECT ONE*         
*        USED BY TCOM AND THEADER.                                    *         
***********************************************************************         
         SPACE 1                                                                
SREC     NTR1                                                                   
         USING TSXRECD,R5                                                       
         XC    BIGKEY,BIGKEY       READ FOR TEMPO X-REF RECORD                  
         LA    R5,BIGKEY                                                        
         MVI   TSXKTYP,TSXKTYPQ    X'3E' - TIMESHEET TEMPO RECORD TYPE          
         MVI   TSXKSUB,TSXKSUBQ    X'13' - TIMESHEET TEMPO SUB RECORD           
         MVC   TSXKCPY,CMPY        COMPANY                                      
         LA    RF,SV1RCODE                                                      
         ZIC   R1,BC1RLNQ3                                                      
         SH    R1,=H'1'                                                         
         EX    R1,*+4                                                           
         MVC   TSXKODS(0),0(RF)    ISOLATE OFF/DPT/SUB                          
         OC    TSXKODS,BCSPACES                                                 
         LA    RF,1(R1,RF)                                                      
         IC    R1,BC1RLEV4                                                      
         SH    R1,=H'1'                                                         
         EX    R1,*+4                                                           
         MVC   TSXKPER(0),0(RF)    ISOLATE PERSON CODE                          
         OC    TSXKPER,BCSPACES                                                 
         MVC   TSXKEND,PRDENDTE    PERIOD END DATE                              
         GOTO1 HIGH                                                             
         B     SREC20                                                           
*                                                                               
SREC10   GOTO1 SEQ                                                              
SREC20   CLC   BIGKEY(TSXKSBR-TSXKEY),KEYSAVE                                   
         BE    SREC30                                                           
         LA    R2,THDPERH           POSITION CURSOR                             
         MVC   GERROR,=Y(ACERECNF)  RECORD NOT FOUND                            
         B     ERECNF                                                           
*                                                                               
SREC30   GOTO1 GETREC                                                           
*                                                                               
         CLI   RECNUM,RTTCM        TEMPO COMMUTER CODE                          
         BE    SRECX               YES SO DONE                                  
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,XTHELQ       X'BD'-TEMPO HEADER ELEMENT                   
         BAS   RE,GETEL                                                         
         BE    SRECX               FOUND ELEMENT                                
*                                                                               
         L     R5,AIO                                                           
         TM    TSXRSTA1,X'10'      ARE THERE OTHER SEQ RECORDS?                 
         BO    SREC10              GET NEXT RECORD                              
*                                                                               
SRECX    XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* LTORG                                                               *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* CLEAR SCREEN AND RESET EVERYTHING                                  *          
**********************************************************************          
         SPACE 1                                                                
CLRSCR   NTR1  BASE=*,LABEL=*                                                   
         CLI   RECNUM,RTTHD        TEMPO HEADER                                 
         BNE   CLRS10                                                           
         LA    R2,THDPPIDH         CLEAR HEADER SCREEN                          
         LA    R3,THDUDTEH                                                      
         B     CLRS40                                                           
*                                                                               
CLRS10   CLI   RECNUM,RTTCM        TEMPO COMMUTER CODE                          
         BNE   CLRS20                                                           
         LA    R2,TCMDAYH          CLEAR COMMUTER CODE SCREEN                   
         LA    R3,TCMNARLH                                                      
         B     CLRS40                                                           
*                                                                               
CLRS20   CLI   RECNUM,RTTDT        TEMPO DETAIL LINE                            
         BNE   CLRS30                                                           
         LA    R2,TDTHRSH          CLEAR DETAIL SCREEN                          
         LA    R3,TDTNRT4H                                                      
         B     CLRS40                                                           
*                                                                               
CLRS30   CLI   RECNUM,RTTLN        TEMPO INFO LINE                              
         BNE   CLRSX                                                            
         LA    R2,TLNDAYH          CLEAR DETAIL SCREEN                          
         LA    R3,TLNSWPLH                                                      
*                                                                               
CLRS40   CR    R3,R2                                                            
         BL    CLRSX               ERROR IF END < START                         
*                                                                               
         CLI   RECNUM,RTTHD        TEMPO HEADER                                 
         BE    *+12                                                             
         CLI   RECNUM,RTTDT        TEMPO DETAIL LINE                            
         BNE   *+12                                                             
         TM    1(R2),X'20'         FOR HEADER/DETIAL SKIP PROT FLDS             
         BO    CLRS50                                                           
*                                                                               
         TWAXC 0(R2),0(R2),PROT=Y                                               
         NI    1(R2),X'FF'-X'20'   UNPROTECT ALL FIELDS                         
         OI    6(R2),X'80'         TRANSMIT                                     
         MVI   5(R2),0                                                          
CLRS50   BAS   RE,BMPFLD3          BUMP TO NEXT FIELD                           
         B     CLRS40                                                           
*                                                                               
CLRSX    XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* BUMP TO NEXT FIELD                                                 *          
**********************************************************************          
         SPACE 1                                                                
BMPFLD3  DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)            IC IN LENGTH TO BUMP                         
         AR    R2,R0               BUMP TO NEXT FIELD                           
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* LTORG                                                               *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE HOURS                                                      *         
*                                                                     *         
* NTRY - R2=A(HOURS FIELD HEADER)                                     *         
*                                                                     *         
* EXIT - CC=EQU - BCHOURS CONTAINS # OF HOURS 2DP                     *         
*      - CC=NEQ - INVALID INPUT & GERROR IS SET CORRECTLY             *         
***********************************************************************         
         SPACE 1                                                                
VALHRS   NTR1  BASE=*,LABEL=*                                                   
         MVI   BCIFMIN,1           REQUIRED FIELD                               
         MVI   BCIFMAX,7           MAX OF 999.99 HOURS                          
         GOTO1 AFVAL,(R2)                                                       
         BH    VRROUTH                                                          
*                                                                               
         ZAP   BCHOURS,=P'0'                                                    
         LA    R0,L'BCIFLD                                                      
         LA    RF,BCIFLD+L'BCIFLD-1                                             
         CLI   0(RF),C' '                                                       
         BNE   *+10                                                             
         BCTR  RF,0                                                             
         BCT   R0,*-10                                                          
*                                                                               
         TM    GFACTST6,X'40'      RUNNING UNDER SCRIPT UPLOAD                  
         BNO   VALH05                                                           
         GOTO1 CASHVAL,DMCB,(X'82',BCIFLD),(X'01',(R0))                         
         B     VALH10                                                           
*                                                                               
VALH05   GOTO1 CASHVAL,DMCB,(X'82',BCIFLD),(R0)                                 
VALH10   CLI   DMCB,X'FF'                                                       
         BNE   *+14                                                             
         MVC   GERROR,=AL2(ACEIVHRS)                                            
         B     VRROUTH                                                          
*                                                                               
         ZAP   BCHOURS,DMCB+8(4)                                                
*        ZAP   BCDUB,BCHOURS       CAN ONLY HAVE QUARTER HOURS                  
*        DP    BCDUB,=P'25'                                                     
*        CP    BCDUB+6(2),=P'0'                                                 
*        BE    *+14                                                             
*        MVC   GERROR,=AL2(ACEQTRHR)                                            
*        B     VRROUTH                                                          
*                                                                               
         CP    BCHOURS,=P'30000'   MAX # HOURS = 300/LINE                       
         BH    *+14                                                             
         CP    BCHOURS,=P'-30000'  MIN # HOURS = -300/LINE                      
         BNL   *+14                                                             
         MVC   GERROR,=AL2(ACEIVHRS)                                            
         B     VRROUTH                                                          
*                                                                               
         B     VRROUTE                                                          
         EJECT                                                                  
***********************************************************************         
* VALREC EXIT POINTS                                                  *         
***********************************************************************         
         SPACE 1                                                                
VRROUTL  MVI   BCDUB,0             SET CC LOW                                   
         B     VRROUTCC                                                         
VRROUTH  MVI   BCDUB,2             SET CC HIGH                                  
         B     VRROUTCC                                                         
VRROUTE  MVI   BCDUB,1             SET CC EQUAL                                 
VRROUTCC CLI   BCDUB,1                                                          
VREXIT   XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LTORG                                                               *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* INCLUDES                                                           *          
**********************************************************************          
         SPACE 1                                                                
*        DDSPOOLD                                                               
*        DDSPLWORKD                                                             
*        DDPERVALD                                                              
*        DDSCANBLKD                                                             
*        FASECRETD                                                              
*        ACCAPWORKD                                                             
*        ACCAPDSECT                                                             
*        ACBMONVALD                                                             
*        ACGENFILE                                                              
*        ACDDEQUS                                                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE ACCAPWORKD                                                     
       ++INCLUDE ACCAPDSECT                                                     
       ++INCLUDE ACBMONVALD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         EJECT                                                                  
**********************************************************************          
* SCREEENS                                                           *          
**********************************************************************          
         SPACE 1                                                                
       ++INCLUDE ACCAPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACCAPC7D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE ACCAPCBD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE ACCAPCAD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE ACCAPCCD                                                       
         ORG   CONHEADH+(3520-STARTWKQ-8)                                       
         EJECT                                                                  
**********************************************************************          
* APPLICATION STORAGE                                                *          
**********************************************************************          
         SPACE 1                                                                
STARTWRK DS    0A                                                               
PRDSTDTE DS    PL3                 PERIOD START DATE                            
PRDENDTE DS    PL3                 PERIOD ENDING DATE                           
PRDNUM   DS    XL1                 PERIOD NUMBER                                
PRDMON   DS    PL2                 PERIOD MONTH                                 
YYMMDD   DS    PL3                                                              
SVYYMMDD DS    PL3                                                              
SVTODAY  DS    PL3                 SAVED AREA FOR TODAY'S DATE                  
SVDTE    DS    XL3                 COMPLIMENT FOR DATES                         
*                                                                               
SAVPERDT DS    CL9                 SAVED PERIOD DATE                            
SAVPERSN DS    CL8                 KEY - SAVED PERSON CODE                      
SAVLIN#  DS    XL2                 SAVED ARE FOR PREVIOUS LINE NUMBER           
*                                                                               
ELMAX    EQU   X'FF'               MAXIMUM ELEMENT LENGTH                       
STARTWKQ EQU   *-STARTWRK                                                       
***********************************************************************         
* DDGENTWA                                                            *         
***********************************************************************         
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
***********************************************************************         
* SAVED STORAGE                                                       *         
***********************************************************************         
         SPACE 1                                                                
STORED   DSECT                     SAVED STORAGE                                
RELO     DS    A                                                                
AEDTFLD  DS    A                   SAVED AREA FOR 1ST FLD ON SCREEN             
ACURFLD  DS    A                   ADDRESS OF CURRENT SCREEN FIELD              
WORK2    DS    10F                                                              
*                                                                               
SVVKBLCK DS    0C                  *** SAVE TOP OF SCREEN ***                   
FLAG     DS    CL1                                                              
FLGKEY   EQU   X'80'               KEY WAS CHANGED                              
FLGREC   EQU   X'40'               RECORD WAS VALIDATED                         
FLGELM   EQU   X'20'               WE DO HAVE AN ELEMENT                        
FLGADD   EQU   X'10'               DO AN ADDREC NOT PUTREC                      
FLGBF    EQU   X'08'               WE HAVE A BF TO DELETE                       
FLGCOM   EQU   X'04'               START OF A DAY/COM COMBO                     
FLGMID   EQU   X'02'               TIME IS MIDNIGHT                             
FLGDEL   EQU   X'01'               REC/KEY WAS MARKED DEL                       
*                                                                               
TMPLIN#  DS    XL2                 TEMPO LINE NUMBER                            
TMSLIN#  DS    XL2                 TMS LINE NUMBER                              
*                                                                               
SVLN     DS    XL1                 SAVED AREA FOR ELEMENT LENGTH                
SVTMSYS  DS    XL1                 SYSTEM FOR ERROR MESSAGES                    
SVCACT   DS    CL14                CONTRA ACCOUNT                               
SVOFF    DS    CL2                 OFFICE                                       
SV1RPRSN DS    CL8                 PERSON CODE                                  
SV1ROFFC DS    CL2                 OFFICE CODE                                  
SV1RDPT  DS    CL3                 DEPARTMENT CODE                              
SV1RSDPT DS    CL3                 SUB DEPARTMENT CODE                          
SV1RSTDT DS    PL3                 LOCATION START DATE                          
SV1RENDT DS    PL3                 LOCATION END   DATE                          
SV1RACT  DS    0XL15               1R ACCOUNT                                   
SV1RCPY  DS    XL1                 1R COMPANY CODE                              
SV1RULA  DS    0CL14               1R UNIT/LEDGER/ACCOUNT                       
SV1RUL   DS    CL2                 1R UNIT/LEDGER                               
SV1RCODE DS    CL12                1R ACCOUNT CODE                              
SVSPINC  DS    CL15                SPECIAL INCOME ACCOUNT                       
SVPIDNO  DS    XL2                 PERSON ID #                                  
SVDFTASK DS    CL2                 DEFAULT TASK CODE                            
SVDFSCRN DS    XL1                 DEFAULT SCREEN CODE                          
SVSEQ    DS    XL1                 SEQUENCE NUMBER FOR BF03 ELEMS               
*                                                                               
SVBIGKEY DS    CL42                SAVED ARE FOR BIGKEY                         
*                                                                               
FLDCNT   DS    XL1                 FIELD COUNT - SWIPES/COMMUTER/HOURS          
SVDAY    DS    CL2                                                              
SVTYP    DS    CL2                 TYPE OF TIME                                 
*                                                                               
SVMINI   DS    XL100               SAVED AREA FOR MINI X'BE' ELEMENT            
SVVKBLKQ EQU   *-SVVKBLCK                                                       
         EJECT                                                                  
***********************************************************************         
* TIME TABLE DSECT                                                    *         
***********************************************************************         
         SPACE 1                                                                
TIMETABD DSECT                     TYPE OF TIME DSECT                           
TTFIELD  DS    CL2                 TIME AS APPEARS ON SCREEN                    
TTTYPE   DS    XL1                 TYPE EQUATE FOR TIMTTYP                      
TTLENQ   EQU   *-TIMETABD                                                       
         EJECT                                                                  
***********************************************************************         
* MIDNIGHT TABLE DSECT                                                *         
***********************************************************************         
         SPACE 1                                                                
MIDNTBD  DSECT                     MIDNIGHT TABLE DSECT                         
MIDTLN   DS    XL1                 LENGTH OF MIDNIGHT FIELD(-1 FOR EX)          
MIDTHRS  DS    CL5                 HOURS                                        
MIDTSTAT DS    XL1                 IF X'80' 2 BYTES HAS TO BE ADDED             
MIDTBLNQ EQU   *-MIDNTBD                                                        
         EJECT                                                                  
***********************************************************************         
* GLOBAL STORAGE                                                      *         
***********************************************************************         
         SPACE 1                                                                
       ++INCLUDE ACCAP30GW                                                      
       ++INCLUDE ACCAP30DST                                                     
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDLANGEQUS                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023ACCAP41   12/11/09'                                      
         END                                                                    

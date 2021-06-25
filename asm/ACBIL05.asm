*          DATA SET ACBIL05    AT LEVEL 004 AS OF 12/17/12                      
*PHASE T60E05A                                                                  
         TITLE 'ACBIL05 - CREATIVE BILLING - LIST'                              
ACBIL05  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LWSX-LWSD,**BIL5**,CLEAR=YES                                     
*                                                                               
         USING GWS,R9                                                           
         USING TWAD,R8                                                          
         USING LWSD,RC                                                          
*                                                                               
         MVC   SWRKEY,WRKEY                                                     
         CLI   LISFRST,0                                                        
         BE    INITHDRS                                                         
         TWAXC LISFRSTH,PROT=Y                                                  
         B     IND00                                                            
         EJECT ,                                                                
***********************************************************************         
* INITIALIZE THE HEADER LINES                                         *         
***********************************************************************         
         SPACE 1                                                                
HD1      USING LINED,R2                                                         
HD2      USING LINED,R3                                                         
         SPACE 1                                                                
INITHDRS DS    0H                                                               
         LA    R2,LISHEDH          ->   HDR  LINE 1                             
         LA    R3,LISHED2H         ->   HDR  LINE 2                             
         LA    R4,2                TWO  SETS                                    
*                                                                               
INITHD10 DS    0H                                                               
*                                  TRANSLATE LOCAL DICTIONARY ITEMS             
         GOTO1 VDICTAT,DMCB,C'LL  ',DICTB05,DICLS05                             
*                                                                               
         MVC   HD1.LNBILL(L'LNBILL),AC$BIL        BILL                          
         MVC   HD2.LNBILL(L'LNBILL),AC$NUM        NUMBER                        
*                                                                               
         MVC   HD1.LNCLIPJ(L'LNCLIPJ),AC$CLIPK CLIENT/PRODUCT/JOB               
         MVC   HD2.LNCLIPJ,=C'------------------'                               
*                                                                               
         MVC   HD1.LNDATE(L'LNDATE-1),AC$DATE     DATE                          
         MVC   HD2.LNDATE(L'LNDATE-1),AC$CRTD     CREATED                       
*                                                                               
         MVC   HD1.LNBILSTA(L'LNBILSTA),AC$4BIL   BILL                          
         MVC   HD2.LNBILSTA(L'LNBILSTA),AC$STT    STATUS                        
*                                                                               
         LA    R2,LINEDLQ1+2(,R2)  ->   NEXT SET  HDR  LINE 1                   
         LA    R3,LINEDLQ1+2(,R3)  ->   NEXT SET  HDR  LINE 2                   
         BCT   R4,INITHD10         INIT NEXT SET                                
         DROP  HD1,HD2                                                          
*                                                                               
         MVI   LISHEDH+6,FVOXMT    TRANSMIT                                     
         MVI   LISHED2H+6,FVOXMT   TRANSMIT                                     
         B     IND00                                                            
         SPACE 3                                                                
CALLDICT DS    0H                                                               
         ST    RE,SAVRE            SAVE     RE                                  
         GOTO1 VDICTAT,DMCB,C'SL  ',(R5),0                                      
         L     RE,SAVRE            RESTORE  RE                                  
         BSM   0,RE                RETURN                                       
         EJECT ,                                                                
***********************************************************************         
* READ WKFILE INDEX AND BUILD TABLE OF REQUIRED ENTRIES               *         
***********************************************************************         
         SPACE 1                                                                
IND00    XC    WRKEY,WRKEY         PRE-SET USER INDEX FOR FILTERED              
*                                                                               
         USING UKRECD,RF                                                        
         LA    RF,WRKEY            SEARCH                                       
         MVC   UKUSRID,TWAUSRID                                                 
         MVI   UKSYSPRG,C'B'                                                    
         DROP  RF                                                               
*                                                                               
         USING TABLED,R6                                                        
         LA    R6,TABLE                                                         
         LA    R1,AIOAREA1                                                      
         SR    R2,R2               CLEAR COUNT                                  
         L     RF,AWRKSEQ                                                       
         L     R3,0(,R1)                                                        
         LA    R4,28(,R3)                                                       
*                                                                               
         USING WKRECD,R4                                                        
IND02    BASR  RE,RF               LOOP FOR AN ENTRY                            
         TM    DMCB+8,X'80'                                                     
         BO    IND04                                                            
         CLI   DMCB+8,0                                                         
         BNE   EXIT                                                             
         OC    WKRECS,WKRECS       NO RECORDS                                   
         BZ    IND02                                                            
         CLC   BILDATE,WKAGED      START DATE FILTER                            
         BH    IND02                                                            
         MVC   TABIND,12(R3)                                                    
         MVC   TABDATE,WKAGED                                                   
         LA    R6,TABLEN(,R6)                                                   
         LA    R2,1(,R2)                                                        
         LA    RE,MAXCNT                                                        
         CR    R2,RE                                                            
         BL    IND02                                                            
         DROP  R4                                                               
*                                                                               
IND04    LTR   R2,R2               SORT TABLE TO BILL NUMBER SEQUENCE           
         BZ    TEND                                                             
         LA    R2,1(,R2)                                                        
         MVI   TABIND,X'FF'                                                     
         LA    RF,TABLE                                                         
         LA    R3,TABLEN                                                        
         LA    R4,8                L'SORT KEY                                   
         SR    R5,R5                                                            
         GOTO1 VXSORT,DMCB,(0,(RF)),(R2),(R3),(R4),(R5)                         
         B     DSP00                                                            
         DROP  R6                                                               
         EJECT ,                                                                
***********************************************************************         
* DISPLAY FROM TABLE                                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING TABLED,R6                                                        
         SPACE 1                                                                
DSP00    LA    R6,TABLE            FIND START POINT FOR DISPLAY                 
*                                                                               
DSP01    CLI   TABIND,X'FF'                                                     
         BE    TEND                                                             
         CLC   SWRKEY(8),TABIND                                                 
         BNH   DSP02                                                            
         LA    R6,TABLEN(,R6)                                                   
         B     DSP01                                                            
*                                                                               
DSP02    LA    R3,LISFRSTH-86      R3/4/5 = BXLE REGS FOR LINE CONTROL          
         LA    R4,86                                                            
         LA    R5,LISFINLH                                                      
*                                                                               
         USING HEADERD,R7                                                       
DSP04    MVC   WRKSAVE(16),TABIND  RESTORE BUFFER AND GET HEADER                
         MVC   WRKSAVE+16(2),TABIND+14                                          
         MVC   WRKSAVE+18(2),=X'0100'                                           
         GOTO1 AWRKBUR,AIOAREA1                                                 
         BNE   EXIT                                                             
         MVI   WRKPARA,0                                                        
         GOTO1 AWRKLGET                                                         
         BNE   DSP15               CAN'T FIND FIRST RECORD - SKIP IT            
         L     R7,AWRKIO                                                        
         CLI   BILFLT,0            BILLED FILTER                                
         BE    DSP05                                                            
         CLC   HBILL,BILFLT                                                     
         BNE   DSP15                                                            
*                                                                               
DSP05    OC    LCLI,LCLI           CLIENT/PRODUCT/JOB FILTERS                   
         BZ    DSP06                                                            
         CLC   HCLI(3),LCLI                                                     
         BNE   DSP15                                                            
         OC    LPRO,LPRO                                                        
         BZ    DSP06                                                            
         CLC   HPRO(3),LPRO                                                     
         BNE   DSP15                                                            
         OC    LJOB,LJOB                                                        
         BZ    DSP06                                                            
         CLC   HJOB,LJOB                                                        
         BNE   DSP15                                                            
*                                                                               
DSP06    BXLE  R3,R4,DSP08         WE WANT IT - IS THERE ROOM ?                 
         OC    TWOUP,TWOUP                                                      
         BNZ   TFULL                                                            
         MVI   TWOUP+1,41          SET FOR 2-UP                                 
         LA    R3,LISFRSTH                                                      
*                                                                               
DSP08    OI    6(R3),X'80'                                                      
         LH    RA,TWOUP                                                         
         AR    RA,R3                                                            
*                                                                               
         USING LINED,RA                                                         
DSP10    BAS   RE,EDBILL           SET UP DISPLAY                               
         MVC   LNBILL,WORK                                                      
         MVC   LNCLI,HCLI                                                       
         LA    R1,LNCLI+5          CLI/PRO/JOB                                  
*                                                                               
DSP11    CLI   0(R1),C' '                                                       
         BH    DSP12                                                            
         BCT   R1,DSP11                                                         
*                                                                               
DSP12    MVI   1(R1),C'/'                                                       
         MVC   2(6,R1),HPRO                                                     
         LA    R1,7(,R1)                                                        
*                                                                               
DSP13    CLI   0(R1),C' '                                                       
         BH    DSP14                                                            
         BCT   R1,DSP13                                                         
*                                                                               
DSP14    MVI   1(R1),C'/'                                                       
         MVC   2(6,R1),HJOB                                                     
         GOTO1 VDATCON,DMCB,(1,TABDATE),(8,LNDATE)                              
         MVC   LNBILMK,HBILL                                                    
*                                                                               
DSP15    LA    R6,TABLEN(,R6)      BUMP TABLE                                   
         CLI   0(R6),X'FF'                                                      
         BNE   DSP04                                                            
         B     TEND                                                             
         DROP  R6,R7,RA                                                         
         EJECT ,                                                                
***********************************************************************         
* EXITS FROM OVERLAY                                                  *         
***********************************************************************         
         SPACE 1                                                                
TFULL    BAS   RE,EDBILL           SCREEN FULL AND MORE TO COME                 
         XC    BILNUM,BILNUM                                                    
         MVC   BILNUM(6),WORK                                                   
         OI    BILNUMH+6,FVOXMT       TRANSMIT                                  
         OI    BILACTH+6,FVOXMT+X'01' TRANSMIT + CHANGE TO MODIFIED FLD         
         LA    R1,BILTABH                                                       
         ST    R1,FADR                                                          
*                                  BILL NUMBERS DISPLAYED -                     
         MVC   FVMSGNO,=AL2(106)     HIT ENTER FOR NEXT                         
         MVI   FVMTYPE,FVMINFO                                                  
         B     OKEND                                                            
*                                                                               
TEND     XC    WRKSAVE,WRKSAVE     END                                          
         MVI   WRKSTAT,0                                                        
         LA    R1,BILACTH                                                       
         ST    R1,FADR                                                          
*                                  ACTION COMPLETED -                           
         MVC   FVMSGNO,=AL2(2101)    ENTER NEXT ACTION                          
         MVI   FVMTYPE,FVMINFO                                                  
*                                                                               
OKEND    MVI   FERN,OK                                                          
*                                                                               
EXIT     XIT1                                                                   
         EJECT ,                                                                
***********************************************************************         
* EDIT BILL NUMBER IN WRKEY TO 6 DIGITS IN WORK                       *         
***********************************************************************         
         SPACE 1                                                                
         USING UKRECD,RF                                                        
         SPACE 1                                                                
EDBILL   LA    RF,WRKEY                                                         
         MVC   WORK(3),UKSYSPRG+1                                               
         MVC   FULL(1),UKDAY                                                    
         MVI   FULL+1,X'0C'                                                     
         UNPK  WORK+3(3),FULL(2)                                                
         MVC   WORK+5(1),UKCLASS                                                
         BR    RE                                                               
         DROP  RF                                                               
         EJECT ,                                                                
***********************************************************************         
* TABLES                                                              *         
***********************************************************************         
         SPACE 1                                                                
DICTB05  DS    0X                  LOCAL DICTIONARY ITEMS                       
         DCDDL AC#BIL,L'LNBILL     BILL                                         
*                                  BILL                                         
         DCDDL AC#BIL,L'LNBILSTA,LABEL=AC@BIL2                                  
         DCDDL AC#CRTD,L'LNDATE-1  CREATED                                      
         DCDDL AC#DATE,L'LNDATE-1  DATE                                         
         DCDDL AC#CLIPK,L'LNCLIPJ  CLIENT/PRODUCT/JOB                           
         DCDDL AC#NUM,L'LNBILL     NUMBER                                       
         DCDDL AC#STT,L'LNBILSTA   STATUS                                       
*                                                                               
DICTB05X DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
* LOCAL W/S AT IOAREA2 TO END OF SAVE                                 *         
***********************************************************************         
         SPACE 1                                                                
LWSD     DSECT                                                                  
SAVRE    DS    F                   SAVE AREA FOR REGISTER RE                    
TWOUP    DS    H                   0=1ST-UP, 41=2ND-UP                          
*                                                                               
DICLS05  DS    0X                  LOCAL DICTIONARY ITEMS                       
AC$BIL   DS    CL(L'LNBILL)        BILL                                         
AC$4BIL  DS    CL(L'LNBILSTA)      BILL                                         
AC$CRTD  DS    CL(L'LNDATE-1)      CREATED                                      
AC$DATE  DS    CL(L'LNDATE-1)      DATE                                         
AC$CLIPK DS    CL(L'LNCLIPJ)       CLIENT/PRODUCT/JOB                           
AC$NUM   DS    CL(L'LNBILL)        NUMBER                                       
AC$STT   DS    CL(L'LNBILSTA)      STATUS                                       
*                                                                               
DICLS05X DS    AL1                 EOT                                          
*                                                                               
SWRKEY   DS    CL8                 SAVED WRKEY IF BILL NUMBER INPUT             
*                                                                               
TABLE    DS    CL(TABLEN*MAXCNT)   START OF TABLE COVERED BY TABLED             
*                                                                               
LWSX     DS    0C                                                               
         EJECT ,                                                                
***********************************************************************         
* DSECT TO COVER TABLE ENTRY FOR A SUBFILE                            *         
***********************************************************************         
         SPACE 1                                                                
TABLED   DSECT                                                                  
TABIND   DS    CL16                USER INDEX                                   
TABDATE  DS    CL3                 CREATION DATE IN PWOS                        
TABLEN   EQU   *-TABLED                                                         
*                                                                               
MAXCNT   EQU   400                 MAX NUM OF TABLE ENTRIES                     
         EJECT ,                                                                
***********************************************************************         
* DSECT TO COVER DISPLAY LINE                                         *         
***********************************************************************         
         SPACE 1                                                                
LINED    DSECT                                                                  
         DS    CL8                 FIELD HEADER                                 
*                                                                               
LNBILL   DS    CL6                 BILL NUMBER                                  
         DS    C                                                                
*                                                                               
LNCLIPJ  DS    0CL18               CLIENT/PRODUCT/JOB WITHOUT THE SPACE         
LNCLI    DS    CL6                 CLIENT CODE                                  
         DS    CL13                                                             
*                                                                               
LNDATE   DS    CL8                 CREATION DATE                                
*                                                                               
LNBILSTA DS    0CL4                BILL STATUS                                  
         DS    CL1                                                              
LNBILMK  DS    CL1                 BILLED MARKER (Y/N)                          
         DS    CL3                                                              
LINEDLQ1 EQU   *-LNBILL            LENGTH OF DATA AREA                          
         EJECT ,                                                                
         SPACE 1                                                                
* ACBILDSECT                                                                    
       ++INCLUDE ACBILDSECT                                                     
* ACBILWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACBILWORK                                                      
         PRINT ON                                                               
         EJECT ,                                                                
         SPACE 1                                                                
* ACBILFBD                                                                      
       ++INCLUDE ACBILFBD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACBIL05   12/17/12'                                      
         END                                                                    

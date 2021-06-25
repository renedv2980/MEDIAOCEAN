*          DATA SET ACBAT31    AT LEVEL 092 AS OF 07/01/02                      
*PHASE T61B31A                                                                  
*INCLUDE PERCALL                                                                
*INCLUDE ACGETRTE                                                               
         TITLE 'T61B31 - TIME SHEETS'                                           
T61B31   CSECT                                                                  
         PRINT NOGEN                                                            
***********************************************************************         
*        TYPE 49, X'31' CLIENT TIMESHEETS USES SCREEN X'CC'           *         
***********************************************************************         
         NMOD1 PROGDX-PROGD,*BAT31*,R8,R7,CLEAR=YES,RR=R2                       
         USING PROGD,RC                                                         
         L     R9,4(R1)                                                         
         USING WORKD,R9                                                         
         USING TWAD,RA                                                          
         ST    R2,RELO2                                                         
         EJECT                                                                  
***********************************************************************         
*              HANDLE BATCH GENERATE INITIALIZATION                   *         
***********************************************************************         
*                                                                               
BGEN     DS    0H                                                               
         CLI   CSOMODE,CSOMPLIN    ACTION --> BATCH/GENERATE                    
         BNE   BGENX                                                            
*                                                                               
         USING LIND,R4                                                          
         LA    R4,TIMDATAH                                                      
         MVI   LINE,1                                                           
BGEN100  CLC   LINE,CSOLINE        CLEAR LINE (# IN CSOLINE)                    
         BNE   *+8                                                              
         BAS   RE,PROTECT                                                       
         SR    R1,R1                                                            
         IC    R1,LINE                                                          
         LA    R1,1(R1)                                                         
         STC   R1,LINE                                                          
         LA    R4,LINLEN(R4)       NEXT LINE                                    
         LA    R2,TIMENDH                                                       
         CR    R4,R2               BOTTOM OF SCREEN YET?                        
         BL    BGEN100                                                          
         B     EXIT1               EXIT W/O SETTING CURSOR POSTN                
*                                                                               
BGENX    DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              HANDLE COMPANY STATUS SETTINGS                         *         
***********************************************************************         
*                                                                               
***********************************************************************         
*              HARD CODE GOES BELOW HERE                              *         
***********************************************************************         
         XC    PROFILES,PROFILES                                                
         CLI   CUABIN,X'AB'        HKNY WANTS PROFILE1 SET ALWAYS               
         BNE   *+8                                                              
         MVI   PROFILE1,C'Y'       Y=ONLY SHOW CLIENT NAME                      
         CLI   CUABIN,X'80'        MWTO HARDCODE                                
         BNE   *+8                                                              
         MVI   PROFILE2,C'Y'       Y=ALLOW TS 1 YEAR FORWARD                    
         CLI   CUABIN,X'84'        OPTO HARDCODE                                
         BNE   *+8                                                              
         MVI   PROFILE2,C'Y'       Y=ALLOW TS 1 YEAR FORWARD                    
         CLI   CUABIN,X'88'        HRNY HARDCODE                                
         BNE   *+8                                                              
         MVI   PROFILE2,C'Y'       Y=ALLOW TS 1 YEAR FORWARD                    
         CLI   CUABIN,X'AB'        HKNY HARDCODE                                
         BNE   *+8                                                              
         MVI   PROFILE2,C'Y'       Y=ALLOW TS 1 YEAR FORWARD                    
***********************************************************************         
*              HARD CODE GOES ABOVE HERE                              *         
***********************************************************************         
*                                                                               
MAIN     XC    STATUS,STATUS                                                    
         USING CPYELD,R6                                                        
         LA    R6,BCCPYEL                                                       
         MVI   SCRIPTEX,C'N'                                                    
         L     R1,BCAUTL                                                        
         TM    TSTAT6-UTLD(R1),TST6SCRP     RUNNING UNDER SCRIPT                
         BZ    *+8                          TESTING                             
         MVI   SCRIPTEX,C'Y'                                                    
*                                                                               
MAIN00   MVC   TSCPYST1,CPYSTAT1   COMPANY ELEMENT STATUS ELEMENTS              
         MVC   TSCPYST2,CPYSTAT2                                                
         MVC   TSCPYST3,CPYSTAT3                                                
         MVC   TSCPYST4,CPYSTAT4                                                
         CLI   CPYLN,CPYLN2Q                                                    
         BL    MAIN10                                                           
         MVC   TSCPYST5,CPYSTAT5                                                
         MVC   TSCPYST6,CPYSTAT6                                                
         MVC   TSCPYST7,CPYSTAT7                                                
         MVC   SVTSD,CPYTSD        FORCE TIMESHEET DAY                          
*                                                                               
MAIN10   TM    CPYSTAT3,CPYSPC1C+CPYSPCSJ          X'24'                        
         BZ    *+8                                                              
         BAS   RE,GET1C                                                         
         DROP  R6                                                               
*                                                                               
         GOTO1 =A(READ1R),BCPARM,(RC),(R9),RR=RELO2                             
         GOTO1 =A(READ1C),BCPARM,(RC),(R9),RR=RELO2                             
         GOTO1 VDATCON,BOPARM,(5,0),(1,TODAYR)                                  
         EJECT                                                                  
***********************************************************************         
*              HANDLE ITEM LIST/CHA/DEL OR ITEM INPUT                 *         
***********************************************************************         
*                                                                               
         DS    0H                                                               
         MVI   CSSPROG,0           SO CORRECT PF KEYS ARE DISPLAYED             
*                                                                               
         CLI   TWASCRN,X'ED'       TAX SCREEN ACTIVE?                           
         BE    CALLTAX             YES, CONTINUE TAXING                         
*                                                                               
         CLI   CSACT,ACTINP        ACTION --> ITEM/INPUT                        
         BNE   INIT30                                                           
         CLI   TWAMODE,2           INPUT TAX?                                   
         BE    CALLTAX             LET TAX OVERLAY HANDLE IT                    
         CLI   BCPFKEY,9           TAX REQUESTED                                
         BE    CALLTAX                                                          
         MVI   CSOLINE,0           INITIALIZE ITEM COUNT FOR ADDITE             
         B     INITX                                                            
*                                                                               
         USING LIND,R4                                                          
INIT30   LA    R4,TIMDATAH                                                      
         MVI   LINE,1                                                           
INIT100  CLC   LINE,CSOLINE        ACTION --> ITEM/CHANGE                       
         BNE   *+12                                                             
         BAS   RE,TRANSMIT         TRANSMIT ITEM TO BE CHANGED                  
         B     *+8                                                              
         BAS   RE,PROTECT          CLEAR & PROTECT ALL OTHER ITEMS              
         SR    R1,R1                                                            
         IC    R1,LINE                                                          
         LA    R1,1(R1)                                                         
         STC   R1,LINE                                                          
         LA    R4,LINLEN(R4)       NEXT LINE                                    
         LA    R2,TIMENDH                                                       
         CR    R4,R2               BOTTOM OF SCREEN YET?                        
         BL    INIT100                                                          
*                                                                               
INITX    DS    0H                                                               
         B     DEPTVAL                                                          
         EJECT                                                                  
***********************************************************************         
*              TRANSMIT A LINE DURING ITEM/CHANGE                     *         
***********************************************************************         
*                                                                               
*              R4 = A(START OF LINE)                                            
*                                                                               
         USING LIND,R4                                                          
TRANSMIT NTR1                                                                   
         LA    R0,LINNUMQ          #FIELDS/LINE                                 
*                                                                               
TRAN100  NI    4(R4),X'FF'-X'20'   MARK AS NOT PREVIOUSLY VALIDATED             
         OI    6(R4),X'80'         TRANSMIT                                     
         SR    R1,R1                                                            
         IC    R1,0(R4)            BUMP TO NEXT FIELD                           
         AR    R4,R1                                                            
         BCT   R0,TRAN100                                                       
*                                                                               
TRANSX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              CLEAR & PROTECT LINE                                   *         
***********************************************************************         
*                                                                               
*              R4 = A(START OF LINE)                                            
*                                                                               
         USING LIND,R4                                                          
PROTECT  NTR1                                                                   
         LA    R0,LINNUMQ          #FIELDS/LINE                                 
*                                                                               
PROT100  TWAXC 0(R4),0(R4),PROT=Y                                               
         CLI   CSOMODE,CSOMPLIN    BATCH GENERATE                               
         BE    *+8                                                              
         OI    1(R4),X'20'         PROTECT FIELD                                
         OI    4(R4),X'20'         MARK PREVIOUSLY VALIDATED                    
         MVI   5(R4),0             CLEAR INPUT LENGTH                           
         OI    6(R4),X'80'         TRANSMIT                                     
         SR    R1,R1                                                            
         IC    R1,0(R4)            BUMP TO NEXT FIELD                           
         AR    R4,R1                                                            
         BCT   R0,PROT100                                                       
*                                                                               
PROTX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              VALIDATE OFFICE/DEPARTMENT                             *         
***********************************************************************         
*                                                                               
DEPTVAL  DS    0H                                                               
         LA    R2,TIMDEPH          DEPARTMENT HEADER                            
         SR    R1,R1                                                            
         IC    R1,LVL1Q                                                         
         AH    R1,=H'1'                                                         
         STC   R1,FVMINL           MIN INPUT IS UPTO & INCLUDING LEV2           
         MVC   FVMAXL,LVL2Q        MAX = L'LEV1 + L'LEV2                        
         GOTO1 AFVAL,TIMDEPH                                                    
         BNE   EXIT                                                             
         MVC   BOWORK1,BCSPACES                                                 
*                                                                               
*              VALIDATE OFFICE                                                  
*                                                                               
         LA    R5,IOKEY                                                         
         USING ACTRECD,R5                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(2),=C'1R'                                                
         SR    R1,R1                                                            
         IC    R1,LVL1Q                                                         
         BCTR  R1,0                                                             
*        EX    R1,*+8                                                           
*        B     *+10                                                             
*        CLC   TIMDEP(0),NINES     DON'T ALLOW OVERHEAD ACCOUNTS                
*        BE    OVRHERR                                                          
         EX    R1,*+4                                                           
         MVC   ACTKACT(0),TIMDEP                                                
         GOTO1 AGETACT,0                                                        
         BNE   EXIT                                                             
*                                                                               
         TM    ACBSTAT,ACBSLOCK    ACCOUNT LOCKED?                              
         BZ    *+20                                                             
         MVC   FVXTRA(14),ACTKUNT                                               
         MVC   FVMSGNO,=AL2(AE$ACTLK)                                           
         B     EXIT2                                                            
*                                                                               
         OC    ACSTAT5,ACSTAT5                                                  
         BZ    DEPT01                                                           
         TM    ACSTAT5,RSTSPRJB    PROD/JOB/TASK REQUIRED                       
         BZ    *+8                                                              
         OI    SCRREQ,FLDPRO+FLDJOB+FLDTSK                                      
         TM    ACSTAT5,RSTSPROD                                                 
         BZ    *+8                                                              
         OI    SCRREQ,FLDPRO                                                    
*                                                                               
DEPT01   MVC   BOWORK1(L'ACNAME),ACNAME                                         
         MVI   BOWORK1+L'ACNAME,C'/'                                            
*                                                                               
         L     R5,AIO1                                                          
         AH    R5,=Y(ACTRFST-ACTRECD)                                           
DEPT02   CLI   0(R5),0                                                          
         BE    DEPT06                                                           
         CLI   0(R5),ACSPELQ       FIND SPECIAL ACCOUNT ELEMENT                 
         BE    DEPT04                                                           
         CLI   0(R5),ACSTELQ       AND STATUS ELEMENT                           
         BE    DEPT05                                                           
DEPT03   SR    R1,R1                                                            
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     DEPT02                                                           
*                                                                               
         USING ACSPECD,R5                                                       
DEPT04   CLI   ACSPTYP,ACSPOIN                                                  
         BNE   DEPT03                                                           
         MVC   SVSIACCT,ACSPACCT                                                
         B     DEPT03                                                           
*                                                                               
         USING ACSTATD,R5                                                       
DEPT05   MVC   POSN,ACSTCPOS                                                    
         MVC   CENTR,ACSTCNTR                                                   
         B     DEPT03                                                           
*                                                                               
DEPT06   TM    TSCPYST3,CPYSPC1C+CPYSPCSJ                                       
         BZ    DEPT06A                                                          
         TM    ACSTAT3,X'10'                                                    
         BZ    *+8                                                              
         OI    STATUS,PC1R                                                      
*                                                                               
*              VALIDATE OFFICE/DEPT                                             
*                                                                               
DEPT06A  SR    R1,R1                                                            
*        IC    R1,LVL1Q                                                         
*        LA    RF,TIMDEP                                                        
*        AR    RF,R1                                                            
*        IC    R1,LVL2Q                                                         
*        BCTR  R1,0                                                             
*        EX    R1,*+8                                                           
*        B     *+10                                                             
*        CLC   0(0,RF),NINES       OVERHEAD ACCOUNT USED?                       
*        BE    OVRHERR                                                          
*                                                                               
         LA    R5,IOKEY                                                         
         USING ACTRECD,R5                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(2),=C'1R'                                                
         SR    R1,R1                                                            
         IC    R1,FVXLEN                                                        
         EX    R1,*+4                                                           
         MVC   ACTKACT(0),TIMDEP                                                
         GOTO1 AGETACT,0                                                        
         BNE   EXIT                                                             
*                                                                               
         TM    ACBSTAT,ACBSLOCK    ACCOUNT LOCKED?                              
         BZ    *+20                                                             
         MVC   FVXTRA(14),ACTKUNT                                               
         MVC   FVMSGNO,=AL2(AE$ACTLK)                                           
         B     EXIT2                                                            
*                                                                               
         OC    ACSTAT5,ACSTAT5                                                  
         BZ    DEPT06B                                                          
         TM    ACSTAT5,RSTSPRJB    PROD/JOB/TASK REQUIRED                       
         BZ    *+8                                                              
         OI    SCRREQ,FLDPRO+FLDJOB+FLDTSK                                      
         TM    ACSTAT5,RSTSPROD                                                 
         BZ    *+8                                                              
         OI    SCRREQ,FLDPRO                                                    
*                                                                               
DEPT06B  MVC   BOWORK1+L'ACNAME+2(L'ACNAME),ACNAME                              
         GOTO1 VSQUASH,BOPARM,BOWORK1,L'BOWORK1                                 
         MVC   DEPGRP,ACCOST       COSTING BYTE                                 
         MVC   TIMDEPN,BOWORK1     DISPLAY OFF/DEPT NAME                        
         OI    TIMDEPNH+6,X'80'                                                 
*                                                                               
         L     R5,AIO1                                                          
         AH    R5,=Y(ACTRFST-ACTRECD)                                           
DEPT07   CLI   0(R5),0                                                          
         BE    DEPT14                                                           
         CLI   0(R5),ACSPELQ       FIND SPECIAL ACCOUNT ELEMENT                 
         BE    DEPT10                                                           
         CLI   0(R5),ACSTELQ       AND STATUS ELEMENT                           
         BE    DEPT12                                                           
DEPT08   SR    R1,R1                                                            
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     DEPT07                                                           
*                                                                               
         USING ACSPECD,R5                                                       
DEPT10   CLI   ACSPTYP,ACSPOIN                                                  
         BNE   DEPT08                                                           
         MVC   SVSIACCT,ACSPACCT                                                
         B     DEPT08                                                           
*                                                                               
         USING ACSTATD,R5                                                       
DEPT12   MVC   POSN,ACSTCPOS                                                    
         MVC   CENTR,ACSTCNTR                                                   
         B     DEPT08              GET NEXT EL                                  
*                                                                               
DEPT14   TM    TSCPYST3,CPYSPC1C+CPYSPCSJ COMPANY ON PROJECT CONTROL            
         BZ    DEPTX                                                            
         TM    ACSTAT3,X'10'                                                    
         BZ    *+8                                                              
         OI    STATUS,PC1R                                                      
*                                                                               
DEPTX    DS    0H                                                               
         TM    TIMDEPH+4,X'20'                                                  
         BO    *+12                                                             
         OI    STATUS,RRSCRN       REREAD THE RATES/INCOME ACCT                 
         OI    STATUS2,RRDUP       CLEAR DUPLICATE FIELD                        
         OI    TIMDEPH+4,X'20'                                                  
         B     SUBDVAL                                                          
*                                                                               
OVRHERR  MVC   FVMSGNO,=AL2(AE$OVRHD)                                           
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              VALIDATE SUB-DEPARTMENT                                *         
***********************************************************************         
*                                                                               
SUBDVAL  LA    R2,TIMSDPH          SUB-DEPARTMENT HEADER                        
         MVI   FVMINL,1            INPUT IS REQUIRED                            
         SR    R1,R1                                                            
         IC    R1,LVL3Q                                                         
         SR    R0,R0                                                            
         IC    R0,LVL2Q                                                         
         SR    R1,R0                                                            
         STC   R1,FVMAXL                                                        
         GOTO1 AFVAL,TIMSDPH                                                    
         BNE   EXIT                                                             
*                                                                               
         LA    R5,IOKEY            VALIDATE OFFICE                              
         USING ACTRECD,R5                                                       
         LA    R3,ACTKACT                                                       
         SR    R1,R1                                                            
         IC    R1,LVL2Q                                                         
         AR    R3,R1                                                            
         IC    R1,FVXLEN                                                        
         EX    R1,*+4                                                           
         MVC   0(0,R3),TIMSDP                                                   
         GOTO1 AGETACT,0                                                        
         BNE   EXIT                                                             
*                                                                               
         TM    ACBSTAT,ACBSLOCK    ACCOUNT LOCKED?                              
         BZ    *+20                                                             
         MVC   FVXTRA(14),ACTKUNT                                               
         MVC   FVMSGNO,=AL2(AE$ACTLK)                                           
         B     EXIT2                                                            
*                                                                               
         OC    ACSTAT5,ACSTAT5                                                  
         BZ    SUBD01                                                           
         TM    ACSTAT5,RSTSPRJB    PROD/JOB/TASK REQUIRED                       
         BZ    *+8                                                              
         OI    SCRREQ,FLDPRO+FLDJOB+FLDTSK                                      
         TM    ACSTAT5,RSTSPROD                                                 
         BZ    *+8                                                              
         OI    SCRREQ,FLDPRO                                                    
*                                                                               
SUBD01   CLI   ACCOST,C' '                                                      
         BNH   *+10                                                             
         MVC   DEPGRP,ACCOST       COSTING BYTE                                 
         MVC   TIMSDPN,ACNAME                                                   
         OI    TIMSDPNH+6,X'80'                                                 
*                                                                               
         L     R5,AIO1                                                          
         AH    R5,=Y(ACTRFST-ACTRECD)                                           
SUBD02   CLI   0(R5),0                                                          
         BE    SUBD08                                                           
         CLI   0(R5),ACSPELQ       FIND SPECIAL ACCOUNT ELEMENT                 
         BE    SUBD06                                                           
SUBD04   SR    R1,R1                                                            
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     SUBD02                                                           
*                                                                               
         USING ACSPECD,R5                                                       
SUBD06   CLI   ACSPTYP,ACSPOIN     IS THIS AN INCOME ACCOUNT?                   
         BNE   SUBD04                                                           
         MVC   SVSIACCT,ACSPACCT                                                
         B     SUBD04                                                           
*                                                                               
SUBD08   TM    TSCPYST3,CPYSPC1C+CPYSPCSJ COMPANY ON PROJECT CONTROL ?          
         BZ    SUBDX                                                            
         TM    ACSTAT3,X'10'                                                    
         BZ    *+8                                                              
         OI    STATUS,PC1R                                                      
*                                                                               
SUBDX    DS    0H                                                               
         TM    TIMSDPH+4,X'20'                                                  
         BO    *+12                                                             
         OI    STATUS,RRSCRN                                                    
         OI    STATUS2,RRDUP       CLEAR DUPLICATE FIELD                        
         OI    TIMSDPH+4,X'20'                                                  
         EJECT                                                                  
***********************************************************************         
*              VALIDATE STAFF                                         *         
***********************************************************************         
*                                                                               
STAFVAL  LA    R2,TIMSTFH                                                       
         MVI   FVMINL,1            INPUT IS REQUIRED                            
         ZIC   R1,LVL4Q                                                         
         ZIC   R0,LVL3Q                                                         
         SR    R1,R0                                                            
         STC   R1,FVMAXL                                                        
         GOTO1 AFVAL,TIMSTFH                                                    
         BNE   EXIT                                                             
*                                                                               
*        CLC   TIMSTF(3),NINES     OVERHEAD ACCT NOT ALLOWED                    
*        BE    OVRHERR                                                          
*                                                                               
         LA    R5,IOKEY            VALIDATE OFFICE                              
         USING ACTRECD,R5                                                       
         LA    R3,ACTKACT                                                       
         ZIC   R1,LVL3Q                                                         
         AR    R3,R1                                                            
         IC    R1,FVXLEN                                                        
         EX    R1,*+4                                                           
         MVC   0(0,R3),TIMSTF                                                   
         GOTO1 AGETACT,0                                                        
         BNE   EXIT                                                             
*                                                                               
         TM    ACBSTAT,ACBSLOCK    ACCOUNT LOCKED?                              
         BZ    *+20                                                             
         MVC   FVXTRA(14),ACTKUNT                                               
         MVC   FVMSGNO,=AL2(AE$ACTLK)                                           
         B     EXIT2                                                            
         TM    ACBSTAT,ACBSABAL    VALID FOR POSTING?                           
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INACP)                                           
         B     EXIT                                                             
*                                                                               
         OC    ACSTAT5,ACSTAT5                                                  
         BZ    STAF25                                                           
         TM    ACSTAT5,RSTSPRJB    PROD/JOB/TASK REQUIRED                       
         BZ    *+8                                                              
         OI    SCRREQ,FLDPRO+FLDJOB+FLDTSK                                      
         TM    ACSTAT5,RSTSPROD                                                 
         BZ    *+8                                                              
         OI    SCRREQ,FLDPRO                                                    
*                                                                               
STAF25   CLI   ACCOST,C' '                                                      
         BNH   *+10                                                             
         MVC   DEPGRP,ACCOST       COSTING BYTE                                 
         MVC   SV1RACT,ACCODE      SAVE AND TRANSMIT NAME                       
         MVC   SV1RNAME,ACNAME                                                  
         MVC   TIMSTFN,ACNAME                                                   
         OI    TIMSTFNH+6,X'80'                                                 
*                                                                               
         MVC   SV1ROFFC,BCSPACES   ISOLATE 1R OFFICE CODE                       
         TM    TSCPYST4,CPYSOFF2                                                
         BO    *+14                                                             
         MVC   SV1ROFFC(1),SV1RACT+3  OFFICE IS 1ST BYTE OF ACCT                
         B     STAF50                                                           
         NI    SV1ROFFP,X'0F'      ISOLATE OFFICE POSITION                      
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$BAD1R)                                           
         B     EXIT                                                             
         LA    R1,SV1RACT                                                       
         ZIC   RE,SV1ROFFP                                                      
         LA    R1,2(RE,R1)                                                      
         MVC   SV1ROFFC,0(R1)      SAVE 2 BYTE OFFICE CODE                      
*                                                                               
STAF50   XC    SVHIRE,SVHIRE                                                    
         XC    SVTERM,SVTERM                                                    
*                                                                               
         L     R5,AIO1                                                          
         AH    R5,=Y(ACTRFST-ACTRECD)                                           
STAF100  CLI   0(R5),0                                                          
         BE    STAF800                                                          
         CLI   0(R5),ACSTELQ       FIND STATUS ELEMENT                          
         BE    STAF500                                                          
         CLI   0(R5),ACSPELQ       FIND SPECIAL ACCOUNT ELEMENT                 
         BE    STAF600                                                          
         CLI   0(R5),X'56'         EMPLOYEE ELEMENT                             
         BE    STAF700                                                          
STAF150  ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     STAF100                                                          
*                                                                               
         USING ACSTATD,R5                                                       
STAF500  CLI   ACSTLEN,ACSTLNQ2    TEST ELEMENT IS LONG ENOUGH                  
         BL    *+10                NO-CANNOT HAVE A TIME SHEET NUMBER           
         MVC   TSNUM,ACSTSNUM      EXTRACT TIME SHEET NUMBER                    
         B     STAF150                                                          
*                                                                               
         USING ACSPECD,R5                                                       
STAF600  CLI   ACSPTYP,ACSPOIN     IS THIS AN INCOME ACCOUNT?                   
         BNE   STAF150             NO                                           
         MVC   SVSIACCT,ACSPACCT   YES, SAVE IT                                 
         B     STAF150                                                          
*                                                                               
STAF700  DS    0H                                                               
         USING ACEMPD,R5                                                        
         MVC   SVHIRE,ACEMPHIR     SAVE HIRE DATE                               
         MVC   SVTERM,ACEMPTRM     SAVE TERMINATION DATE                        
         B     STAF150                                                          
*                                                                               
STAF800  TM    TSCPYST3,CPYSPC1C+CPYSPCSJ                                       
         BZ    STAFX                                                            
         TM    ACSTAT3,X'10'                                                    
         BZ    *+8                                                              
         OI    STATUS,PC1R                                                      
*                                                                               
STAFX    DS    0H                                                               
         TM    TIMSTFH+4,X'20'                                                  
         BO    *+12                                                             
         OI    STATUS,RRSCRN                                                    
         OI    STATUS2,RRDUP       CLEAR DUPLICATE FIELD                        
         OI    TIMSTFH+4,X'20'                                                  
         EJECT                                                                  
***********************************************************************         
*              VALIDATE DEPARTMENT GROUP                              *         
***********************************************************************         
*                                                                               
GRPVAL   DS    0H                                                               
         LA    R5,IOKEY                                                         
         USING ACTRECD,R5                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(2),=C'14'   VALIDATE DEPT GROUP                          
         MVC   ACTKACT(1),DEPGRP                                                
         GOTO1 AGETACT,0                                                        
         BNE   EXIT                                                             
         CLI   DEPGRP,C' '                                                      
         BH    GRPX                                                             
         MVC   FVMSGNO,=AL2(FVFSET)                                             
         MVC   BASMSG,=CL60'EA#0017 Invalid account 14'                         
         B     EXIT                                                             
*                                                                               
GRPX     DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              VALIDATE DATE                                          *         
***********************************************************************         
*                                                                               
DATEVAL  DS    0H                                                               
         LA    R2,TIMDATH                                                       
         MVI   FVMINL,1            DATE IS REQUIRED                             
         GOTO1 AFVAL,TIMDATH                                                    
         BNE   EXIT                                                             
*                                                                               
         USING PERVALD,R6                                                       
         LA    R6,BOWORK1                                                       
         GOTO1 VPERVAL,BOPARM,(TIMDATH+5,TIMDAT),(X'60',BOWORK1)                
         CLI   4(R1),X'04'                                                      
         BNE   DATE125                                                          
         GOTO1 VDATCON,BOPARM,(1,PVALPSTA),(5,TEMP)                             
         MVC   TIMDAT(8),TEMP                                                   
         MVI   TIMDATH+5,8                                                      
         OI    TIMDATH+6,X'80'                                                  
*                                                                               
DATE100  GOTO1 VDATVAL,BOPARM,(0,8(R2)),BOWORK1                                 
         OC    BOPARM(4),BOPARM                                                 
         BNZ   *+14                                                             
DATE125  MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXIT                                                             
         CLC   BOWORK1(2),=C'85'                                                
         BL    DATEERR2                                                         
*                                                                               
         MVC   BOWORK2,BCSPACES                                                 
         GOTO1 VDATCON,BOPARM,(0,BOWORK1),(2,BOWORK2)  COMPRESS DATE            
         USING CPYELD,R5                                                        
         LA    R5,BCCPYEL                                                       
         MVC   FVMSGNO,=AL2(AE$USTMS)                                           
         LA    R2,TIMDEPH          POSITION THE CURSOR                          
         TM    CPYSTAT7,CPYSTMSY   SET UP FOR TMS?                              
         BZ    DATE200             NO - CONTINUE DATE CHECK                     
         CLI   CPYLN,CPYLN3Q       DOES EL HAVE THE TMS START DATE              
         BL    EXIT                                                             
         OC    CPYTMSSD,CPYTMSSD   TMS START DATE?                              
         BZ    EXIT                NO - THEY MUST USE =COST                     
         LA    R2,TIMDATH          POSITION THE CURSOR                          
         CLC   CPYTMSSD,BOWORK2    TMS START MUST BE HIGH TO USE TY49           
         BNH   EXIT                                                             
         DROP  R5                                                               
*                                                                               
DATE200  OC    SVTSD,SVTSD                                                      
         BZ    DATE250                                                          
         GOTO1 VGETDAY,BOPARM,(0,BOWORK1),BOWORK2                               
         CLC   SVTSD,BOPARM                                                     
         BE    DATE250                                                          
         MVC   FVMSGNO,=AL2(AE$IDAY)                                            
         B     EXIT                                                             
*                                                                               
DATE250  GOTO1 VDATCON,BOPARM,(0,BOWORK1),(1,TODAY3)                            
         CLI   PROFILE2,C'Y'       ALLOW TS FORWARD 1 YEAR                      
         BNE   DATE260                                                          
         GOTO1 VDATCON,BOPARM,(1,TODAYR),(0,TEMP)                               
         GOTO1 VADDAY,BOPARM,(C'Y',TEMP),TEMP+6,1                               
         GOTO1 VDATCON,BOPARM,(0,TEMP+6),(1,TEMP)                               
         CLC   TODAY3,TEMP                                                      
         BNH   DATE275                                                          
         MVC   FVMSGNO,=AL2(AE$1YEAR)                                           
         B     EXIT                                                             
*                                                                               
DATE260  CLC   TODAY3,TODAYR                                                    
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$TODAY)                                           
         B     EXIT                                                             
*                                                                               
DATE275  GOTO1 =A(PERCINTF),BCPARM,(RC),(R9),RR=RELO2                           
         BNE   DATEERR1                                                         
         CLC   TODAY3,SVHIRE                                                    
         BL    DATEERR2                                                         
         OC    SVTERM,SVTERM                                                    
         BZ    DATE300                                                          
         CLC   TODAY3,SVTERM                                                    
         BNH   DATE300                                                          
*                                                                               
DATEERR1 DS    0H                                                               
         MVC   FVMSGNO,=AL2(AE$OLCTN)                                           
         B     EXIT                                                             
*                                                                               
DATEERR2 DS    0H                                                               
         MVC   FVMSGNO,=AL2(AE$OHIRE)                                           
         B     EXIT                                                             
*                                                                               
DATE300  DS    0H                                                               
         TM    TIMDATH+4,X'20'                                                  
         BO    *+12                                                             
         OI    STATUS,RRSCRN                                                    
         OI    STATUS2,RRDUP       CLEAR DUPLICATE FIELD                        
         OI    TIMDATH+4,X'20'                                                  
         EJECT                                                                  
***********************************************************************         
*              VALIDATE REFERENCE NUMBER                              *         
***********************************************************************         
*                                                                               
REFVAL   LA    R2,TIMREFH                                                       
         CLI   5(R2),0             TEST FOR INPUT                               
         BE    REFX                                                             
*                                                                               
         CLI   5(R2),6             MUST INPUT ALL SIX DIGITS                    
         BNE   REFERR                                                           
         MVC   REFNUM,TIMREF                                                    
         OC    REFNUM,BCSPACES                                                  
         CLI   REFNUM,C'T'         MUST START WITH 'T'                          
         BNE   REFERR                                                           
         LA    R0,4                FOLLOWED BY 4 NUMBERS                        
         LA    R1,REFNUM+1                                                      
*                                                                               
REF100   CLI   0(R1),C'0'                                                       
         BL    REFERR                                                           
         CLI   0(R1),C'9'                                                       
         BH    REFERR                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,REF100                                                        
         MVI   TIMADJ,C'A'         ALL CORRECTIONS ARE CONSIDERED               
         OI    TIMADJH+6,X'80'     ADJUSTMENTS                                  
         B     REFX                                                             
*                                                                               
REFERR   DS    0H                                                               
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXIT                                                             
*                                                                               
REFX     DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              GET TYPE OF TIME                                       *         
***********************************************************************         
*                                                                               
MISS     DS    0H                                                               
         MVI   TYPETIME,TRSSTIME   REGULAR TIME - DEFAULT - X'01'               
         LA    R2,TIMADJH          ANYTHING INPUT IN MISS/ADJ FIELD?            
         CLI   TIMADJH+5,0                                                      
         BE    MISSX                                                            
         CLI   TIMADJ,C'T'                                                      
         BE    MISSX                                                            
         CLI   TIMADJ,C'A'         ADJUSTMENT TYPE = X'08'                      
         BNE   *+12                                                             
         MVI   TYPETIME,TRSSTADJ   ADJUSTED TIME                                
         B     MISSX                                                            
         CLI   TIMADJ,C'M'         MISSING TYPE = X'04'                         
         BNE   *+12                                                             
         MVI   TYPETIME,TRSSTMSS   MISSING TIME                                 
         B     MISSX                                                            
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXIT                                                             
*                                                                               
MISSX    DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              CHECK FOR DUPLICATE ITEM ON FILE                       *         
***********************************************************************         
*                                                                               
DUPVAL   DS    0H                                                               
         TM    STATUS2,RRDUP                                                    
         BZ    DUP50                                                            
         TM    TIMDUPH+4,X'20'                                                  
         BZ    DUP50                                                            
         TWAXC TIMDUPH,TIMDUPH                                                  
         OI    TIMDUPH+6,X'80'                                                  
*                                                                               
         USING UTLD,R1                                                          
DUP50    L     R1,BCAUTL           SKIP DUPLICATE CHECKING IF                   
         TM    TSTAT6,TST6SCRP     RUNNING UNDER A SCRIPT                       
         BO    DUPX                                                             
         DROP  R1                                                               
*                                                                               
         OC    REFNUM,REFNUM       IF REFERENCE NUMBER INPUT THEN               
         BNZ   DUPX                                                             
         LA    R2,TIMDUPH                                                       
         CLI   TIMDUP,C'+'         OVERRIDE DUPLICATE CHECK                     
         BE    DUPX                                                             
         CLI   TIMDUP,C' '                                                      
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXIT                                                             
*                                                                               
         MVC   IOKEY,BCSPACES                                                   
         LA    R5,IOKEY                                                         
         USING TRNRECD,R5                                                       
         MVC   TRNKCULA,SV1RACT                                                 
         GOTO1 AIO,IOHI+IOACCMST+IO1+IORDEL                                     
         B     DUP200                                                           
*                                                                               
DUP100   GOTO1 AIO,IOSQ+IOACCMST+IO1                                            
*                                                                               
DUP200   L     R5,AIO1                                                          
         CLC   SV1RACT,TRNKEY                                                   
         BNE   DUPX                                                             
         CLC   TRNKDATE,TODAY3     MATCH TRANSACTION DATE                       
         BNE   DUP100                                                           
*                                                                               
         LA    R5,TRNRFST                                                       
         USING TRNELD,R5                                                        
         CLI   TRNEL,TRNELQ        X'44' ELEMENT - LOOK FOR TRNSACTNS           
         BNE   DUP100                                                           
         CLI   TRNTYPE,BT27        ELIMINATE NON TIMESHEET BATCHES              
         BE    DUP250                                                           
         CLI   TRNTYPE,BT41                                                     
         BE    DUP250                                                           
         CLI   TRNTYPE,BT49                                                     
         BNE   DUP100                                                           
*                                                                               
DUP250   L     RF,AIO1                                                          
         TM    TRNRSTAT-TRNRECD(RF),TRNSDRFT                                    
         BZ    DUP300                                                           
         USING LSTTABD,RF                                                       
         LA    RF,BCBATCUR         SKIP DRAFT TRANS FROM CURRENT BATCH          
         CLC   LSTBREFN,TRNBTCH                                                 
         BE    DUP100                                                           
*                                                                               
DUP300   SR    RE,RE                                                            
         IC    RE,1(R5)                                                         
         AR    R5,RE                                                            
         CLI   0(R5),0                                                          
         BE    DUP400                                                           
         CLI   0(R5),X'60'         EXTRA STATUS ELEMENT                         
         BNE   *-18                                                             
*                                                                               
         USING TRSELD,R5                                                        
         TM    TYPETIME,TRSSTADJ   FOR ADJUSTED TIME DONT FLAG AS A             
         BZ    *+12                DUPLICATE UNLESS ALREADY ADJUSTED            
         TM    TRSSTAT2,TRSSTADJ                                                
         BZ    DUP100                                                           
*                                                                               
         TM    TYPETIME,TRSSTMSS+TRSSTIME     FOR MISSING/REGULAR TIME          
         BZ    *+12                           DONT FLAG AS A DUPLICATE          
         TM    TRSSTAT2,TRSSTMSS+TRSSTIME     UNLESS THERE IS ALREADY           
         BZ    DUP100                         MISSING/REGULAR TIME.             
*                                                                               
DUP400   MVC   FVMSGNO,=AL2(AE$DUPTS)                                           
         B     EXIT                                                             
*                                                                               
DUPX     DS    0H                                                               
         TM    TIMDUPH+4,X'20'                                                  
         BO    *+8                                                              
         OI    STATUS,RRSCRN                                                    
         OI    TIMDUPH+4,X'20'                                                  
         EJECT                                                                  
***********************************************************************         
*              VALIDATE INPUT LINE                                    *         
***********************************************************************         
*                                                                               
VALIDATE DS    0H                                                               
         LA    R6,TIMENDH          CLEAR OUT RATE/AMOUNT/INCOME ACC             
         LA    R4,TIMDATAH                                                      
         CLI   CSACT,ACTDSP        ARE WE DISPLAYING RECORD?                    
         BE    CPJ50               YES, DON'T CLEAR THEN                        
*                                                                               
         USING LIND,R4                                                          
         TM    STATUS,RRSCRN          REFRESH THE RATE AND INCOME AC            
         BZ    CPJ50                  NO, NOT NEEDED                            
*                                                                               
CLEAR    CLI   LINRAT,C'*'            YES, BUT NOT IF TYPED IN                  
         BNE   CLEAR20                                                          
         TWAXC LINRATH,LINRATH                                                  
         TWAXC LINAMTH,LINAMTH,PROT=Y                                           
*                                                                               
CLEAR20  CLI   LININC,C'*'                                                      
         BNE   CLEAR60                                                          
         TWAXC LININCH,LININCH                                                  
*                                                                               
CLEAR60  LA    R4,LINLEN(R4)                                                    
         CR    R4,R6               AT END?                                      
         BNH   CLEAR               NO                                           
*                                                                               
CPJ50    LA    R2,BASOLY2H         TRANSMIT WHOLE SCREEN WHEN DONE              
         SR    RF,RF                                                            
CPJ60    IC    RF,0(R2)                                                         
         AR    R2,RF                                                            
         CLI   0(R2),0             TEST FOR EOS                                 
         BNE   CPJ60                                                            
         MVC   1(2,R2),=X'0101'    SET INDICATOR TO XMIT WHOLE SCREEN           
*                                                                               
         LA    R6,SVTABL                                                        
         USING SVTABLD,R6          START AT BEGINNING OF TABLE                  
         LA    R4,TIMDATAH                                                      
         USING LIND,R4                                                          
         MVI   NEWINPUT,0          ASSUME THAT NOTHING WAS INPUT                
*                                                                               
*              INITALIZE FOR VALIDATION LOOP                                    
*                                                                               
CPJ100   DS    0H                                                               
         XC    FLDSTAT,FLDSTAT                CLEAR FIELD INDICATORS            
*                                                                               
         TM    STATUS,PC1R         WAS PC=Y ON ANY 1R ACCOUNTS                  
         BZ    *+8                 NO                                           
         OI    FLDREQ,FLDCLI+FLDPRO+FLDJOB+FLDTSK                               
*                                                                               
         OI    FLDREQ,FLDTIM+FLDHRS+FLDCLI    SET REQUIRED FIELDS               
         NI    STATUS,X'FF'-PCSJ              TURN OFF PRJ/CNTL                 
*                                                                               
         CLI   CSOMODE,CSOMDCHA    IF DOING AN ITEM/CHANGE THEN                 
         BE    CPJ200              DONT BUMP LINE NUMBER                        
         ZIC   R1,CSOLINE                                                       
         LA    R1,1(R1)                                                         
         STC   R1,CSOLINE                                                       
CPJ200   MVC   SVCSLINE,CSOLINE                                                 
         MVI   SVENTRY+SVLNQ,X'FF' MARK NEW END OF TABLE                        
*                                                                               
*              DETERMINE WHICH FIELDS HAVE INPUT                                
*                                                                               
         CLI   LINBNH+5,0          TYPE OF TIME                                 
         BE    *+8                                                              
         OI    FLDINPT,FLDTIM                                                   
         CLI   LINHRH+5,0          HOURS FIELD                                  
         BE    *+8                                                              
         OI    FLDINPT,FLDHRS                                                   
         CLI   LINCLIH+5,0         CLIENT FIELD                                 
         BE    *+8                                                              
         OI    FLDINPT,FLDCLI                                                   
         CLI   LINPROH+5,0         PRODUCT FIELD                                
         BE    *+8                                                              
         OI    FLDINPT,FLDPRO                                                   
         CLI   LINJOBH+5,0         JOB FIELD                                    
         BE    *+8                                                              
         OI    FLDINPT,FLDJOB                                                   
         CLI   LINTSKH+5,0         TASK FIELD                                   
         BE    *+8                                                              
         OI    FLDINPT,FLDTSK                                                   
         CLI   LINRATH+5,0         RATE FIELD                                   
         BE    *+8                                                              
         OI    FLDINPT,FLDRATE                                                  
         CLI   LININCH+5,0         INCOME ACCOUNT                               
         BE    *+8                                                              
         OI    FLDINPT,FLDINC                                                   
         OC    FLDINPT,FLDINPT     WAS ANYTHING INPUT ON THIS LINE?             
         BZ    EOL100                                                           
         MVI   NEWINPUT,X'FF'      INDICATE THAT SOMETHING WAS INPUT            
         EJECT                                                                  
***********************************************************************         
*              VALIDATE TIME OF TIME                                  *         
***********************************************************************         
*                                                                               
TIMEVAL  DS    0H                                                               
         LA    R2,LINBNH           BILLABLE OR NON                              
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,LINBNH                                                     
         BNE   EXIT                                                             
*                                                                               
         OC    REFNUM,REFNUM       IF REFERENCE NUMBER INPUT THEN               
         BZ    TIME100                                                          
         CLI   LINBN,C'B'          TIME MUST BE BILLABLE TIME (B)               
         BE    TIME100                                                          
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXIT                                                             
*                                                                               
TIME100  DS    0H                                                               
         CLI   LINBN,C'N'          N-TIME                                       
         BNE   TIME200                                                          
         OI    FLDINV,FLDRATE+FLDINC                                            
         B     TIMEX                                                            
*                                                                               
TIME200  CLI   LINBN,C'R'          SPECIAL TIME                                 
         BNE   TIME300                                                          
         OI    FLDINV,FLDINC                                                    
         B     TIMEX                                                            
*                                                                               
TIME300  CLI   LINBN,C'B'          BILLABLE TIME                                
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXIT                                                             
         OI    FLDREQ,FLDCLI+FLDPRO+FLDJOB+FLDTSK                               
*                                                                               
TIMEX    TM    LINBNH+4,X'20'      CLEAR RATE/INCOME IF TIME CHANGES            
         BO    *+8                                                              
         OI    STATUS,RRLINE                                                    
         OI    LINBNH+4,X'20'                                                   
*                                                                               
         MVC   SVTABN,LINBN                                                     
         EJECT                                                                  
***********************************************************************         
*              VALIDATE HOURS                                         *         
***********************************************************************         
*                                                                               
HRSVAL   DS    0H                                                               
         LA    R2,LINHRH                                                        
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,LINHRH        HOURS ARE REQUIRED                           
         BNE   EXIT                                                             
*                                                                               
         ZIC   R3,5(R2)                                                         
         GOTO1 AAMTVAL,BOPARM,8(R2),(R3)                                        
         CLI   0(R1),X'FF'                                                      
         BE    HRSERR              ERROR IN HOUR FIELD                          
         L     RF,BOPARM+4                                                      
         ZAP   BODUB1,0(8,RF)                                                   
         CP    BODUB1,=P'0'                                                     
         BE    HRSERR                                                           
         OC    BODUB1(L'BODUB1-L'SVHRS),BODUB1 PROTECT HOURS OVERFLOW           
         BNZ   HRSERR                                                           
*                                                                               
         ZAP   SVHRS,BODUB1                                                     
         DP    BODUB1,=P'25'       YES, MUST BE QUARTER HOURS                   
         CP    BODUB1+6(2),=P'0'                                                
         BE    HRSX                                                             
*                                                                               
HRSERR   DS    0H                  ERROR IN HOUR FIELD                          
         LA    R2,LINHRH                                                        
         MVC   FVMSGNO,=AL2(AE$INVAM)                                           
         MVC   FVXTRA,BCSPACES                                                  
         B     EXIT                                                             
*                                                                               
HRSX     DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              VALIDATE NON CLIENT 1N ACCT FROM CLIENT FIELD          *         
***********************************************************************         
*                                                                               
NCLIVAL  DS    0H                                                               
         XC    SV1NACT,SV1NACT                                                  
         LA    R2,LINCLIH                                                       
         MVI   FVMINL,1                                                         
         MVI   FVMAXL,L'LINCLI                                                  
         GOTO1 AFVAL,LINCLIH                                                    
         BNE   EXIT                                                             
*                                                                               
         CLI   SVTABN,C'N'         ALL 1N TIME IS N TIME                        
         BNE   NCLIX                                                            
         TM    FLDINPT,FLDPRO+FLDJOB+FLDTSK                                     
         BNZ   NCLIX                                                            
*                                                                               
         LA    R5,IOKEY                                                         
         USING ACTRECD,R5                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(2),=C'1N'                                                
         ZIC   R1,LINCLIH+5                                                     
         SH    R1,=H'1'                                                         
         EX    R1,*+4                                                           
         MVC   ACTKACT(0),LINCLI                                                
         GOTO1 AGETACT,0                                                        
         BE    NCLI50                                                           
         CLI   LINCLIH+5,L'SVSJCLI IF L'INPUT <=3 THEN MIGHT BE                 
         BH    EXIT                A CLIENT BUT IF > 3 IT'S A 1N ACCT           
         B     NCLIX                                                            
*                                                                               
NCLI50   MVC   SV1NACT,ACCODE                                                   
         TM    TSCPYST7,CPYSL1NA   IN ANALYSIS=X ON '1NVACATION'                
         BO    NCLI100             THEN MAKE ACCT'1NX VACATION'                 
         CLI   ACCOST,C' '         UNLESS ON LOOKUP ANALYSIS @ALLOCTIME         
         BE    NCLI100                                                          
         MVC   BOWORK1(12),SV1NACT+3                                            
         MVC   SV1NACT+3(12),BCSPACES                                           
         MVC   SV1NACT+3(1),ACCOST                                              
         MVC   SV1NACT+5(10),BOWORK1                                            
         B     NCLI200                                                          
*                                                                               
NCLI100  TM    ACBSTAT,ACBSLOCK    ACCOUNT LOCKED?                              
         BZ    *+20                                                             
         MVC   FVXTRA(14),ACTKUNT                                               
         MVC   FVMSGNO,=AL2(AE$ACTLK)                                           
         B     EXIT2                                                            
         TM    ACBSTAT,ACBSABAL    VALID FOR POSTING?                           
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INACP)                                           
         B     EXIT                                                             
*                                                                               
NCLI200  MVC   SV1NNAME,ACNAME                                                  
         MVC   LINNM,ACNAME                                                     
         OI    LINNMH+6,X'80'                                                   
         OI    STATUS,RRLINE       CLEAR OUT LINE                               
         OI    FLDINV,FLDPRO+FLDJOB+FLDTSK+FLDRATE+FLDINC                       
         NI    FLDREQ,X'FF'-FLDPRO-FLDJOB-FLDTSK-FLDRATE-FLDINC                 
         B     PROVAL                                                           
*                                                                               
NCLIX    DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              VALIDATE CLIENT CODE                                   *         
***********************************************************************         
*                                                                               
CLIVAL   DS    0H                                                               
         LA    R2,LINCLIH                                                       
         MVI   FVMINL,1                                                         
         MVI   FVMAXL,L'SVSJCLI                                                 
         GOTO1 AFVAL,LINCLIH                                                    
         BNE   EXIT                                                             
*                                                                               
         OC    FLDREQ,SCRREQ       SET REQUIRED FIELDS FROM 1R ACCT             
*                                  PROFILES                                     
         MVC   SVSJACT,BCSPACES                                                 
         MVC   SVSJCMP,CUABIN                                                   
         MVC   SVSJCUL,=C'SJ'                                                   
         ZIC   R1,LINCLIH+5                                                     
         SH    R1,=H'1'                                                         
         EX    R1,*+4                                                           
         MVC   SVSJCLI(0),LINCLI                                                
         MVI   BOFLAG1,ACIPRCLI    VALIDATE CLIENT                              
         XC    PSCLICOD,PSCLICOD   INSURE CLIENT REC IS REREAD                  
         GOTO1 AVALCPJ,LINCLIH                                                  
         BNE   EXIT                                                             
*                                                                               
         TM    ACBSTAT,ACBSLOCK    ACCOUNT LOCKED?                              
         BZ    *+20                                                             
         MVC   FVXTRA(14),ACTKUNT                                               
         MVC   FVMSGNO,=AL2(AE$ACTLK)                                           
         B     EXIT2                                                            
*                                                                               
         MVC   LINNM,ACNAME                                                     
         OI    LINNMH+6,X'80'                                                   
         TM    TSCPYST3,CPYSPC1C+CPYSPCSJ         MARKED FOR PROJ/CNTL          
         BZ    CLIX                                                             
         TM    ACSTAT3,X'10'                                                    
         BZ    CLIX                                                             
         OI    STATUS,PCSJ                                                      
         OI    FLDREQ,FLDPRO+FLDJOB+FLDTSK                                      
*                                                                               
CLIX     DS    0H                                                               
         TM    LINCLIH+4,X'20'                                                  
         BO    *+8                                                              
         OI    STATUS,RRLINE                                                    
         OI    LINCLIH+4,X'20'                                                  
         EJECT                                                                  
***********************************************************************         
*              VALIDATE PRODUCT CODE                                  *         
***********************************************************************         
*                                                                               
PROVAL   DS    0H                                                               
         LA    R2,LINPROH                                                       
         TM    FLDREQ,FLDPRO       FIELD REQUIRED?                              
         BZ    *+8                                                              
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,LINPROH                                                    
         BH    EXIT                                                             
*                                                                               
         TM    FLDINV,FLDPRO       IF INPUT NOT ALLOWED                         
         BZ    PRO100                                                           
         TM    FLDINPT,FLDPRO      THEN FLAG ERROR IF SOMETHING INPUT           
         BZ    PROX                                                             
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXIT                                                             
*                                                                               
PRO100   ZIC   R1,LINPROH+5                                                     
         SH    R1,=H'1'                                                         
         BM    PROX                                                             
         EX    R1,*+4                                                           
         MVC   SVSJPRO(0),LINPRO                                                
         MVI   BOFLAG1,ACIPRPRO    VALIDATE PRODUCT                             
         XC    PSPROCOD,PSPROCOD   INSURE PRODREC IS REREAD                     
         GOTO1 AVALCPJ,LINPROH                                                  
         BNE   EXIT                                                             
*                                                                               
         TM    ACBSTAT,ACBSLOCK    ACCOUNT LOCKED?                              
         BZ    *+20                                                             
         MVC   FVXTRA(L'LINPRO),LINPRO                                          
         MVC   FVMSGNO,=AL2(AE$ACTLK)                                           
         B     EXIT2                                                            
*                                                                               
         MVC   SVSJCPN,ACNAME      SAVE SJ C/P NAME                             
         CLI   PROFILE1,C'Y'                                                    
         BE    *+14                                                             
         MVC   LINNM,ACNAME                                                     
         OI    LINNMH+6,X'80'                                                   
*                                                                               
         TM    TSCPYST3,CPYSPC1C+CPYSPCSJ          USES PROJECT CONTROL         
         BZ    PROX                                                             
         TM    ACSTAT3,X'10'       THEN C IF CLIENT MARKED FOR PRJ/CNTL         
         BZ    PROX                                                             
         OI    STATUS,PCSJ                                                      
         OI    FLDREQ,FLDJOB+FLDTSK                                             
*                                                                               
PROX     DS    0H                                                               
         TM    LINPROH+4,X'20'                                                  
         BO    *+8                                                              
         OI    STATUS,RRLINE                                                    
         OI    LINPROH+4,X'20'                                                  
         EJECT                                                                  
***********************************************************************         
*              VALIDATE JOB CODE                                      *         
***********************************************************************         
*                                                                               
JOBVAL   DS    0H                                                               
         LA    R2,LINJOBH                                                       
         TM    FLDREQ,FLDJOB       FIELD REQUIRED?                              
         BZ    *+8                                                              
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,LINJOBH                                                    
         BH    EXIT                                                             
*                                                                               
         TM    FLDINV,FLDJOB       IF INPUT NOT ALLOWED                         
         BZ    JOB100                                                           
         TM    FLDINPT,FLDJOB      THEN FLAG ERROR IF SOMETHING INPUT           
         BZ    JOBX                                                             
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXIT                                                             
*                                                                               
JOB100   TM    FLDINPT,FLDJOB                                                   
         BZ    JOBX                                                             
         TM    FLDINPT,FLDCLI+FLDPRO                                            
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXIT                                                             
*                                                                               
         ZIC   R1,LINJOBH+5                                                     
         SH    R1,=H'1'                                                         
         BM    JOBX                                                             
         EXMVC R1,SVSJJOB,LINJOB                                                
         MVI   BOFLAG1,ACIPRJOB                                                 
         XC    PSJOBCOD,PSJOBCOD                                                
         GOTO1 AVALCPJ,LINJOBH                                                  
         BNE   EXIT                                                             
*                                                                               
JOB150   TM    ACBSTAT,ACBSLOCK    ACCOUNT LOCKED?                              
         BZ    *+20                                                             
         MVC   FVXTRA(L'LINJOB),LINJOB                                          
         MVC   FVMSGNO,=AL2(AE$ACTLK)                                           
         B     EXIT2                                                            
         TM    ACBSTAT,ACBSCLSE    ACCOUNT CLOSED?                              
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$JBCLO)                                           
         B     EXIT                                                             
*                                                                               
         USING ACTRECD,R5                                                       
         L     R5,AIO1                                                          
         LA    R5,ACTRFST                                                       
         SR    RE,RE                                                            
JOB200   CLI   0(R5),0                                                          
         BE    JOB300                                                           
         CLI   0(R5),X'26'         EMPLOYEE ELEMENT                             
         BE    *+14                                                             
         IC    RE,1(R5)                                                         
         AR    R5,RE                                                            
         B     JOB200                                                           
*                                                                               
         USING JOBELD,R5                                                        
         TM    JOBSTA1,JOBSART     QUALIFY FOR RATE ADJUSTMENT ?                
         BZ    *+8                                                              
         OI    SVSTAT,SVEADJ       ELIGIBLE FOR ADJUSTMENT                      
         TM    JOBSTA1,JOBSXJOB                                                 
         BZ    JOB300                                                           
         OI    SVSTAT,SVXJOB       ALLOW POSTING TO XJOB                        
         CLI   SVTABN,C'B'         EXCEPT IF IT'S B TIME                        
         BNE   JOB300                                                           
         MVC   FVMSGNO,=AL2(AE$TXJOB)                                           
         B     EXIT                                                             
*                                                                               
JOB300   CLI   PROFILE1,C'Y'                                                    
         BE    *+14                                                             
         MVC   LINNM,ACNAME                                                     
         OI    LINNMH+6,X'80'                                                   
         MVC   SVSJCPJN,ACNAME     SAVE SJ C/P/J NAME                           
         OI    FLDREQ,FLDTSK       TASK REQUIRED IF JOB IS INPUT                
*                                                                               
JOBX     DS    0H                                                               
         TM    LINJOBH+4,X'20'                                                  
         BO    *+8                                                              
         OI    STATUS,RRLINE                                                    
         OI    LINJOBH+4,X'20'                                                  
         EJECT                                                                  
***********************************************************************         
*              VALIDATE 1C COST ACCOUNT                               *         
***********************************************************************         
*                                                                               
AC1CVAL  DS    0H                                                               
         OC    SV1NACT,SV1NACT                                                  
         BNZ   AC1CX                                                            
         CLC   PSCOMPPR+(PPRGAOFF-PPRELD)(L'PPRGAOFF),BCSPACES                  
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ANFAN)                                           
         B     EXIT                                                             
         MVC   SVOFFICE,PSCOMPPR+(PPRGAOFF-PPRELD)                              
*                                                                               
         LA    R5,IOKEY                                                         
         USING ACTRECD,R5                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKEY(L'PPRCOST),PSCOMPPR+(PPRCOST-PPRELD)                      
*                                                                               
         OC    CENTR,BCSPACES                                                   
         CLI   POSN,0              IS THERE AN OVERRIDE POSITION ?              
         BE    AC1C100                                                          
         LA    R3,ACTKEY+2                                                      
         ZIC   R1,POSN                                                          
         AR    R3,R1                                                            
         B     *+8                                                              
*                                                                               
AC1C100  LA    R3,ACTKEY+7         NO OVERRIDE POS'N DESIGNATED                 
         LA    R0,3                MAX 3 CHARS CAN BE OVERRIDDEN                
         LA    R1,CENTR                                                         
AC1C110  CLI   CENTR,C' '          CANNOT OVERRIDE WITH A SPACE                 
         BE    *+10                                                             
         MVC   0(1,R3),0(R1)       MOVE IN OVERRIDE CHARACTERS                  
         LA    R1,1(R1)                                                         
         LA    R3,1(R3)                                                         
         BCT   R0,AC1C110                                                       
*                                                                               
         GOTO1 AGETACT,0           VALIDATE 1C ACCT                             
         BNE   EXIT                                                             
         TM    ACBSTAT,ACBSLOCK    ACCOUNT LOCKED?                              
         BZ    *+20                                                             
         MVC   FVXTRA(14),ACTKUNT                                               
         MVC   FVMSGNO,=AL2(AE$ACTLK)                                           
         B     EXIT2                                                            
         TM    ACBSTAT,ACBSABAL    VALID FOR POSTING?                           
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INACP)                                           
         B     EXIT                                                             
         MVC   SV1CACT,ACCODE      VALIDATE 1C COST ACCOUNT                     
         MVC   SV1CNAME,ACNAME                                                  
*                                                                               
         LA    R5,4                # TIMES TO LOOP                              
         LA    R3,LVL4CST          1C HIERARCHY                                 
*                                                                               
AC1C200  SR    R1,R1                                                            
         ICM   R1,1,0(R3)                                                       
         BZ    AC1C400                                                          
         MVC   IOKEY,BCSPACES                                                   
         LA    R1,2(R1)                                                         
         EX    R1,*+4                                                           
         MVC   IOKEY(0),SV1CACT                                                 
         GOTO1 AGETACT,0                                                        
         BNE   AC1C400                                                          
*                                                                               
         OC    ACSTAT5,ACSTAT5     IF NO STATUS BYTE SET UP GO UP TO            
         BZ    AC1C400             NEXT HIGHTEST LEVEL OF ACCOUNT               
         TM    ACSTAT5,RSTSPROD+RSTSPRJB                                        
         BZ    AC1C300                                                          
         LA    R2,LINPROH                                                       
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,LINPROH       PRODUCT REQUIRED ON 1C ACCT PROFILE          
         BH    EXIT                                                             
*                                                                               
AC1C300  TM    ACSTAT5,RSTSPRJB                                                 
         BZ    AC1C400                                                          
         LA    R2,LINJOBH                                                       
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,LINJOBH       JOB REQUIRED ON 1C ACCT PROFILE              
         BH    EXIT                                                             
*                                                                               
         LA    R2,LINTSKH                                                       
         MVI   FVMINL,1                                                         
         TM    TSCPYST7,CPYSJTIM   IF OLD COSTING THEN TASK                     
         BO    AC1C325             NOT REQUIRED FOR R TIME                      
         CLI   LINBN,C'R'                                                       
         BNE   *+8                                                              
         MVI   FVMINL,0                                                         
AC1C325  GOTO1 AFVAL,LINTSKH                                                    
         BH    EXIT                                                             
         B     AC1CX                                                            
*                                                                               
AC1C400  BCTR  R3,0                DROP BACK A LEVEL IN HIERARCH TABLE          
         BCT   R5,AC1C200                                                       
*                                                                               
AC1CX    DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              VALIDATE TASK CODE                                     *         
***********************************************************************         
*                                                                               
TSKVAL   DS    0H                                                               
         LA    R2,LINTSKH                                                       
         TM    FLDINV,FLDTSK       IF INPUT NOT ALLOWED                         
         BZ    TSK50                                                            
         TM    FLDINPT,FLDTSK      THEN FLAG ERROR IF SOMETHING INPUT           
         BZ    TSKX                                                             
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXIT                                                             
*                                                                               
TSK50    DS    0H                                                               
         TM    TSCPYST7,CPYSJTIM   IF OLD COSTING THEN TASK                     
         BO    TSK75               NOT REQUIRED FOR R TIME                      
         TM    STATUS,PCSJ+PC1R    IF PROJECT CONTROL, HOWEVER, FORCE           
         BNZ   TSK75               TASK (FLDREQ SET ABOVE)                      
         CLI   SVTABN,C'R'                                                      
         BNE   *+8                                                              
         NI    FLDREQ,X'FF'-FLDTSK                                              
*                                                                               
TSK75    TM    FLDREQ,FLDTSK       IF INPUT REQUIRED                            
         BZ    *+8                                                              
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,LINTSKH                                                    
         BH    EXIT                                                             
*                                                                               
TSK100   TM    FLDINPT,FLDTSK                                                   
         BZ    TSKX                                                             
         TM    FLDINPT,FLDCLI      MUST HAVE AT LEAST A CLIENT                  
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXIT                                                             
*                                                                               
TSK125   TM    STATUS,PC1R+PCSJ                                                 
         BZ    TSK150                                                           
         LA    R5,IOKEY                                                         
         USING WCORECD,R5                                                       
         MVC   WCOKEY,BCSPACES                                                  
         MVI   WCOKTYP,WCOKTYPQ    X'0A' RECORD                                 
         MVC   WCOKCPY,CUABIN                                                   
         MVC   WCOKUNT(2),=C'1J'   ALWAYS CHECK 1J WORKCODE IF 1R OR SJ         
         ZIC   R1,LINTSKH+5        PC=Y IS SET                                  
         SH    R1,=H'1'                                                         
         BM    TSKX                                                             
         EXMVC R1,WCOKWRK,LINTSK                                                
         MVC   SVTSK,WCOKWRK                                                    
         GOTO1 AIO,IOREAD+IOACCMST+IO1                                          
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$1JTSK)                                           
         B     EXIT                                                             
*                                                                               
TSK150   TM    TSCPYST7,CPYSJTIM   IF OLD COSTING THEN TASK                     
         BO    *+20                                                             
         TM    STATUS,PC1R+PCSJ    DONT VALIDATE SJ WORKCODE IF                 
         BZ    *+12                USING PROJECT CONTROL AND N TIME             
         CLI   SVTABN,C'N'         UNLESS ON NEWCOST                            
         BE    TSKX                                                             
*                                                                               
         LA    R5,IOKEY                                                         
         USING WCORECD,R5                                                       
         MVC   WCOKEY,BCSPACES                                                  
         MVI   WCOKTYP,WCOKTYPQ    X'0A' RECORD                                 
         MVC   WCOKCPY,CUABIN                                                   
         MVC   WCOKUNT(2),=C'SJ'                                                
         ZIC   R1,LINTSKH+5                                                     
         SH    R1,=H'1'                                                         
         BM    TSKX                                                             
         EXMVC R1,WCOKWRK,LINTSK                                                
         MVC   SVTSK,WCOKWRK                                                    
         GOTO1 AIO,IOREAD+IOACCMST+IO1                                          
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ITASK)                                           
         B     EXIT                                                             
*                                                                               
         USING ACTRECD,R5                                                       
         L     R5,AIO1             SEE IF WORKCODE IS MARKED ELIGIBLE           
         LA    R5,ACTRFST          FOR ADJUSTMENT                               
         SR    RE,RE                                                            
*                                                                               
         USING WCOELD,R5                                                        
TSK200   CLI   0(R5),0                                                          
         BE    TSKX                                                             
         CLI   WCOEL,WCOELQ        X'12'                                        
         BE    *+14                                                             
         IC    RE,1(R5)                                                         
         AR    R5,RE                                                            
         B     TSK200                                                           
         TM    WCOSTAT2,WCOSART    ELIGIBLE FOR ADJUSTMENT RATE?                
         BZ    *+8                                                              
         OI    SVSTAT,SVEADJ       MARK ELIGIBLE FOR ADJUSTMENT                 
*                                                                               
TSKX     DS    0H                                                               
         TM    LINTSKH+4,X'20'                                                  
         BO    *+8                                                              
         OI    STATUS,RRLINE                                                    
         OI    LINTSKH+4,X'20'                                                  
*                                                                               
         EJECT                                                                  
***********************************************************************         
*              CALL OPTION MAINT, FLAG REQUIRED FIELDS, SAVE TAX OPTS *         
***********************************************************************         
*                                                                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         OC    SV1NACT,SV1NACT     IS THIS 1N CLIENT TIME                       
         BNZ   *+8                 YES, NO OPTIONS                              
         BAS   RE,OPTMNT                                                        
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    *+12                                                             
         L     R2,BCPARM           BCPARM=A(FIELD FOR ERROR MSG)                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              VALIDATE TAX FIELD                                     *         
***********************************************************************         
*                                                                               
VALTAX   DS    0H                                                               
         XC    FLAGT,FLAGT                                                      
         TM    STATUS,RRLINE+RRSCRN   CLEAR OUT TAX FIELD                       
         BZ    VALT15                                                           
         CLI   LINTAX,C'*'         DONT CLEAR TAX IF USER TYPED IT IN           
         BNE   VALT15                                                           
         TWAXC LINTAXH,LINTAXH                                                  
         B     VALTX20                                                          
*                                                                               
VALT15   LA    R2,LINTAXH                                                       
         CLI   5(R2),0             ANY I/P                                      
         BNE   VALTX50             YES, USE IT                                  
*                                                                               
VALTX20  CLI   SVTABN,C'B'         B TIME                                       
         BNE   VALTXX              NO, LEAVE FIELD BLAMK                        
*                                                                               
         OI    6(R2),X'80'         SET TAX FIELD FOR B TIME                     
         MVI   LINTAX,C'N'                                                      
         TM    SVSTAT,SVTAX        IS THIS ENTRY TAXABLE                        
         BZ    *+8                                                              
         MVI   LINTAX,C'Y'                                                      
         MVI   FLAGT,X'FF'         FLAG FIELD AS SET INTERNALLY                 
         B     VALTXX                                                           
*                                                                               
VALTX50  LA    RF,LINTAX                                                        
         CLI   0(RF),C'*'                                                       
         BNE   *+8                                                              
         LA    RF,1(RF)                                                         
*                                                                               
         CLI   0(RF),C'Y'                                                       
         BNE   VALTX60                                                          
         CLI   SVTABN,C'B'         MUST BE B TIME                               
         BNE   VALTX55                                                          
         OI    SVSTAT,SVTAX                                                     
         B     VALTXX                                                           
*                                                                               
VALTX55  MVC   FVMSGNO,=AL2(AE$TAXB)                                            
         B     EXIT                                                             
*                                                                               
VALTX60  CLI   0(RF),C'N'                                                       
         BNE   *+12                                                             
         NI    SVSTAT,X'FF'-SVTAX                                               
         B     VALTXX                                                           
         MVC   FVMSGNO,=AL2(AE$TXFLD)                                           
         B     EXIT                                                             
*                                                                               
VALTXX   DS    0H                                                               
         CLI   FLAGT,X'FF'         FIELD SET INTERNALY                          
         BNE   VALTXXX             NO                                           
         MVC   LINTAX+1(1),LINTAX                                               
         MVI   LINTAX,C'*'         DISPLAY AS SUCH                              
VALTXXX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              VALIDATE PRJ/CNTL ACCOUNT - MIMICS EITHER SJ OR 1C     *         
***********************************************************************         
*                                                                               
PRJVAL   DS    0H                                                               
         TM    TSCPYST3,CPYSPC1C+CPYSPCSJ         ON PRJ/CTL                    
         BZ    PRJX                                                             
         TM    FLDINPT,FLDJOB                                                   
         BZ    PRJX                                                             
         TM    STATUS,PC1R+PCSJ    WAS PC=Y ON ANY LEVEL OF 1R OR SJ            
         BZ    PRJX                                                             
         OC    SV1NACT,SV1NACT     NO PROJECT CONTROL IF 1N POSTING             
         BNZ   PRJX                                                             
*                                                                               
         LA    R5,IOKEY                                                         
         USING ACTRECD,R5                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(2),=C'1J'                                                
         MVC   ACTKACT(L'SVSJCPJ),SVSJCPJ                                       
         TM    BCCPYEL+(CPYSTAT3-CPYELD),X'04'      PC=SJ?                      
         BO    PRJ100                                                           
         LA    R3,SV1CACT+2                                                     
         ZIC   R1,CLIPOS                                                        
         AR    R3,R1                                                            
         MVC   ACTKACT(6),0(R3)                                                 
*                                                                               
PRJ100   GOTO1 AGETACT,0                                                        
         BNE   EXIT                                                             
         MVC   SVPCACT,ACCODE      SAVE PROJECT CONTROL ACCOUNT                 
*                                                                               
PRJX     DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              PREVENT TRANSFER OF -HRS > HRS CURRENTLY ON JOB        *         
***********************************************************************         
*                                                                               
PRVXFER  DS    0H                                                               
         ZAP   SCRNHRS,=P'0'                                                    
         LA    RF,SVTABL                                                        
PRV25    CLI   0(RF),X'FF'                                                      
         BE    PRV50                                                            
         CLC   SVSJCPJ,SVSJCPJ-SVTABLD(RF)                                      
         BNE   PRV30                                                            
         ZAP   BODUB1,SVHRS-SVTABLD(L'SVHRS,RF)                                 
         MP    BODUB1,=P'-1'                                                    
         AP    SCRNHRS,BODUB1                                                   
PRV30    LA    RF,SVLNQ(RF)                                                     
         B     PRV25                                                            
*                                                                               
PRV50    CLC   SVSJJOB,BCSPACES    JOB MUST HAVE BEEN INPUT                     
         BNH   PRVX                TO CHECK TRANSFER OF HOURS                   
         CP    SVHRS,=P'0'                                                      
         BNL   PRVX                MUST HAVE NEGATIVE HOURS                     
         ZAP   TOTHRS,=P'0'        -HRS ACCUMULATOR                             
*                                                                               
         LA    R5,IOKEY                                                         
         XC    IOKEY,IOKEY                                                      
         USING TRNRECD,R5                                                       
         MVC   TRNKEY,BCSPACES                                                  
         MVC   TRNKCULA,SV1RACT                                                 
         TM    TSCPYST4,CPYSOFF2                                                
         BNO   *+10                                                             
         MVC   TRNKOFF,SVOFFICE                                                 
         MVC   TRNKCULC,SV1CACT                                                 
         MVC   IOKEYSAV,IOKEY                                                   
         GOTO1 AIO,IOHI+IOACCMST+IO1                                            
         B     PRV200                                                           
*                                                                               
PRV100   GOTO1 AIO,IOSQ+IOACCMST+IO1                                            
*                                                                               
PRV200   L     R5,AIO1                                                          
         CLC   TRNKEY(TRNKDATE-TRNKEY),IOKEYSAV                                 
         BE    PRV225                                                           
         CP    SCRNHRS,TOTHRS      COMPARE TO TOTAL HRS ON JOB                  
         BNH   PRVX                                                             
         LA    R2,LINHRH                                                        
         MVC   FVMSGNO,=AL2(AE$NEGHR)                                           
         B     EXIT                                                             
*                                                                               
PRV225   LA    R5,TRNRFST                                                       
         USING TRNELD,R5                                                        
         CLI   TRNEL,TRNELQ        X'44' ELEMENT - LOOK FOR TRNSACTNS           
         BNE   PRV100                                                           
         CLI   TRNTYPE,BT27        ELIMINATE NON TIMESHEET BATCHES              
         BE    PRV250                                                           
         CLI   TRNTYPE,BT34        INCLUDE JOB TO JOB TRANSFERS                 
         BE    PRV250                                                           
         CLI   TRNTYPE,BT41                                                     
         BE    PRV250                                                           
         CLI   TRNTYPE,BT49                                                     
         BNE   PRV100                                                           
*                                                                               
PRV250   L     RF,AIO1                                                          
         TM    TRNRSTAT-TRNRECD(RF),TRNSDRFT                                    
         BZ    PRV300                                                           
         USING LSTTABD,RF                                                       
         LA    RF,BCBATCUR         SKIP DRAFT TRANS FROM OTHER BATCHES          
         CLC   LSTBREFN,TRNBTCH                                                 
         BNE   PRV100                                                           
*                                                                               
PRV300   SR    RE,RE                                                            
         IC    RE,1(R5)                                                         
         AR    R5,RE                                                            
         CLI   0(R5),0                                                          
         BE    PRV100                                                           
         CLI   0(R5),X'50'         HOURS ELEMENT                                
         BNE   *-18                                                             
*                                                                               
         USING TRCASHD,R5                                                       
         CLI   TRCSTYPE,C'H'                                                    
         BNE   PRV100                                                           
         ZAP   BODUB1,TRCSAMNT                                                  
*                                                                               
         IC    RE,1(R5)                                                         
         AR    R5,RE                                                            
         CLI   0(R5),0                                                          
         BE    PRV100                                                           
         CLI   0(R5),X'51'                                                      
         BNE   *-18                                                             
*                                                                               
         USING PCIELD,R5                                                        
         CLC   SVSJCPJ,PCICLI+3    SAME CLI/PRD/JOB                             
         BE    *+14                                                             
         CLC   SVSJCPJ,PCIPRJT+3   JOB IN THIS FIELD IF POSTING FROM            
         BNE   PRV100              OLD BATCH PROGRAM                            
         AP    TOTHRS,BODUB1       SAVE HOURS                                   
         B     PRV100                                                           
*                                                                               
PRVX     DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              VALIDATE RATE                                          *         
***********************************************************************         
*                                                                               
RATEVAL  DS    0H                                                               
         LA    R2,LINRATH                                                       
         TM    FLDINV,FLDRATE      IF INPUT NOT ALLOWED                         
         BZ    RATE5                                                            
         TM    FLDINPT,FLDRATE     THEN FLAG ERROR IF SOMETHING INPUT           
         BZ    RATE5                                                            
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXIT                                                             
*                                                                               
RATE5    TWAXC LINAMTH,LINAMTH,PROT=Y                                           
         TM    STATUS,RRLINE+RRSCRN   CLEAR OUT RATE                            
         BZ    RATE15                                                           
         MVC   LINADJ,BCSPACES     ADJUSTMENT INDICATOR                         
         OI    LINADJH+6,X'80'                                                  
         CLI   LINRAT,C'*'         DONT CLEAR RATE IF USER TYPED IT IN          
         BNE   RATE15                                                           
         TWAXC LINRATH,LINRATH                                                  
*                                                                               
RATE15   ZAP   SVRATE,=P'0'        INITIALIZE FOR NO RATE                       
         ZAP   SVTOTAL,=P'0'                                                    
         XC    FLAGT,FLAGT                                                      
         CLI   SVTABN,C'N'         NO RATES FOR N TIME                          
         BE    RATEX                                                            
*                                                                               
         CLC   LINRAT,BCSPACES     WAS A RATE ENTERED ON SCREEN                 
         BNH   RATE100                                                          
*                                                                               
         MVI   FVMINL,0                                                         
         MVI   FVMAXL,L'LINRAT                                                  
         GOTO1 AFVAL,LINRATH                                                    
         LA    R5,FVIFLD                                                        
         ZIC   R3,FVILEN                                                        
*                                                                               
         CLI   LINRAT,C'*'         ADJUST FOR C'*' IN RATE FIELD                
         BNE   RATE25                                                           
         LA    R5,LINRAT+1                                                      
         BCTR  R3,0                                                             
         MVI   FLAGT,X'FF'         INDICATE RATE FROM RATE RECORD               
*                                                                               
RATE25   GOTO1 AAMTVAL,BOPARM,(R5),(R3)                                         
         CLI   0(R1),X'FF'         ERROR IN RATE FIELD                          
         BE    RATE50              YES                                          
*                                                                               
         L     RF,BOPARM+4                                                      
         CP    0(8,RF),=P'999999'  MAX RATE 9999.99.                            
         BH    RATE50                                                           
*                                                                               
         ZAP   SVRATE,4(4,RF)                                                   
         BM    RATE50              NEGATIVE INVALID                             
         B     RATE200                                                          
*                                                                               
RATE50   LA    R2,LINRATH                                                       
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXIT                                                             
*                                                                               
         USING RATED,R3                                                         
RATE100  LA    R3,BLOCK                                                         
         XC    BLOCK(RATEDQ),BLOCK                                              
         LA    R1,SV1RACT+3        SAVE OFFICE                                  
         ZIC   RE,LVL1Q                                                         
         SH    RE,=H'1'                                                         
         EXMVC RE,RAT1ROFF,0(R1)                                                
*                                                                               
         LA    R1,SV1RACT+3        SAVE DEPARTMENT                              
         ZIC   R0,LVL1Q                                                         
         AR    R1,R0                                                            
         ZIC   RE,LVL2Q                                                         
         SR    RE,R0                                                            
         SH    RE,=H'1'                                                         
         EXMVC RE,RAT1RDPT,0(R1)                                                
*                                                                               
         LA    R1,SV1RACT+3        SAVE SUBDEPT                                 
         ZIC   R0,LVL2Q                                                         
         AR    R1,R0                                                            
         ZIC   RE,LVL3Q                                                         
         ZIC   R0,LVL2Q                                                         
         SR    RE,R0                                                            
         SH    RE,=H'1'                                                         
         EXMVC RE,RAT1RSUB,0(R1)                                                
*                                                                               
         LA    R1,SV1RACT+3        SAVE PERSON                                  
         ZIC   R0,LVL3Q                                                         
         AR    R1,R0                                                            
         ZIC   RE,LVL4Q                                                         
         ZIC   R0,LVL3Q                                                         
         SR    RE,R0                                                            
         SH    RE,=H'1'                                                         
         EXMVC RE,RAT1RPER,0(R1)                                                
*                                                                               
         MVC   RATSJTSK,SVTSK      SAVE TASK CODE                               
*                                                                               
         MVC   RATSJOFF,SVOFFICE   SAVE CLIENT OFFICE CODE                      
*                                                                               
         MVC   RATSJCLI,SVSJCLI    SAVE CLIENT                                  
*                                                                               
         MVC   RATSJPRD,SVSJPRO    SAVE PRODUCT CODE                            
*                                                                               
         MVC   RATSJJOB,SVSJJOB    SAVE JOB CODE                                
*                                                                               
         MVC   RATCRDTE,TODAY3     SAVE DATE                                    
*                                                                               
         TM    SVSTAT,SVEADJ       ELIGIBLE FOR ADJUSTMENT                      
         BNO   *+8                                                              
         OI    RATSTAT,RATEADJ                                                  
*                                                                               
         MVC   RAT1CMPY,CUABIN                                                  
         MVC   RDATAMGR,VDMGR                                                   
*                                                                               
         GOTO1 =V(ACGETRTE),BLOCK,RR=RELO2                                      
         MVC   SVRATE,RATEAMNT                                                  
         MVC   SVEFFD,RATEEFFD                                                  
         TM    RATSTAT2,RATEFND    WAS A RATE FOUND                             
         BO    RATE150                                                          
         LA    R2,LINRATH                                                       
         NI    1(R2),X'FF'-X'20'                                                
         OI    6(R2),X'80'                                                      
         MVC   FVMSGNO,=AL2(AE$MRATE)                                           
         B     EXIT                                                             
*                                                                               
RATE150  MVI   FLAGT,X'FF'                                                      
*                                                                               
RATE200  ZAP   BODUB1,SVHRS                                                     
         MP    BODUB1,SVRATE                                                    
         SRP   BODUB1,64-2,5                                                    
         ZAP   SVTOTAL,BODUB1                                                   
*                                                                               
         MVC   BOWORK1,BCSPACES                                                 
         MVI   BOWORK1,C'*'                                                     
         CURED SVRATE,(8,BOWORK1+1),2,DMCB=BOPARM,FLOAT=-,ALIGN=LEFT            
         LA    R1,BOWORK1                                                       
         CLI   FLAGT,X'FF'                                                      
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
         MVC   LINRAT,0(R1)                                                     
*                                                                               
         CURED SVTOTAL,(9,LINAMT),2,DMCB=BOPARM,FLOAT=-                         
         LA    R3,BLOCK          MAKE SURE R3 POINTS AT BLOCK                   
         TM    RATSTAT2,RATEWADJ                                                
*        TM    SVSTAT,SVWADJ                                                    
         BZ    *+8                                                              
         MVI   LINADJ,C'A'         INDICATE THAT RATE WAS ADJUSTED              
         OI    LINRATH+6,X'80'                                                  
         OI    LINAMTH+6,X'80'                                                  
         OI    LINADJH+6,X'80'                                                  
*                                                                               
         CLI   SVTABN,C'R'                                                      
         BNE   RATEX                                                            
         ZAP   SVTOTAL,=P'0'                                                    
*                                                                               
RATEX    DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              VALIDATE INCOME ACCOUNT                                *         
***********************************************************************         
*                                                                               
INCVAL   DS    0H                                                               
         LA    R2,LININCH                                                       
         TM    FLDINV,FLDINC       IF INPUT NOT ALLOWED                         
         BZ    INC50                                                            
         TM    FLDINPT,FLDINC      THEN FLAG ERROR IF SOMETHING INPUT           
         BZ    INCX                                                             
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXIT                                                             
*                                                                               
INC50    TM    STATUS,RRLINE+RRSCRN   CLEAR OUT INCOME ACCT                     
         BZ    INC75                                                            
         CLI   LININC,C'*'         DONT CLEAR IF USER TYPED IT IN               
         BNE   INC75                                                            
         TWAXC LININCH,LININCH                                                  
*                                                                               
INC75    XC    FLAGT,FLAGT                                                      
         CLC   LININC,BCSPACES     GET DEFAULT IF NOTHING ON SCREEN             
         BNH   INC200                                                           
         CLC   LININC(2),=C'SI'    ELSE ACCT MUST BE SI/SK                      
         BE    INC100                                                           
         CLC   LININC(2),=C'SK'                                                 
         BE    INC100                                                           
         CLC   LININC(3),=C'*SI'                                                
         BE    INC100                                                           
         CLC   LININC(3),=C'*SK'                                                
         BE    INC100                                                           
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXIT                                                             
*                                                                               
INC100   DS    0H                                                               
         MVC   SVINCACT,BCSPACES                                                
         MVC   SVINCACT(1),CUABIN                                               
*                                                                               
         MVI   FVMINL,0                                                         
         MVI   FVMAXL,L'LININC                                                  
         GOTO1 AFVAL,LININCH                                                    
         ZIC   R1,FVXLEN                                                        
         LA    RF,FVIFLD                                                        
         CLI   LININC,C'*'         DEFAULT ACCT OR DID USER ENTER IT            
         BNE   *+14                                                             
         MVI   FLAGT,X'FF'                                                      
         LA    RF,1(RF)                                                         
         BCTR  R1,0                                                             
         EXMVC R1,SVINCACT+1,0(RF)                                              
         B     INC500                                                           
*                                                                               
INC200   MVI   FLAGT,X'FF'         INDICATE ACCT IS FROM DEFAULT LOOKUP         
         CLC   SVSIACCT,BCSPACES                                                
         BNH   INC250                                                           
         MVC   SVINCACT(1),CUABIN                                               
         MVC   SVINCACT+1(L'SVSIACCT),SVSIACCT                                  
         B     INC500                                                           
*                                                                               
INC250   CLC   SVICA,BCSPACES      DO WE HAVE AN ACCOUNT FROM GETOPT?           
         BNH   INC400              NO                                           
         MVC   SVINCACT,SVICA      YES, USE IT                                  
         B     INC500                                                           
*                                                                               
INC400   DS    0H                                                               
         MVC   SVINCACT,BCSPACES                                                
         MVC   SVINCACT(1),CUABIN                                               
         MVC   SVINCACT+1(5),=C'SKIPS'                                          
         MVC   IOKEY,BCSPACES                                                   
         MVC   IOKEY(15),SVINCACT                                               
         GOTO1 AGETACT,0           IS SKIPS VALID FOR THIS CMPY?                
         BNE   INC475                                                           
         TM    ACBSTAT,ACBSABAL    VALID FOR POSTING?                           
         BO    INC550                                                           
*                                                                               
INC475   DS    0H                                                               
         XC    SVINCACT,SVINCACT                                                
         CLI   LINBN,C'B'          INCOME ACCT REQUIRED FOR 'B' TIME            
         BNE   INCX                                                             
         MVC   FVMSGNO,=AL2(AE$MINCA)                                           
         B     EXIT                                                             
*                                                                               
INC500   MVC   IOKEY,BCSPACES                                                   
         MVC   IOKEY(15),SVINCACT                                               
         GOTO1 AGETACT,0                                                        
         BNE   EXIT                                                             
*                                                                               
INC550   MVC   SVINCACN,ACNAME                                                  
         TM    ACBSTAT,ACBSLOCK    ACCOUNT LOCKED?                              
         BZ    *+20                                                             
         MVC   FVXTRA(14),SVINCACT+1                                            
         MVC   FVMSGNO,=AL2(AE$ACTLK)                                           
         B     EXIT2                                                            
         TM    ACBSTAT,ACBSABAL    VALID FOR POSTING?                           
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INACP)                                           
         B     EXIT                                                             
         CLC   SVINCACT+1(2),=C'SI' TEST POSTING TO SVSIACCT                    
         BNE   INCX                                                             
         TM    TSCPYST1,CPYSCOST   MAKE COSTING POSTINGS                        
         BZ    INCX                                                             
*                                                                               
         L     R5,AIO1                                                          
         AH    R5,=Y(ACTRFST-ACTRECD)                                           
INC600   CLI   0(R5),0             TEST FOR EOR                                 
         BE    INC700                                                           
*                                                                               
         USING ACSPECD,R5                                                       
         CLI   ACSPTYP,ACSPOAN     TEST FOR ANALYSIS OVERRIDE                   
         BNE   INC625              NO                                           
         MVC   SV12ACT(1),CUABIN                                                
         MVC   SV12ACT+1(2),=C'12'                                              
         MVC   SV12ACT+3(12),ACSPACCT                                           
         B     INC675                                                           
*                                                                               
         USING ACSTATD,R5                                                       
INC625   CLI   0(R5),ACSTELQ                                                    
         BNE   INC675              NO                                           
         MVC   SICPOS,ACSTCPOS     COST CENTER POSITION                         
         MVC   SICNTR,ACSTCNTR     COST CENTER                                  
*                                                                               
INC675   ZIC   R1,1(R5)            BUMP TO NEXT ELEMENT                         
         AR    R5,R1                                                            
         B     INC600                                                           
*                                                                               
INC700   CLI   SV12ACT,0           TEST FOR REVENUE OVERRIDE                    
         BNE   INC725                                                           
         MVC   SV12ACT,BCSPACES                                                 
         MVC   SV12ACT(1),CUABIN                                                
         MVC   SV12ACT+1(2),=C'12'                                              
         MVC   SV12ACT+3(1),ACCOST COST ACCOUNTING ANALYSIS CODE                
*                                                                               
INC725   MVC   IOKEY,BCSPACES      VALIDATE REVENUE ACCOUNT                     
         MVC   IOKEY(L'SV12ACT),SV12ACT                                         
         CLC   SV12ACT+3(12),BCSPACES                                           
         BH    INC750                                                           
         MVC   FVMSGNO,=AL2(AE$SIMSS)                                           
         B     EXIT                                                             
*                                                                               
INC750   GOTO1 AGETACT,0                                                        
         BNE   EXIT                                                             
         TM    ACBSTAT,ACBSLOCK    ACCOUNT LOCKED?                              
         BZ    *+20                                                             
         MVC   FVXTRA(14),SV12ACT+1                                             
         MVC   FVMSGNO,=AL2(AE$ACTLK)                                           
         B     EXIT2                                                            
         TM    ACBSTAT,ACBSABAL    VALID FOR POSTING?                           
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INACP)                                           
         B     EXIT                                                             
         MVC   SV12NAME,ACNAME                                                  
*                                                                               
INCX     DS    0H                                                               
         MVC   BOWORK1,BCSPACES                                                 
         MVI   BOWORK1,C'*'                                                     
         MVC   BOWORK1+1(L'SVINCACT-1),SVINCACT+1                               
         CLC   SVINCACT,BCSPACES                                                
         BNH   INCXX                                                            
         LA    R1,BOWORK1                                                       
         CLI   FLAGT,X'FF'                                                      
         BE    *+8                                                              
         LA    R1,BOWORK1+1                                                     
         MVC   LININC,0(R1)        DISPLAY INCOME ACCOUNT                       
         OI    LININCH+6,X'80'                                                  
*                                                                               
INCXX    DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              CORRECTION OF PREVIOUS TIMESHEET                       *         
***********************************************************************         
*                                                                               
CORVAL   DS    0H                                                               
         OC    REFNUM,REFNUM       IF REFERENCE NUMBER WAS INPUT                
         BZ    CORX                THEN ENTRY IS A CORRECTION                   
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         GOTO1 =A(CORRECT),BCPARM,(RC),(R9),RR=RELO2                            
         L     R2,BCPARM+8         GET A(FIELD) TO POINT CURSOR AT              
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   EXIT                                                             
*                                                                               
CORX     DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              PROCESS NARRATIVE                                      *         
***********************************************************************         
*                                                                               
NARVAL   DS    0H                                                               
         MVC   SVNARR,BCSPACES                                                  
         MVI   SVNARR,X'01'                                                     
         CLI   LINARRH+5,0                                                      
         BE    NARX                                                             
         ZIC   R1,LINARRH+5                                                     
         STC   R1,SVNARR           SAVE LENGTH                                  
         SH    R1,=H'1'                                                         
         EXMVC R1,SVNARR+1,LINARR                                               
*                                                                               
NARX     DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              END OF SCREEN                                          *         
***********************************************************************         
*                                                                               
EOLVAL   DS    0H                                                               
         LA    R6,SVLNQ(R6)                                                     
*                                                                               
EOL100   NI    STATUS,X'FF'-RRLINE TURN OFF REFRESH FLAG                        
         LA    R4,LINLEN(R4)       END OF SCREEN YET?                           
         LA    R1,TIMENDH                                                       
         CR    R4,R1                                                            
         BL    CPJ100                                                           
         MVI   0(R6),X'FF'         MARK END OF TABLE                            
*                                                                               
         OC    NEWINPUT,NEWINPUT   ERROR IF NOTHING WAS INPUT                   
         BNZ   EOL125                                                           
         LA    R2,TIMDATAH                                                      
         MVC   FVMSGNO,=AL2(AE$MISIF)                                           
         B     EXIT                                                             
*                                                                               
EOL125   CLI   TIMUPDH+5,0         ANY INPUT INTO UPDATE FIELD                  
         BNE   EOL150                                                           
         MVC   FVMSGNO,=AL2(FVFSET)                                             
         MVC   BASMSG,=CL60'Enter Changes or ''Y'' in Update Field'             
         LA    R2,TIMUPDH                                                       
         OI    CSINDSL2,CSIACFRM   FLAG WE ARE AWAITING CONFIRMATION            
         B     EXIT                                                             
*                                                                               
EOL150   DS    0H                                                               
         MVI   TAXPASS,0                                                        
         CLI   TIMUPD,C'Y'                                                      
         BE    EOLX                                                             
         CLI   SCRIPTEX,C'Y'                                                    
         BNE   EOL160                                                           
         CLI   TIMUPD,C'S'         FOR SCRIPT ONLY                              
         BNE   EOL160                                                           
         MVI   TAXPASS,1           DON'T ADD ITEM                               
         B     EOLX                                                             
EOL160   LA    R2,TIMUPDH                                                       
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXIT                                                             
*                                                                               
EOLX     DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              BUILD ELEMENTS                                         *         
***********************************************************************         
*                                                                               
         USING SVTABLD,R6                                                       
BUILDEL  LA    R6,SVTABL                                                        
*                                                                               
BLD100   CLI   0(R6),X'FF'                                                      
         BE    BLD700                                                           
         CLI   CSOMODE,CSOMPCHA    PREPARE SCREEN FOR CHANGE?                   
         BE    BLD800                                                           
*                                                                               
*              BUILD DESCRIPTION ELEMENT                                        
*                                                                               
         LA    R5,IOAREA+2                                                      
         USING DLDESCD,R5                                                       
         MVI   DLDSEL,X'64'                                                     
         MVC   DLDSDATE,TODAY3                                                  
         CLI   CSOMODE,CSOMDCHA    USE OLD REF# DURING ITEM/COUNT               
         BNE   *+14                                                             
         MVC   DLDSREF,BCITECUR+(LSTIREF-LSTTABD)                               
         B     *+8                                                              
         BAS   RE,REFER            BUILD REFERENCE NO (SET DLDSREF)             
*                                                                               
         MVC   SVREF,DLDSREF       SAVE REFERENCE NUMBER                        
*                                                                               
         MVI   DLDSSBRF,0                                                       
         XC    DLDSSTAT(7),DLDSSTAT                                             
         XC    DLDSNARR,DLDSNARR                                                
         ZIC   R1,SVNARR                                                        
         SH    R1,=H'1'                                                         
         EXMVC R1,DLDSNARR,SVNARR+1                                             
         AH    R1,=Y(DLDSNARR-DLDSEL)                                           
         LA    R1,1(R1)                                                         
         STC   R1,DLDSLEN                                                       
         AR    R5,R1                                                            
*                                                                               
*              POSTING BACK TO JOB                                              
*                                                                               
         CLI   SVTABN,C'B'         'B' TIME - ALWAYS POST TO JOB                
         BE    BLD200                                                           
         TM    TSCPYST7,CPYSJTIM   IF OLD COSTING THEN TASK                     
         BZ    BLD500              ON NEWCOST AND JOB WAS INPUT                 
         CLC   SVSJJOB,BCSPACES                                                 
         BNH   BLD500                                                           
*                                                                               
*              MEMO X'40' ELEMENT                                               
*                                                                               
BLD200   DS    0H                                                               
         USING PRTELD,R5                                                        
         XC    PRTEL(PRTLNQ),PRTEL                                              
         MVI   PRTEL,PRTELQ        X'40'                                        
         MVI   PRTLN,PRTLNQ                                                     
         MVC   PRTSTRT,SVEFFD      EFFECTIVE DATE                               
         ZAP   PRTRATE,SVRATE      RATE                                         
         ZAP   PRTHOUR,SVHRS       HOURS                                        
         CLI   SVTABN,C'B'                                                      
         BNE   *+8                                                              
         OI    PRTSTAT,PRTSBILQ    'B' TIME                                     
         CLI   SVTABN,C'N'                                                      
         BNE   *+8                                                              
         OI    PRTSTAT,PRTSNOTQ    'N' TIME                                     
         CLI   SVTABN,C'R'                                                      
         BNE   *+8                                                              
         OI    PRTSTAT,PRTSRTEQ    'R' TIME                                     
         TM    SVSTAT,SVWADJ                                                    
         BZ    *+8                                                              
         OI    PRTSTAT,PRTSADJ     RATE WAS ADJUSTED                            
         ZIC   R1,PRTLN                                                         
         AR    R5,R1                                                            
*                                                                               
*              X'4C' ELEMENT                                                    
*                                                                               
         USING TRSDESCD,R5                                                      
         MVI   TRSDEL,X'4C'                                                     
         MVC   TRSDACCS(80),BCSPACES                                            
         MVC   TRSDACCS(14),SVINCACT+1                                          
         GOTO1 VSQUASH,BOPARM,TRSDACCS,80                                       
         ZIC   R1,BOPARM+7                                                      
         LA    R1,2(R1)                                                         
         STC   R1,TRSDLEN                                                       
         AR    R5,R1                                                            
*                                                                               
*              X'60' ELEMENT - TRANSACTION STATUS ELEMENT                       
*                                                                               
         USING TRSELD,R5                                                        
BLD300   XC    TRSEL(TRSELQ),TRSEL                                              
         MVI   TRSEL,TRSELQ        X'60' ELEMENT                                
         MVI   TRSLN,TRSLNQ                                                     
         OC    TRSSTAT2,TYPETIME   INDICATE MISSING/ADJUSTMENT                  
         ZIC   R1,TRSLN                                                         
         AR    R5,R1                                                            
*                                                                               
*              DR SJ/1R                                                         
*                                                                               
         USING DLPOSTD,R5                                                       
         MVI   DLPSEL,X'69'                                                     
         MVI   DLPSLEN,X'71'                                                    
         MVC   DLPSDBAC,SVSJACT                                                 
         MVC   DLPSDBNM,SVSJCPJN                                                
         MVC   DLPSCRAC,SV1RACT                                                 
         MVC   DLPSCRNM,SV1RNAME                                                
         MVI   DLPSTYPE,0                                                       
         OI    DLPSTYPE,X'40'                                                   
         ZAP   DLPSAMNT,SVTOTAL                                                 
         MVC   DLPSANAL,SVTSK                                                   
         ZIC   R1,DLPSLEN                                                       
         AR    R5,R1                                                            
*                                                                               
*              MEDIA TRANSFER ELEMENT IF POSTING TO U/L=SI                      
*                                                                               
         CLI   SVTABN,C'B'         ONLY POST TO SI IF B TIME                    
         BNE   BLD500                                                           
         CLC   SVINCACT+1(2),=C'SI'                                             
         BNE   BLD400                                                           
         USING ACMTD,R5                                                         
         XC    ACMTEL(ACMTLNQ),ACMTEL CLEAR ELEMENT AREA                        
         MVI   ACMTEL,ACMTELQ                                                   
         MVI   ACMTLEN,ACMTLNQ                                                  
         MVI   ACMTSYS,C'J'                                                     
         MVC   ACMTMED,SVSJJOB     MEDIA                                        
         MVC   ACMTCLI(12),SVSJCPJ CLIENT/PRODUCT/JOB                           
         MVC   ACMTMOS,CSLSTCUR+(LSTBMOSP-LSTTABD)                              
         MVC   ACMTDSCP,SVSJCPJN                                                
         ZAP   BODUB1,SVTOTAL                                                   
         CVB   R0,BODUB1                                                        
         STCM  R0,15,ACMTCOM       ITS REVENUE                                  
         STCM  R0,15,ACMTINTL      AND INTERNAL INCOME                          
         ZIC   R1,ACMTLEN                                                       
         AR    R5,R1                                                            
*                                                                               
*              CR SI  C/A SJ  OR CR SK  C/A SJ                                  
*                                                                               
BLD400   DS    0H                                                               
         USING DLPOSTD,R5                                                       
         MVI   DLPSEL,X'6A'                                                     
         MVI   DLPSLEN,X'71'                                                    
         MVC   DLPSCRAC,SVINCACT        CREDIT SVSIACCT ACCOUNT                 
         MVC   DLPSCRNM,SVINCACN                                                
         MVC   DLPSDBAC,BCSPACES                                                
         MVC   DLPSDBAC,SVSJACT         SK AND SI C/A -  CLI/PRD/JOB            
         MVC   DLPSDBNM,SVSJCPJN                                                
         MVI   DLPSTYPE,0                                                       
         ZAP   DLPSAMNT,SVTOTAL         AMOUNT                                  
         MVC   DLPSANAL,SVOFFICE                                                
         ZIC   R1,DLPSLEN                                                       
         AR    R5,R1                                                            
*                                                                               
*              X'40' ELEMENT                                                    
*                                                                               
         USING PRTELD,R5                                                        
BLD500   XC    PRTEL(PRTLNQ),PRTEL                                              
         MVI   PRTEL,PRTELQ        X'40'                                        
         MVI   PRTLN,PRTLNQ                                                     
         MVC   PRTSTRT,SVEFFD      EFFECTIVE DATE                               
         ZAP   PRTRATE,SVRATE      RATE                                         
         ZAP   PRTHOUR,SVHRS       HOURS                                        
         CLI   SVTABN,C'B'                                                      
         BNE   *+8                                                              
         OI    PRTSTAT,PRTSBILQ    'B' TIME                                     
         CLI   SVTABN,C'N'                                                      
         BNE   *+8                                                              
         OI    PRTSTAT,PRTSNOTQ    'N' TIME                                     
         CLI   SVTABN,C'R'                                                      
         BNE   *+8                                                              
         OI    PRTSTAT,PRTSRTEQ    'R' TIME                                     
         TM    SVSTAT,SVWADJ                                                    
         BZ    *+8                                                              
         OI    PRTSTAT,PRTSADJ     RATE WAS ADJUSTED                            
         ZIC   R1,PRTLN                                                         
         AR    R5,R1                                                            
*                                                                               
*              X'50' ELEMENT - HOURS MEMO ELEMENT                               
*                                                                               
         USING TRCASHD,R5                                                       
         MVI   TRCSEL,X'50'                                                     
         MVI   TRCSLEN,X'09'                                                    
         MVI   TRCSTYPE,C'H'       MEMO TIME ITEM                               
         CLC   SV1NACT+3(2),=C'L ' DO NOT WANT UPDATE TO GENERATE               
         BNE   *+8                 BUCKETS FOR LEAVE OF ABSENCE                 
         MVI   TRCSTYPE,C'X'                                                    
         ZAP   TRCSAMNT,SVHRS                                                   
         ZAP   BOPL61,SVHRS                                                     
         ZIC   R1,TRCSLEN                                                       
         AR    R5,R1                                                            
*                                                                               
*              BUILD X'51' ELEMENT - PROJECT CONTROL ELEMENT                    
*                                                                               
         USING PCIELD,R5                                                        
         XC    PCIEL(PCILN2Q),PCIEL                                             
         MVI   PCIEL,PCIELQ        X'51' ELEMENT                                
         MVI   PCILN,PCILN2Q                                                    
         MVC   PCICLI,BCSPACES                                                  
         CLC   SVSJCPJ,BCSPACES                                                 
         BNH   BLD550                                                           
         MVC   PCICLI,SVSJACT                                                   
         MVC   PCIPRJT,SVSJACT                                                  
         MVC   PCITSK,SVTSK                                                     
         OC    SVPCACT,SVPCACT     ANY PROJECT CONTROL ACCOUNT?                 
         BZ    *+10                                                             
         MVC   PCIPRJT,SVPCACT                                                  
         ZIC   R1,PCILN                                                         
         AR    R5,R1                                                            
*                                                                               
*              X'60' ELEMENT - TRANSACTION STATUS ELEMENT                       
*                                                                               
         USING TRSELD,R5                                                        
BLD550   XC    TRSEL(TRSELQ),TRSEL                                              
         MVI   TRSEL,TRSELQ        X'60' ELEMENT                                
         MVI   TRSLN,TRSLNQ                                                     
         OC    TRSSTAT2,TYPETIME   INDICATE MISSING/ADJUSTMENT                  
         ZIC   R1,TRSLN                                                         
         AR    R5,R1                                                            
*                                                                               
*              DR 1R/1C OR DR 1R/1N                                             
*                                                                               
         USING DLPOSTD,R5                                                       
         MVI   DLPSEL,X'69'                                                     
         MVI   DLPSLEN,X'71'                                                    
         MVC   DLPSDBAC,SV1RACT                                                 
         MVC   DLPSDBNM,SV1RNAME                                                
         MVC   DLPSCRAC,SV1CACT                                                 
         MVC   DLPSCRNM,SV1CNAME                                                
         OC    SV1NACT,SV1NACT                                                  
         BZ    *+16                                                             
         MVC   DLPSCRAC,SV1NACT                                                 
         MVC   DLPSCRNM,SV1NNAME                                                
         MVI   DLPSTYPE,0                                                       
         ZAP   DLPSAMNT,=P'0'                                                   
         MVC   DLPSANAL,SVOFFICE                                                
         OC    SV1NACT,SV1NACT                                                  
         BZ    *+10                                                             
         MVC   DLPSANAL,SV1ROFFC   USE 1R OFFICE IN A 1N POSTING                
         ZIC   R1,DLPSLEN                                                       
         AR    R5,R1                                                            
*                                                                               
         CLI   SVTABN,C'B'         ONLY POST IF B TIME                          
         BNE   BLD600                                                           
         TM    TSCPYST1,CPYSCOST   ONLY POST IF AGENCY ON COSTING               
         BZ    BLD600                                                           
         CLC   SVINCACT+1(2),=C'SI'     MUST HAVE POSTING TO SI                 
         BNE   BLD600                                                           
*                                                                               
         MVI   DLPSEL,X'68'        DOUBLE POSTING                               
         MVI   DLPSLEN,X'71'                                                    
         MVC   DLPSDBAC,SV1CACT    DR=1C A/C                                    
         MVC   DLPSDBNM,SV1CNAME                                                
         MVC   DLPSCRAC,SV12ACT    CR=12 A/C (REVENUE)                          
         MVC   DLPSCRNM,SV12NAME                                                
         MVI   DLPSTYPE,X'80'      SUBSIDIARY POSTINGS                          
         ZAP   DLPSAMNT,SVTOTAL                                                 
         MVC   DLPSANAL,SVOFFICE   OFFICE=CLI/PRO OFFICE                        
         ZIC   R1,DLPSLEN                                                       
         AR    R5,R1                                                            
*                                                                               
BLD600   MVI   0(R5),0             END OF RECORD                                
         LA    R5,1(R5)                                                         
         LA    R1,IOAREA                                                        
         SR    R5,R1               PUT OUT ACCDAY RECORD                        
         STH   R5,BOHALF1                                                       
         MVC   IOAREA(2),BOHALF1                                                
         MVC   CSOLINE,SVCSLINE                                                 
*                                                                               
         LA    R1,BCPARM                                                        
         XC    0(4,R1),0(R1)       CLEAR DISK ADDRESS                           
         ST    R1,BOPARM+8                                                      
         MVC   BOWORK1(L'DLDSREF),IOAREA+2+(DLDSREF-DLDESCD)                    
         L     R5,BOPARM+8                                                      
         MVC   BOWORK1+10(4),0(R5) DISK ADDRESS                                 
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         CLI   TAXPASS,0                                                        
         BNZ   BLD680              SCRIPT BYPASS                                
         GOTO1 AADDITE,BOPARM,IOAREA,BOPL61,BOWORK1                             
BLD680   CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   EXIT                                                             
         LA    R6,SVLNQ(R6)        TABLE WIDTH                                  
         B     BLD100                                                           
*                                                                               
BLD700   DS    0H                                                               
         LA    R2,TIMSTFH                                                       
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         TWAXC TIMTABH,TIMTABH                                                  
         TWAXC TIMDATAH,TIMENDH,PROT=Y                                          
         TWAXC TIMUPDH,TIMUPDH                                                  
*                                                                               
         OC    REFNUM,REFNUM       TEST FOR CORRECTION                          
         BNZ   BLD800                                                           
         LA    R5,IOKEY                                                         
         USING ACTRECD,R5                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKEY(L'SV1RACT),SV1RACT                                        
         GOTO1 AIO,IORDUPD+IOACCDIR+IO2                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOGETRUP+IOACCMST+IO2                                        
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$BAD1R)                                           
         B     EXIT                                                             
*                                                                               
         L     R5,AIO2                                                          
         AH    R5,=Y(ACTRFST-ACTRECD)                                           
BLD725   CLI   0(R5),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R5),X'30'         STATUS ELEMENT                               
         BE    BLD750                                                           
         ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     BLD725                                                           
*                                                                               
         USING ACSTATD,R5                                                       
BLD750   XC    BOELEM,BOELEM                                                    
         ZIC   R1,ACSTLEN                                                       
         SH    R1,=H'1'                                                         
         EXMVC R1,BOELEM,ACSTATD   EXTRACT ELEMENT FROM RECORD                  
         LA    R5,BOELEM           ADDRESS NEW POSITION                         
         CLI   ACSTLEN,ACSTLNQ2    TEST ELEMENT IS LARGE ENOUGH                 
         BNL   *+8                 YES                                          
         MVI   ACSTLEN,ACSTLNQ2    ITS LESS THAN MINIMUM                        
         MVC   ACSTSNUM,TSNUM      SET NEW TIME SHEET NUMBER                    
         L     R4,AIO2                                                          
         GOTO1 VHELLO,BCPARM,(C'D',ACCMST),('ACSTELQ',(R4)),0                   
         GOTO1 (RF),(R1),(C'P',ACCMST),(R4),ACSTATD,0                           
         CLI   12(R1),0            TEST UPDATE IS OK                            
         BE    *+6                                                              
         DC    H'0'                UNABLE TO ADD NEW ELEMENT TO REC             
         GOTO1 AIO,IOPUTREC+IOACCMST+IO2                                        
         BE    *+6                                                              
         DC    H'0'                COULDNT WRITE BACK AMENDED RECORD            
*                                                                               
BLD800   DS    0H                                                               
         CLI   CSACT,ACTINP        ITEM/INPUT                                   
         BNE   EXIT                DONT CALL TAX AGAIN FOR ITE CHA              
*                                                                               
*      CALL TAX SCREEN, IF PF9 PRESSED, FORCE UPDATE OF ENTERED FIELDS          
*                                                                               
CALLTAX  DS    0H                                                               
         CLI   BCPFKEY,9                                                        
         BNE   CTX100                                                           
         CLI   TWAMODE,2           ALREADY DOING TAX                            
         BE    CTX100              YES, LET TAX OVERLAY HANDLE PF9              
*                                                                               
         LA    R2,TIMDEPH          ENSURE NO FIELDS ARE I/P BEFORE              
         XR    R0,R0               ALLOWING PF9 TO TAX                          
*                                                                               
CTX10    CLI   0(R2),0             END OF TABLE                                 
         BE    CTX100              YES                                          
         TM    1(R2),X'20'         PROTECTED?                                   
         BO    CTX30               YES, DOSEN'T COUNT                           
         CLI   5(R2),0             ANY I/P                                      
         BNE   CTXERR              YES, CAN'T PF9 TO TAX                        
CTX30    IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     CTX10                                                            
*                                                                               
CTX100   GOTO1 =A(TAXMOD),BCPARM,(RC),(R9),RR=RELO2                             
         B     EXIT1               FVADDR SET IN TAX OVERLAY                    
*                                                                               
CTXERR   MVC   FVMSGNO,=AL2(AE$UPDAT)                                           
         LA    R2,TIMUPDH                                                       
*                                                                               
EXIT     ST    R2,FVADDR                                                        
EXIT1    XIT1                                                                   
*                                                                               
EXIT2    L     RF,BCAUTL           EXECUTING SCRIPT?                            
         TM    TSTAT6-UTLD(RF),TST6SCRP                                         
         BZ    EXIT                                                             
         MVC   FVXTRA,SPACES                                                    
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              BUILD REFERENCE NUMBER                                 *         
***********************************************************************         
*                                                                               
REFER    NTR1                                                                   
         USING DLDESCD,R5                                                       
         XC    BOWORK1,BOWORK1                                                  
         MVC   BOWORK1(6),BCSPACES                                              
         OC    REFNUM,REFNUM       IF USER ENTERED REFERENCE NUMBER             
         BZ    REFER2              THE USE IT                                   
         MVC   DLDSREF,REFNUM                                                   
         MVC   BOWORK1(6),DLDSREF                                               
         B     REFERX                                                           
*                                                                               
REFER2   LH    R4,TSNUM            OTHERWISE GENERATE ONE                       
         AH    R4,=H'1'                                                         
         CH    R4,=H'9999'         IF # > 10,000 THEN RESET TO 1                
         BNH   *+8                                                              
         LA    R4,1                                                             
         STH   R4,TSNUM                                                         
         MVI   BOWORK1,C'T'           PREFIX THE NUMBER WITH T                  
         CVD   R4,BODUB1                                                        
         OI    BODUB1+7,X'0F'                                                   
         UNPK  BOWORK1+1(4),BODUB1+5(3)                                         
         MVC   DLDSREF,BOWORK1                                                  
*                                                                               
REFERX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              ROUTINE TO GET 1C/SJ LEDGER ELEMENT                    *         
***********************************************************************         
*                                                                               
GET1C    NTR1                                                                   
         MVI   CLIPOS,2                                                         
         MVC   IOKEY,BCSPACES                                                   
         MVC   IOKEY(1),CUABIN                                                  
         MVC   IOKEY+1(2),=C'1C'                                                
         TM    BCCPYEL+(CPYSTAT3-CPYELD),X'04'                                  
         BZ    *+14                                                             
         MVC   IOKEY+1(2),=C'SJ'     PROJECT CONTROL USES SJ COSES              
         MVI   CLIPOS,1                                                         
         GOTO1 AGETACT,0                                                        
         BNE   EXIT                                                             
*                                                                               
         L     R3,AIO1                                                          
         AH    R3,=Y(ACTRFST-ACTRECD)                                           
GETL02   CLI   0(R3),0                                                          
         BE    EXIT                                                             
         CLI   0(R3),X'14'         FIND LEDGER ELEMENT                          
         BE    GETL04                                                           
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     GETL02                                                           
*                                                                               
         USING ACLEDGD,R3                                                       
GETL04   CLI   ACLTLEN,X'20'       EXIT IF OLD FORMAT                           
         BL    EXIT                                                             
         CLI   ACLTCLI,0                                                        
         BE    EXIT                                                             
         MVC   CLIPOS,ACLTCLI      SAVE DISPLACEMENT OF CLIENT CODE             
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              READ GETOPT PROFILE                                    *         
***********************************************************************         
*                                                                               
OPTMNT   NTR1                                                                   
         L     R0,AGOPBLK                                                       
         LH    R1,=Y(GOBLOCKX-GOBLOCKD)                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R2,AGOPBLK                                                       
         USING GOBLOCKD,R2                                                      
         MVC   GOADM,VDMGR                                                      
         MVC   GOSELCUL,CUABIN                                                  
         MVC   GOSELCUL+1(2),=C'SJ'                                             
         MVC   GOSELCLI,BCSPACES                                                
         MVC   GOSELCLI(3),SVSJCLI                                              
         MVC   GOAEXT,AGOXBLK      EXTENSION BLOCK                              
         CLC   SVSJPRO,BCSPACES                                                 
         BNH   OPT100                                                           
         MVC   GOSELPRO(3),SVSJPRO                                              
         OC    GOSELPRO,BCSPACES                                                
         CLC   SVSJJOB,BCSPACES                                                 
         BNH   OPT100                                                           
         MVC   GOSELJOB(6),SVSJJOB                                              
         OC    GOSELJOB,BCSPACES                                                
         MVC   GOSELWC,SVTSK                                                    
*                                                                               
OPT100   DS    0H                                                               
         GOTO1 VGETOPT,BCPARM,AGOPBLK                                           
*                                                                               
         L     R2,AGOXBLK                                                       
         USING GOXBLKD,R2                                                       
*                                                                               
OPT200   DS    0H                                                               
         MVC   SVICA,GOICA                                                      
         CLI   GOFPT,C' '          ANY PROFILE ENTERED                          
         BNH   OPT300                                                           
         CLI   GOFPT,C'A'          PROD REQUIRED FOR ALL TYPES TIME             
         BE    OPT250                                                           
         CLC   GOFPT,SVTABN        REQUIRED FOR THIS TYPE?                      
         BNE   OPT300                                                           
OPT250   CLC   SVSJPRO,BCSPACES                                                 
         BH    OPT300                                                           
         LA    R2,LINPROH                                                       
         ST    R2,BCPARM                                                        
         MVC   FVMSGNO,=AL2(AE$RQPRD)                                           
         B     OPTX                                                             
*                                                                               
OPT300   DS    0H                                                               
         CLI   GOFJT,C' '          ANY PROFILE ENTERED                          
         BNH   OPT400                                                           
         CLI   GOFJT,C'A'          PROD REQUIRED FOR ALL TYPES TIME             
         BE    OPT350                                                           
         CLC   GOFJT,SVTABN        REQUIRED FOR THIS TYPE?                      
         BNE   OPT400                                                           
OPT350   CLC   SVSJJOB,BCSPACES                                                 
         BH    OPT400                                                           
         LA    R2,LINJOBH                                                       
         ST    R2,BCPARM                                                        
         MVC   FVMSGNO,=AL2(AE$RQJOB)                                           
         B     OPTX                                                             
*                                                                               
OPT400   NI    SVSTAT,X'FF'-SVTAX                                               
         CLI   SVTABN,C'B'         BILLABLE TIME?                               
         BNE   OPTX                TAX IS MOOT                                  
*                                                                               
         CLI   GOTAX,C'Y'          IS THIS TAXABLE?                             
         BNE   *+8                                                              
         OI    SVSTAT,SVTAX        FLAG THIS TABLE ENTRY                        
*                                                                               
         XC    SVTXLOC,SVTXLOC                                                  
         CLC   GOTXLOC,BCSPACES                                                 
         BNH   *+10                                                             
         MVC   SVTXLOC,GOTXLOC                                                  
         XC    SVTXWC,SVTXWC                                                    
         CLC   GOTXWC,BCSPACES                                                  
         BNH   *+10                                                             
         MVC   SVTXWC,GOTXWC                                                    
*                                                                               
OPTX     B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              LITERALS                                               *         
***********************************************************************         
*                                                                               
ACCMST   DC    C'ACCMST  '                                                      
NINES    DC    CL14'99999999999999'                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              VALIDATE CORRECTION AGAINST ITEM ON FILE               *         
***********************************************************************         
*                                                                               
CORRECT  NMOD1 0,*CORRCT*                                                       
         L     RC,0(R1)                                                         
         L     R9,4(R1)                                                         
*                                                                               
         USING LIND,R4             INSURE THAT ONLY 1 LINE WAS ENTERED          
         LA    R1,TIMENDH          ON THE SCREEN FOR CORRECTIONS.               
         LA    R4,TIMDATAH         INPUT ON FIRST LINE IS OK SO                 
         LA    R4,LINLEN(R4)       SKIP FIRST LINE                              
*                                                                               
CORR50   CLI   5(R4),0             ANY INPUT AFTER 1ST LINE IS AN ERROR         
         BE    CORR75                                                           
         ST    R4,BCPARM+8         SAVE ADDRESS OF FIELD                        
         MVC   FVMSGNO,=AL2(AE$1ONLY)                                           
         B     CORRX                                                            
*                                                                               
CORR75   ZIC   RE,0(R4)            LENGTH OF FIELD                              
         AR    R4,RE               BUMP TO NEXT FIELD                           
         CR    R4,R1                                                            
         BL    CORR50                                                           
*                                                                               
         LA    R6,SVTABL                                                        
         USING SVTABLD,R6                                                       
         MVC   IOKEY,BCSPACES                                                   
         LA    R5,IOKEY                                                         
         USING TRNRECD,R5                                                       
         MVC   TRNKCULA,SVSJACT                                                 
         MVC   TRNKWORK,SVTSK                                                   
         MVC   TRNKCULC,SV1RACT                                                 
         MVC   TRNKDATE,TODAY3                                                  
         MVC   TRNKREF,REFNUM                                                   
         MVI   TRNKREF+5,C' '                                                   
         MVI   TRNKSBR,0                                                        
         MVC   IOKEYSAV,IOKEY                                                   
         GOTO1 AIO,IOHI+IOACCMST+IO1                                            
         B     CORR200                                                          
*                                                                               
CORR100  GOTO1 AIO,IOSQ+IOACCMST+IO1                                            
*                                                                               
CORR200  L     R5,AIO1                                                          
         CLC   TRNKEY(TRNKSBR-TRNKEY),IOKEYSAV                                  
         BNE   CORRN               DID NOT FIND IT                              
*                                                                               
CORR400  L     R5,AIO1                                                          
         AH    R5,=Y(ACTRFST-ACTRECD)                                           
         USING TRNELD,R5                                                        
         CLI   TRNEL,TRNELQ        X'44' ELEMENT - LOOK FOR TRNSACTNS           
         BNE   CORR100                                                          
         CLI   TRNTYPE,49          BATCH TYPE 49                                
         BNE   CORR100                                                          
*                                                                               
         USING TRNRECD,R5                                                       
CORR600  LA    R5,IOKEY                                                         
         MVC   IOKEY,BCSPACES                                                   
         MVC   IOKEY,IOKEYSAV      NOW CHECK FOR DUPLICATE ITEM                 
         MVC   TRNKREF,REFNUM      SET FOR FULL REFERENCE NUMBER                
         MVC   IOKEYSAV,IOKEY                                                   
         GOTO1 AIO,IOHI+IOACCMST+IO1                                            
         BE    CORR800                                                          
         B     CORRX                                                            
*                                                                               
CORR700  GOTO1 AIO,IOSQ+IOACCMST+IO1                                            
         BNE   CORRX                                                            
*                                                                               
CORR800  L     R5,AIO1                                                          
         CLC   TRNKEY(TRNKSBR-TRNKEY),IOKEYSAV                                  
         BNE   CORRX                                                            
         L     R5,AIO1                                                          
         AH    R5,=Y(ACTRFST-ACTRECD)                                           
         USING TRNELD,R5                                                        
         CLI   TRNEL,TRNELQ        X'44' ELEMENT - LOOK FOR TRNSACTNS           
         BNE   CORR700                                                          
         CLI   TRNTYPE,49          BATCH TYPE 49                                
         BNE   CORR700                                                          
         MVC   FVMSGNO,=AL2(AE$DUPIT)                                           
         LA    R4,TIMDATAH                                                      
         ST    R4,BCPARM+8         SAVE A(1ST FIELD)                            
         B     CORRX                                                            
*                                                                               
CORRN    MVC   FVMSGNO,=AL2(AE$NFILE)                                           
         LA    R4,TIMDATAH                                                      
         ST    R4,BCPARM+8         SAVE A(1ST FIELD)                            
*                                                                               
CORRX    XMOD1 1                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              READ U/L=1R                                            *         
***********************************************************************         
*                                                                               
READ1R   NMOD1 0,*READ1R*                                                       
         L     RC,0(R1)                                                         
         L     R9,4(R1)                                                         
*                                                                               
         LA    R5,IOKEY                                                         
         USING ACTRECD,R5                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(2),=C'1R'                                                
         GOTO1 AGETLDG                                                          
*                                                                               
         USING LDGTABD,R5                                                       
         ICM   R5,15,ACALDG                                                     
         MVC   LVL1Q,LDGTLVA                                                    
         MVC   LVL2Q,LDGTLVB                                                    
         MVC   LVL3Q,LDGTLVC                                                    
         MVC   LVL4Q,LDGTLVD                                                    
         MVC   SV1ROFFP,LDGTOFFP   OFFICE POSITION                              
         XMOD1 1                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              READ U/L=1C                                            *         
***********************************************************************         
*                                                                               
READ1C   NMOD1 0,*READ1C*                                                       
         L     RC,0(R1)                                                         
         L     R9,4(R1)                                                         
*                                                                               
         LA    R5,IOKEY                                                         
         USING ACTRECD,R5                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(2),=C'1C'                                                
         GOTO1 AGETLDG                                                          
*                                                                               
         USING LDGTABD,R5                                                       
         ICM   R5,15,ACALDG                                                     
         MVC   LVL1CST,LDGTLVA                                                  
         MVC   LVL2CST,LDGTLVB                                                  
         MVC   LVL3CST,LDGTLVC                                                  
         MVC   LVL4CST,LDGTLVD                                                  
         XMOD1 1                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              PERCALL INTERFACE                                      *         
***********************************************************************         
*                                                                               
PERCINTF NMOD1 0,*PRCINT*                                                       
         L     RC,0(R1)                                                         
         L     R9,4(R1)                                                         
*                                                                               
         L     R0,AIO1             CLEAR PERCALL BLOCK AREA                     
         LH    R1,=Y(PERLNQ)                                                    
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R2,AIO1                                                          
         USING PERD,R2                                                          
         MVC   PERDMGR,VDMGR       A(DATAMGR)                                   
         MVC   PERCOMP,CUABIN      COMPANY CODE                                 
*                                                                               
         LA    RF,SV1RACTC         ISOLATE OFFICE CODE                          
         SR    R1,R1                                                            
         IC    R1,LVL1Q                                                         
         SH    R1,=H'1'                                                         
         EX    R1,*+4                                                           
         MVC   PEROFF(0),0(RF)                                                  
         OC    PEROFF,BCSPACES                                                  
         LA    RF,1(R1,RF)                                                      
*                                                                               
         SR    R1,R1               ISOLATE DEPT CODE                            
         IC    R1,LVL2Q                                                         
         SR    R0,R0                                                            
         IC    R0,LVL1Q                                                         
         SR    R1,R0                                                            
         SH    R1,=H'1'                                                         
         EX    R1,*+4                                                           
         MVC   PERDPT(0),0(RF)                                                  
         OC    PERDPT,BCSPACES                                                  
         LA    RF,1(R1,RF)                                                      
*                                                                               
         SR    R1,R1               ISOLATE SUBDEPT CODE                         
         IC    R1,LVL3Q                                                         
         SR    R0,R0                                                            
         IC    R0,LVL2Q                                                         
         SR    R1,R0                                                            
         SH    R1,=H'1'                                                         
         EX    R1,*+4                                                           
         MVC   PERSUB(0),0(RF)                                                  
         OC    PERSUB,BCSPACES                                                  
         LA    RF,1(R1,RF)                                                      
*                                                                               
         SR    R1,R1               ISOLATE PERSON CODE                          
         IC    R1,LVL4Q                                                         
         SR    R0,R0                                                            
         IC    R0,LVL3Q                                                         
         SR    R1,R0                                                            
         SH    R1,=H'1'                                                         
         EX    R1,*+4                                                           
         MVC   PERAC(0),0(RF)                                                   
         OC    PERAC,BCSPACES                                                   
         LA    RF,1(R1,RF)                                                      
*                                                                               
         MVC   PERSTD,TODAY3       START DATE                                   
         MVC   PERENDD,TODAY3      END DATE                                     
         OI    PERFLAGS,PEROVER    OVERRIDE LOCATION WITH TS LOCK               
         GOTO1 =V(PERCALL),(R2),RR=RB                                           
         TM    PERERR,PERNOTER     PERSON RECORD NOT FOUND?                     
         BO    PERCXYES                                                         
         CLI   PERLNUM,0           CHECK NUMBER OF LOCATIONS                    
         BE    PERCXNO                                                          
         MVC   SVHIRE,PERLSTD                                                   
         MVC   SVTERM,PERLENDD                                                  
*                                                                               
PERCXYES CLI   *+1,0                                                            
         B     *+8                                                              
PERCXNO  CLI   *,0                                                              
         XMOD1 1                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
TAXMOD   NMOD1 TAXDLN,**TAX**                                                   
         USING TAXD,R8                                                          
         LR    R8,RC                                                            
         L     RC,0(R1)                                                         
         XC    TXHNUM,TXHNUM                                                    
*                                                                               
         CLI   TWASCRN,X'ED'       TAX SCREEN ACTIVE?                           
         BE    TAX110                                                           
*                                                                               
         CLI   BCPFKEY,9           CALLING DIRECT                               
         BNE   *+12                                                             
         MVI   TXHMODE,C'D'                                                     
         B     TAX110                                                           
*                                                                               
         MVI   TXHMODE,C'E'        EDIT MODE                                    
         CLI   TWAMODE,2                                                        
         BE    TAX110              CALL TAX                                     
         MVI   TXHMODE,C'B'        BUILD SCREEN                                 
*                                                                               
         CLI   CSOMODE,CSOMPCHA    PREPARE SCREEN FOR CHANGE?                   
*              BUILD BLOCK FOR TAX OVERLAY                                      
*                                                                               
         GOTO1 VDATCON,BOPARM,(1,TODAY3),(5,TAXDATE)                            
*                                                                               
         USING STXD,R5                                                          
         LA    R5,TXDDATA                                                       
         MVC   STXPASS,TAXPASS                                                  
         USING SVTABLD,R6                                                       
         LA    R6,SVTABL                                                        
*                                                                               
TAX10    CLI   0(R6),X'FF'         END OF INPUT LINES                           
         BE    TAX100                                                           
         TM    SVSTAT,SVTAX                                                     
         BNO   TAX60                                                            
         CLI   SVTABN,C'B'         B TIME ONLY                                  
         BNE   TAX60                                                            
*                                                                               
         ZIC   RF,TXHNUM                                                        
         LA    RF,1(RF)                                                         
         STC   RF,TXHNUM                                                        
*                                                                               
         MVC   STXACC,SVSJACT                                                   
         MVC   STXREF,SVREF                                                     
         MVC   STXREF,SVREF                                                     
         XC    STXORD,STXORD                                                    
         MVC   STXDTE,TAXDATE                                                   
         MVC   STXCOFF,SVOFFICE                                                 
         ZAP   STXAMT,SVTOTAL      CASH AMOUNT                                  
*                                                                               
         XR    R1,R1                                                            
         LA    R2,SVNARR                                                        
         ICM   R1,1,0(R2)          LENGTH                                       
         BZ    TAX50                                                            
         BCTR  R1,0                                                             
         EXMVC R1,STXNARR,SVNARR+1                                              
*                                                                               
TAX50    LA    R5,STXLNQ(R5)       BUMP OUTPUT                                  
TAX60    LA    R6,SVLNQ(R6)        BUMP INPUT                                   
         B     TAX10                                                            
*                                                                               
TAX100   OC    TXHNUM,TXHNUM       ANYTHING TAXABLE HERE                        
         BZ    TAXXIT                                                           
*                                                                               
TAX110   LA    R3,X'45'                                                         
         GOTO1 VCOLY,BOPARM,((R3),0),(0,0)  (CALLOV)                            
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         MVI   CSSPROG,1                USEING ALT PFKEYS                       
         GOTO1 (RF),BOPARM,(TAXPASS,TXDATA),(R9)  GO TO TAX OVERLAY             
*                                                                               
         CLI   CSACT,ACTINP        CALLED FRON ITE INP                          
         BNE   TAXXIT              NO                                           
*                                                                               
         MVI   TWAMODE,2                                                        
         CLI   TXHMODE,C'E'        STILL IN EDIT MODE                           
         BE    TAXXIT              OVERLAY HAS SET-UP CURSOR                    
         MVI   TWAMODE,0           SALES TAX IS DONE                            
*                                                                               
         CLI   TWASCRN,X'ED'       TAX SCREEN STILL ACTIVE                      
         BE    TAXXIT              YES                                          
*                                                                               
         LA    R2,TIMDEPH                                                       
         ST    R2,FVADDR                                                        
*                                                                               
TAXXIT   XMOD1                                                                  
         EJECT                                                                  
***********************************************************************         
*              DSECT FOR WORKING STORAGE                              *         
***********************************************************************         
*                                                                               
PROGD    DSECT                                                                  
RELO2    DS    A                                                                
TSNUM    DS    H                   TIME SHEET TRANSACTION NUMBER                
FLAGT    DS    XL1                 TEMP FLAG                                    
LINE     DS    XL1                                                              
BLOCK    DS    480C                                                             
*                                                                               
PROFILES DS    0XL8                PROFILES                                     
PROFILE1 DS    XL1                 Y = ONLY DISPLAY CLIENT NAME                 
PROFILE2 DS    XL1                 Y = ALLOW TS DATE FORWARD 1 YEAR             
         DS    XL6                 SPARE                                        
*                                                                               
TSCPYST1 DS    XL1                 COMPANY ELEMENT STATUS BYTE #1               
TSCPYST2 DS    XL1                 COMPANY ELEMENT STATUS BYTE #2               
TSCPYST3 DS    XL1                 COMPANY ELEMENT STATUS BYTE #3               
TSCPYST4 DS    XL1                 COMPANY ELEMENT STATUS BYTE #4               
TSCPYST5 DS    XL1                 COMPANY ELEMENT STATUS BYTE #5               
TSCPYST6 DS    XL1                 COMPANY ELEMENT STATUS BYTE #6               
TSCPYST7 DS    XL1                 COMPANY ELEMENT STATUS BYTE #7               
*                                                                               
STATUS   DS    XL1                                                              
RRLINE   EQU   X'80'               REREAD RATE/INCOME FOR THIS LINE             
RRSCRN   EQU   X'01'               REREAD RATE/INCOME ENTIRE SCREEN             
PC1R     EQU   X'04'               1R ACCT FLAGGED FOR PROJ/CNTL                
PCSJ     EQU   X'08'               SJ ACCT FLAGGED FOR PROJ/CNTL                
*                                                                               
STATUS2  DS    XL1                                                              
RTEFND   EQU   X'10'               RATE WAS FOUND                               
RTEADJ   EQU   X'20'               ADJUSTMENT RATE WAS FOUND                    
RRDUP    EQU   X'40'               CLEAR DUPLICATE FIELD                        
*                                                                               
SCRNHRS  DS    PL8                 TOTAL -HRS CURRENTLY ON SCREEN\              
TOTHRS   DS    PL8                 TEMPORARY HRS ACCUMULATOR                    
*                                                                               
FLDSTAT  DS    0XL3                                                             
FLDINV   DS    XL1                 FIELDS THAT ARE INVALID                      
FLDINPT  DS    XL1                 FIELDS THAT WERE INPUT                       
FLDREQ   DS    XL1                 FIELDS THAT ARE REQUIRED                     
FLDTIM   EQU   X'01'               TYPE CLASSIFICATION B/N/R                    
FLDHRS   EQU   X'02'               HOURS FIELD                                  
FLDCLI   EQU   X'04'               CLIENT FIELD                                 
FLDPRO   EQU   X'08'               PRODUCT FIELD                                
FLDJOB   EQU   X'10'               JOB FIELD                                    
FLDTSK   EQU   X'20'               TASK FIELD                                   
FLDRATE  EQU   X'40'               RATE FIELD                                   
FLDINC   EQU   X'80'               INCOME FIELD                                 
*                                                                               
SCRREQ   DS    XL1                 SCREEN 'REQUIRED FIELD INDICATOR'            
TYPETIME DS    CL1                 TYPE OF TIME (ADJ/MISS/REG)                  
*                                                                               
LVL1Q    DS    XL1                 LENGTH OF LEVEL 1 OF 1R                      
LVL2Q    DS    XL1                 LENGTH OF LEVEL 2 OF 1R                      
LVL3Q    DS    XL1                 LENGTH OF LEVEL 3 OF 1R                      
LVL4Q    DS    XL1                 LENGTH OF LEVEL 4 OF 1R                      
*                                                                               
LVL1CST  DS    XL1                 LENGTH OF LEVEL 1 OF 1C                      
LVL2CST  DS    XL1                 LENGTH OF LEVEL 2 OF 1C                      
LVL3CST  DS    XL1                 LENGTH OF LEVEL 3 OF 1C                      
LVL4CST  DS    XL1                 LENGTH OF LEVEL 4 OF 1C                      
*                                                                               
SVTSD    DS    XL1                 SAVED TIME SHEET DAY - TSD=XXX               
SV1RACT  DS    0CL15               SAVED 1R ACCOUNT                             
SV1RCUL  DS    CL3                       - CMPY/UNIT/LEDG                       
SV1RACTC DS    CL12                      - ACCOUNT CODE                         
SV1RNAME DS    CL36                SAVED 1R ACCOUNT NAME                        
SV1ROFFP DS    CL1                 SAVED 1R OFFICE POSITION                     
SV1ROFFC DS    CL2                 SAVED 1R OFFICE CODE                         
SVHIRE   DS    PL3                 SAVED HIRE DATE                              
SVTERM   DS    PL3                 SAVED TERMINATION DATE                       
SVADJEFD DS    CL3                 EFFECTIVE DATE FOR AJRATE USEAGE             
MAXLINES EQU   5                   MAX # ENTRIES/SCREEN                         
*                                                                               
NEWINPUT DS    XL1                 SOMETHING WAS INPUT                          
PL13     DS    PL13                                                             
POSN     DS    CL1                                                              
CENTR    DS    CL3                                                              
SVSIACCT DS    CL14                SPECIAL INCOME ACCOUNT (1R)                  
SICPOS   DS    XL(L'ACSTCPOS)                                                   
SICNTR   DS    CL(L'ACSTCNTR)                                                   
CLIPOS   DS    CL1                 DISPLACEMENT TO CLIENT IN 1C                 
DEPGRP   DS    CL1                                                              
TODAY3   DS    CL3                                                              
TODAYR   DS    CL3                 TODAY'S REAL DATE                            
REFNUM   DS    CL6                                                              
SCRIPTEX DS    CL1                 SCRIPT EXECUTION                             
TAXPASS  DS    XL1                                                              
*                                  TAX INTERFACE BLOCK                          
TXDATA   DS    0C                                                               
TXHMODE  DS    CL1                 'E'DIT OR 'B'UILD                            
TXHNUM   DS    CL1                 'E'DIT OR 'B'UILD                            
TXDDATA  DS    (STXLNQ*MAXLINES)C   LINE DATA                                   
IOAREA   DS    2000C                                                            
SVTABL   DS    CL(MAXLINES*SVLNQ+1)                                             
PROGDX   DS    0C                                                               
         EJECT                                                                  
***********************************************************************         
*              DSECT FOR TAX MODULE                                   *         
***********************************************************************         
*                                                                               
TAXD     DSECT                                                                  
TAXDATE  DS    CL8                                                              
TAXDLN   EQU   *-TAXD                                                           
         EJECT                                                                  
***********************************************************************         
*              DSECT FOR A SCREEN LINE                                *         
***********************************************************************         
*                                                                               
LIND     DSECT                                                                  
LINBNH   DS    CL8                                                              
LINBN    DS    CL1                 BILLABLE OR NOT                              
LINBNX   DS    CL8                                                              
LINHRH   DS    CL8                                                              
LINHR    DS    CL6                 HOURS                                        
LINHRX   DS    CL8                                                              
LINCLIH  DS    CL8                                                              
LINCLI   DS    CL7                 CLIENT                                       
LINCLIX  DS    CL8                                                              
LINPROH  DS    CL8                                                              
LINPRO   DS    CL3                 PRODUCT                                      
LINPROX  DS    CL8                                                              
LINJOBH  DS    CL8                                                              
LINJOB   DS    CL6                 JOB                                          
LINJOBX  DS    CL8                                                              
LINTSKH  DS    CL8                                                              
LINTSK   DS    CL2                 TASK                                         
LINTSKX  DS    CL8                                                              
LINTAXH  DS    CL8                                                              
LINTAX   DS    CL2                 TAX?                                         
LINTAXX  DS    CL8                                                              
LINRATH  DS    CL8                                                              
LINRAT   DS    CL8                 RATE                                         
LINRATX  DS    CL8                                                              
LINAMTH  DS    CL8                                                              
LINAMT   DS    CL9                 AMOUNT                                       
LINAMTX  DS    CL8                                                              
LINADJH  DS    CL8                                                              
LINADJ   DS    CL1                 ADJUSTMENT INDICATOR                         
LINADJX  DS    CL8                                                              
LININCH  DS    CL8                                                              
LININC   DS    CL15                INCOME ACCOUNT                               
LININCX  DS    CL8                                                              
LINNMH   DS    CL8                                                              
LINNM    DS    CL7                 NAME                                         
LINNMX   DS    CL8                                                              
LINARRH  DS    CL8                                                              
LINARR   DS    CL78                NARRATIVE                                    
LINARRX  DS    CL8                                                              
LINLEN   EQU   *-LIND                                                           
LINNUMQ  EQU   13                  NUMBER OF FIELDS/SCREEN ENTRY                
         EJECT                                                                  
***********************************************************************         
*              DSECT FOR TAPE RECORD TABLE                            *         
***********************************************************************         
*                                                                               
SVTABLD  DSECT                                                                  
SVENTRY  DS    0C                                                               
SVCSLINE DS    XL1                 CURRENT ITEM NUMBER                          
SVTABN   DS    CL1                 (B)ILLABLE, (N)ON OR R(RATE)                 
SVHRS    DS    PL3                 HOURS                                        
SV1NACT  DS    CL15                1N ACCOUNT                                   
SV1NNAME DS    CL36                1N ACCOUNT NAME                              
SVSJACT  DS    0CL15                                                            
SVSJCMP  DS    CL1                 COMPANY                                      
SVSJCUL  DS    CL2                 UNIT/LEDGER                                  
SVSJCPJ  DS    0CL12               SJ CLI/PRO/JOB                               
SVSJCLI  DS    CL3                 CLIENT                                       
SVSJPRO  DS    CL3                 PRODUCT                                      
SVSJJOB  DS    CL6                 JOB                                          
SVSJCPN  DS    CL36                CLI/PRO NAME                                 
SVSJCPJN DS    CL36                CLI/PRO/JOB NAME                             
SVTSK    DS    CL2                 TASK CODE                                    
SVREF    DS    CL6                 REFERENCE NUMBER (SET IN POSTIT)             
SVINCACT DS    CL15                SVSIACCT(SUSPENSE) FOR BILLABLE              
SVINCACN DS    CL36                SVSIACCT ACCOUNT NAME                        
SVOFFICE DS    CL2                 UNIT FOR ANALYSIS                            
SV1CACT  DS    CL15                COSTING ACCOUNT CODE                         
SV1CNAME DS    CL36                COSTING ACCOUNT NAME                         
SVPCACT  DS    CL15                PROJECT CONTROL ACCOUNT                      
SV12ACT  DS    CL15                REVENUE A/C CODE (12)                        
SV12NAME DS    CL36                REVENUE A/C NAME                             
SVEFFD   DS    CL3                 EFFECTIVE DATE                               
SVAEFFD  DS    CL3                 EFFECTIVE DATE (ADJUSTEMENT RATE)            
SVRATE   DS    PL4                 HOURLY RATE                                  
SVTOTAL  DS    PL6                 TOTAL CHARGE                                 
SVTXLOC  DS    CL(L'GOTXLOC)       LOCALITY, IF TAXABLE                         
SVTXWC   DS    CL(L'GOTXWC)        TAXABLE WORKCODE                             
SVSTAT   DS    CL1                 STATUS                                       
SVEADJ   EQU   X'01'               RATE ELIGIBLE FOR ADJUSTMENT                 
SVWADJ   EQU   X'02'               RATE WAS ADJUSTED                            
SVTAX    EQU   X'04'               ITEM IS TAXABLE                              
SVXJOB   EQU   X'08'               JOB IS AN X-JOB                              
SVNARR   DS    CL76                1 BYTE LENGTH, THEN NARRATIVE                
SVICA    DS    CL(L'GOICA)         INPUT CREDIT ACCOUNT                         
SVLNQ EQU      *-SVTABLD                                                        
         EJECT                                                                  
***********************************************************************         
*              DSECT FOR TAX OVERLAY INTERFACE                        *         
***********************************************************************         
*                                                                               
STXD     DSECT                                                                  
       ++INCLUDE ACBATSTAX                                                      
         EJECT                                                                  
***********************************************************************         
*              DSECTS                                                 *         
***********************************************************************         
         PRINT OFF                                                              
       ++INCLUDE ACBATDSECT                                                     
         PRINT  ON                                                              
         ORG   BASOLY2H                                                         
       ++INCLUDE ACBATCCD                                                       
         EJECT                                                                  
*ACGENBOTH                                                                      
*ACGENDAY                                                                       
*DDFLDIND                                                                       
*ACPERCALLD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENDAY                                                       
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE ACPERCALLD                                                     
       ++INCLUDE FAUTL                                                          
       ++INCLUDE ACGETRATED                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'092ACBAT31   07/01/02'                                      
         END                                                                    

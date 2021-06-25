*          DATA SET ACBAT1B    AT LEVEL 045 AS OF 05/01/02                      
*PHASE T61B1BA                                                                  
*INCLUDE PERCALL                                                                
         TITLE 'TIME SHEETS'                                                    
T61B1B   CSECT                                                                  
         PRINT NOGEN                                                            
***********************************************************************         
*        TYPE 27, X'11' TIMESHEETS USES SCREEN        X'E3'           *         
*        TYPE 41, X'29' PROJECT TIME USES SCREEN      X'D2'           *         
***********************************************************************         
         NMOD1 PROGDX-PROGD,*BAT1B*,R7,CLEAR=YES,RR=R2                          
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
*        CLI   CUABIN,X'AB'        HKNY WANTS PROFILE1 SET ALWAYS               
*        BNE   *+8                                                              
*        MVI   PROFILE1,C'Y'       Y=ONLY SHOW CLIENT NAME                      
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
         SPACE 1                                                                
         USING CPYELD,R6                                                        
MAIN     LA    R6,BCCPYEL                                                       
*                                                                               
MAIN00   MVC   TSCPYST1,CPYSTAT1   COMPANY ELEMENT STATUS BYTE #1               
         MVC   TSCPYST2,CPYSTAT2   COMPANY ELEMENT STATUS BYTE #2               
         MVC   TSCPYST3,CPYSTAT3   COMPANY ELEMENT STATUS BYTE #3               
         MVC   TSCPYST4,CPYSTAT4   COMPANY ELEMENT STATUS BYTE #4               
         CLI   CPYLN,CPYLN2Q                                                    
         BL    MAIN10                                                           
         MVC   TSCPYST5,CPYSTAT5   COMPANY ELEMENT STATUS BYTE #5               
         MVC   TSCPYST6,CPYSTAT6   COMPANY ELEMENT STATUS BYTE #6               
         MVC   TSCPYST7,CPYSTAT7   COMPANY ELEMENT STATUS BYTE #7               
         MVC   SVTSD,CPYTSD        FORCE TIMESHEET DAY                          
*                                                                               
MAIN10   DS    0H                                                               
*        TM    TSCPYST7,CPYSJTIM   TURN OFF PROJECT CONTROL IF POST SJ          
*        BNO   *+8                                                              
*        NI    TSCPYST3,X'FF'-CPYSPC1C-CPYSPCSJ                                 
         TM    TSCPYST3,CPYSPC1C+CPYSPCSJ          X'24'                        
         BZ    *+8                                                              
         BAS   RE,GET1C                                                         
         DROP  R6                                                               
*                                                                               
         BAS   RE,READ1R           GET HEIRARCHY FOR 1R                         
         BAS   RE,READ1C           GET HEIRARCHY FOR 1C                         
         GOTO1 VDATCON,BOPARM,(5,0),(1,TODAYR)                                  
         EJECT                                                                  
***********************************************************************         
*              HANDLE ITEM LIST/CHA/DEL OR ITEM INPUT                 *         
***********************************************************************         
*                                                                               
INIT0    DS    0H                                                               
         CLI   CSACT,ACTINP        TEST ITEM/INPUT                              
         BNE   *+12                                                             
         MVI   CSOLINE,0           INITIALIZE ITEM COUNT FOR ADDITE             
         B     INITX                                                            
*                                                                               
         USING LIND,R4                                                          
         LA    R4,TIMDATAH         FIRST LINE                                   
         MVI   LINE,1                                                           
INIT100  CLC   LINE,CSOLINE                                                     
         BNE   *+12                DONT CLEAR LINE TO BE CHANGED.               
         BAS   RE,TRANSMIT                                                      
         B     *+8                 TO GET REST OF SCREEN PROTECTED              
         BAS   RE,PROTECT                                                       
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
         OI    6(R4),X'80'         TRANSMIT                                     
         ZIC   R1,0(R4)            BUMP TO NEXT FIELD                           
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
         TM    TIMDEPH+4,X'20'     IF CHANGED THEN CLEAR                        
         BO    *+8                 DUPLICATE CHECK                              
         OI    STATUS2,RRDUP                                                    
*                                                                               
         LA    R2,TIMDEPH                                                       
         MVI   FVMINL,1            INPUT IS REQUIRED                            
         MVC   FVMAXL,LVL2Q        MAX = L'LEV1 + L'LEV2                        
         GOTO1 AFVAL,TIMDEPH                                                    
         BNE   EXIT                                                             
         MVC   BOWORK1,BCSPACES                                                 
*                                                                               
         LA    R5,IOKEY                                                         
         USING ACTRECD,R5                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(2),=C'1R'   VALIDATE U/L=1R OFFICE                       
         ZIC   R1,LVL1Q                                                         
         SH    R1,=H'1'                                                         
         EXMVC R1,ACTKACT,TIMDEP                                                
         GOTO1 AGETACT,0                                                        
         BNE   EXIT                                                             
*                                                                               
         TM    ACBSTAT,ACBSLOCK    ACCOUNT LOCKED?                              
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ACTLK)                                           
         B     EXIT                                                             
*                                                                               
         OC    ACSTAT5,ACSTAT5                                                  
         BZ    DEPT10                                                           
         TM    ACSTAT5,RSTSPRJB    PROD/JOB/TASK REQUIRED                       
         BZ    *+8                                                              
         OI    SCRREQ,FLDPRO+FLDJOB+FLDTSK                                      
         TM    ACSTAT5,RSTSPROD                                                 
         BZ    *+8                                                              
         OI    SCRREQ,FLDPRO                                                    
*                                                                               
DEPT10   MVC   BOWORK1(L'ACNAME),ACNAME                                         
         MVI   BOWORK1+L'ACNAME,C'/'                                            
*                                                                               
         TM    TSCPYST3,CPYSPC1C+CPYSPCSJ                                       
         BZ    DEPT20                                                           
         TM    ACSTAT3,X'10'                                                    
         BZ    DEPT20                                                           
         OI    STATUS,PC1R         PRJ/CNTL SET UP ON U/L=1R                    
         CLI   CSBTYP,BT27         NO PC=Y FOR TYPE 27                          
         BNE   DEPT20                                                           
         MVC   FVMSGNO,=AL2(FVFSET)                                             
         MVC   BASMSG,=CL60'EA#9999 Must Use Batch Type 41'                     
         B     EXIT                                                             
*                                                                               
*              VALIDATE OFFICE/DEPT                                             
*                                                                               
DEPT20   LA    R5,IOKEY            VALIDATE OFF/DEPT                            
         USING ACTRECD,R5                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(2),=C'1R'   VALIDATE U/L=1R OFF/DEPT                     
         ZIC   R1,TIMDEPH+5                                                     
         SH    R1,=H'1'                                                         
         EXMVC R1,ACTKACT,TIMDEP                                                
         GOTO1 AGETACT,0                                                        
         BNE   EXIT                                                             
*                                                                               
         TM    ACBSTAT,ACBSLOCK    ACCOUNT LOCKED?                              
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ACTLK)                                           
         B     EXIT                                                             
*                                                                               
         OC    ACSTAT5,ACSTAT5                                                  
         BZ    DEPT30                                                           
         TM    ACSTAT5,RSTSPRJB    PROD/JOB/TASK REQUIRED                       
         BZ    *+8                                                              
         OI    SCRREQ,FLDPRO+FLDJOB+FLDTSK                                      
         TM    ACSTAT5,RSTSPROD                                                 
         BZ    *+8                                                              
         OI    SCRREQ,FLDPRO                                                    
*                                                                               
DEPT30   MVC   BOWORK1+L'ACNAME+2(L'ACNAME),ACNAME                              
         GOTO1 VSQUASH,BOPARM,BOWORK1,L'BOWORK1                                 
         MVC   SVDPGRP,ACCOST      SAVE COSTING BYTE                            
         MVC   TIMDEPN,BOWORK1     DISPLAY OFFICE/DEPT NAME                     
         OI    TIMDEPNH+6,X'80'                                                 
*                                                                               
         TM    TSCPYST3,CPYSPC1C+CPYSPCSJ                                       
         BZ    DEPTX               THEN C IF PC SET UP ON U/L=1R                
         TM    ACSTAT3,X'10'                                                    
         BZ    DEPTX                                                            
         OI    STATUS,PC1R         PRJ/CNTL SET UP ON U/L=1R                    
         CLI   CSBTYP,BT27         NO PC=Y FOR TYPE 27                          
         BNE   DEPTX                                                            
         MVC   FVMSGNO,=AL2(FVFSET)                                             
         MVC   BASMSG,=CL60'EA#9999 Must Use Batch Type 41'                     
         B     EXIT                                                             
*                                                                               
DEPTX    DS    0H                                                               
         OI    TIMDEPH+4,X'20'                                                  
         EJECT                                                                  
***********************************************************************         
*              VALIDATE SUB-DEPARTMENT                                *         
***********************************************************************         
*                                                                               
SUBDVAL  DS    0H                                                               
         TM    TIMSDPH+4,X'20'     IF CHANGED THEN CLEAR                        
         BO    *+8                 DUPLICATE CHECK                              
         OI    STATUS2,RRDUP                                                    
*                                                                               
         LA    R2,TIMSDPH                                                       
         MVI   FVMINL,1            INPUT IS REQUIRED                            
         ZIC   R1,LVL3Q                                                         
         ZIC   R0,LVL2Q                                                         
         SR    R1,R0                                                            
         STC   R1,FVMAXL                                                        
         GOTO1 AFVAL,TIMSDPH                                                    
         BNE   EXIT                                                             
*                                                                               
         LA    R3,ACTKACT                                                       
         ZIC   R1,LVL2Q                                                         
         AR    R3,R1                                                            
         ZIC   R1,TIMSDPH+5                                                     
         SH    R1,=H'1'                                                         
         EXMVC R1,0(R3),TIMSDP                                                  
         GOTO1 AGETACT,0                                                        
         BNE   EXIT                                                             
*                                                                               
         TM    ACBSTAT,ACBSLOCK    ACCOUNT LOCKED?                              
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ACTLK)                                           
         B     EXIT                                                             
*                                                                               
         OC    ACSTAT5,ACSTAT5                                                  
         BZ    SUBD10                                                           
         TM    ACSTAT5,RSTSPRJB    PROD/JOB/TASK REQUIRED                       
         BZ    *+8                                                              
         OI    SCRREQ,FLDPRO+FLDJOB+FLDTSK                                      
         TM    ACSTAT5,RSTSPROD                                                 
         BZ    *+8                                                              
         OI    SCRREQ,FLDPRO                                                    
*                                                                               
SUBD10   CLI   ACCOST,C' '                                                      
         BNH   *+10                                                             
         MVC   SVDPGRP,ACCOST      SAVE COSTING BYTE                            
         MVC   TIMSDPN,ACNAME                                                   
         OI    TIMSDPNH+6,X'80'                                                 
*                                                                               
         TM    TSCPYST3,CPYSPC1C+CPYSPCSJ                                       
         BZ    SUBDX                                                            
         TM    ACSTAT3,X'10'                                                    
         BZ    SUBDX                                                            
         OI    STATUS,PC1R                                                      
         CLI   CSBTYP,BT27         NO PC=Y FOR TYPE 27                          
         BNE   SUBDX                                                            
         MVC   FVMSGNO,=AL2(FVFSET)                                             
         MVC   BASMSG,=CL60'EA#9999 Must Use Batch Type 41'                     
         B     EXIT                                                             
*                                                                               
SUBDX    DS    0H                                                               
         OI    TIMSDPH+4,X'20'                                                  
         EJECT                                                                  
***********************************************************************         
*              VALIDATE STAFF                                                   
***********************************************************************         
*                                                                               
STAFVAL  DS    0H                                                               
         TM    TIMSTFH+4,X'20'     IF CHANGED THEN CLEAR                        
         BO    *+8                 DUPLICATE CHECK                              
         OI    STATUS2,RRDUP                                                    
*                                                                               
         LA    R2,TIMSTFH                                                       
         MVI   FVMINL,1            INPUT IS REQUIRED                            
         ZIC   R1,LVL4Q                                                         
         ZIC   R0,LVL3Q                                                         
         SR    R1,R0                                                            
         STC   R1,FVMAXL                                                        
         GOTO1 AFVAL,TIMSTFH                                                    
         BNE   EXIT                                                             
*                                                                               
         LA    R3,ACTKACT                                                       
         ZIC   R1,LVL3Q                                                         
         AR    R3,R1                                                            
         ZIC   R1,TIMSTFH+5                                                     
         SH    R1,=H'1'                                                         
         EXMVC R1,0(R3),TIMSTF                                                  
         GOTO1 AGETACT,0                                                        
         BNE   EXIT                                                             
*                                                                               
         TM    ACBSTAT,ACBSLOCK    ACCOUNT LOCKED?                              
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ACTLK)                                           
         B     EXIT                                                             
         TM    ACBSTAT,ACBSABAL    VALID FOR POSTING?                           
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INACP)                                           
         B     EXIT                                                             
*                                                                               
STAF30   OC    ACSTAT5,ACSTAT5                                                  
         BZ    STAF40                                                           
         TM    ACSTAT5,RSTSPRJB    PROD/JOB/TASK REQUIRED                       
         BZ    *+8                                                              
         OI    SCRREQ,FLDPRO+FLDJOB+FLDTSK                                      
         TM    ACSTAT5,RSTSPROD                                                 
         BZ    *+8                                                              
         OI    SCRREQ,FLDPRO                                                    
*                                                                               
STAF40   CLI   ACCOST,C' '                                                      
         BNH   *+10                                                             
         MVC   SVDPGRP,ACCOST      SAVE COSTING BYTE                            
         MVC   TIMSTFN,ACNAME                                                   
         OI    TIMSTFNH+6,X'80'                                                 
         MVC   SV1RACT,ACCODE                                                   
         MVC   SV1RNAME,ACNAME                                                  
*                                                                               
         MVC   SV1ROFFC,BCSPACES   ISOLATE 1R OFFICE CODE                       
         TM    TSCPYST4,CPYSOFF2                                                
         BO    *+14                                                             
         MVC   SV1ROFFC(1),SV1RACT+3  OFFICE IS 1ST BYTE OF ACCT                
         B     STAF50                 FOR CMPYS ON OLD OFFICES                  
         NI    SV1ROFFP,X'0F'         ISOLATE OFFICE POSITION                   
         BNZ   *+20                                                             
         MVC   FVMSGNO,=AL2(FVFSET)                                             
         MVC   BASMSG,=CL60'EA#9999 Bad 1R Ledger - Call DDS'                   
         B     EXIT                                                             
         LA    R1,SV1RACT                                                       
         ZIC   RE,SV1ROFFP                                                      
         LA    R1,2(RE,R1)                                                      
         MVC   SV1ROFFC,0(R1)      SAVE 2 BYTE OFFICE CODE                      
*                                                                               
STAF50   DS    0H                                                               
         TM    TSCPYST3,CPYSPC1C+CPYSPCSJ                                       
         BZ    STAF100             THEN C IF PC SET UP ON U/L=1R                
         TM    ACSTAT3,X'10'                                                    
         BZ    STAF100                                                          
         OI    STATUS,PC1R         PRJ/CNTL SET UP ON U/L=1R                    
         CLI   CSBTYP,BT27         NO PC=Y FOR TYPE 27                          
         BNE   STAF100                                                          
         MVC   FVMSGNO,=AL2(FVFSET)                                             
         MVC   BASMSG,=CL60'EA#9999 Must Use Batch Type 41'                     
         B     EXIT                                                             
*                                                                               
STAF100  DS    0H                                                               
         XC    SVHIRE,SVHIRE                                                    
         XC    SVTERM,SVTERM                                                    
*                                                                               
         USING ACTRECD,R5                                                       
         L     R5,AIO1                                                          
         LA    R5,ACTRFST                                                       
         SR    RE,RE                                                            
STAF125  CLI   0(R5),0                                                          
         BE    STAFX                                                            
         CLI   0(R5),X'56'         EMPLOYEE ELEMENT                             
         BE    *+14                                                             
         IC    RE,1(R5)                                                         
         AR    R5,RE                                                            
         B     STAF125                                                          
*                                                                               
         USING ACEMPD,R5                                                        
         MVC   SVHIRE,ACEMPHIR     SAVE HIRE DATE                               
         MVC   SVTERM,ACEMPTRM     SAVE TERMINATION DATE                        
*                                                                               
STAFX    DS    0H                                                               
         OI    TIMSTFH+4,X'20'     MARK AS VALIDATED                            
         EJECT                                                                  
***********************************************************************         
*              VALIDATE DEPT GROUP                                    *         
***********************************************************************         
*                                                                               
GRPVAL   DS    0H                                                               
         LA    R5,IOKEY                                                         
         USING ACTRECD,R5                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(2),=C'14'   VALIDATE DEPT GROUP                          
         MVC   ACTKACT(1),SVDPGRP  (TAKEN FROM ANALYSIS=ON DPT/SUB/STF)         
         GOTO1 AGETACT,0                                                        
         BNE   EXIT                                                             
         CLI   SVDPGRP,C' '                                                     
         BNE   GRPX                                                             
         MVC   FVMSGNO,=AL2(FVFSET)                                             
         MVC   BASMSG,=CL60'EA#9999 - Invalid Account 14'                       
         B     EXIT                                                             
*                                                                               
GRPX     DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              VALIDATE DATE                                          *         
***********************************************************************         
*                                                                               
DATEVAL  DS    0H                                                               
         TM    TIMDATH+4,X'20'     IF DATE HAS CHANGED THEN CLEAR               
         BO    *+8                 DUPLICATE CHECK                              
         OI    STATUS2,RRDUP                                                    
*                                                                               
         LA    R2,TIMDATH                                                       
         OC    SVTSD,SVTSD         DATE IS REQUIRED IF                          
         BZ    DATE50              'FORCE TIMESHEET DAY' OPTION                 
         MVI   FVMINL,1            IS SET ON COMPANY RECORD (TSD=XXX)           
         GOTO1 AFVAL,TIMDATH                                                    
         BNE   EXIT                                                             
*                                                                               
DATE50   CLI   TIMDATH+5,0         USE ENTERED DATE OR TODAY                    
         BNE   DATE75                                                           
         GOTO1 VDATCON,BOPARM,(5,0),(0,BOWORK1)                                 
         GOTO1 VDATCON,BOPARM,(5,0),(5,TEMPT)                                   
         B     DATE80                                                           
*                                                                               
         USING PERVALD,R6                                                       
DATE75   LA    R6,BOWORK1                                                       
         GOTO1 VPERVAL,BOPARM,(TIMDATH+5,TIMDAT),(X'60',BOWORK1)                
         CLI   4(R1),X'04'                                                      
         BNE   DATE125                                                          
         GOTO1 VDATCON,BOPARM,(1,PVALPSTA),(5,TEMPT)                            
DATE80   MVC   TIMDAT(8),TEMPT                                                  
         MVI   TIMDATH+5,8                                                      
         OI    TIMDATH+6,X'80'                                                  
*                                                                               
DATE100  DS    0H                                                               
         GOTO1 VDATVAL,BOPARM,(0,8(R2)),BOWORK1                                 
         OC    BOPARM(4),BOPARM                                                 
         BNZ   *+14                                                             
DATE125  MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXIT                                                             
         CLC   BOWORK1(2),=C'85'                                                
         BH    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXIT                                                             
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
         CLC   CPYTMSSD,BOWORK2    TMS START MUST BE HIGH TO USE TY41           
         BNH   EXIT                                                             
         DROP  R5                                                               
*                                                                               
DATE200  OC    SVTSD,SVTSD                                                      
         BZ    DATE250                                                          
         GOTO1 VGETDAY,BOPARM,(0,BOWORK1),BOWORK2                               
         CLC   SVTSD,BOPARM                                                     
         BE    DATE250                                                          
         MVC   FVMSGNO,=AL2(FVFSET)                                             
         MVC   BASMSG,=CL60'EA#9999 - Timesheet Date Is Incorrect Day OX        
               f The Week'                                                      
         B     EXIT                                                             
*                                                                               
DATE250  GOTO1 VDATCON,BOPARM,(0,BOWORK1),(1,TODAY3)                            
         CLI   PROFILE2,C'Y'       ALLOW TS FORWARD 1 YEAR                      
         BNE   DATE260                                                          
         GOTO1 VDATCON,BOPARM,(1,TODAYR),(0,TEMPT)                              
         GOTO1 VADDAY,BOPARM,(C'Y',TEMPT),TEMPT+6,1                             
         GOTO1 VDATCON,BOPARM,(0,TEMPT+6),(1,TEMPT)                             
         CLC   TODAY3,TEMPT                                                     
         BNH   DATE275                                                          
         MVC   FVMSGNO,=AL2(FVFSET)                                             
         MVC   BASMSG,=CL60'EA#9999 Date Cannot Be Greater Than 1 Year X        
               From Today'                                                      
         B     EXIT                                                             
*                                                                               
DATE260  CLC   TODAY3,TODAYR                                                    
         BNH   *+20                                                             
         MVC   FVMSGNO,=AL2(FVFSET)                                             
         MVC   BASMSG,=CL60'EA#9999 Timesheet Date Must Not Be Greater X        
               Than Today'                                                      
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
         MVC   FVMSGNO,=AL2(FVFSET)                                             
         MVC   BASMSG,=CL60'EA#9999 Date Outside Of Location Start/End X        
               Range'                                                           
         B     EXIT                                                             
*                                                                               
DATEERR2 DS    0H                                                               
         MVC   FVMSGNO,=AL2(FVFSET)                                             
         MVC   BASMSG,=CL60'EA#9999 Date Outside Of Hire/Term Range'            
         B     EXIT                                                             
*                                                                               
DATE300  DS    0H                                                               
         OI    TIMDATH+4,X'20'                                                  
         EJECT                                                                  
***********************************************************************         
*              VALIDATE MISS/ADJ FIELD                                *         
***********************************************************************         
*                                                                               
MISS     DS    0H                                                               
         MVI   TYPETIME,TRSSTIME   REGULAR TIME                                 
         LA    R2,TIMADJH          ANYTHING INPUT IN MISS/ADJ FIELD?            
         CLI   TIMADJH+5,0                                                      
         BE    MISS100                                                          
         CLI   TIMADJ,C'T'                                                      
         BE    MISS100                                                          
         CLI   TIMADJ,C'A'         ADJUSTMENT TYPE = X'08'                      
         BNE   *+12                                                             
         MVI   TYPETIME,TRSSTADJ                                                
         B     MISS100                                                          
         CLI   TIMADJ,C'M'         MISSING TYPE = X'04'                         
         BNE   *+12                                                             
         MVI   TYPETIME,TRSSTMSS                                                
         B     MISS100                                                          
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXIT                                                             
*                                                                               
MISS100  DS    0H                                                               
         OI    TIMADJH+4,X'20'                                                  
*                                                                               
         OC    SVTERM,SVTERM       ALL TIME ALLOWED IF TSD < TERM DATE          
         BZ    MISSX                                                            
         CLC   TODAY3,SVTERM                                                    
         BNH   MISSX                                                            
         TM    TYPETIME,TRSSTADJ   ONLY 'A' TIME IF TSD > TERM DATE             
         BO    MISSX                                                            
         MVC   FVMSGNO,=AL2(FVFSET)                                             
         MVC   BASMSG,=CL60'EA#9999 Only "A" Time Allowed If TSD > TermX        
               ination Date'                                                    
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
DUP50    LA    R2,TIMDUPH                                                       
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
DUP400   MVC   FVMSGNO,=AL2(FVFSET)                                             
         MVC   BASMSG,BCSPACES                                                  
         MVC   BASMSG(60),=CL60'EA#9999 Duplicate Timesheet Date Found X        
               - "+" Will Override'                                             
         B     EXIT                                                             
*                                                                               
DUPX     DS    0H                                                               
         OI    TIMDUPH+4,X'20'                                                  
         EJECT                                                                  
***********************************************************************         
*              ANY INPUT BETWEEN TRANSACTIONS                         *         
***********************************************************************         
*                                                                               
CHKSCR   DS    0H                                                               
         CLI   CSACT,ACTDEL        SKIP THIS ROUTINE IF ACTION DELETE           
         BE    CHKX                                                             
         CLI   CSACT,ACTDSP        SKIP THIS ROUTINE IF ACTION DISPLAY          
         BE    CHKX                                                             
*                                                                               
         USING LIND,R4                                                          
         LA    R4,TIMDATAH         FIRST LINE                                   
*                                                                               
CHK50    ST    R4,BODUB1           SAVE A(CURRENT LINE)                         
         LA    R0,LINNUMQ          #INPUT LINES PER LINE                        
         MVI   NEWINPUT,0                                                       
*                                                                               
CHK100   CLI   5(R4),0             ANY INPUT INTO FIELD?                        
         BE    CHK110                                                           
         TM    1(R4),X'20'         PROTECTED                                    
         BO    CHK110                                                           
         TM    4(R4),X'20'         PREVIOUSLY VALIDATED                         
         BO    CHK110                                                           
         MVI   NEWINPUT,X'FF'      SOMETHING NEW WAS INPUT                      
CHK110   ZIC   R1,0(R4)            BUMP TO NEXT FIELD                           
         AR    R4,R1                                                            
         BCT   R0,CHK100                                                        
*                                                                               
*              CLEAR LINES WITH NO INPUT                                        
*                                                                               
         L     R4,BODUB1           GET A(CURRENT LINE)                          
         LA    R0,LINNUMQ          #INPUT FIELDS/LINE                           
CHK200   NI    4(R4),X'FF'-X'20'   TURN OFF PREVIOUSLY VALIDATED                
         CLI   NEWINPUT,X'FF'                                                   
         BE    CHK220                                                           
         MVI   5(R4),0             SET L'INPUT TO ZERO                          
         ZIC   R1,0(R4)            GET L'HEADER+L'FIELD+L'EXTENDED HEAD         
         SH    R1,=Y(L'LINHRH+L'LINHRX+1)                                       
         EXMVC R1,8(R4),BCSPACES                                                
         OI    6(R4),X'80'         TRANSMIT                                     
CHK220   ZIC   R1,0(R4)            BUMP TO NEXT FIELD                           
         AR    R4,R1                                                            
         BCT   R0,CHK200                                                        
*                                                                               
CHK300   L     R4,BODUB1                                                        
         LA    R4,LINLEN(R4)       NEXT LINE                                    
         LA    R2,TIMENDH                                                       
         CR    R4,R2               BOTTOM OF SCREEN YET?                        
         BL    CHK50                                                            
*                                                                               
CHKX     DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              VALIDATE INPUT LINE                                    *         
***********************************************************************         
*                                                                               
VALIDATE DS    0H                                                               
         LA    R1,TIMENDH          CLEAR OUT ALL NAMES                          
         LA    R4,TIMDATAH                                                      
         USING LIND,R4                                                          
         MVC   LINNM,BCSPACES                                                   
         OI    LINNMH+6,X'80'                                                   
         LA    R4,LINLEN(R4)                                                    
         CR    R4,R1                                                            
         BL    *-16                                                             
*                                                                               
         LA    R8,SVTABL           START AT BEGINNING OF TABLE                  
         USING SVTABLD,R8                                                       
         LA    R4,TIMDATAH         AND BEGINNING OF SCREEN                      
         USING LIND,R4                                                          
         MVI   NEWINPUT,0          ASSUME NOTHING WAS INPUT                     
*                                                                               
*              INITIALIZE FOR VALIDATION LOOP                                   
*                                                                               
CPJ100   XC    SVENTRY(SVLNQ),SVENTRY     CLEAR TABLE ENTRY                     
         XC    FLDSTAT,FLDSTAT            CLEAR FIELDS INDICATORS               
         OI    FLDREQ,FLDHRS+FLDCLI       HOURS & CLIENT REQUIRED               
         NI    STATUS,X'FF'-PCSJ          TURN OFF CLI/PRD PRJ/CNTL             
         TM    STATUS,PC1R                PROJECT CONTROL SET UP ON 1R          
         BZ    *+8                                                              
         OI    SCRREQ,FLDPRO+FLDJOB+FLDTSK                                      
*                                                                               
         CLI   CSOMODE,CSOMDCHA    IF DOING AN ITEM/CHANGE THEN                 
         BE    CPJ200              DONT BUMP LINE NUMBER                        
         ZIC   R1,CSOLINE                                                       
         LA    R1,1(R1)                                                         
         STC   R1,CSOLINE                                                       
CPJ200   MVC   SVCSLINE,CSOLINE                                                 
         MVI   SVENTRY+SVLNQ,X'FF' MARK END OF TABLE                            
*                                                                               
*              DETERMINE WHICH FIELDS HAVE INPUT                                
*                                                                               
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
         OC    FLDINPT,FLDINPT     WAS ANYTHING INPUT ON THIS LINE?             
         BZ    EOL100                                                           
         MVI   NEWINPUT,X'FF'      INDICATE THAT SOMETHING WAS INPUT            
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
         ZIC   R3,5(R2)                                                         
         GOTO1 AAMTVAL,BOPARM,8(R2),(R3)                                        
         CLI   0(R1),X'FF'                                                      
         BE    HRSERR              ERROR IN HOUR FIELD                          
         L     RF,BOPARM+4                                                      
         ZAP   BODUB1,0(8,RF)                                                   
         CP    BODUB1,=P'0'                                                     
         BE    HRSERR                                                           
         ZAP   SVHRS,BODUB1        NO ERRORS SO SAVE HOURS                      
         DP    BODUB1,=P'25'       MUST BE 1/4 HRS                              
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
         LA    R2,LINCLIH                                                       
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,LINCLIH                                                    
         BNE   EXIT                                                             
*                                                                               
         TM    FLDINPT,FLDPRO+FLDJOB+FLDTSK                                     
         BNZ   NCLIX                                                            
*                                                                               
         LA    R5,IOKEY                                                         
         USING ACTRECD,R5                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(2),=C'1N'   TRY TO VALIDATE U/L=1N ACCOUNT               
         ZIC   R1,LINCLIH+5        EX/ 1NVACATION                               
         SH    R1,=H'1'                                                         
         EXMVC R1,ACTKACT,LINCLI                                                
         GOTO1 AGETACT,0                                                        
         BE    NCLI50                                                           
         CLI   LINCLIH+5,L'SVSJCLI IF L'INPUT <=3 THEN MIGHT BE                 
         BH    EXIT                A CLIENT BUT IF >3 IT'S A 1N ACCT            
         B     NCLIX                                                            
*                                                                               
NCLI50   MVC   SV1NACT,ACCODE                                                   
         TM    TSCPYST7,CPYSL1NA   IF ANALYSIS=X ON '1NVACATION'                
         BO    NCLI110             THEN MAKE ACCT '1NX VACATION'                
         CLI   ACCOST,C' '         UNLESS ON NEWCOST                            
         BE    NCLI110                                                          
         MVC   BOWORK1(12),SV1NACT+3                                            
         MVC   SV1NACT+3(12),BCSPACES                                           
         MVC   SV1NACT+3(1),ACCOST                                              
         MVC   SV1NACT+5(10),BOWORK1                                            
*                                                                               
NCLI110  TM    ACBSTAT,ACBSLOCK    ACCOUNT LOCKED?                              
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ACTLK)                                           
         B     EXIT                                                             
         TM    ACBSTAT,ACBSABAL    VALID FOR POSTING?                           
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INACP)                                           
         B     EXIT                                                             
*                                                                               
         MVC   SV1NNAME,ACNAME                                                  
         MVC   LINNM,ACNAME                                                     
         OI    LINNMH+6,X'80'                                                   
         OI    FLDINPT,FLD1NACT                                                 
         OI    FLDINV,FLDPRO+FLDJOB+FLDTSK                                      
         NI    FLDREQ,X'FF'-FLDPRO-FLDJOB-FLDTSK                                
         B     PROVAL                                                           
*                                                                               
NCLIX    DS    0H                                                               
         MVC   FVXTRA,BCSPACES                                                  
         EJECT                                                                  
***********************************************************************         
*              VALIDATE SJ CLIENT CODE                                *         
***********************************************************************         
*                                                                               
CLIVAL   DS    0H                                                               
         CLC   CUAALF,=C'OD'       ***HARDCODE FOR O&M DIRECT                   
         BNE   *+8                                                              
         OI    FLDREQ,FLDPRO+FLDJOB+FLDTSK                                      
*                                                                               
         OC    FLDREQ,SCRREQ       SET REQUIRED FIELDS FROM 1R                  
*                                  ACCOUNT PROFILES                             
         LA    R2,LINCLIH                                                       
         MVI   FVMINL,1                                                         
         MVI   FVMAXL,L'SVSJCLI                                                 
         GOTO1 AFVAL,LINCLIH                                                    
         BNE   EXIT                                                             
*                                                                               
         MVC   SVSJCPJ,BCSPACES                                                 
         ZIC   R1,LINCLIH+5                                                     
         SH    R1,=H'1'                                                         
         EXMVC R1,SVSJCLI,LINCLI                                                
         MVI   BOFLAG1,ACIPRCLI    VALIDATE CLIENT                              
         XC    PSCLICOD,PSCLICOD   INSURE CLIREC IS REREAD                      
         GOTO1 AVALCPJ,LINCLIH                                                  
         BNE   EXIT                                                             
*                                                                               
*        TM    ACBSTAT,ACBSLOCK    ACCOUNT LOCKED?                              
*        BZ    *+14                                                             
*        MVC   FVMSGNO,=AL2(AE$ACTLK)                                           
*        B     EXIT                                                             
*                                                                               
         MVC   LINNM,ACNAME                                                     
         OI    LINNMH+6,X'80'                                                   
*                                                                               
         TM    TSCPYST3,CPYSPC1C+CPYSPCSJ                                       
         BZ    CLIX                                                             
         TM    ACSTAT3,X'10'       THEN C IF CLIENT MARKED FOR PRJ/CNTL         
         BZ    CLIX                                                             
         CLI   CSBTYP,BT27         NO PC=Y FOR TYPE 27                          
         BNE   *+20                                                             
         MVC   FVMSGNO,=AL2(FVFSET)                                             
         MVC   BASMSG,=CL60'EA#9999 Must Use Batch Type 41'                     
         B     EXIT                                                             
         OI    STATUS,PCSJ                                                      
         OI    FLDREQ,FLDPRO+FLDJOB+FLDTSK                                      
*                                                                               
CLIX     DS    0H                                                               
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
         BZ    PRO100                                                           
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXIT                                                             
*                                                                               
PRO100   ZIC   R1,LINPROH+5                                                     
         SH    R1,=H'1'                                                         
         BM    PROX                                                             
         EXMVC R1,SVSJPRO,LINPRO                                                
         MVI   BOFLAG1,ACIPRPRO    VALIDATE PRODUCT                             
         XC    PSPROCOD,PSPROCOD   INSURE PRODREC IS REREAD                     
         GOTO1 AVALCPJ,LINPROH                                                  
         BNE   EXIT                                                             
*                                                                               
*        TM    ACBSTAT,ACBSLOCK    ACCOUNT LOCKED?                              
*        BZ    *+14                                                             
*        MVC   FVMSGNO,=AL2(AE$ACTLK)                                           
*        B     EXIT                                                             
*                                                                               
         MVC   LINNM,ACNAME                                                     
         OI    LINNMH+6,X'80'                                                   
*                                                                               
         TM    TSCPYST3,CPYSPC1C+CPYSPCSJ                                       
         BZ    PROX                                                             
         TM    ACSTAT3,X'10'       THEN C IF CLIENT MARKED FOR PRJ/CNTL         
         BZ    PROX                                                             
         CLI   CSBTYP,BT27         NO PC=Y FOR TYPE 27                          
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXIT                                                             
         OI    STATUS,PCSJ                                                      
         OI    FLDREQ,FLDJOB+FLDTSK                                             
*                                                                               
PROX     DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              VALIDATE JOBCODE                                       *         
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
         BZ    JOB100                                                           
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
         MVI   BOFLAG1,ACIPRJOB    VALIDATE JOB                                 
         XC    PSJOBCOD,PSJOBCOD   INSURE JOBREC IS REREAD                      
         GOTO1 AVALCPJ,LINJOBH                                                  
         BNE   EXIT                                                             
*                                                                               
*        TM    ACBSTAT,ACBSLOCK    JOB LOCKED?                                  
*        BZ    *+14                                                             
*        MVC   FVMSGNO,=AL2(AE$ACTLK)                                           
*        B     EXIT                                                             
*                                                                               
         TM    ACBSTAT,ACBSCLSE    JOB CLOSED?                                  
         BZ    JOB300                                                           
         CP    SVHRS,=P'0'         IF JOB IS CLOSED BUT AMOUNT IS               
         BNL   *+12                IT'S A -ADJUSTMENT THEN OK                   
         TM    TYPETIME,TRSSTADJ                                                
         BO    JOB300                                                           
         MVC   FVMSGNO,=AL2(AE$JBCLO)                                           
         B     EXIT                                                             
*                                                                               
JOB300   MVC   LINNM,ACNAME                                                     
         OI    LINNMH+6,X'80'                                                   
         MVC   SVSJCPJN,ACNAME     SAVE SJ C/P/J NAME                           
         OI    FLDREQ,FLDTSK       TASK REQUIRED IF JOB IS INPUT                
*                                                                               
JOBX     DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              VALIDATE 1C COST ACCOUNT                               *         
***********************************************************************         
*                                                                               
AC1CVAL  DS    0H                                                               
         TM    FLDINPT,FLD1NACT    DONT VALIDATE COST ACCT IF 1NACCT            
         BO    AC1CX                                                            
*                                                                               
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
         GOTO1 AGETACT,0                                                        
         BNE   EXIT                                                             
         TM    ACBSTAT,ACBSLOCK    ACCOUNT LOCKED?                              
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ACTLK)                                           
         B     EXIT                                                             
         TM    ACBSTAT,ACBSABAL    VALID FOR POSTING?                           
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INACP)                                           
         B     EXIT                                                             
         DROP  R5                                                               
*                                                                               
         MVC   SV1CACT,ACCODE      VALIDATE 1C COST ACCOUNT                     
         MVC   SV1CNAME,ACNAME                                                  
*                                                                               
         LA    R5,4                # TIMES TO LOOP                              
         LA    R6,LVL4CST          1C HIERARCHY                                 
*                                                                               
AC1C100  SR    R1,R1                                                            
         ICM   R1,1,0(R6)                                                       
         BZ    AC1C300                                                          
         MVC   IOKEY,BCSPACES                                                   
         LA    R1,2(R1)                                                         
         EX    R1,*+4                                                           
         MVC   IOKEY(0),SV1CACT                                                 
         GOTO1 AGETACT,0                                                        
         BNE   AC1C300                                                          
*                                                                               
         OC    ACSTAT5,ACSTAT5     IF NO STATUS BYTE SET UP GO UP TO            
         BZ    AC1C300             NEXT HIGHEST LEVEL OF ACCOUNT                
         TM    ACSTAT5,RSTSPROD+RSTSPRJB                                        
         BZ    AC1C200                                                          
         LA    R2,LINPROH                                                       
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,LINPROH       PRODUCT REQUIRED ON 1C ACCT PROFILE          
         BH    EXIT                                                             
*                                                                               
AC1C200  TM    ACSTAT5,RSTSPRJB                                                 
         BZ    AC1C300                                                          
         LA    R2,LINJOBH                                                       
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,LINJOBH       JOB REQUIRED ON 1C ACCT PROFILE              
         BH    EXIT                                                             
         LA    R2,LINTSKH                                                       
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,LINTSKH       TASK REQUIRED IF JOB INPUT                   
         BH    EXIT                                                             
         B     AC1CX                                                            
*                                                                               
AC1C300  BCTR  R6,0                DROP BACK A LEVEL IN HIERARCH TABLE          
         BCT   R5,AC1C100                                                       
*                                                                               
AC1CX    DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              CHECK OPT/MAINT PROFILE                                *         
***********************************************************************         
*                                                                               
OPMTVAL  DS    0H                                                               
         TM    FLDINPT,FLD1NACT    DONT VALIDATE IF 1NACCT                      
         BO    OPMTX                                                            
*                                                                               
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
         MVC   GOSELCLI(L'SVSJCLI),SVSJCLI                                      
         MVC   GOAEXT,AGOXBLK      EXTENSION BLOCK                              
         CLC   SVSJPRO,BCSPACES                                                 
         BNH   OPMT100                                                          
         MVC   GOSELPRO(L'SVSJPRO),SVSJPRO                                      
         OC    GOSELPRO,BCSPACES                                                
         CLC   SVSJJOB,BCSPACES                                                 
         BNH   OPMT100                                                          
         MVC   GOSELJOB(L'SVSJJOB),SVSJJOB                                      
         OC    GOSELJOB,BCSPACES                                                
*                                                                               
OPMT100  DS    0H                                                               
         GOTO1 VGETOPT,BCPARM,AGOPBLK                                           
         L     R2,AGOXBLK                                                       
         USING GOXBLKD,R2                                                       
         CLC   GOTOT,BCSPACES                                                   
         BNH   OPMTX                                                            
         CLI   GOTOT,C'N'          ALLOW ONLY N TIME                            
         BE    OPMTX                                                            
*                                                                               
         MVC   FVMSGNO,=AL2(FVFSET)                                             
         MVC   BASMSG,=CL60'EA#9999 Opt/Maint Profile Not N-Time'               
         MVC   FVXTRA,BCSPACES                                                  
         LA    R2,LINJOBH                                                       
         TM    FLDINPT,FLDJOB                                                   
         BO    EXIT                                                             
         LA    R2,LINPROH                                                       
         TM    FLDINPT,FLDPRO                                                   
         BO    EXIT                                                             
         LA    R2,LINCLIH                                                       
         B     EXIT                                                             
*                                                                               
OPMTX    DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              VALIDATE TASK CODE                                     *         
***********************************************************************         
*                                                                               
TSKVAL   DS    0H                                                               
         CLI   CSBTYP,BT27                                                      
         BE    EOLVAL                                                           
         LA    R2,LINTSKH                                                       
         TM    FLDREQ,FLDTSK       IF INPUT REQUIRED                            
         BZ    *+8                                                              
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,LINTSKH                                                    
         BH    EXIT                                                             
*                                                                               
         TM    FLDINV,FLDTSK       IF INPUT NOT ALLOWED                         
         BZ    TSK100                                                           
         TM    FLDINPT,FLDTSK      THEN FLAG ERROR IF SOMETHING INPUT           
         BZ    TSK100                                                           
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXIT                                                             
*                                                                               
TSK100   TM    FLDINPT,FLDTSK                                                   
         BZ    TSKX                                                             
         TM    FLDINPT,FLDCLI+FLDPRO+FLDJOB                                     
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXIT                                                             
*                                                                               
         TM    STATUS,PC1R+PCSJ                                                 
         BZ    TSK150                                                           
         LA    R5,IOKEY                                                         
         USING WCORECD,R5                                                       
         MVC   WCOKEY,BCSPACES                                                  
         MVI   WCOKTYP,WCOKTYPQ    X'0A' RECORD                                 
         MVC   WCOKCPY,CUABIN                                                   
         MVC   WCOKUNT(2),=C'1J'   ALWAYS CHECK 1J WORKCODE IF 1R OR SJ         
         ZIC   R1,LINTSKH+5        MARKED WITH PROJECT CONTROL                  
         SH    R1,=H'1'                                                         
         BM    TSKX                                                             
         EXMVC R1,WCOKWRK,LINTSK                                                
         MVC   SVTSK,WCOKWRK                                                    
         GOTO1 AIO,IOREAD+IOACCMST+IO1                                          
         BE    *+20                                                             
         MVC   FVMSGNO,=AL2(FVFSET)                                             
         MVC   BASMSG,=CL60'EA#9999 Invalid 1j Task Code'                       
         B     EXIT                                                             
*                                                                               
TSK150   DS    0H                                                               
         TM    TSCPYST7,CPYSJTIM   IF NEWCOST THEN MUST VALIDATE SJ TSK         
         BO    *+12                                                             
         TM    STATUS,PC1R+PCSJ    IF OLDCOST & PC=Y THEN SKIP SJ TSK           
         BNZ   TSKX                                                             
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
         BE    *+20                                                             
         MVC   FVMSGNO,=AL2(FVFSET)                                             
         MVC   BASMSG,=CL60'EA#9999 Invalid Task Code'                          
         B     EXIT                                                             
*                                                                               
TSKX     DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              VALIDATE PRJ/CNTL ACCOUNT - MIMICS EITHER SJ OR 1C     *         
***********************************************************************         
*                                                                               
PRJVAL   DS    0H                                                               
         TM    TSCPYST3,CPYSPC1C+CPYSPCSJ                                       
         BZ    PRJX                                                             
         TM    FLDINPT,FLDJOB                                                   
         BZ    PRJX                                                             
         TM    STATUS,PC1R+PCSJ    WAS PC=Y ON ANY LEVEL OF 1R OR SJ            
         BZ    PRJX                                                             
         OC    SV1NACT,SV1NACT     NO PROJECT CONTROL IF 1N POSTING             
         BNZ   PRJX                                                             
*                                                                               
PRJ050   LA    R5,IOKEY                                                         
         USING ACTRECD,R5                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(2),=C'1J'                                                
         MVC   ACTKACT(L'SVSJCPJ),SVSJCPJ                                       
         TM    BCCPYEL+(CPYSTAT3-CPYELD),X'04'      PC=SJ?                      
         BO    PRJ100                                                           
         LA    R3,SV1CACT+2        CLIENT PRODUCT FROM 1C                       
         ZIC   R1,SVCLIPOS                                                      
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
         MVC   IOKEY,BCSPACES                                                   
         USING TRNRECD,R5                                                       
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
         MVC   FVMSGNO,=AL2(FVFSET)                                             
         MVC   BASMSG,=CL60'EA#9999 Negative Hours May Not Exceed TotalX        
                Hours On Job'                                                   
         B     EXIT                                                             
*                                                                               
PRV225   LA    R5,TRNRFST                                                       
         USING TRNELD,R5                                                        
         CLI   TRNEL,TRNELQ        X'44' ELEMENT - LOOK FOR TRNSACTNS           
         BNE   PRV100                                                           
         CLI   TRNTYPE,BT27        ELIMINATE NON TIMESHEET BATCHES              
         BE    PRV250                                                           
         CLI   TRNTYPE,BT34        JOB TO JOB TRANSFERS                         
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
*              END OF LINE                                            *         
***********************************************************************         
*                                                                               
EOLVAL   DS    0H                                                               
         LA    R8,SVLNQ(R8)                                                     
*                                                                               
EOL100   LA    R4,LINLEN(R4)       END OF SCREEN YET?                           
         LA    R1,TIMENDH                                                       
         CR    R4,R1                                                            
         BL    CPJ100                                                           
         OC    NEWINPUT,NEWINPUT                                                
         BNZ   EOL200                                                           
         LA    R2,TIMDATAH                                                      
         MVC   FVMSGNO,=AL2(AE$MISIF)                                           
         B     EXIT                                                             
*                                                                               
EOL200   MVI   0(R8),X'FF'         MARK END OF TABLE                            
         USING LIND,R4                                                          
         LA    R4,TIMDATAH         FIRST LINE                                   
EOL300   TM    1(R4),X'20'         PROTECTED                                    
         BO    *+8                                                              
         OI    4(R4),X'20'         MARK AS PREVIOUSLY VALIDATED                 
         ZIC   R1,0(R4)            BUMP TO NEXT FIELD                           
         AR    R4,R1                                                            
         LA    R2,TIMENDH                                                       
         CR    R4,R2               BOTTOM OF SCREEN YET?                        
         BL    EOL300                                                           
*                                                                               
EOLX     DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              BUILD ELEMENTS                                         *         
***********************************************************************         
*                                                                               
         USING SVTABLD,R8                                                       
BUILD    LA    R8,SVTABL                                                        
*                                                                               
BUILD100 CLI   0(R8),X'FF'         END OF TABLE?                                
         BE    BUILD200                                                         
         CLI   CSOMODE,CSOMPCHA    PREPARE SCREEN FOR CHANGE?                   
         BE    BUILD200                                                         
*                                                                               
*              BUILD DESCRIPTION ELEMENT                                        
*                                                                               
         LA    R5,IOAREA+2                                                      
         USING DLDESCD,R5                                                       
         MVI   DLDSEL,X'64'                                                     
         MVC   DLDSDATE,TODAY3                                                  
         MVC   DLDSREF,BCSPACES                                                 
         CLI   CSOMODE,CSOMDCHA    USE OLD REF# DURING ITEM/CHANGE              
         BNE   *+14                                                             
         MVC   DLDSREF,BCITECUR+(LSTIREF-LSTTABD)                               
         B     BUILD125                                                         
         SR    R3,R3               ELSE USE ITEM COUNT + 1                      
         ICM   R3,3,CSLSTCUR+(LSTBITMA-LSTTABD)                                 
         LA    R3,1(R3)                                                         
         CURED (R3),(3,DLDSREF),0,DMCB=BOPARM,ALIGN=LEFT                        
BUILD125 MVI   DLDSSBRF,0                                                       
         XC    DLDSSTAT(7),DLDSSTAT                                             
         XC    DLDSNARR,DLDSNARR                                                
         LA    R2,TIMNARH                                                       
         LA    R3,DLDSNARR                                                      
         GOTO1 AVALNAR,BOPARM,(R2),(R3)                                         
         SR    R3,R5                                                            
         AR    R3,R6               R6=L'NARRATIVE (RETRNED FROM VALNAR)         
         STC   R3,DLDSLEN                                                       
         AR    R5,R3                                                            
*                                                                               
*              MEMO POSTING - X'40' ELEMENT                                     
*                                                                               
         USING PRTELD,R5                                                        
         XC    PRTEL(PRTLNQ),PRTEL                                              
         MVI   PRTEL,PRTELQ        X'40'                                        
         MVI   PRTLN,PRTLNQ                                                     
         ZAP   PRTRATE,=P'0'                                                    
         ZAP   PRTHOUR,SVHRS                                                    
         OI    PRTSTAT,PRTSNOTQ    NOT BILLABLE TIME                            
         ZIC   R1,PRTLN                                                         
         AR    R5,R1                                                            
*                                                                               
*              BUILD X'50' ELEMENT - MEMO ELEMENT                               
*                                                                               
         USING TRCASHD,R5                                                       
         XC    TRCSEL(TRCSLNQ1),TRCSEL                                          
         MVI   TRCSEL,X'50'                                                     
         MVI   TRCSLEN,TRCSLNQ1    X'09'                                        
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
         BNH   BUILD150                                                         
         MVC   PCICLI(1),CUABIN                                                 
         MVC   PCICLI+1(2),=C'SJ'                                               
         MVC   PCICLI+3(L'SVSJCPJ),SVSJCPJ                                      
         MVC   PCIPRJT,PCICLI                                                   
         MVC   PCITSK,SVTSK                                                     
         OC    SVPCACT,SVPCACT     ANY PROJECT CONTROL ACCOUNT?                 
         BZ    *+10                                                             
         MVC   PCIPRJT,SVPCACT                                                  
         ZIC   R1,PCILN                                                         
         AR    R5,R1                                                            
*                                                                               
*              MEMO X'60' ELEMENT - TRANSACTION STATUS ELEMENT                  
*                                                                               
         USING TRSELD,R5                                                        
BUILD150 XC    TRSEL(TRSLNQ),TRSEL                                              
         MVI   TRSEL,TRSELQ        X'60' ELEMENT                                
         MVI   TRSLN,TRSLNQ                                                     
         OC    TRSSTAT2,TYPETIME   INDICATE MISSING/ADJUSTMENT                  
         ZIC   R1,TRSLN                                                         
         AR    R5,R1                                                            
*                                                                               
*              DR U/L=1R C/A=1C  OR DR U/L=1R  C/A=1N                           
*                                                                               
         USING DLPOSTD,R5                                                       
         MVI   DLPSEL,X'69'                                                     
         MVI   DLPSLEN,X'71'                                                    
         MVC   DLPSDBAC,SV1RACT                                                 
         MVC   DLPSDBNM,SV1RNAME                                                
         MVC   DLPSCRAC,SV1CACT    CONTRA IS EITHER A 1C ACCOUNT                
         MVC   DLPSCRNM,SV1CNAME                                                
         OC    SV1NACT,SV1NACT     OR A 1N ACCOUNT                              
         BZ    *+16                                                             
         MVC   DLPSCRAC,SV1NACT                                                 
         MVC   DLPSCRNM,SV1NNAME                                                
         MVI   DLPSTYPE,0                                                       
         ZAP   DLPSAMNT,=P'0'                                                   
         MVC   DLPSANAL,SVOFFICE                                                
         OC    SV1NACT,SV1NACT                                                  
         BZ    *+10                                                             
         MVC   DLPSANAL,SV1ROFFC   USE 1R OFFICE                                
         ZIC   R1,DLPSLEN                                                       
         AR    R5,R1                                                            
*                                                                               
         CLC   SVSJJOB,BCSPACES    ONLY MAKE JOB POSTING IF JOB                 
         BNH   BUILD175            WAS INPUT                                    
         TM    TSCPYST7,CPYSJTIM   AND ONLY IF COMPANY IS ON NEWCOST            
         BZ    BUILD175                                                         
*                                                                               
*              MEMO POSTING - X'40' ELEMENT                                     
*                                                                               
         USING PRTELD,R5                                                        
         XC    PRTEL(PRTLNQ),PRTEL                                              
         MVI   PRTEL,PRTELQ        X'40'                                        
         MVI   PRTLN,PRTLNQ                                                     
         ZAP   PRTRATE,=P'0'                                                    
         ZAP   PRTHOUR,SVHRS                                                    
         OI    PRTSTAT,PRTSNOTQ    NOT BILLABLE TIME                            
         ZIC   R1,PRTLN                                                         
         AR    R5,R1                                                            
*                                                                               
*              BUILD X'50' ELEMENT - MEMO ELEMENT                               
*                                                                               
         USING TRCASHD,R5                                                       
         XC    TRCSEL(TRCSLNQ1),TRCSEL                                          
         MVI   TRCSEL,X'50'                                                     
         MVI   TRCSLEN,TRCSLNQ1    X'09'                                        
         MVI   TRCSTYPE,C'H'       MEMO TIME ITEM                               
         CLC   SV1NACT+3(2),=C'L ' DO NOT WANT UPDATE TO GENERATE               
         BNE   *+8                 BUCKETS FOR LEAVE OF ABSENCE                 
         MVI   TRCSTYPE,C'X'                                                    
         ZAP   TRCSAMNT,SVHRS                                                   
         ZIC   R1,TRCSLEN                                                       
         AR    R5,R1                                                            
*                                                                               
*              MEMO X'60' ELEMENT - TRANSACTION STATUS ELEMENT                  
*                                                                               
         USING TRSELD,R5                                                        
         XC    TRSEL(TRSLNQ),TRSEL                                              
         MVI   TRSEL,TRSELQ        X'60' ELEMENT                                
         MVI   TRSLN,TRSLNQ                                                     
         OC    TRSSTAT2,TYPETIME   INDICATE MISSING/ADJUSTMENT                  
         ZIC   R1,TRSLN                                                         
         AR    R5,R1                                                            
*                                                                               
*              DR U/L=SJ C/A=1R                                                 
*                                                                               
         USING DLPOSTD,R5                                                       
         MVI   DLPSEL,X'69'                                                     
         MVI   DLPSLEN,X'71'                                                    
         MVC   DLPSDBAC(1),CUABIN                                               
         MVC   DLPSDBAC+1(2),=C'SJ'                                             
         MVC   DLPSDBAC+3(L'SVSJCPJ),SVSJCPJ                                    
         MVC   DLPSDBNM,SVSJCPJN                                                
         MVC   DLPSCRAC,SV1RACT                                                 
         MVC   DLPSCRNM,SV1RNAME                                                
         MVI   DLPSTYPE,X'40'      JOB POSTING IS NON-COMM                      
         ZAP   DLPSAMNT,=P'0'                                                   
         MVC   DLPSANAL,SVTSK                                                   
         ZIC   R1,DLPSLEN                                                       
         AR    R5,R1                                                            
*                                                                               
BUILD175 MVI   0(R5),0             END OF RECORD                                
         LA    R5,1(R5)                                                         
         LA    R1,IOAREA                                                        
         SR    R5,R1               PUT OUT ACCDAY RECORD                        
         STH   R5,BOHALF1                                                       
         MVC   IOAREA(2),BOHALF1                                                
         MVC   CSOLINE,SVCSLINE                                                 
*                                                                               
         LA    R1,BCFULL                                                        
         XC    0(4,R1),0(R1)       CLEAR DISK ADDRESS                           
         ST    R1,BOPARM+8                                                      
         MVC   BOWORK1(L'DLDSREF),IOAREA+2+(DLDSREF-DLDESCD)                    
         L     R5,BOPARM+8                                                      
         MVC   BOWORK1+10(4),0(R5) DISK ADDRESS                                 
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         GOTO1 AADDITE,BOPARM,IOAREA,BOPL61,BOWORK1                             
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   EXIT                                                             
         LA    R8,SVLNQ(R8)        NEXT TABLE ENTRY                             
         B     BUILD100                                                         
*                                                                               
*              END OF TRANSACTION                                               
*                                                                               
BUILD200 LA    R2,TIMSTFH                                                       
         MVC   FVMSGNO,=AL2(FVFOK)                                              
EXIT     ST    R2,FVADDR                                                        
EXIT1    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*              READ U/L=1C OR SJ                                      *         
***********************************************************************         
*                                                                               
GET1C    NTR1                                                                   
         LA    R5,IOKEY                                                         
         USING ACTRECD,R5                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(2),=C'1C'                                                
         MVI   SVCLIPOS,2                                                       
*                                                                               
         TM    BCCPYEL+(CPYSTAT3-CPYELD),X'04'      PC=SJ?                      
         BZ    *+14                                                             
         MVC   ACTKUNT(2),=C'SJ'                                                
         MVI   SVCLIPOS,1                                                       
*                                                                               
         GOTO1 AGETLDG                                                          
         L     R5,AIO1                                                          
         LA    R5,ACTRFST                                                       
         SR    RE,RE                                                            
GET1C10  CLI   0(R5),0                                                          
         BE    EXIT                                                             
         CLI   0(R5),X'14'                                                      
         BE    *+14                                                             
         IC    RE,1(R5)                                                         
         AR    R5,RE                                                            
         B     GET1C10                                                          
*                                                                               
         USING LDGELD,R5                                                        
         CLI   LDGLN,LDGLNQ        SKIP OLD ELEMENTS                            
         BL    EXIT                                                             
         CLI   LDGCPOS,0                                                        
         BE    EXIT                                                             
         MVC   SVCLIPOS,LDGCPOS    SAVE CLIENT POSITION                         
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*              READ U/L=1R                                            *         
***********************************************************************         
*                                                                               
READ1R   NTR1                                                                   
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
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              READ U/L=1C                                            *         
***********************************************************************         
*                                                                               
READ1C   NTR1                                                                   
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
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              LITERALS                                               *         
***********************************************************************         
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
         LA    RF,SV1RACT+3        ISOLATE OFFICE CODE                          
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
*              WORKING STORAGE                                        *         
***********************************************************************         
*                                                                               
PROGD    DSECT                                                                  
RELO2    DS    A                                                                
*                                                                               
PROFILES DS    0XL8                PROFILES                                     
PROFILE1 DS    XL1                 Y=ONLY SHOW CLIENT NAME                      
PROFILE2 DS    XL2                 Y=ALLOW TS 1 YEAR INTO FUTURE                
         DS    XL6                 N/D                                          
*                                                                               
TSCPYST1 DS    XL1                 COMPANY ELEMENT STATUS BYTE #1               
TSCPYST2 DS    XL1                 COMPANY ELEMENT STATUS BYTE #2               
TSCPYST3 DS    XL1                 COMPANY ELEMENT STATUS BYTE #3               
TSCPYST4 DS    XL1                 COMPANY ELEMENT STATUS BYTE #4               
TSCPYST5 DS    XL1                 COMPANY ELEMENT STATUS BYTE #5               
TSCPYST6 DS    XL1                 COMPANY ELEMENT STATUS BYTE #6               
TSCPYST7 DS    XL1                 COMPANY ELEMENT STATUS BYTE #7               
*                                                                               
STATUS   DS    XL1                 INDICATES IF ON NEW COSTING OR NOT           
PC1R     EQU   X'20'               1R ACCOUNT FLAGGED FOR PRJ/CTL               
PCSJ     EQU   X'40'               CLI/PRD FLAGGED FOR PRJ/CTL                  
*                                                                               
STATUS2  DS    XL1                                                              
RRDUP    EQU   X'80'               CLEAR OUT DUPLICATE ENTRY FIELD              
*                                                                               
SCRNHRS  DS    PL8                 TOTAL -HRS CURRENTLY ON SCREEN               
TOTHRS   DS    PL8                 TEMPORARY HRS ACCUMULATOR                    
ELCODE   DS    XL1                 USED IN GETEL                                
LINE     DS    CL1                                                              
FLAGT    DS    CL1                 RENAMED ALREADY IN ACBATDSECT                
TODAYR   DS    PL3                 TODAYS REAL DATE                             
TODAY3   DS    PL3                 DATE THAT USER HAS ENTERED                   
TYPETIME DS    XL1                 TYPE OF TIME (ADJ/REG/MISS)                  
NEWINPUT DS    XL1                 INDICATES IF SOMETHING WAS INPUT             
SCRREQ   DS    XL1                 SCREEN 'REQUIRED FIELD INDICATOR'            
TEMPT    DS    CL50                                                             
*                                                                               
FLDSTAT  DS    0XL3                                                             
FLDINV   DS    XL1                 FIELDS THAT INPUT ISNT VALID                 
FLDINPT  DS    XL1                 FIELDS THAT HAVE INPUT                       
FLDREQ   DS    XL1                 FIELDS THAT ARE REQUIRED                     
FLDCLI   EQU   X'01'               CLIENT CODE                                  
FLDPRO   EQU   X'02'               PRODUCT                                      
FLDJOB   EQU   X'04'               JOB                                          
FLDTSK   EQU   X'08'               TASK                                         
FLDHRS   EQU   X'10'               HOURS                                        
FLD1NACT EQU   X'20'               1N ACCT WAS ENTERED IN CLIENT FIELD          
*                                                                               
SVTSD    DS    XL1                 SAVED TIMESHEET DAY - TSD=XXX                
SVHIRE   DS    CL3                 HIRE DATE YYMMDD                             
SVTERM   DS    CL3                 TERMINATION DATE YYMMDD                      
SV1RACT  DS    CL15                                                             
SV1RNAME DS    CL36                                                             
SV1ROFFC DS    CL2                 1R OFFICE CODE                               
SV1ROFFP DS    XL1                 POSITION OF 1R OFFICE IN ACCOUNT             
SVCLIPOS DS    CL1                 DISPLACEMENT TO CLIENT IN 1C                 
SVDPGRP  DS    CL1                 DEPT GRP CODE FROM 1R DEPT REC               
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
SVTABL   DS    (10*SVLNQ)C         TABLE OF CLI/PRD/JOB/TSK/HRS                 
SVTABMAX EQU   8                   MAX # ENTRIES/SCREEN                         
IOAREA   DS    2000C                                                            
*                                                                               
PROGDX   DS    0C                                                               
         EJECT                                                                  
***********************************************************************         
*              DSECT FOR A SCREEN LINE                                *         
***********************************************************************         
*                                                                               
LIND     DSECT                                                                  
LINHRH   DS    CL8                                                              
LINHR    DS    CL6                 HOURS                                        
LINHRX   DS    CL8                                                              
LINCLIH  DS    CL8                                                              
LINCLI   DS    CL12                CLIENT                                       
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
LINNMH   DS    CL8                                                              
LINNM    DS    CL30                NAME                                         
LINNMX   DS    CL8                                                              
LINLEN   EQU   *-LIND                                                           
LINNUMQ  EQU   6                   NUMBER OF INPUT FIELDS PER LINE              
         EJECT                                                                  
***********************************************************************         
*              DSECT FOR ACCOUNT/HOURS TABLE                          *         
***********************************************************************         
*                                                                               
SVTABLD  DSECT                                                                  
SVENTRY  DS    0C                                                               
SVCSLINE DS    XL1                 CURRENT ITEM NUMBER                          
SVHRS    DS    PL3                 HOURS                                        
SVSJCPJ  DS    0CL12               SJ CLIENT/PROD                               
SVSJCLI  DS    CL3                                                              
SVSJPRO  DS    CL3                                                              
SVSJJOB  DS    CL6                                                              
SVSJCPJN DS    CL36                                                             
SVPCACT  DS    CL15                PROJECT CONTROL ACCOUNT                      
SVOFFICE DS    CL2                 CLIENT OFFICE CODE                           
SVTSK    DS    CL2                 TASK CODE                                    
SV1CACT  DS    CL15                COSTING ACCOUNT CODE                         
SV1CNAME DS    CL36                COSTING ACCOUNT NAME                         
SV1NACT  DS    CL15                NON CLIENT ACCOUNT                           
SV1NNAME DS    CL36                                                             
SVLNQ    EQU   *-SVTABLD                                                        
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE ACBATDSECT                                                     
         ORG BASOLY2H                                                           
       ++INCLUDE ACBATE3D                                                       
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
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045ACBAT1B   05/01/02'                                      
         END                                                                    

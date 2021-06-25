*          DATA SET PPINF13    AT LEVEL 070 AS OF 06/01/07                      
*PHASE T41A13A,+0,NOAUTO                                                        
         TITLE 'T41A13 - CHANGE LOG'                                            
*                                                                               
* BPLA 06/01/07   USER DATA UNDERLINING FIX                                     
*                                                                               
* KWAN 08/23/99   ADD FILTER DATA=COS2                                          
*                                                                               
* KWAN 05/17/99   CORRECT FILTER ERROR DISPLAY                                  
*                                                                               
* SMYE 01/06/98   MODIFY DATA=COMMENT (AND JOB AND REP) TO ALLOW FOR            
*                 SECOND ESTIMATE STANDARD COMMENT                              
*                                                                               
* SMYE 11/04/97   ADD FILTER FOR SPECIAL FINANCIAL HANDLING (SFH) AND           
*                 SHOW "S" (IF SFH) BEFORE "RET SCH" COLUMN ON SCREEN           
*                                                                               
* SMYE 10/17/97   CHANGED PROC GEFEST19 TO USE TM.. TO TEST PESTTEST            
*                 FIELD FOR LIVE OR TEST STATUS                                 
*                                                                               
* SMYE 2/20/97    ADD "DATA=RATETYPE" OPTION AND FILTER                         
*                                                                               
* SMYE 2/14/97    ADD "DATA=STATUS" OPTION                                      
*                                                                               
* BPLA 4/26/95    SET CUSER TO '* UNDEFINED *' SO GARBAGE WON'T                 
*                 BE DISPLAYED IF THERE IS NO USER DATA DEFINED                 
*                 IN THE CLIENT RECORD                                          
*                                                                               
* LWEI 2/02/93 -- ADD FILTER FOR USER FIELDS                                    
*                                                                  L01          
* ROSA 6/13/88 -- ADD FILTER FOR RETAIL SCHEME FOR ESTIMAGE ON     L01          
*                 DISPLAY                                          L01          
         TITLE 'T41A13 - PRINTPAK INFO ESTIMATE HEADERS'                        
T41A13   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 (ESTWRKX-ESTWRKD),T41A13                                         
         LR    R3,RC                                                            
         USING ESTWRKD,R3                                                       
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T41AFFD,RA                                                       
         LA    R8,T41A13+4095                                                   
         LA    R8,1(R8)                                                         
         USING T41A13+4096,R8      ** NOTE USE OF SECOND BASE REG **            
         USING FLDHDRD,R2                                                       
         XC    WORK,WORK                                                        
         XC    MGSW(8),MGSW        CLEAR SWITCHES                               
         XC    FILT1(3),FILT1      FILTERS                                      
         XC    FILTREP,FILTREP                                                  
         XC    FILTJOB,FILTJOB                                                  
         XC    FILTRTYP,FILTRTYP                                                
         XC    FILTSFH,FILTSFH                                                  
         XC    USESW,USESW                                                      
         XC    CPROFLT,CPROFLT                                                  
         XC    FILTRSCH,FILTRSCH   CLR  RETAIL SCHEME FLT SAVE AREAL01          
         LA    R5,REC              SET RECORD ADDRESS                           
         ST    R5,AREC                                                          
*                                                                               
         MVI   FLTSW,0             INITIALIZE FILTER SWITCH                     
*                                                                               
*                                                                               
*                                                                               
CKFILT1  GOTO1 GETFLTR,DUB,(64,SINIFLT),(2,=C'1=')                              
         OC    4(4,R1),4(R1)                                                    
         BZ    CKFILT2                                                          
         L     R4,4(R1)                                                         
         LR    R6,R4                                                            
         LA    RE,4                                                             
         LA    R4,2(R4)                                                         
         CLI   FILT1,0             SEE IF POSTION ALREADY USED                  
         BNE   FLTERR                                                           
         MVC   FILT1,0(R4)                                                      
         CLI   0(R4),C'-'                                                       
         BNE   CKFILT1C                                                         
         LA    R4,1(R4)                                                         
         MVC   FILT1,0(R4)                                                      
         NI    FILT1,X'7F'         SET OFF HIGH ORDER BIT FOR NEGATIVE          
CKFILT1C CLI   0(R4),C'A'          MUST BE ALPHA-NUMERIC                        
         BL    FLTERR                                                           
         CLI   1(R4),C','                                                       
         BE    CKFILT1X                                                         
         CLI   1(R4),0                                                          
         BNE   FLTERR                                                           
*                                                                               
CKFILT1X DS    0H                                                               
         MVI   FLTSW,1             CONTAIN A VALID FILTER FIELD                 
*                                                                               
*                                                                               
*                                                                               
CKFILT2  GOTO1 GETFLTR,DUB,(64,SINIFLT),(2,=C'2=')                              
         OC    4(4,R1),4(R1)                                                    
         BZ    CKFILT3                                                          
         L     R4,4(R1)                                                         
         LR    R6,R4                                                            
         LA    RE,4                                                             
         LA    R4,2(R4)                                                         
         CLI   FILT2,0             SEE IF POSTION ALREADY USED                  
         BNE   FLTERR                                                           
         MVC   FILT2,0(R4)                                                      
         CLI   0(R4),C'-'                                                       
         BNE   CKFILT2C                                                         
         LA    R4,1(R4)                                                         
         MVC   FILT2,0(R4)                                                      
         NI    FILT2,X'7F'         SET OFF HIGH ORDER BIT FOR NEGATIVE          
CKFILT2C CLI   0(R4),C'A'          MUST BE ALPHA-NUMERIC                        
         BL    FLTERR                                                           
         CLI   1(R4),C','                                                       
         BE    CKFILT2X                                                         
         CLI   1(R4),0                                                          
         BNE   FLTERR                                                           
*                                                                               
CKFILT2X DS    0H                                                               
         MVI   FLTSW,1             CONTAIN A VALID FILTER FIELD                 
*                                                                               
*                                                                               
*                                                                               
CKFILT3  GOTO1 GETFLTR,DUB,(64,SINIFLT),(2,=C'3=')                              
         OC    4(4,R1),4(R1)                                                    
         BZ    CKSTAT                                                           
         L     R4,4(R1)                                                         
         LR    R6,R4                                                            
         LA    RE,4                                                             
         LA    R4,2(R4)                                                         
         CLI   FILT3,0             SEE IF POSTION ALREADY USED                  
         BNE   FLTERR                                                           
         MVC   FILT3,0(R4)                                                      
         CLI   0(R4),C'-'                                                       
         BNE   CKFILT3C                                                         
         LA    R4,1(R4)                                                         
         MVC   FILT3,0(R4)                                                      
         NI    FILT3,X'7F'         SET OFF HIGH ORDER BIT FOR NEGATIVE          
CKFILT3C CLI   0(R4),C'A'          MUST BE ALPHA-NUMERIC                        
         BL    FLTERR                                                           
         CLI   1(R4),C','                                                       
         BE    CKFILT3X                                                         
         CLI   1(R4),0                                                          
         BNE   FLTERR                                                           
*                                                                               
CKFILT3X DS    0H                                                               
         MVI   FLTSW,1             CONTAIN A VALID FILTER FIELD                 
*                                                                               
*                                                                               
*                                                                               
CKSTAT   GOTO1 GETFLTR,DUB,(64,SINIFLT),(7,=C'STATUS=')                         
         OC    4(4,R1),4(R1)                                                    
         BZ    CKJOB                                                            
         L     R4,4(R1)                                                         
         LR    R6,R4                                                            
         LA    RE,7                                                             
         LA    R4,7(R4)                                                         
         CLI   0(R4),C'1'                                                       
         BE    CKSTAT10                                                         
         CLI   0(R4),C'2'                                                       
         BE    CKSTAT10                                                         
         CLC   0(4,R4),=C'LIVE'      ONLY LIVE                                  
         BNE   CKSTAT5                                                          
         MVI   TESTSW,X'08'                                                     
         B     CKSTATX                                                          
*                                                                               
CKSTAT5  CLC   0(4,R4),=C'TEST'      ONLY TEST                                  
         BNE   FLTERR                                                           
         MVI   TESTSW,X'80'                                                     
         B     CKSTATX                                                          
*                                                                               
CKSTAT10 MVC   LOCKSW,0(R4)                                                     
         CLC   1(4,R4),=C'LIVE'     LIVE AND LOCKED                             
         BNE   CKSTAT15                                                         
         MVI   TESTSW,X'08'                                                     
         B     CKSTATX                                                          
*                                                                               
CKSTAT15 CLC   1(4,R4),=C'TEST'     TEST AND LOCKED                             
         BNE   CKSTAT20                                                         
         MVI   TESTSW,X'80'                                                     
         B     CKSTATX                                                          
*                                                                               
CKSTAT20 CLI   1(R4),C' '           BAD ENTRY AFTER 1 OR 2                      
         BH    FLTERR                                                           
*                                                                               
CKSTATX  DS    0H                                                               
         MVI   FLTSW,1             CONTAIN A VALID FILTER FIELD                 
*                                                                               
*                                                                               
*                                                                               
CKJOB    GOTO1 GETFLTR,DUB,(64,SINIFLT),(4,=C'JOB=')                            
         OC    4(4,R1),4(R1)                                                    
         BZ    CKAD                                                             
         L     R4,4(R1)                                                         
         LR    R6,R4                                                            
         LA    RE,4                                                             
         LA    R4,4(R4)                                                         
         BAS   R9,GETFLD                                                        
         LTR   R5,R5                                                            
         BZ    FLTERR                                                           
         CHI   R5,6                                                             
         BH    FLTERR                                                           
         L     R4,4(R1)                                                         
         LA    R4,4(R4)                                                         
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
*                                                                               
CKJOBX   MVC   FILTJOB(0),0(R4)                                                 
         OC    FILTJOB,=CL6'      '                                             
*                                                                               
         MVI   FLTSW,1             CONTAIN A VALID FILTER FIELD                 
*                                                                               
*                                                                               
*                                                                               
CKAD     GOTO1 GETFLTR,DUB,(64,SINIFLT),(3,=C'AD=')                             
         OC    4(4,R1),4(R1)                                                    
         BZ    CKREP                                                            
         L     R4,4(R1)                                                         
         LR    R6,R4                                                            
         LA    RE,3                                                             
         LA    R4,3(R4)                                                         
         BAS   R9,GETFLD                                                        
         LTR   R5,R5                                                            
         BZ    FLTERR                                                           
         OC    FILTJOB,FILTJOB       SEE IF ALREADY ENTERED                     
         BNZ   FLTERR                                                           
*                                                                               
         CHI   R5,6                                                             
         BH    FLTERR                                                           
         L     R4,4(R1)                                                         
         LA    R4,3(R4)                                                         
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
*                                                                               
CKADX    MVC   FILTJOB(0),0(R4)                                                 
         OC    FILTJOB,=CL6'      '                                             
*                                                                               
         MVI   FLTSW,1             CONTAIN A VALID FILTER FIELD                 
*                                                                               
*                                                                               
*                                                                               
CKREP    GOTO1 GETFLTR,DUB,(64,SINIFLT),(4,=C'REP=')                            
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKEST                                                           
         L     R4,4(R1)                                                         
         LR    R6,R4                                                            
         LA    RE,4                                                             
         LA    R4,4(R4)                                                         
         CLI   0(R4),C'A'          SOME REPS START WITH 'A'                     
         BE    CKREP5                                                           
         BAS   R9,GETNUM                                                        
         L     R6,4(R1)                                                         
         LA    RE,4                                                             
         LTR   R5,R5                                                            
         BZ    FLTERR                                                           
         L     R4,4(R1)            CONVERT ESTIMATE 1                           
         LA    R4,4(R4)                                                         
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R4)         * EXECUTED *                                 
         CVB   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  FILTREP,DUB                                                      
         B     CKREPX                                                           
*                                                                               
CKREP5   BAS   R9,GETFLD                                                        
         LTR   R5,R5                                                            
         BZ    FLTERR                                                           
         CHI   R5,4                                                             
         BNE   FLTERR                                                           
*                                                                               
         MVC   FILTREP,0(R4)                                                    
*                                                                               
CKREPX   DS    0H                                                               
         MVI   FLTSW,1             CONTAIN A VALID FILTER FIELD                 
*                                                                               
*                                                                               
*                                                                               
CHKEST   GOTO1 GETFLTR,DUB,(64,SINIFLT),(4,=C'EST=')                            
         OC    4(4,R1),4(R1)                                                    
         BNZ   CNVEST                                                           
         MVC   EST1(4),=X'0001FFFF'                                             
         XC    SDATE,SDATE                                                      
         B     CHKDATE                                                          
*                                                                               
*                                                                               
*                                                                               
CNVEST   XC    EST1(4),EST1                                                     
         XC    SDATE,SDATE                                                      
         L     R4,4(R1)            SET FIELD POINTER                            
         LA    R4,4(R4)                                                         
         CLC   0(2,R4),=C'NO'                                                   
         BNE   CE1                                                              
         MVC   EST1(4),=X'0001FFFF'                                             
         B     CHKESTX                                                          
*                                                                               
CE1      CLC   0(5,R4),=C'FIRST'                                                
         BNE   CE2                                                              
         MVC   EST1(4),=X'0001FFFE'                                             
         B     CHKESTX                                                          
*                                                                               
CE2      BAS   R9,GETNUM                                                        
         L     R6,4(R1)                                                         
         LA    RE,4                                                             
         LTR   R5,R5                                                            
         BZ    FLTERR                                                           
         L     R4,4(R1)            CONVERT ESTIMATE 1                           
         LA    R4,4(R4)                                                         
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R4)         * EXECUTED *                                 
         CVB   RF,DUB                                                           
         STH   RF,EST1                                                          
         STH   RF,EST2                                                          
         LA    R4,1(R5,R4)                                                      
         CLI   0(R4),C'-'          ESTIMATE SERIES                              
         BNE   CHKESTX             NO - CHECK DATA FORMAT                       
         LA    R4,1(R4)                                                         
         LR    RF,R4                                                            
         BAS   R9,GETNUM                                                        
         LTR   R5,R5                                                            
         BZ    FLTERR                                                           
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,RF)         * EXECUTED *                                 
         CVB   RF,DUB                                                           
         STH   RF,EST2                                                          
         CLC   EST1,=2X'00'                                                     
         BE    FLTERR                                                           
         CLC   EST2,=2X'00'                                                     
         BE    CHKESTX                                                          
         CLC   EST1,EST2                                                        
         BH    FLTERR                                                           
*                                                                               
CHKESTX  DS    0H                                                               
         MVI   FLTSW,1             CONTAIN A VALID FILTER FIELD                 
*                                                                               
*                                                                               
*                                                                               
CHKDATE  GOTO1 GETFLTR,DUB,(64,SINIFLT),(5,=C'DATE=')                           
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKRETS             CHECK FOR RETAIL SCHEME                      
         L     R6,4(R1)                                                         
         LR    R4,R6                                                            
         LA    R4,5(R4)                                                         
         GOTO1 VDATVAL,DMCB,(R4),SDATE                                          
         LA    RE,5                                                             
         CLI   DMCB+3,0                                                         
         BE    FLTERR                                                           
         IC    RE,DMCB+3                                                        
         LA    R4,1(RE,R4)                                                      
         GOTO1 VDATVAL,DMCB,(R4),EDATE                                          
         LA    RE,5                                                             
         CLI   DMCB+3,0                                                         
         BE    FLTERR                                                           
*                                                                               
CHKDATEX DS    0H                                                               
         MVI   FLTSW,1             CONTAIN A VALID FILTER FIELD                 
*                                                                               
*                                                                               
*                                                                               
CHKRETS  GOTO1 GETFLTR,DUB,(64,SINIFLT),(7,=C'RETSCH=')                         
         OC    4(4,R1),4(R1)       ANYTHING THERE                               
         BZ    CHKRTYP             CHECK FOR RATE TYPE                          
         L     R4,4(R1)            LOAD ADDRESS OF RETSCH=                      
         LR    R6,R4               SAVE                                         
         LA    R4,7(R4)            BUMP PAST DESCRIPTION                        
         BAS   R9,GETFLD           DETERMINE LENGTH                             
         LA    RE,7                FLTERR ROUTINE NEEDS THIS IN RE              
         LTR   R5,R5               IF ZERO IS ERROR                             
         BZ    FLTERR                                                           
         CHI   R5,2                LENGTH M/B 2                                 
         BH    FLTERR                                                           
         MVC   FILTRSCH,7(R6)      MOVE FILTER                                  
*                                                                               
CHKRETSX DS    0H                                                               
         MVI   FLTSW,1             CONTAIN A VALID FILTER FIELD                 
*                                                                               
*                                                                               
*                                                                               
CHKRTYP  GOTO1 GETFLTR,DUB,(64,SINIFLT),(9,=C'RATETYPE=')                       
         OC    4(4,R1),4(R1)       ANYTHING THERE                               
         BZ    CHKSFH                                                           
         L     R4,4(R1)            LOAD ADDRESS OF RATETYPE=                    
         LR    R6,R4               SAVE                                         
         LA    R4,9(R4)            BUMP PAST DESCRIPTION                        
         BAS   R9,GETFLD           DETERMINE LENGTH                             
         LA    RE,9                FLTERR ROUTINE NEEDS THIS IN RE              
         LTR   R5,R5               IF ZERO IS ERROR                             
         BZ    FLTERR                                                           
         CHI   R5,1                LENGTH M/B 1                                 
         BH    FLTERR                                                           
         MVC   FILTRTYP,9(R6)      MOVE FILTER                                  
*                                                                               
CHKRTYPX DS    0H                                                               
         MVI   FLTSW,1             CONTAIN A VALID FILTER FIELD                 
*                                                                               
* CHECK FOR SFH (SPECIAL FINANCIAL HANDLING) FILTERS                            
*                                                                               
CHKSFH   GOTO1 GETFLTR,DUB,(64,SINIFLT),(4,=C'SFH=')                            
         OC    4(4,R1),4(R1)       ANYTHING THERE                               
         BZ    CHKFLT                                                           
         L     R4,4(R1)            LOAD ADDRESS OF SFH=                         
         LR    R6,R4               SAVE                                         
         LA    R4,4(R4)           BUMP PAST DESCRIPTION                         
         BAS   R9,GETFLD           DETERMINE LENGTH                             
         LA    RE,4       **FLTERR ROUTINE NEEDS THIS IN RE                     
         LTR   R5,R5               IF ZERO IS ERROR                             
         BZ    FLTERR                                                           
         CHI   R5,1                LENGTH M/B 1                                 
         BH    FLTERR                                                           
         CLI   4(R6),C'Y'          IS FILTER (Y)ES ?                            
         BE    CHKSFHX             YES                                          
         CLI   4(R6),C'N'          IS FILTER (N)O ?                             
         BNE   FLTERR              SFH FILTER MUST BE Y OR N                    
CHKSFHX  MVC   FILTSFH,4(R6)       MOVE FILTER                                  
*                                                                               
         MVI   FLTSW,1             CONTAIN A VALID FILTER FIELD                 
*                                                                               
*                                                                               
*                                                                               
CHKFLT   DS    0H                                                               
CHKFLT2  OC    EST1(4),EST1                                                     
         BNZ   CHKFRMT                                                          
         MVC   EST1(4),=X'0001FFFF'                                             
         CLI   SDATE,0                                                          
         BNE   CHKFRMT                                                          
         MVC   FLDDATA(38),=C'EST= OR DATE= FILTER MUST BE SPECIFIED'           
         FOUT  (R2)                                                             
         LA    R2,SINIFLTH                                                      
         B     MODEXIT                                                          
         EJECT                                                                  
*                                                                               
* DETERMINE SCREEN FORMAT                                                       
*                                                                               
CHKFRMT  DS    0H                                                               
         XC    USERDATA,USERDATA                                                
         GOTO1 GETFLTR,DMCB,(64,SINIFLT),(6,=C'USER1=')                         
         OC    4(4,R1),4(R1)                                                    
         BZ    CKUSER2                                                          
         MVI   USESW,1                                                          
         L     R4,4(R1)                                                         
         LA    R4,6(R4)                                                         
         LA    R6,USERDAT1                                                      
         BAS   RE,CKUDATA                                                       
*                                                                               
         MVI   FLTSW,1             CONTAIN A VALID FILTER FIELD                 
*                                                                               
CKUSER2  DS    0H                  CHK FOR USER DESCRIPTION                     
         GOTO1 GETFLTR,DMCB,(64,SINIFLT),(6,=C'USER2=')                         
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKDATA                                                          
         MVI   USESW,2                                                          
         L     R4,4(R1)                                                         
         LA    R4,6(R4)                                                         
         LA    R6,USERDAT2                                                      
         BAS   RE,CKUDATA                                                       
*                                                                               
         MVI   FLTSW,1             CONTAIN A VALID FILTER FIELD                 
*                                                                               
*                                                                               
*                                                                               
CHKDATA  GOTO1 GETFLTR,DUB,(64,SINIFLT),(5,=C'DATA=')                           
         OC    4(4,R1),4(R1)                                                    
         BZ    FRMTDTE             FORCE DATE DISPLAY                           
FRMTDEM  L     R4,4(R1)                                                         
         LA    RE,5                SET FOR ERROR                                
         LR    R6,R4                                                            
         CLC   5(5,R4),=C'DATES'                                                
         BE    CHKDATAX                                                         
         CLC   5(5,R4),=C'BILLF'   BILLING FORMULA                              
         BNE   *+12                                                             
         MVI   FORMSW,1                                                         
         B     CHKDATAX                                                         
         CLC   5(3,R4),=C'JOB'     JOBS                                         
         BNE   *+12                                                             
         MVI   JOBSW,1                                                          
         B     CHKDATAX                                                         
         CLC   5(3,R4),=C'REP'     REPS                                         
         BNE   *+12                                                             
         MVI   JOBSW,1                                                          
         B     CHKDATAX                                                         
         CLC   5(3,R4),=C'COM'     COMMENTS                                     
         BNE   *+12                                                             
*NOP*    MVI   JOBSW,3                                                          
         MVI   JOBSW,4                                                          
         B     CHKDATAX                                                         
         CLC   5(3,R4),=C'STA'     STATUS                                       
         BNE   *+12                                                             
         MVI   JOBSW,2                                                          
         B     CHKDATAX                                                         
         CLC   5(4,R4),=C'RATE'    RATE TYPE                                    
         BNE   *+12                                                             
         MVI   JOBSW,3                                                          
         B     CHKDATAX                                                         
         CLC   5(5,R4),=C'USER1'   USER1 FIELD                                  
         BNE   *+12                                                             
         MVI   USESW,1                                                          
         B     CHKDATAX                                                         
         CLC   5(5,R4),=C'USER2'   USER2 FIELD                                  
         BNE   *+12                                                             
         MVI   USESW,2                                                          
         B     CHKDATAX                                                         
*                                                                               
*                                                                               
*                                                                               
         CLC   5(4,R4),=C'COS2'    COST2 FACTOR                                 
         BNE   CHKD50                                                           
         ST    RE,FULL             RE IS NEEDED FOR FLTERR                      
         BAS   RE,GCLTCOS2                                                      
         L     RE,FULL                                                          
*                                                                               
         CLI   FORMSW,2            SWITCH IS SET IN GCLTCOS2                    
         BNE   FLTERR              DATA=COS2 IS NOT ALLOWED                     
         B     CHKDATAX                                                         
*                                                                               
*                                                                               
*                                                                               
CHKD50   DS    0H                                                               
*                                                                               
         CLC   5(4,R4),=C'PROF'                                                 
         BNE   FLTERR                                                           
         MVI   PROFSW,1                                                         
*                                                                               
* GET PROFILE FILTERS                                                           
*                                                                               
PF1      LA    R5,SINIFLT                                                       
         XC    CPROFLT,CPROFLT                                                  
POS1     GOTO1 GETFLTR,DMCB,(64,(R5)),(4,=C'CHAR')                              
         XC    WORK,WORK                                                        
         MVC   WORK(07),=C'CHARNN='                                             
         OC    4(4,R1),4(R1)                                                    
         BNZ   POS1A                                                            
         MVC   WORK(7),=C'BYTENN='                                              
         GOTO1 GETFLTR,DMCB,(64,(R5)),(4,=C'BYTE')                              
         OC    4(4,R1),4(R1)                                                    
         BZ    POSEX                                                            
POS1A    L     R5,4(R1)            SET FOR NEXT SCAN                            
         LR    R6,R5               POINT TO FIELD                               
         LA    RE,8                SET FIELD LENGTH FOR ERROR                   
         LA    R5,1(R5)                                                         
         MVC   HALF,=C'00'                                                      
         CLI   5(R6),C'='                                                       
         BNE   *+14                                                             
         MVC   HALF+1(1),4(R6)                                                  
         B     POS2                                                             
         CLI   6(R6),C'='                                                       
         BNE   FLTERR              ERROR                                        
         MVC   HALF,4(R6)                                                       
*                                                                               
POS2     CLI   HALF,C'0'           EDIT FIELD NUMBER                            
         BL    FLTERR                                                           
         CLI   HALF,C'9'                                                        
         BH    FLTERR                                                           
         CLI   HALF+1,C'0'                                                      
         BL    FLTERR                                                           
         CLI   HALF+1,C'9'                                                      
         BH    FLTERR                                                           
         PACK  DUB,HALF                                                         
         CVB   R9,DUB                                                           
         LTR   R9,R9                                                            
         BZ    FLTERR                                                           
         CHI   R9,32                                                            
         BH    FLTERR                                                           
         BCTR  R9,0                                                             
         LA    R9,CPROFLT(R9)                                                   
POS3     CLI   0(R6),C'='                                                       
         BE    *+12                                                             
         LA    R6,1(R6)                                                         
         B     POS3                                                             
         LA    R6,1(R6)                                                         
POS4     MVC   0(1,R9),0(R6)                                                    
         LA    R9,1(R9)                                                         
         LA    R6,1(R6)                                                         
         CLI   0(R6),0                                                          
         BE    POSEX                                                            
         CLI   0(R6),C','                                                       
         BE    POS1                                                             
         B     POS4                                                             
         EJECT                                                                  
*                                                                               
POSEX    B     CHKDATAX                                                         
         B     FLTERR                                                           
*                                                                               
CHKDATAX DS    0H                                                               
         MVI   FLTSW,1             CONTAIN A VALID FILTER FIELD                 
*                                                                               
*                                                                               
*                                                                               
FRMTDTE  DS    0H                                                               
*                                                                               
*                                                                               
*                                                                               
         CLI   SINIFLTH+5,0        ANY BAD INPUTS?                              
         BE    FRMTD00                                                          
         CLI   FLTSW,1                                                          
         BE    FRMTD00                                                          
         LA    R2,SINIFLTH         POSITION CURSOR                              
         LA    R3,2                FIELD INVALID ERR MSG                        
         B     ERROR                                                            
*                                                                               
*                                                                               
*                                                                               
FRMTD00  LA    R2,SINHDRH                                                       
         MVC   FLDDATA+1(4),=C'PROD'                                            
         MVC   FLDDATA+36(4),=C'FIL-'                            L01            
         MVC   FLDDATA+32(3),=C'RET'                             L01            
         CLC   SVPRD,=C'ALL'                                                    
         BNE   FRMTD2                                                           
         CLC   EST1(4),=X'0001FFFE' SEE IF DOING FIRST ESTIMATE                 
         BNE   FRMTD2                                                           
         MVC   FLDDATA+25(5),=C'FIRST'                                          
FRMTD2   DS    0H                                                               
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         MVC   FLDDATA+1(17),=C'CODE PRODUCT NAME'                              
         MVC   FLDDATA+27(3),=C'EST'                             L01            
         MVC   FLDDATA+32(3),=C'SCH'                             L01            
         MVC   FLDDATA+36(4),=C'TERS'                            L01            
         CLI   PROFSW,1            SEE IF DOING PROFILE                         
         BNE   FRMTD4                                                           
         MVC   FLDDATA+41(16),=C'ESTIMATE PROFILE'               L01            
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         MVC   FLDDATA+1(17),DASH                                               
         MVI   FLDDATA+5,C' '                                                   
         MVC   FLDDATA+27(3),DASH                                               
         MVC   FLDDATA+32(3),DASH                                L01            
         MVC   FLDDATA+36(4),DASH                                L01            
         MVC   FLDDATA+41(17),DASH                               L01            
         B     FRMTD10                                                          
*                                                                               
FRMTD4   CLI   USESW,0             SEE IF DOING USER FIELDS                     
         BE    FRMTD5                                                           
         BAS   RE,GETCLTU          GET CLT UDEF FIELD                           
         MVC   FLDDATA+41(20),CUSER                              L01            
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         MVC   FLDDATA+1(17),DASH                                               
         MVI   FLDDATA+5,C' '                                                   
         MVC   FLDDATA+27(3),DASH                                               
         MVC   FLDDATA+32(3),DASH                                L01            
         MVC   FLDDATA+36(4),DASH                                L01            
         MVC   FLDDATA+41(20),DASH                               L01            
*                                                                               
*                             SCAN BACKWARDS FOR FIRT NON-SPACE                 
*                             TO FIX UNDERLINING OF CUSER                       
         LA    RE,CUSER+19                                                      
         LA    RF,FLDDATA+60                                                    
         LA    R6,19     FOR BCT                                                
FRMTD4C  CLI   0(RE),C' '                                                       
         BH    FRMTD10                                                          
         BCTR  RE,0                                                             
         MVI   0(RF),C' '                                                       
         BCTR  RF,0                                                             
         BCT   R6,FRMTD4C                                                       
         B     FRMTD10                                                          
*                                                                               
FRMTD5   MVC   FLDDATA+41(6),=C' START'                          L01            
         MVC   FLDDATA+50(6),=C'   END'                                         
         MVC   FLDDATA+59(4),=C'NAME'                                           
         CLI   FORMSW,1                                                         
         BNE   *+14                                                             
         MVC   FLDDATA+59(15),=C'BILLING FORMULA'                               
         B     FRMTD8                                                           
*                                                                               
         CLI   FORMSW,2                                                         
         BNE   FRMTD5B                                                          
         MVC   FLDDATA+59(06),=C'COST 2'                                        
         B     FRMTD8                                                           
*                                                                               
FRMTD5B  CLI   JOBSW,1                                                          
         BE    FRMTD5D                                                          
         CLI   JOBSW,2                                                          
         BE    FRMTD5D                                                          
         CLI   JOBSW,3                                                          
         BE    FRMTD5D                                                          
         CLI   JOBSW,4                                                          
         BNE   FRMTD8                                                           
*NOP*FRMTD5D  MVC   FLDDATA+59(19),=C'JOB    REP  COMMENT'                      
FRMTD5D  MVC   FLDDATA+59(10),=C'JOB    REP'                                    
FRMTD8   DS    0H                                                               
         CLI   JOBSW,2                                                          
         BE    FRMTD8B                                                          
         CLI   JOBSW,3                                                          
*NOP*    BNE   FRMTD8C                                                          
         BE    FRMTD8A                                                          
         CLI   JOBSW,4                                                          
         BNE   FRMTD8C                                                          
         MVC   FLDDATA+66(8),=C'COMMENTS'                                       
         B     FRMTD8C                                                          
FRMTD8A  MVC   FLDDATA+70(9),=C'RATE TYPE'                                      
         B     FRMTD8C                                                          
FRMTD8B  MVC   FLDDATA+71(7),=C'STATUS '                                        
FRMTD8C  FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         MVC   FLDDATA+1(17),DASH                                               
         MVI   FLDDATA+5,C' '                                                   
         MVC   FLDDATA+27(3),DASH                                L01            
         MVC   FLDDATA+32(3),DASH                                L01            
         MVC   FLDDATA+36(4),DASH                                L01            
         MVC   FLDDATA+41(8),DASH                                L01            
         MVC   FLDDATA+50(8),DASH                                               
         MVC   FLDDATA+59(4),DASH                                               
         CLI   FORMSW,1                                                         
         BNE   *+14                                                             
         MVC   FLDDATA+59(15),DASH                                              
         B     FRMTD10                                                          
*                                                                               
         CLI   FORMSW,2            COST 2 FACTOR DASHES                         
         BNE   FRMTD8D                                                          
         MVC   FLDDATA+59(06),DASH                                              
         B     FRMTD10                                                          
*                                                                               
FRMTD8D  CLI   JOBSW,1                                                          
         BE    FRMTD8F                                                          
         CLI   JOBSW,2                                                          
         BE    FRMTD8F                                                          
         CLI   JOBSW,3                                                          
         BE    FRMTD8F                                                          
         CLI   JOBSW,4                                                          
         BNE   FRMTD10                                                          
FRMTD8F  MVC   FLDDATA+59(4),=C'--- '                                           
         CLI   JOBSW,4                                                          
         BNE   FRMTD8H                                                          
         MVC   FLDDATA+66(8),DASH                                               
         B     FRMTD10                                                          
FRMTD8H  MVC   FLDDATA+66(3),DASH                                               
         CLI   JOBSW,1                                                          
         BE    FRMTD10                                                          
         MVC   FLDDATA+71(7),DASH                                               
         CLI   JOBSW,2                                                          
         BNE   FRMTD8K                                                          
         MVI   FLDDATA+77,C' '                                                  
         B     FRMTD10                                                          
FRMTD8K  CLI   JOBSW,3                                                          
         BNE   FRMTD10                                                          
         MVC   FLDDATA+70(9),DASH                                               
FRMTD10  FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         CLI   PROFSW,1            SEE IF DOING PROFILE                         
         BNE   FRMTD12                                                          
         MVC   FLDDATA+41(4),=C'1-10'                              L01          
         MVC   FLDDATA+53(5),=C'11-20'                             L01          
         MVC   FLDDATA+65(5),=C'21-32'                             L01          
         FOUT  (R2)                                                             
FRMTD12  DS    0H                                                               
         LA    R2,LINLEN(R2)                                                    
*                                                                               
*GET ESTIMATES FROM THE FILE                                                    
*                                                                               
         LA    R6,MAXLINE          SET BCT LOOP                                 
         MVI   FRSTTIM,1           SET FIRST TIME                               
F3EST    BAS   RE,GETEST                                                        
         CLI   RECFLAG,1           END                                          
         BNE   *+14                                                             
         XC    PREVKEY,PREVKEY      YES - SET FOR NEXT REQUEST                  
         B     MODEXIT                                                          
*                                                                               
         L     R5,AREC             BUILD DATE LIST                              
         USING ESTHDRD,R5                                                       
         CLI   PROFSW,1            SEE IF DOING PROFILE                         
         BNE   F3E5                                                             
         MVC   FLDDATA+41(10),PESTPROF                              L01         
         MVC   FLDDATA+53(10),PESTPROF+10                           L01         
         MVC   FLDDATA+65(12),PESTPROF+20                           L01         
         B     F3SEND                                                           
*                                                                               
F3E5     CLI   USESW,0             SEE IF DOING USER FIELDS                     
         BE    F3E10                                                            
         BAS   RE,GETUSER                                                       
         MVC   FLDDATA+41(32),WORK2                                 L01         
         B     F3SEND                                                           
*                                                                               
F3E10    DS    0H                                                               
         LA    R9,FLDDATA+41                                        L01         
         GOTO1 VDATCON,DMCB,PESTST,(5,(R9))                                     
         LA    R9,FLDDATA+50                                                    
         GOTO1 VDATCON,DMCB,PESTEND,(5,(R9))                                    
*                                                                               
F31      CLI   FORMSW,1                                                         
         BE    F32                                                              
         CLI   FORMSW,2            COST 2 FACTOR                                
         BE    F35                                                              
         CLI   JOBSW,1                                                          
         BE    F31B                                                             
         CLI   JOBSW,2                                                          
         BE    F31B                                                             
         CLI   JOBSW,3                                                          
         BE    F31B                                                             
         CLI   JOBSW,4                                                          
         BNE   F31K                                                             
F31B     MVC   FLDDATA+59(6),PESTJOB                                            
         MVC   FLDDATA+66(4),PESTREP                                            
         CLI   JOBSW,1                                                          
         BE    F3SEND                                                           
         CLI   JOBSW,2                                                          
         BE    F31D                                                             
         CLI   JOBSW,3                                                          
         BE    F31F                                                             
*NOP*    MVC   FLDDATA+71(6),PESTCOM                                            
         MVC   FLDDATA+66(6),PESTCOM                                            
         CLI   PESTCOM2,X'39'      2ND COMMENT THERE ?                          
         BNH   F3SEND              NO                                           
         MVI   FLDDATA+72,C','                                                  
         MVC   FLDDATA+73(6),PESTCOM2                                           
         B     F3SEND                                                           
F31D     MVC   FLDDATA+73(4),=C'LIVE'                                           
         TM    PESTTEST,X'80'                                                   
         BZ    *+10                                                             
         MVC   FLDDATA+73(4),=C'TEST'                                           
         MVC   FLDDATA+71(1),PESTSTAT                                           
         B     F3SEND                                                           
F31F     MVC   FLDDATA+74(1),PESTRTYP                                           
         B     F3SEND                                                           
*                                                                               
F31K     MVC   FLDDATA+59(20),PESTNAME                                          
         B     F3SEND                                                           
F32      DS    0H                                                               
         LA    R4,PESTBILP                                                      
         USING BILPROFD,R4                                                      
**NEW 4/25/89                                                                   
         CLI   BILCMSW,C' '                                                     
         BNH   F33                                                              
         MVC   FLDDATA+59(1),BILCMSW                                            
*                                                                               
F33      LA    R7,BILBASA                                                       
         LA    R9,FLDDATA+60                                                    
         BAS   RE,FMTBAS                                                        
         LA    R9,FLDDATA+69                                                    
         OC    BILADJ,BILADJ                                                    
         BNZ   DISP2                                                            
         CLI   BILBASA,0                                                        
         BE    DISP4                                                            
         MVC   FLDDATA+65(2),=C'+0'                                             
         B     DISP4                                                            
*                                                                               
DISP2    LA    R9,FLDDATA+66                                                    
         EDIT  (B3,BILADJ),(8,0(R9)),4,ALIGN=LEFT,DROP=2                        
         MVI   FLDDATA+65,C'+'                                                  
         TM    BILADJ,X'80'                                                     
         BZ    *+8                                                              
         MVI   FLDDATA+65,C'-'                                                  
         LA    R9,FLDDATA+73                                                    
         CLI   0(R9),C' '                                                       
         BH    DISP3                                                            
         BCT   R9,*-8                                                           
*                                                                               
DISP3    LA    R9,2(R9)                                                         
*                                                                               
DISP4    LA    R7,BILBASB                                                       
         BAS   RE,FMTBAS                                                        
         B     F3SEND                                                           
*                                                                               
         DROP  R4                                                               
*                                                                               
*                                                                               
*                                                                               
F35      DS    0H                  COST 2 FACTOR                                
         CLI   PESTELEM,X'07'                                                   
         BE    *+6                                                              
         DC    H'0'                MAKE SURE IT'S THERE                         
         OC    PESTCF,PESTCF                                                    
         BZ    F35M                NO COS2 IN EST, USE CLT'S                    
*                                                                               
         EDIT  (P5,PESTCF),(8,FLDDATA+59),6,ALIGN=LEFT,FILL=0,DROP=5            
         B     F35XXX                                                           
*                                                                               
F35M     OC    SVCLTCOS,SVCLTCOS   DOES CLT HDR CONTAIN COST2 FACTOR?           
         BZ    F35XXX                                                           
         EDIT  (P5,SVCLTCOS),(8,FLDDATA+59),6,ALIGN=LEFT,FILL=0,DROP=5          
*                                                                               
F35XXX   B     F3SEND                                                           
*                                                                               
*                                                                               
*                                                                               
F36      DS    0H                  FUTURE USE                                   
*                                                                               
*                                                                               
*                                                                               
F3SEND   LA    RE,FLDDATA+27                                        L01         
         EDIT  (B2,PESTKEST),(3,(RE)),0,ALIGN=LEFT                              
         TM    PESTTEST,X'80'                                                   
         BZ    *+8                                                              
         MVI   FLDDATA+30,C'T'             TO INDICATE TEST EST     L01         
         TM    PESTTEST,X'01'                                                   
         BZ    *+8                                                              
         MVI   FLDDATA+31,C'S'             TO INDICATE "SFH" EST                
*                                                                               
         MVC   FLDDATA+32(2),PESTRSCH       RETAIL SCHEME           L01         
         MVC   FLDDATA+36(3),PESTGRPS         EST FILTERS           L01         
         CLI   FRSTTIM,0                                                        
         BE    F3SEND1                                                          
         CLC   SVPRD,=C'ALL'                                                    
         BE    *+8                                                              
         MVI   FRSTTIM,0                                                        
         MVC   FLDDATA+1(3),PESTKPRD                                            
         LA    R7,PESTKPRD                                                      
         BAS   RE,GETPRD                                                        
         MVC   FLDDATA+6(20),WORK                                               
F3SEND1  FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         LA    R5,KEY                                                           
         MVI   PESTKEST+2,X'FF'                                                 
         CLC   SVPRD,=C'ALL'       SEE IF ALL PRDS AND FIRST EST                
         BNE   F3SENDX                                                          
         CLC   EST1(4),=X'0001FFFE'                                             
         BNE   F3SENDX                                                          
         MVC   PESTKEST,=2X'FF'    TO GET NEXT PRODUCT                          
F3SENDX  DS    0H                                                               
         MVC   PREVKEY,KEY                                                      
         BCT   R6,F3EST                                                         
*                                                                               
         B     MODEXIT                                                          
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                               
GETNUM   LA    R5,0                                                             
GN1      CLI   0(R4),C'-'                                                       
         BER   R9                                                               
         CLI   0(R4),C','                                                       
         BER   R9                                                               
         CLI   0(R4),X'00'                                                      
         BER   R9                                                               
         CLI   0(R4),C' '                                                       
         BER   R9                                                               
         CLI   0(R4),C'0'                                                       
         BL    GNERR                                                            
         CLI   0(R4),C'9'                                                       
         BH    GNERR                                                            
         LA    R4,1(R4)                                                         
         LA    R5,1(R5)                                                         
         B     GN1                                                              
GNERR    SR    R5,R5                                                            
         BR    R9                                                               
         SPACE 2                                                                
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
GETFLD   LA    R5,0                                                             
GF1      CLI   0(R4),C','                                                       
         BER   R9                                                               
         CLI   0(R4),X'00'                                                      
         BER   R9                                                               
         CLI   0(R4),C' '                                                       
         BER   R9                                                               
         LA    R4,1(R4)                                                         
         LA    R5,1(R5)                                                         
         B     GF1                                                              
GFERR    SR    R5,R5                                                            
         BR    R9                                                               
*                                                                               
*                                                                               
*                                                                               
NODATAEX MVI   ERRCD,NOFNDERR                                                   
         LA    R3,NOFNDERR                                                      
         LA    R2,SINIKEYH                                                      
         B     ERROR                                                            
         B     MODEXIT                                                          
*                                                                               
* SEND FILTER ERROR MESSAGE                                                     
*                                                                               
FLTERR   BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SINMSG(0),0(R6)   * EXECUTED *                                   
         LA    RE,3(RE)                                                         
         LA    RF,SINMSG(RE)                                                    
         MVC   0(22,RF),=C'- INVALID FILTER FIELD'                              
         LA    R2,SINIFLTH                                                      
         FOUT  (R2)                                                             
         MVI   ERRAREA,X'FF'                                                    
         B     *+8                                                              
*                                                                               
MODEXIT  LA    R2,SINIKEYH                                                      
         OC    PREVKEY,PREVKEY                                                  
         BZ    *+8                                                              
         LA    R2,SINENDH                                                       
         OI    6(R2),X'C0'                                                      
         XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                               
* READ ESTIMATES AND FILTER THEM                                                
*                                                                               
***********************************************************************         
*                                                                               
GETEST   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING ESTHDRD,R5                                                       
         MVI   RECFLAG,0                                                        
         MVC   PESTKAGY,SVAGY                                                   
         MVC   PESTKMED,SVEBCMED                                                
         MVI   PESTKRCD,X'07'                                                   
         MVC   PESTKCLT,SVCLT                                                   
         CLC   SVPRD,=C'ALL'                                                    
         BE    *+16                   IF SVPRD = ALL SKIP SVESTB                
         MVC   PESTKPRD,SVPRD                                                   
         MVC   PESTKEST,SVESTB        START AT EST                              
         OC    PREVKEY,PREVKEY                                                  
         BZ    *+10                                                             
         MVC   KEY,PREVKEY                                                      
GEHIGH   BAS   RE,HIGH                                                          
         B     GEREC                                                            
*                                                                               
GESEQ    BAS   RE,SEQ                                                           
GEREC    LA    R5,KEY                                                           
         CLC   KEY(7),KEYSAVE                                                   
         BE    *+12                                                             
         MVI   RECFLAG,1                                                        
         B     GEEXIT                                                           
         CLC   SVPRD,=C'ALL'                                                    
         BE    GEFEST                                                           
         CLC   PESTKPRD,SVPRD                                                   
         BE    GEFEST                                                           
         MVI   RECFLAG,1                                                        
         B     GEEXIT                                                           
*                                                                               
* FILTER KEY ON ESTIMATE NUMBER                                                 
*                                                                               
GEFEST   DS    0H                                                               
         CLC   PESTKEST,EST1                                                    
         BNL   *+14                                                             
         MVC   PESTKEST,EST1                                                    
         B     GEHIGH                                                           
*                                                                               
         CLC   PESTKEST,EST2                                                    
         BH    GESEQ                                                            
         GOTO1 GETREC                                                           
         L     R5,AREC                                                          
         MVI   RECFLAG,0                                                        
*                                                                               
GEFEST4  CLI   SDATE,0                                                          
         BE    GEFEST5                                                          
         L     R5,AREC                                                          
         CLC   PESTEND,SDATE                                                    
         BL    GESEQ                                                            
         CLC   PESTST,EDATE                                                     
         BH    GESEQ                                                            
*                                                                               
GEFEST5  OC    CPROFLT,CPROFLT         SEE IF FILTERING ON PROFILE              
         BZ    GEFEST10            NO                                           
         LA    RF,PESTPROF                                                      
         LA    RE,CPROFLT                                                       
         LA    R9,32                                                            
*                                                                               
GEFEST6  CLI   0(RE),0                                                          
         BE    GEFEST8                                                          
         CLC   0(1,RF),0(RE)                                                    
         BNE   GESEQ                                                            
*                                                                               
GEFEST8  LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R9,GEFEST6                                                       
*                                                                               
GEFEST10 CLC   USERDAT1,=CL20' '   FILTERING ON USER                            
         BNH   GEFEST12                                                         
         LA    R1,USERDAT1                                                      
         BAS   RE,FNDUSER                                                       
         BNE   GESEQ                                                            
*                                                                               
GEFEST12 DS    0H                                                               
         CLC   USERDAT2,=CL20' '   FILTERING ON USER                            
         BNH   GEFEST15                                                         
         LA    R1,USERDAT2                                                      
         BAS   RE,FNDUSER                                                       
         BNE   GESEQ                                                            
*                                                                               
GEFEST15 DS    0H                                                               
         OC    FILTRSCH,FILTRSCH RETAIL SCHEME FILTER              L01          
         BZ    NOTESTSH                                            L01          
         CLC   FILTRSCH,PESTRSCH     AGREE WITH ESTIMATE           L01          
         BNE   GESEQ            GET NEXT ESTIMATE                  L01          
*                                                                  L01          
NOTESTSH DS    0H                                                  L01          
         CLI   LOCKSW,0                                                         
         BE    GEFEST18                                                         
         CLC   PESTSTAT,LOCKSW                                                  
         BE    GEFEST18                                                         
         B     GESEQ                                                            
*                                                                               
GEFEST18 CLI   TESTSW,0                                                         
         BE    GEFEST20                                                         
         CLI   TESTSW,X'08'        CHECK FOR ONLY LIVE                          
         BNE   GEFEST19         TESTSW MUST BE X'80' (TEST ESTS ONLY)           
         TM    PESTTEST,X'80'                                                   
         BNZ   GESEQ               SKIP TEST ESTS                               
         B     GEFEST20                                                         
*                                                                               
*****GEFEST19 CLC   PESTTEST,TESTSW                                             
*****    BE    GEFEST20                                                         
*****    B     GESEQ                                                            
*                                                                               
GEFEST19 DS    0H               TESTSW MUST BE X'80' (TEST ESTS ONLY)           
         TM    PESTTEST,X'80'                                                   
         BZ    GESEQ               SKIP LIVE ESTS                               
*                                                                               
GEFEST20 OC    FILT1(3),FILT1      CHK FILTERS                                  
         BZ    GEFEST50                                                         
         LA    RF,FILT1                                                         
         LA    RE,PESTGRPS                                                      
         LA    R9,3                FOR BCT                                      
GEFEST25 CLI   0(RF),0                                                          
         BE    GEFEST40            NOT FILTERING ON THIS POSITION               
         TM    0(RF),X'80'         CHK FOR NEGATIVE FILTER                      
         BZ    GEFEST30            YES                                          
         CLC   0(1,RF),0(RE)                                                    
         BNE   GESEQ               BYPASS THIS EST                              
         B     GEFEST40                                                         
*                                                                               
GEFEST30 MVC   BYTE,0(RF)                                                       
         OI    BYTE,X'80'          SET ON 80 BIT                                
         CLC   BYTE,0(RE)                                                       
         BE    GESEQ                                                            
*                                                                               
GEFEST40 LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         BCT   R9,GEFEST25                                                      
*                                                                               
GEFEST50 OC    FILTJOB,FILTJOB     SEE IF FILTERING ON JOB                      
         BZ    GEFEST55                                                         
         CLC   PESTJOB,FILTJOB                                                  
         BNE   GESEQ                                                            
*                                                                               
GEFEST55 OC    FILTREP,FILTREP     SEE IF FILTERING ON REP                      
         BZ    GEFEST60                                                         
         CLC   PESTREP,FILTREP                                                  
         BNE   GESEQ                                                            
*                                                                               
GEFEST60 OC    FILTRTYP,FILTRTYP   SEE IF FILTERING ON RATE TYPE                
         BZ    GEFEST65                                                         
         CLC   PESTRTYP,FILTRTYP                                                
         BNE   GESEQ                                                            
*                                                                               
GEFEST65 OC    FILTSFH,FILTSFH     SEE IF FILTERING ON SFH                      
         BZ    GEEXIT                                                           
         CLI   FILTSFH,C'Y'        IS FILTER Y ?                                
         BNE   GEFEST66            MUST BE N                                    
         TM    PESTTEST,X'01'      IS ESTIMATE SFH ?                            
         BNO   GESEQ               NO - SKIP                                    
         B     GEEXIT                                                           
GEFEST66 TM    PESTTEST,X'01'      FILTER IS N (NOT SFH)                        
         BO    GESEQ               IF EST IS SFH, SKIP                          
*                                                                               
GEEXIT   XIT1  1                                                                
         EJECT                                                                  
FMTBAS   NTR1                                                                   
         LA    R5,BASLST                                                        
         LA    R6,BASLSTN                                                       
FB2      CLC   0(1,R7),0(R5)                                                    
         BE    FB3                                                              
         LA    R5,5(R5)                                                         
         BCT   R6,FB2                                                           
         B     FBX                                                              
*                                                                               
FB3      MVC   0(4,R9),1(R5)                                                    
*                                                                               
FBX      XIT   XIT1                                                             
*                                                                               
*                                                                               
*                                                                               
BASLST   DS    0C                                                               
         DC    X'01',C'G   '                                                    
         DC    X'02',C'N   '                                                    
         DC    X'08',C'AC  '              **NEW 4/25/89                         
         DC    X'05',C'G-CD'                                                    
         DC    X'06',C'N-CD'                                                    
BASLSTN  EQU   (*-BASLST)/5        NUMBER IN LIST                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                               
GETPRD   NTR1                                                                   
         MVC   WORK2,KEY                                                        
         LA    R6,KEY                                                           
         USING PRDHDRD,R6                                                       
         XC    KEY,KEY                                                          
         MVC   PPRDKAGY,SVAGY                                                   
         MVC   PPRDKMED,SVEBCMED                                                
         MVI   PPRDKRCD,X'06'                                                   
         MVC   PPRDKCLT,SVCLT                                                   
         MVC   PPRDKPRD,0(R7)                                                   
         BAS   RE,READ                                                          
         BAS   RE,GETREC                                                        
         L     R6,AREC                                                          
         MVC   WORK(20),PPRDNAME                                                
         MVC   KEY(40),WORK2                                                    
         XIT1  1                                                                
         EJECT                                                                  
*                                                                               
*        GET USER=XXXXX INTO USERFIELD (R6)                                     
*        R4 - START OF USER FIELD                                               
*                                                                               
CKUDATA  NTR1                                                                   
         LR    R2,R4                                                            
         LA    R5,SINIFLT+64       END OF FILTER FIELD                          
         SR    R1,R1               COUNTER                                      
*                                                                               
CKU10    CR    R4,R5                                                            
         BNL   CKU30                                                            
         CLI   0(R4),C','                                                       
         BE    CKU30                                                            
         CLI   0(R4),C' '          SPACES ARE ALLOWED                           
         BNL   CKU20                                                            
         CLI   0(R4),C'-'          AND SO ARE DASHES                            
         BE    CKU20                                                            
         CLI   0(R4),C'*'          '*' IS A WILDCARD                            
         BNE   *+8                                                              
*                                                                               
CKU20    LA    R1,1(R1)            INCREMENT COUNTER                            
         LA    R4,1(R4)                                                         
         B     CKU10                                                            
*                                                                               
CKU30    LTR   R1,R1                                                            
         BZ    CKUX                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),0(R2)                                                    
*                                                                               
CKUX     XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        FILTERING ON USERDAT FIELD                                             
*        R1 - USERDATX FIELD                                                    
*                                                                               
FNDUSER  NTR1                                                                   
         LR    R2,R1                                                            
         LA    R4,PESTELEM                                                      
         USING PESTUDEF,R4                                                      
*                                                                               
FD10     ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         CLI   0(R4),0             END OF RECORD                                
         BE    FDNO                                                             
         CLI   0(R4),X'08'         UDEF ELEMENT                                 
         BNE   FD10                                                             
         LA    R1,20               MAX L'USERDATX FIELD                         
         LA    R6,PEUSER1                                                       
         CLI   USESW,1             SET R4 TO CORRECT USERDATA FIELD             
         BE    *+8                 NO                                           
         LA    R6,PEUSER2                                                       
*                                                                               
FD20     CLI   0(R2),0             END OF DATA                                  
         BE    FDYES                                                            
         CLI   0(R2),C'*'          WILDCARD                                     
         BE    FD30                                                             
         CLC   0(1,R2),0(R6)                                                    
         BNE   FDNO                                                             
*                                                                               
FD30     LA    R2,1(R2)            BUMP TO NEXT CHARACTER                       
         LA    R6,1(R6)                                                         
         BCT   R1,FD20                                                          
*                                                                               
FDYES    SR    RC,RC                                                            
FDNO     LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
GETUSER  NTR1                                                                   
         MVI   WORK2,C' '                                                       
         MVC   WORK2+1(31),WORK2                                                
         LA    R4,PESTELEM                                                      
         USING PESTUDEF,R4                                                      
*                                                                               
GU10     ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         CLI   0(R4),0             END OF RECORD                                
         BE    GUX                                                              
         CLI   0(R4),X'08'                                                      
         BNE   GU10                                                             
         CLI   USESW,1             IF USER 1 DATA                               
         BNE   GU20                NO                                           
         MVC   WORK2(32),PEUSER1                                                
         B     GUX                                                              
*                                                                               
GU20     MVC   WORK2(16),PEUSER2                                                
*                                                                               
GUX      XIT1                                                                   
         DROP  R4                                                               
*                                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
GETCLTU  NTR1                                                                   
         MVC   CUSER,SPACES                                                     
         MVC   CUSER(13),=C'* UNDEFINED *'                                      
         LA    R6,KEY                                                           
         USING CLTHDRD,R6                                                       
         XC    KEY,KEY                                                          
         MVC   PCLTKAGY,SVAGY                                                   
         MVC   PCLTKMED,SVEBCMED                                                
         MVI   PCLTKRCD,X'02'                                                   
         MVC   PCLTKCLT,SVCLT                                                   
         BAS   RE,READ                                                          
         BAS   RE,GETREC                                                        
         L     R6,AREC                                                          
         LA    R5,PCLTELEM                                                      
         USING PCLTUDEF,R5                                                      
*                                                                               
GC10     ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         CLI   0(R5),0             END OF RECORD                                
         BE    GCX                                                              
         CLI   0(R5),X'20'         UDEF ELEMENT                                 
         BNE   GC10                                                             
         LA    R1,PCLTEU1                                                       
         CLI   USESW,1             SET R4 TO CORRECT USERDATA FIELD             
         BE    *+8                 NO                                           
         LA    R1,PCLTEU2                                                       
         OC    0(20,R1),0(R1)      SEE IF DEFINED                               
         BZ    GCX                                                              
         MVC   CUSER,0(R1)                                                      
*                                                                               
GCX      XIT1  1                                                                
         EJECT                                                                  
*                                                                               
DASH     DC    40C'-'                                                           
SPACES   DC    40C' '                                                           
*                                                                               
***********************************************************************         
*                                                                               
GCLTCOS2 NTR1                      GET CLIENT COST 2 FACTOR                     
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CLTHDRD,R6                                                       
         MVC   PCLTKAGY,SVAGY                                                   
         MVC   PCLTKMED,SVEBCMED                                                
         MVI   PCLTKRCD,X'02'                                                   
         MVC   PCLTKCLT,SVCLT                                                   
         BAS   RE,READ                                                          
         BAS   RE,GETREC                                                        
         L     R6,AREC                                                          
         LA    R5,PCLTELEM                                                      
         USING PCLTELEM,R5                                                      
*                                                                               
         CLI   0(R5),X'02'                                                      
         BE    *+6                                                              
         DC    H'0'                HAS TO BE THERE                              
         TM    PCLTSTAT,X'08'                                                   
         BZ    GCCOS50                                                          
         MVI   FORMSW,2            DATA=COS2 IS VALID                           
         B     GCCOSX                                                           
         DROP  R5                                                               
*                                                                               
GCCOS50  MVI   FORMSW,0                                                         
*                                                                               
GCCOSX   XIT1                                                                   
*                                                                               
*                                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPGENEROL                                                      
ESTWRKD  DSECT                                                                  
EST1     DS    H                   ESTIMATE 1                                   
EST2     DS    H                   ESTIMATE 2                                   
RECFLAG  DS    C                   RECORD FLAG 0=FOUND 1=END                    
SDATE    DS    CL6                 START DATE                                   
EDATE    DS    CL6                 END DATE                                     
FRSTTIM  DS    C                   FIRST TIME                                   
MGSW     DS    C                   MAKEGOOD DATE                                
DEMOSW   DS    C                   DEMO FILTER                                  
FORMSW   DS    C                   DISPLAY ESTIMATE FORM                        
PROFSW   DS    C                   DISPLAY PROFILE                              
LOCKSW   DS    C                   DISPLAY LOCKED RECORDS ONLY                  
TESTSW   DS    C            X'80' =DISPLAY TEST ESTS ONLY                       
*                           X'08' =DISPLAY LIVE ESTS ONLY                       
*                           X'00' =DISPLAY BOTH                                 
HOLDSW   DS    C                   DISPLAY HELD RECORDS ONLY                    
JOBSW    DS    C                                                                
USESW    DS    C                                                                
*                                                                               
FLTSW    DS    XL1                                                              
*                                                                               
FILT1    DS    C                                                                
FILT2    DS    C                                                                
FILT3    DS    C                                                                
FILTJOB  DS    CL6                                                              
FILTREP  DS    CL4                                                              
FILTRTYP DS    C                   RATE TYPE FILTER                             
FILTSFH  DS    C                   SPECIAL FINANCIAL HANDLING FILTER            
FILTRSCH DS    CL2                 RETAIL FILTER SCHEME            L01          
MAXLINE  EQU   13                  MAXIMUM DATA LINES                           
MAXF1    EQU   28                  MAXIMUM FORMAT 1 ITEM                        
LINLEN   EQU   88                  LINE LENGTH                                  
NOFNDERR EQU   53                  RECORD NOT FOUND                             
*                                                                               
USERDATA DS    0CL40               USER FIELD FILTERS                           
USERDAT1 DS    CL20                                                             
USERDAT2 DS    CL20                                                             
*                                                                               
CPROFLT  DS    CL32                PROFLIE FILTERS                              
WORK2    DS    CL40                                                             
CUSER    DS    CL20                                                             
ESTWRKX  EQU   *                                                                
*                                                                               
BILPROFD DSECT                                                                  
       ++INCLUDE PBILPROF                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE PPRDREC                                                        
         EJECT                                                                  
*                                                                               
ESTHDRD  DSECT                                                                  
       ++INCLUDE PESTREC                                                        
*                                                                               
CLTHDRD  DSECT                                                                  
       ++INCLUDE PCLTREC                                                        
*                                                                               
       ++INCLUDE PPSINFOWRK                                                     

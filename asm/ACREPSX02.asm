*          DATA SET ACREPSX02  AT LEVEL 019 AS OF 10/21/14                      
*PHASE ACSX02A,*                                                                
*INCLUDE ACPAYCHK                                                               
*INCLUDE CLPACK                                                                 
*INCLUDE PUBVAL                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
ACSX02   TITLE 'SPOT/PRINT CLEARANCE UPDATE'                                    
ACSX02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACSX**,RA                                                    
         L     R9,0(R1)                                                         
         USING ACWORKD,R9          R9=A(GLOBAL W/S)                             
         LA    RC,SPACEND                                                       
         USING ACSXD,RC            RC=A(LOCAL W/S)                              
*                                                                               
         CLI   MODE,RUNFRST        FIRST FOR RUN                                
         BE    RUNF                                                             
         CLI   MODE,REQFRST        FIRST FOR REQUEST                            
         BE    REQF                                                             
         CLI   MODE,RUNLAST        LAST FOR RUN                                 
         BE    RUNL                                                             
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* RUNFRST FIRST                                                       *         
***********************************************************************         
         SPACE 1                                                                
RUNF     DS    0H                                                               
         ICM   R0,15,TOTLN         GET STORAGE FOR COUNT TABLE                  
         GETMAIN R,LV=(0)                                                       
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LR    R6,R1                                                            
         ST    R6,ACNTTAB          SAVE ADDRESS OF TABLE                        
         MVC   BPARM2,ACNTTAB      SET ADDRESS OF TABLE                         
*                                                                               
         LA    R3,FILEIN           OPEN INPUT FILES                             
RUNF5    ICM   R4,15,0(R3)                                                      
         OPEN  ((R4),INPUT)                                                     
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LA    R3,L'FILEIN(R3)                                                  
         CLI   0(R3),EOT                                                        
         BNE   RUNF5                                                            
*                                                                               
*              INITIALIZE ACPAYCHK BLOCK                                        
*                                                                               
         MVI   PCBACTN,PCBAPRCV    PROCESS RECOVERY RECORDS                     
         MVC   PCBCOMF,ADCOMFAC    A(COMFACS)                                   
         MVC   PCBCLPK,CLPACK      A(CLPACK)                                    
         MVC   PCBPUBV,PUBVAL      A(PUBVAL)                                    
         L     RE,ADMASTC                                                       
         MVC   PCBAUTL,MCUTL-MASTD(RE)  A(UTL)                                  
         LA    RE,IDTAB                                                         
         STCM  RE,15,PCBIDT        A(ID TABLE)                                  
         L     RF,=A(IDTABX)                                                    
         SR    RF,RE                                                            
         STCM  RF,3,PCBIDTL        LENGTH OF ID TABLE                           
         L     RE,ASEQTAB                                                       
         STCM  RE,15,PCBSQTB       A(SEQUENCE TABLE)                            
*                                                                               
         BAS   RE,BSYSN            BUILD SYSTEM NAME LIST                       
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS DAILY RECOVERY FILES                                        *         
***********************************************************************         
         SPACE 1                                                                
REQF     DS    0H                                                               
         LA    R3,FILEIN           READ INPUT FILES                             
         LA    R2,RECL             R2=A(INPUT IO AREA)                          
         ST    R2,PCBINPUT                                                      
*                                                                               
REQF10   L     R4,0(R3)            R4=A(INPUT DCB)                              
         MVC   FILTYP,4(R3)        SAVE FILE TYPE                               
*                                                                               
REQF20   GET   (R4),(R2)                                                        
         CLI   QOPT2,C' '                                                       
         BE    *+8                                                              
         BAS   RE,PDMP                                                          
*                                                                               
         L     RE,ASEQTAB          INITIALIZE SEQUENCE TABLE                    
         MVI   0(RE),X'FF'                                                      
*                                                                               
         GOTO1 ACPAYCHK,PCB        PROCESS RECOVERY RECORD                      
         BNE   REQF20              NOT NEEDED                                   
*                                                                               
         CLI   QOPT2,C' '                                                       
         BE    *+8                                                              
         BAS   RE,PDMP                                                          
*                                                                               
         TM    PCBTYPE,PCBTREC     IS THIS A CASH RECONCILIATION?               
         BO    *+12                                                             
         BAS   RE,PSORT            WRITE OUTPUT TO SORT                         
         B     REQF20                                                           
*                                                                               
         USING SEQTBD,R5                                                        
         L     R5,ASEQTAB          INITIALIZE SEQUENCE TABLE                    
REQF30   CLI   0(R5),X'FF'                                                      
         BE    REQF20                                                           
*                                                                               
         MVC   PCBSEQN,SEQNUM      UPDATE SEQUENCE NUMBER                       
         MVC   PCBCLRDT,SEQCLRDT   UPDATE CLEAR DATE                            
         MVC   PCBINV#,SEQINV#     INVOICE NUMBER                               
         MVC   PCBPRO,SEQPRO       PRODUCT 1                                    
         MVC   PCBPRO2,SEQPRO2     PRODUCT 2                                    
         MVC   PCBEST,SEQEST       ESTIMATE #                                   
         MVC   PCBBMKT,SEQBMKT     BINARY MARKET NUMBER                         
         CLI   PCBKSYS,PCBKSYPR    ARE WE DOING PRINT?                          
         BNE   *+20                                                             
         MVC   PCBKPUB,SEQPUB      PUB FOR PRINT                                
         MVC   PCBKPCLT,SEQPCLI    PRINT CLIENT                                 
         B     *+16                                                             
         MVC   PCBKSTN,SEQSTA      STATION FOR SPOT/NET                         
         MVC   PCBKSCLT,SEQSCLI    SPOT CLIENT                                  
*                                                                               
         BAS   RE,PSORT            WRITE OUTPUT TO SORT                         
         LA    R5,SEQTLNQ(R5)                                                   
         B     REQF30                                                           
         DROP  R5                                                               
*                                                                               
REQF40   CLOSE ((R4))              ** EODAD **                                  
         LA    R3,L'FILEIN(R3)                                                  
         CLI   0(R3),EOT                                                        
         BNE   REQF10                                                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PUT RECORDS TO SORT - ADD TO COUNT TABLE                            *         
***********************************************************************         
         SPACE 1                                                                
         USING RCVRECD,R2                                                       
PSORT    NTR1  ,                                                                
         OC    NCNT,NCNT           TEST FIRST TIME                              
         BNZ   PSORT3                                                           
         GOTO1 ADSORTER,DMCB,SORTCARD,RECCARD                                   
*                                                                               
PSORT3   GOTO1 ADSORTER,DMCB,=C'PUT',PCBKEY                                     
*                                                                               
         LA    RF,SPOTCNT          SPOT                                         
         CLI   PCBKSYS,PCBKSYSP                                                 
         BE    PSORT5                                                           
         LA    RF,NETCNT           NET                                          
         CLI   PCBKSYS,PCBKSYNE                                                 
         BE    PSORT5                                                           
         LA    RF,PRNTCNT          PRINT                                        
*                                                                               
PSORT5   ICM   R1,15,0(RF)         COUNT BY SYSTEM                              
         AHI   R1,1                                                             
         STCM  R1,15,0(RF)                                                      
*                                                                               
         L     R6,ACNTTAB                                                       
         USING CNTD,R6                                                          
         ICM   R0,15,NCNT          TEST FIRST TIME                              
         BZ    PSORT11                                                          
*                                                                               
PSORT7   CLC   CNTACSE,RCVSYSSE    ACC SE NUMBER                                
         BNE   PSORT9                                                           
         CLC   CNTALPHA,PCBKALPH   AGENCY ALPHA MUST MATCH                      
         BE    PSORT13                                                          
*                                                                               
PSORT9   AHI   R6,CNTLQ                                                         
         BCT   R0,PSORT7                                                        
*                                                                               
PSORT11  BAS   RE,BINADD           ADD NEW ITEM TO TABLE                        
         ICM   R6,15,BPARM1        R6=A(NEW ITEM)                               
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
*                                                                               
PSORT13  L     RF,TOTCNT           COUNT RECORDS                                
         AHI   RF,1                                                             
         ST    RF,TOTCNT                                                        
*                                                                               
         LA    R4,LDGRTAB          LEDGER TABLE                                 
PSORT15  CLC   PCBLDGR,0(R4)       MATCH LEDGER                                 
         BE    PSORT17                                                          
         LA    R4,L'LDGRTAB(R4)                                                 
         CLI   0(R4),EOT                                                        
         BNE   PSORT15                                                          
         DC    H'0'                UNKNOWN LEDGER                               
*                                                                               
PSORT17  SR    R0,R0                                                            
         IC    R0,1(R4)            DISP. TO ACCUMS FOR LEDGER                   
*                                                                               
         LA    R4,TYPTAB           TYPE TABLE                                   
PSORT19  MVC   BYTE,PCBTYPE                                                     
         NI    BYTE,PCBTCHK+PCBTREC+PCBTVOD+PCBTUNV                             
         CLC   BYTE,0(R4)                                                       
         BE    PSORT21                                                          
         LA    R4,L'TYPTAB(R4)                                                  
         CLI   0(R4),EOT                                                        
         BNE   PSORT19                                                          
         DC    H'0'                UNKNOWN TYPE                                 
*                                                                               
PSORT21  SR    R1,R1                                                            
         ICM   R1,3,1(R4)          DISP. TO ACCUMS FOR THIS TYPE                
*                                                                               
PSORT23  LA    RF,CNTD(R1)                                                      
         AR    RF,R0               ADD DISP. TO LEDGER ACCUMS                   
         BAS   RE,ADDEM            ADD TO TOTALS FOR LEDGER                     
*                                                                               
         LA    RF,CNTD(R1)                                                      
         AH    RF,=Y(LDGRTOTN-LDGRD)  ADD TO TOTAL FOR TYPE                     
         BAS   RE,ADDEM            ADD TO TOTALS FOR 'ALL' LEDGERS              
*                                                                               
         LA    RF,TYPTOT                                                        
         CR    RF,R4               TEST ALREADY ADDED TO TOTAL                  
         BE    XIT                                                              
         LR    R4,RF               R4=A(TOTAL LINE)                             
         B     PSORT21             ADD TO TOTAL                                 
*                                                                               
ADDEM    ICM   R3,15,0(RF)         ADD TO ITEM COUNT                            
         AHI   R3,1                                                             
         STCM  R3,15,0(RF)                                                      
         AP    4(L'LDGRPA,RF),PCBAMNT  ADD CASH                                 
         BR    RE                                                               
*                                                                               
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* RUNLAST                                                                       
***********************************************************************         
         SPACE 1                                                                
RUNL     DS    0H                                                               
         ICM   R3,15,NCNT                                                       
         BZ    RUNL5                                                            
         BAS   RE,BINADD           ADD TOTAL RECORD TO TABLE                    
         ICM   R6,15,BPARM1                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         ST    R6,ACNTTOT                                                       
*                                                                               
         L     R6,ACNTTAB          START OF AGENCY TABLE                        
         ICM   R3,15,NCNT                                                       
         BCTR  R3,0                                                             
         USING CNTD,R6                                                          
         BAS   RE,REPRT            PRINT AGENCY REPORT                          
         AHI   R6,CNTLQ            FOR EACH AGENCY                              
         BCT   R3,*-8                                                           
*                                                                               
         L     R6,ACNTTOT          SET FOR OVERALL TOTAL                        
         BAS   RE,REPRT            PRINT OVERALL TOTAL                          
*                                                                               
RUNL5    BAS   RE,PUTF             GET SORT RECORDS & PUT THEM TO FILE          
*                                                                               
         EDIT  TOTCNT,(12,P+2),0,COMMAS=YES,ZERO=NOBLANK                        
         MVC   P+15(30),=CL30'RECORDS'                                          
         GOTO1 ACREPORT                                                         
*                                                                               
         EDIT  SPOTCNT,(12,P+2),0,COMMAS=YES,ZERO=NOBLANK                       
         MVC   P+15(30),=CL30'SPOT RECORDS'                                     
         GOTO1 ACREPORT                                                         
*                                                                               
         EDIT  NETCNT,(12,P+2),0,COMMAS=YES,ZERO=NOBLANK                        
         MVC   P+15(30),=CL30'NET RECORDS'                                      
         GOTO1 ACREPORT                                                         
*                                                                               
         EDIT  PRNTCNT,(12,P+2),0,COMMAS=YES,ZERO=NOBLANK                       
         MVC   P+15(30),=CL30'PRINT RECORDS'                                    
         GOTO1 ACREPORT                                                         
*                                                                               
         ICM   R1,15,ACNTTAB                                                    
         ICM   R0,15,TOTLN                                                      
         FREEMAIN R,A=(1),LV=(0)                                                
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT TOTALS FOR AGENCY                                             *         
***********************************************************************         
                                                                                
         USING CNTD,R6                                                          
REPRT    NTR1  ,                                                                
         MVI   FORCEHED,C'Y'                                                    
         MVC   P(6),=C'TOTALS'                                                  
         CLI   CNTACSE,ALL                                                      
         BE    REPRT5                                                           
*                                                                               
         L     RF,ACNTTOT          RF=A(OVERALL ACCUMS)                         
         LA    RF,CNTCHCK-CNTD(RF)                                              
         LA    RE,CNTCHCK-CNTD(R6) RE=A(THIS RECORD ACCUMS)                     
         LHI   R0,CNTNQ*LDGRNQ     R0=NUMBER OF SETS OF ACCUMS                  
*                                                                               
REPRT3   ICM   R1,15,0(RF)         ADD THIS (RE) TO TOTAL (RF)                  
         ICM   R2,15,0(RE)                                                      
         AR    R1,R2                                                            
         STCM  R1,15,0(RF)                                                      
         AP    LDGRPA-LDGRD(L'LDGRPA,RF),LDGRPA-LDGRD(L'LDGRPA,RE)              
         LA    RE,LDGRCLNQ(RE)                                                  
         LA    RF,LDGRCLNQ(RF)                                                  
         BCT   R0,REPRT3                                                        
*                                                                               
         LA    RF,SYSTAB                                                        
         CLC   CNTACSE,SYSSE-SYSD(RF)  MATCH ACC SE NUMNER                      
         BE    *+12                                                             
         LA    RF,SYSLNQ(RF)                                                    
         B     *-14                                                             
*                                                                               
         MVC   P(7),SYSNAME-SYSD(RF)  ACC FILE NAME                             
         MVC   P+20(6),=C'AGENCY'                                               
         MVC   P+28(2),CNTALPHA      AGENCY ALPHA                               
*                                                                               
REPRT5   GOTO1 ACREPORT                                                         
*                                                                               
         LA    R0,CNTNQ            NUMBER OF TYPES                              
         LA    R2,CNTCHCK          ROW FOR EACH TYPE                            
         LA    R4,TYPTAB                                                        
*                                                                               
         BAS   RE,EDIT             EDIT BY LEDGER                               
         LA    R2,L'CNTCHCK(R2)                                                 
         LA    R4,L'TYPTAB(R4)                                                  
         BCT   R0,*-12                                                          
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* EDIT                                                                *         
***********************************************************************         
                                                                                
         USING LDGRD,R2                                                         
EDIT     NTR1  ,                                                                
         LA    R3,LDGRTAB                                                       
         LA    R6,LDGRNQ-1                                                      
*                                                                               
EDIT3    MVC   P(6),=C'LEDGER'                                                  
         MVC   P+7(1),0(R3)        LEDGER CODE                                  
         BAS   RE,EDIT9                                                         
         GOTO1 ACREPORT                                                         
         LA    R2,LDGRCLNQ(R2)                                                  
         LA    R3,L'LDGRTAB(R3)                                                 
         BCT   R6,EDIT3                                                         
*                                                                               
         MVC   P(13),3(R4)         TYPE TOTAL                                   
         BAS   RE,EDIT9                                                         
         MVI   PSECOND,0                                                        
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
*                                                                               
EDIT9    EDIT  LDGRPN,(12,P+22),0,COMMAS=YES,ZERO=NOBLANK                       
         EDIT  LDGRPA,(14,P+38),2,COMMAS=YES,ZERO=NOBLANK                       
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PUT RECORDS TO OUTPUT DATASET                                       *         
***********************************************************************         
                                                                                
PUTF     NTR1  ,                                                                
         LA    R3,FILEOUT          OPEN OUTPUT FILES                            
PUTF3    ICM   R4,15,1(R3)         OPEN DATA FILE                               
         OPEN  ((R4),OUTPUT)                                                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ICM   R4,15,5(R3)         OPEN TAPE COPY                               
         OPEN  ((R4),OUTPUT)                                                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,L'FILEOUT(R3)                                                 
         CLI   0(R3),EOT                                                        
         BNE   PUTF3                                                            
*                                                                               
PUTF5    OC    NCNT,NCNT           TEST ANY DATA                                
         BZ    PUTF12                                                           
         GOTO1 ADSORTER,DMCB,=C'GET'                                            
         L     R2,DMCB+4                                                        
         LTR   R2,R2                                                            
         BZ    PUTF11                                                           
*                                                                               
         MVC   PCBKEY(PCBDLNQ),0(R2)   MOVE TO WORK AREA                        
*                                                                               
         LA    R3,FILEOUT          FIND DCBS FOR THIS SYSTEM                    
PUTF7    CLC   0(1,R3),PCBKSYS                                                  
         BE    PUTF9                                                            
         LA    R3,L'FILEOUT(R3)                                                 
         CLI   0(R3),EOT                                                        
         BNE   PUTF7                                                            
         DC    H'0'                INVALID SYSTEM                               
*                                                                               
PUTF9    LA    R4,PCBKSEN          OUTPUT DATA STARTS WITH SE NUMBER            
         ICM   R2,15,1(R3)                                                      
         PUT   (R2),(R4)           PUT DATA TO OUTPUT FILE                      
         ICM   R2,15,5(R3)                                                      
         PUT   (R2),(R4)           PUT DATA TO COPY FILE                        
         B     PUTF5                                                            
*                                                                               
PUTF11   GOTO1 ADSORTER,DMCB,=C'END'    END SORT                                
*                                                                               
PUTF12   LA    R3,FILEOUT          CLOSE OUTPUT FILES                           
PUTF13   ICM   R4,15,1(R3)                                                      
         CLOSE ((R4))                                                           
         ICM   R4,15,5(R3)         CLOSE TAPE COPY                              
         CLOSE ((R4))                                                           
         LA    R3,L'FILEOUT(R3)                                                 
         CLI   0(R3),EOT                                                        
         BNE   PUTF13                                                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ADD NEW ITEM TO AGENCY TABLE                                        *         
***********************************************************************         
                                                                                
BINADD   NTR1  ,                                                                
         LA    R6,WRKCNT                                                        
         USING CNTD,R6                                                          
*                                                                               
         LR    R0,R6               R0=START OF TABLE                            
         LA    R1,CNTLQ            R1=LENGTH OF ENTRY                           
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR ENTRY                                  
*                                                                               
         LHI   R1,CNTNQ*LDGRNQ     R1=NUMBER ENTRIES PER AGENCY                 
         LA    R2,CNTCHCK+(LDGRPA-LDGRD)                                        
         ZAP   0(L'LDGRPA,R2),=P'0'                                             
         LA    R2,LDGRCLNQ(R2)                                                  
         BCT   R1,*-10                                                          
*                                                                               
         MVI   CNTACSE,ALL         ADD TOTAL RECORD AT RUN LAST                 
         CLI   MODE,RUNLAST                                                     
         BE    BINADD3                                                          
         MVC   CNTACSE,PCBACSE     ACC SE                                       
         MVC   CNTALPHA,PCBKALPH   AGENCY ALPHA                                 
*                                                                               
BINADD3  ST    R6,BPARM1           A(RECORD TO BE ADDED)                        
         OI    BPARM1,X'01'        INSERT IF NOT FOUND                          
         GOTO1 BINSRCH,BPARM                                                    
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD SYSTEM NAME LIST                                              *         
***********************************************************************         
                                                                                
BSYSN    NTR1  ,                                                                
         LA    R6,SYSTAB                                                        
         USING SYSD,R6                                                          
         LA    R5,REC              GET SYSTEM LIST RECORD                       
         USING CTWREC,R5                                                        
         XC    CTWKEY,CTWKEY                                                    
         MVI   CTWKTYP,C'W'                                                     
         MVI   CTWKREC,C'S'                                                     
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,CTFILE,REC,REC,0                             
         B     BSYSN4                                                           
*                                                                               
BSYSN3   GOTO1 DATAMGR,DMCB,DMRSEQ,CTFILE,REC,REC,0                             
BSYSN4   LA    R5,REC                                                           
         CLI   CTWKTYP,C'W'                                                     
         BNE   XIT                                                              
         CLI   CTWKREC,C'S'                                                     
         BNE   XIT                                                              
         CLI   CTWKSYSN,CTWKACC                                                 
         BNE   BSYSN3                                                           
*                                                                               
         LA    R5,CTWDATA                                                       
BSYSN5   CLI   0(R5),X'A4'         SYSTEM LIST ELEMENT                          
         BE    BSYSN9                                                           
         CLI   0(R5),0             END OF RECORD                                
         BE    BSYSN3                                                           
BSYSN7   SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     BSYSN5                                                           
*                                                                               
         USING CTLSTD,R5                                                        
BSYSN9   MVC   SYSSE,CTLSTDTA+8    SE NUMBER TO SYSTAB                          
         MVC   SYSNAME,CTLSTDTA    SYSTEM NAME TO SYSTAB                        
         LA    R6,SYSLNQ(R6)                                                    
         B     BSYSN7                                                           
         DROP  R5,R6                                                            
         EJECT                                                                  
***********************************************************************         
* PDUMP OF PCB BLOCK                                                  *         
***********************************************************************         
         SPACE 1                                                                
PDMP     NTR1  ,                                                                
         CLI   QOPT2,C'B'          DUMP BOTH RECORDS                            
         BNE   PDMP3                                                            
         L     R2,PCBINPUT         R2=A(INPUT RECORD)                           
         SR    RF,RF                                                            
         ICM   RF,3,0(R2)          RF=RECORD LENGTH                             
         GOTO1 PRNTBL,DMCB,0,(R2),C'DUMP',(RF),=C'2D',0                         
*                                                                               
PDMP3    LA    RF,PCBRTNL                                                       
         GOTO1 PRNTBL,DMCB,0,PCBRTN,C'DUMP',(RF),=C'2D',0                       
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CONSTANTS AND LITERAL POOL                                          *         
***********************************************************************         
         SPACE 1                                                                
ASEQTAB  DC    A(SEQTAB)           PAYABLE CHECK SEQUENCE NUM TABLE             
ACPAYCHK DC    V(ACPAYCHK)                                                      
CLPACK   DC    V(CLPACK)                                                        
PUBVAL   DC    V(PUBVAL)                                                        
PRNTBL   DC    V(PRNTBL)                                                        
*                                                                               
CTFILE   DC    C'CTFILE  '                                                      
*                                                                               
EOT      EQU   X'FF'                                                            
ALL      EQU   X'FF'                                                            
*                                                                               
FILEIN   DS    0XL5                                                             
         DC    AL4(RECVIN2),AL1(FIL133)                                         
         DC    AL4(RECVIN),AL1(FIL135)                                          
         DC    AL1(EOT)                                                         
*                                                                               
FILEOUT  DS    0XL9                                                             
         DC    C'S',AL4(SPOTDATA),AL4(SPOTTAPE)                                 
         DC    C'N',AL4(NETDATA),AL4(NETTAPE)                                   
         DC    C'P',AL4(PRNTDATA),AL4(PRNTTAPE)                                 
         DC    AL1(EOT)                                                         
*                                                                               
SORTFRST DC    C'Y'                                                             
SORTCARD DC    CL80'SORT FIELDS=(1,14,A),FORMAT=BI,EQUALS'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(50)'                                  
*                                                                               
TOTLN    DC    AL4((CNTMAX)*CNTLQ)                                              
*                                                                               
TYPTAB   DS    0XL16                                                            
         DC    AL1(PCBTCHK),AL2(CNTCHCK-CNTD)                                   
         DC    CL13'CHECKS       '                                              
         DC    AL1(PCBTVOD),AL2(CNTVOID-CNTD)                                   
         DC    CL13'VOIDS        '                                              
         DC    AL1(PCBTUNV),AL2(CNTUNVD-CNTD)                                   
         DC    CL13'UNVOIDS/OTHER'                                              
         DC    AL1(PCBTREC),AL2(CNTRCNL-CNTD)                                   
         DC    CL13'RECONCILES   '                                              
TYPTOT   DC    AL1(EOT),AL2(CNTTOTL-CNTD)                                       
         DC    CL13'TOTAL RECORDS'                                              
*                                                                               
LDGRTAB  DS    0XL2                                                             
         DC    C'P',AL1(LDGRPN-LDGRD)                                           
         DC    C'Q',AL1(LDGRQN-LDGRD)                                           
         DC    C'U',AL1(LDGRUN-LDGRD)                                           
         DC    C'S',AL1(LDGRSN-LDGRD)                                           
         DC    C'T',AL1(LDGRTN-LDGRD)                                           
         DC    AL1(EOT)                                                         
         EJECT                                                                  
BPARM    DS    0F                  BINSRCH PARAMETERS                           
BPARM1   DC    AL4(0)              A(RECORD TO BE ADDED)                        
BPARM2   DC    AL4(0)              A(TABLE)                                     
BPARM3   DC    AL4(0)                                                           
         ORG   BPARM3                                                           
NCNT     DS    F                   COUNT NUMBER OF AGENCYS IN TABLE             
BPARM4   DC    AL4(CNTLQ)          LENGTH OF RECORD                             
BPARM5   DC    AL4(L'CNTACSE+L'CNTALPHA)   LENGTH OF KEY                        
BPARM6   DC    AL4(CNTMAX)         MAX NUMBER OF RECORDS                        
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DCBS                                                                *         
***********************************************************************         
                                                                                
SPOTDATA DCB   DDNAME=SPOTDATA,DSORG=PS,MACRF=PM,                      X        
               RECFM=FB,LRECL=PCBBLKQ,BLKSIZE=PCBBLKQ*10                        
*                                                                               
NETDATA  DCB   DDNAME=NETDATA,DSORG=PS,MACRF=PM,                       X        
               RECFM=FB,LRECL=PCBBLKQ,BLKSIZE=PCBBLKQ*10                        
*                                                                               
PRNTDATA DCB   DDNAME=PRNTDATA,DSORG=PS,MACRF=PM,                      X        
               RECFM=FB,LRECL=PCBBLKQ,BLKSIZE=PCBBLKQ*10                        
*                                                                               
SPOTTAPE DCB   DDNAME=SPOTTAPE,DSORG=PS,MACRF=PM,                      X        
               RECFM=FB,LRECL=PCBBLKQ,BLKSIZE=PCBBLKQ*10                        
*                                                                               
NETTAPE  DCB   DDNAME=NETTAPE,DSORG=PS,MACRF=PM,                       X        
               RECFM=FB,LRECL=PCBBLKQ,BLKSIZE=PCBBLKQ*10                        
*                                                                               
PRNTTAPE DCB   DDNAME=PRNTTAPE,DSORG=PS,MACRF=PM,                      X        
               RECFM=FB,LRECL=PCBBLKQ,BLKSIZE=PCBBLKQ*10                        
*                                                                               
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,MACRF=GM,                        X        
               RECFM=VB,LRECL=3000,EODAD=REQF40                                 
*                                                                               
RECVIN2  DCB   DDNAME=RECVIN2,DSORG=PS,MACRF=GM,                       X        
               RECFM=VB,LRECL=3000,EODAD=REQF40                                 
         EJECT                                                                  
***********************************************************************         
* TABLES                                                              *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                  ID TABLE                                     
*                                                                               
* TABLE 1 - ID TABLE                                                            
*                                                                               
         DC    CL8'*IDTAB**'                                                    
IDTAB    DS    1000CL(IDLNQ)       ALPHA ID/SE # TABLE                          
IDTABX   EQU   *                                                                
*                                                                               
* TABLE 2 - PAYABLE SEQUENCE NUMBER TABLE                                       
*                                                                               
         DC    CL8'*SEQTAB*'       SEQUENCE TABLES                              
SEQTAB   DS    XL(SEQTLNQ*SEQTMAX) SEQ NUMBER AND AMOUNT                        
         EJECT                                                                  
***********************************************************************         
* DSECT FOR LOCAL STORAGE                                             *         
***********************************************************************         
                                                                                
ACSXD    DSECT                     LOCAL WORKING STORAGE                        
       ++INCLUDE ACPAYCHKD                                                      
*                                                                               
FILTYP   DS    X                   FILE TYPE                                    
FIL133   EQU   X'80'               133 RECOVERY (ONLINE)                        
FIL135   EQU   X'40'               135 RECOVERY (AFTER UPDATE)                  
*                                                                               
*                                                                               
TOTCNT   DS    F                   COUNT OF OUTPUT RECORDS                      
SPOTCNT  DS    F                   COUNTER OF SPOT RECORDS                      
NETCNT   DS    F                   NET                                          
PRNTCNT  DS    F                   PRINT                                        
*                                                                               
ACNTTAB  DS    A                   A(START OF THE COUNT TABLE)                  
ACNTTOT  DS    A                   A(ENTRY FOR THE COUNT TOTALS)                
*                                                                               
SYSTAB   DS    (SYSLNQ*20)C        SYSTEM NAME TABLE                            
*                                                                               
WRKCNT   DS    XL(CNTLQ)           WORK RECORD                                  
*                                                                               
RECL     DS    F                   INPUT RECORD LENGTH                          
REC      DS    2100X               INPUT RECORD                                 
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER TABLE OF COUNTERS                                    *         
***********************************************************************         
                                                                                
CNTD     DSECT                                                                  
CNTACSE  DS    XL1                 ACC SE NUMBER                                
CNTALPHA DS    CL2                 AGENCY ALPHA                                 
         DS    XL1                 N/D                                          
CNTCHCK  DS    XL(LDGRLNQ)         CHECKS                                       
CNTVOID  DS    XL(LDGRLNQ)         VOIDS                                        
CNTUNVD  DS    XL(LDGRLNQ)         UNVOIDS                                      
CNTRCNL  DS    XL(LDGRLNQ)         RECONCILIATIONS                              
CNTTOTL  DS    XL(LDGRLNQ)         TOTALS                                       
CNTNQ    EQU   (*-CNTCHCK)/(L'CNTTOTL) NUMBER                                   
CNTLQ    EQU   *-CNTD                                                           
*                                                                               
CNTMAX   EQU   275                 MAX NUMBER OF AGENCYS                        
*                                                                               
*                                                                               
* DSECT TO COVER ROW OF ACCUMULATORS                                            
*                                                                               
LDGRD    DSECT                                                                  
LDGRPN   DS    F                   LEDGER 'P' NUMBER                            
LDGRPA   DS    PL8                            AMOUNT                            
LDGRQN   DS    F                   LEDGER 'Q' NUMBER                            
LDGRQA   DS    PL8                            AMOUNT                            
LDGRUN   DS    F                   LEDGER 'U' NUMBER                            
LDGRUA   DS    PL8                            AMOUNT                            
LDGRSN   DS    F                   LEDGER 'S' NUMBER                            
LDGRSA   DS    PL8                            AMOUNT                            
LDGRTN   DS    F                   LEDGER 'T' NUMBER                            
LDGRTA   DS    PL8                            AMOUNT                            
LDGRTOTN DS    F                   TOTAL      NUMBER                            
LDGRTOTA DS    PL8                            AMOUNT                            
LDGRLNQ  EQU   *-LDGRD                                                          
LDGRCLNQ EQU   (L'LDGRPN+L'LDGRPA) WIDTH                                        
LDGRNQ   EQU   LDGRLNQ/LDGRCLNQ    NUMBER                                       
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER ID TABLE                                             *         
***********************************************************************         
                                                                                
IDD      DSECT                     ALPHA ID/SE # TABLE DSECT                    
IDALPH   DS    CL2                 ALPHA ID                                     
IDCTRY   DS    CL1                 COUNTRY                                      
IDSPSE   DS    XL1                 SPOT SE NUMBER                               
IDNESE   DS    XL1                 NET SE NUMBER                                
IDPRSE   DS    XL1                 PRINT SE NUMBER                              
IDACSE   DS    XL1                 ACC SE NUMBER                                
IDLNQ    EQU   *-IDD                                                            
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER LIST OF SYSTEM FILES(SYSTAB)                         *         
***********************************************************************         
         SPACE 1                                                                
SYSD     DSECT                                                                  
SYSSE    DS    CL1                 SE NUMBER                                    
SYSNAME  DS    CL7                 NAME (ACC1,SPOTB...ETC)                      
SYSLNQ   EQU   *-SYSD                                                           
         EJECT                                                                  
***********************************************************************         
* ++INCLUDES                                                          *         
***********************************************************************         
         SPACE 1                                                                
* ACPAYSEQD                                                                     
       ++INCLUDE ACPAYSEQD                                                      
         EJECT                                                                  
* ACRCVRECD                                                                     
* ACGENFILE                                                                     
* ACGENMODES                                                                    
* ACREPWORKD                                                                    
* CTGENFILE                                                                     
* DMDTFIS                                                                       
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACRCVRECD                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DMDTFIS                                                        
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019ACREPSX02 10/21/14'                                      
         END                                                                    

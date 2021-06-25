*          DATA SET CTCONOSA   AT LEVEL 002 AS OF 07/06/09                      
*PHASE CONOSAA                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE DMUTLCT                                                                
*INCLUDE CARDS                                                                  
*INCLUDE XSORT                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*INCLUDE SORTER                                                                 
***********************************************************************         
*    CTFILE CONVERSION FOR CIP TERMINAL TO OSA.                       *         
***********************************************************************         
*THIS IS BATCH JOB.                                                   *         
*IT SHOULD BE RUN AFTER CTFILE IS DUMPED.                             *         
*THE OUTPUT WILL BE IN SORTED ORDER.                                  *         
*SO, THE OUTPUT DATASET CAN BE LOADED BACK TO CTFILE DIRECTLY.        *         
***********************************************************************         
*                                                                     *         
*PARMS FOR DIFFERENT OPTIONS.  (CAN BE MIXED.)                        *         
*1A)COPY ONE CIP TERMINAL TO ONE OSA TERMINAL (W/ REPLACE)            *         
*   COPY    KZTN100T  TO  KZTNB00T                                    *         
*   COPY    KZTN100T  TO  KZTNB00T   REPLACE                          *         
*                                                                     *         
*1B)COPY A GROUP OF CIP TERMINALS TO A GROUP OF OSA TERMINALS         *         
*   COPY*   KZTN10*T  TO  KZTNB1*T                                    *         
*   COPY*   KZTN1**T  TO  KZTNB**T                                    *         
*   COPY*   KZTN1**T  TO  KZTNB**T   REPLACE                          *         
*                                                                     *         
*2) COMPARE TERMINALS' USERID LIST                                    *         
*   COMP    KZTN100T  TO  KZTNB00T                                    *         
*   COMP*   KZTN1**T  TO  KZTNB**T                                    *         
*   NOTE: USE 'COMPARE ONLY' AS PARM TO SKIP CTFILE UPDATE            *         
*                                                                     *         
*3) DELETE TERMINAL(S)                                                *         
*   DELETE  KZTN100T                                                  *         
*   DELETE* KZTN1**T                                                  *         
*                                                                     *         
***********************************************************************         
YYUNOSA  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE WORKX-WORKD,**OSA***,RA,WORK=A(WORKC),CLEAR=YES                  
         ENTRY UTL                 FOR DATAMGR                                  
         ENTRY SSB                                                              
         TITLE 'TERMINAL CONVERSION TO OSA'                                     
         USING WORKD,RC            RC=A(GLOBAL W/S)                             
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
         B     MAIN                                                             
         EJECT                                                                  
***********************************************************************         
* MAIN CONTROL CODE                                                   *         
***********************************************************************         
MAIN     BRAS  RE,VALCARDS         VALIDATE JCL CARD DATA LINE                  
         BNE   MERR                  EXIT IF ERROR                              
*                                                                               
         BRAS  RE,GENINIT          GENERAL INTIALISATION                        
         BRAS  RE,READIN           READ TERMINAL LIST INPUT                     
         BRAS  RE,COMPTERM         CHECK TERMINAL TABLE FOR COMPARSION          
         BRAS  RE,CHKTRMTB         CHECK TERMINAL TABLE                         
         CLI   COMPONLY,C'Y'                                                    
         BE    MXIT                                                             
*                                                                               
*        BRAS  RE,PRTRMTAB    ***********FOR TRACE**********                    
*                                                                               
         GOTO1 VSORTER,PARM,SORTCRD1,SORTCRD2,0                                 
*                                                                               
         BRAS  RE,READCTFL         READ CTFILE AND PUT RECS TO SORTER           
*                                                                               
         BRAS  RE,WRITEOUT         WRITE RECORDS FROM SORTER TO OUTPUT          
         B     MXIT                                                             
*                                                                               
MERR     MVI   RETCODE,8                                                        
*                                                                               
*                                                                               
MXIT     XBASE RC=RETCODE,RL=1                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
* GENERAL INITIALISATION                                              *         
* 1) INITIALIZE TERMINAL LIST TABLE                                   *         
*                                                                     *         
***********************************************************************         
GENINIT  NTR1  ,                                                                
*        MVC   P(80),=CL80'GENINIT'                                             
*        GOTO1 VPRINTER                                                         
*                                                                               
         MVC   LINE#,=H'0'                                                      
         MVI   ERROR,C'N'          SET ERROR TO N                               
*                                  INITIALIZE TERMINAL LIST TABLE               
         ICM   RE,15,=A(TRMTAB)                                                 
         STCM  RE,15,ATRMTAB                                                    
         STCM  RE,15,ATRMTABP                                                   
         ICM   RF,15,=AL4(TRMTABX-TRMTAB)                                       
         LA    R0,*                                                             
         L     R1,=F'0'                                                         
         MVCL  RE,R0                                                            
         ICM   RE,15,=A(TRMTABX)                                                
         STCM  RE,15,ATRMTABX                                                   
*                                                                               
*        MVC   P(80),=CL80'TERMINAL TABLE INITIALIZED'                          
*        GOTO1 VPRINTER                                                         
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMOPEN,CONTROL,FLISTCTF,IO                         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        MVC   P(80),=CL80'CTFILE OPENED'                                       
*        GOTO1 VPRINTER                                                         
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(3,TODAYYMD)                                  
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE JCL CARDS                                                  *         
***********************************************************************         
VALCARDS NTR1                                                                   
VCLP1    GOTO1 VCARDS,DMCB,P,=C'RE00'                                           
         CLC   =C'/*',P            IF END OF JCL                                
         BE    VCNO                  CHECK REQUIRED CARDS INPUT                 
*                                                                               
         CLC   =C'DSPACE=A',P                                                   
         BE    VCYES                                                            
         CLC   =C'DSPACE=C',P                                                   
         BE    VCYES                                                            
         CLC   =C'DSPACE=Q',P                                                   
         BE    VCYES                                                            
         CLC   =C'DSPACE=T',P                                                   
         BE    VCYES                                                            
         B     VCNO                                                             
*                                                                               
VCYES    EQU   *                                                                
         L     RF,=A(SSB)                                                       
         MVC   SSODSPAC-SSOOFF(1,RF),P+7                                        
         B     YES                                                              
VCNO     B     NO                  EXIT ERROR CONDITION                         
         EJECT                                                                  
***********************************************************************         
* READ IN TERMINAL LIST AND BUILT TRMLSTAB                            *         
***********************************************************************         
READIN   NTR1  ,                                                                
*        MVC   P(80),=CL80'READ IN TERMINAL LIST INPUT'                         
*        GOTO1 VPRINTER                                                         
         OPEN  (TRMLIST,INPUT)                                                  
*        MVC   P(80),=CL80'READIN OPENED'                                       
*        GOTO1 VPRINTER                                                         
*                                                                               
READ010  EQU   *                                                                
         GET   TRMLIST,WORK                                                     
         B     READ020                                                          
EODADDI  EQU   *                                                                
         SR    R0,R0               PREPARE FOR DIVIDE                           
         L     R1,ATRMTABP                                                      
         S     R1,ATRMTAB                                                       
         BNZ   *+6                                                              
         DC    H'0'                NO INPUT AT ALL                              
         D     R0,=AL4(TRMTTLQ)    R1=RESULT                                    
         ST    R1,TRMENTY#                                                      
*                                                                               
*        MVC   P(80),=CL80'READIN EOD'                                          
*        GOTO1 VPRINTER                                                         
         CLI   ERROR,C'Y'          ANY PARMS ERROR?                             
         BNE   READOK              NO - GOOD, EXIT                              
         DC    H'0'                                                             
*                                                                               
READ020  EQU   *                                                                
         CLC   =C'COMPARE ONLY',WORK                                            
         BNE   *+12                                                             
         MVI   COMPONLY,C'Y'                                                    
         B     READ010                                                          
*                                                                               
         BRAS  RE,READFLD                                                       
         BRAS  RE,VALFLD                                                        
         BRAS  RE,ADDTTAB                                                       
         B     READ010                                                          
*                                                                               
READOK   XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* PRINT OUT WHOLE TERM TABLE                                          *         
***********************************************************************         
PRTRMTAB NTR1  ,                                                                
         L     R3,ATRMTAB                                                       
         L     R2,TRMENTY#                                                      
PTT10    MVC   P(TRMTTLQ),0(R3)                                                 
         GOTO1 VPRINTER                                                         
         AHI   R3,TRMTTLQ                                                       
         BCT   R2,PTT10                                                         
PTTX     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* CHECK TERMINAL TABLE FOR DUP/EXITED TARGET TERM                     *         
***********************************************************************         
CHKTRMTB NTR1  ,                                                                
*        MVC   P(80),=CL80'CHECK TERMINAL TABLE'                                
*        GOTO1 VPRINTER                                                         
*                                                                               
         CLC   TRMENTY#,=F'1'                                                   
         BNH   CTT80               ONLY 1 ENTRY, SKIP DUPS CHECK                
*                                                                               
         L     R2,TRMENTY#         SORT W/SOURE TERMINAL                        
         LHI   R3,TRMTTLQ                                                       
         LHI   R4,TRMTTLQ-(TRMTFR-TRMTABD)                                      
         LHI   R5,TRMTFR-TRMTABD                                                
         GOTO1 VXSORT,DMCB,ATRMTAB,(R2),(R3),(R4),(R5),0                        
*                                                                               
*                                  FIND DUPS: SOURCE TERMS                      
         L     R3,ATRMTAB          A(TERM TABLE)                                
         LA    R4,TRMTTLQ(,R3)     A(2ND TERM TABEL ENTRY)                      
         L     R2,TRMENTY#         # TABLE ENTRY                                
         BCTR  R2,0                -1                                           
*                                                                               
CTT20    EQU   *                                                                
         CLI   TRMTACT-TRMTABD(R3),C'='   IGNORE IT IF ACTION=COMPARE           
         BE    CTT30                                                            
         CLC   TRMTFR-TRMTABD(L'TRMTFR,R3),TRMTFR-TRMTABD(R4)                   
         BNE   CTT30               NOT SAME, THEN NEXT ENTRY                    
         MVI   TRMTFRD-TRMTABD(R3),C'Y'    MARK BOTH DUP'ED                     
         MVI   TRMTFRD-TRMTABD(R4),C'Y'                                         
CTT30    AHI   R3,TRMTTLQ                                                       
         AHI   R4,TRMTTLQ                                                       
         BCT   R2,CTT20                                                         
*                                  PRINT OUT DUP SOURCE TERMS                   
         L     R3,ATRMTAB                                                       
         L     R2,TRMENTY#                                                      
CTT35    CLI   TRMTFRD-TRMTABD(R3),C'Y'                                         
         BNE   CTT35J                                                           
         MVC   P(5),=CL5'LINE#'                                                 
         SR    RF,RF                                                            
         ICM   RF,3,TRMTLN#-TRMTABD(R3)                                         
         EDIT  (RF),(5,P+5),ALIGN=LEFT,ZERO=NOBLANK                             
         MVC   P+11(40),=CL40'SOURCE TERMINAL DUPLICATED:'                      
         MVC   P+41(L'TRMTFR),TRMTFR-TRMTABD(R3)                                
         GOTO1 VPRINTER                                                         
         MVI   ERROR,C'Y'          SET DUP ERROR                                
CTT35J   AHI   R3,TRMTTLQ                                                       
         BCT   R2,CTT35                                                         
*                                                                               
*                                                                               
         L     R2,TRMENTY#         SORT W/TARGET TERMINAL                       
         LHI   R3,TRMTTLQ                                                       
         LHI   R4,TRMTTLQ-(TRMTTO-TRMTABD)                                      
         LHI   R5,TRMTTO-TRMTABD                                                
         GOTO1 VXSORT,DMCB,ATRMTAB,(R2),(R3),(R4),(R5),0                        
*                                                                               
*                                  FIND DUPS: TARGET TERMS                      
         L     R3,ATRMTAB          A(TERM TABLE)                                
         LA    R4,TRMTTLQ(,R3)     A(2ND TERM TABEL ENTRY)                      
         L     R2,TRMENTY#         # TABLE ENTRY                                
         BCTR  R2,0                -1                                           
*                                                                               
CTT40    EQU   *                                                                
         CLI   TRMTACT-TRMTABD(R3),C'='   IGNORE IT IF ACTION=COMPARE           
         BE    CTT50                                                            
         CLI   TRMTACT-TRMTABD(R3),C'D'    ACTION=DELETE                        
         BE    CTT50                       YES, NO TARGET TERMS                 
         CLC   TRMTTO-TRMTABD(L'TRMTTO,R3),TRMTTO-TRMTABD(R4)                   
         BNE   CTT50               NOT SAME, THEN NEXT ENTRY                    
         MVI   TRMTTOD-TRMTABD(R3),C'Y'    MARK BOTH DUP'ED                     
         MVI   TRMTTOD-TRMTABD(R4),C'Y'                                         
CTT50    AHI   R3,TRMTTLQ                                                       
         AHI   R4,TRMTTLQ                                                       
         BCT   R2,CTT40                                                         
*                                  PRINT OUT DUP TARGET TERMS                   
         L     R3,ATRMTAB                                                       
         L     R2,TRMENTY#                                                      
CTT55    CLI   TRMTTOD-TRMTABD(R3),C'Y'                                         
         BNE   CTT55J                                                           
         MVC   P(5),=CL5'LINE#'                                                 
         SR    RF,RF                                                            
         ICM   RF,3,TRMTLN#-TRMTABD(R3)                                         
         EDIT  (RF),(5,P+5),ALIGN=LEFT,ZERO=NOBLANK                             
         MVC   P+11(40),=CL40'TARGET TERMINAL DUPLICATED:'                      
         MVC   P+41(L'TRMTTO),TRMTTO-TRMTABD(R3)                                
         GOTO1 VPRINTER                                                         
         MVI   ERROR,C'Y'          SET DUP ERROR                                
CTT55J   AHI   R3,TRMTTLQ                                                       
         BCT   R2,CTT55                                                         
*                                                                               
CTT80    EQU   *                                                                
         CLI   ERROR,C'Y'          ANY DUPS?                                    
         BNE   CTTX                NO - GOOD, EXIT                              
         DC    H'0'                                                             
CTTX     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* READ A LINE INPUT AND FILL IN THE PARMS                             *         
***********************************************************************         
READFLD  NTR1  ,                                                                
*        MVC   P(80),=CL80'READ FIELDS'                                         
*        GOTO1 VPRINTER                                                         
*                                                                               
         XC    P1,P1                                                            
         XC    P2,P2                                                            
         XC    P3,P3                                                            
         XC    P4,P4                                                            
         XC    P5,P5                                                            
*                                                                               
         MVI   WORK+70,X'FF'       MARK END OF THE LINE                         
         LA    R1,WORK                                                          
         LA    R2,P1                                                            
         LA    R3,5                NUMBER OF PARMS                              
*                                                                               
RDFLD10  CLI   0(R1),X'FF'                                                      
         BE    RDFLDX                                                           
         CLI   0(R1),C' '                                                       
         BNE   *+12                                                             
         AHI   R1,1                                                             
         B     RDFLD10                                                          
*                                                                               
         MVC   0(8,R2),0(R1)       SAVE THIS PARM                               
         BCT   R3,*+8                                                           
         B     RDFLDX              ONLY GET 5 PARMS                             
*                                                                               
         AHI   R2,8                NEXT PARM SAVEAREA                           
RDFLD20  AHI   R1,1                BUMP TO NEXT FIELD                           
         CLI   0(R1),X'FF'                                                      
         BE    RDFLDX                                                           
         CLI   0(R1),C' '                                                       
         BE    RDFLD10                                                          
         B     RDFLD20                                                          
*                                                                               
RDFLDX   EQU   *                                                                
*        MVC   P(10),=CL10'PARMS:'                                              
*        MVC   P+10(8),P1                                                       
*        MVC   P+20(8),P2                                                       
*        MVC   P+30(8),P3                                                       
*        MVC   P+40(8),P4                                                       
*        MVC   P+50(8),P5                                                       
*        GOTO1 VPRINTER                                                         
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE FIELDS PER LINE                                        *         
***********************************************************************         
VALFLD   NTR1  ,                                                                
*        MVC   P(80),=CL80'VALIDATE FIELDS'                                     
*        GOTO1 VPRINTER                                                         
*                                                                               
VFP1     CLC   =C'COPY ',P1                                                     
         BE    VFP2                                                             
         CLC   =C'COPY* ',P1                                                    
         BE    VFP2                                                             
         CLC   =C'DELETE ',P1                                                   
         BE    VFP2                                                             
         CLC   =C'DELETE* ',P1                                                  
         BE    VFP2                                                             
         CLC   =C'COMP ',P1                                                     
         BE    VFP2                                                             
         CLC   =C'COMP* ',P1                                                    
         BE    VFP2                                                             
         MVC   P+20(L'P1),P1                                                    
         B     VFERR                                                            
*                                                                               
VFP2     CLI   P2+7,C'T'           ONLY ALLOW TERMINAL TYPE NOW                 
         BNE   VFP2ERR                                                          
         CLC   P2,P4               SOURCE AND TARGET TERM MUST NOT SAME         
         BE    VFP2ERR                                                          
*                                  CHECK IF WILD CARD FOR COPYING               
         XC    TERMSTR,TERMSTR                                                  
         XC    TERMEND,TERMEND                                                  
         CLC   =C'COPY* ',P1                                                    
         BE    VFP2W                                                            
         CLC   =C'DELETE* ',P1                                                  
         BE    VFP2W4                                                           
         CLC   =C'COMP* ',P1                                                    
         BE    VFP2W4                                                           
         B     VFP3                                                             
*                                  WILD CARD FOR COPY                           
VFP2W    LA    R1,P2+L'P2-2        2ND LAST CHAR OF SOURCE TERMINAL             
         LA    R2,P4+L'P4-2        2ND LAST CHAR OF TARGET TERMINAL             
VFP2W1   CLC   0(1,R1),0(R2)       GO BACKWARD                                  
         BNE   VFP2W2               *** MUST CONTINUOUS                         
         CLI   0(R1),C'*'           *** MUST BE IN SAME COLUMNS BOTH            
         BNE   VFP2W2                                                           
         MVI   0(R1),X'00'         REPLACE '*' WITH X'00' ON BOTH               
         MVI   0(R2),X'00'                                                      
         BCTR  R1,0                                                             
         BCT   R2,VFP2W1                                                        
VFP2W2   CLI   0(R1),C'*'                                                       
         BE    VFP2WER                                                          
         CLI   0(R2),C'*'                                                       
         BE    VFP2WER                                                          
         MVC   TERMSTR,P2          SAVE START TERM FROM P2                      
         MVI   1(R1),X'FF'         EDIT P2 TO MAKE END TERM                     
         MVC   TERMEND,P2          SAVE END TERM                                
         MVC   P2,TERMSTR          RESTORE START TERM BACK TO P2                
         B     VFP3                                                             
*                                                                               
VFP2W4   EQU   *                   WILD CARD FOR DELETE                         
         LA    R1,P2+L'P2-2        2ND LAST CHAR OF SOURCE TERMINAL             
         LA    R2,L'P2-2                                                        
         CLI   0(R1),C'*'                                                       
         BNE   VFP2WER                                                          
*                                                                               
VFP2W41  CLI   0(R1),C'*'                                                       
         BNE   VFP2W42                                                          
         MVI   0(R1),X'00'         REPLACE '*' WITH X'00'                       
         BCTR  R1,0                                                             
         BCT   R2,VFP2W41                                                       
         B     VFP2WER                                                          
VFP2W42  MVC   TERMSTR,P2          SAVE START TERM FROM P2                      
         MVI   1(R1),X'FF'         EDIT P2 TO MAKE END TERM                     
         MVC   TERMEND,P2          SAVE END TERM                                
         MVC   P2,TERMSTR          RESTORE START TERM BACK TO P2                
         B     VFP3                                                             
*                                                                               
VFP2WER  MVC   P+40(20),=CL20'WILD CARD ERROR'                                  
*                                                                               
VFP2ERR  MVC   P+20(L'P2),P2                                                    
         B     VFERR                                                            
*                                                                               
VFP3     EQU   *                                                                
         CLC   =C'DELETE',P1                                                    
         BNE   VFP301              'DELETE' ONLY HAS 2 PARM                     
         MVC   P3,TERMEND          REPLACE W/END TERM FOR WILD CARD             
         B     VFX                                                              
VFP301   CLC   =C'TO ',P3                                                       
         BNE   VFP302                                                           
         MVC   P3,TERMEND          REPLACE W/END TERM FOR WILD CARD             
         B     VFP4                  OTHERWISE P3 IS X'00'                      
VFP302   MVC   P+20(L'P3),P3                                                    
         B     VFERR                                                            
*                                                                               
VFP4     CLI   P4+7,C'T'           ONLY ALLOW TERMINAL TYPE NOW                 
         BE    VFP5                                                             
VFP4ERR  MVC   P+20(L'P4),P4                                                    
         B     VFERR                                                            
*                                                                               
VFP5     CLC   =C'COMP',P1                                                      
         BE    VFX                 'COMP' ONLY HAS 4 PARM                       
         CLC   =C'ADD ',P5                                                      
         BE    VFX                                                              
         CLC   =C'REPLACE ',P5                                                  
         BE    VFX                                                              
         OC    P5,P5                                                            
         BZ    VFX                                                              
VFP5X    MVC   P+20(L'P5),P5                                                    
         B     VFERR                                                            
*                                                                               
VFERR    MVC   P(20),=CL20'INVALID FIELD:'                                      
         GOTO1 VPRINTER                                                         
         MVI   ERROR,C'Y'                                                       
         B     VFXX                                                             
*                                                                               
VFX      EQU   *                                                                
*        MVC   P(10),=CL10'P PARMS:'                                            
*        MVC   P+10(8),P1                                                       
*        MVC   P+20(8),P2                                                       
*        MVC   P+30(8),P3                                                       
*        MVC   P+40(8),P4                                                       
*        MVC   P+50(8),P5                                                       
*        GOTO1 VPRINTER                                                         
*                                                                               
VFXX     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* ADD ENTRIES TO TERMINAL TABLE                                       *         
***********************************************************************         
ADDTTAB  NTR1  ,                                                                
*        MVC   P(80),=CL80'ADD TO TERMINAL TABLE'                               
*        GOTO1 VPRINTER                                                         
*                                                                               
         LH    RE,LINE#                                                         
         AHI   RE,1                                                             
         STH   RE,LINE#                                                         
*                                  FOR ACTION=COMPARE,TARGET MUST EXIST         
         CLC   =C'COMP',P1                                                      
         BNE   ADDTT02                                                          
         LA    R2,IO                                                            
         USING CTTREC,R2                                                        
         XC    CTTKEY,CTTKEY                                                    
         MVI   CTTKTYP,CTTKTYPQ                                                 
         MVC   CTTKTID,P4          TERMINAL                                     
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,CTTKEY,CTTKEY                        
         CLI   DMCB+8,0                                                         
         BE    ADDTT02                                                          
         MVC   P(40),=CL40'THIS TERMINAL RECORD DOES NOT EXIT:'                 
         MVC   P+40(8),P4                                                       
         GOTO1 VPRINTER                                                         
         MVI   ERROR,C'Y'                                                       
         B     ADDTTX                                                           
*                                                                               
ADDTT02  OC    P3,P3               ANY WILD CARD RANGE?                         
         BNZ   ADDTTW                                                           
*                                  IF NOT WILD CARD, ADD ONE & EXIT             
         LA    R2,IO                                                            
         XC    CTTKEY,CTTKEY                                                    
         MVI   CTTKTYP,CTTKTYPQ                                                 
         MVC   CTTKTID,P2          TERMINAL                                     
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,CTTKEY,CTTKEY                        
         CLI   DMCB+8,0                                                         
         BE    ADDTT05                                                          
         MVC   P(40),=CL40'THIS TERMINAL RECORD DOES NOT EXIT:'                 
         MVC   P+40(8),P2                                                       
         GOTO1 VPRINTER                                                         
         MVI   ERROR,C'Y'                                                       
         B     ADDTTX                                                           
*                                                                               
ADDTT05  L     R1,ATRMTABP                                                      
         USING TRMTABD,R1                                                       
         MVC   TRMTACT,P1                                                       
         CLC   =C'COMP',P1                                                      
         BNE   *+10                                                             
         MVC   TRMTACT,=C'='                                                    
         MVC   TRMTFR,P2                                                        
         MVC   TRMTTO,P4                                                        
         MVC   TRMTACT2,P5                                                      
         MVC   TRMTLN#,LINE#                                                    
         DROP  R1                                                               
         AHI   R1,TRMTTLQ                                                       
         ST    R1,ATRMTABP                                                      
         C     R1,ATRMTABX                                                      
         BL    ADDTTX                                                           
         DC    H'0'                EXCEED TABLE SIZE!                           
*                                                                               
ADDTTW   EQU   *                   WILD CARD ENTRY                              
*                                  READHI AND EXPAND SOURCE TERM LIST           
         L     R5,ATRMTABP         REMEMBER WHERE WE ARE                        
         LA    R2,IO                                                            
         USING CTTREC,R2                                                        
         XC    CTTKEY,CTTKEY                                                    
         MVI   CTTKTYP,CTTKTYPQ                                                 
         MVC   CTTKTID,P2          START TERMINAL                               
         GOTO1 VDATAMGR,DMCB,DMRDHI,CTFILE,CTTKEY,CTTKEY                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ADDTTW10 EQU   *                                                                
         CLC   CTTKTID+7(1),P2+7   MUST BE SAME TERMINAL TYPE                   
         BNE   ADDTTW20                                                         
         CLC   CTTKTID,P3          PASS END TERMINAL                            
         BH    ADDTTWX             YES, EXIT                                    
*                                                                               
         L     R1,ATRMTABP                                                      
         USING TRMTABD,R1                                                       
         MVC   TRMTACT,P1                                                       
         CLC   =C'COMP',P1                                                      
         BNE   *+10                                                             
         MVC   TRMTACT,=C'='                                                    
         MVC   TRMTFR,CTTKTID                                                   
         MVC   TRMTACT2,P5                                                      
         MVC   TRMTLN#,LINE#                                                    
         CLI   TRMTACT,C'D'        ACTION=DELETE                                
         BE    ADDTTW18            YES, NO TARGET TERM                          
         CLI   TRMTACT,C'='        ACTION=COMPARE                               
         BNE   ADDTTW12            NO                                           
         MVC   TRMTTO,P4           YES, SAME TARGET TERM                        
         B     ADDTTW18                                                         
*                                                                               
ADDTTW12 EQU   *                                                                
         MVC   TRMTTO,CTTKTID                                                   
*                                  MASK ON TOP OF TARGET TERM                   
         LA    RE,TRMTTO                                                        
         LA    R4,P4                                                            
         LHI   RF,L'TRMTTO                                                      
ADDTTW15 CLI   0(R4),X'00'                                                      
         BE    *+10                                                             
         MVC   0(1,RE),0(R4)                                                    
         AHI   RE,1                                                             
         AHI   R4,1                                                             
         BCT   RF,ADDTTW15                                                      
         DROP  R1                                                               
*                                                                               
ADDTTW18 AHI   R1,TRMTTLQ                                                       
         ST    R1,ATRMTABP                                                      
         C     R1,ATRMTABX                                                      
         BL    ADDTTW20                                                         
         DC    H'0'                EXCEED TABLE SIZE!                           
*                                                                               
ADDTTW20 EQU   *                                                                
         GOTO1 VDATAMGR,DMCB,DMRSEQ,CTFILE,CTTKEY,CTTKEY                        
         CLI   DMCB+8,0                                                         
         BE    ADDTTW10                                                         
         DC    H'0'                                                             
         DROP  R2                                                               
ADDTTWX  EQU   *                                                                
         C     R5,ATRMTABP         ANY WILD CARD ENTRY ADDED?                   
         BL    ADDTTX              YES                                          
         MVC   P(40),=CL40'NO RECORD MATCHED WITH THIS WILD CARD:'              
         MVC   P+40(8),P2                                                       
         GOTO1 VPRINTER                                                         
         MVI   ERROR,C'Y'                                                       
*                                                                               
ADDTTX   EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* READ CTFILE RECORDS AND PUT TO SORTER                               *         
***********************************************************************         
*READ CTFILE, ONE TERMINAL AT A TIME                                            
*LOOK UP THE TABLE                                                              
* IF MATCH SOURCE/COPY, COPY TO TARGET AND NODE=OSA                             
* IF MATAH SOURCE/DEL, DROP IT                                                  
* IF MATCH TARGET, CHECK ACTION2=REPLACE, YES, DROP THIS                        
* IF MATCH TARGET, CHECK ACTION2=NOT REPLACE, ERROR OUT                         
READCTFL NTR1  ,                                                                
         ZAP   LINE,=P'99'                                                      
*        MVC   P(11),=C'READ CTFILE'                                            
*        GOTO1 VPRINTER                                                         
*                                                                               
         LA    R2,IO               READ ANY CTFILE RECORD                       
         USING CTTREC,R2                                                        
         XC    CTTKEY,CTTKEY                                                    
         XC    IOKEY,IOKEY                                                      
RCTF005  EQU   *                                                                
*        MVC   P(5),=C'RDHI '                                                   
*        MVC   P+5(70),CTTKEY                                                   
*        GOTO1 VPRINTER                                                         
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMRDHI,CTFILE,CTTKEY,CTTKEY                        
         CLI   8(R1),0                                                          
         BE    RCTF014                                                          
         DC    H'0'                                                             
*                                                                               
RCTF010  EQU   *                                                                
*        MVC   P(5),=C'RSEQ '                                                   
*        MVC   P+5(70),CTTKEY                                                   
*        GOTO1 VPRINTER                                                         
         GOTO1 VDATAMGR,DMCB,DMRSEQ,CTFILE,CTTKEY,CTTKEY                        
*                                                                               
RCTF014  CLI   8(R1),0                                                          
         BE    *+14                                                             
         TM    8(R1),X'80'                                                      
         BNZ   RCTF200                                                          
         DC    H'0'                                                             
         CLC   =X'FFFFFFFF',CTTKEY        SKIP FFFFFF REC                       
         BNE   RCTF014B                                                         
         SR    R1,R1                                                            
         IC    R1,IOKEY                                                         
         AHI   R1,1                                                             
         STC   R1,IOKEY                                                         
         CLI   IOKEY,X'00'                                                      
         BNE   *+12                                                             
         BRAS  RE,PUTSORT          ADD THIS TRAILER X'FFFF' RECORD              
         B     RCTF200             END OF FILE                                  
*                                                                               
         XC    CTTKEY,CTTKEY                                                    
         STC   R1,CTTKEY                                                        
         B     RCTF005             READ HIGH FOR NEXT KEY                       
RCTF014B EQU   *                                                                
         MVC   IOKEY(L'CTTKEY),CTTKEY     SAVE KEY JUST READ                    
*                                                                               
         CLI   CTTKTYP,CTTKTYPQ    ONLY CHECK FOR TERMINAL REC                  
         BNE   RCTF100             NOT TERM, FEED TO SORTER                     
*                                                                               
         BRAS  RE,PROTERM          PROCESS THIS TERM                            
         B     RCTF010             NEXT REC                                     
         DROP  R2                                                               
*                                                                               
RCTF100  BRAS  RE,PUTSORT                                                       
         B     RCTF010                                                          
*                                                                               
RCTF200  EQU   *                                                                
*        MVC   P(13),=C'END OF CTFILE'                                          
*        GOTO1 VPRINTER                                                         
         B     RCTFOK                                                           
*                                                                               
RCTFNO   B     NO                                                               
RCTFOK   B     YES                                                              
         EJECT                                                                  
*                                                                               
***********************************************************************         
* COMPARE TERMINALS                                                             
***********************************************************************         
COMPTERM NTR1  ,                                                                
         L     R3,ATRMTAB                                                       
         L     R5,TRMENTY#                                                      
         USING TRMTABD,R3                                                       
CTRM10   EQU   *                                                                
         CLI   TRMTACT,C'='        ACTION=COMPARE ONLY                          
         BNE   CTRM90                                                           
*                                  READ SOURCE TERM INTO IO1                    
         LA    R2,IO                                                            
         USING CTTREC,R2                                                        
         XC    CTTKEY,CTTKEY                                                    
         MVI   CTTKTYP,CTTKTYPQ                                                 
         MVC   CTTKTID,TRMTFR                                                   
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,CTTKEY,CTTKEY                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2                                                               
*                                  READ TARGET TERM INTO IO2                    
         LA    R4,IO2                                                           
         USING CTTREC,R4                                                        
         XC    CTTKEY,CTTKEY                                                    
         MVI   CTTKTYP,CTTKTYPQ                                                 
         MVC   CTTKTID,TRMTTO                                                   
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,CTTKEY,CTTKEY                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
*                                  FIND 1ST SOURCE ID ELEMENT                   
         LA    R2,CTTDATA-CTTKEY(R2)                                            
CTRM20   CLI   0(R2),CTPIDELQ      X'1F' PRINCIPAL ID ELEMENT                   
         BNL   *+12                EXIT ON '1F'/'20' OR HIGHER ELEM             
         BRAS  RE,CTRMNXF          BUMP TO NEXT ELEMENT                         
         B     CTRM20                                                           
*                                  FIND 1ST TARGET ID ELEMENT                   
         LA    R4,CTTDATA-CTTKEY(R4)                                            
CTRM30   CLI   0(R4),CTPIDELQ      X'1F' PRINCIPAL ID ELEMENT                   
         BNL   *+12                EXIT ON '1F'/'20' OR HIGHER ELEM             
         BRAS  RE,CTRMNXT          BUMP TO NEXT ELEMENT                         
         B     CTRM30                                                           
         B     CTRM50                                                           
*                                  COMPARE THE VALUES                           
CTRM40   SR    R1,R1                                                            
         IC    R1,1(R2)            LENGTH OF THE ELEMENT                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),0(R4)       SAME VALUE?                                  
         BNE   CTRM80              NO - MARK "NOT THE SAME"                     
*                                                                               
         BRAS  RE,CTRMNXFT         BUMP TO NEXT ELEMENT FOR BOTH TERMS          
CTRM50   CLI   0(R2),CTIDELQ       SOURCE TERM HAS X'1F'/'20' ID ELEM?          
         BNH   CTRM40              YES - CONTINUE COMPARING                     
         CLI   0(R4),CTIDELQ       TARGET TERM HAS X'1F'/'20' ID ELEM?          
         BNH   CTRM40              YES - CONTINUE COMPARING                     
         B     CTRM90              NO-ID ELEMS ARE THE SAME, NEXT TERM          
*                                                                               
CTRM80   MVI   TRMTCMP,C'N'        ID ELEMENTS ARE NOT THE SAME                 
*                                                                               
         MVC   P(5),=CL5'LINE#'                                                 
         SR    RF,RF                                                            
         ICM   RF,3,TRMTLN#-TRMTABD(R3)                                         
         EDIT  (RF),(5,P+5),ALIGN=LEFT,ZERO=NOBLANK                             
         MVC   P+11(L'TRMTFR),TRMTFR-TRMTABD(R3)                                
         MVC   P+21(2),=C'<>'                                                   
         MVC   P+25(L'TRMTTO),TRMTTO-TRMTABD(R3)                                
         GOTO1 VPRINTER                                                         
*                                                                               
CTRM90   AHI   R3,TRMTTLQ          NEXT TERMINAL                                
         BCT   R5,CTRM10                                                        
         B     CTRMX               AND EXIT                                     
*                                                                               
CTRMX    XIT1  ,                                                                
         DROP  R3                                                               
*                                                                               
CTRMNXF  SR    R0,R0               SOURCE TERMINAL, NEXT ELEMENT                
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
CTRMNXT  SR    R0,R0               TARGET TERMINAL, NEXT ELEMENT                
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         BR    RE                                                               
CTRMNXFT SR    R0,R0               BOTH TERMINALS, NEXT ELEMENT                 
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         BR    RE                                                               
*                                                                               
***********************************************************************         
* IF MATCH SOURCE/COPY, COPY TO TARGET AND NODE=OSA                             
* IF MATAH SOURCE/DEL, DROP IT                                                  
* IF MATCH TARGET, CHECK ACTION2=REPLACE, YES, DROP THIS                        
* IF MATCH TARGET, CHECK ACTION2=NOT REPLACE, ERROR OUT                         
***********************************************************************         
PROTERM  NTR1  ,                                                                
*                                                                               
         LA    R5,IO                                                            
         USING CTTREC,R5                                                        
*                                                                               
         L     R3,ATRMTAB                                                       
         L     R2,TRMENTY#                                                      
         USING TRMTABD,R3                                                       
PRTM10   EQU   *                                                                
         CLI   TRMTACT,C'='        SKIP ENTRY FOR COMPARE ONLY                  
         BE    PRTM20                                                           
         CLC   CTTKTID,TRMTFR                                                   
         BE    PRTM200             MATCH ON SOURCE TERMINAL                     
         OC    TRMTTO,TRMTTO                                                    
         BZ    PRTM20                                                           
         CLC   CTTKTID,TRMTTO                                                   
         BE    PRTM300             MATCH ON TARGET TERMINAL                     
*                                                                               
PRTM20   AHI   R3,TRMTTLQ                                                       
         BCT   R2,PRTM10                                                        
         BRAS  RE,PUTSORT          NO MATCH, JUST FEED TO SORTER                
         B     PRTMX                AND EXIT                                    
*                                                                               
PRTM200  EQU   *                   MATCH ON SOURCE TERMINAL                     
         CLI   TRMTACT,C'D'        ACTION=DELETE?                               
         BE    PRTDELS             YES-DON'T FEED TO SORTER AND EXIT            
*                                  ACTION=COPY                                  
         LA    R0,IO2              MVCL  IO2,IO                                 
         LA    RE,IO                                                            
         LHI   R1,2000                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R5,IO2                                                           
         MVC   CTTKTID,TRMTTO      IO2 W/ NEW TO TERM NAME                      
*                                                                               
         LA    RE,CTTDATA                                                       
PRTM210  CLI   0(RE),CTACTELQ      FIND TERM ACTIVITY ELEMENT                   
         BE    PRTM215                                                          
         CLI   0(RE),CTTRMELQ      FIND TERM DEF ELEMENT                        
         BE    PRTM220                                                          
         CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                MUST HAVE THIS ELEMENT                       
PRTM212  SR    RF,RF               BUMP TO NEXT ELEMENT                         
         IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     PRTM210                                                          
*                                                                               
PRTM215  EQU   *                                                                
         USING CTACTD,RE                                                        
         MVC   CTACTDT,TODAYYMD    SET DATA CHANGED AT TODAY                    
         B     PRTM212             NEXT ELEMENT                                 
         DROP  RE                                                               
*                                                                               
PRTM220  EQU   *                                                                
         USING CTTRMD,RE                                                        
         MVI   CTTRMNDE,X'1F'      SET NODE=OSA                                 
         DROP  RE                                                               
*                                                                               
         BRAS  RE,PUTSORT          FEED BOTH SOURCE REC                         
         BRAS  RE,PUTSORT2              AND TARGET NEW REC                      
         B     PRTMCPY                  AND THEN EXIT                           
*                                                                               
PRTM300  EQU   *                                                                
         CLI   TRMTACT2,C'R'       ACTION2=REPLACE?                             
         BE    PRTMREPT            YES-DROP THIS ONE                            
         B     PRTMEXTT                                                         
*                                                                               
PRTDELS  EQU   *                                                                
         MVC   P(30),=CL30'DELETE THIS TERMINAL:'                               
         MVC   P+30(8),TRMTFR                                                   
         GOTO1 VPRINTER                                                         
         B     PRTMX                                                            
PRTMCPY  EQU   *                                                                
         MVC   P(30),=CL30'COPY TERMINAL:'                                      
         MVC   P+30(8),TRMTFR                                                   
         MVC   P+39(2),=CL2'TO'                                                 
         MVC   P+42(8),TRMTTO                                                   
         GOTO1 VPRINTER                                                         
         B     PRTMX                                                            
PRTMREPT EQU   *                                                                
         MVC   P(30),=CL30'DROP+REPLACE THIS TERMINAL(T):'                      
         MVC   P+30(8),TRMTTO                                                   
         GOTO1 VPRINTER                                                         
         B     PRTMX                                                            
PRTMEXTT EQU   *                                                                
         MVC   P(40),=CL40'TARGET TERMINAL ALREADY EXITS:'                      
         MVC   P+40(8),TRMTTO                                                   
         GOTO1 VPRINTER                                                         
         DC    H'0'                                                             
*                                                                               
PRTMX    XIT1  ,                                                                
         DROP  R5,R3                                                            
***********************************************************************         
* PUT RECORD TO SORTER                                                *         
***********************************************************************         
PUTSORT  NTR1  ,                                                                
         SR    RE,RE                                                            
         ICM   RE,3,IO+X'19'                                                    
         LA    RE,4(RE)                                                         
         SLL   RE,16                                                            
         STCM  RE,15,IOL           SET TAPEOUT RECORD LENGTH                    
         GOTO1 VSORTER,PARM,SORTPUT,IOL                                         
         XIT1  ,                                                                
***********************************************************************         
* PUT RECORD TO SORTER FROM IO2                                       *         
***********************************************************************         
PUTSORT2 NTR1  ,                                                                
         SR    RE,RE                                                            
         ICM   RE,3,IO2+X'19'                                                   
         LA    RE,4(RE)                                                         
         SLL   RE,16                                                            
         STCM  RE,15,IO2L          SET TAPEOUT RECORD LENGTH                    
         GOTO1 VSORTER,PARM,SORTPUT,IO2L                                        
         XIT1  ,                                                                
***********************************************************************         
* WRITE RECORDS FROM SORTER TO OUTPUT FILE                            *         
***********************************************************************         
WRITEOUT NTR1  ,                                                                
         OPEN  (TAPEOUT,OUTPUT)                                                 
*                                                                               
WOUT010  GOTO1 VSORTER,PARM,SORTGET                                             
         ICM   R2,15,4(R1)                                                      
         BZ    WOUT100                                                          
         CLC   WORK(25),4(R2)       CHECK DUPLICATE KEY                         
         BNE   WOUT020                                                          
         BRAS  RE,DUPEKEY           REPORT DUPLICATE KEY & DIFF. DATA           
         DC    H'0'                                                             
         B     WOUT010                                                          
*                                                                               
WOUT020  MVC   WORK(25),4(R2)       SAVE RECORD KEY                             
         PUT   TAPEOUT,(R2)                                                     
         B     WOUT010                                                          
*                                                                               
WOUT100  CLOSE (TAPEOUT)                                                        
         MVC   P,SPACES                                                         
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         MVC   P(12),=C'END OF WRITE'                                           
         GOTO1 VPRINTER                                                         
         B     WOUTOK                                                           
*                                                                               
WOUTNO   B     NO                                                               
*                                                                               
WOUTOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* REPORT DUPLICATE KEY                                                *         
***********************************************************************         
         SPACE 1                                                                
DUPEKEY  NTR1                                                                   
         MVC   P(24),=C'WARNING: DUPLICATE KEY: '                               
         MVC   P+24(08),11(R2)                                                  
         LA    R2,4(R2)                                                         
         GOTO1 VPRINTER                                                         
         GOTO1 VHEXOUT,PARM,(R2),P,25,=C'TOG'                                   
         GOTO1 VPRINTER                                                         
         XIT1  ,                                                                
         EJECT                                                                  
         SPACE 2                                                                
***********************************************************************         
         EJECT                                                                  
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         SPACE 2                                                                
* FASYSLST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASYSLST                                                       
         PRINT ON                                                               
         SPACE 1                                                                
         EJECT                                                                  
         SPACE 1                                                                
         DS    0D                                                               
* ++INCLUDE FASSBOFF                                                            
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG                                                                    
SSBL     EQU   *-SSB                                                            
         SPACE 3                                                                
UTL      DC    F'0',X'0A'          FOR DATAMGR (MUST SPECIFY SYSTEM)            
         SPACE 2                                                                
COMFACS  DS    0A                                                               
VDATAMGR DC    V(DATAMGR)                                                       
VCPRINT  DC    V(CPRINT)                                                        
VPRINT   DC    V(PRINT)                                                         
VPRINTER DC    V(PRINTER)                                                       
VCARDS   DC    V(CARDS)                                                         
VXSORT   DC    V(XSORT)                                                         
VHEXOUT  DC    V(HEXOUT)                                                        
VDATCON  DC    V(DATCON)                                                        
VSORTER  DC    V(SORTER)                                                        
         SPACE 1                                                                
TAPEOUT  DCB   DDNAME=TAPEOUT,DSORG=PS,MACRF=(PM),RECFM=VB,            *        
               BLKSIZE=8200,LRECL=2048,BUFNO=2                                  
DDOUT    DCB   DDNAME=DDOUT,DSORG=PS,MACRF=(PM),RECFM=VB,              *        
               BLKSIZE=8200,LRECL=2048,BUFNO=2                                  
TRMLIST  DCB   DDNAME=TRMLIST,DSORG=PS,RECFM=FB,MACRF=GM,              *        
               BLKSIZE=8000,LRECL=80,EODAD=EODADDI                              
SORTCRD1 DC    C'SORT FIELDS=(5,25,A),FORMAT=BI,WORK=1 '                        
SORTCRD2 DC    C'RECORD TYPE=V,LENGTH=(2100,,,,) '                              
SORTPUT  DC    C'PUT'                                                           
SORTGET  DC    C'GET'                                                           
*                                                                               
FLISTCTF DC    CL8'NCTFILE '                                                    
         DC    C'X'                                                             
*                                                                               
DMOPEN   DC    C'DMOPEN '                                                       
DMREAD   DC    C'DMREAD '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
DMRDHI   DC    C'DMRDHI '                                                       
         SPACE 1                                                                
CONTROL  DC    C'CONTROL'                                                       
CTFILE   DC    C'CTFILE '                                                       
         EJECT                                                                  
TRMTAB   EQU   *                                                                
         DS    (TRMTBQ)XL(TRMTTLQ)                                              
TRMTABX  EQU   *                                                                
TRMTBQ   EQU   10000                                                            
TRMTABD  DSECT                                                                  
TRMTFR   DS    CL8                 COPY/COMPARE FROM TERMINAL NAME              
TRMTTO   DS    CL8                 COPY/COMPARE TO TERMINAL NAME                
TRMTACT  DS    C                   ACTION:COPY(C)/DELETE(D)/COMPARE(=)          
TRMTACT2 DS    C                   ACTION2:REPLACE(R) OR ADD(A)                 
TRMTFRD  DS    C                   DUP ENTRY (FROM TERMINAL LIST)               
TRMTTOD  DS    C                   DUP ENTRY (TO TERMINAL LIST)                 
TRMTCMP  DS    C                   'N' - ID ELEMENTS ARE NOT THE SAME           
TRMTLN#  DS    XL2                 INPUT LINE#                                  
TRMTTLQ  EQU   *-TRMTABD                                                        
YYUNOSA  CSECT                                                                  
         EJECT                                                                  
         EJECT                                                                  
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDDPRINTL                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDDPRINTL                                                      
         PRINT ON                                                               
         SPACE 1                                                                
WORKD    DSECT                     ** GLOBAL WORKING STORAGE **                 
*                                                                               
PARM     DS    6F                                                               
RETCODE  DS    XL1                                                              
COMPONLY DS    CL1                                                              
*                                                                               
DUB      DS    D                                                                
DMCB     DS    6F                                                               
*                                                                               
TODAYYMD DS    XL3                 TODAY'S DATE IN YMD BINARY                   
LINE#    DS    H                                                                
ERROR    DS    XL1                                                              
         DS    0F                                                               
ATRMTAB  DS    A                   A(TRM TABLE START)                           
ATRMTABP DS    A                   A(TRM TABLE NEXT AVALIABLE ENTRY)            
ATRMTABX DS    A                   A(TRM TABLE END)                             
TRMENTY# DS    F                   TOTAL # TRM TABLE ENTRY                      
WORK     DS    XL256                                                            
WORK2    DS    XL256                                                            
*                                                                               
P1       DS    CL8                 ACTION: COPY/DELETE/COPY*/DELETE*            
P2       DS    CL8                 SOURCE TERMINAL                              
P3       DS    CL8                 'TO'-REPLACE BY WILD CARD END TERM           
P4       DS    CL8                 TARGET TERMINAL                              
P5       DS    CL8                 ACTION2: ADD/REPLACE                         
TERMEND  DS    CL8                 ENDING TERMINAL OF WILD CARD                 
TERMSTR  DS    CL8                 STARTING TERMINAL OF WILD CARD               
*                                                                               
IOKEY    DS    XL(L'CTTKEY)                                                     
IOL      DS    XL4                                                              
IO       DS    2000X                                                            
IO2L     DS    XL4                                                              
IO2      DS    2000X                                                            
WORKX    DS    0D                                                               
         SPACE 1                                                                
WORKC    CSECT                     ** WORKING STORAGE POOL 1 **                 
         DS    (64*1024)X                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002CTCONOSA  07/06/09'                                      
         END                                                                    

*          DATA SET DETCR99    AT LEVEL 003 AS OF 05/01/02                      
*PHASE DETCR99                                                                  
         TITLE '- NTI TCAR DEMO TAPE CONVERSION'                                
VHWC18   EQU   K0521               VHWC18                                       
RHWC18   EQU   K0621               RHWC18                                       
IHWC18   EQU   K0633               IHWC18                                       
GAHOMES  EQU   K1501-K0000+4       BWW55+                                       
AAHOMES  EQU   K0533-K0000         RHOMES                                       
TPPUT    EQU   K1197-K0000         WW25                                         
PUTSTRT  EQU   K0821-K0000         FIRST O                                      
PUTEND   EQU   K1041-K0000+4       LAST O BEFORE HOMES                          
TPHUTR   EQU   K1045-K0000         OHOMES                                       
GAIMPS   EQU   K1353-K0000         BW25                                         
AAIMPS   EQU   K0193-K0000         YW25                                         
*                                                                               
GAAIMP2  EQU   K1505-K0000         V18+                                         
AAIMP2   EQU   K0349-K0000         M18+                                         
GAALN1   EQU   K0345-K0193         COPY UNTIL YHOMES                            
GAALN2   EQU   K0641-K0349         COPY AFTER YHOMES                            
*                                                                               
NUMCATS  EQU   38                                                               
         EJECT                                                                  
DETCR99 CSECT                                                                   
         PRINT NOGEN                                                            
         NMOD1 0,DETCR99,RA,RR=RE                                               
         USING DEMCOND,R8          R8 POINTS TO GLOBAL WORKING STORAGE          
         LH    R4,=Y(WORKD-DETCR99)                                             
         LA    R4,DETCR99(R4)                                                   
         USING WORKD,R4                                                         
         ST    RE,RELO                                                          
         L     RC,ARREC                                                         
         LA    RC,4(RC)            RC POINTS TO RATING SERVICE RECORD           
         L     R2,AIREC            R2 POINTS TO INTERIM RECORD - SORT           
         USING INTERD,R2           KEY, VALUES, AND DEMO VALUES                 
         SPACE 1                                                                
         B     *+4(R1)             ROUTINE HOOK                                 
         SPACE 1                                                                
         B     READ                PROCESS INPUT TAPE                           
         B     CNVWR               SORT RECORD HOOK                             
         B     MORET               E-O-F ON INPUT                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
READ     DS    0H                                                               
         CLI   INTAPESW,1          TEST FOR OPENING INPUT TAPE                  
         BE    READ20                                                           
         MVI   NOTAVAL,0                                                        
         L     RE,=A(PRGAVG)                                                    
         ST    RE,ACOMWRK          PASS ON TO OUTPUT PHASE                      
         MVI   IPGAASW,C'N'                                                     
         MVI   BYPREAD,X'FF'       SET TO 1ST-TIME-THRU                         
         OPEN  (IN1,(INPUT))                                                    
         SPACE 1                                                                
READ20   DS    0H                                                               
         L     R3,ARREC                                                         
         XC    0(4,R3),0(R3)       INSERT VARIABLE LENGTH RECORD                
         MVC   0(2,R3),=H'401'     HEADER                                       
         LA    R3,4(R3)                                                         
         CLI   BYPREAD,1           TEST FOR BYPASSING INPUT READ                
         BE    READ40                                                           
*                                                                               
         GET   IN1,(R3)            READ NEXT RECD                               
         USING MIREC,RC                                                         
         CLI   GAASW,C'P'          IGNORE ALL GAA ON -NTI-                      
         BNE   READ25                                                           
         CLC   MISEQ(2),=C'04'                                                  
         BNE   READ30                                                           
         CLI   MIESTYPE,C'2'       GAA?                                         
         BE    READ20              YES, BYPASS RECD                             
         B     READ30                                                           
*                                                                               
READ25   MVI   GAASW,C'N'          DEFAULT IS NON-GAA DATA                      
         CLC   MISEQ(2),=C'04'     CHECK GAA FOR DATA                           
         BNE   READ30                                                           
         CLI   MIESTYPE,C'2'                                                    
         BNE   READ30                                                           
         MVI   GAASW,C'Y'                                                       
*                                                                               
READ30   CLI   BYPREAD,X'FF'       TEST FOR 1ST-TIME-THRU                       
         BNE   READ40                                                           
         CLC   MISEQ,=C'00'        TEST FOR REPORT RECORD                       
         BE    *+6                                                              
         DC    H'0'                WRONG TAPE                                   
         CLI   MITYPE,C' '         DESCRIPTOR                                   
         BE    B0RTN                                                            
         DC    H'0'                WRONG TAPE                                   
*                                                                               
READ40   CLC   SAVEPNUM,MINUM      BREAK ON PROGRAM NUMBER                      
         BE    READ41                                                           
         MVC   SAVEPNUM,MINUM                                                   
         MVI   BYPASS01,0                                                       
         MVI   VARIOUS,0                                                        
         MVC   VARS,ZEROS                                                       
         MVC   DAYS,ZEROS                                                       
         XC    SAVVAR(49),SAVVAR                                                
*                                                                               
READ44   CLI   BOOKTYPE,C'I'       TEST BOOKTYPE                                
         BNE   READ46                                                           
         CLI   MIVCR,C'1'          CONVERT NON-VCR (INTEGRATED (SIC))           
         B     READ47                                                           
READ46   CLI   MIVCR,C' '          CONVERT VCR (ASCRIBED)                       
READ47   BNE   READ20                                                           
         CLC   MISEQ,=C'04'        TEST FOR PROGRAM HH EST RECORDS              
         BNE   READ70                                                           
*                                                                               
         CLC   MIDYSWKS,=C'00'                                                  
         BNE   *+8                                                              
         MVI   BYPASS01,0                                                       
         CLI   BYPASS01,1          BYPASS SINGLE DAY AVERAGES                   
         BE    READ20                                                           
         CLI   MIORIG,C'0'         ***TEMP***                                   
         BE    READ48              *        *                                   
         CLI   MIORIG,C'1'         *        *                                   
         BNE   READ20              *        *                                   
         CLC   MINET(3),=C'XYZ'    *        *                                   
         NOP   READ20              *        *                                   
READ48   EQU   *                   ***TEMP***                                   
         CLI   MITYPE,C'D'         DESCRIPTOR                                   
         BE    D4RTN                                                            
         CLI   NOTAVAL,1           DATA NOT AVAILABLE                           
         BE    READ20              READ NEXT RECORD                             
         CLI   MITYPE,C'H'         HALF HOUR DETAIL                             
         BE    H4RTN                                                            
         CLI   MITYPE,C'P'         PERSONS ESTIMATES                            
         BNE   READ20                                                           
         MVI   BYPASSH4,0          RESET H4 RECORD BYPASS SWITCH                
         CLI   NOTAVAL,2           BYPASS 1/2HR P-RECDS (DNA)                   
         BE    READ20              NO DATA ON RECD, BYPASS THEM                 
         B     P4RTN               NO, PROCESS IT                               
*                                                                               
READ70   CLC   MISEQ,=C'03'        TEST FOR USAGE RECORDS                       
         BNE   READ80                                                           
         CLI   MITYPE,C'H'         HOUSEHOLD                                    
         BE    H3RTN                                                            
         CLI   MITYPE,C'P'         PERSONS DEMOGRAPHICS                         
         BE    P3RTN                                                            
         B     READ20                                                           
*                                                                               
READ80   CLC   MISEQ,=C'05'        TEST FOR NON-NETWORK SOURCES                 
         BNE   READ90                                                           
         CLI   MITYPE,C'H'         HOUSEHOLD                                    
         BE    H5RTN                                                            
         CLI   MITYPE,C'P'         PERSONS DEMOGRAPHICS                         
         BE    P5RTN                                                            
         B     READ20                                                           
*                                                                               
READ90   MVI   BYPREAD,0                                                        
         MVI   INTAPESW,X'40'      DROP RECORD                                  
         B     EXIT                                                             
         EJECT                                                                  
*********************************************************************           
*B0RTN - REPORT DESCRIPTOR RECORD                                               
*********************************************************************           
B0RTN    DS    0H                                                               
         USING B0REC,RC                                                         
         BAS   RE,DAYRTN           SET UP DAY TABLE                             
         MVC   HDAYTABS,HDAYTAB    SAVE DAY TABLE                               
         PACK  DUB,B0END+1(2)      CONVERT YEAR (YY)                            
         CVB   R0,DUB                                                           
         STC   R0,HALF                                                          
         GOTO1 VNETWEEK,DMCB,B0END+1,VGETDAY,VADDAY                             
         MVC   HALF,4(R1)          YEAR                                         
         MVC   HALF+1(1),8(R1)     WEEK                                         
*                                                                               
         MVC   BOOK1,HALF                                                       
         MVC   IBOOK1,HALF                                                      
         MVC   BOOK1S,HALF                                                      
         MVC   IBOOK1S,HALF                                                     
         MVI   BYPREAD,0           SET TO READ NEXT RECORD                      
*                                                                               
         MVI   GAASW,0             FOR NSS, IF THERE, ALLOW THEM                
         CLC   =C'TOTAL COMMERCIAL ACTIVITY - WARNER',B0REPORT                  
         BE    *+6                 NSS - NO UNIVS ON TAPE, RD OUR FILE          
         DC    H'0'                UNKNOWN FILE TYPE                            
         MVI   BOOKTYPE,C'T'       FORCE BKTYPE TO T FOR TCAR TAPE              
         B     B0RTN50             NO LK UPS NEED YET                           
*                                                                               
         DS    0H            KEEP THIS LK UP CODE IN CASE NEEDED LATER          
         XC    PAVKEY,PAVKEY       BUILD DIRECTORY KEY                          
         LA    R5,PAVKEY                                                        
         USING PRKEY,R5                                                         
         MVC   PRCODE(8),=C'PNNUUUUT'   LOOK UP UNIVERSE RECD                   
         MVC   PRBOOK,BOOK1        FOR CURRENT WEEK                             
         MVC   PRBTYP,BOOKTYPE                                                  
         L     R1,=A(UNIVSAVE)                                                  
         ST    R1,ASLOT                                                         
         BAS   RE,RESLOT           READ RECD IN AND SLOT DEMOS                  
         BZ    *+6                                                              
         DC    H'0'                UNIV RECD NOT FOUND                          
         L     R5,=A(UNIVSAVE)         SLOT HOMES UNIV PROPERLY                 
         LA    R1,K0345-K0193      DISP TO YHOMES IN UNIVSAVE                   
         SRL   R1,2                DIVIDE BY 4                                  
         AR    R5,R1               PT TO HOMES                                  
         MVC   SVHHUNIV,0(R5)                                                   
         DROP  R5                                                               
*                                                                               
         L     R1,=A(INTAB)        READ INTABS RECDS AND CREATE TABLE           
         USING INTABD,R1                                                        
         MVC   DATADISP,=Y(PRFRSTEL-PRKEY)                                      
         MVI   ELCODE,X'21'        NAME ELEMENT (HAS DATE IN IT)                
         LA    R5,X'40'            READ 7-DAYS STARTING WITH MONDAY             
*                                                                               
B0RTN30  LA    RF,PAVKEY                                                        
         XC    PAVKEY,PAVKEY       READ SAMPLE COUNTS                           
         USING PRKEY,RF                                                         
         MVC   PRCODE(8),=C'PNNITABT'                                           
         MVC   PRBOOK,BOOK1        FOR CURRENT WEEK                             
         MVC   PRBTYP,BOOKTYPE                                                  
         STC   R5,PRDW                                                          
         ST    R1,INTABADR                                                      
         LA    RE,INTABCNT                                                      
         ST    RE,ASLOT            SLOT INTO INTABCNT                           
         DROP  RF                                                               
         BAS   RE,RESLOT           READ RECD IN AND SLOT DEMOS                  
         BNZ   B0RTN35             THIS DAY NOT FOUND--TRY NEXT ONE             
         L     R6,ASREC                                                         
         LA    R6,4(R6)                                                         
         USING PRKEY,R6                                                         
         MVC   INTABDAY,PRDW       SAVE DAY OF WEEK                             
         BAS   RE,GETEL            GET NAME ELEMENT FOR DATE                    
         MVC   INTABDAT,2(R6)      DATE                                         
         LA    R1,L'INTABTAB(R1)     NEXT INTAB BUCKET AREA                     
B0RTN35  SRL   R5,1                NEXT DAY BIT                                 
         LTR   R5,R5                                                            
         BNZ   B0RTN30                                                          
         DROP  R6                                                               
*                                                                               
B0RTN50  DS    0H                                                               
         L     R5,VBITMAP1                                                      
         ICM   R5,8,=C'S'          FOR NSS -> SYND MAP                          
         GOTO1 VNTIPRG,DMCB,=C'BLDM',(R5),0,RR=RELO                             
*                                                                               
         MVI   INTAPESW,X'40'      DROP THIS RECORD                             
         CLI   RPRINT,0            TEST FOR PRINT OF RAT SER REC                
         BE    *+8                 NO                                           
         OI    INTAPESW,X'03'      YES-SET INDICATOR FOR CONTROLLER             
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
*RESLOT- READ RECD FROM FILE BUILT BY REG NTI AND SLOT DEMOS BACK               
*        INTO APPROPRIATE TABLE AND BUCKETS (RE-SLOT THEM)                      
**********************************************************************          
RESLOT   NTR1                                                                   
         MVC   SVKEY,PAVKEY                                                     
         L     R6,ASREC                                                         
         USING PRKEY,R6                                                         
         LA    R6,4(R6)                                                         
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI ',=C'PAVDIR',PAVKEY,(R6)                 
         CLI   DMCB+8,0            ANY READ ERRORS                              
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   PAVKEY(18),0(R6)    KEY READ IN                                  
         BNE   RESLEOF             NOT FOUND                                    
         MVC   SVDA,PRNDXDA-PRKEY(R6)                                           
         MVC   PAVKEY,SVKEY        RESTORE SAVED KEY                            
         MVC   0(L'SVKEY,R6),SVKEY                                              
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI ',=C'PAVFIL',SVDA,(R6)                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   SVKEY(18),0(R6)     SAME KEY?                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,PRFRSTEL-PRKEY(R6) PT TO 1ST ELEMENT                          
         SR    R0,R0                                                            
RESL10   CLI   0(R6),X'00'         ENDREC?                                      
         BNE   *+6                                                              
         DC    H'0'                END OF REC --NO '5E' ELEMENT???              
         CLI   0(R6),X'41'                                                      
         BE    RESL15              DEMO ELEMENT                                 
         IC    R0,1(R6)            BUMP TO NEXT ELEMENT                         
         AR    R6,R0               NEXT ELEMENT                                 
         B     RESL10                                                           
*                                                                               
RESL15   ZIC   R0,1(R6)                                                         
         AR    R0,R6                                                            
         ST    R0,DMCB             ADDR OF NEXT ELEM                            
         MVC   DMCB+4(1),2(R6)     SAVE PRECISION                               
         NI    DMCB+4,X'0F'                                                     
         ZIC   R0,DMCB+4           LENGTH OF EACH FLD IN ELEMT                  
         LA    R1,4                                                             
         SR    R1,R0               #BYTES TO DISP INTO UNIVSAVE                 
         L     R5,ASLOT            SLOT INTO BEGIN SLOT SPECIFIED               
         AR    R5,R1                                                            
         LA    R6,3(R6)            PT TO 1ST DEMO IN ELEMENT                    
*                                                                               
RESL20   ZIC   R1,DMCB+4           PRECISION                                    
         C     R6,DMCB             END OF THIS ELELMENT                         
         BL    RESL25                                                           
         SR    R1,R1               SET CC=ZERO ON EXIT                          
         B     RELSLOTX                                                         
*                                                                               
RESL25   BCTR  R1,0                MOVE DEMO INTO SLOT                          
         EXMVC R1,0(R5),0(R6)                                                   
         LA    R5,4(R5)            BUMP UNIV PTR                                
         AR    R6,R1               BUMP ELEMENT PTR                             
         LA    R6,1(R6)            +1 FROM BCTR                                 
         B     RESL20                                                           
*                                                                               
RESLEOF  LA    R1,1                SET CC TO NON ZERO                           
*                                                                               
RELSLOTX LTR   R1,R1                                                            
         B     EXIT                                                             
         DROP  R6                                                               
*                                                                               
*********************************************************************           
*D4RTN - PROGRAM DESCRIPTOR RECORD D-P-P                                        
*********************************************************************           
*                                                                               
D4RTN    DS    0H                                                               
         USING D4REC,RC                                                         
         CLC   D4MKTBRK,=C'000'    RESET AVAILABILITY ON PRIME REC              
         BNE   *+8                                                              
         MVI   NOTAVAL,0                                                        
         CLI   NOTAVAL,1           BYPASS EXTRA DESC. RECS                      
         BE    D4RX                WHICH MAY BE MISSING IND                     
         CLI   D4NAVAL,C'1'        N/A FLAG                                     
         BNE   *+12                                                             
         MVI   NOTAVAL,1                                                        
         B     D4RX                                                             
*                                                                               
         CLC   D4MKTBRK,=C'350'    ANY UNDER 18                                 
         BE    D350RTN                                                          
         CLC   D4MKTBRK,=C'351'    ANY UNDER 12                                 
         BE    D351RTN                                                          
         CLC   D4MKTBRK,=C'352'    ANY UNDER 6                                  
         BE    D352RTN                                                          
         XC    HWCAREA,HWCAREA                                                  
*                                                                               
         CLI   D4ORIG,C'0'         TEST CORRECTION RECORD                       
         BE    D4R01               NO                                           
         MVI   CORRSW,1            SET CORRECTION SWITCH                        
         MVC   INTPNO,BOOK1S       ORIGINATING BOOK FOR CORRECTIONS             
         BAS   RE,DAYRTN           SET UP DAY TABLE                             
         PACK  DUB,D4END+1(2)      ***TEMP*** CONVERT YEAR (YY)                 
         CVB   R0,DUB              ***TEMP***                                   
         STC   R0,HALF             ***TEMP***                                   
         GOTO1 VNETWEEK,DMCB,D4END+1,VGETDAY,VADDAY                             
         MVC   HALF+0(1),4(R1)     YEAR                                         
         MVC   HALF+1(1),8(R1)     WEEK (NETWORK)                               
         MVC   BOOK1,HALF          KEY BOOK ALLOWS BLACK WEEK                   
         MVC   IBOOK1,HALF         WEEK TO PREVIOUS REGULAR WEEK                
*                                                                               
D4R01    MVC   SVPROJ,ZEROS        RESET H4 RECORD'S SAVE AREAS                 
         MVC   SVRTG,ZEROS                                                      
         MVC   SVSHR,ZEROS                                                      
*                                                                               
         CLC   D4TOTDUR,=C'000000' ARE TOTAL FIELDS ON THIS RECORD              
         BE    *+16                                                             
         MVC   SVD4,D4STA          IF YES - SAVE POS.305-363                    
         XC    DBUFF(DBUFFQ),DBUFF                                              
         MVC   D4STA(L'SVD4),SVD4  IF NOT - RESTORE FROM A PREVIOUS D4          
         XC    SAVETIME(56),SAVETIME                                            
         MVI   GOSUBN,SETDB#       SET DBUFFER AND SAVETIME                     
         GOTO1 =A(SUBR2),DMCB,(RC),RR=RELO                                      
*                                                                               
         CLC   D4DYSWKS,=C'01'     TEST AVERAGES                                
         BE    D4R06                                                            
         CLI   D4BREAK,C'1'        BREAKOUT                                     
         BE    D4R10                                                            
         LA    RE,D4DAYS                                                        
         LA    RF,DAYS             SET ON POINTERS FOR INDIVIDUAL DAYS          
         LA    R0,7                (NEEDED FOR PROCESSING VARIOUS)              
D4R05    CLI   0(RE),C'1'                                                       
         BNE   *+8                                                              
         MVI   0(RF),C'1'                                                       
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,D4R05                                                         
         B     D4R10                                                            
*                                                                               
D4R06    CLC   D4DAYS,=C'1111100'  M-F                                          
         BE    D4R10                                                            
         CLC   D4DAYS,=C'1111111'  M-S                                          
         BE    D4R10                                                            
         CLC   D4NET(3),=C'SYN'    SYND HAVE NO INDIV. DAY                      
         BNE   *+12                                                             
         MVI   VARIOUS,1           SO FUDGE IT HERE                             
         B     D4R07                                                            
*                                  VARIOUS                                      
         CLI   VARIOUS,1                                                        
         BE    D4R07                                                            
         MVI   VARIOUS,1                                                        
         MVC   VARS,ZEROS          1ST-TIME-THRU                                
         XC    VARSTIME(56),VARSTIME                                            
*                                                                               
         LA    RE,DAYS             TEST NUMBER OF DAYS IN AVERAGE               
         SR    RF,RF                                                            
         LA    R0,7                                                             
         CLI   0(RE),C'1'                                                       
         BNE   *+8                                                              
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,*-16                                                          
         CH    RF,=H'1'            BYPASS SINGLE DAY AVERAGES                   
         BH    D4R07                                                            
         MVI   VARIOUS,0                                                        
         OC    INTVAR(49),INTVAR   DO WE ALREADY HAVE A VAR                     
         BNZ   D4RX                                                             
         MVI   BYPASS01,1                                                       
         B     D4RX                                                             
*                                                                               
D4R07    LA    RE,D4DAYS                                                        
         LA    RF,VARS             SET ON POINTERS FOR VARIOUS DAYS             
         LA    R0,7                                                             
D4R08    CLI   0(RE),C'1'                                                       
         BNE   *+8                                                              
         MVI   0(RF),C'1'                                                       
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,D4R08                                                         
         CLC   D4NET(3),=C'SYN'    NO INDIV. DAYS FO SYND.                      
         BNE   *+10                                                             
         MVC   DAYS,VARS           SO FORCE THIS                                
*                                                                               
D4R10    XC    PVALS,PVALS                                                      
         GOTO1 VNTIPRG,DMCB,=C'LKUP',(0,VBITMAP1),D4NUM,RR=RELO                 
         MVC   PNUM,0(R1)          PNUM=DDS INTERNAL NUMBER                     
         MVC   PACK16(10),D4NUM                                                 
         MVI   PACK16+10,C'0'                                                   
         PACK  DUB,PACK16(11)                                                   
         MVC   INTPNTI,DUB+2       5 CHAR PWOS                                  
         MVC   PNTINUM,INTPNTI     PNTINUM= NEILSENS NTI PRG NUMBER             
         MVI   PDPT,0              PDPT IS ALWAYS ZERO                          
         MVC   PDPT2,D4DPT                                                      
         MVC   PNET,D4NET          SET UP R/S CALL LETTERS                      
         MVI   PNET+3,C' '                                                      
*                                                                               
NOTAGG   CLC   D4NET(3),=C'SYN'       SYNDICATION                               
         BNE   *+10                                                             
         MVC   PNET(3),D4SYNID     SET SYNDICATION CALL LETTERS                 
         MVC   PNAME,D4NAME                                                     
         MVC   PTITLE,D4TITLE                                                   
         MVC   PTYP,D4ALPHA                                                     
         MVC   PPREM,D4PREM                                                     
         MVC   INTMRKT,=H'1'       SET NETWORK MARKET TO 1                      
         CLC   D4DYSWKS,=C'01'                                                  
         BNE   D4R18                                                            
         L     R1,=A(PRGAVG)           PUT PRG IN BUFFER                        
D4R15    OC    0(4,R1),0(R1)                                                    
         BZ    D4R16               ADD PRG NUMBER TO LIST                       
         CLC   0(2,R1),=X'FFFF'                                                 
         BNE   *+6                                                              
         DC    H'0'                TABLE FULL                                   
         CLC   INTSTA(3),0(R1)                                                  
         BNE   *+14                                                             
         CLC   PNUM,4(R1)                                                       
         BE    D4R18               PRG ALREADY IN TABLE                         
         LA    R1,L'PRGAVG(R1)                                                  
         B     D4R15                                                            
*                                                                               
D4R16    MVC   0(4,R1),INTSTA                                                   
         MVC   4(2,R1),PNUM                                                     
         MVI   6(R1),X'90'         ASSUME VARIOUS DAYS                          
         CLC   D4DAYS,=X'1111100'                                               
         BNE   *+8                                                              
         MVI   6(R1),X'00'         M-F                                          
         CLC   D4DAYS,=X'1111111'                                               
         BNE   *+8                                                              
         MVI   6(R1),X'80'         M-S                                          
         LA    R1,L'PRGAVG(R1)                                                  
         CLC   0(2,R1),=X'FFFF'                                                 
         BE    *+14                                                             
         XC    0(2,R1),0(R1)       SET NEXT SLOT TO ZERO                        
*                                                                               
D4R18    L     RE,=A(NETTAB)                                                    
         LA    R0,NETWORKS                                                      
         CLC   PNET,0(RE)          GET CALL LETTERS FOR NETWORK                 
         BE    D4GSTA                                                           
         LA    RE,L'NETTAB(RE)                                                  
         BCT   R0,*-14                                                          
         CLC   PNET,=C'UPN '      CALL LETTER CHANGE                            
         BNE   *+20                                                             
         MVC   INTSTA(4),=C'PAR '                                               
         MVC   PNET,INTSTA                                                      
         B     D4GSTAB                                                          
*                                                                               
         CLC   D4NET(3),=C'SYN'    NETWORK NOT IN NETTAB                        
         BE    *+6                                                              
         DC    H'0'                CANNOT IDENTIFY:NET/NEW NET/NEW AGG          
         MVC   INTSTA(3),D4SYNID   IF SYN JUST MOVE IN WHAT WAS GIVEN           
         MVI   INTSTA+3,C' '       BLANK PAD                                    
         B     *+10                                                             
*                                                                               
D4GSTA   MVC   INTSTA,4(RE)                                                     
D4GSTAB  MVI   INTSTA+4,C'T'       FOR REG NET & AGG 'T'                        
         CLC   D4NET(3),=C'SYN'                                                 
         BNE   *+8                                                              
         MVI   INTSTA+4,C'S'       SYND IS C'S'                                 
*                                                                               
         PACK  DUB,D4PROJ          PROJECTION (XXX,XXX,XXX)                     
         CVB   R1,DUB                                                           
         SR    R0,R0                                                            
         AH    R1,=H'5000'         ROUND TO TEN THOUSANDS                       
         D     R0,=F'10000'                                                     
         STCM  R1,3,PWK1AUD        REACH                                        
*                                                                               
         PACK  DUB,D4STA                                                        
         CVB   R1,DUB                                                           
         STCM  R1,3,PWK1STAC       STATION COUNT                                
*                                                                               
         PACK  DUB,D4COV                                                        
         CVB   R1,DUB                                                           
         STCM  R1,3,PWK1COV        COVERAGE                                     
*                                                                               
         TM    D4AVEHUT+(L'D4AVEHUT-1),X'F0'                                    
         BO    *+10                                                             
         MVC   D4AVEHUT,=16C'0'                                                 
         PACK  DUB,D4AVEHUT                                                     
         CVB   RF,DUB                                                           
         ST    RF,SVAVGHI          SAVE AVG HOMES IMPS                          
*                                                                               
         L     RE,=A(TYPTAB)       POINT RE AT TELECAST TYPE TABLE              
         LA    R0,TYPES            COUNTER                                      
D4R20    CLC   D4SPEC,0(RE)        0=REGULAR   1=SPECIAL                        
         BNE   *+14                                                             
         CLC   D4REPEAT,1(RE)      BLANK=ORIGINAL   Y=REPEAT                    
         BE    D4R30                                                            
         LA    RE,L'TYPTAB(RE)                                                  
         BCT   R0,D4R20                                                         
         DC    H'0'                BLOW UP IF TYPE IS NOT IN TABLE              
D4R30    MVC   PWK1DTYP,2(RE)      EXTRACT CORRESPONDING BIT SETTING            
*                                                                               
         CLI   INTSTA+4,C'S'       SYNDICATOR                                   
         BNE   D4R31                                                            
         CLI   D4TTYPE,C'C'                                                     
         BNE   *+8                                                              
         OI    PWK1DTYP,X'18'      ORIG+REPEAT PROGRAMMING                      
         CLI   D4TTYPE,C'M'                                                     
         BNE   *+8                                                              
         OI    PWK1DTYP,X'20'      MULTIPLE PROGRAMS                            
*                                                                               
D4R31    CLI   D4OPT,C'1'          TEST OPTIONAL DATA                           
         BNE   D4R34                                                            
* NOTE - EARLY TAPES OF SEP/87 HAVE ERROR FOR NON-VCR (INTEGRATED)              
         CLI   D4VCR,C' '          TEST VCR                                     
         BE    D4R33                                                            
         CLC   D4DYSWKS,=C'01'     TEST AVERAGES                                
         BE    D4R34                                                            
         CLI   D4MULTI,C' '        TEST MULTI-DAYS                              
         BE    D4R34                                                            
D4R33    OI    PWK1DTYP,X'80'      SET OPTIONAL BIT IN PHTDTYP                  
*                                                                               
D4R34    MVI   PWK1RSF,0                                                        
         CLI   D4BREAK,C'1'        TEST FOR REDUCED STATION                     
         BNE   *+8                                                              
         MVI   PWK1RSF,X'01'       REDUCED STATION INDICATOR                    
         MVC   INTDPT,PDPT         PDPT IS ALWAYS ZERO                          
         MVC   INTDPT2,PDPT2                                                    
         MVC   INTPREM,PPREM                                                    
         MVC   INTPNUM,PNUM                                                     
         MVC   INTPTYP,PTYP                                                     
         MVC   INTPNAME,PNAME                                                   
         MVC   INTTITLE,PTITLE                                                  
         MVI   INTSTYP,0           DEFAULT TO REG PROGRAM                       
         CLI   D4BREAK,C'1'        BREAKOUT?                                    
         BNE   *+8                                                              
         MVI   INTSTYP,C'B'                                                     
         CLI   D4SPEC,C'1'         SPECIAL?                                     
         BNE   *+8                                                              
         MVI   INTSTYP,C'S'                                                     
         MVI   INTMTYP,1           METRO A MARKET                               
         MVC   INTBOOK,BOOK1       WEEK VALUES                                  
         MVC   INTIBOOK,IBOOK1                                                  
         MVC   INTAUD,PWK1AUD                                                   
         MVC   INTSTAC,PWK1STAC                                                 
         MVC   INTCOV,PWK1COV                                                   
         MVC   INTDTYP,PWK1DTYP                                                 
         MVC   INTRSF,PWK1RSF                                                   
*                                                                               
D4R40    LA    RE,D4DAYS           POINT TO DAY FIELDS                          
D4R42    LA    R0,7                COUNTER                                      
         LA    R5,X'40'            START AT MONDAY                              
         SR    RF,RF               CLEAR CUMULATIVE BITS                        
*                                                                               
D4R50    CLI   0(RE),C'0'          TEST FOR NO ACTIVITY                         
         BE    D4R60                                                            
         CLI   0(RE),C'1'          TEST FOR PROGRAM RAN                         
         BNE   D4R60                                                            
         OR    RF,R5               UPDATE CUMULATIVE BITS                       
*                                                                               
D4R60    LA    RE,1(RE)                                                         
         SRL   R5,1                NEXT DAY                                     
         BCT   R0,D4R50                                                         
*                                                                               
         STC   RF,INTDAYWK         SAVE BITS                                    
         MVI   BYTE,X'90'          SET INTERNAL DAY CODE TO VAR                 
         L     RE,=A(NETDAYTB)                                                  
         LA    R0,NETDAYS                                                       
         CLC   INTDAYWK,0(RE)      TEST BITS VS. TABLE                          
         BE    *+16                                                             
         LA    RE,L'NETDAYTB(RE)                                                
         BCT   R0,*-14                                                          
         B     D4R70                                                            
         MVC   BYTE,1(RE)          INTERNAL DAY CODE                            
D4R70    MVC   PDAY1,BYTE                                                       
         MVC   PDAY1BIT,INTDAYWK                                                
         GOTO1 PDATA,DMCB,SAVETIME,PDAY1BIT                                     
         MVI   INTRTYP,PMCODEQU    ***** -Q- RECD ******                        
         MVC   HALF,PWK1STIM                                                    
         MVC   INTSTIM,HALF        START TIME                                   
         MVC   INTDURM,PWK1DURM    DURATION IN MINUTES                          
         GOTO1 VHRTOQH,DMCB,HALF,INTSQH       START QH                          
*                                                                               
*        DEVELOP END QH (INTEQH) AND DURATION IN QH'S (INTDUR)                  
*                           INTDUR = 1 ( 1-22 MIN.)                             
*                                  = 2 (23-37 MIN.)                             
*                                  = 3 (38-52 MIN.)...ETC.                      
         ZIC   R0,INTSQH                                                        
         ZIC   RF,PWK1DURM         DURATION IN MINUTES                          
         LA    RF,8(RF)            ADD 8 BEFORE CNV TO QH                       
         SR    RE,RE                                                            
         D     RE,=F'15'                                                        
         CH    RF,=H'1'            TEST FOR DURATION OF AT LEAST ONE            
         BH    *+8                                                              
         LA    RF,1                                                             
         STC   RF,INTDUR           DURATION IN QH                               
         AR    R0,RF               SQH PLUS DUR                                 
         BCTR  R0,0                LESS ONE GIVE END QH                         
         STC   R0,INTEQH                                                        
*                                                                               
*              ADD DURATION IN MINUTES TO START TIME (MILITARY) TO              
*              GET END TIME                                                     
         SR    R0,R0                                                            
         LH    R1,HALF             START TIME                                   
         D     R0,=F'100'          MINUTES IN R0, HOURS IN R1                   
         ZIC   RF,INTDURM          DURATION IN MINUTES                          
         SR    RE,RE                                                            
         D     RE,=F'60'           MINUTES IN RE, HOURS IN RF                   
         AR    R0,RE               ADD MINUTES TOGETHER                         
         CH    R0,=H'60'           TEST IF SUM GT OR EQ TO 1 HOUR               
         BL    *+12                NO                                           
         SH    R0,=H'60'           SUBTRACT 60 MINUTES                          
         LA    RF,1(RF)            AND ADD 1 TO HOURS                           
         AR    R1,RF               ADD HOURS TOGETHER                           
         MH    R1,=H'100'          HOURS X 100 FOR MILITARY TIME                
         AR    R1,R0               ADD MINUTES TO HOURS                         
         CH    R1,=H'2400'         TEST FOR PAST MIDNIGHT                       
         BNH   *+8                                                              
         SH    R1,=H'2400'                                                      
         STCM  R1,3,INTETIM        END TIME                                     
*                                                                               
         CLC   D4NET(3),=C'SYN'    NO INDIV. DAY FOR SYNDS.                     
         BE    *+20                                                             
         CLC   D4DYSWKS,=C'01'     AVERAGES                                     
         BE    D4R77                                                            
         MVC   VARS,ZEROS                                                       
*                                  USE INDIVIDUAL DAY(S) FOR INTVAR             
         LA    RE,D4DAYS                                                        
         LA    RF,SAVVAR           SET UP START TIMES IN VAR SAVE AREA          
         LA    R0,7                                                             
D4R74    CLI   0(RE),C'1'                                                       
         BNE   D4R76                                                            
         MVC   0(1,RF),INTSQH      START QH                                     
         MVC   1(1,RF),INTEQH      END QH                                       
         PACK  DUB,D4DUR                                                        
         CVB   R1,DUB                                                           
         CH    R1,=H'240'          4-HOUR MAXIMUM SUPPORTED                     
         BNH   *+8                                                              
         LH    R1,=H'240'                                                       
         STC   R1,2(RF)            DURATION IN MINUTES                          
         MVC   3(2,RF),INTSTIM     START TIME                                   
         MVC   5(2,RF),INTETIM     END TIME                                     
D4R76    LA    RE,1(RE)                                                         
         LA    RF,7(RF)                                                         
         BCT   R0,D4R74                                                         
         CLC   D4NET(3),=C'SYN'    CAN BE VAR FOR SYND.                         
         BNE   D4R80                                                            
*                                                                               
D4R77    CLI   VARIOUS,1           VARIOUS                                      
         BNE   D4R80                                                            
         LA    RE,SAVETIME         ACCUMULATE INDIVIDUAL VAR DAY(S)             
         LA    RF,VARSTIME             INTO VARSTIME                            
         LA    R0,7                                                             
D4R78    OC    0(8,RE),0(RE)                                                    
         BZ    *+10                                                             
         MVC   0(8,RF),0(RE)                                                    
         LA    RE,8(RE)                                                         
         LA    RF,8(RF)                                                         
         BCT   R0,D4R78                                                         
*                                                                               
         CLC   VARS,DAYS           HAVE ALL VARIOUS DAYS PROCESSED              
         BNE   D4R90                                                            
D4R79    MVI   VARIOUS,0           RESET                                        
         MVC   DAYS,ZEROS          RESET                                        
         MVC   INTVAR(49),SAVVAR                                                
         XC    SAVVAR(49),SAVVAR                                                
         MVC   SAVETIME(56),VARSTIME                                            
         XC    VARSTIME(56),VARSTIME                                            
         LA    RE,VARS             GENERATE VAR DATA USING                      
         B     D4R42                   ACCUMULATED DAYS                         
*                                                                               
D4R80    LA    R1,CALVPH           SET CALVPH FIELDS                            
         USING CALVPHD,R1                                                       
         MVC   VPHDPT,D4DPT                                                     
         MVC   VPHPCOD,D4ALPHA                                                  
         MVI   VPHSPCL,C' '                                                     
         CLI   D4SPEC,C'1'         1=SPECIAL                                    
         BNE   *+8                                                              
         MVI   VPHSPCL,C'S'                                                     
         MVC   VPHACTD,INTDYBIT                                                 
         CLI   VPHACTD,0                                                        
         BNE   *+8                                                              
         MVI   VPHACTD,X'40'                                                    
         MVI   VPHTELE,1                                                        
         XC    VPHDUR,VPHDUR                                                    
         MVC   VPHDUR+1(1),PWK1DURM                                             
         DROP  R1                                                               
*                                                                               
         L     R7,ARREC                                                         
         LA    R7,4(R7)                                                         
         L     R6,AIREC                                                         
         GOTO1 =V(FKPROC),RR=RELO                                               
*                                                                               
D4R90    MVC   INTDAYWK,PDAY1                                                   
         MVC   INTDYBIT,PDAY1BIT                                                
         MVC   SAVEINT,INTVALS     SAVE FOR H4RTN/P4RTN                         
*                                                                               
D4RX     MVI   BYPREAD,0           FOR P4REC, SKIP PROCESSING AND               
         B     READ20              READ NEXT REC W/O GOING TO CONTRLLER         
*                                                                               
D350RTN  LA    RE,HWCV18                                                        
         B     D35XRTN                                                          
*                                                                               
D351RTN  LA    RE,HWCV12                                                        
         B     D35XRTN                                                          
*                                                                               
D352RTN  LA    RE,HWCV06                                                        
         B     D35XRTN                                                          
*                                                                               
D35XRTN  TM    D4AVEHUT+(L'D4AVEHUT-1),X'F0'                                    
         BO    *+10                                                             
         MVC   D4AVEHUT,=16C'0'                                                 
         PACK  DUB,D4AVEHUT                                                     
         CVB   RF,DUB                                                           
         ST    RF,24(RE)                                                        
         TM    D4AVERTG+(L'D4AVERTG-1),X'F0'                                    
         BO    *+10                                                             
         MVC   D4AVERTG,=16C'0'                                                 
         PACK  DUB,D4AVERTG                                                     
         CVB   RF,DUB                                                           
         ST    RF,12(RE)                                                        
         SR    R0,R0                                                            
         L     R1,SVAVGHI          INDEX TO PROGRAM HOMES                       
         A     R1,=F'500'                                                       
         D     R0,=F'1000'                                                      
         ST    R1,FULL                                                          
         L     R1,24(RE)                                                        
         SR    R0,R0                                                            
         D     R0,FULL                                                          
         ST    R1,0(RE)                                                         
         B     READ20                                                           
         EJECT                                                                  
**********************************************************************          
*H4RTN - PROCESS HALF HOUR DETAIL RECORD                                        
**********************************************************************          
*                                                                               
H4RTN    DS    0H                                                               
         USING H4REC,RC                                                         
         CLI   BYPASSH4,1          BYPASS ALL BUT 1ST H4 RECORD                 
         BE    READ20                                                           
         MVI   BYPASSH4,1          FIRST H, BYPASS NEXT TIME                    
         MVI   NOTAVAL,0                                                        
         CLI   H4REPT,C'4'         WAS THE DATA PROVIDED?                       
         BNE   *+12                YES                                          
         MVI   NOTAVAL,2           NO, BYPASS 1/2HR PUTS                        
         B     READ20                                                           
*                                                                               
         MVC   INTVALS(L'SAVEINT),SAVEINT   RESTORE FROM D4RTN                  
         CLC   H4PROJ,ZEROS        ARE TOTAL FIELDS ON THIS RECORD              
         BE    *+22                                                             
         MVC   SVPROJ,H4PROJ       IF YES - SAVE 3 FIELDS                       
         MVC   SVRTG,H4RTG                                                      
         MVC   SVSHR,H4SHR                                                      
         MVC   H4PROJ,SVPROJ       IF NOT - RESTORE FROM A PREVIOUS H4          
         MVC   H4RTG,SVRTG                                                      
         MVC   H4SHR,SVSHR                                                      
*                                                                               
         MVI   INTRTYP,PRCODEQU    **** -P- RECD *****                          
         MVC   PACK16(10),H4NUM                                                 
         MVI   PACK16+10,C'0'                                                   
         PACK  DUB,PACK16(11)                                                   
         MVC   INTPNTI,DUB+2       5 CHAR PWOS                                  
*                                                                               
         PACK  DUB,H4EVDUR(4)      ACTUAL HALF HOUR DURATION                    
         CVB   RF,DUB                                                           
         STC   RF,INTADUR          DUR OF PRG W/IN THIS 1/2HR                   
         PACK  DUB,H4HALF          HALF HOUR CODE                               
         CVB   RF,DUB                                                           
         BCTR  RF,0                CONVERT TO QUARTER HOUR                      
         SLL   RF,1                                                             
*                                                                               
         SR    R1,R1               CLEAR ACCUM                                  
         PACK  DUB,H4DUR1          MINUTES IN FIRST QH                          
         PACK  DUB1,H4DUR2         MINUTES IN SECOND QH                         
         CP    DUB,=P'0'           TEST IF RAN IN FIRST QH                      
         BE    *+12                NO                                           
         LA    R1,1(R1)            YES-BUMP QH DURATION                         
         B     *+8                                                              
         LA    RF,1(RF)            BUMP START QH                                
         CP    DUB1,=P'0'                                                       
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
         LTR   R1,R1               TEST IF PROGRAM RAN DURING HALF HOUR         
         BNZ   *+6                 YES                                          
         DC    H'0'                                                             
         STC   RF,INTSQH           SET START QUARTER HOUR                       
         STC   R1,INTDUR                                                        
         AR    R1,RF               GET END QH                                   
         BCTR  R1,0                                                             
         STC   R1,INTEQH                                                        
         MVC   SAVEINT,INTVALS     SAVE FOR P4RTN                               
         L     R7,ARREC                                                         
         LA    R7,4(R7)                                                         
         L     R6,AIREC                                                         
         GOTO1 =V(GKPROC),RR=RELO                                               
         MVI   BYPREAD,0           FOR P4REC, SKIP PROCESSING AND               
         B     READ20              READ NEXT REC W/O GOING TO CONTRLLER         
         EJECT                                                                  
**********************************************************************          
*P4RTN - PROCESS PERSONS ESTIMATES RECORD                                       
**********************************************************************          
P4RTN    DS    0H                                                               
         MVC   SVD4,ZEROS          RESET D4 RECORD'S SAVE AREA                  
         MVI   BYPASSH4,0          RESET H4 RECORD BYPASS SWITCH                
         USING P4REC,RC                                                         
*                                                                               
         MVC   INTVALS(L'SAVEINT),SAVEINT   RESTORE FROM D4RTN/H4RTN            
         L     R7,ARREC                                                         
         LA    R7,4(R7)                                                         
         L     R6,AIREC                                                         
         CLI   P4DEMTYP,C'1'                                                    
         BE    P4R20               GO HANDLE PRG PUTS                           
*                                                                               
         CLC   P4DEMGRP,=C'001'    NON-PUTS DEMOS                               
         BNE   P4R10                                                            
         MVI   HAVPP,0             RESET PROGRAM PUT SWITCH                     
         GOTO1 =V(AKPROC),RR=RELO  DEMOS 001-020                                
         B     P4R40               BUILD KEY                                    
P4R10    GOTO1 =V(BKPROC),RR=RELO  DEMOS 021-040                                
         B     P4RX                RELEASE RECD                                 
*                                                                               
P4R20    CLC   P4DEMGRP,=C'001'    PROGRAM PUTS                                 
         BNE   P4R30                                                            
         MVI   HAVPP,1             SET HAVE PROG. PUTS                          
         GOTO1 =V(CKPROC),RR=RELO  PRG PUTS DEMOS: 001-020                      
         B     P4R40               BUILD KEY                                    
P4R30    GOTO1 =V(DKPROC),RR=RELO  PRG PUTS DEMOS: 021-040                      
         B     P4RX                RELEASE RECD                                 
         EJECT                                                                  
**********************************************************************          
*P4R40 - BUILD KEY AND RELEASE RECD TO SORT                                     
**********************************************************************          
P4R40    CLI   INTRTYP,PMCODEQU    TEST FOR 'Q' RECORD                          
         BE    P4R70                                                            
*                                                                               
P4R60    LA    R7,INTKEY           BUILD PAV KEY                                
         USING PRKEY,R7                                                         
         MVI   PRCODE,PRCODEQU     *** -P- RECD *********                       
         MVI   PRMED,C'N'                                                       
         MVI   PRSRC,C'N'                                                       
         CLI   CORRSW,0            TEST CORRECTION RECORD                       
         BE    P4R64                                                            
         MVC   PRCODE(2),BOOK1     (BOOK FORCES PROPER SORT)                    
         MVI   PRSRC,PRCODEQU      -P-                                          
P4R64    MVC   PRSTAT,INTSTA                                                    
         MVC   PRSTYP,INTSTYP                                                   
         MVC   PRBOOK,INTBOOK                                                   
         MVC   PRSTIM,INTSQH                                                    
         MVC   PRDW,INTDAYWK                                                    
         MVC   PRDW+1(1),INTDUR    FORCE HIGHER DURATIONS FIRST                 
         XI    PRDW+1,X'FF'                                                     
         B     P4RX                                                             
         DROP  R7                                                               
*                                                                               
P4R70    LA    R7,INTKEY           BUILD 'Q' RECORD KEY                         
         USING PMKEY,R7                                                         
         MVI   PMCODE,PMCODEQU     -Q-                                          
         MVI   PMMEDIA,C'N'                                                     
         MVI   PMSRC,C'N'                                                       
         CLI   CORRSW,0            TEST CORRECTION RECORD                       
         BE    P4R74                                                            
         MVC   PMCODE(2),BOOK1     (BOOK FORCES PROPER SORT)                    
         MVI   PMSRC,PMCODEQU      -Q-                                          
P4R74    MVC   PMBOOK,INTBOOK                                                   
         MVC   PMSTAT,INTSTA                                                    
         MVC   PMSTYP,INTSTYP                                                   
         MVC   PMPNUM,INTPNUM                                                   
         L     RE,=A(QSORTAB)          SORT 'Q' RECORDS BY DAY...               
         LA    R0,QSORTABS         M-S, M-F, VAR, MON...SUN                     
         CLC   INTDAYWK,0(RE)                                                   
         BE    *+14                                                             
         LA    RE,L'QSORTAB(RE)                                                 
         BCT   R0,*-14                                                          
         DC    H'0'                                                             
         MVC   PMRLEN(1),1(RE)     USE FIELD FOLLOWING PMPNUM                   
         DROP  R7                                                               
*                                                                               
P4RX     MVI   BYPREAD,0                                                        
         MVC   1800(4,R2),=F'1'    FORCE LONG SORT RECD TO ADD TP PUTS          
*                                   AND GAA FIGURES                             
         CLC   P4DEMGRP,=C'001'    DEMOGRAPHIC GROUP                            
         BE    READ20              FOR 2ND P4REC, SKIP PROCESSING AND           
*                                  READ NEXT REC W/O GOING TO CONTRLLER         
         CLI   HAVPP,0             HANDLE MISSING PROG. PUTS                    
         BE    *+10                 NTI MUCKS IT UP BY NOT PROVIDING            
*                                   THEM ON A RANDOM BASIS                      
         SPACE 1                                                                
         CLC   P4NET(3),=C'SYN'    NO PROG PUTS FOR SYN                         
         BE    *+12                                                             
         CLI   P4DEMTYP,C'1'       PUT OUT AFTER PROGRAM PUTS                   
         BNE   READ20                                                           
*                                                                               
         LA    R1,CALVPH           CALL CALVPH                                  
         USING CALVPHD,R1                                                       
         LA    RE,INTACCS                                                       
         ST    RE,VPHDEMS                                                       
         L     RE,=A(INTAB)                                                     
         ST    RE,VPHINT                                                        
         GOTO1 VCALVPH,CALVPH                                                   
         L     R7,AIREC                                                         
         USING K0000,R7                                                         
         MVC   VHWC18(12),HWCV18                                                
         MVC   RHWC18(12),HWCR18                                                
         MVC   IHWC18(12),HWCI18                                                
         XC    HWCAREA,HWCAREA                                                  
         DROP  R7                                                               
         DROP  R1                                                               
*                                                                               
P4RX20   MVI   INTKEY+29,255                                                    
         CLI   GAASW,C'Y'                                                       
         BNE   EXIT                                                             
         MVI   INTKEY+29,C'G'                                                   
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*********************************************************************           
* CNVWR- SORT RECORD HOOK. ALL PROCESSING DONE IN CNVWRTN NMOD.                 
*********************************************************************           
*                                                                               
CNVWR    DS    0H                                                               
         GOTO1 =A(CNVRTN),DMCB,(R8),(R4),RR=RELO                                
         B     EXIT                                                             
         EJECT                                                                  
                                                                                
*********************************************************************           
*DAYRTN- ROUTINE TO SET UP DAYS (MON THRU FRI, M-F AND M-S)                     
*        OR TO SET UP A SINGLE DAY FOR A CORRECTION RECORD                      
*********************************************************************           
*                                                                               
DAYRTN   NTR1                                                                   
         USING MIREC,RC                                                         
*                                                                               
         CLC   MISTART,MIEND       TEST SINGLE DAY CORRECTION RECORD            
         BNE   DAY10                                                            
*                                                                               
* SINGLE DAY - APPROPRIATE DAY IS ENTERED IN HDAYTAB                            
*              & REMAINDER OF HDAYTAB IS LEFT INTACT.                           
*                                                                               
         LA    R1,X'10'            MON                                          
         LA    RE,MIREC+(D4DAYS-D4REC)                                          
         LA    R0,7                                                             
DAY02    CLI   0(RE),C'1'          FIND APPROPRIATE DAY                         
         BE    DAY04                                                            
         AH    R1,=H'16'           INCREMENT TO NEXT DAY                        
         LA    RE,1(RE)                                                         
         BCT   R0,DAY02                                                         
         DC    H'0'                                                             
DAY04    STC   R1,BYTE             SAVE DAY CODE IN BYTE                        
*                                                                               
         L     RE,=A(HDAYTAB)                                                   
         LA    R0,HDAYS                                                         
DAY06    CLC   BYTE,1(RE)          TEST DAY CODE                                
         BE    DAY08                                                            
         LA    RE,L'HDAYTAB(RE)                                                 
         BCT   R0,DAY06                                                         
         DC    H'0'                                                             
DAY08    MVC   2(7,RE),MISTART     2(7,RE) = START DAY                          
         MVC   9(7,RE),MIEND       9(7,RE) = END DAY                            
         B     EXIT                                                             
*                                                                               
DAY10    MVC   HDMON+2(7),MISTART  SET UP MONDAY FROM START                     
         MVC   HDMON+9(7),MISTART                                               
         MVC   HDSUN+2(7),MIEND    SET UP SUNDAY FROM END                       
         MVC   HDSUN+9(7),MIEND                                                 
*                                                                               
         XC    HDTUE+2(14),HDTUE+2 CLEAR REAINING DAYS IN TABLE                 
         XC    HDWED+2(14),HDWED+2                                              
         XC    HDTHU+2(14),HDTHU+2                                              
         XC    HDFRI+2(14),HDFRI+2                                              
         XC    HDSAT+2(14),HDSAT+2                                              
         XC    HDM#F+2(14),HDM#F+2                                              
         XC    HDM#S+2(14),HDM#S+2                                              
*                                                                               
         LA    R5,HDMON+3          FILL IN MISSING DATES                        
         LA    R7,HDTUE+3                                                       
         LA    R2,5                                                             
DAY20    GOTO1 VADDAY,DMCB,(R5),(X'20',(R7)),1                                  
         BCTR  R7,0                                                             
         MVC   0(1,R7),HDMON+2                                                  
         MVC   7(7,R7),0(R7)       FOR EQUAL START AND END                      
         LA    R7,1(R7)                                                         
         LA    R5,16(R5)           DO REST OF DAYS                              
         LA    R7,16(R7)                                                        
         BCT   R2,DAY20                                                         
         B     DAY80                                                            
*                                                                               
*                                                                               
DAY80    MVC   HDM#F+2(7),MISTART  SET UP MON-FRI                               
         MVC   HDM#F+9(7),HDFRI+9                                               
         MVC   HDM#S+2(7),MISTART  SET UP MON-SUN                               
         MVC   HDM#S+9(7),MIEND                                                 
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* ********************************************************************          
* PDATA- ROUTINE TO FIND ACTUAL RUN TIME AND AVERAGE DURATION                   
*              P1 =  A(SAVETIME)                                                
*              P2 =  A(DAY BITS FOR WEEK)                                       
* ********************************************************************          
PDATA    NTR1                                                                   
         L     R6,0(R1)            ADDRESS OF SAVETIME                          
         L     RE,4(R1)            ADDRESS OF DAY BITS                          
         MVC   BYTE,0(RE)          DAY BITS                                     
         XC    FULL,FULL           CLEAR DAY COUNT FOR DURATION                 
         LA    R3,X'40'            START AT MONDAY                              
         LA    R5,7                COUNTER                                      
         SR    R7,R7               CLEAR DURATION ACCUM                         
         XC    TIMTAB(7*L'TIMTAB),TIMTAB                                        
         XC    FULL1,FULL1         CLEAR TABLE ENTRY COUNT                      
*                                                                               
PDATA2   EX    R3,*+8              TEST IF PROGRAM RAN ON DAY                   
         B     *+8                                                              
         TM    BYTE,0                                                           
         BZ    PDATA4              NO                                           
         OC    0(8,R6),0(R6)       BYPASS IF NO TIMES                           
         BZ    PDATA4                                                           
*                                                                               
         PACK  DUB,0(2,R6)         START TIME                                   
         CVB   R1,DUB                                                           
         MH    R1,=H'100'          HOURS X 100                                  
         PACK  DUB,2(2,R6)         START MINUTE                                 
         CVB   R0,DUB              MINUTES                                      
         AR    R1,R0                                                            
         CH    R1,=H'2400'         TEST FOR PAST MIDNIGHT                       
         BNH   *+8                 BEFORE OR AT MIDNIGHT                        
         SH    R1,=H'2400'         SUBTRACT FOR MIL TIME                        
         STH   R1,HALF             SAVE MIL TIME                                
         PACK  DUB,4(4,R6)         DURATION IN MINUTES                          
         CVB   R0,DUB                                                           
         AR    R7,R0               UPDATE TOTAL DURATION                        
         L     RE,FULL                                                          
         LA    RE,1(RE)            UPDATE DAY COUNT                             
         ST    RE,FULL                                                          
         BAS   RE,UPTIME           UPDATE START TIME TABLE                      
*                                                                               
PDATA4   SRL   R3,1                NEXT DAY                                     
         LA    R6,L'SAVETIME(R6)   NEXT DAY'S TIME DATA                         
         BCT   R5,PDATA2                                                        
*                                                                               
PDATA6   SR    RE,RE               CALCULATE AVERAGE DURATION                   
         LR    RF,R7               DURATION                                     
         SLDA  RE,1                                                             
         D     RE,FULL                                                          
         LA    RF,1(RF)                                                         
         SRA   RF,1                                                             
         CH    RF,=H'240'          4-HOUR MAXIMUM DURATION SUPPORTED            
         BNH   *+8                                                              
         LH    RF,=H'240'                                                       
         STC   RF,PWK1DURM         RETURN AVE DURATION                          
*                                  SORT TIMES IN DESCENDING ORDER               
         L     R0,FULL1            NUMBER OF ENTRIES IN TABLE                   
         GOTO1 VXSORT,DMCB,(X'FF',TIMTAB),(R0),3,1,2                            
         MVC   PWK1STIM,TIMTAB     FIRST OR MOST FREQUENT TIME                  
         B     EXIT                                                             
*                                                                               
         SPACE 3                                                                
* ********************************************************************          
* UPTIME - ROUTINE TO UPDATE RUN TIME TABLE                                     
*              HALF CONTAINS TIME                                               
*              FULL1 CONTAINS TABLE ENTRY COUNT                                 
* ********************************************************************          
*                                                                               
UPTIME   DS    0H                                                               
         LR    R0,RE                                                            
         LA    R1,7                COUNTER                                      
         LA    RE,TIMTAB                                                        
*                                                                               
UPTIME2  OC    0(L'TIMTAB,RE),0(RE)     TEST FOR E-O-T                          
         BZ    UPTIME4                  INSERT ENTRY                            
         CLC   HALF,0(RE)               TEST IF TIME IS IN TABLE                
         BE    *+12                     YES-BUMP FREQUENCY COUNT                
         LA    RE,L'TIMTAB(RE)                                                  
         BCT   R1,UPTIME2                                                       
*                                                                               
         ZIC   RF,2(RE)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,2(RE)                                                         
         B     UPTIMEX                                                          
*                                                                               
UPTIME4  MVC   0(2,RE),HALF        ADD NEW ENTRY                                
         MVI   2(RE),1             SET FREQUENCY TO 1                           
         L     R1,FULL1                                                         
         LA    R1,1(R1)            INCREMENT TABLE ENTRY COUNT                  
         ST    R1,FULL1                                                         
*                                                                               
UPTIMEX  LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
* *******************************************************************           
* IOERROR - READ ERROR - SET ERROR FLAG AND CLOSE FILE                          
* *******************************************************************           
*                                                                               
IOERROR  DS    0H                                                               
         GOTO1 VLOGIO,DMCB,1,=C'***READ ERROR ON INPUT***'                      
         CLOSE (IN1,REWIND)                                                     
         MVI   INTAPESW,X'82'                                                   
         B     EXIT                                                             
         SPACE 2                                                                
********************************************************************            
* WLR -  WRONG LENGTH RECORD ON INPUT                                           
********************************************************************            
*                                                                               
WLR      DS    0H                                                               
         GOTO1 VLOGIO,DMCB,1,=C'WRONG LENGTH RECORD ON INPUT'                   
         CLOSE (IN1,REWIND)                                                     
         MVI   INTAPESW,X'82'                                                   
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* EOF ON INPUT FILE                                                             
**********************************************************************          
                                                                                
MORET    DS    0H                                                               
ENDJOB   CLOSE (IN1,REWIND)                                                     
         MVI   INTAPESW,X'02'                                                   
         B     EXIT                                                             
                                                                                
XIT      XIT1                                                                   
                                                                                
EXIT     XMOD1 1                                                                
         GETEL R6,DATADISP,ELCODE                                               
         DROP  R2,R4,R8,R9,RA,RB,RC,RE                                          
         EJECT                                                                  
                                                                                
* *****************************                                                 
* LITERAL POOL - FOR MAIN NMOD                                                  
* *****************************                                                 
         LTORG                                                                  
         SPACE 4                                                                
**********************************************                                  
* HUT RECORD DAY CODE TABLE                                                     
*              BYTE 0 = NTI DAY CODE                                            
*              BYTE 1 = INTERNAL DAY CODE                                       
*              BYTES 3-9 = START DAY (CYYMMDD)                                  
*              BYTES 10-16 = END DAY (CYYMMDD)                                  
**********************************************                                  
*                                                                               
HDAYTAB  DS    0CL16                                                            
HDM#S    DC    C'0',X'80',14X'00'  M-SUN                                        
HDMON    DC    C'2',X'10',14X'00'  MON                                          
HDTUE    DC    C'3',X'20',14X'00'  TUE                                          
HDWED    DC    C'4',X'30',14X'00'  WED                                          
HDTHU    DC    C'5',X'40',14X'00'  THU                                          
HDFRI    DC    C'6',X'50',14X'00'  FRI                                          
HDSAT    DC    C'7',X'60',14X'00'  SAT                                          
HDSUN    DC    C'8',X'70',14X'00'  SUN                                          
HDM#F    DC    C'9',X'00',14X'00'  M-FRI                                        
HDAYS    EQU   (*-HDAYTAB)/L'HDAYTAB                                            
*                                                                               
HDAYTABS DS    CL(L'HDAYTAB*HDAYS) SAVE AREA                                    
         SPACE 4                                                                
**********************************************                                  
* INTAB COUNTS TABLE                                                            
*              BYTES 1-3 = DATE (YYMMDD)                                        
*              BYTE 4 = DAY (MON=X'40' - SUN=X'01')                             
*              BYTES 5-156 = COUNTS                                             
**********************************************                                  
         DC    C'**INTAB**'                                                     
         DS    0F                                                               
INTAB    DS    0XL156                                                           
         DS    XL3,XL1,38F         MON                                          
         DS    XL3,XL1,38F         TUE                                          
         DS    XL3,XL1,38F         WED                                          
         DS    XL3,XL1,38F         THU                                          
         DS    XL3,XL1,38F         FRI                                          
         DS    XL3,XL1,38F         SAT                                          
         DS    XL3,XL1,38F         SUN                                          
INTABS   EQU   (*-INTAB)/L'INTAB                                                
         DC    XL3'00'                                                          
         EJECT                                                                  
*********************************************************************           
* CNVWR- SORT RECORD HOOK                                                       
*        NON-NETWORK M-F ROTATORS EMITTED FROM INDIVIDUAL DAY RECORDS           
*        DUPLICATE HUT RECORDS DROPPED                                          
*********************************************************************           
*                                                                               
*ALLIGN1  EQU   (((*-DETCR99+4095)/4096)*4096)    ALLIGN AT 2000                
*         ORG   DETCR99+ALLIGN1                                                 
                                                                                
CNVRTN   NMOD1 0,**CNVW**,RA,RR=RE                                              
         USING DEMCOND,R8          R8 = DEMCNV GLOBAL WORK                      
         USING WORKD,R4            R4 = DETCR99 WORK                            
*                                                                               
CNVWR40  L     R2,ASREC            R2 - USES INTERD DSECT                       
         USING INTERD,R2                                                        
         CLI   INTKEY+29,C'G'      HANDLE GAA AUDIENCE FIGURES                  
         BE    CNVWR100                                                         
*                                                                               
         CLC   INTSTA,=C'UUUUT'    UNIVERSE RECD                                
         BNE   CNVWR42                                                          
         LA    R1,CALVPH                                                        
         USING CALVPHD,R1                                                       
         LA    RE,INTACCS                                                       
         ST    RE,VPHDEMS                                                       
         DROP  R1                                                               
         GOTO1 VCALUNV,CALVPH    CALCULATE UNIVERSE VPH'S                       
         B     CNVWR120                                                         
*                                                                               
CNVWR42  CLC   INTSTA,=C'ITABT'                                                 
         BE    CNVWR120            INTABS   RECD                                
*                                                                               
         CLC   INTSTA,=C'HUT T'    SKIP TV USAGE RECORDS                        
         BE    CNVWR50                                                          
         CLI   INTSTA+4,C'S'       AND SYNDICATORS ALSO                         
         BE    CNVWR50                                                          
         XC    SUMWGHT,SUMWGHT     INIT FOR TP PUTS                             
         XC    SUMHPT,SUMHPT       RAW IMPS                                     
         L     RE,=A(PUTSUM)                                                    
         XC    0(L'PUTSUM,RE),0(RE)                                             
*        XC    PUTSUM,PUTSUM       CALCULATED PERCENTS                          
         CLI   INTDAYWK,X'8F'      THIS PART FOR NON-VAR ONLY                   
         BH    CNVWR45                                                          
         ZIC   RE,INTDAYWK         CONVERT DAY TO NORMAL                        
         SRL   RE,4                                                             
         STC   RE,SUMDAY                                                        
         MVC   SUMSQH,INTSQH                                                    
         MVC   SUMEQH,INTEQH                                                    
*                                                                               
         XC    SUMSTIM,SUMSTIM                                                  
         XC    SUMETIM,SUMETIM                                                  
         CLI   INTRTYP,C'Q'        PROGRAM AVERAGE                              
         BNE   *+16                                                             
         MVC   SUMSTIM,INTSTIM                                                  
         MVC   SUMETIM,INTETIM                                                  
         BAS   RE,QHSUM            SUM THE QUARTER HOURS                        
         B     CNVWR48                                                          
*                                                                               
CNVWR45  LA    R7,INTVAR-L'INTVAR  THIS PART FOR VAR                            
         LA    R9,1                                                             
CNVWR46  SR    R6,R6                                                            
         LA    R6,L'INTVAR(R6)                                                  
         STH   R6,HALF                                                          
         LR    R6,R9               SET THE DAY                                  
         MH    R6,HALF             INDEX                                        
         AR    R6,R7               THEN SLOT                                    
         OC    0(L'INTVAR,R6),0(R6)  ACTIVITY CHECK                             
         BZ    CNVWR47                                                          
         STC   R9,SUMDAY           SET DAY AND TIMES                            
         MVC   SUMSQH,0(R6)                                                     
         MVC   SUMEQH,1(R6)                                                     
*                                                                               
         XC    SUMSTIM,SUMSTIM                                                  
         XC    SUMETIM,SUMETIM                                                  
         CLI   INTRTYP,C'Q'        PROGRAM AVERAGE                              
         BNE   *+16                                                             
         MVC   SUMSTIM,3(R6)                                                    
         MVC   SUMETIM,5(R6)                                                    
*                                                                               
         BAS   RE,QHSUM            ADD UP THE HPT VALUES                        
CNVWR47  LA    R9,1(R9)                                                         
         CH    R9,=H'8'            LOOP UNTIL 7 DAYS ARE DONE                   
         BL    CNVWR46                                                          
*                                                                               
CNVWR48  SR    R6,R6                                                            
         ICM   R6,3,SUMWGHT                                                     
         GOTO1 CHPTDIV,DMCB,SUMHPT,SUMHPT,(R6) GET THE AVERAGE                  
         LA    RE,SUMHPT                                                        
         USING HPTD,RE                                                          
         MVC   TPPUT(L'HPTPUT,R2),HPTPUT SEED THE TP RAW PUTS                   
         MVC   TPHUTR(4,R2),HPTAVER      SEED HH TP HUT                         
         L     RF,=A(PUTSUM)                                                    
         MVC   PUTSAVE,0(RF)                                                    
         BAS   RE,PUTDIV                                                        
         MVC   PUTSTRT(L'PUTSUM,R2),PUTSAVE SEED THE TP PUTS                    
         L     RE,ASREC                                                         
         L     RF,AWREC                                                         
         LH    R1,0(RE)                                                         
         MOVE  ((RF),(R1)),(RE)                                                 
         B     CNVWR50                                                          
*                                                                               
CNVWR50  CLI   IPGAASW,C'Y'        MERGE IN GAA FIGURES IF THERE                
         BNE   CNVWR51                                                          
         L     RE,=A(SVGAIMP)      SRC                                          
         LA    RF,GAIMPS(R2)       DEST                                         
         LA    R1,GAALN1+GAALN2                                                 
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
CNVWR51  MVI   IPGAASW,C'N'        RESET GAA SWITCH                             
         LA    R7,INTKEY                                                        
         USING PRKEY,R7                                                         
         CLI   INTRTYP,PRCODEQU    -P-                                          
         BNE   CNVXIT                                                           
*                                                                               
         CLC   PRSTAT,=C'HUT T'    HUT OR                                       
         BNE   CNV10               AGG (NON-NETWORK)                            
*                                                                               
         B     CNVXIT              EMIT RECORD                                  
*                                                                               
CNVWR100 MVI   IPGAASW,C'Y'        TURN ON FOR MERGE                            
         MVI   BYPSORT,X'80'       DON'T RELEASE TO OP PHASE                    
         LA    RE,AAIMPS(R2)       SOURCE                                       
         L     RF,=A(SVGAIMP)      DEST  - SAVE THE GAA FIGURES                 
         LA    R1,GAALN1           LEN                                          
         MOVE  ((RF),(R1)),(RE)    MOVE DEMS FROM TOP UNTIL YHOMES              
         LA    RE,AAIMP2(R2)       DON'T MOVE HOMES--MOVE AFTER IT              
         L     RF,=A(SVGAIMP)      DEST                                         
         LA    RF,GAALN1(RF)                                                    
         LA    R1,GAALN2           LEN                                          
         MOVE  ((RF),(R1)),(RE)                                                 
         B     CNVXIT                                                           
*                                                                               
CNVWR120 DS    0H                  UNIVERSE RECD JUST GETS RELEASED             
         USING KDSCT,R2                                                         
         MVC   K0537,SVHHUNIV      SAVE UNIV HOMES IMPRESSIONS                  
         B     CNVXIT              RELEASE UNIVERSE RECD                        
*                                                                               
* GENERATE M-F RECORDS FOR AGG (NON-NETWORK) SOURCES                            
         USING INTERD,R2           SORT RECORD                                  
CNV10    CLI   PRDW,X'05'          TEST X'01'(MON) THRU X'05'(FRI)              
         BH    CNVXIT                                                           
         CLI   PRDW,X'01'                                                       
         BL    CNVXIT                                                           
         BH    CNV20               TUE THRU FRI - ACCUMULATE TO SVSREC          
*                                                                               
         L     RF,=A(SVSREC)       MON - CLEAR SVSREC                           
         XCEF  (RF),2000                                                        
         XC    SVNOACCS,SVNOACCS                                                
         XC    SVRECLN,SVRECLN                                                  
         L     RE,ASREC            SAVE SORT RCD IN SVSREC                      
         L     R1,=F'1800'                                                      
         L     RF,=A(SVSREC)                                                    
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
CNV20    DS    0H                  MON THRU FRI                                 
         LA    R1,INTACCS-INTRECLN R1 - DISPLACEMENT TO INTACCS                 
         L     R5,=A(SVSREC)                                                    
         AR    R5,R1               R5 - INTACCS (SVSREC)                        
         LA    RE,INTACCS          RE - INTACCS (SREC)                          
         LH    RF,INTRECLN                                                      
         SR    RF,R1                                                            
         BP    CNV30               OK - WE HAVE DATA                            
*                                                                               
*                                  (NO INTACCS - ZERO DATA)                     
         CLI   PRDW,X'05'          TEST FOR FRIDAY                              
         BNE   CNV60               PURGE MON, TUE, WED & THU                    
         L     RF,SVNOACCS         FRI                                          
         LTR   RF,RF               WAS THERE ANY DATA AT ALL                    
         BNZ   CNV70               YES - AVERAGE THE DATA AND EMIT RCD          
         B     CNV90               NO - SIMPLY EMIT RECORD                      
*                                                                               
CNV30    LA    RF,3(RF)            (IN CASE OF TRUNCATED FULLWORD)              
         SRL   RF,2                RF - NUMBER OF INTACCS FULLWORDS             
         CLC   SVRECLN,INTRECLN                                                 
         BNL   CNV36                                                            
         MVC   SVRECLN,INTRECLN    SAVE MAX. RECORD LENGTH                      
         ST    RF,SVNOACCS         SAVE MAX. NO. OF INTACCS FULLWORDS           
*                                                                               
CNV36    CLI   PRDW,X'01'          TEST FOR MONDAY                              
         BE    CNV60               PURGE MON RECORD (DATA SAVED)                
*                                                                               
CNV40    L     R1,0(R5)            ACCUMULATE FIVE DAYS' DATA                   
         A     R1,0(RE)                                                         
         ST    R1,0(R5)                                                         
         LA    RE,4(RE)                                                         
         LA    R5,4(R5)                                                         
         BCT   RF,CNV40                                                         
*                                                                               
         CLI   PRDW,X'05'          TEST FOR FRIDAY                              
         BE    CNV70                                                            
CNV60    MVI   INTRTYP,0           PURGE RECORD (MON, TUE, WED & THU)           
         B     CNVXIT                                                           
*                                                                               
CNV70    L     RE,=A(SVSREC)       FRIDAY - EMIT RECORD                         
         L     R1,=F'1800'                                                      
         L     RF,ASREC                                                         
         MOVE  ((RF),(R1)),(RE)    MOVE SVSREC TO SREC                          
         OC    SVRECLN,SVRECLN                                                  
         BZ    CNV90               (ZERO DATA)                                  
         LH    RF,SVRECLN          USE MAXIMUM RECORD LENGTH                    
         LA    RF,7(RF)            ADD 4 FOR SHOMES (& 3 FOR SAFETY)            
         STH   RF,INTRECLN         SET NEW MAXIMUM RECORD LENGTH                
*                                                                               
         LA    RE,INTACCS          RE - INTACCS (SREC)                          
         L     RF,SVNOACCS         RF - MAX. NO. OF INTACCS FULLWORDS           
CNV80    L     R1,0(RE)            AVERAGE FIVE DAYS' DATA                      
         LTR   R1,R1                                                            
         BZ    CNV84                                                            
         SR    R0,R0                                                            
         A     R1,=F'2'            ROUND                                        
         D     R0,=F'5'            DIVIDE BY FIVE DAYS                          
         ST    R1,0(RE)                                                         
CNV84    LA    RE,4(RE)                                                         
         BCT   RF,CNV80                                                         
*                                                                               
         SPACE 2                                                                
         USING KDSCT,R2                                                         
CNV88    XC    K0533,K0533         CALC RHOMES USING IHOMES & UNIV              
         L     R1,K0537            IHOMES                                       
         LTR   R1,R1                                                            
         BZ    CNV90                                                            
         M     R0,=F'10'                                                        
         D     R0,SVHHUNIV         (USE UNIVERSE SAVED IN H1RTN)                
         SR    R0,R0                                                            
         A     R1,=F'5'            ROUND                                        
         D     R0,=F'10'                                                        
         ST    R1,K0533            RHOMES (XX.X)                                
*                                                                               
         SPACE 2                                                                
         XC    K0541,K0541         CALC SHOMES USING IHOMES & HUT               
         L     R1,K0537            IHOMES                                       
         ZIC   RF,PRSTIM           6A=0, 630A=2  -  6P=48, 630P=50              
         SRL   RF,1                QHR --> HHR                                  
         LA    RE,HPTLN            INDEX INTO BUFFER                            
         MR    RE,RE                                                            
         LR    RE,RF               SHIFT PRODUCT                                
         A     RE,VPUTBUFF                                                      
         USING HPTD,RE                                                          
*                                                                               
         OC    HPTAVEI,HPTAVEI                                                  
         BZ    CNV90               (ZERO HUT)                                   
         M     R0,=F'1000'                                                      
         D     R0,HPTAVEI          HUT PROJ                                     
         SR    R0,R0                                                            
         A     R1,=F'5'            ROUND                                        
         D     R0,=F'10'                                                        
         M     R0,=F'10'                                                        
         ST    R1,K0541            SHOMES (XX.0)                                
         MVC   K1045,HPTAVER       SET PUT                                      
         DROP  R2                                                               
*                                                                               
CNV90    MVI   PRDW,X'00'          SET DAY/WEEK TO M-F                          
         B     CNVXIT              EMIT RECORD                                  
*                                                                               
CNVX     XIT1                      FOR NTR1'S                                   
         DROP  R7                                                               
*                                                                               
CNVXIT   XMOD1                                                                  
         EJECT                                                                  
* ********************************************************************          
* PUTDIV - ?                                                                    
* ********************************************************************          
PUTDIV   NTR1                                                                   
         SR    R6,R6                                                            
         ICM   R6,3,SUMWGHT                                                     
         LR    R0,R6                                                            
         LA    R1,L'PUTSAVE                                                     
         SRL   R1,2                DIV BY 4                                     
         LA    RE,PUTSAVE                                                       
PUTDIV1  L     R6,0(RE)                                                         
         SR    R7,R7                                                            
         SRDA  R6,31                                                            
*                                                                               
         LTR   R0,R0               IF ZERO, BYPASS DIVIDE & CLR R7              
         BNZ   *+10                                                             
         SR    R7,R7                                                            
         B     PUTDIV4                                                          
*                                                                               
         DR    R6,R0                                                            
         A     R7,=F'1'                                                         
         SRA   R7,1                                                             
PUTDIV4  ST    R7,0(RE)                                                         
         LA    RE,4(RE)                                                         
         BCT   R1,PUTDIV1                                                       
         B     CNVX                                                             
         EJECT                                                                  
* ********************************************************************          
* QHSUM -REQUIRES SUMSQH,SUMEQH,SUMDAY TO BE SET                                
* ********************************************************************          
QHSUM    NTR1                                                                   
         MVI   QHSFRST,1                                                        
         CLC   SUMSTIM,SUMETIM     END LESS THAN START                          
         BNH   QHSUM1                                                           
         SR    RE,RE               ADJUST FOR AFTER MIDNIGHT                    
         ICM   RE,3,SUMETIM                                                     
         LA    RE,2400(RE)                                                      
         STCM  RE,3,SUMETIM                                                     
*                                                                               
QHSUM1   LA    R6,HPTLN                                                         
         STH   R6,HALF                                                          
         MH    R6,=H'48'           48 HALF HOURS IN A DAY                       
         ZIC   R7,SUMDAY                                                        
         MR    R6,R6                                                            
         A     R7,VPUTBUFF                                                      
         OC    SUMSTIM,SUMSTIM     PROGS HAVE START AND END TIMES               
         BZ    QHSUM4                                                           
         CLI   QHSFRST,1           HANDLE ONE MINUTE PROGRAMS                   
         BE    *+14                                                             
         CLC   SUMSTIM,SUMETIM                                                  
         BNL   QHSUM8                                                           
         MVI   QHSFRST,0                                                        
         GOTO1 VHRTOQH,DMCB,SUMSTIM,SUMSQH                                      
         SR    RF,RF                                                            
         ICM   RF,3,SUMSTIM        GET THE START                                
         LA    RF,1(RF)            INC. BY 1 MINUTE                             
         SR    RE,RE                                                            
         D     RE,=F'100'          GET HOURS AND MINUTES                        
         CH    RE,=H'60'           TAKE CARE OF 60 SEC BASE                     
         BNE   *+8                                                              
         LH    RE,=H'100'          SET TO BUMP THE HOUR                         
         MH    RF,=H'100'                                                       
         AR    RE,RF                                                            
         STCM  RE,3,SUMSTIM                                                     
         B     QHSUM5                                                           
*                                                                               
QHSUM4   CLC   SUMSQH,SUMEQH                                                    
         BH    QHSUM8                                                           
QHSUM5   ZIC   R6,SUMSQH                                                        
         SRL   R6,1                CONVERT TO HALF                              
         MH    R6,HALF             SET TO CORRECT DISP                          
         AR    R7,R6                                                            
         SR    R6,R6                                                            
         ICM   R6,3,SUMWGHT                                                     
         LA    R6,1(R6)                                                         
         STCM  R6,3,SUMWGHT                                                     
         ZIC   R6,SUMSQH                                                        
         LA    R6,1(R6)                                                         
         STC   R6,SUMSQH                                                        
         GOTO1 CHPTMAD,DMCB,(R7),SUMHPT,1                                       
*                                                                               
         L     R6,=A(UNIVSAVE)                                                  
         LA    R7,HPTPUT-HPTFLAG(R7) POINT TO RAW PUTS                          
         GOTO1 VCALPUT,DMCB,0,0,(R6),(R7),PUTSAVE                               
         LA    R1,PUTEND-PUTSTRT                                                
         SRL   R1,2                DIVIDE BY 4                                  
         L     RE,=A(PUTSAVE)                                                   
         L     RF,=A(PUTSUM)                                                    
QHSUM6   L     R7,0(RE)                                                         
         A     R7,0(RF)                                                         
         ST    R7,0(RF)                                                         
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R1,QHSUM6                                                        
         B     QHSUM1                                                           
*                                                                               
QHSUM8   B     CNVX                                                             
         EJECT                                                                  
*                                                                               
CHPTMAD  NTR1                      WEIGHTED ADD ROUTINE                         
         LA    RE,HPTAVEI-HPTFLAG  START OF ACCUMS                              
         LR    RF,RE                                                            
         A     RE,0(R1)            SOURCE                                       
         A     RF,4(R1)            DEST                                         
         L     R0,8(R1)            WEIGHT                                       
         LA    R9,HPTDEND-HPTAVEI  FIND NUMBER OF ACCUMS                        
         SRL   R9,2                                                             
CHPTMAD1 L     R6,0(RE)            GET A VALUE FROM SOURCE                      
         LR    R7,R0               SWAP IN WEIGHT                               
         MR    R6,R6               DO WEIGHTING                                 
         A     R7,0(RF)            ADD IN PREVIOUS VALUES                       
         ST    R7,0(RF)            SAVE IN DEST.                                
         LA    RE,4(RE)            NEXT SLOT                                    
         LA    RF,4(RF)                                                         
         BCT   R9,CHPTMAD1         LOOP UNTIL DONE                              
         B     CNVX                                                             
                                                                                
*                                                                               
CHPTDIV  NTR1                      UNWEIGHT ACCUMS ROUTINE                      
         LA    RE,HPTAVEI-HPTFLAG  START OF ACCUMS                              
         LR    RF,RE                                                            
         A     RE,0(R1)            SOURCE                                       
         A     RF,4(R1)            DEST                                         
         L     R0,8(R1)            WEIGHT                                       
         LA    R9,HPTDEND-HPTAVEI  FIND NUMBER OF ACCUMS                        
         SRL   R9,2                                                             
CHPTDIV1 L     R6,0(RE)            GET A VALUE                                  
         SR    R7,R7               GET READY FOR DIV                            
         SRDA  R6,31                                                            
*                                                                               
         LTR   R0,R0               IF ZERO, BYPASS DIVIDE & CLR R7              
         BNZ   *+10                                                             
         SR    R7,R7                                                            
         B     CHPTDIV4                                                         
*                                                                               
         DR    R6,R0               UNWEIGHT                                     
         A     R7,=F'1'            AND ROUND                                    
         SRA   R7,1                                                             
CHPTDIV4 ST    R7,0(RF)            SAVE IN DEST.                                
         LA    RE,4(RE)            NEXT SLOT                                    
         LA    RF,4(RF)                                                         
         BCT   R9,CHPTDIV1          LOOP UNTIL DONE                             
         B     CNVX                                                             
         EJECT                                                                  
* *****************************                                                 
* LITERAL POOL - CNVWRTN NMOD                                                   
* *****************************                                                 
         LTORG                                                                  
         EJECT                                                                  
                                                                                
**********************************************************************          
*SUBRTN2 -EXTRA PROCESING RTNS AVOIDING ADDRESS ERRORS                          
**********************************************************************          
SUBR2    DS    0H                                                               
         NMOD1 0,**SUB2**,RA,RR=RE                                              
         USING DEMCOND,R8          R8 = DEMCNV GLOBAL WORK                      
         USING WORKD,R4            R4 = DETCR99 WORK                            
         L     RC,0(R1)                                                         
         USING D4REC,RC                                                         
*                                                                               
         ZIC   R1,GOSUBN                                                        
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         B     R02_00(R1)                                                       
*                                                                               
SETDB#   EQU   (R02_01-*)/4+1     SET DBUFFER AND SAVETIME TABLE                
GTIME#   EQU   (R02_02-*)/4+1     RTNS ACTIVE DAYS, TOT DUR AND AVG DUR         
*                                                                               
R02_00   DS    0H                                                               
R02_01   B     SETDBUF             SET DBUFF AND SAVETIME BUFFERS               
R02_02   B     GTIME               GET TIMES FROM DBUFF                         
*                                                                               
R01#     EQU   (*-R02_00)/4                                                     
         DC    H'0'                                                             
*                                                                               
XIT_2    XIT1                                                                   
         EJECT                                                                  
*                                                                               
**********************************************************************          
*SETDBUF - USING D4 RECD FIELDS, SET FIELDS IN DBUFFER FOR ACTIVE DAYS          
**********************************************************************          
SETDBUF  DS    0H                                                               
         OC    DBDAYS,D4DAYS       SAVE TOTAL DAYS PRGM RAN                     
         PACK  DUB,D4EVSHR         START TIME                                   
         CVB   R1,DUB                                                           
         MH    R1,=H'100'          HOURS X 100                                  
         PACK  DUB,D4EVSMIN        START MINUTE                                 
         CVB   R0,DUB              MINUTES                                      
         AR    R1,R0                                                            
         CH    R1,=H'2400'         TEST FOR PAST MIDNIGHT                       
         BNH   *+8                 BEFORE OR AT MIDNIGHT                        
         SH    R1,=H'2400'         SUBTRACT FOR MIL TIME                        
         STH   R1,HALF             SAVE MIL TIME                                
         GOTO1 VHRTOQH,DMCB,HALF,STQHR         START QH                         
*                                                                               
         LA    R7,DBUFF                                                         
         LA    R3,SAVETIME         SET IN SAVETIME AS WELL                      
         LA    R1,D4DAYS           PT TO DAY BITS                               
         LA    R0,7                LOOP THRU 7 DAYS                             
STDB10   CLI   0(R1),C'1'          IS BIT ON                                    
         BNE   STDB15                                                           
         BAS   RE,DTIME                                                         
*                                                                               
STDB15   LA    R1,1(R1)            NEXT DAY ON TAPE RECD                        
         LA    R7,DBUFDAYQ(R7)     NEXT DAY IN BUFFER                           
         LA    R3,L'SAVETIME(R3)                                                
         BCT   R0,STDB10                                                        
         OC    DBDAYS,D4DAYS       SAVE TOTAL DAYS PRGM RAN                     
*                                                                               
SETDBX   B     XIT_2                                                            
         DROP  RC                                                               
         EJECT                                                                  
**********************************************************************          
*DTIME - FILLS DBUFF WITH 1/2HRS AND DURTN PRG AIRED FOR DAY                    
*        INPUT:  R7     ->  DAY IN DBUFF                                        
*                R3     ->  DAY IN SAVETIME                                     
*                STSQH   =  START TIME                                          
*                D4DUR   =  DURATION                                            
*                D4EVSMIN =  START MINUTE                                       
**********************************************************************          
DTIME    NTR1                                                                   
         USING D4REC,RC                                                         
         MVC   DMCB(1),STQHR                                                    
         NI    DMCB,X'FE'          FORCE TO 1/2HR                               
         XC    DMCB+4(4),DMCB+4    ACCUM TOT DUR FOR DAY                        
*                                                                               
         OC    0(4,R7),0(R7)       WAS A START TIME SET FOR DAY?                
         BNZ   *+10                                                             
         MVC   0(4,R7),D4EVSHR     SAVE START TIME                              
         MVC   0(4,R3),0(R7)       PUT START TIME IN DBUFF IN SAVETIME          
         ZIC   R1,4(R7)            PICK UP TOT DUR SO FAR FOR DAY               
         PACK  DUB,D4DUR                                                        
         CVB   R2,DUB              R2=DURATION OF PRGM                          
         AR    R1,R2               ADD THIS DUR TO TOT FOR DAY                  
         CH    R1,=H'240'          1BYTE MAX DEFAULT= 240 MIN PRGM              
         BNH   *+8                                                              
         LH    R1,=H'240'                                                       
         STC   R1,4(R7)            SAVE TOT DUR                                 
         EDIT  (1,4(R7)),(4,4(R3))                                              
         OC    4(4,R3),ZEROS                                                    
         LA    R7,5(R7)            PT TO HLF HRS LIST                           
*                                                                               
DTIM05   CLI   0(R7),0                                                          
         BE    DTIM10                                                           
         CLC   DMCB(1),0(R7)       SAME QHR CODE?                               
         BE    DTIM10                                                           
         LA    R7,2(R7)            NEXT 1/2HR SLOT                              
         B     DTIM05                                                           
*                                                                               
DTIM10   MVC   0(1,R7),DMCB                                                     
         LA    R1,30               1ST HLF HR MAX = '30'                        
         CLC   D4EVSMIN,=C'30'     STARTS AFTER END OF 1ST HLFHR                
         BL    *+8                                                              
         LA    R1,60               MAX MIN = 60 FOR 2ND 1/2HR                   
         PACK  DUB,D4EVSMIN                                                     
         CVB   R0,DUB                                                           
         SR    R1,R0               CALC # MIN AVAIL TO RUN IN 1/2 HR            
*                                                                               
DTIM15   CR    R1,R2               AT LEAST 30 MIN LEFT IN TOTDUR               
         BNH   *+6                                                              
         LR    R1,R2               ELSE USE REMAINING DURN                      
         ZIC   R0,1(R7)                                                         
         AR    R0,R1               R1 + ANY EXISTING DUR                        
         STC   R0,1(R7)                                                         
         SR    R2,R1               REDUCE TOT DUR BY AMT USED IN 1/2HR          
         BZ    DTIMEX                       ACCTD FOR ALL MINS OF DURN          
*                                                                               
DTIM20   ZIC   R3,0(R7)                                                         
         LA    R3,2(R3)            BUMP TO NEXT 1/2HR                           
         LA    R7,2(R7)            NEXT 1/2HR SLOT FOR DAY                      
         ZIC   R0,0(R7)            SEE IF MATCH ON 1/2HR                        
         BNZ   *+12                IF EMPTY SLOT, SAVE 1/2HR CODE               
         STC   R3,0(R7)                                                         
         B     *+12                                                             
         CR    R0,R3               BUCKET NOT EMPTY --SAME 1/2 HR?              
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R1,30               30 MIN MAX IN 1/2HR                          
         B     DTIM15                                                           
*                                                                               
DTIMEX   B     XIT_2                                                            
         DROP  RC                                                               
         EJECT                                                                  
**********************************************************************          
*GTIME - GET ACCUMULATED DATA FROM DBUFF                                        
*        INPUT - STQHR  = 1/2 HOUR CODE                                         
*                DBDAYS = DAYS PROGRAM RAN                                      
*                                                                               
*        OUTPUT- HLFADUR= AVG DUR FOR 1/2HR                                     
*                HLFDAYS= DAYS ACTIVE FOR THIS 1/2HR                            
*                HTOTDUR= TOTAL DUR FOR HLFHR ACROSS ACTIVE DAYS                
**********************************************************************          
GTIME    DS    0H                                                               
         XC    HTOTDUR,HTOTDUR                                                  
         XC    HLFADUR,HLFADUR                                                  
         MVC   HLFDAYS,ZEROS                                                    
         XC    DMCB(8),DMCB                                                     
         LA    R0,7                                                             
         LA    R7,DBUFF                                                         
         LA    R3,DBDAYS                                                        
         LA    R6,HLFDAYS                                                       
*                                                                               
GTIME5   CLI   0(R3),C'1'          WAS THIS DAY ACTIVE                          
         BE    GTIME7                                                           
         MVI   0(R6),C'0'                                                       
GTIME6   LA    R7,DBUFDAYQ(R7)                                                  
         LA    R6,1(R6)                                                         
         LA    R3,1(R3)                                                         
         BCT   R0,GTIME5                                                        
         B     GTIM30                                                           
*                                                                               
GTIME7   DS    0H                                                               
         ST    R7,DMCB             SAVE DAY PTR TO DBUFF ENTY                   
         LA    R7,5(R7)            PT TO 1/2HR CODE LIST                        
         SR    R1,R1                                                            
*                                                                               
GTIM10   CLI   0(R7),0                                                          
         BE    GTIM20              DONE WITH DAY                                
         CLC   STQHR,0(R7)                                                      
         BE    *+12                                                             
         LA    R7,2(R7)                                                         
         B     GTIM10                                                           
         MVI   0(R6),C'1'          SET ACTIVE DAY                               
         ZIC   R1,1(R7)                                                         
         A     R1,HTOTDUR          SAVE HLF HOUR DUR                            
         ST    R1,HTOTDUR                                                       
         L     R1,DMCB+4           BUMP ACTIVE DAYS                             
         LA    R1,1(R1)                                                         
         ST    R1,DMCB+4                                                        
GTIM20   L     R7,DMCB                                                          
         B     GTIME6                                                           
*                                                                               
GTIM30   DS    0H                                                               
         OC    DMCB+4(4),DMCB+4    NUMBER OF ACTIVE DAYS                        
         BZ    GTIMEX                                                           
         SR    RE,RE               CALCULATE AVERAGE DURATION                   
         L     RF,HTOTDUR          DURATION                                     
         SLDA  RE,1                                                             
         D     RE,DMCB+4                                                        
         LA    RF,1(RF)                                                         
         SRA   RF,1                                                             
         STC   RF,HLFADUR          RETURN AVG DURATION                          
*                                                                               
GTIMEX   DS    0H                                                               
         B     XIT_2                                                            
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL - SURB2 NMOD                                                     
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* WORKING STORAGE                                                               
***********************************************************************         
                                                                                
*ALLIGN2  EQU   (((*-DETCR99+4095)/4096)*4096)    ALLIGN AT 1000 BNDRY          
*         ORG   DETCR99+ALLIGN2                                                 
WORKD    DS    0F                                                               
RELO     DS    A                                                                
INTABADR DS    A                   INTAB TABLE ADDRESS                          
DATADISP DS    H                                                                
ELCODE   DS    X                                                                
PACK16   DS    PL16                                                             
ASLOT    DS    A                                                                
GOSUBN   DS    X                                                                
SVHHUNIV DS    F                   HOUSEHOLD UNIVERSE FROM H1 RECORD            
HPTPARA  DS    6F                                                               
HPTNOW   DS    F                                                                
HPTCNT   DC    H'0'                                                             
HPTFRST  DC    X'01'                                                            
*                                                                               
RELOFRST DC    X'01'                                                            
BYPREAD  DC    X'00'                                                            
BYPASSH4 DC    X'00'               BYPASS ALL BUT 1ST H4 RECORD                 
BYPASS01 DC    X'00'               BYPASS SINGLE DAY AVERAGES                   
CORRSW   DC    X'00'               CORRRECTION RECORD SWITCH                    
HUTSW    DC    X'00'               HUT RECORD GENERATION SWITCH                 
MTWTFSW  DC    X'00'               HUT RECORD (M-T-W-T-F USED)                  
PREVSTIM DC    X'FF'               HUT RECORD PREVIOUS START QTR HR             
*                                                                               
GAASW    DC    C'N'                GROSS AVG AUD SWITCH                         
HAVPP    DC    X'00'               HAVE PROGRAM PUTS                            
QHSFRST  DS    C                                                                
QHRSW    DS    C                                                                
BYTE     DS    C                                                                
SWITCH   DS    C                                                                
CNVWRSW  DS    C                                                                
VARIOUS  DS    X                   1=VARIOUS                                    
VARS     DC    CL7'0000000'        USED FOR VARIOUS (AVERAGES)                  
DAYS     DC    CL7'0000000'        USED FOR VARIOUS (INDIVIDUAL DAYS)           
*                                                                               
SVDA     DS    F                                                                
SVKEY    DC    XL24'00'                                                         
PAVKEY   DC    XL24'00'                                                         
*                                                                               
BLANKS   DC    80C' '                                                           
ZEROS    DC    256C'0'             ZEROS - USE FOR A VARIETY OF THINGS          
*                                                                               
SAVETIME DS    7XL8                SAVE D4EVSHR, D4EVSMIN AND D4DUR             
VARSTIME DS    7XL8                SAVE D4EVSHR, D4EVSMIN AND D4DUR             
*                                                                               
SAVVAR   DS    7XL7                SAVE VAR INFO FROM INDIVIDUAL DAYS           
*                                                                               
                                                                                
*-------------------------------------------------------------------            
*DBUFF - CLEARED WHEN D-RECD W/TOT DUR PRESENT.                                 
*        HOLDS START TIME OF PRGM ON DAY, TOTAL DUR, UP TO                      
*        12- HLF HOURS (QHR CODE) WITH DURATION RAN IN THAT HLF HR              
*-------------------------------------------------------------------            
DBUFF    DS    0C                  7-DAYS, 12-HLFHRS W/1 BYTE DUR BKTS          
DBUFMON  DS    CL4                 START HOUR/MIN                               
         DS    CL1                 TOTAL DUR FOR DAY                            
         DS    12CL2               12 BKTS: HLFHR CODE/DUR IN HLFHR             
DBUFDAYQ EQU   *-DBUFMON           DISP TO NEXT DAY IN BUFFER                   
         DS    6CL(DBUFDAYQ)       6 MORE DAYS                                  
DBUFFQ   EQU   *-DBUFF             L'DBUFFER                                    
*                                                                               
DBDAYS   DS    CL7                 DAYS ACTIVE IN DBUFF                         
*-------------------             PROCESS H-RECS USING DBUFF INFO                
HLFDAYS  DS    CL7                 DAYS THIS 1/2HR WAS ACTIVE                   
HLFADUR  DS    X                   AVERAGE 1/2HR DUR                            
HTOTDUR  DS    F                   TOTAL DUR FOR 1/2HR ACROSS DAYS              
STQHR    DS    X                                                                
*-------------------------------------------------------------------            
*                                                                               
THREE    DS    CL3                                                              
         DS    0F                                                               
CALVPH   DS    CL(VPHINT-VPHDEMS+4) CALCULATE VPH'S                             
SAVEINT  DS    CL(INTACCS-INTVALS) SAVE INTERIM RECORD                          
SAVEPNUM DS    CL10                SAVE PROGRAM NUMBER                          
*                                                                               
TIMTAB   DS    7CL3                                                             
*                                                                               
         EJECT                                                                  
* REPORT DESCRIPTOR SAVE AREA (RESET FOR CORRECTION RECORDS)                    
*                                                                               
BOOK1    DS    XL2                 KEY BOOK                                     
IBOOK1   DS    XL2                 INTERNAL BOOK                                
*                                                                               
BOOK1S   DS    XL2                 KEY BOOK (SAVE AREA)                         
IBOOK1S  DS    XL2                 INTERNAL BOOK (SAVE AREA)                    
*                                                                               
* HOUSEHOLD USAGE SAVE AREA                                                     
*                                                                               
HUTSQH   DS    X                   NEXT QUARTER HOUR                            
HUTDUR   DS    X                                                                
HUTBOOK  DS    XL2                 KEY BOOK VALUE                               
HUTIBOOK DS    XL2                 INTERNAL BOOK VALUE                          
HUTSTA   DS    CL5                                                              
HUTDAYWK DS    X                                                                
*                                                                               
* PROGRAM DESCRIPTOR SAVE AREA                                                  
*                                                                               
PVALS    DS    0CL87                                                            
PNTINUM  DS    CL5                 NTI PRG NUMBER PWOS                          
PNUM     DS    XL2                 PROGRAM NUMBER                               
PNET     DS    CL4                 NETWORK                                      
PDPT     DS    C                   NSI DAYPART (ALWAYS ZERO)                    
PDPT2    DS    CL2                 NTI DAYPART                                  
PTYP     DS    CL2                 PROGRAM TYPE                                 
PPREM    DS    C                   PREMIERE CODE                                
PNAME    DS    CL25                PROGRAM NAME                                 
PTITLE   DS    CL32                EPISODE TITLE                                
*                                                                               
PDAY1    DS    X                   DAY CODE                                     
PDAY1BIT DS    X                   DAY BITS                                     
*                                                                               
PWK1STIM DS    XL2                 START TIME                                   
PWK1DURM DS    X                   DURATION IN MINUTES                          
PWK1AUD  DS    XL2                                                              
PWK1STAC DS    XL2                                                              
PWK1COV  DS    XL2                                                              
PWK1DTYP DS    X                                                                
PWK1RSF  DS    X                                                                
*                                                                               
         EJECT                                                                  
SVD4     DS    0CL83               SAVE D4 RCD'S POSITIONS 305-387              
*        THESE FIELDS ARE NOT REPORTED ON ALL D4 RECORDS                        
S4STA    DS    CL5        305-309  TOTAL PROGRAM STATION COUNT                  
         DS    CL5        310-314  TOT PROGRAM HEADEND COUNT                    
S4COV    DS    CL2        315-316  TOTAL PROGRAM COVERAGE PERCENT               
         DS    CL2        317-318  CA PROGRAM COVERAGE                          
         DS    CL9        319-327  (FILLER)                                     
S4TELE   DS    CL3        328-330  TELECASTS 01=DAY 02-07=AVERAGES              
S4TOTDUR DS    CL6        331-336  TOTAL DURATION                               
S4AVEHUT DS    CL9        337-345  PROGRAM AVERAGE PROJ. (XXX,XXX,XXX)          
S4AVERTG DS    CL3        346-348  PROGRAM AVERAGE RATING (XX.X)                
S4REPORT DS    CL1        349-349  REPORTABILITY IND. 0=REPORTABLE              
         DS    CL3        350-352  CA HH RTG                                    
         DS    CL1        353      CA REPORTABILITY                             
S4HUT    DS    CL9        354-362  PROGRAM HUT PROJ. (XXX,XXX,XXX)              
S4SHR    DS    CL2        363-364  PROGRAM SHARE (XX)                           
         DS    CL9        365-373  CA AUD PROJ                                  
         DS    CL2        374-375  CA HH SHR                                    
S4PROJ   DS    CL9        376-384  TOTAL AUDIENCE PROJ. (XXX,XXX,XXX)           
S4RTG   DS    CL3        385-387  TOTAL AUDIENCE RATING (XX.X)                  
         DS    CL13       388-400  (FILLER)                                     
*                                                                               
*        THESE FIELDS ARE NOT REPORTED ON ALL H4 RECORDS                        
SVPROJ   DS    CL9         92-100  HALF HOUR AUD. PROJ. (XXX,XXX,XXX)           
SVRTG    DS    CL3        101-103  HALF HOUR AUDIENCE RATING (XX.X)             
SVSHR    DS    CL2        114-115  HALF HOUR PROGRAM SHARE (XX)                 
*                                                                               
SVRECLN  DS    F                   SAVE INTERIM RECORD'S LENGTH                 
SVNOACCS DS    F                   SAVE NUMBER OF INTACCS FULLWORDS             
*                                                                               
* FILE DCB AND BUFFER AREA                                                      
*                                                                               
IN1      DCB   DDNAME=IN1,                                             X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=400,                                              X        
               BLKSIZE=16800,                                          X        
               MACRF=GM,                                               X        
               EODAD=MORET                                                      
*                                                                               
SUMWGHT  DS    CL2                                                              
SUMDAY   DS    C                                                                
SUMSQH   DS    C                                                                
SUMEQH   DS    C                                                                
SUMSTIM  DS    CL2                                                              
SUMETIM  DS    CL2                                                              
         DS    0F                                                               
SUMHPT   DS    CL188                                                            
SVAVGHI  DS    F                   SAVE PROGRAM HH                              
HWCAREA  DS    0CL36               SAVE HH WITH CHILD DATA                      
HWCV18   DS    F                                                                
HWCV12   DS    F                                                                
HWCV06   DS    F                                                                
HWCR18   DS    F                                                                
HWCR12   DS    F                                                                
HWCR06   DS    F                                                                
HWCI18   DS    F                                                                
HWCI12   DS    F                                                                
HWCI06   DS    F                                                                
*                                                                               
*                                                                               
IPGAASW  DS    C                                                                
NOTAVAL  DS    C                   DATA NOT AVAILABLE                           
PUTSAVE  DS    CL(PUTEND-PUTSTRT)  SAVE AREA FOR CALCULATED PUTS                
PUTSUM   DS    CL(PUTEND-PUTSTRT)  SUM AREA FOR CALCULATED PUTS                 
UNIVSAVE DS    50F                 SAVE AREA FOR PER. UNIVERSES                 
**SVHREC   DS    2000C               SAVE HUT RECORD                            
*                                                                               
*        THE NEXT 1500 BYTES USED FOR EITHER THE SORT OR INTERIM RECORD         
*                                                                               
SVSREC   EQU   *                   SAVE SORT RECORD                             
SVIREC   DS    2000C               SAVE INTERIM RECORD                          
SVGAIMP  DS    444C                                                             
         SPACE 1                                                                
*                            NET(4),PRG#(2),DAY(1) E.G. 7C,7F,90                
PRGAVG   DS    0XL7                PROG#'S HAVING ASSOCD AVG RECD               
         DC    1000X'00'           PROG#'S HAVING ASSOCD AVG RECD               
         DC    X'FFFF'                                                          
         EJECT                                                                  
******************************************                                      
* TABLE OF DAY BIT SETTINGS AND DAY CODES                                       
******************************************                                      
                                                                                
NETDAYTB DS    0CL2                                                             
         DC    X'4010'             MON                                          
         DC    X'2020'             TUE                                          
         DC    X'1030'             WED                                          
         DC    X'0840'             THU                                          
         DC    X'0450'             FRI                                          
         DC    X'0260'             SAT                                          
         DC    X'0170'             SUN                                          
         DC    X'7C00'             M-F                                          
         DC    X'7F80'             M-SUN                                        
NETDAYS  EQU   (*-NETDAYTB)/L'NETDAYTB                                          
*                                                                               
*******************************************************                         
* TABLE OF DAY CODES AND SORT VALUES FOR 'Q' RECORDS                            
*******************************************************                         
QSORTAB  DS    0XL2                                                             
         DC    X'8000'             M-S                                          
         DC    X'0001'             M-F                                          
         DC    X'9002'             VAR                                          
         DC    X'1003'             MON                                          
         DC    X'2004'             TUE                                          
         DC    X'3005'             WED                                          
         DC    X'4006'             THU                                          
         DC    X'5007'             FRI                                          
         DC    X'6008'             SAT                                          
         DC    X'7009'             SUN                                          
QSORTABS EQU   (*-QSORTAB)/L'QSORTAB                                            
*                                                                               
         SPACE 1                                                                
* TABLE OF TELECAST TYPES AND THEIR BIT SETTINGS USED FOR INTDTYP               
*                                                                               
TYPTAB   DS    0CL3                                                             
         DC    C'  ',X'09'         REGULAR - ORIGINAL (PROTECT)                 
         DC    C'0 ',X'09'         REGULAR - ORIGINAL                           
         DC    C' Y',X'11'         REGULAR - REPEAT   (PROTECT)                 
         DC    C'0Y',X'11'         REGULAR - REPEAT                             
         DC    C'1 ',X'0C'         SPECIAL - ORIGINAL                           
         DC    C'1Y',X'14'         SPECIAL - REPEAT                             
TYPES    EQU   (*-TYPTAB)/L'TYPTAB                                              
*                                                                               
         EJECT                                                                  
*********************************************************************           
* TABLE OF NETWORK CODES AND THEIR CALL LETTERS                                 
*********************************************************************           
*                                                                               
NETTAB   DS    0CL9                                                             
*      ******************************************                               
*              NETWORKS                                                         
*      ******************************************                               
         DC    C'ABC ',CL5'ABC T'                                               
         DC    C'CBS ',CL5'CBS T'                                               
         DC    C'FOX ',CL5'FOX T'                                               
         DC    C'NBC ',CL5'NBC T'                                               
         DC    C'PAR ',CL5'PAR T'      PARAMOUNT NTI                            
         DC    C'PAX ',CL5'PAX T'                                               
         DC    C'PX  ',CL5'PX  T'                                               
         DC    C'WB  ',CL5'WB  T'      WARNER BROTHERS NTI                      
*      ******************************************                               
*      AS OF 7/99 NEW DEFINITIONS FOR AGGREGATES:                               
*      ******************************************                               
         DC    C'AGG',AL1(03),CL5'PBS T'  PBS                                   
         DC    C'AGG',AL1(04),CL5'PPY T'  <= NEW DEFN HAS EXCLUSIONS            
         DC    C'AGG',AL1(07),CL5'XIN T'  IND W/O FOX,SUPS,TBS                  
         DC    C'AGG',AL1(08),CL5'XSU T'  SPST:WGN,XWGN,WPIX,KTLA,WSBK          
         DC    C'AGG',AL1(10),CL5'AGA T'  ABC AFFILIATES                        
         DC    C'AGG',AL1(11),CL5'AGB T'  CBS AFFILIATES                        
         DC    C'AGG',AL1(12),CL5'AGC T'  NBC AFFILIATES                        
         DC    C'AGG',AL1(21),CL5'FAF T'  FOX AFFIL INCL BCAST & CABLE          
         DC    C'AGG',AL1(24),CL5'ONA T'  OTHER NETWORK AFFILIATES              
         DC    C'AGG',AL1(26),CL5'TNA T'  TOTAL NETWORK AFFILIATES              
         DC    C'AGG',AL1(27),CL5'IBR T'  INDEP BROADCAST                       
         DC    C'AGG',AL1(28),CL5'ADC T'   AD SUPPORTED CABLE ORIG              
         DC    C'AGG',AL1(29),CL5'NAC T'   NON-AD SUPORTED CABLE NETS           
*                                                                               
*OLD FILES:                                                                     
*&&DO                                                                           
         DC    C'AGG',X'01',CL5'IND T'                                          
         DC    C'AGG',X'02',CL5'SUP T'                                          
         DC    C'AGG',X'03',CL5'PBS T'                                          
         DC    C'AGG',X'04',CL5'PAY T'                                          
         DC    C'AGG',X'05',CL5'CAB T'                                          
         DC    C'AGG',X'06',CL5'FAF T'  EFF. 01/91                              
         DC    C'AGG',X'07',CL5'XIN T'                                          
         DC    C'AGG',X'08',CL5'XSU T'                                          
         DC    C'AGG',X'09',CL5'CAT T'                                          
         DC    C'AGG',X'0A',CL5'AGA T'                                          
         DC    C'AGG',X'0B',CL5'AGB T'                                          
         DC    C'AGG',X'0C',CL5'AGC T'                                          
         DC    C'AGG',X'0D',CL5'AGD T'                                          
         DC    C'AGG',X'0E',CL5'AGE T'                                          
         DC    C'AGG',X'0F',CL5'AGF T'                                          
*&&                                                                             
*                                                                               
*      ******************************************                               
*                SYNDICATORS                                                    
*      ******************************************                               
         DC    C'B/O ',CL5'BLORS'       BLAIR ENT./ORBIS                        
         DC    C'ESPN',CL5'ESPNS'       ESPN SPORTS NETWORK                     
         DC    C'F/L ',CL5'F/L S'       FRIES DIST./LBS                         
         DC    C'G-T ',CL5'G-T S'       GOODSEN-TODMAN                          
         DC    C'G/Y ',CL5'G/Y S'       GAYLORD/LBS COMMUNICATIONS              
         DC    C'G/L ',CL5'G/L S'       GAYLORD SYNDICOM                        
         DC    C'J/M ',CL5'J/M S'       JOHNSON PUB./MK THOMAS                  
         DC    C'L-T ',CL5'L-T S'       LORIMAR-TELEPICTURES                    
         DC    C'L/C ',CL5'L/C S'       LBS COMMUNICATIONS/COKE                 
         DC    C'L/O ',CL5'L/O S'       LBS/JIM OWENS                           
         DC    C'M&&M ',CL5'M&&M S'       M & M SYNDICATION                     
         DC    C'M-T ',CL5'M-T S'       MCA-TV                                  
         DC    C'M/&& ',CL5'M/&& S'       MGM/UA COMM. INC./LBS                 
         DC    C'M/C ',CL5'M/C S'       MGM/UA CAMELOT                          
         DC    C'M/L ',CL5'M/L S'       MGM ENT./LBS COMM.                      
         DC    C'M/U ',CL5'M/U S'       MGM/UA TEVISION DIST.                   
         DC    C'O&&M ',CL5'O&&M S'       OLGILVY AND MATHER                    
         DC    C'P/A ',CL5'P/A S'       PARKSIDE/ACCESS                         
         DC    C'S/O ',CL5'S/O S'       SYNDICOM ORBIS                          
         DC    C'T/G ',CL5'T/G S'       TELETRIB/GROUP W                        
         DC    C'T/H ',CL5'T/H S'       TELETRIB/HAL ROACH                      
         DC    C'T/L ',CL5'T/L S'       TELETRIB/LORIMAR                        
         DC    C'T/M ',CL5'T/M S'       TELETRIB/MCA                            
         DC    C'T/Q ',CL5'T/Q S'       TELETRIB/QINTEX ENT.                    
         DC    C'T/S ',CL5'T/S S'       TITAN SPORTS                            
         DC    C'T/T ',CL5'T/T S'       TPE/TELEREP                             
         DC    C'T/V ',CL5'T/V S'       TELETRIB/VIACOM                         
         DC    C'TBS ',CL5'TBS S'       TURNER BROADCASTING SALES               
         DC    C'TEC ',CL5'TEC S'       TELETRIB/EMBASSY                        
         DC    C'TEL ',CL5'TEL S'       TELEREP                                 
         DC    C'TEN ',CL5'TEN S'       TELEVISION ENTERPRISE NETW.             
         DC    C'TLT ',CL5'TLT S'       TELETRIB                                
         DC    C'TM  ',CL5'TM  S'       TEN MEDIA                               
         DC    C'TOT ',CL5'TOT S'       TOTAL VIDEO                             
         DC    C'TPE ',CL5'TPE S'       TELEVISON PROGRAM ENTERP.               
         DC    C'TRC ',CL5'TRC S'       TELEVISION RADIO CABLE GROUP            
         DC    C'TRE ',CL5'TRE S'       TRIBUNE ENTERTAINMENT                   
         DC    C'TRI ',CL5'TRI S'       TRI-STAR PICTURES INC.                  
         DC    C'TTW ',CL5'TTW S'       WTTW                                    
         DC    C'TW  ',CL5'TW  S'       TRANS WORLD INTERNATIONAL               
         DC    C'VIA ',CL5'VIA S'       VIACOM ENTERPRISES, INC.                
         DC    C'VIC ',CL5'VIC S'       VICTORY TELEVISION                      
         DC    C'WB  ',CL5'WB  S'       WARNER BROTHERS TELEVISION              
         DC    C'WC  ',CL5'WC  S'       WINNER COMM.                            
         DC    C'WV  ',CL5'WV  S'       WORLDVISION ENTERP.                     
         DC    C'WW  ',CL5'WW  S'       WESTERN WORLD TELEVISION                
         DC    C'Y&&R ',CL5'Y&&R S'       YOUNG & RUBICAM                       
         DC    C'2/L ',CL5'2/L S'       20TH CENTURY FOX/LBS                    
         DC    C'2/M ',CL5'2/M S'       20TH CENTURY FOX/MPC, INC.              
         DC    C'20  ',CL5'20F S'       20TH CENTURY FOX TV                     
NETWORKS EQU   (*-NETTAB)/L'NETTAB                                              
                                                                                
WORKX    EQU   *                                                                
                                                                                
         EJECT                                                                  
***********************************************************************         
* DSECTS  ******  DSECTS  ****** DSECTS ****** DSECTS ****** DSECTS             
***********************************************************************         
       ++INCLUDE DENNHPTD                                                       
         EJECT                                                                  
* DSECT TO COVER NIELSEN'S MEDIA INFORMATION TAPE (1ST 60 POSITIONS)            
*                                                                               
MIREC    DSECT                                                                  
MISEQ    DS    CL2          1-2    RECORD SEQUENCE CODE                         
MISAMPLE DS    CL1          3      SAMPLE INDICATOR                             
*                                    BLANK = NATIONAL PEOPLE METER              
*                                    H     = NATIONAL HISPANIC                  
MICORDTE DS    CL7          4-10   START OF CORRECTION INTERVAL                 
*                                    CYYMMDD                                    
MIORIG   DS    CL1         11      0=ORIGINAL 1=CORRECTION 2=DELETION           
MICORWHY DS    CL3         12-14   CORRECTION REASON                            
*                                   BLANK = N/A                                 
*                                   100=NEW 101=VIEWING CHANGE                  
*                                   102=DESCRIPTIVE DATA CHANGE                 
*                                   103=INFORMATIONAL ONLY CHANGE               
*                                   200=PROPRIETARY DATA CHANGE                 
MINET    DS    CL6         15-20   NETWORK (ABC, CBS, NBC, ETC.)                
*                                  OR - DATA TYPE CODE (TVU, INT, ETC.)         
MINUM    DS    CL10        21-30   PROGRAM/STATION/CATEGORY CODE                
MITRACK  DS    CL3         31-33   TRACKAGE ID (CABLE ONLY)                     
MIFEED   DS    CL1         34      FEED PATTERN                                 
*                                   " "=NTI D=DUAL L=LIVE H=HISPANIC            
MIBREAK  DS    CL1         35      0=NOT A BREAKOUT...1=BREAKOUT                
MISPEC   DS    CL1         36      0=NOT A SPECIAL....1=SPECIAL                 
*                                  2=RECURRING SPECIAL                          
MIESTYPE DS    CL1         37      AUDIENCE ESTIMATE TYPE                       
*                                  1=AVG AUD  2=GAA 3=GSA(SYND W/SPOT)          
MIDELVRY DS    CL2         38-39   DELIVERY OPTIONS                             
*                                   "  "  = NOT USED                            
*                                   01-20 = AIRS ACROSS MULTIPLE DPTS           
*                                   21-40 = DELIVERY VEHICLE                    
*                                   41-40 = IN-HOME/OUT-OF-HOME                 
MISYNORG DS    CL1         40      SYNDICATORS ONLY - ORIG/REPEAT/COMB          
MISYNPRO DS    CL1         41      SYNDICATORS ONLY - PROGRAM SPOTS             
MIDYSWKS DS    CL2         42-43   NUMBER OF DAYS/WEEKS                         
MISTART  DS    CL7         44-50   CYYMMDD                                      
MIEND    DS    CL7         51-57   CYYMMDD                                      
MITELNUM DS    CL10        58-67   TELECAST NUMBER                              
MICOMNUM DS    CL3         68-70   COMPONENT NUMBER                             
MICOVSAM DS    CL6         71-76   COVERAGE SAMPLE ID                           
MICOVCAL DS    CL1         77      COVERAGE CALCULATION IND                     
*                                   BLANK = TOTAL US                            
*                                   1     = HOMES ABLE TO VIEW                  
*                                   2     = TOTAL CABLE UNIVERSE                
*                                   3     = OTHER                               
MIMKTBRK DS    CL3         78-80   MARKET BREAK IDENTIFIER                      
MITPHH   DS    CL2         81-82   TOTAL PROGRAM/HALF HOUR IDENTIFIER           
MIQHID   DS    CL2         83-84   QUARTER HOUR ID                              
MITYPE   DS    CL1         85      RECORD TYPE - B,C,D,E,F,G,H,M,P              
*                                                OR BLANK                       
MIEVSHR  DS    CL2         86-87   EVENT START HOUR                             
MIEVSMIN DS    CL2         88-89   EVENT START MINUTE                           
MIEVSSEC DS    CL2         90-91   EVENT START SECOND                           
MIDEMGRP DS    CL3         92-94   DEMOGRAPHIC GROUP START ID                   
MIDEMTYP DS    CL1         95      DEMOGRAPHIC RECORD TYPE                      
MIDEMAVI DS    CL1         96      DEMO AVERAGE IND                             
MIVCR    DS    CL1         97      VCR INDICATOR - BLANK=INCLUDES VCR           
*                                    1=EXCLUDES   2=INCLUDES PLAYBACK           
MINA     DS    CL1         98      NOT AVAILABLE FLAG - BLANK=OK                
*                                    1=NOT AVAILABLE                            
MIRCTR   DS    CL1         99      RECORD CTR - 0=UNIQUE RECORD IN 1-58         
*                                    1-9=MULTIPLE OCCURRENCES                   
MIBUCTYP DS    CL1        100      BUCKET TYPE                                  
MIPPPV   DS    CL1        101      POCKETPIECE VS PRELIMINARY IND               
MIVARRSN DS    CL4        102-105  VARIANCE REASON                              
*                                                                               
         DS    CL9        106-114  (FILLER - ALWAYS BLANK)                      
*                                                                               
         EJECT                                                                  
* DSECT TO COVER REPORT DESCRIPTOR RECORD                                       
*                                                                               
B0REC    DSECT                                                                  
B0SEQ    DS    CL2          1-2    C'00' - REPORT DESCRIPTOR RECORD             
         DS    CL39         3-41                                                
B0DYSWKS DS    CL2         42-43   NUM DAYS/WEEKS=01                            
B0START  DS    CL7         44-50   CYYMMDD                                      
B0END    DS    CL7         51-57   CYYMMDD                                      
         DS    CL57        58-114                                               
B0REPORT DS    CL25        115-139 REPORT/FILE ID - "POCKETPIECE"               
* DSECT TO COVER PROGRAM DESCRIPTOR RECORD                                      
*                                                                               
D4REC    DSECT                                                                  
D4SEQ    DS    CL2          1-2    C'04' - PROGRAM DESCRIPTOR RECORD            
         DS    CL8          3-10                                                
D4ORIG   DS    CL1         11      0=ORIGINAL.........1=CORRECTION              
         DS    CL3         12-14                                                
D4NET    DS    CL6         15-20   NETWORK (ABC, CBS OR NBC)                    
D4NUM    DS    CL10        21-30   PROGRAM/STATION/CATEGORY CODE                
         DS    CL4         31-34                                                
D4BREAK  DS    CL1         35      0=NOT A BREAKOUT...1=BREAKOUT                
D4SPEC   DS    CL1         36      0=NOT A SPECIAL....1=SPECIAL                 
         DS    CL1         37                                                   
         DS    CL2         38-39                                                
D4TTYPE  DS    CL1         40      TELECAST TYPE (SYND)                         
*                                  O=ORIGINAL   R=REPEAT                        
*                                  C=COMBINED   M=MULTIPLE                      
         DS    CL1         41                                                   
D4DYSWKS DS    CL2         42-43   NUMBER OF DAYS/WEEKS                         
D4START  DS    CL7         44-50   CYYMMDD                                      
D4END    DS    CL7         51-57   CYYMMDD                                      
         DS    CL20        58-77                                                
D4MKTBRK DS    CL3         78-80   MARKET BREAK                                 
         DS    CL4         81-84                                                
D4TYPE   DS    CL1         85      C'D' - PROGRAM DESCRIPTOR DATA               
D4EVSHR  DS    CL2         86-87   EVENT START HOUR (06-29)                     
D4EVSMIN DS    CL2         88-89   EVENT START MINUTE (00-59)                   
         DS    CL7         90-96                                                
D4VCR    DS    CL1         97      VCR INDICATOR - BLANK=INCLUDES VCR           
*                                    1=EXCLUDES   2=INCLUDES PLAYBACK           
D4NAVAL  DS    CL1         98      1 = DATA NOT AVAILABLE                       
         DS    CL16        99-114                                               
D4NAME   DS    CL25       115-139  PROGRAM NAME                                 
         DS    CL40       140-179                                               
D4TITLE  DS    CL32       180-211  EPISODE TITLE                                
         DS    CL4        212-215                                               
D4ALPHA  DS    CL4        216-219  PROGRAM TYPE (ALPHA)                         
         DS    CL9        220-228                                               
D4MULTI  DS    CL1        229      MULTI-DAY     Y=MULTIPLE TELECASTS           
D4REPEAT DS    CL1        230      REPEAT IND.   Y=COMPLEX PROGRAM              
         DS    CL7        231-237                                               
D4PREM   DS    CL1        238      PREMIER IND.  Y=PREMIER TELECAST             
         DS    CL11       239-249                                               
D4SYNID  DS    CL4        250-253  SYNDICATOR ID (SEE NETWORK TABLE)            
         DS    CL22       254-275                                               
D4DPT    DS    CL2        276-277  REPORTED DAYPART                             
*                                    PR=PRIME TIME   WM=WEEKDAY MORNING         
*                                    EF=EARLY FRINGE WD=WEEKDAY DAYTIME         
*                                    LF=LATE FRINGE  ED=WEEKEND DAYTIME         
D4DUR    DS    CL4        278-281  EVENT DURATION   0000-1440 MINUTES           
D4DAYS   DS    0CL7      (282-288) DAYS OF WEEK INDICATOR                       
D4MON    DS    CL1        282        MONDAY      (1 OR 0)                       
D4TUE    DS    CL1        283        TUESDAY     (1 OR 0)                       
D4WED    DS    CL1        284        WEDNESDAY   (1 OR 0)                       
D4THU    DS    CL1        285        THURSDAY    (1 OR 0)                       
D4FRI    DS    CL1        286        FRIDAY      (1 OR 0)                       
D4SAT    DS    CL1        287        SATURDAY    (1 OR 0)                       
D4SUN    DS    CL1        288        SUNDAY      (1 OR 0)                       
         DS    CL16       289-304                                               
*                                                                               
         EJECT                                                                  
*        THESE FIELDS (305-   ) ARE NOT REPORTED ON ALL D4 RECORDS              
D4STA    DS    CL5        305-309  TOTAL PROGRAM STATION COUNT                  
         DS    CL5        310-314                                               
D4COV    DS    CL2        315-316  TOTAL PROGRAM COVERAGE PERCENT               
         DS    CL11       317-327                                               
D4TELE   DS    CL3        328-330  TELECASTS 01=DAY 02-07=AVERAGES              
D4TOTDUR DS    CL6        331-336  TOTAL DURATION                               
D4AVEHUT DS    CL9        337-345  PROGRAM AVERAGE PROJ. (XXX,XXX,XXX)          
D4AVERTG DS    CL3        346-348  PROGRAM AVERAGE RATING (XX.X)                
D4REPORT DS    CL1        349      REPORTABILITY IND. 0=REPORTABLE              
         DS    CL4        350-353                                               
D4HUT    DS    CL9        354-362  PROGRAM HUT PROJ. (XXX,XXX,XXX)              
D4SHR    DS    CL2        363-364  PROGRAM SHARE (XX)                           
         DS    CL11       365-375                                               
D4PROJ   DS    CL9        376-384  TOTAL AUDIENCE PROJ. (XXX,XXX,XXX)           
D4RTG    DS    CL3        385-387  TOTAL AUDIENCE RATING (XX.X)                 
         DS    CL12       388-400                                               
*                                                                               
D4OPT    DS    CL1        401-401  0=REGULAR DATA   1=OPTIONAL DATA             
*                                                                               
         EJECT                                                                  
* DSECT TO COVER HOUSEHOLD PROGRAM RECORD                                       
*                                                                               
H4REC    DSECT                                                                  
H4SEQ    DS    CL2          1-2    C'04' - HALF HOUR DETAIL RECORD              
         DS    CL8          3-10                                                
H4ORIG   DS    CL1         11      0=ORIGINAL.........1=CORRECTION              
         DS    CL3         12-14                                                
H4NET    DS    CL6         15-20   NETWORK (ABC, CBS OR NBC)                    
H4NUM    DS    CL10        21-30   PROGRAM/STATION/CATEGORY CODE                
         DS    CL13        31-43                                                
H4START  DS    CL7         44-50   CYYMMDD                                      
H4END    DS    CL7         51-57   CYYMMDD                                      
         DS    CL23        58-80                                                
H4HALF   DS    CL2         81-82   HALF HOUR ID   01-48                         
         DS    CL2         83-84                                                
H4TYPE   DS    CL1         85      C'H' - HALF HOUR HOUSEHOLD DATA              
H4EVSHR  DS    CL2         86-87   EVENT START HOUR                             
H4EVSMIN DS    CL2         88-89   EVENT START MINUTE                           
         DS    CL25        90-114                                               
H4EVDUR  DS    CL4        115-118  DURATION PRG RAN W/IN 1/2HR                  
H4DAYS   DS    0CL7      (119-125) DAYS OF WEEK INDICATOR                       
H4MON    DS    CL1        119        MONDAY      (1 OR 0)                       
H4TUE    DS    CL1        120        TUESDAY     (1 OR 0)                       
H4WED    DS    CL1        121        WEDNESDAY   (1 OR 0)                       
H4THU    DS    CL1        122        THURSDAY    (1 OR 0)                       
H4FRI    DS    CL1        123        FRIDAY      (1 OR 0)                       
H4SAT    DS    CL1        124        SATURDAY    (1 OR 0)                       
H4SUN    DS    CL1        125        SUNDAY      (1 OR 0)                       
         DS    CL54       126-179                                               
H4PROJ   DS    CL9        180-188  HALF HOUR AUD. PROJ. (XXX,XXX,XXX)           
H4RTG    DS    CL3        189-191  HALF HOUR AUDIENCE RATING (XX.X)             
H4REPT   DS    CL1        192      1/2HR REPORTABILITY INDICATOR                
         DS    CL13       192-205                                               
H4SHR    DS    CL2        206-207  HALF HOUR PROGRAM SHARE (XX)                 
         DS    CL46       208-253                                               
H4DUR1   DS    CL6        254-259  1ST QTR HOUR   1-15 MINS.-INDIV. DAY         
*                                    DURATION     2-75 MINS.-AVERAGES           
         DS    CL43       260-302                                               
H4DUR2   DS    CL6        303-308  2ND QTR HOUR   0-15 MINS.-INDIV. DAY         
*                                    DURATION     2-75 MINS.-AVERAGES           
         SPACE 2                                                                
* DSECT TO COVER DEMO PROGRAM RECORD                                            
*                                                                               
P4REC    DSECT                                                                  
P4SEQ    DS    CL2          1-2    C'04' - DEMO PROGRAM RECORD                  
         DS    CL12         3-14                                                
P4NET    DS    CL6         15-20   NETWORK                                      
         DS    CL64        21-84                                                
P4TYPE   DS    CL1         85      C'P' - PERSONS                               
         DS    CL6         86-91                                                
P4DEMGRP DS    CL3         92-94   DEMOGRAPHIC GROUP START ID                   
P4DEMTYP DS    CL1         95      DEMOGRAPHIC RECORD TYPE                      
*                                    0=NOT PROGRAM PUTS  1=PROGRAM PUTS         
         DS    CL20        96-115                                               
P4DCATS  DS    CL180      116-295  DEMOGRAPHIC CATEGORIES (20CL9)               
         EJECT                                                                  
*        DEINTD                                                                 
       ++INCLUDE DEINTD                                                         
*        DEINTNTID                                                              
       ++INCLUDE DEINTNT3D                                                      
         EJECT                                                                  
*        DECALVPHD                                                              
       ++INCLUDE DECALVPHD                                                      
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
         PRINT OFF                                                              
         ENDCODE                                                                
         TITLE '- DEMO CONVERSION - NTI POCKETPIECE'                            
*        DEDEMFILE                                                              
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
         SPACE 2                                                                
*        DEDEMCNVD                                                              
         PRINT OFF                                                              
       ++INCLUDE DEDEMCNVD                                                      
         PRINT ON                                                               
         SPACE 2                                                                
         ENDCODE                                                                
